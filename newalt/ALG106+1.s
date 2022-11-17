fof(f5119, plain, $false, inference(avatar_sat_refutation, [], [f766, f783, f800, f834, f851, f868, f894, f895, f896, f897, f898, f900, f905, f906, f907, f908, f909, f910, f915, f951, f985, f1036, f1087, f1138, f1172, f1189, f1192, f1197, f1198, f1199, f1200, f1201, f1202, f1206, f1209, f1210, f1211, f1212, f1213, f1219, f1230, f1241, f1248, f1250, f1261, f1267, f1271, f1281, f1288, f1290, f1291, f1301, f1307, f1310, f1311, f1316, f1328, f1330, f1341, f1350, f1356, f1368, f1371, f1377, f1394, f1411, f1428, f1454, f1465, f1472, f1473, f1474, f1475, f1485, f1491, f1495, f1505, f1512, f1513, f1514, f1515, f1525, f1531, f1532, f1534, f1535, f1540, f1552, f1553, f1554, f1565, f1574, f1580, f1592, f1593, f1595, f1601, f1618, f1635, f1652, f1670, f1672, f1673, f1675, f1730, f1751, f1959, f2375, f2377, f2378, f2413, f2439, f2447, f2485, f2501, f2504, f2532, f2615, f2633, f2650, f2652, f2657, f2659, f2663, f2671, f2673, f2688, f2694, f2713, f2731, f2762, f2775, f2784, f2787, f2798, f2852, f2874, f2880, f2889, f2890, f2894, f2919, f2936, f2944, f2950, f2955, f2967, f2973, f2978, f2982, f2994, f3017, f3021, f3024, f3029, f3061, f3062, f3072, f3076, f3080, f3090, f3102, f3106, f3164, f3170, f3179, f3200, f3204, f3216, f3234, f3246, f3250, f3251, f3255, f3262, f3276, f3291, f3298, f3303, f3308, f3319, f3334, f3343, f3354, f3356, f3375, f3377, f3382, f3400, f3410, f3411, f3423, f3444, f3456, f3463, f3507, f3577, f3581, f3603, f3626, f3632, f3643, f3646, f3664, f3703, f3719, f3723, f3740, f3748, f3751, f3773, f3794, f3802, f3816, f3817, f3885, f3899, f3906, f3965, f3984, f4005, f4017, f4050, f4096, f4106, f4143, f4153, f4162, f4205, f4257, f4292, f4300, f4308, f4319, f4425, f4451, f4482, f4484, f4511, f4585, f4677, f4695, f4704, f4708, f4727, f4733, f4749, f4767, f4772, f4828, f4903, f4909, f4981, f4982, f5000, f5003, f5014, f5070, f5076, f5099, f5104])).
fof(f5104, plain, (~ spl42_102 | ~ spl42_103), inference(avatar_contradiction_clause, [], [f5103])).
fof(f5103, plain, ($false | (~ spl42_102 | ~ spl42_103)), inference(subsumption_resolution, [], [f5102, f309])).
fof(f309, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax8)).
fof(f5102, plain, ((e21 = e22) | (~ spl42_102 | ~ spl42_103)), inference(backward_demodulation, [], [f1082, f1078])).
fof(f1078, plain, ((e21 = op2(e21, e22)) | ~ spl42_102), inference(avatar_component_clause, [], [f1076])).
fof(f1076, plain, (spl42_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_102])])).
fof(f1082, plain, ((e22 = op2(e21, e22)) | ~ spl42_103), inference(avatar_component_clause, [], [f1080])).
fof(f1080, plain, (spl42_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_103])])).
fof(f5099, plain, (~ spl42_99 | ~ spl42_103), inference(avatar_split_clause, [], [f5095, f1080, f1063])).
fof(f1063, plain, (spl42_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_99])])).
fof(f5095, plain, (~ (e22 = op2(e21, e23)) | ~ spl42_103), inference(backward_demodulation, [], [f287, f1082])).
fof(f287, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax6)).
fof(f5076, plain, (~ spl42_102 | ~ spl42_118), inference(avatar_split_clause, [], [f5071, f1144, f1076])).
fof(f1144, plain, (spl42_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_118])])).
fof(f5071, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_118), inference(backward_demodulation, [], [f264, f1146])).
fof(f1146, plain, ((e21 = op2(e20, e22)) | ~ spl42_118), inference(avatar_component_clause, [], [f1144])).
fof(f264, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f5070, plain, (~ spl42_116 | ~ spl42_124), inference(avatar_split_clause, [], [f5066, f1169, f1135])).
fof(f1135, plain, (spl42_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_116])])).
fof(f1169, plain, (spl42_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_124])])).
fof(f5066, plain, (~ (e23 = op2(e20, e23)) | ~ spl42_124), inference(backward_demodulation, [], [f280, f1171])).
fof(f1171, plain, ((e23 = op2(e20, e21)) | ~ spl42_124), inference(avatar_component_clause, [], [f1169])).
fof(f280, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f5014, plain, (~ spl42_126 | ~ spl42_62 | ~ spl42_192 | ~ spl42_196 | spl42_221), inference(avatar_split_clause, [], [f5013, f1896, f1748, f1727, f874, f1178])).
fof(f1178, plain, (spl42_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl42_126])])).
fof(f874, plain, (spl42_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl42_62])])).
fof(f1727, plain, (spl42_192 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_192])])).
fof(f1748, plain, (spl42_196 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_196])])).
fof(f1896, plain, (spl42_221 <=> (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_221])])).
fof(f5013, plain, (~ (op2(e20, e20) = e21) | (~ spl42_62 | ~ spl42_192 | ~ spl42_196 | spl42_221)), inference(forward_demodulation, [], [f5012, f1728])).
fof(f1728, plain, ((e21 = h4(e11)) | ~ spl42_192), inference(avatar_component_clause, [], [f1727])).
fof(f5012, plain, (~ (op2(e20, e20) = h4(e11)) | (~ spl42_62 | ~ spl42_196 | spl42_221)), inference(forward_demodulation, [], [f5011, f876])).
fof(f876, plain, ((op1(e10, e10) = e11) | ~ spl42_62), inference(avatar_component_clause, [], [f874])).
fof(f5011, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | (~ spl42_196 | spl42_221)), inference(forward_demodulation, [], [f1898, f1749])).
fof(f1749, plain, ((e20 = h4(e10)) | ~ spl42_196), inference(avatar_component_clause, [], [f1748])).
fof(f1898, plain, (~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10))) | spl42_221), inference(avatar_component_clause, [], [f1896])).
fof(f5003, plain, (~ spl42_199 | ~ spl42_22 | ~ spl42_192 | spl42_230), inference(avatar_split_clause, [], [f5002, f1932, f1727, f704, f1764])).
fof(f1764, plain, (spl42_199 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_199])])).
fof(f704, plain, (spl42_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_22])])).
fof(f1932, plain, (spl42_230 <=> (h3(e11) = h4(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_230])])).
fof(f5002, plain, (~ (e21 = h3(e11)) | (~ spl42_22 | ~ spl42_192 | spl42_230)), inference(forward_demodulation, [], [f5001, f1728])).
fof(f5001, plain, (~ (h3(e11) = h4(e11)) | (~ spl42_22 | spl42_230)), inference(forward_demodulation, [], [f1934, f706])).
fof(f706, plain, ((e11 = op1(e12, e12)) | ~ spl42_22), inference(avatar_component_clause, [], [f704])).
fof(f1934, plain, (~ (h3(e11) = h4(op1(e12, e12))) | spl42_230), inference(avatar_component_clause, [], [f1932])).
fof(f5000, plain, (~ spl42_119 | ~ spl42_55 | ~ spl42_196 | spl42_236), inference(avatar_split_clause, [], [f4999, f1956, f1748, f844, f1148])).
fof(f1148, plain, (spl42_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_119])])).
fof(f844, plain, (spl42_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_55])])).
fof(f1956, plain, (spl42_236 <=> (h4(op1(e10, e12)) = op2(h4(e10), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_236])])).
fof(f4999, plain, (~ (e22 = op2(e20, e22)) | (~ spl42_55 | ~ spl42_196 | spl42_236)), inference(forward_demodulation, [], [f4998, f1703])).
fof(f1703, plain, (e22 = h4(e12)), inference(forward_demodulation, [], [f549, f533])).
fof(f533, plain, (e22 = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(e23, op2(e23, e23))) & (e21 = op2(e23, e23)) & (e20 = op2(op2(e23, op2(e23, e23)), e23))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax13)).
fof(f549, plain, (op2(e23, op2(e23, e23)) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, op2(e23, e23)) = h4(e12)) & (op2(e23, e23) = h4(e11)) & (op2(op2(e23, op2(e23, e23)), e23) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax17)).
fof(f4998, plain, (~ (op2(e20, e22) = h4(e12)) | (~ spl42_55 | ~ spl42_196 | spl42_236)), inference(forward_demodulation, [], [f4997, f846])).
fof(f846, plain, ((e12 = op1(e10, e12)) | ~ spl42_55), inference(avatar_component_clause, [], [f844])).
fof(f4997, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl42_196 | spl42_236)), inference(forward_demodulation, [], [f1958, f1749])).
fof(f1958, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | spl42_236), inference(avatar_component_clause, [], [f1956])).
fof(f4982, plain, (~ spl42_92 | ~ spl42_256), inference(avatar_split_clause, [], [f4972, f2104, f1033])).
fof(f1033, plain, (spl42_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_92])])).
fof(f2104, plain, (spl42_256 <=> (e23 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_256])])).
fof(f4972, plain, (~ (e23 = op2(e22, e21)) | ~ spl42_256), inference(backward_demodulation, [], [f1698, f2105])).
fof(f2105, plain, ((e23 = h3(e11)) | ~ spl42_256), inference(avatar_component_clause, [], [f2104])).
fof(f1698, plain, ~ (op2(e22, e21) = h3(e11)), inference(backward_demodulation, [], [f291, f544])).
fof(f544, plain, (op2(e22, e22) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, op2(e22, e22)) = h3(e12)) & (op2(e22, e22) = h3(e11)) & (h3(e10) = op2(op2(e22, op2(e22, e22)), e22)) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax16)).
fof(f291, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f4981, plain, (~ spl42_72 | ~ spl42_256), inference(avatar_split_clause, [], [f4971, f2104, f948])).
fof(f948, plain, (spl42_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_72])])).
fof(f4971, plain, (~ (e23 = op2(e23, e22)) | ~ spl42_256), inference(backward_demodulation, [], [f1696, f2105])).
fof(f1696, plain, ~ (op2(e23, e22) = h3(e11)), inference(backward_demodulation, [], [f269, f544])).
fof(f269, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4909, plain, (~ spl42_104 | ~ spl42_40 | ~ spl42_192 | spl42_234), inference(avatar_split_clause, [], [f4894, f1948, f1727, f780, f1084])).
fof(f1084, plain, (spl42_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_104])])).
fof(f780, plain, (spl42_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_40])])).
fof(f1948, plain, (spl42_234 <=> (h4(op1(e11, e12)) = op2(h4(e11), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_234])])).
fof(f4894, plain, (~ (e23 = op2(e21, e22)) | (~ spl42_40 | ~ spl42_192 | spl42_234)), inference(forward_demodulation, [], [f4893, f546])).
fof(f546, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f4893, plain, (~ (op2(e21, e22) = h4(e13)) | (~ spl42_40 | ~ spl42_192 | spl42_234)), inference(forward_demodulation, [], [f4892, f782])).
fof(f782, plain, ((e13 = op1(e11, e12)) | ~ spl42_40), inference(avatar_component_clause, [], [f780])).
fof(f4892, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | (~ spl42_192 | spl42_234)), inference(forward_demodulation, [], [f1950, f1728])).
fof(f1950, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | spl42_234), inference(avatar_component_clause, [], [f1948])).
fof(f4903, plain, (~ spl42_86 | spl42_91 | ~ spl42_181), inference(avatar_contradiction_clause, [], [f4902])).
fof(f4902, plain, ($false | (~ spl42_86 | spl42_91 | ~ spl42_181)), inference(subsumption_resolution, [], [f4901, f1030])).
fof(f1030, plain, (~ (e22 = op2(e22, e21)) | spl42_91), inference(avatar_component_clause, [], [f1029])).
fof(f1029, plain, (spl42_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_91])])).
fof(f4901, plain, ((e22 = op2(e22, e21)) | (~ spl42_86 | ~ spl42_181)), inference(forward_demodulation, [], [f1630, f1010])).
fof(f1010, plain, ((e21 = op2(e22, e22)) | ~ spl42_86), inference(avatar_component_clause, [], [f1008])).
fof(f1008, plain, (spl42_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_86])])).
fof(f1630, plain, ((e22 = op2(e22, op2(e22, e22))) | ~ spl42_181), inference(avatar_component_clause, [], [f1628])).
fof(f1628, plain, (spl42_181 <=> (e22 = op2(e22, op2(e22, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl42_181])])).
fof(f4828, plain, (~ spl42_66 | ~ spl42_182 | ~ spl42_248 | ~ spl42_257), inference(avatar_contradiction_clause, [], [f4827])).
fof(f4827, plain, ($false | (~ spl42_66 | ~ spl42_182 | ~ spl42_248 | ~ spl42_257)), inference(subsumption_resolution, [], [f4826, f310])).
fof(f310, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f4826, plain, ((e21 = e23) | (~ spl42_66 | ~ spl42_182 | ~ spl42_248 | ~ spl42_257)), inference(forward_demodulation, [], [f4817, f925])).
fof(f925, plain, ((e21 = op2(e23, e23)) | ~ spl42_66), inference(avatar_component_clause, [], [f923])).
fof(f923, plain, (spl42_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_66])])).
fof(f4817, plain, ((e23 = op2(e23, e23)) | (~ spl42_182 | ~ spl42_248 | ~ spl42_257)), inference(backward_demodulation, [], [f4784, f2121])).
fof(f2121, plain, ((e23 = h3(e10)) | ~ spl42_257), inference(avatar_component_clause, [], [f2120])).
fof(f2120, plain, (spl42_257 <=> (e23 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_257])])).
fof(f4784, plain, ((e23 = op2(e23, h3(e10))) | (~ spl42_182 | ~ spl42_248)), inference(forward_demodulation, [], [f1634, f3768])).
fof(f3768, plain, ((op2(e23, e22) = h3(e10)) | ~ spl42_248), inference(backward_demodulation, [], [f1702, f2061])).
fof(f2061, plain, ((e23 = h3(e12)) | ~ spl42_248), inference(avatar_component_clause, [], [f2060])).
fof(f2060, plain, (spl42_248 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_248])])).
fof(f1702, plain, (h3(e10) = op2(h3(e12), e22)), inference(forward_demodulation, [], [f1701, f1700])).
fof(f1700, plain, (h3(e12) = op2(e22, h3(e11))), inference(backward_demodulation, [], [f545, f544])).
fof(f545, plain, (op2(e22, op2(e22, e22)) = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f1701, plain, (h3(e10) = op2(op2(e22, h3(e11)), e22)), inference(forward_demodulation, [], [f543, f544])).
fof(f543, plain, (h3(e10) = op2(op2(e22, op2(e22, e22)), e22)), inference(cnf_transformation, [], [f16])).
fof(f1634, plain, ((e23 = op2(e23, op2(e23, e22))) | ~ spl42_182), inference(avatar_component_clause, [], [f1632])).
fof(f1632, plain, (spl42_182 <=> (e23 = op2(e23, op2(e23, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl42_182])])).
fof(f4772, plain, (~ spl42_23 | ~ spl42_31), inference(avatar_split_clause, [], [f4768, f742, f708])).
fof(f708, plain, (spl42_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_23])])).
fof(f742, plain, (spl42_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_31])])).
fof(f4768, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_31), inference(backward_demodulation, [], [f241, f744])).
fof(f744, plain, ((e12 = op1(e12, e10)) | ~ spl42_31), inference(avatar_component_clause, [], [f742])).
fof(f241, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax5)).
fof(f4767, plain, (~ spl42_23 | ~ spl42_55), inference(avatar_split_clause, [], [f4762, f844, f708])).
fof(f4762, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_55), inference(backward_demodulation, [], [f217, f846])).
fof(f217, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4749, plain, (~ spl42_66 | ~ spl42_68), inference(avatar_contradiction_clause, [], [f4748])).
fof(f4748, plain, ($false | (~ spl42_66 | ~ spl42_68)), inference(subsumption_resolution, [], [f4747, f310])).
fof(f4747, plain, ((e21 = e23) | (~ spl42_66 | ~ spl42_68)), inference(forward_demodulation, [], [f933, f925])).
fof(f933, plain, ((e23 = op2(e23, e23)) | ~ spl42_68), inference(avatar_component_clause, [], [f931])).
fof(f931, plain, (spl42_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_68])])).
fof(f4733, plain, (spl42_31 | ~ spl42_17 | ~ spl42_146), inference(avatar_split_clause, [], [f4732, f1387, f683, f742])).
fof(f683, plain, (spl42_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_17])])).
fof(f1387, plain, (spl42_146 <=> (e12 = op1(e12, op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_146])])).
fof(f4732, plain, ((e12 = op1(e12, e10)) | (~ spl42_17 | ~ spl42_146)), inference(forward_demodulation, [], [f1389, f685])).
fof(f685, plain, ((e10 = op1(e12, e13)) | ~ spl42_17), inference(avatar_component_clause, [], [f683])).
fof(f1389, plain, ((e12 = op1(e12, op1(e12, e13))) | ~ spl42_146), inference(avatar_component_clause, [], [f1387])).
fof(f4727, plain, (~ spl42_80 | ~ spl42_16 | ~ spl42_196 | spl42_228), inference(avatar_split_clause, [], [f4726, f1924, f1748, f678, f982])).
fof(f982, plain, (spl42_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_80])])).
fof(f678, plain, (spl42_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_16])])).
fof(f1924, plain, (spl42_228 <=> (h4(op1(e13, e10)) = op2(e23, h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_228])])).
fof(f4726, plain, (~ (e23 = op2(e23, e20)) | (~ spl42_16 | ~ spl42_196 | spl42_228)), inference(forward_demodulation, [], [f4725, f546])).
fof(f4725, plain, (~ (op2(e23, e20) = h4(e13)) | (~ spl42_16 | ~ spl42_196 | spl42_228)), inference(forward_demodulation, [], [f4724, f680])).
fof(f680, plain, ((e13 = op1(e13, e10)) | ~ spl42_16), inference(avatar_component_clause, [], [f678])).
fof(f4724, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | (~ spl42_196 | spl42_228)), inference(forward_demodulation, [], [f1926, f1749])).
fof(f1926, plain, (~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | spl42_228), inference(avatar_component_clause, [], [f1924])).
fof(f4708, plain, (~ spl42_111 | ~ spl42_95), inference(avatar_split_clause, [], [f4707, f1046, f1114])).
fof(f1114, plain, (spl42_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_111])])).
fof(f1046, plain, (spl42_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_95])])).
fof(f4707, plain, (~ (e22 = op2(e21, e20)) | ~ spl42_95), inference(forward_demodulation, [], [f255, f1048])).
fof(f1048, plain, ((e22 = op2(e22, e20)) | ~ spl42_95), inference(avatar_component_clause, [], [f1046])).
fof(f255, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f4704, plain, (~ spl42_97 | ~ spl42_196), inference(avatar_split_clause, [], [f3257, f1748, f1055])).
fof(f1055, plain, (spl42_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_97])])).
fof(f3257, plain, (~ (e20 = op2(e21, e23)) | ~ spl42_196), inference(backward_demodulation, [], [f1715, f1749])).
fof(f1715, plain, ~ (op2(e21, e23) = h4(e10)), inference(backward_demodulation, [], [f273, f1712])).
fof(f1712, plain, (op2(e22, e23) = h4(e10)), inference(forward_demodulation, [], [f1711, f1710])).
fof(f1710, plain, (e22 = op2(e23, h4(e11))), inference(backward_demodulation, [], [f533, f548])).
fof(f548, plain, (op2(e23, e23) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f1711, plain, (h4(e10) = op2(op2(e23, h4(e11)), e23)), inference(forward_demodulation, [], [f547, f548])).
fof(f547, plain, (op2(op2(e23, op2(e23, e23)), e23) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f273, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4695, plain, (~ spl42_2 | ~ spl42_11 | ~ spl42_147), inference(avatar_contradiction_clause, [], [f4694])).
fof(f4694, plain, ($false | (~ spl42_2 | ~ spl42_11 | ~ spl42_147)), inference(subsumption_resolution, [], [f4693, f305])).
fof(f305, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax7)).
fof(f4693, plain, ((e12 = e13) | (~ spl42_2 | ~ spl42_11 | ~ spl42_147)), inference(forward_demodulation, [], [f4692, f659])).
fof(f659, plain, ((e12 = op1(e13, e11)) | ~ spl42_11), inference(avatar_component_clause, [], [f657])).
fof(f657, plain, (spl42_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_11])])).
fof(f4692, plain, ((e13 = op1(e13, e11)) | (~ spl42_2 | ~ spl42_147)), inference(forward_demodulation, [], [f1393, f621])).
fof(f621, plain, ((e11 = op1(e13, e13)) | ~ spl42_2), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl42_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_2])])).
fof(f1393, plain, ((e13 = op1(e13, op1(e13, e13))) | ~ spl42_147), inference(avatar_component_clause, [], [f1391])).
fof(f1391, plain, (spl42_147 <=> (e13 = op1(e13, op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_147])])).
fof(f4677, plain, (~ spl42_35 | spl42_38 | ~ spl42_145), inference(avatar_contradiction_clause, [], [f4676])).
fof(f4676, plain, ($false | (~ spl42_35 | spl42_38 | ~ spl42_145)), inference(subsumption_resolution, [], [f4675, f773])).
fof(f773, plain, (~ (e11 = op1(e11, e12)) | spl42_38), inference(avatar_component_clause, [], [f772])).
fof(f772, plain, (spl42_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_38])])).
fof(f4675, plain, ((e11 = op1(e11, e12)) | (~ spl42_35 | ~ spl42_145)), inference(forward_demodulation, [], [f1385, f761])).
fof(f761, plain, ((e12 = op1(e11, e13)) | ~ spl42_35), inference(avatar_component_clause, [], [f759])).
fof(f759, plain, (spl42_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_35])])).
fof(f1385, plain, ((e11 = op1(e11, op1(e11, e13))) | ~ spl42_145), inference(avatar_component_clause, [], [f1383])).
fof(f1383, plain, (spl42_145 <=> (e11 = op1(e11, op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_145])])).
fof(f4585, plain, (~ spl42_54 | ~ spl42_57 | spl42_148), inference(avatar_contradiction_clause, [], [f4584])).
fof(f4584, plain, ($false | (~ spl42_54 | ~ spl42_57 | spl42_148)), inference(subsumption_resolution, [], [f4580, f855])).
fof(f855, plain, ((e10 = op1(e10, e11)) | ~ spl42_57), inference(avatar_component_clause, [], [f853])).
fof(f853, plain, (spl42_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_57])])).
fof(f4580, plain, (~ (e10 = op1(e10, e11)) | (~ spl42_54 | spl42_148)), inference(backward_demodulation, [], [f1397, f842])).
fof(f842, plain, ((e11 = op1(e10, e12)) | ~ spl42_54), inference(avatar_component_clause, [], [f840])).
fof(f840, plain, (spl42_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_54])])).
fof(f1397, plain, (~ (e10 = op1(e10, op1(e10, e12))) | spl42_148), inference(avatar_component_clause, [], [f1396])).
fof(f1396, plain, (spl42_148 <=> (e10 = op1(e10, op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_148])])).
fof(f4511, plain, (~ spl42_64 | ~ spl42_52), inference(avatar_split_clause, [], [f4510, f831, f882])).
fof(f882, plain, (spl42_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl42_64])])).
fof(f831, plain, (spl42_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_52])])).
fof(f4510, plain, (~ (op1(e10, e10) = e13) | ~ spl42_52), inference(forward_demodulation, [], [f230, f833])).
fof(f833, plain, ((e13 = op1(e10, e13)) | ~ spl42_52), inference(avatar_component_clause, [], [f831])).
fof(f230, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f4484, plain, (~ spl42_87 | spl42_181), inference(avatar_contradiction_clause, [], [f4483])).
fof(f4483, plain, ($false | (~ spl42_87 | spl42_181)), inference(subsumption_resolution, [], [f4476, f1014])).
fof(f1014, plain, ((e22 = op2(e22, e22)) | ~ spl42_87), inference(avatar_component_clause, [], [f1012])).
fof(f1012, plain, (spl42_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_87])])).
fof(f4476, plain, (~ (e22 = op2(e22, e22)) | (~ spl42_87 | spl42_181)), inference(backward_demodulation, [], [f3760, f4472])).
fof(f4472, plain, ((e22 = h3(e11)) | ~ spl42_87), inference(forward_demodulation, [], [f544, f1014])).
fof(f3760, plain, (~ (e22 = op2(e22, h3(e11))) | spl42_181), inference(backward_demodulation, [], [f1629, f544])).
fof(f1629, plain, (~ (e22 = op2(e22, op2(e22, e22))) | spl42_181), inference(avatar_component_clause, [], [f1628])).
fof(f4482, plain, (~ spl42_95 | ~ spl42_87), inference(avatar_split_clause, [], [f4474, f1012, f1046])).
fof(f4474, plain, (~ (e22 = op2(e22, e20)) | ~ spl42_87), inference(backward_demodulation, [], [f1697, f4472])).
fof(f1697, plain, ~ (op2(e22, e20) = h3(e11)), inference(backward_demodulation, [], [f289, f544])).
fof(f289, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f4451, plain, (~ spl42_2 | ~ spl42_4), inference(avatar_contradiction_clause, [], [f4450])).
fof(f4450, plain, ($false | (~ spl42_2 | ~ spl42_4)), inference(subsumption_resolution, [], [f4449, f304])).
fof(f304, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f4449, plain, ((e11 = e13) | (~ spl42_2 | ~ spl42_4)), inference(forward_demodulation, [], [f629, f621])).
fof(f629, plain, ((e13 = op1(e13, e13)) | ~ spl42_4), inference(avatar_component_clause, [], [f627])).
fof(f627, plain, (spl42_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_4])])).
fof(f4425, plain, (~ spl42_45 | ~ spl42_48), inference(avatar_contradiction_clause, [], [f4424])).
fof(f4424, plain, ($false | (~ spl42_45 | ~ spl42_48)), inference(subsumption_resolution, [], [f4423, f302])).
fof(f302, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f4423, plain, ((e10 = e13) | (~ spl42_45 | ~ spl42_48)), inference(backward_demodulation, [], [f816, f804])).
fof(f804, plain, ((e10 = op1(e11, e10)) | ~ spl42_45), inference(avatar_component_clause, [], [f802])).
fof(f802, plain, (spl42_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_45])])).
fof(f816, plain, ((e13 = op1(e11, e10)) | ~ spl42_48), inference(avatar_component_clause, [], [f814])).
fof(f814, plain, (spl42_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_48])])).
fof(f4319, plain, (~ spl42_120 | ~ spl42_116), inference(avatar_split_clause, [], [f4318, f1135, f1152])).
fof(f1152, plain, (spl42_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_120])])).
fof(f4318, plain, (~ (e23 = op2(e20, e22)) | ~ spl42_116), inference(forward_demodulation, [], [f281, f1137])).
fof(f1137, plain, ((e23 = op2(e20, e23)) | ~ spl42_116), inference(avatar_component_clause, [], [f1135])).
fof(f281, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f4308, plain, (spl42_95 | ~ spl42_81 | ~ spl42_177), inference(avatar_split_clause, [], [f3856, f1611, f987, f1046])).
fof(f987, plain, (spl42_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_81])])).
fof(f1611, plain, (spl42_177 <=> (e22 = op2(e22, op2(e22, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl42_177])])).
fof(f3856, plain, ((e22 = op2(e22, e20)) | (~ spl42_81 | ~ spl42_177)), inference(forward_demodulation, [], [f1613, f989])).
fof(f989, plain, ((e20 = op2(e22, e23)) | ~ spl42_81), inference(avatar_component_clause, [], [f987])).
fof(f1613, plain, ((e22 = op2(e22, op2(e22, e23))) | ~ spl42_177), inference(avatar_component_clause, [], [f1611])).
fof(f4300, plain, (~ spl42_22 | spl42_27 | ~ spl42_150), inference(avatar_contradiction_clause, [], [f4299])).
fof(f4299, plain, ($false | (~ spl42_22 | spl42_27 | ~ spl42_150)), inference(subsumption_resolution, [], [f4298, f726])).
fof(f726, plain, (~ (e12 = op1(e12, e11)) | spl42_27), inference(avatar_component_clause, [], [f725])).
fof(f725, plain, (spl42_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_27])])).
fof(f4298, plain, ((e12 = op1(e12, e11)) | (~ spl42_22 | ~ spl42_150)), inference(forward_demodulation, [], [f1406, f706])).
fof(f1406, plain, ((e12 = op1(e12, op1(e12, e12))) | ~ spl42_150), inference(avatar_component_clause, [], [f1404])).
fof(f1404, plain, (spl42_150 <=> (e12 = op1(e12, op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_150])])).
fof(f4292, plain, (~ spl42_37 | spl42_46 | ~ spl42_149), inference(avatar_contradiction_clause, [], [f4291])).
fof(f4291, plain, ($false | (~ spl42_37 | spl42_46 | ~ spl42_149)), inference(subsumption_resolution, [], [f4290, f807])).
fof(f807, plain, (~ (e11 = op1(e11, e10)) | spl42_46), inference(avatar_component_clause, [], [f806])).
fof(f806, plain, (spl42_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_46])])).
fof(f4290, plain, ((e11 = op1(e11, e10)) | (~ spl42_37 | ~ spl42_149)), inference(forward_demodulation, [], [f1402, f770])).
fof(f770, plain, ((e10 = op1(e11, e12)) | ~ spl42_37), inference(avatar_component_clause, [], [f768])).
fof(f768, plain, (spl42_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_37])])).
fof(f1402, plain, ((e11 = op1(e11, op1(e11, e12))) | ~ spl42_149), inference(avatar_component_clause, [], [f1400])).
fof(f1400, plain, (spl42_149 <=> (e11 = op1(e11, op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_149])])).
fof(f4257, plain, (~ spl42_2 | ~ spl42_8 | ~ spl42_151), inference(avatar_contradiction_clause, [], [f4256])).
fof(f4256, plain, ($false | (~ spl42_2 | ~ spl42_8 | ~ spl42_151)), inference(subsumption_resolution, [], [f4255, f304])).
fof(f4255, plain, ((e11 = e13) | (~ spl42_2 | ~ spl42_8 | ~ spl42_151)), inference(forward_demodulation, [], [f4251, f621])).
fof(f4251, plain, ((e13 = op1(e13, e13)) | (~ spl42_8 | ~ spl42_151)), inference(backward_demodulation, [], [f1410, f646])).
fof(f646, plain, ((e13 = op1(e13, e12)) | ~ spl42_8), inference(avatar_component_clause, [], [f644])).
fof(f644, plain, (spl42_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_8])])).
fof(f1410, plain, ((e13 = op1(e13, op1(e13, e12))) | ~ spl42_151), inference(avatar_component_clause, [], [f1408])).
fof(f1408, plain, (spl42_151 <=> (e13 = op1(e13, op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_151])])).
fof(f4205, plain, (~ spl42_38 | ~ spl42_42), inference(avatar_split_clause, [], [f4197, f789, f772])).
fof(f789, plain, (spl42_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_42])])).
fof(f4197, plain, (~ (e11 = op1(e11, e12)) | ~ spl42_42), inference(backward_demodulation, [], [f237, f791])).
fof(f791, plain, ((e11 = op1(e11, e11)) | ~ spl42_42), inference(avatar_component_clause, [], [f789])).
fof(f237, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4162, plain, (~ spl42_39 | ~ spl42_35), inference(avatar_split_clause, [], [f4124, f759, f776])).
fof(f776, plain, (spl42_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_39])])).
fof(f4124, plain, (~ (e12 = op1(e11, e12)) | ~ spl42_35), inference(forward_demodulation, [], [f239, f761])).
fof(f239, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4153, plain, (~ spl42_24 | ~ spl42_66 | ~ spl42_247 | ~ spl42_248), inference(avatar_contradiction_clause, [], [f4152])).
fof(f4152, plain, ($false | (~ spl42_24 | ~ spl42_66 | ~ spl42_247 | ~ spl42_248)), inference(subsumption_resolution, [], [f4151, f309])).
fof(f4151, plain, ((e21 = e22) | (~ spl42_24 | ~ spl42_66 | ~ spl42_247 | ~ spl42_248)), inference(backward_demodulation, [], [f542, f4150])).
fof(f4150, plain, ((e21 = h3(e13)) | (~ spl42_24 | ~ spl42_66 | ~ spl42_247 | ~ spl42_248)), inference(backward_demodulation, [], [f4051, f714])).
fof(f714, plain, ((e13 = op1(e12, e12)) | ~ spl42_24), inference(avatar_component_clause, [], [f712])).
fof(f712, plain, (spl42_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_24])])).
fof(f4051, plain, ((e21 = h3(op1(e12, e12))) | (~ spl42_66 | ~ spl42_247 | ~ spl42_248)), inference(forward_demodulation, [], [f3918, f925])).
fof(f3918, plain, ((op2(e23, e23) = h3(op1(e12, e12))) | (~ spl42_247 | ~ spl42_248)), inference(forward_demodulation, [], [f2057, f2061])).
fof(f2057, plain, ((h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ spl42_247), inference(avatar_component_clause, [], [f2056])).
fof(f2056, plain, (spl42_247 <=> (h3(op1(e12, e12)) = op2(h3(e12), h3(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_247])])).
fof(f542, plain, (e22 = h3(e13)), inference(cnf_transformation, [], [f16])).
fof(f4143, plain, (~ spl42_5 | ~ spl42_11 | ~ spl42_155), inference(avatar_contradiction_clause, [], [f4142])).
fof(f4142, plain, ($false | (~ spl42_5 | ~ spl42_11 | ~ spl42_155)), inference(subsumption_resolution, [], [f4141, f302])).
fof(f4141, plain, ((e10 = e13) | (~ spl42_5 | ~ spl42_11 | ~ spl42_155)), inference(forward_demodulation, [], [f4140, f634])).
fof(f634, plain, ((e10 = op1(e13, e12)) | ~ spl42_5), inference(avatar_component_clause, [], [f632])).
fof(f632, plain, (spl42_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_5])])).
fof(f4140, plain, ((e13 = op1(e13, e12)) | (~ spl42_11 | ~ spl42_155)), inference(forward_demodulation, [], [f1427, f659])).
fof(f1427, plain, ((e13 = op1(e13, op1(e13, e11))) | ~ spl42_155), inference(avatar_component_clause, [], [f1425])).
fof(f1425, plain, (spl42_155 <=> (e13 = op1(e13, op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_155])])).
fof(f4106, plain, (~ spl42_26 | ~ spl42_154), inference(avatar_contradiction_clause, [], [f4105])).
fof(f4105, plain, ($false | (~ spl42_26 | ~ spl42_154)), inference(subsumption_resolution, [], [f4104, f303])).
fof(f303, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f4104, plain, ((e11 = e12) | (~ spl42_26 | ~ spl42_154)), inference(forward_demodulation, [], [f4100, f723])).
fof(f723, plain, ((e11 = op1(e12, e11)) | ~ spl42_26), inference(avatar_component_clause, [], [f721])).
fof(f721, plain, (spl42_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_26])])).
fof(f4100, plain, ((e12 = op1(e12, e11)) | (~ spl42_26 | ~ spl42_154)), inference(backward_demodulation, [], [f1423, f723])).
fof(f1423, plain, ((e12 = op1(e12, op1(e12, e11))) | ~ spl42_154), inference(avatar_component_clause, [], [f1421])).
fof(f1421, plain, (spl42_154 <=> (e12 = op1(e12, op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_154])])).
fof(f4096, plain, (~ spl42_31 | ~ spl42_32), inference(avatar_contradiction_clause, [], [f4095])).
fof(f4095, plain, ($false | (~ spl42_31 | ~ spl42_32)), inference(subsumption_resolution, [], [f4094, f305])).
fof(f4094, plain, ((e12 = e13) | (~ spl42_31 | ~ spl42_32)), inference(forward_demodulation, [], [f748, f744])).
fof(f748, plain, ((e13 = op1(e12, e10)) | ~ spl42_32), inference(avatar_component_clause, [], [f746])).
fof(f746, plain, (spl42_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_32])])).
fof(f4050, plain, (~ spl42_31 | ~ spl42_95 | ~ spl42_196 | spl42_232), inference(avatar_contradiction_clause, [], [f4049])).
fof(f4049, plain, ($false | (~ spl42_31 | ~ spl42_95 | ~ spl42_196 | spl42_232)), inference(subsumption_resolution, [], [f4048, f1048])).
fof(f4048, plain, (~ (e22 = op2(e22, e20)) | (~ spl42_31 | ~ spl42_196 | spl42_232)), inference(forward_demodulation, [], [f4047, f1703])).
fof(f4047, plain, (~ (op2(e22, e20) = h4(e12)) | (~ spl42_31 | ~ spl42_196 | spl42_232)), inference(forward_demodulation, [], [f4046, f744])).
fof(f4046, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl42_196 | spl42_232)), inference(forward_demodulation, [], [f1942, f1749])).
fof(f1942, plain, (~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | spl42_232), inference(avatar_component_clause, [], [f1940])).
fof(f1940, plain, (spl42_232 <=> (h4(op1(e12, e10)) = op2(e22, h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_232])])).
fof(f4017, plain, (~ spl42_52 | ~ spl42_116 | ~ spl42_196 | spl42_235), inference(avatar_contradiction_clause, [], [f4016])).
fof(f4016, plain, ($false | (~ spl42_52 | ~ spl42_116 | ~ spl42_196 | spl42_235)), inference(subsumption_resolution, [], [f4015, f1137])).
fof(f4015, plain, (~ (e23 = op2(e20, e23)) | (~ spl42_52 | ~ spl42_196 | spl42_235)), inference(forward_demodulation, [], [f4014, f546])).
fof(f4014, plain, (~ (op2(e20, e23) = h4(e13)) | (~ spl42_52 | ~ spl42_196 | spl42_235)), inference(forward_demodulation, [], [f4013, f833])).
fof(f4013, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | (~ spl42_196 | spl42_235)), inference(forward_demodulation, [], [f1954, f1749])).
fof(f1954, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | spl42_235), inference(avatar_component_clause, [], [f1952])).
fof(f1952, plain, (spl42_235 <=> (h4(op1(e10, e13)) = op2(h4(e10), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_235])])).
fof(f4005, plain, (~ spl42_28 | ~ spl42_92 | ~ spl42_192 | spl42_231), inference(avatar_contradiction_clause, [], [f4004])).
fof(f4004, plain, ($false | (~ spl42_28 | ~ spl42_92 | ~ spl42_192 | spl42_231)), inference(subsumption_resolution, [], [f4003, f1035])).
fof(f1035, plain, ((e23 = op2(e22, e21)) | ~ spl42_92), inference(avatar_component_clause, [], [f1033])).
fof(f4003, plain, (~ (e23 = op2(e22, e21)) | (~ spl42_28 | ~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f4002, f546])).
fof(f4002, plain, (~ (op2(e22, e21) = h4(e13)) | (~ spl42_28 | ~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f4001, f731])).
fof(f731, plain, ((e13 = op1(e12, e11)) | ~ spl42_28), inference(avatar_component_clause, [], [f729])).
fof(f729, plain, (spl42_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_28])])).
fof(f4001, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | (~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f1938, f1728])).
fof(f1938, plain, (~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | spl42_231), inference(avatar_component_clause, [], [f1936])).
fof(f1936, plain, (spl42_231 <=> (h4(op1(e12, e11)) = op2(e22, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_231])])).
fof(f3984, plain, (~ spl42_35 | ~ spl42_99 | ~ spl42_192 | spl42_233), inference(avatar_contradiction_clause, [], [f3983])).
fof(f3983, plain, ($false | (~ spl42_35 | ~ spl42_99 | ~ spl42_192 | spl42_233)), inference(subsumption_resolution, [], [f3982, f1065])).
fof(f1065, plain, ((e22 = op2(e21, e23)) | ~ spl42_99), inference(avatar_component_clause, [], [f1063])).
fof(f3982, plain, (~ (e22 = op2(e21, e23)) | (~ spl42_35 | ~ spl42_192 | spl42_233)), inference(forward_demodulation, [], [f3981, f1703])).
fof(f3981, plain, (~ (op2(e21, e23) = h4(e12)) | (~ spl42_35 | ~ spl42_192 | spl42_233)), inference(forward_demodulation, [], [f3980, f761])).
fof(f3980, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | (~ spl42_192 | spl42_233)), inference(forward_demodulation, [], [f1946, f1728])).
fof(f1946, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | spl42_233), inference(avatar_component_clause, [], [f1944])).
fof(f1944, plain, (spl42_233 <=> (h4(op1(e11, e13)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_233])])).
fof(f3965, plain, (~ spl42_42 | ~ spl42_106 | ~ spl42_192 | spl42_224), inference(avatar_contradiction_clause, [], [f3964])).
fof(f3964, plain, ($false | (~ spl42_42 | ~ spl42_106 | ~ spl42_192 | spl42_224)), inference(subsumption_resolution, [], [f3963, f1095])).
fof(f1095, plain, ((e21 = op2(e21, e21)) | ~ spl42_106), inference(avatar_component_clause, [], [f1093])).
fof(f1093, plain, (spl42_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_106])])).
fof(f3963, plain, (~ (e21 = op2(e21, e21)) | (~ spl42_42 | ~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f3962, f1728])).
fof(f3962, plain, (~ (op2(e21, e21) = h4(e11)) | (~ spl42_42 | ~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f3961, f791])).
fof(f3961, plain, (~ (op2(e21, e21) = h4(op1(e11, e11))) | (~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f1910, f1728])).
fof(f1910, plain, (~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | spl42_224), inference(avatar_component_clause, [], [f1908])).
fof(f1908, plain, (spl42_224 <=> (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_224])])).
fof(f3906, plain, (~ spl42_11 | spl42_227), inference(avatar_contradiction_clause, [], [f3905])).
fof(f3905, plain, ($false | (~ spl42_11 | spl42_227)), inference(subsumption_resolution, [], [f3904, f1703])).
fof(f3904, plain, (~ (e22 = h4(e12)) | (~ spl42_11 | spl42_227)), inference(forward_demodulation, [], [f1922, f659])).
fof(f1922, plain, (~ (e22 = h4(op1(e13, e11))) | spl42_227), inference(avatar_component_clause, [], [f1920])).
fof(f1920, plain, (spl42_227 <=> (e22 = h4(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_227])])).
fof(f3899, plain, (~ spl42_22 | ~ spl42_66 | ~ spl42_199 | spl42_247 | ~ spl42_248), inference(avatar_contradiction_clause, [], [f3898])).
fof(f3898, plain, ($false | (~ spl42_22 | ~ spl42_66 | ~ spl42_199 | spl42_247 | ~ spl42_248)), inference(subsumption_resolution, [], [f3897, f925])).
fof(f3897, plain, (~ (e21 = op2(e23, e23)) | (~ spl42_22 | ~ spl42_199 | spl42_247 | ~ spl42_248)), inference(forward_demodulation, [], [f3896, f1765])).
fof(f1765, plain, ((e21 = h3(e11)) | ~ spl42_199), inference(avatar_component_clause, [], [f1764])).
fof(f3896, plain, (~ (op2(e23, e23) = h3(e11)) | (~ spl42_22 | spl42_247 | ~ spl42_248)), inference(forward_demodulation, [], [f3895, f706])).
fof(f3895, plain, (~ (op2(e23, e23) = h3(op1(e12, e12))) | (spl42_247 | ~ spl42_248)), inference(forward_demodulation, [], [f2058, f2061])).
fof(f2058, plain, (~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | spl42_247), inference(avatar_component_clause, [], [f2056])).
fof(f3885, plain, (~ spl42_2 | spl42_225), inference(avatar_contradiction_clause, [], [f3884])).
fof(f3884, plain, ($false | (~ spl42_2 | spl42_225)), inference(trivial_inequality_removal, [], [f3883])).
fof(f3883, plain, (~ (h4(e11) = h4(e11)) | (~ spl42_2 | spl42_225)), inference(forward_demodulation, [], [f1914, f621])).
fof(f1914, plain, (~ (h4(e11) = h4(op1(e13, e13))) | spl42_225), inference(avatar_component_clause, [], [f1912])).
fof(f1912, plain, (spl42_225 <=> (h4(e11) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_225])])).
fof(f3817, plain, (~ spl42_102 | ~ spl42_106), inference(avatar_split_clause, [], [f3809, f1093, f1076])).
fof(f3809, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_106), inference(backward_demodulation, [], [f1689, f3806])).
fof(f3806, plain, ((e21 = h2(e11)) | ~ spl42_106), inference(backward_demodulation, [], [f540, f1095])).
fof(f540, plain, (op2(e21, e21) = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, op2(e21, e21)) = h2(e12)) & (op2(e21, e21) = h2(e11)) & (h2(e10) = op2(op2(e21, op2(e21, e21)), e21)) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax15)).
fof(f1689, plain, ~ (op2(e21, e22) = h2(e11)), inference(backward_demodulation, [], [f285, f540])).
fof(f285, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3816, plain, (~ spl42_90 | ~ spl42_106), inference(avatar_split_clause, [], [f3808, f1093, f1025])).
fof(f1025, plain, (spl42_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_90])])).
fof(f3808, plain, (~ (e21 = op2(e22, e21)) | ~ spl42_106), inference(backward_demodulation, [], [f1686, f3806])).
fof(f1686, plain, ~ (op2(e22, e21) = h2(e11)), inference(backward_demodulation, [], [f261, f540])).
fof(f261, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3802, plain, (~ spl42_66 | ~ spl42_75 | ~ spl42_178), inference(avatar_contradiction_clause, [], [f3801])).
fof(f3801, plain, ($false | (~ spl42_66 | ~ spl42_75 | ~ spl42_178)), inference(subsumption_resolution, [], [f3800, f311])).
fof(f311, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f3800, plain, ((e22 = e23) | (~ spl42_66 | ~ spl42_75 | ~ spl42_178)), inference(forward_demodulation, [], [f3799, f963])).
fof(f963, plain, ((e22 = op2(e23, e21)) | ~ spl42_75), inference(avatar_component_clause, [], [f961])).
fof(f961, plain, (spl42_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_75])])).
fof(f3799, plain, ((e23 = op2(e23, e21)) | (~ spl42_66 | ~ spl42_178)), inference(forward_demodulation, [], [f1617, f925])).
fof(f1617, plain, ((e23 = op2(e23, op2(e23, e23))) | ~ spl42_178), inference(avatar_component_clause, [], [f1615])).
fof(f1615, plain, (spl42_178 <=> (e23 = op2(e23, op2(e23, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl42_178])])).
fof(f3794, plain, (~ spl42_90 | ~ spl42_199), inference(avatar_split_clause, [], [f3786, f1764, f1025])).
fof(f3786, plain, (~ (e21 = op2(e22, e21)) | ~ spl42_199), inference(backward_demodulation, [], [f1698, f1765])).
fof(f3773, plain, (~ spl42_17 | spl42_229), inference(avatar_contradiction_clause, [], [f3772])).
fof(f3772, plain, ($false | (~ spl42_17 | spl42_229)), inference(trivial_inequality_removal, [], [f3771])).
fof(f3771, plain, (~ (h4(e10) = h4(e10)) | (~ spl42_17 | spl42_229)), inference(forward_demodulation, [], [f1930, f685])).
fof(f1930, plain, (~ (h4(e10) = h4(op1(e12, e13))) | spl42_229), inference(avatar_component_clause, [], [f1928])).
fof(f1928, plain, (spl42_229 <=> (h4(e10) = h4(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_229])])).
fof(f3751, plain, (spl42_92 | ~ spl42_206 | ~ spl42_276), inference(avatar_split_clause, [], [f3741, f2234, f1799, f1033])).
fof(f1799, plain, (spl42_206 <=> (e22 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_206])])).
fof(f2234, plain, (spl42_276 <=> (e23 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_276])])).
fof(f3741, plain, ((e23 = op2(e22, e21)) | (~ spl42_206 | ~ spl42_276)), inference(backward_demodulation, [], [f3700, f1800])).
fof(f1800, plain, ((e22 = h2(e12)) | ~ spl42_206), inference(avatar_component_clause, [], [f1799])).
fof(f3700, plain, ((e23 = op2(h2(e12), e21)) | ~ spl42_276), inference(forward_demodulation, [], [f1693, f2235])).
fof(f2235, plain, ((e23 = h2(e10)) | ~ spl42_276), inference(avatar_component_clause, [], [f2234])).
fof(f1693, plain, (h2(e10) = op2(h2(e12), e21)), inference(forward_demodulation, [], [f1692, f1691])).
fof(f1691, plain, (h2(e12) = op2(e21, h2(e11))), inference(backward_demodulation, [], [f541, f540])).
fof(f541, plain, (op2(e21, op2(e21, e21)) = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f1692, plain, (h2(e10) = op2(op2(e21, h2(e11)), e21)), inference(forward_demodulation, [], [f539, f540])).
fof(f539, plain, (h2(e10) = op2(op2(e21, op2(e21, e21)), e21)), inference(cnf_transformation, [], [f15])).
fof(f3748, plain, (~ spl42_69 | ~ spl42_75 | ~ spl42_186), inference(avatar_contradiction_clause, [], [f3747])).
fof(f3747, plain, ($false | (~ spl42_69 | ~ spl42_75 | ~ spl42_186)), inference(subsumption_resolution, [], [f3746, f308])).
fof(f308, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f3746, plain, ((e20 = e23) | (~ spl42_69 | ~ spl42_75 | ~ spl42_186)), inference(forward_demodulation, [], [f3745, f938])).
fof(f938, plain, ((e20 = op2(e23, e22)) | ~ spl42_69), inference(avatar_component_clause, [], [f936])).
fof(f936, plain, (spl42_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_69])])).
fof(f3745, plain, ((e23 = op2(e23, e22)) | (~ spl42_75 | ~ spl42_186)), inference(forward_demodulation, [], [f1651, f963])).
fof(f1651, plain, ((e23 = op2(e23, op2(e23, e21))) | ~ spl42_186), inference(avatar_component_clause, [], [f1649])).
fof(f1649, plain, (spl42_186 <=> (e23 = op2(e23, op2(e23, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl42_186])])).
fof(f3740, plain, (~ spl42_57 | ~ spl42_121 | ~ spl42_192 | ~ spl42_196 | spl42_222), inference(avatar_contradiction_clause, [], [f3739])).
fof(f3739, plain, ($false | (~ spl42_57 | ~ spl42_121 | ~ spl42_192 | ~ spl42_196 | spl42_222)), inference(subsumption_resolution, [], [f3738, f1159])).
fof(f1159, plain, ((e20 = op2(e20, e21)) | ~ spl42_121), inference(avatar_component_clause, [], [f1157])).
fof(f1157, plain, (spl42_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_121])])).
fof(f3738, plain, (~ (e20 = op2(e20, e21)) | (~ spl42_57 | ~ spl42_192 | ~ spl42_196 | spl42_222)), inference(forward_demodulation, [], [f3737, f1749])).
fof(f3737, plain, (~ (op2(e20, e21) = h4(e10)) | (~ spl42_57 | ~ spl42_192 | ~ spl42_196 | spl42_222)), inference(forward_demodulation, [], [f3736, f855])).
fof(f3736, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | (~ spl42_192 | ~ spl42_196 | spl42_222)), inference(forward_demodulation, [], [f3735, f1749])).
fof(f3735, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e21)) | (~ spl42_192 | spl42_222)), inference(forward_demodulation, [], [f1902, f1728])).
fof(f1902, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | spl42_222), inference(avatar_component_clause, [], [f1900])).
fof(f1900, plain, (spl42_222 <=> (h4(op1(e10, e11)) = op2(h4(e10), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_222])])).
fof(f3723, plain, (~ spl42_90 | ~ spl42_185), inference(avatar_contradiction_clause, [], [f3722])).
fof(f3722, plain, ($false | (~ spl42_90 | ~ spl42_185)), inference(subsumption_resolution, [], [f3721, f309])).
fof(f3721, plain, ((e21 = e22) | (~ spl42_90 | ~ spl42_185)), inference(forward_demodulation, [], [f3720, f1027])).
fof(f1027, plain, ((e21 = op2(e22, e21)) | ~ spl42_90), inference(avatar_component_clause, [], [f1025])).
fof(f3720, plain, ((e22 = op2(e22, e21)) | (~ spl42_90 | ~ spl42_185)), inference(forward_demodulation, [], [f1647, f1027])).
fof(f1647, plain, ((e22 = op2(e22, op2(e22, e21))) | ~ spl42_185), inference(avatar_component_clause, [], [f1645])).
fof(f1645, plain, (spl42_185 <=> (e22 = op2(e22, op2(e22, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl42_185])])).
fof(f3719, plain, (~ spl42_45 | ~ spl42_109 | ~ spl42_192 | ~ spl42_196 | spl42_223), inference(avatar_contradiction_clause, [], [f3718])).
fof(f3718, plain, ($false | (~ spl42_45 | ~ spl42_109 | ~ spl42_192 | ~ spl42_196 | spl42_223)), inference(subsumption_resolution, [], [f3717, f1108])).
fof(f1108, plain, ((e20 = op2(e21, e20)) | ~ spl42_109), inference(avatar_component_clause, [], [f1106])).
fof(f1106, plain, (spl42_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_109])])).
fof(f3717, plain, (~ (e20 = op2(e21, e20)) | (~ spl42_45 | ~ spl42_192 | ~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f3716, f1749])).
fof(f3716, plain, (~ (op2(e21, e20) = h4(e10)) | (~ spl42_45 | ~ spl42_192 | ~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f3715, f804])).
fof(f3715, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | (~ spl42_192 | ~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f3714, f1728])).
fof(f3714, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | (~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f1906, f1749])).
fof(f1906, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | spl42_223), inference(avatar_component_clause, [], [f1904])).
fof(f1904, plain, (spl42_223 <=> (h4(op1(e11, e10)) = op2(h4(e11), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_223])])).
fof(f3703, plain, (spl42_206 | ~ spl42_99 | ~ spl42_275), inference(avatar_split_clause, [], [f3702, f2218, f1063, f1799])).
fof(f2218, plain, (spl42_275 <=> (e23 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_275])])).
fof(f3702, plain, ((e22 = h2(e12)) | (~ spl42_99 | ~ spl42_275)), inference(forward_demodulation, [], [f3701, f1065])).
fof(f3701, plain, ((op2(e21, e23) = h2(e12)) | ~ spl42_275), inference(forward_demodulation, [], [f1691, f2219])).
fof(f2219, plain, ((e23 = h2(e11)) | ~ spl42_275), inference(avatar_component_clause, [], [f2218])).
fof(f3664, plain, (~ spl42_5 | ~ spl42_69 | ~ spl42_196 | spl42_226), inference(avatar_contradiction_clause, [], [f3663])).
fof(f3663, plain, ($false | (~ spl42_5 | ~ spl42_69 | ~ spl42_196 | spl42_226)), inference(subsumption_resolution, [], [f3662, f938])).
fof(f3662, plain, (~ (e20 = op2(e23, e22)) | (~ spl42_5 | ~ spl42_196 | spl42_226)), inference(forward_demodulation, [], [f3661, f1749])).
fof(f3661, plain, (~ (op2(e23, e22) = h4(e10)) | (~ spl42_5 | spl42_226)), inference(forward_demodulation, [], [f1918, f634])).
fof(f1918, plain, (~ (op2(e23, e22) = h4(op1(e13, e12))) | spl42_226), inference(avatar_component_clause, [], [f1916])).
fof(f1916, plain, (spl42_226 <=> (op2(e23, e22) = h4(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_226])])).
fof(f3646, plain, (~ spl42_109 | ~ spl42_110), inference(avatar_contradiction_clause, [], [f3645])).
fof(f3645, plain, ($false | (~ spl42_109 | ~ spl42_110)), inference(subsumption_resolution, [], [f3644, f306])).
fof(f306, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f3644, plain, ((e20 = e21) | (~ spl42_109 | ~ spl42_110)), inference(forward_demodulation, [], [f1112, f1108])).
fof(f1112, plain, ((e21 = op2(e21, e20)) | ~ spl42_110), inference(avatar_component_clause, [], [f1110])).
fof(f1110, plain, (spl42_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_110])])).
fof(f3643, plain, (~ spl42_118 | ~ spl42_119), inference(avatar_contradiction_clause, [], [f3642])).
fof(f3642, plain, ($false | (~ spl42_118 | ~ spl42_119)), inference(subsumption_resolution, [], [f3641, f309])).
fof(f3641, plain, ((e21 = e22) | (~ spl42_118 | ~ spl42_119)), inference(backward_demodulation, [], [f1150, f1146])).
fof(f1150, plain, ((e22 = op2(e20, e22)) | ~ spl42_119), inference(avatar_component_clause, [], [f1148])).
fof(f3632, plain, (spl42_276 | ~ spl42_184 | ~ spl42_275), inference(avatar_split_clause, [], [f3631, f2218, f1641, f2234])).
fof(f1641, plain, (spl42_184 <=> (e21 = op2(e21, op2(e21, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl42_184])])).
fof(f3631, plain, ((e23 = h2(e10)) | (~ spl42_184 | ~ spl42_275)), inference(backward_demodulation, [], [f3525, f2219])).
fof(f3525, plain, ((h2(e10) = h2(e11)) | ~ spl42_184), inference(backward_demodulation, [], [f540, f3524])).
fof(f3524, plain, ((op2(e21, e21) = h2(e10)) | ~ spl42_184), inference(backward_demodulation, [], [f1693, f3523])).
fof(f3523, plain, ((e21 = h2(e12)) | ~ spl42_184), inference(backward_demodulation, [], [f1691, f3522])).
fof(f3522, plain, ((e21 = op2(e21, h2(e11))) | ~ spl42_184), inference(backward_demodulation, [], [f1643, f540])).
fof(f1643, plain, ((e21 = op2(e21, op2(e21, e21))) | ~ spl42_184), inference(avatar_component_clause, [], [f1641])).
fof(f3626, plain, (~ spl42_128 | spl42_294), inference(avatar_contradiction_clause, [], [f3625])).
fof(f3625, plain, ($false | (~ spl42_128 | spl42_294)), inference(subsumption_resolution, [], [f3624, f2334])).
fof(f2334, plain, (~ (e23 = h1(e11)) | spl42_294), inference(avatar_component_clause, [], [f2332])).
fof(f2332, plain, (spl42_294 <=> (e23 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_294])])).
fof(f3624, plain, ((e23 = h1(e11)) | ~ spl42_128), inference(backward_demodulation, [], [f536, f1188])).
fof(f1188, plain, ((op2(e20, e20) = e23) | ~ spl42_128), inference(avatar_component_clause, [], [f1186])).
fof(f1186, plain, (spl42_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl42_128])])).
fof(f536, plain, (op2(e20, e20) = h1(e11)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, op2(e20, e20)) = h1(e12)) & (op2(e20, e20) = h1(e11)) & (h1(e10) = op2(op2(e20, op2(e20, e20)), e20)) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax14)).
fof(f3603, plain, (~ spl42_36 | ~ spl42_52), inference(avatar_split_clause, [], [f3602, f831, f763])).
fof(f763, plain, (spl42_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_36])])).
fof(f3602, plain, (~ (e13 = op1(e11, e13)) | ~ spl42_52), inference(forward_demodulation, [], [f222, f833])).
fof(f222, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3581, plain, (~ spl42_81 | ~ spl42_88 | ~ spl42_181), inference(avatar_contradiction_clause, [], [f3580])).
fof(f3580, plain, ($false | (~ spl42_81 | ~ spl42_88 | ~ spl42_181)), inference(subsumption_resolution, [], [f3579, f307])).
fof(f307, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f3579, plain, ((e20 = e22) | (~ spl42_81 | ~ spl42_88 | ~ spl42_181)), inference(forward_demodulation, [], [f3578, f989])).
fof(f3578, plain, ((e22 = op2(e22, e23)) | (~ spl42_88 | ~ spl42_181)), inference(backward_demodulation, [], [f1630, f1018])).
fof(f1018, plain, ((e23 = op2(e22, e22)) | ~ spl42_88), inference(avatar_component_clause, [], [f1016])).
fof(f1016, plain, (spl42_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_88])])).
fof(f3577, plain, (~ spl42_90 | ~ spl42_92), inference(avatar_contradiction_clause, [], [f3576])).
fof(f3576, plain, ($false | (~ spl42_90 | ~ spl42_92)), inference(subsumption_resolution, [], [f3574, f310])).
fof(f3574, plain, ((e21 = e23) | (~ spl42_90 | ~ spl42_92)), inference(backward_demodulation, [], [f1035, f1027])).
fof(f3507, plain, (~ spl42_39 | ~ spl42_55), inference(avatar_split_clause, [], [f3506, f844, f776])).
fof(f3506, plain, (~ (e12 = op1(e11, e12)) | ~ spl42_55), inference(forward_demodulation, [], [f216, f846])).
fof(f216, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f3463, plain, (~ spl42_55 | ~ spl42_56), inference(avatar_contradiction_clause, [], [f3462])).
fof(f3462, plain, ($false | (~ spl42_55 | ~ spl42_56)), inference(subsumption_resolution, [], [f3461, f305])).
fof(f3461, plain, ((e12 = e13) | (~ spl42_55 | ~ spl42_56)), inference(backward_demodulation, [], [f850, f846])).
fof(f850, plain, ((e13 = op1(e10, e12)) | ~ spl42_56), inference(avatar_component_clause, [], [f848])).
fof(f848, plain, (spl42_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_56])])).
fof(f3456, plain, (~ spl42_41 | ~ spl42_57), inference(avatar_split_clause, [], [f3449, f853, f785])).
fof(f785, plain, (spl42_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_41])])).
fof(f3449, plain, (~ (e10 = op1(e11, e11)) | ~ spl42_57), inference(backward_demodulation, [], [f210, f855])).
fof(f210, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f3444, plain, (~ spl42_46 | ~ spl42_62), inference(avatar_split_clause, [], [f3435, f874, f806])).
fof(f3435, plain, (~ (e11 = op1(e11, e10)) | ~ spl42_62), inference(backward_demodulation, [], [f204, f876])).
fof(f204, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f3423, plain, (spl42_102 | ~ spl42_99 | ~ spl42_176), inference(avatar_split_clause, [], [f3422, f1607, f1063, f1076])).
fof(f1607, plain, (spl42_176 <=> (e21 = op2(e21, op2(e21, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl42_176])])).
fof(f3422, plain, ((e21 = op2(e21, e22)) | (~ spl42_99 | ~ spl42_176)), inference(backward_demodulation, [], [f1609, f1065])).
fof(f1609, plain, ((e21 = op2(e21, op2(e21, e23))) | ~ spl42_176), inference(avatar_component_clause, [], [f1607])).
fof(f3411, plain, (~ spl42_101 | ~ spl42_109), inference(avatar_split_clause, [], [f3403, f1106, f1072])).
fof(f1072, plain, (spl42_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_101])])).
fof(f3403, plain, (~ (e20 = op2(e21, e22)) | ~ spl42_109), inference(backward_demodulation, [], [f283, f1108])).
fof(f283, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3410, plain, (~ spl42_77 | ~ spl42_109), inference(avatar_split_clause, [], [f3402, f1106, f970])).
fof(f970, plain, (spl42_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_77])])).
fof(f3402, plain, (~ (e20 = op2(e23, e20)) | ~ spl42_109), inference(backward_demodulation, [], [f256, f1108])).
fof(f256, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3400, plain, (~ spl42_294 | ~ spl42_116), inference(avatar_split_clause, [], [f3398, f1135, f2332])).
fof(f3398, plain, (~ (e23 = h1(e11)) | ~ spl42_116), inference(backward_demodulation, [], [f1681, f1137])).
fof(f1681, plain, ~ (op2(e20, e23) = h1(e11)), inference(backward_demodulation, [], [f278, f536])).
fof(f278, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3382, plain, (spl42_257 | ~ spl42_72 | ~ spl42_248), inference(avatar_split_clause, [], [f3381, f2060, f948, f2120])).
fof(f3381, plain, ((e23 = h3(e10)) | (~ spl42_72 | ~ spl42_248)), inference(forward_demodulation, [], [f3379, f950])).
fof(f950, plain, ((e23 = op2(e23, e22)) | ~ spl42_72), inference(avatar_component_clause, [], [f948])).
fof(f3379, plain, ((op2(e23, e22) = h3(e10)) | ~ spl42_248), inference(backward_demodulation, [], [f1702, f2061])).
fof(f3377, plain, (spl42_215 | ~ spl42_127), inference(avatar_split_clause, [], [f3376, f1182, f1845])).
fof(f1845, plain, (spl42_215 <=> (e22 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_215])])).
fof(f1182, plain, (spl42_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl42_127])])).
fof(f3376, plain, ((e22 = h1(e11)) | ~ spl42_127), inference(forward_demodulation, [], [f536, f1184])).
fof(f1184, plain, ((op2(e20, e20) = e22) | ~ spl42_127), inference(avatar_component_clause, [], [f1182])).
fof(f3375, plain, (~ spl42_215 | ~ spl42_95), inference(avatar_split_clause, [], [f3374, f1046, f1845])).
fof(f3374, plain, (~ (e22 = h1(e11)) | ~ spl42_95), inference(forward_demodulation, [], [f1677, f1048])).
fof(f1677, plain, ~ (op2(e22, e20) = h1(e11)), inference(backward_demodulation, [], [f253, f536])).
fof(f253, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f3356, plain, (~ spl42_78 | ~ spl42_192), inference(avatar_split_clause, [], [f2895, f1727, f974])).
fof(f974, plain, (spl42_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_78])])).
fof(f2895, plain, (~ (e21 = op2(e23, e20)) | ~ spl42_192), inference(forward_demodulation, [], [f1707, f1728])).
fof(f1707, plain, ~ (op2(e23, e20) = h4(e11)), inference(backward_demodulation, [], [f296, f548])).
fof(f296, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3354, plain, (~ spl42_79 | ~ spl42_75), inference(avatar_split_clause, [], [f3087, f961, f978])).
fof(f978, plain, (spl42_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_79])])).
fof(f3087, plain, (~ (e22 = op2(e23, e20)) | ~ spl42_75), inference(forward_demodulation, [], [f294, f963])).
fof(f294, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3343, plain, (~ spl42_47 | ~ spl42_31), inference(avatar_split_clause, [], [f3342, f742, f810])).
fof(f810, plain, (spl42_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_47])])).
fof(f3342, plain, (~ (e12 = op1(e11, e10)) | ~ spl42_31), inference(forward_demodulation, [], [f207, f744])).
fof(f207, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f3334, plain, (~ spl42_27 | ~ spl42_11), inference(avatar_split_clause, [], [f3221, f657, f725])).
fof(f3221, plain, (~ (e12 = op1(e12, e11)) | ~ spl42_11), inference(forward_demodulation, [], [f215, f659])).
fof(f215, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3319, plain, (~ spl42_38 | ~ spl42_39), inference(avatar_contradiction_clause, [], [f3318])).
fof(f3318, plain, ($false | (~ spl42_38 | ~ spl42_39)), inference(subsumption_resolution, [], [f3317, f303])).
fof(f3317, plain, ((e11 = e12) | (~ spl42_38 | ~ spl42_39)), inference(forward_demodulation, [], [f778, f774])).
fof(f774, plain, ((e11 = op1(e11, e12)) | ~ spl42_38), inference(avatar_component_clause, [], [f772])).
fof(f778, plain, ((e12 = op1(e11, e12)) | ~ spl42_39), inference(avatar_component_clause, [], [f776])).
fof(f3308, plain, (~ spl42_35 | ~ spl42_51), inference(avatar_split_clause, [], [f3307, f827, f759])).
fof(f827, plain, (spl42_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_51])])).
fof(f3307, plain, (~ (e12 = op1(e11, e13)) | ~ spl42_51), inference(backward_demodulation, [], [f222, f829])).
fof(f829, plain, ((e12 = op1(e10, e13)) | ~ spl42_51), inference(avatar_component_clause, [], [f827])).
fof(f3303, plain, (~ spl42_52 | ~ spl42_60), inference(avatar_split_clause, [], [f3301, f865, f831])).
fof(f865, plain, (spl42_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_60])])).
fof(f3301, plain, (~ (e13 = op1(e10, e13)) | ~ spl42_60), inference(backward_demodulation, [], [f232, f867])).
fof(f867, plain, ((e13 = op1(e10, e11)) | ~ spl42_60), inference(avatar_component_clause, [], [f865])).
fof(f232, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3298, plain, (~ spl42_66 | ~ spl42_67), inference(avatar_contradiction_clause, [], [f3297])).
fof(f3297, plain, ($false | (~ spl42_66 | ~ spl42_67)), inference(subsumption_resolution, [], [f3296, f309])).
fof(f3296, plain, ((e21 = e22) | (~ spl42_66 | ~ spl42_67)), inference(forward_demodulation, [], [f929, f925])).
fof(f929, plain, ((e22 = op2(e23, e23)) | ~ spl42_67), inference(avatar_component_clause, [], [f927])).
fof(f927, plain, (spl42_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_67])])).
fof(f3291, plain, (~ spl42_95 | ~ spl42_96), inference(avatar_contradiction_clause, [], [f3290])).
fof(f3290, plain, ($false | (~ spl42_95 | ~ spl42_96)), inference(subsumption_resolution, [], [f3289, f311])).
fof(f3289, plain, ((e22 = e23) | (~ spl42_95 | ~ spl42_96)), inference(forward_demodulation, [], [f1052, f1048])).
fof(f1052, plain, ((e23 = op2(e22, e20)) | ~ spl42_96), inference(avatar_component_clause, [], [f1050])).
fof(f1050, plain, (spl42_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_96])])).
fof(f3276, plain, (~ spl42_121 | ~ spl42_124), inference(avatar_contradiction_clause, [], [f3275])).
fof(f3275, plain, ($false | (~ spl42_121 | ~ spl42_124)), inference(subsumption_resolution, [], [f3274, f308])).
fof(f3274, plain, ((e20 = e23) | (~ spl42_121 | ~ spl42_124)), inference(backward_demodulation, [], [f1171, f1159])).
fof(f3262, plain, (~ spl42_113 | ~ spl42_196), inference(avatar_split_clause, [], [f3256, f1748, f1123])).
fof(f1123, plain, (spl42_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_113])])).
fof(f3256, plain, (~ (e20 = op2(e20, e23)) | ~ spl42_196), inference(backward_demodulation, [], [f1714, f1749])).
fof(f1714, plain, ~ (op2(e20, e23) = h4(e10)), inference(backward_demodulation, [], [f271, f1712])).
fof(f271, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3255, plain, (spl42_196 | ~ spl42_81), inference(avatar_split_clause, [], [f3125, f987, f1748])).
fof(f3125, plain, ((e20 = h4(e10)) | ~ spl42_81), inference(forward_demodulation, [], [f1712, f989])).
fof(f3251, plain, (~ spl42_114 | ~ spl42_192), inference(avatar_split_clause, [], [f2934, f1727, f1127])).
fof(f1127, plain, (spl42_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_114])])).
fof(f2934, plain, (~ (e21 = op2(e20, e23)) | ~ spl42_192), inference(forward_demodulation, [], [f1704, f1728])).
fof(f1704, plain, ~ (op2(e20, e23) = h4(e11)), inference(backward_demodulation, [], [f272, f548])).
fof(f272, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3250, plain, (~ spl42_110 | ~ spl42_106), inference(avatar_split_clause, [], [f3249, f1093, f1110])).
fof(f3249, plain, (~ (e21 = op2(e21, e20)) | ~ spl42_106), inference(forward_demodulation, [], [f1688, f2961])).
fof(f2961, plain, ((e21 = h2(e11)) | ~ spl42_106), inference(backward_demodulation, [], [f540, f1095])).
fof(f1688, plain, ~ (op2(e21, e20) = h2(e11)), inference(backward_demodulation, [], [f282, f540])).
fof(f282, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f3246, plain, (spl42_110 | ~ spl42_101 | ~ spl42_180), inference(avatar_split_clause, [], [f3141, f1624, f1072, f1110])).
fof(f1624, plain, (spl42_180 <=> (e21 = op2(e21, op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl42_180])])).
fof(f3141, plain, ((e21 = op2(e21, e20)) | (~ spl42_101 | ~ spl42_180)), inference(forward_demodulation, [], [f1626, f1074])).
fof(f1074, plain, ((e20 = op2(e21, e22)) | ~ spl42_101), inference(avatar_component_clause, [], [f1072])).
fof(f1626, plain, ((e21 = op2(e21, op2(e21, e22))) | ~ spl42_180), inference(avatar_component_clause, [], [f1624])).
fof(f3234, plain, (~ spl42_38 | ~ spl42_54), inference(avatar_contradiction_clause, [], [f3233])).
fof(f3233, plain, ($false | (~ spl42_38 | ~ spl42_54)), inference(subsumption_resolution, [], [f3232, f842])).
fof(f3232, plain, (~ (e11 = op1(e10, e12)) | ~ spl42_38), inference(forward_demodulation, [], [f216, f774])).
fof(f3216, plain, (~ spl42_9 | ~ spl42_11), inference(avatar_contradiction_clause, [], [f3215])).
fof(f3215, plain, ($false | (~ spl42_9 | ~ spl42_11)), inference(subsumption_resolution, [], [f3213, f301])).
fof(f301, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f3213, plain, ((e10 = e12) | (~ spl42_9 | ~ spl42_11)), inference(backward_demodulation, [], [f659, f651])).
fof(f651, plain, ((e10 = op1(e13, e11)) | ~ spl42_9), inference(avatar_component_clause, [], [f649])).
fof(f649, plain, (spl42_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_9])])).
fof(f3204, plain, (~ spl42_37 | ~ spl42_38), inference(avatar_contradiction_clause, [], [f3203])).
fof(f3203, plain, ($false | (~ spl42_37 | ~ spl42_38)), inference(subsumption_resolution, [], [f3202, f300])).
fof(f300, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f3202, plain, ((e10 = e11) | (~ spl42_37 | ~ spl42_38)), inference(forward_demodulation, [], [f774, f770])).
fof(f3200, plain, (spl42_34 | ~ spl42_44 | ~ spl42_153), inference(avatar_contradiction_clause, [], [f3199])).
fof(f3199, plain, ($false | (spl42_34 | ~ spl42_44 | ~ spl42_153)), inference(subsumption_resolution, [], [f3196, f756])).
fof(f756, plain, (~ (e11 = op1(e11, e13)) | spl42_34), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl42_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_34])])).
fof(f3196, plain, ((e11 = op1(e11, e13)) | (~ spl42_44 | ~ spl42_153)), inference(backward_demodulation, [], [f1419, f799])).
fof(f799, plain, ((e13 = op1(e11, e11)) | ~ spl42_44), inference(avatar_component_clause, [], [f797])).
fof(f797, plain, (spl42_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_44])])).
fof(f1419, plain, ((e11 = op1(e11, op1(e11, e11))) | ~ spl42_153), inference(avatar_component_clause, [], [f1417])).
fof(f1417, plain, (spl42_153 <=> (e11 = op1(e11, op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_153])])).
fof(f3179, plain, (~ spl42_57 | ~ spl42_61), inference(avatar_split_clause, [], [f3171, f870, f853])).
fof(f870, plain, (spl42_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_61])])).
fof(f3171, plain, (~ (e10 = op1(e10, e11)) | ~ spl42_61), inference(backward_demodulation, [], [f228, f872])).
fof(f872, plain, ((e10 = op1(e10, e10)) | ~ spl42_61), inference(avatar_component_clause, [], [f870])).
fof(f228, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f3170, plain, (~ spl42_73 | ~ spl42_75), inference(avatar_contradiction_clause, [], [f3169])).
fof(f3169, plain, ($false | (~ spl42_73 | ~ spl42_75)), inference(subsumption_resolution, [], [f3168, f307])).
fof(f3168, plain, ((e20 = e22) | (~ spl42_73 | ~ spl42_75)), inference(backward_demodulation, [], [f963, f955])).
fof(f955, plain, ((e20 = op2(e23, e21)) | ~ spl42_73), inference(avatar_component_clause, [], [f953])).
fof(f953, plain, (spl42_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_73])])).
fof(f3164, plain, (spl42_248 | ~ spl42_92 | ~ spl42_199), inference(avatar_split_clause, [], [f3163, f1764, f1033, f2060])).
fof(f3163, plain, ((e23 = h3(e12)) | (~ spl42_92 | ~ spl42_199)), inference(backward_demodulation, [], [f3135, f1035])).
fof(f3135, plain, ((op2(e22, e21) = h3(e12)) | ~ spl42_199), inference(backward_demodulation, [], [f1700, f1765])).
fof(f3106, plain, (~ spl42_122 | ~ spl42_106), inference(avatar_split_clause, [], [f3105, f1093, f1161])).
fof(f1161, plain, (spl42_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_122])])).
fof(f3105, plain, (~ (e21 = op2(e20, e21)) | ~ spl42_106), inference(forward_demodulation, [], [f1685, f2961])).
fof(f1685, plain, ~ (op2(e20, e21) = h2(e11)), inference(backward_demodulation, [], [f258, f540])).
fof(f258, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f3102, plain, (~ spl42_123 | ~ spl42_75), inference(avatar_split_clause, [], [f3101, f961, f1165])).
fof(f1165, plain, (spl42_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_123])])).
fof(f3101, plain, (~ (e22 = op2(e20, e21)) | ~ spl42_75), inference(forward_demodulation, [], [f260, f963])).
fof(f260, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3090, plain, (~ spl42_91 | ~ spl42_75), inference(avatar_split_clause, [], [f3089, f961, f1029])).
fof(f3089, plain, (~ (e22 = op2(e22, e21)) | ~ spl42_75), inference(forward_demodulation, [], [f263, f963])).
fof(f263, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3080, plain, (~ spl42_59 | ~ spl42_11), inference(avatar_split_clause, [], [f3079, f657, f861])).
fof(f861, plain, (spl42_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_59])])).
fof(f3079, plain, (~ (e12 = op1(e10, e11)) | ~ spl42_11), inference(forward_demodulation, [], [f212, f659])).
fof(f212, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3076, plain, (~ spl42_56 | ~ spl42_52), inference(avatar_split_clause, [], [f3075, f831, f848])).
fof(f3075, plain, (~ (e13 = op1(e10, e12)) | ~ spl42_52), inference(forward_demodulation, [], [f233, f833])).
fof(f233, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3072, plain, (~ spl42_43 | ~ spl42_11), inference(avatar_split_clause, [], [f3071, f657, f793])).
fof(f793, plain, (spl42_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_43])])).
fof(f3071, plain, (~ (e12 = op1(e11, e11)) | ~ spl42_11), inference(forward_demodulation, [], [f214, f659])).
fof(f214, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3062, plain, (~ spl42_21 | ~ spl42_17), inference(avatar_split_clause, [], [f2861, f683, f700])).
fof(f700, plain, (spl42_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_21])])).
fof(f2861, plain, (~ (e10 = op1(e12, e12)) | ~ spl42_17), inference(forward_demodulation, [], [f245, f685])).
fof(f245, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f3061, plain, (~ spl42_7 | ~ spl42_11), inference(avatar_split_clause, [], [f3058, f657, f640])).
fof(f640, plain, (spl42_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_7])])).
fof(f3058, plain, (~ (e12 = op1(e13, e12)) | ~ spl42_11), inference(backward_demodulation, [], [f249, f659])).
fof(f249, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3029, plain, (~ spl42_37 | ~ spl42_40), inference(avatar_contradiction_clause, [], [f3028])).
fof(f3028, plain, ($false | (~ spl42_37 | ~ spl42_40)), inference(subsumption_resolution, [], [f3027, f302])).
fof(f3027, plain, ((e10 = e13) | (~ spl42_37 | ~ spl42_40)), inference(backward_demodulation, [], [f782, f770])).
fof(f3024, plain, (spl42_49 | ~ spl42_52 | ~ spl42_144), inference(avatar_contradiction_clause, [], [f3023])).
fof(f3023, plain, ($false | (spl42_49 | ~ spl42_52 | ~ spl42_144)), inference(subsumption_resolution, [], [f3022, f820])).
fof(f820, plain, (~ (e10 = op1(e10, e13)) | spl42_49), inference(avatar_component_clause, [], [f819])).
fof(f819, plain, (spl42_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_49])])).
fof(f3022, plain, ((e10 = op1(e10, e13)) | (~ spl42_52 | ~ spl42_144)), inference(backward_demodulation, [], [f1381, f833])).
fof(f1381, plain, ((e10 = op1(e10, op1(e10, e13))) | ~ spl42_144), inference(avatar_component_clause, [], [f1379])).
fof(f1379, plain, (spl42_144 <=> (e10 = op1(e10, op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_144])])).
fof(f3021, plain, (~ spl42_57 | ~ spl42_58), inference(avatar_contradiction_clause, [], [f3020])).
fof(f3020, plain, ($false | (~ spl42_57 | ~ spl42_58)), inference(subsumption_resolution, [], [f3019, f300])).
fof(f3019, plain, ((e10 = e11) | (~ spl42_57 | ~ spl42_58)), inference(backward_demodulation, [], [f859, f855])).
fof(f859, plain, ((e11 = op1(e10, e11)) | ~ spl42_58), inference(avatar_component_clause, [], [f857])).
fof(f857, plain, (spl42_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_58])])).
fof(f3017, plain, (~ spl42_26 | ~ spl42_58), inference(avatar_split_clause, [], [f3013, f857, f721])).
fof(f3013, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_58), inference(backward_demodulation, [], [f211, f859])).
fof(f211, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2994, plain, (~ spl42_71 | ~ spl42_75), inference(avatar_split_clause, [], [f2993, f961, f944])).
fof(f944, plain, (spl42_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_71])])).
fof(f2993, plain, (~ (e22 = op2(e23, e22)) | ~ spl42_75), inference(backward_demodulation, [], [f297, f963])).
fof(f297, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2982, plain, (~ spl42_81 | ~ spl42_83), inference(avatar_contradiction_clause, [], [f2981])).
fof(f2981, plain, ($false | (~ spl42_81 | ~ spl42_83)), inference(subsumption_resolution, [], [f2980, f307])).
fof(f2980, plain, ((e20 = e22) | (~ spl42_81 | ~ spl42_83)), inference(forward_demodulation, [], [f997, f989])).
fof(f997, plain, ((e22 = op2(e22, e23)) | ~ spl42_83), inference(avatar_component_clause, [], [f995])).
fof(f995, plain, (spl42_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_83])])).
fof(f2978, plain, (spl42_199 | ~ spl42_86), inference(avatar_split_clause, [], [f2976, f1008, f1764])).
fof(f2976, plain, ((e21 = h3(e11)) | ~ spl42_86), inference(backward_demodulation, [], [f544, f1010])).
fof(f2973, plain, (~ spl42_80 | ~ spl42_96), inference(avatar_split_clause, [], [f2972, f1050, f982])).
fof(f2972, plain, (~ (e23 = op2(e23, e20)) | ~ spl42_96), inference(backward_demodulation, [], [f257, f1052])).
fof(f257, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2967, plain, (~ spl42_101 | ~ spl42_104), inference(avatar_contradiction_clause, [], [f2966])).
fof(f2966, plain, ($false | (~ spl42_101 | ~ spl42_104)), inference(subsumption_resolution, [], [f2965, f308])).
fof(f2965, plain, ((e20 = e23) | (~ spl42_101 | ~ spl42_104)), inference(forward_demodulation, [], [f1086, f1074])).
fof(f1086, plain, ((e23 = op2(e21, e22)) | ~ spl42_104), inference(avatar_component_clause, [], [f1084])).
fof(f2955, plain, (spl42_117 | ~ spl42_119 | ~ spl42_179), inference(avatar_contradiction_clause, [], [f2954])).
fof(f2954, plain, ($false | (spl42_117 | ~ spl42_119 | ~ spl42_179)), inference(subsumption_resolution, [], [f2951, f1141])).
fof(f1141, plain, (~ (e20 = op2(e20, e22)) | spl42_117), inference(avatar_component_clause, [], [f1140])).
fof(f1140, plain, (spl42_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_117])])).
fof(f2951, plain, ((e20 = op2(e20, e22)) | (~ spl42_119 | ~ spl42_179)), inference(backward_demodulation, [], [f1622, f1150])).
fof(f1622, plain, ((e20 = op2(e20, op2(e20, e22))) | ~ spl42_179), inference(avatar_component_clause, [], [f1620])).
fof(f1620, plain, (spl42_179 <=> (e20 = op2(e20, op2(e20, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl42_179])])).
fof(f2950, plain, (~ spl42_121 | ~ spl42_122), inference(avatar_contradiction_clause, [], [f2949])).
fof(f2949, plain, ($false | (~ spl42_121 | ~ spl42_122)), inference(subsumption_resolution, [], [f2948, f306])).
fof(f2948, plain, ((e20 = e21) | (~ spl42_121 | ~ spl42_122)), inference(backward_demodulation, [], [f1163, f1159])).
fof(f1163, plain, ((e21 = op2(e20, e21)) | ~ spl42_122), inference(avatar_component_clause, [], [f1161])).
fof(f2944, plain, (~ spl42_125 | ~ spl42_126), inference(avatar_contradiction_clause, [], [f2943])).
fof(f2943, plain, ($false | (~ spl42_125 | ~ spl42_126)), inference(subsumption_resolution, [], [f2942, f306])).
fof(f2942, plain, ((e20 = e21) | (~ spl42_125 | ~ spl42_126)), inference(forward_demodulation, [], [f1180, f1176])).
fof(f1176, plain, ((e20 = op2(e20, e20)) | ~ spl42_125), inference(avatar_component_clause, [], [f1174])).
fof(f1174, plain, (spl42_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_125])])).
fof(f1180, plain, ((op2(e20, e20) = e21) | ~ spl42_126), inference(avatar_component_clause, [], [f1178])).
fof(f2936, plain, (spl42_113 | ~ spl42_116 | ~ spl42_175), inference(avatar_split_clause, [], [f2935, f1603, f1135, f1123])).
fof(f1603, plain, (spl42_175 <=> (e20 = op2(e20, op2(e20, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl42_175])])).
fof(f2935, plain, ((e20 = op2(e20, e23)) | (~ spl42_116 | ~ spl42_175)), inference(forward_demodulation, [], [f1605, f1137])).
fof(f1605, plain, ((e20 = op2(e20, op2(e20, e23))) | ~ spl42_175), inference(avatar_component_clause, [], [f1603])).
fof(f2919, plain, (~ spl42_98 | ~ spl42_192), inference(avatar_split_clause, [], [f2918, f1727, f1059])).
fof(f1059, plain, (spl42_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_98])])).
fof(f2918, plain, (~ (e21 = op2(e21, e23)) | ~ spl42_192), inference(forward_demodulation, [], [f1705, f1728])).
fof(f1705, plain, ~ (op2(e21, e23) = h4(e11)), inference(backward_demodulation, [], [f274, f548])).
fof(f274, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2894, plain, (~ spl42_74 | ~ spl42_192), inference(avatar_split_clause, [], [f2893, f1727, f957])).
fof(f957, plain, (spl42_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_74])])).
fof(f2893, plain, (~ (e21 = op2(e23, e21)) | ~ spl42_192), inference(forward_demodulation, [], [f1708, f1728])).
fof(f1708, plain, ~ (op2(e23, e21) = h4(e11)), inference(backward_demodulation, [], [f298, f548])).
fof(f298, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2890, plain, (spl42_75 | ~ spl42_192), inference(avatar_split_clause, [], [f2746, f1727, f961])).
fof(f2746, plain, ((e22 = op2(e23, e21)) | ~ spl42_192), inference(backward_demodulation, [], [f1710, f1728])).
fof(f2889, plain, (~ spl42_70 | ~ spl42_192), inference(avatar_split_clause, [], [f2888, f1727, f940])).
fof(f940, plain, (spl42_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_70])])).
fof(f2888, plain, (~ (e21 = op2(e23, e22)) | ~ spl42_192), inference(forward_demodulation, [], [f1709, f1728])).
fof(f1709, plain, ~ (op2(e23, e22) = h4(e11)), inference(backward_demodulation, [], [f299, f548])).
fof(f299, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2880, plain, (spl42_53 | ~ spl42_55 | ~ spl42_148), inference(avatar_split_clause, [], [f2879, f1396, f844, f836])).
fof(f836, plain, (spl42_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_53])])).
fof(f2879, plain, ((e10 = op1(e10, e12)) | (~ spl42_55 | ~ spl42_148)), inference(forward_demodulation, [], [f1398, f846])).
fof(f1398, plain, ((e10 = op1(e10, op1(e10, e12))) | ~ spl42_148), inference(avatar_component_clause, [], [f1396])).
fof(f2874, plain, (~ spl42_50 | ~ spl42_2), inference(avatar_split_clause, [], [f2684, f619, f823])).
fof(f823, plain, (spl42_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_50])])).
fof(f2684, plain, (~ (e11 = op1(e10, e13)) | ~ spl42_2), inference(forward_demodulation, [], [f224, f621])).
fof(f224, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2852, plain, (~ spl42_6 | ~ spl42_2), inference(avatar_split_clause, [], [f2851, f619, f636])).
fof(f636, plain, (spl42_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_6])])).
fof(f2851, plain, (~ (e11 = op1(e13, e12)) | ~ spl42_2), inference(forward_demodulation, [], [f251, f621])).
fof(f251, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2798, plain, (~ spl42_88 | spl42_256), inference(avatar_contradiction_clause, [], [f2797])).
fof(f2797, plain, ($false | (~ spl42_88 | spl42_256)), inference(subsumption_resolution, [], [f2796, f2106])).
fof(f2106, plain, (~ (e23 = h3(e11)) | spl42_256), inference(avatar_component_clause, [], [f2104])).
fof(f2796, plain, ((e23 = h3(e11)) | ~ spl42_88), inference(backward_demodulation, [], [f544, f1018])).
fof(f2787, plain, (~ spl42_101 | ~ spl42_103), inference(avatar_contradiction_clause, [], [f2786])).
fof(f2786, plain, ($false | (~ spl42_101 | ~ spl42_103)), inference(subsumption_resolution, [], [f2785, f307])).
fof(f2785, plain, ((e20 = e22) | (~ spl42_101 | ~ spl42_103)), inference(forward_demodulation, [], [f1082, f1074])).
fof(f2784, plain, (spl42_275 | ~ spl42_108), inference(avatar_split_clause, [], [f2783, f1101, f2218])).
fof(f1101, plain, (spl42_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_108])])).
fof(f2783, plain, ((e23 = h2(e11)) | ~ spl42_108), inference(backward_demodulation, [], [f540, f1103])).
fof(f1103, plain, ((e23 = op2(e21, e21)) | ~ spl42_108), inference(avatar_component_clause, [], [f1101])).
fof(f2775, plain, (~ spl42_115 | ~ spl42_116), inference(avatar_contradiction_clause, [], [f2774])).
fof(f2774, plain, ($false | (~ spl42_115 | ~ spl42_116)), inference(subsumption_resolution, [], [f2773, f311])).
fof(f2773, plain, ((e22 = e23) | (~ spl42_115 | ~ spl42_116)), inference(forward_demodulation, [], [f1137, f1133])).
fof(f1133, plain, ((e22 = op2(e20, e23)) | ~ spl42_115), inference(avatar_component_clause, [], [f1131])).
fof(f1131, plain, (spl42_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_115])])).
fof(f2762, plain, (~ spl42_90 | ~ spl42_122), inference(avatar_split_clause, [], [f2757, f1161, f1025])).
fof(f2757, plain, (~ (e21 = op2(e22, e21)) | ~ spl42_122), inference(backward_demodulation, [], [f259, f1163])).
fof(f259, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2731, plain, (~ spl42_119 | ~ spl42_115), inference(avatar_split_clause, [], [f2730, f1131, f1148])).
fof(f2730, plain, (~ (e22 = op2(e20, e22)) | ~ spl42_115), inference(forward_demodulation, [], [f281, f1133])).
fof(f2713, plain, (~ spl42_81 | ~ spl42_84), inference(avatar_contradiction_clause, [], [f2712])).
fof(f2712, plain, ($false | (~ spl42_81 | ~ spl42_84)), inference(subsumption_resolution, [], [f2711, f308])).
fof(f2711, plain, ((e20 = e23) | (~ spl42_81 | ~ spl42_84)), inference(forward_demodulation, [], [f1001, f989])).
fof(f1001, plain, ((e23 = op2(e22, e23)) | ~ spl42_84), inference(avatar_component_clause, [], [f999])).
fof(f999, plain, (spl42_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_84])])).
fof(f2694, plain, (~ spl42_57 | ~ spl42_53), inference(avatar_split_clause, [], [f2693, f836, f853])).
fof(f2693, plain, (~ (e10 = op1(e10, e11)) | ~ spl42_53), inference(forward_demodulation, [], [f231, f838])).
fof(f838, plain, ((e10 = op1(e10, e12)) | ~ spl42_53), inference(avatar_component_clause, [], [f836])).
fof(f231, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f2688, plain, (~ spl42_49 | ~ spl42_17), inference(avatar_split_clause, [], [f2687, f683, f819])).
fof(f2687, plain, (~ (e10 = op1(e10, e13)) | ~ spl42_17), inference(forward_demodulation, [], [f223, f685])).
fof(f223, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2673, plain, (~ spl42_33 | ~ spl42_17), inference(avatar_split_clause, [], [f2672, f683, f751])).
fof(f751, plain, (spl42_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_33])])).
fof(f2672, plain, (~ (e10 = op1(e11, e13)) | ~ spl42_17), inference(forward_demodulation, [], [f225, f685])).
fof(f225, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2671, plain, (~ spl42_34 | ~ spl42_2), inference(avatar_split_clause, [], [f2670, f619, f755])).
fof(f2670, plain, (~ (e11 = op1(e11, e13)) | ~ spl42_2), inference(forward_demodulation, [], [f226, f621])).
fof(f226, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2663, plain, (~ spl42_29 | ~ spl42_17), inference(avatar_split_clause, [], [f2662, f683, f734])).
fof(f734, plain, (spl42_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_29])])).
fof(f2662, plain, (~ (e10 = op1(e12, e10)) | ~ spl42_17), inference(forward_demodulation, [], [f242, f685])).
fof(f242, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2659, plain, (~ spl42_25 | ~ spl42_17), inference(avatar_split_clause, [], [f2658, f683, f717])).
fof(f717, plain, (spl42_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_25])])).
fof(f2658, plain, (~ (e10 = op1(e12, e11)) | ~ spl42_17), inference(forward_demodulation, [], [f244, f685])).
fof(f244, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2657, plain, (~ spl42_17 | ~ spl42_20), inference(avatar_contradiction_clause, [], [f2656])).
fof(f2656, plain, ($false | (~ spl42_17 | ~ spl42_20)), inference(subsumption_resolution, [], [f2655, f302])).
fof(f2655, plain, ((e10 = e13) | (~ spl42_17 | ~ spl42_20)), inference(forward_demodulation, [], [f697, f685])).
fof(f697, plain, ((e13 = op1(e12, e13)) | ~ spl42_20), inference(avatar_component_clause, [], [f695])).
fof(f695, plain, (spl42_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_20])])).
fof(f2652, plain, (~ spl42_10 | ~ spl42_2), inference(avatar_split_clause, [], [f2651, f619, f653])).
fof(f653, plain, (spl42_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_10])])).
fof(f2651, plain, (~ (e11 = op1(e13, e11)) | ~ spl42_2), inference(forward_demodulation, [], [f250, f621])).
fof(f250, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2650, plain, (spl42_11 | ~ spl42_2), inference(avatar_split_clause, [], [f2645, f619, f657])).
fof(f2645, plain, ((e12 = op1(e13, e11)) | ~ spl42_2), inference(backward_demodulation, [], [f530, f621])).
fof(f530, plain, (e12 = op1(e13, op1(e13, e13))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(e13, op1(e13, e13))) & (e11 = op1(e13, e13)) & (e10 = op1(op1(e13, op1(e13, e13)), e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax12)).
fof(f2633, plain, (~ spl42_11 | ~ spl42_12), inference(avatar_contradiction_clause, [], [f2632])).
fof(f2632, plain, ($false | (~ spl42_11 | ~ spl42_12)), inference(subsumption_resolution, [], [f2631, f305])).
fof(f2631, plain, ((e12 = e13) | (~ spl42_11 | ~ spl42_12)), inference(backward_demodulation, [], [f663, f659])).
fof(f663, plain, ((e13 = op1(e13, e11)) | ~ spl42_12), inference(avatar_component_clause, [], [f661])).
fof(f661, plain, (spl42_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_12])])).
fof(f2615, plain, (~ spl42_17 | ~ spl42_19), inference(avatar_contradiction_clause, [], [f2614])).
fof(f2614, plain, ($false | (~ spl42_17 | ~ spl42_19)), inference(subsumption_resolution, [], [f2613, f301])).
fof(f2613, plain, ((e10 = e12) | (~ spl42_17 | ~ spl42_19)), inference(backward_demodulation, [], [f693, f685])).
fof(f693, plain, ((e12 = op1(e12, e13)) | ~ spl42_19), inference(avatar_component_clause, [], [f691])).
fof(f691, plain, (spl42_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_19])])).
fof(f2532, plain, (spl42_61 | ~ spl42_57 | ~ spl42_152), inference(avatar_split_clause, [], [f2526, f1413, f853, f870])).
fof(f1413, plain, (spl42_152 <=> (e10 = op1(e10, op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_152])])).
fof(f2526, plain, ((e10 = op1(e10, e10)) | (~ spl42_57 | ~ spl42_152)), inference(backward_demodulation, [], [f1415, f855])).
fof(f1415, plain, ((e10 = op1(e10, op1(e10, e11))) | ~ spl42_152), inference(avatar_component_clause, [], [f1413])).
fof(f2504, plain, (~ spl42_65 | ~ spl42_66), inference(avatar_contradiction_clause, [], [f2503])).
fof(f2503, plain, ($false | (~ spl42_65 | ~ spl42_66)), inference(subsumption_resolution, [], [f2502, f306])).
fof(f2502, plain, ((e20 = e21) | (~ spl42_65 | ~ spl42_66)), inference(backward_demodulation, [], [f925, f921])).
fof(f921, plain, ((e20 = op2(e23, e23)) | ~ spl42_65), inference(avatar_component_clause, [], [f919])).
fof(f919, plain, (spl42_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_65])])).
fof(f2501, plain, (spl42_192 | ~ spl42_66), inference(avatar_split_clause, [], [f2500, f923, f1727])).
fof(f2500, plain, ((e21 = h4(e11)) | ~ spl42_66), inference(backward_demodulation, [], [f548, f925])).
fof(f2485, plain, (~ spl42_75 | ~ spl42_76), inference(avatar_contradiction_clause, [], [f2484])).
fof(f2484, plain, ($false | (~ spl42_75 | ~ spl42_76)), inference(subsumption_resolution, [], [f2483, f311])).
fof(f2483, plain, ((e22 = e23) | (~ spl42_75 | ~ spl42_76)), inference(backward_demodulation, [], [f967, f963])).
fof(f967, plain, ((e23 = op2(e23, e21)) | ~ spl42_76), inference(avatar_component_clause, [], [f965])).
fof(f965, plain, (spl42_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_76])])).
fof(f2447, plain, (~ spl42_196 | ~ spl42_89), inference(avatar_split_clause, [], [f2443, f1021, f1748])).
fof(f1021, plain, (spl42_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_89])])).
fof(f2443, plain, (~ (e20 = h4(e10)) | ~ spl42_89), inference(backward_demodulation, [], [f1717, f1023])).
fof(f1023, plain, ((e20 = op2(e22, e21)) | ~ spl42_89), inference(avatar_component_clause, [], [f1021])).
fof(f1717, plain, ~ (op2(e22, e21) = h4(e10)), inference(backward_demodulation, [], [f292, f1712])).
fof(f292, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2439, plain, (~ spl42_196 | ~ spl42_93), inference(avatar_split_clause, [], [f2435, f1038, f1748])).
fof(f1038, plain, (spl42_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_93])])).
fof(f2435, plain, (~ (e20 = h4(e10)) | ~ spl42_93), inference(backward_demodulation, [], [f1716, f1040])).
fof(f1040, plain, ((e20 = op2(e22, e20)) | ~ spl42_93), inference(avatar_component_clause, [], [f1038])).
fof(f1716, plain, ~ (op2(e22, e20) = h4(e10)), inference(backward_demodulation, [], [f290, f1712])).
fof(f290, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2413, plain, (spl42_211 | ~ spl42_105), inference(avatar_split_clause, [], [f2412, f1089, f1825])).
fof(f1825, plain, (spl42_211 <=> (e20 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_211])])).
fof(f1089, plain, (spl42_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_105])])).
fof(f2412, plain, ((e20 = h2(e11)) | ~ spl42_105), inference(backward_demodulation, [], [f540, f1091])).
fof(f1091, plain, ((e20 = op2(e21, e21)) | ~ spl42_105), inference(avatar_component_clause, [], [f1089])).
fof(f2378, plain, (~ spl42_211 | ~ spl42_121), inference(avatar_split_clause, [], [f2371, f1157, f1825])).
fof(f2371, plain, (~ (e20 = h2(e11)) | ~ spl42_121), inference(backward_demodulation, [], [f1685, f1159])).
fof(f2377, plain, (spl42_125 | ~ spl42_121 | ~ spl42_183), inference(avatar_split_clause, [], [f2370, f1637, f1157, f1174])).
fof(f1637, plain, (spl42_183 <=> (e20 = op2(e20, op2(e20, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl42_183])])).
fof(f2370, plain, ((e20 = op2(e20, e20)) | (~ spl42_121 | ~ spl42_183)), inference(backward_demodulation, [], [f1639, f1159])).
fof(f1639, plain, ((e20 = op2(e20, op2(e20, e21))) | ~ spl42_183), inference(avatar_component_clause, [], [f1637])).
fof(f2375, plain, (~ spl42_117 | ~ spl42_121), inference(avatar_split_clause, [], [f2368, f1157, f1140])).
fof(f2368, plain, (~ (e20 = op2(e20, e22)) | ~ spl42_121), inference(backward_demodulation, [], [f279, f1159])).
fof(f279, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f1959, plain, (~ spl42_221 | ~ spl42_222 | ~ spl42_223 | ~ spl42_224 | spl42_194 | spl42_191 | ~ spl42_225 | ~ spl42_226 | ~ spl42_227 | ~ spl42_228 | ~ spl42_229 | ~ spl42_230 | ~ spl42_231 | ~ spl42_232 | ~ spl42_233 | ~ spl42_234 | ~ spl42_235 | ~ spl42_236), inference(avatar_split_clause, [], [f1894, f1956, f1952, f1948, f1944, f1940, f1936, f1932, f1928, f1924, f1920, f1916, f1912, f1723, f1739, f1908, f1904, f1900, f1896])).
fof(f1739, plain, (spl42_194 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl42_194])])).
fof(f1723, plain, (spl42_191 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl42_191])])).
fof(f1894, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | ~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1893, f1703])).
fof(f1893, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1892, f546])).
fof(f1892, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1891, f1703])).
fof(f1891, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1890, f546])).
fof(f1890, plain, (~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1889, f1703])).
fof(f1889, plain, (~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1888, f1703])).
fof(f1888, plain, (~ (h3(e11) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1887, f544])).
fof(f1887, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1886, f1703])).
fof(f1886, plain, (~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1885, f1712])).
fof(f1885, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1884, f1703])).
fof(f1884, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1883, f546])).
fof(f1883, plain, (~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1882, f546])).
fof(f1882, plain, (~ (e22 = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1881, f1710])).
fof(f1881, plain, (~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1880, f546])).
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
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', co1)).
fof(f1751, plain, (~ spl42_194 | ~ spl42_196), inference(avatar_split_clause, [], [f558, f1748, f1739])).
fof(f558, plain, (~ (e20 = h4(e10)) | ~ sP39), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP39), inference(nnf_transformation, [], [f62])).
fof(f1730, plain, (~ spl42_191 | ~ spl42_192), inference(avatar_split_clause, [], [f555, f1727, f1723])).
fof(f555, plain, (~ (e21 = h4(e11)) | ~ sP40), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP40), inference(nnf_transformation, [], [f63])).
fof(f1675, plain, spl42_81, inference(avatar_split_clause, [], [f1674, f987])).
fof(f1674, plain, (e20 = op2(e22, e23)), inference(forward_demodulation, [], [f531, f533])).
fof(f531, plain, (e20 = op2(op2(e23, op2(e23, e23)), e23)), inference(cnf_transformation, [], [f13])).
fof(f1673, plain, spl42_66, inference(avatar_split_clause, [], [f532, f923])).
fof(f532, plain, (e21 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f1672, plain, spl42_17, inference(avatar_split_clause, [], [f1671, f683])).
fof(f1671, plain, (e10 = op1(e12, e13)), inference(forward_demodulation, [], [f528, f530])).
fof(f528, plain, (e10 = op1(op1(e13, op1(e13, e13)), e13)), inference(cnf_transformation, [], [f12])).
fof(f1670, plain, spl42_2, inference(avatar_split_clause, [], [f529, f619])).
fof(f529, plain, (e11 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f1652, plain, (spl42_183 | spl42_184 | spl42_185 | spl42_186), inference(avatar_split_clause, [], [f519, f1649, f1645, f1641, f1637])).
fof(f519, plain, ((e23 = op2(e23, op2(e23, e21))) | (e22 = op2(e22, op2(e22, e21))) | (e21 = op2(e21, op2(e21, e21))) | (e20 = op2(e20, op2(e20, e21)))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15) & ((e23 = op2(e23, op2(e23, e23))) | (e22 = op2(e22, op2(e22, e23))) | (e21 = op2(e21, op2(e21, e23))) | (e20 = op2(e20, op2(e20, e23)))) & ((e23 = op2(e23, op2(e23, e22))) | (e22 = op2(e22, op2(e22, e22))) | (e21 = op2(e21, op2(e21, e22))) | (e20 = op2(e20, op2(e20, e22)))) & ((e23 = op2(e23, op2(e23, e21))) | (e22 = op2(e22, op2(e22, e21))) | (e21 = op2(e21, op2(e21, e21))) | (e20 = op2(e20, op2(e20, e21)))) & ((e23 = op2(e23, op2(e23, e20))) | (e22 = op2(e22, op2(e22, e20))) | (e21 = op2(e21, op2(e21, e20))) | (e20 = op2(e20, op2(e20, e20))))), inference(definition_folding, [], [f11, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37])).
fof(f37, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(usedef, [], [e37])).
fof(e37, plain, (sP15 <=> (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f38, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP16), inference(usedef, [], [e38])).
fof(e38, plain, (sP16 <=> (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f39, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP17), inference(usedef, [], [e39])).
fof(e39, plain, (sP17 <=> (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f40, plain, ((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP18), inference(usedef, [], [e40])).
fof(e40, plain, (sP18 <=> (~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f41, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP19), inference(usedef, [], [e41])).
fof(e41, plain, (sP19 <=> (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f42, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(usedef, [], [e42])).
fof(e42, plain, (sP20 <=> (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f43, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP21), inference(usedef, [], [e43])).
fof(e43, plain, (sP21 <=> (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f44, plain, ((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP22), inference(usedef, [], [e44])).
fof(e44, plain, (sP22 <=> (~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f45, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP23), inference(usedef, [], [e45])).
fof(e45, plain, (sP23 <=> (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f46, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP24), inference(usedef, [], [e46])).
fof(e46, plain, (sP24 <=> (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f47, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(usedef, [], [e47])).
fof(e47, plain, (sP25 <=> (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f48, plain, ((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP26), inference(usedef, [], [e48])).
fof(e48, plain, (sP26 <=> (~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f49, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP27), inference(usedef, [], [e49])).
fof(e49, plain, (sP27 <=> (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f50, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP28), inference(usedef, [], [e50])).
fof(e50, plain, (sP28 <=> (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f51, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP29), inference(usedef, [], [e51])).
fof(e51, plain, (sP29 <=> (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | (~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | (~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | (~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | (~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))) & ((e23 = op2(e23, op2(e23, e23))) | (e22 = op2(e22, op2(e22, e23))) | (e21 = op2(e21, op2(e21, e23))) | (e20 = op2(e20, op2(e20, e23)))) & ((e23 = op2(e23, op2(e23, e22))) | (e22 = op2(e22, op2(e22, e22))) | (e21 = op2(e21, op2(e21, e22))) | (e20 = op2(e20, op2(e20, e22)))) & ((e23 = op2(e23, op2(e23, e21))) | (e22 = op2(e22, op2(e22, e21))) | (e21 = op2(e21, op2(e21, e21))) | (e20 = op2(e20, op2(e20, e21)))) & ((e23 = op2(e23, op2(e23, e20))) | (e22 = op2(e22, op2(e22, e20))) | (e21 = op2(e21, op2(e21, e20))) | (e20 = op2(e20, op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax11)).
fof(f1635, plain, (spl42_179 | spl42_180 | spl42_181 | spl42_182), inference(avatar_split_clause, [], [f520, f1632, f1628, f1624, f1620])).
fof(f520, plain, ((e23 = op2(e23, op2(e23, e22))) | (e22 = op2(e22, op2(e22, e22))) | (e21 = op2(e21, op2(e21, e22))) | (e20 = op2(e20, op2(e20, e22)))), inference(cnf_transformation, [], [f52])).
fof(f1618, plain, (spl42_175 | spl42_176 | spl42_177 | spl42_178), inference(avatar_split_clause, [], [f521, f1615, f1611, f1607, f1603])).
fof(f521, plain, ((e23 = op2(e23, op2(e23, e23))) | (e22 = op2(e22, op2(e22, e23))) | (e21 = op2(e21, op2(e21, e23))) | (e20 = op2(e20, op2(e20, e23)))), inference(cnf_transformation, [], [f52])).
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
fof(f95, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(nnf_transformation, [], [f37])).
fof(f1593, plain, (~ spl42_174 | ~ spl42_125), inference(avatar_split_clause, [], [f514, f1174, f1587])).
fof(f514, plain, (~ (e20 = op2(e20, e20)) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f1592, plain, (~ spl42_174 | ~ spl42_105), inference(avatar_split_clause, [], [f515, f1089, f1587])).
fof(f515, plain, (~ (e20 = op2(e21, e21)) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f1580, plain, (~ spl42_173 | ~ spl42_66), inference(avatar_split_clause, [], [f511, f923, f1577])).
fof(f511, plain, (~ (e21 = op2(e23, e23)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP16), inference(nnf_transformation, [], [f38])).
fof(f1574, plain, (~ spl42_172 | spl42_93), inference(avatar_split_clause, [], [f501, f1038, f1567])).
fof(f501, plain, ((e20 = op2(e22, e20)) | ~ sP17), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP17), inference(nnf_transformation, [], [f39])).
fof(f1565, plain, (~ spl42_171 | spl42_113), inference(avatar_split_clause, [], [f494, f1123, f1557])).
fof(f494, plain, ((e20 = op2(e20, e23)) | ~ sP18), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP18), inference(nnf_transformation, [], [f40])).
fof(f1554, plain, (~ spl42_170 | spl42_122), inference(avatar_split_clause, [], [f489, f1161, f1547])).
fof(f489, plain, ((e21 = op2(e20, e21)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP19), inference(nnf_transformation, [], [f41])).
fof(f1553, plain, (~ spl42_170 | ~ spl42_125), inference(avatar_split_clause, [], [f490, f1174, f1547])).
fof(f490, plain, (~ (e20 = op2(e20, e20)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f1552, plain, (~ spl42_170 | ~ spl42_105), inference(avatar_split_clause, [], [f491, f1089, f1547])).
fof(f491, plain, (~ (e20 = op2(e21, e21)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f1540, plain, (~ spl42_169 | ~ spl42_66), inference(avatar_split_clause, [], [f487, f923, f1537])).
fof(f487, plain, (~ (e21 = op2(e23, e23)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(nnf_transformation, [], [f42])).
fof(f1535, plain, (~ spl42_168 | spl42_102), inference(avatar_split_clause, [], [f476, f1076, f1527])).
fof(f476, plain, ((e21 = op2(e21, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP21), inference(nnf_transformation, [], [f43])).
fof(f1534, plain, (~ spl42_168 | spl42_90), inference(avatar_split_clause, [], [f477, f1025, f1527])).
fof(f477, plain, ((e21 = op2(e22, e21)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1532, plain, (~ spl42_168 | ~ spl42_107), inference(avatar_split_clause, [], [f479, f1097, f1527])).
fof(f1097, plain, (spl42_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_107])])).
fof(f479, plain, (~ (e22 = op2(e21, e21)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1531, plain, (~ spl42_168 | ~ spl42_87), inference(avatar_split_clause, [], [f480, f1012, f1527])).
fof(f480, plain, (~ (e22 = op2(e22, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1525, plain, (~ spl42_167 | spl42_98), inference(avatar_split_clause, [], [f470, f1059, f1517])).
fof(f470, plain, ((e21 = op2(e21, e23)) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP22), inference(nnf_transformation, [], [f44])).
fof(f1515, plain, (~ spl42_166 | spl42_95), inference(avatar_split_clause, [], [f464, f1046, f1507])).
fof(f464, plain, ((e22 = op2(e22, e20)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP23), inference(nnf_transformation, [], [f45])).
fof(f1514, plain, (~ spl42_166 | spl42_119), inference(avatar_split_clause, [], [f465, f1148, f1507])).
fof(f465, plain, ((e22 = op2(e20, e22)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1513, plain, (~ spl42_166 | ~ spl42_125), inference(avatar_split_clause, [], [f466, f1174, f1507])).
fof(f466, plain, (~ (e20 = op2(e20, e20)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1512, plain, (~ spl42_166 | ~ spl42_105), inference(avatar_split_clause, [], [f467, f1089, f1507])).
fof(f467, plain, (~ (e20 = op2(e21, e21)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1505, plain, (~ spl42_165 | spl42_91), inference(avatar_split_clause, [], [f458, f1029, f1497])).
fof(f458, plain, ((e22 = op2(e22, e21)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f86, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP24), inference(nnf_transformation, [], [f46])).
fof(f1495, plain, (~ spl42_164 | spl42_87), inference(avatar_split_clause, [], [f452, f1012, f1487])).
fof(f452, plain, ((e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(nnf_transformation, [], [f47])).
fof(f1491, plain, (~ spl42_164 | ~ spl42_87), inference(avatar_split_clause, [], [f456, f1012, f1487])).
fof(f456, plain, (~ (e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f1485, plain, (~ spl42_163 | spl42_83), inference(avatar_split_clause, [], [f446, f995, f1477])).
fof(f446, plain, ((e22 = op2(e22, e23)) | ~ sP26), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP26), inference(nnf_transformation, [], [f48])).
fof(f1475, plain, (~ spl42_162 | spl42_80), inference(avatar_split_clause, [], [f440, f982, f1467])).
fof(f440, plain, ((e23 = op2(e23, e20)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP27), inference(nnf_transformation, [], [f49])).
fof(f1474, plain, (~ spl42_162 | spl42_116), inference(avatar_split_clause, [], [f441, f1135, f1467])).
fof(f441, plain, ((e23 = op2(e20, e23)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f1473, plain, (~ spl42_162 | ~ spl42_125), inference(avatar_split_clause, [], [f442, f1174, f1467])).
fof(f442, plain, (~ (e20 = op2(e20, e20)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f1472, plain, (~ spl42_162 | ~ spl42_105), inference(avatar_split_clause, [], [f443, f1089, f1467])).
fof(f443, plain, (~ (e20 = op2(e21, e21)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f1465, plain, (~ spl42_161 | spl42_76), inference(avatar_split_clause, [], [f434, f965, f1457])).
fof(f434, plain, ((e23 = op2(e23, e21)) | ~ sP28), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP28), inference(nnf_transformation, [], [f50])).
fof(f1454, plain, (~ spl42_160 | spl42_84), inference(avatar_split_clause, [], [f429, f999, f1447])).
fof(f429, plain, ((e23 = op2(e22, e23)) | ~ sP29), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP29), inference(nnf_transformation, [], [f51])).
fof(f1428, plain, (spl42_152 | spl42_153 | spl42_154 | spl42_155), inference(avatar_split_clause, [], [f419, f1425, f1421, f1417, f1413])).
fof(f419, plain, ((e13 = op1(e13, op1(e13, e11))) | (e12 = op1(e12, op1(e12, e11))) | (e11 = op1(e11, op1(e11, e11))) | (e10 = op1(e10, op1(e10, e11)))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e13 = op1(e13, op1(e13, e13))) | (e12 = op1(e12, op1(e12, e13))) | (e11 = op1(e11, op1(e11, e13))) | (e10 = op1(e10, op1(e10, e13)))) & ((e13 = op1(e13, op1(e13, e12))) | (e12 = op1(e12, op1(e12, e12))) | (e11 = op1(e11, op1(e11, e12))) | (e10 = op1(e10, op1(e10, e12)))) & ((e13 = op1(e13, op1(e13, e11))) | (e12 = op1(e12, op1(e12, e11))) | (e11 = op1(e11, op1(e11, e11))) | (e10 = op1(e10, op1(e10, e11)))) & ((e13 = op1(e13, op1(e13, e10))) | (e12 = op1(e12, op1(e12, e10))) | (e11 = op1(e11, op1(e11, e10))) | (e10 = op1(e10, op1(e10, e10))))), inference(definition_folding, [], [f10, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, ((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> (~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, ((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> (~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | (~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | (~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | (~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | (~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))) & ((e13 = op1(e13, op1(e13, e13))) | (e12 = op1(e12, op1(e12, e13))) | (e11 = op1(e11, op1(e11, e13))) | (e10 = op1(e10, op1(e10, e13)))) & ((e13 = op1(e13, op1(e13, e12))) | (e12 = op1(e12, op1(e12, e12))) | (e11 = op1(e11, op1(e11, e12))) | (e10 = op1(e10, op1(e10, e12)))) & ((e13 = op1(e13, op1(e13, e11))) | (e12 = op1(e12, op1(e12, e11))) | (e11 = op1(e11, op1(e11, e11))) | (e10 = op1(e10, op1(e10, e11)))) & ((e13 = op1(e13, op1(e13, e10))) | (e12 = op1(e12, op1(e12, e10))) | (e11 = op1(e11, op1(e11, e10))) | (e10 = op1(e10, op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax10)).
fof(f1411, plain, (spl42_148 | spl42_149 | spl42_150 | spl42_151), inference(avatar_split_clause, [], [f420, f1408, f1404, f1400, f1396])).
fof(f420, plain, ((e13 = op1(e13, op1(e13, e12))) | (e12 = op1(e12, op1(e12, e12))) | (e11 = op1(e11, op1(e11, e12))) | (e10 = op1(e10, op1(e10, e12)))), inference(cnf_transformation, [], [f36])).
fof(f1394, plain, (spl42_144 | spl42_145 | spl42_146 | spl42_147), inference(avatar_split_clause, [], [f421, f1391, f1387, f1383, f1379])).
fof(f421, plain, ((e13 = op1(e13, op1(e13, e13))) | (e12 = op1(e12, op1(e12, e13))) | (e11 = op1(e11, op1(e11, e13))) | (e10 = op1(e10, op1(e10, e13)))), inference(cnf_transformation, [], [f36])).
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
fof(f422, plain, ((e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1371, plain, (~ spl42_143 | spl42_61), inference(avatar_split_clause, [], [f412, f870, f1363])).
fof(f412, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1368, plain, (~ spl42_143 | ~ spl42_41), inference(avatar_split_clause, [], [f415, f785, f1363])).
fof(f415, plain, (~ (e10 = op1(e11, e11)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f1356, plain, (~ spl42_142 | ~ spl42_2), inference(avatar_split_clause, [], [f411, f619, f1353])).
fof(f411, plain, (~ (e11 = op1(e13, e13)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1350, plain, (~ spl42_141 | spl42_29), inference(avatar_split_clause, [], [f401, f734, f1343])).
fof(f401, plain, ((e10 = op1(e12, e10)) | ~ sP2), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1341, plain, (~ spl42_140 | spl42_49), inference(avatar_split_clause, [], [f394, f819, f1333])).
fof(f394, plain, ((e10 = op1(e10, e13)) | ~ sP3), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f1330, plain, (~ spl42_139 | spl42_58), inference(avatar_split_clause, [], [f389, f857, f1323])).
fof(f389, plain, ((e11 = op1(e10, e11)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f1328, plain, (~ spl42_139 | ~ spl42_41), inference(avatar_split_clause, [], [f391, f785, f1323])).
fof(f391, plain, (~ (e10 = op1(e11, e11)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f1316, plain, (~ spl42_138 | ~ spl42_2), inference(avatar_split_clause, [], [f387, f619, f1313])).
fof(f387, plain, (~ (e11 = op1(e13, e13)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f1311, plain, (~ spl42_137 | spl42_38), inference(avatar_split_clause, [], [f376, f772, f1303])).
fof(f376, plain, ((e11 = op1(e11, e12)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f1310, plain, (~ spl42_137 | spl42_26), inference(avatar_split_clause, [], [f377, f721, f1303])).
fof(f377, plain, ((e11 = op1(e12, e11)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1307, plain, (~ spl42_137 | ~ spl42_23), inference(avatar_split_clause, [], [f380, f708, f1303])).
fof(f380, plain, (~ (e12 = op1(e12, e12)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1301, plain, (~ spl42_136 | spl42_34), inference(avatar_split_clause, [], [f370, f755, f1293])).
fof(f370, plain, ((e11 = op1(e11, e13)) | ~ sP7), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f1291, plain, (~ spl42_135 | spl42_31), inference(avatar_split_clause, [], [f364, f742, f1283])).
fof(f364, plain, ((e12 = op1(e12, e10)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f1290, plain, (~ spl42_135 | spl42_55), inference(avatar_split_clause, [], [f365, f844, f1283])).
fof(f365, plain, ((e12 = op1(e10, e12)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1288, plain, (~ spl42_135 | ~ spl42_41), inference(avatar_split_clause, [], [f367, f785, f1283])).
fof(f367, plain, (~ (e10 = op1(e11, e11)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1281, plain, (~ spl42_134 | spl42_27), inference(avatar_split_clause, [], [f358, f725, f1273])).
fof(f358, plain, ((e12 = op1(e12, e11)) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f1271, plain, (~ spl42_133 | spl42_23), inference(avatar_split_clause, [], [f352, f708, f1263])).
fof(f352, plain, ((e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f1267, plain, (~ spl42_133 | ~ spl42_23), inference(avatar_split_clause, [], [f356, f708, f1263])).
fof(f356, plain, (~ (e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f1261, plain, (~ spl42_132 | spl42_19), inference(avatar_split_clause, [], [f346, f691, f1253])).
fof(f346, plain, ((e12 = op1(e12, e13)) | ~ sP11), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ((~ (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f1250, plain, (~ spl42_131 | spl42_52), inference(avatar_split_clause, [], [f341, f831, f1243])).
fof(f341, plain, ((e13 = op1(e10, e13)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ((~ (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f1248, plain, (~ spl42_131 | ~ spl42_41), inference(avatar_split_clause, [], [f343, f785, f1243])).
fof(f343, plain, (~ (e10 = op1(e11, e11)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f1241, plain, (~ spl42_130 | spl42_12), inference(avatar_split_clause, [], [f334, f661, f1233])).
fof(f334, plain, ((e13 = op1(e13, e11)) | ~ sP13), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ((~ (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f1230, plain, (~ spl42_129 | spl42_20), inference(avatar_split_clause, [], [f329, f695, f1223])).
fof(f329, plain, ((e13 = op1(e12, e13)) | ~ sP14), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f1219, plain, (spl42_126 | spl42_122 | spl42_118 | spl42_114), inference(avatar_split_clause, [], [f174, f1127, f1144, f1161, f1178])).
fof(f174, plain, ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax4)).
fof(f1213, plain, (spl42_109 | spl42_105 | spl42_101 | spl42_97), inference(avatar_split_clause, [], [f180, f1055, f1072, f1089, f1106])).
fof(f180, plain, ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1212, plain, (spl42_121 | spl42_105 | spl42_89 | spl42_73), inference(avatar_split_clause, [], [f181, f953, f1021, f1089, f1157])).
fof(f181, plain, ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1211, plain, (spl42_110 | spl42_106 | spl42_102 | spl42_98), inference(avatar_split_clause, [], [f182, f1059, f1076, f1093, f1110])).
fof(f182, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1210, plain, (spl42_122 | spl42_106 | spl42_90 | spl42_74), inference(avatar_split_clause, [], [f183, f957, f1025, f1093, f1161])).
fof(f183, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1209, plain, (spl42_111 | spl42_107 | spl42_103 | spl42_99), inference(avatar_split_clause, [], [f184, f1063, f1080, f1097, f1114])).
fof(f184, plain, ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1206, plain, (spl42_124 | spl42_108 | spl42_92 | spl42_76), inference(avatar_split_clause, [], [f187, f965, f1033, f1101, f1169])).
fof(f187, plain, ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1202, plain, (spl42_118 | spl42_102 | spl42_86 | spl42_70), inference(avatar_split_clause, [], [f191, f940, f1008, f1076, f1144])).
fof(f191, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1201, plain, (spl42_95 | spl42_91 | spl42_87 | spl42_83), inference(avatar_split_clause, [], [f192, f995, f1012, f1029, f1046])).
fof(f192, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1200, plain, (spl42_119 | spl42_103 | spl42_87 | spl42_71), inference(avatar_split_clause, [], [f193, f944, f1012, f1080, f1148])).
fof(f193, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1199, plain, (spl42_96 | spl42_92 | spl42_88 | spl42_84), inference(avatar_split_clause, [], [f194, f999, f1016, f1033, f1050])).
fof(f194, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1198, plain, (spl42_120 | spl42_104 | spl42_88 | spl42_72), inference(avatar_split_clause, [], [f195, f948, f1016, f1084, f1152])).
fof(f195, plain, ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1197, plain, (spl42_77 | spl42_73 | spl42_69 | spl42_65), inference(avatar_split_clause, [], [f196, f919, f936, f953, f970])).
fof(f196, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1192, plain, (spl42_115 | spl42_99 | spl42_83 | spl42_67), inference(avatar_split_clause, [], [f201, f927, f995, f1063, f1131])).
fof(f201, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1189, plain, (spl42_125 | spl42_126 | spl42_127 | spl42_128), inference(avatar_split_clause, [], [f156, f1186, f1182, f1178, f1174])).
fof(f156, plain, ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax3)).
fof(f1172, plain, (spl42_121 | spl42_122 | spl42_123 | spl42_124), inference(avatar_split_clause, [], [f157, f1169, f1165, f1161, f1157])).
fof(f157, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f1138, plain, (spl42_113 | spl42_114 | spl42_115 | spl42_116), inference(avatar_split_clause, [], [f159, f1135, f1131, f1127, f1123])).
fof(f159, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f1087, plain, (spl42_101 | spl42_102 | spl42_103 | spl42_104), inference(avatar_split_clause, [], [f162, f1084, f1080, f1076, f1072])).
fof(f162, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f1036, plain, (spl42_89 | spl42_90 | spl42_91 | spl42_92), inference(avatar_split_clause, [], [f165, f1033, f1029, f1025, f1021])).
fof(f165, plain, ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f3])).
fof(f985, plain, (spl42_77 | spl42_78 | spl42_79 | spl42_80), inference(avatar_split_clause, [], [f168, f982, f978, f974, f970])).
fof(f168, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f951, plain, (spl42_69 | spl42_70 | spl42_71 | spl42_72), inference(avatar_split_clause, [], [f170, f948, f944, f940, f936])).
fof(f170, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f915, plain, (spl42_62 | spl42_58 | spl42_54 | spl42_50), inference(avatar_split_clause, [], [f126, f823, f840, f857, f874])).
fof(f126, plain, ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax2)).
fof(f910, plain, (spl42_64 | spl42_48 | spl42_32 | spl42_16), inference(avatar_split_clause, [], [f131, f678, f746, f814, f882])).
fof(f131, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f909, plain, (spl42_45 | spl42_41 | spl42_37 | spl42_33), inference(avatar_split_clause, [], [f132, f751, f768, f785, f802])).
fof(f132, plain, ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f908, plain, (spl42_57 | spl42_41 | spl42_25 | spl42_9), inference(avatar_split_clause, [], [f133, f649, f717, f785, f853])).
fof(f133, plain, ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f907, plain, (spl42_46 | spl42_42 | spl42_38 | spl42_34), inference(avatar_split_clause, [], [f134, f755, f772, f789, f806])).
fof(f134, plain, ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f906, plain, (spl42_58 | spl42_42 | spl42_26 | spl42_10), inference(avatar_split_clause, [], [f135, f653, f721, f789, f857])).
fof(f135, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f905, plain, (spl42_47 | spl42_43 | spl42_39 | spl42_35), inference(avatar_split_clause, [], [f136, f759, f776, f793, f810])).
fof(f136, plain, ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f900, plain, (spl42_53 | spl42_37 | spl42_21 | spl42_5), inference(avatar_split_clause, [], [f141, f632, f700, f768, f836])).
fof(f141, plain, ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f898, plain, (spl42_54 | spl42_38 | spl42_22 | spl42_6), inference(avatar_split_clause, [], [f143, f636, f704, f772, f840])).
fof(f143, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f897, plain, (spl42_31 | spl42_27 | spl42_23 | spl42_19), inference(avatar_split_clause, [], [f144, f691, f708, f725, f742])).
fof(f144, plain, ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f896, plain, (spl42_55 | spl42_39 | spl42_23 | spl42_7), inference(avatar_split_clause, [], [f145, f640, f708, f776, f844])).
fof(f145, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f895, plain, (spl42_32 | spl42_28 | spl42_24 | spl42_20), inference(avatar_split_clause, [], [f146, f695, f712, f729, f746])).
fof(f146, plain, ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f894, plain, (spl42_56 | spl42_40 | spl42_24 | spl42_8), inference(avatar_split_clause, [], [f147, f644, f712, f780, f848])).
fof(f147, plain, ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f868, plain, (spl42_57 | spl42_58 | spl42_59 | spl42_60), inference(avatar_split_clause, [], [f109, f865, f861, f857, f853])).
fof(f109, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG106+1.p', ax1)).
fof(f851, plain, (spl42_53 | spl42_54 | spl42_55 | spl42_56), inference(avatar_split_clause, [], [f110, f848, f844, f840, f836])).
fof(f110, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f834, plain, (spl42_49 | spl42_50 | spl42_51 | spl42_52), inference(avatar_split_clause, [], [f111, f831, f827, f823, f819])).
fof(f111, plain, ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f1])).
fof(f800, plain, (spl42_41 | spl42_42 | spl42_43 | spl42_44), inference(avatar_split_clause, [], [f113, f797, f793, f789, f785])).
fof(f113, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f783, plain, (spl42_37 | spl42_38 | spl42_39 | spl42_40), inference(avatar_split_clause, [], [f114, f780, f776, f772, f768])).
fof(f114, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f766, plain, (spl42_33 | spl42_34 | spl42_35 | spl42_36), inference(avatar_split_clause, [], [f115, f763, f759, f755, f751])).
fof(f115, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).