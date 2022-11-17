fof(f4657, plain, $false, inference(avatar_sat_refutation, [], [f674, f691, f708, f810, f827, f844, f861, f895, f912, f931, f934, f935, f941, f950, f951, f952, f953, f954, f955, f956, f957, f959, f960, f961, f978, f995, f1012, f1131, f1165, f1182, f1199, f1216, f1235, f1236, f1239, f1245, f1250, f1251, f1254, f1255, f1256, f1257, f1261, f1263, f1265, f1273, f1278, f1281, f1288, f1289, f1298, f1305, f1307, f1315, f1316, f1325, f1334, f1341, f1352, f1361, f1370, f1377, f1388, f1397, f1406, f1413, f1415, f1423, f1424, f1425, f1429, f1433, f1441, f1449, f1456, f1457, f1466, f1473, f1483, f1484, f1493, f1502, f1509, f1520, f1529, f1538, f1545, f1556, f1565, f1574, f1581, f1583, f1591, f1592, f1593, f1597, f1601, f1602, f1604, f1605, f1607, f1659, f1690, f1705, f1721, f1742, f1760, f1973, f2090, f2340, f2348, f2350, f2359, f2360, f2361, f2367, f2376, f2385, f2392, f2412, f2413, f2423, f2425, f2440, f2448, f2457, f2468, f2482, f2484, f2493, f2495, f2501, f2504, f2516, f2525, f2535, f2542, f2555, f2562, f2572, f2574, f2577, f2581, f2612, f2616, f2618, f2622, f2624, f2628, f2631, f2640, f2648, f2683, f2691, f2692, f2696, f2702, f2709, f2718, f2730, f2741, f2758, f2778, f2782, f2792, f2809, f2847, f2853, f2878, f2888, f2889, f2891, f2903, f2924, f2927, f2933, f2946, f2963, f2967, f2988, f2995, f3001, f3008, f3011, f3018, f3022, f3024, f3030, f3053, f3060, f3064, f3065, f3072, f3082, f3097, f3127, f3130, f3137, f3197, f3204, f3234, f3237, f3283, f3305, f3319, f3334, f3356, f3360, f3383, f3403, f3415, f3431, f3444, f3455, f3476, f3485, f3499, f3512, f3526, f3537, f3542, f3551, f3567, f3624, f3633, f3643, f3673, f3700, f3739, f3770, f3771, f3776, f3780, f3789, f3797, f3812, f3846, f3858, f3863, f3875, f3880, f3891, f3896, f3907, f3910, f3916, f3919, f3925, f3930, f3932, f3954, f3959, f3963, f3969, f4001, f4010, f4018, f4042, f4048, f4051, f4164, f4177, f4265, f4279, f4281, f4299, f4309, f4310, f4374, f4377, f4382, f4409, f4448, f4460, f4468, f4473, f4484, f4552, f4601, f4617, f4635, f4653])).
fof(f4653, plain, (~ spl48_52 | ~ spl48_116 | ~ spl48_233 | spl48_242), inference(avatar_contradiction_clause, [], [f4652])).
fof(f4652, plain, ($false | (~ spl48_52 | ~ spl48_116 | ~ spl48_233 | spl48_242)), inference(subsumption_resolution, [], [f4651, f1181])).
fof(f1181, plain, ((e23 = op2(e20, e23)) | ~ spl48_116), inference(avatar_component_clause, [], [f1179])).
fof(f1179, plain, (spl48_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_116])])).
fof(f4651, plain, (~ (e23 = op2(e20, e23)) | (~ spl48_52 | ~ spl48_233 | spl48_242)), inference(forward_demodulation, [], [f4650, f2040])).
fof(f2040, plain, ((e23 = h3(e13)) | ~ spl48_233), inference(avatar_component_clause, [], [f2039])).
fof(f2039, plain, (spl48_233 <=> (e23 = h3(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_233])])).
fof(f4650, plain, (~ (op2(e20, e23) = h3(e13)) | (~ spl48_52 | ~ spl48_233 | spl48_242)), inference(forward_demodulation, [], [f4649, f877])).
fof(f877, plain, ((e13 = op1(e10, e13)) | ~ spl48_52), inference(avatar_component_clause, [], [f875])).
fof(f875, plain, (spl48_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_52])])).
fof(f4649, plain, (~ (op2(e20, e23) = h3(op1(e10, e13))) | (~ spl48_233 | spl48_242)), inference(forward_demodulation, [], [f2077, f2040])).
fof(f2077, plain, (~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | spl48_242), inference(avatar_component_clause, [], [f2075])).
fof(f2075, plain, (spl48_242 <=> (h3(op1(e10, e13)) = op2(e20, h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_242])])).
fof(f4635, plain, (~ spl48_37 | ~ spl48_101 | ~ spl48_182 | spl48_240), inference(avatar_contradiction_clause, [], [f4634])).
fof(f4634, plain, ($false | (~ spl48_37 | ~ spl48_101 | ~ spl48_182 | spl48_240)), inference(subsumption_resolution, [], [f4633, f1118])).
fof(f1118, plain, ((e20 = op2(e21, e22)) | ~ spl48_101), inference(avatar_component_clause, [], [f1116])).
fof(f1116, plain, (spl48_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_101])])).
fof(f4633, plain, (~ (e20 = op2(e21, e22)) | (~ spl48_37 | ~ spl48_182 | spl48_240)), inference(forward_demodulation, [], [f4632, f1641])).
fof(f1641, plain, (e20 = h3(e10)), inference(forward_demodulation, [], [f1640, f1632])).
fof(f1632, plain, (e20 = op2(e22, h3(e13))), inference(backward_demodulation, [], [f543, f557])).
fof(f557, plain, (op2(e22, e22) = h3(e13)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, e22) = h3(e13)) & (op2(e22, op2(e22, op2(e22, e22))) = h3(e11)) & (op2(e22, op2(e22, e22)) = h3(e10)) & (e22 = h3(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax16)).
fof(f543, plain, (e20 = op2(e22, op2(e22, e22))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(e22, e22)) & (e21 = op2(e22, op2(e22, op2(e22, e22)))) & (e20 = op2(e22, op2(e22, e22)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax13)).
fof(f1640, plain, (h3(e10) = op2(e22, h3(e13))), inference(forward_demodulation, [], [f555, f557])).
fof(f555, plain, (op2(e22, op2(e22, e22)) = h3(e10)), inference(cnf_transformation, [], [f16])).
fof(f4632, plain, (~ (op2(e21, e22) = h3(e10)) | (~ spl48_37 | ~ spl48_182 | spl48_240)), inference(forward_demodulation, [], [f4631, f814])).
fof(f814, plain, ((e10 = op1(e11, e12)) | ~ spl48_37), inference(avatar_component_clause, [], [f812])).
fof(f812, plain, (spl48_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_37])])).
fof(f4631, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | (~ spl48_182 | spl48_240)), inference(forward_demodulation, [], [f2069, f1740])).
fof(f1740, plain, ((e21 = h3(e11)) | ~ spl48_182), inference(avatar_component_clause, [], [f1739])).
fof(f1739, plain, (spl48_182 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_182])])).
fof(f2069, plain, (~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | spl48_240), inference(avatar_component_clause, [], [f2067])).
fof(f2067, plain, (spl48_240 <=> (h3(op1(e11, e12)) = op2(h3(e11), e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_240])])).
fof(f4617, plain, (~ spl48_34 | ~ spl48_98 | ~ spl48_182 | spl48_230 | ~ spl48_233), inference(avatar_contradiction_clause, [], [f4616])).
fof(f4616, plain, ($false | (~ spl48_34 | ~ spl48_98 | ~ spl48_182 | spl48_230 | ~ spl48_233)), inference(subsumption_resolution, [], [f4615, f1105])).
fof(f1105, plain, ((e21 = op2(e21, e23)) | ~ spl48_98), inference(avatar_component_clause, [], [f1103])).
fof(f1103, plain, (spl48_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_98])])).
fof(f4615, plain, (~ (e21 = op2(e21, e23)) | (~ spl48_34 | ~ spl48_182 | spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f4614, f1740])).
fof(f4614, plain, (~ (op2(e21, e23) = h3(e11)) | (~ spl48_34 | ~ spl48_182 | spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f4613, f801])).
fof(f801, plain, ((e11 = op1(e11, e13)) | ~ spl48_34), inference(avatar_component_clause, [], [f799])).
fof(f799, plain, (spl48_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_34])])).
fof(f4613, plain, (~ (op2(e21, e23) = h3(op1(e11, e13))) | (~ spl48_182 | spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f4612, f1740])).
fof(f4612, plain, (~ (h3(op1(e11, e13)) = op2(h3(e11), e23)) | (spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f2029, f2040])).
fof(f2029, plain, (~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | spl48_230), inference(avatar_component_clause, [], [f2027])).
fof(f2027, plain, (spl48_230 <=> (h3(op1(e11, e13)) = op2(h3(e11), h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_230])])).
fof(f4601, plain, (~ spl48_44 | ~ spl48_108 | ~ spl48_182 | spl48_229 | ~ spl48_233), inference(avatar_contradiction_clause, [], [f4600])).
fof(f4600, plain, ($false | (~ spl48_44 | ~ spl48_108 | ~ spl48_182 | spl48_229 | ~ spl48_233)), inference(subsumption_resolution, [], [f4599, f1147])).
fof(f1147, plain, ((e23 = op2(e21, e21)) | ~ spl48_108), inference(avatar_component_clause, [], [f1145])).
fof(f1145, plain, (spl48_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_108])])).
fof(f4599, plain, (~ (e23 = op2(e21, e21)) | (~ spl48_44 | ~ spl48_182 | spl48_229 | ~ spl48_233)), inference(forward_demodulation, [], [f4598, f2040])).
fof(f4598, plain, (~ (op2(e21, e21) = h3(e13)) | (~ spl48_44 | ~ spl48_182 | spl48_229)), inference(forward_demodulation, [], [f4597, f843])).
fof(f843, plain, ((e13 = op1(e11, e11)) | ~ spl48_44), inference(avatar_component_clause, [], [f841])).
fof(f841, plain, (spl48_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_44])])).
fof(f4597, plain, (~ (op2(e21, e21) = h3(op1(e11, e11))) | (~ spl48_182 | spl48_229)), inference(forward_demodulation, [], [f2025, f1740])).
fof(f2025, plain, (~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | spl48_229), inference(avatar_component_clause, [], [f2023])).
fof(f2023, plain, (spl48_229 <=> (h3(op1(e11, e11)) = op2(h3(e11), h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_229])])).
fof(f4552, plain, (~ spl48_61 | ~ spl48_207 | spl48_245), inference(avatar_contradiction_clause, [], [f4551])).
fof(f4551, plain, ($false | (~ spl48_61 | ~ spl48_207 | spl48_245)), inference(subsumption_resolution, [], [f4550, f1868])).
fof(f1868, plain, ((e20 = h1(e13)) | ~ spl48_207), inference(avatar_component_clause, [], [f1867])).
fof(f1867, plain, (spl48_207 <=> (e20 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_207])])).
fof(f4550, plain, (~ (e20 = h1(e13)) | (~ spl48_61 | spl48_245)), inference(forward_demodulation, [], [f4549, f1641])).
fof(f4549, plain, (~ (h1(e13) = h3(e10)) | (~ spl48_61 | spl48_245)), inference(forward_demodulation, [], [f2089, f916])).
fof(f916, plain, ((e10 = op1(e10, e10)) | ~ spl48_61), inference(avatar_component_clause, [], [f914])).
fof(f914, plain, (spl48_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_61])])).
fof(f2089, plain, (~ (h1(e13) = h3(op1(e10, e10))) | spl48_245), inference(avatar_component_clause, [], [f2087])).
fof(f2087, plain, (spl48_245 <=> (h1(e13) = h3(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl48_245])])).
fof(f4484, plain, (~ spl48_12 | ~ spl48_44), inference(avatar_split_clause, [], [f4479, f841, f705])).
fof(f705, plain, (spl48_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_12])])).
fof(f4479, plain, (~ (e13 = op1(e13, e11)) | ~ spl48_44), inference(backward_demodulation, [], [f226, f843])).
fof(f226, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax5)).
fof(f4473, plain, (~ spl48_55 | ~ spl48_119 | spl48_243), inference(avatar_contradiction_clause, [], [f4472])).
fof(f4472, plain, ($false | (~ spl48_55 | ~ spl48_119 | spl48_243)), inference(subsumption_resolution, [], [f4467, f554])).
fof(f554, plain, (e22 = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f4467, plain, (~ (e22 = h3(e12)) | (~ spl48_55 | ~ spl48_119 | spl48_243)), inference(backward_demodulation, [], [f4420, f890])).
fof(f890, plain, ((e12 = op1(e10, e12)) | ~ spl48_55), inference(avatar_component_clause, [], [f888])).
fof(f888, plain, (spl48_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_55])])).
fof(f4420, plain, (~ (e22 = h3(op1(e10, e12))) | (~ spl48_119 | spl48_243)), inference(forward_demodulation, [], [f2081, f1194])).
fof(f1194, plain, ((e22 = op2(e20, e22)) | ~ spl48_119), inference(avatar_component_clause, [], [f1192])).
fof(f1192, plain, (spl48_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_119])])).
fof(f2081, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | spl48_243), inference(avatar_component_clause, [], [f2079])).
fof(f2079, plain, (spl48_243 <=> (op2(e20, e22) = h3(op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl48_243])])).
fof(f4468, plain, (~ spl48_39 | ~ spl48_55), inference(avatar_split_clause, [], [f4463, f888, f820])).
fof(f820, plain, (spl48_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_39])])).
fof(f4463, plain, (~ (e12 = op1(e11, e12)) | ~ spl48_55), inference(backward_demodulation, [], [f228, f890])).
fof(f228, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4460, plain, (~ spl48_42 | ~ spl48_58), inference(avatar_split_clause, [], [f4452, f901, f833])).
fof(f833, plain, (spl48_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_42])])).
fof(f901, plain, (spl48_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_58])])).
fof(f4452, plain, (~ (e11 = op1(e11, e11)) | ~ spl48_58), inference(backward_demodulation, [], [f222, f903])).
fof(f903, plain, ((e11 = op1(e10, e11)) | ~ spl48_58), inference(avatar_component_clause, [], [f901])).
fof(f222, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4448, plain, (~ spl48_53 | ~ spl48_61), inference(avatar_split_clause, [], [f4442, f914, f880])).
fof(f880, plain, (spl48_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_53])])).
fof(f4442, plain, (~ (e10 = op1(e10, e12)) | ~ spl48_61), inference(backward_demodulation, [], [f241, f916])).
fof(f241, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f4409, plain, (~ spl48_39 | ~ spl48_47), inference(avatar_split_clause, [], [f4408, f854, f820])).
fof(f854, plain, (spl48_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_47])])).
fof(f4408, plain, (~ (e12 = op1(e11, e12)) | ~ spl48_47), inference(forward_demodulation, [], [f247, f856])).
fof(f856, plain, ((e12 = op1(e11, e10)) | ~ spl48_47), inference(avatar_component_clause, [], [f854])).
fof(f247, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4382, plain, (~ spl48_13 | ~ spl48_16), inference(avatar_contradiction_clause, [], [f4381])).
fof(f4381, plain, ($false | (~ spl48_13 | ~ spl48_16)), inference(subsumption_resolution, [], [f4380, f314])).
fof(f314, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax7)).
fof(f4380, plain, ((e10 = e13) | (~ spl48_13 | ~ spl48_16)), inference(backward_demodulation, [], [f724, f712])).
fof(f712, plain, ((e10 = op1(e13, e10)) | ~ spl48_13), inference(avatar_component_clause, [], [f710])).
fof(f710, plain, (spl48_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_13])])).
fof(f724, plain, ((e13 = op1(e13, e10)) | ~ spl48_16), inference(avatar_component_clause, [], [f722])).
fof(f722, plain, (spl48_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_16])])).
fof(f4377, plain, (~ spl48_41 | ~ spl48_42), inference(avatar_contradiction_clause, [], [f4376])).
fof(f4376, plain, ($false | (~ spl48_41 | ~ spl48_42)), inference(subsumption_resolution, [], [f4375, f312])).
fof(f312, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f4375, plain, ((e10 = e11) | (~ spl48_41 | ~ spl48_42)), inference(backward_demodulation, [], [f835, f831])).
fof(f831, plain, ((e10 = op1(e11, e11)) | ~ spl48_41), inference(avatar_component_clause, [], [f829])).
fof(f829, plain, (spl48_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_41])])).
fof(f835, plain, ((e11 = op1(e11, e11)) | ~ spl48_42), inference(avatar_component_clause, [], [f833])).
fof(f4374, plain, (~ spl48_47 | ~ spl48_111 | ~ spl48_182 | spl48_241), inference(avatar_contradiction_clause, [], [f4373])).
fof(f4373, plain, ($false | (~ spl48_47 | ~ spl48_111 | ~ spl48_182 | spl48_241)), inference(subsumption_resolution, [], [f4372, f554])).
fof(f4372, plain, (~ (e22 = h3(e12)) | (~ spl48_47 | ~ spl48_111 | ~ spl48_182 | spl48_241)), inference(backward_demodulation, [], [f4311, f856])).
fof(f4311, plain, (~ (e22 = h3(op1(e11, e10))) | (~ spl48_111 | ~ spl48_182 | spl48_241)), inference(forward_demodulation, [], [f4143, f1160])).
fof(f1160, plain, ((e22 = op2(e21, e20)) | ~ spl48_111), inference(avatar_component_clause, [], [f1158])).
fof(f1158, plain, (spl48_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_111])])).
fof(f4143, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | (~ spl48_182 | spl48_241)), inference(forward_demodulation, [], [f2073, f1740])).
fof(f2073, plain, (~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | spl48_241), inference(avatar_component_clause, [], [f2071])).
fof(f2071, plain, (spl48_241 <=> (h3(op1(e11, e10)) = op2(h3(e11), e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_241])])).
fof(f4310, plain, (~ spl48_12 | ~ spl48_60), inference(avatar_split_clause, [], [f3619, f909, f705])).
fof(f909, plain, (spl48_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_60])])).
fof(f3619, plain, (~ (e13 = op1(e13, e11)) | ~ spl48_60), inference(backward_demodulation, [], [f224, f911])).
fof(f911, plain, ((e13 = op1(e10, e11)) | ~ spl48_60), inference(avatar_component_clause, [], [f909])).
fof(f224, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4309, plain, (~ spl48_12 | ~ spl48_16), inference(avatar_split_clause, [], [f3784, f722, f705])).
fof(f3784, plain, (~ (e13 = op1(e13, e11)) | ~ spl48_16), inference(backward_demodulation, [], [f258, f724])).
fof(f258, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4299, plain, (~ spl48_50 | ~ spl48_101 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_212), inference(avatar_contradiction_clause, [], [f4298])).
fof(f4298, plain, ($false | (~ spl48_50 | ~ spl48_101 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_212)), inference(subsumption_resolution, [], [f4293, f1118])).
fof(f4293, plain, (~ (e20 = op2(e21, e22)) | (~ spl48_50 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_212)), inference(backward_demodulation, [], [f4274, f1657])).
fof(f1657, plain, ((e22 = h4(e13)) | ~ spl48_166), inference(avatar_component_clause, [], [f1656])).
fof(f1656, plain, (spl48_166 <=> (e22 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_166])])).
fof(f4274, plain, (~ (e20 = op2(e21, h4(e13))) | (~ spl48_50 | ~ spl48_172 | ~ spl48_175 | spl48_212)), inference(backward_demodulation, [], [f4256, f1688])).
fof(f1688, plain, ((e21 = h4(e10)) | ~ spl48_172), inference(avatar_component_clause, [], [f1687])).
fof(f1687, plain, (spl48_172 <=> (e21 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_172])])).
fof(f4256, plain, (~ (e20 = op2(h4(e10), h4(e13))) | (~ spl48_50 | ~ spl48_175 | spl48_212)), inference(backward_demodulation, [], [f4077, f1703])).
fof(f1703, plain, ((e20 = h4(e11)) | ~ spl48_175), inference(avatar_component_clause, [], [f1702])).
fof(f1702, plain, (spl48_175 <=> (e20 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_175])])).
fof(f4077, plain, (~ (h4(e11) = op2(h4(e10), h4(e13))) | (~ spl48_50 | spl48_212)), inference(forward_demodulation, [], [f1904, f869])).
fof(f869, plain, ((e11 = op1(e10, e13)) | ~ spl48_50), inference(avatar_component_clause, [], [f867])).
fof(f867, plain, (spl48_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_50])])).
fof(f1904, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | spl48_212), inference(avatar_component_clause, [], [f1902])).
fof(f1902, plain, (spl48_212 <=> (h4(op1(e10, e13)) = op2(h4(e10), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_212])])).
fof(f4281, plain, (~ spl48_45 | ~ spl48_122 | ~ spl48_172 | ~ spl48_175 | spl48_213), inference(avatar_contradiction_clause, [], [f4280])).
fof(f4280, plain, ($false | (~ spl48_45 | ~ spl48_122 | ~ spl48_172 | ~ spl48_175 | spl48_213)), inference(subsumption_resolution, [], [f4275, f1207])).
fof(f1207, plain, ((e21 = op2(e20, e21)) | ~ spl48_122), inference(avatar_component_clause, [], [f1205])).
fof(f1205, plain, (spl48_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_122])])).
fof(f4275, plain, (~ (e21 = op2(e20, e21)) | (~ spl48_45 | ~ spl48_172 | ~ spl48_175 | spl48_213)), inference(backward_demodulation, [], [f4258, f1688])).
fof(f4258, plain, (~ (h4(e10) = op2(e20, h4(e10))) | (~ spl48_45 | ~ spl48_175 | spl48_213)), inference(backward_demodulation, [], [f4083, f1703])).
fof(f4083, plain, (~ (h4(e10) = op2(h4(e11), h4(e10))) | (~ spl48_45 | spl48_213)), inference(forward_demodulation, [], [f1908, f848])).
fof(f848, plain, ((e10 = op1(e11, e10)) | ~ spl48_45), inference(avatar_component_clause, [], [f846])).
fof(f846, plain, (spl48_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_45])])).
fof(f1908, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | spl48_213), inference(avatar_component_clause, [], [f1906])).
fof(f1906, plain, (spl48_213 <=> (h4(op1(e11, e10)) = op2(h4(e11), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl48_213])])).
fof(f4279, plain, (~ spl48_53 | ~ spl48_98 | ~ spl48_172 | spl48_226), inference(avatar_contradiction_clause, [], [f4278])).
fof(f4278, plain, ($false | (~ spl48_53 | ~ spl48_98 | ~ spl48_172 | spl48_226)), inference(subsumption_resolution, [], [f4270, f1105])).
fof(f4270, plain, (~ (e21 = op2(e21, e23)) | (~ spl48_53 | ~ spl48_172 | spl48_226)), inference(backward_demodulation, [], [f4084, f1688])).
fof(f4084, plain, (~ (h4(e10) = op2(h4(e10), e23)) | (~ spl48_53 | spl48_226)), inference(forward_demodulation, [], [f1960, f882])).
fof(f882, plain, ((e10 = op1(e10, e12)) | ~ spl48_53), inference(avatar_component_clause, [], [f880])).
fof(f1960, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e23)) | spl48_226), inference(avatar_component_clause, [], [f1958])).
fof(f1958, plain, (spl48_226 <=> (h4(op1(e10, e12)) = op2(h4(e10), e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_226])])).
fof(f4265, plain, (~ spl48_116 | ~ spl48_39 | ~ spl48_175 | spl48_225), inference(avatar_split_clause, [], [f4259, f1954, f1702, f820, f1179])).
fof(f1954, plain, (spl48_225 <=> (h4(op1(e11, e12)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_225])])).
fof(f4259, plain, (~ (e23 = op2(e20, e23)) | (~ spl48_39 | ~ spl48_175 | spl48_225)), inference(backward_demodulation, [], [f4089, f1703])).
fof(f4089, plain, (~ (e23 = op2(h4(e11), e23)) | (~ spl48_39 | spl48_225)), inference(forward_demodulation, [], [f4088, f558])).
fof(f558, plain, (e23 = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, e23) = h4(e13)) & (h4(e11) = op2(e23, op2(e23, op2(e23, e23)))) & (h4(e10) = op2(e23, op2(e23, e23))) & (e23 = h4(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax17)).
fof(f4088, plain, (~ (h4(e12) = op2(h4(e11), e23)) | (~ spl48_39 | spl48_225)), inference(forward_demodulation, [], [f1956, f822])).
fof(f822, plain, ((e12 = op1(e11, e12)) | ~ spl48_39), inference(avatar_component_clause, [], [f820])).
fof(f1956, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e23)) | spl48_225), inference(avatar_component_clause, [], [f1954])).
fof(f4177, plain, (~ spl48_101 | ~ spl48_102), inference(avatar_contradiction_clause, [], [f4176])).
fof(f4176, plain, ($false | (~ spl48_101 | ~ spl48_102)), inference(subsumption_resolution, [], [f4175, f318])).
fof(f318, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax8)).
fof(f4175, plain, ((e20 = e21) | (~ spl48_101 | ~ spl48_102)), inference(backward_demodulation, [], [f1122, f1118])).
fof(f1122, plain, ((e21 = op2(e21, e22)) | ~ spl48_102), inference(avatar_component_clause, [], [f1120])).
fof(f1120, plain, (spl48_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_102])])).
fof(f4164, plain, (~ spl48_115 | ~ spl48_119), inference(avatar_split_clause, [], [f4163, f1192, f1175])).
fof(f1175, plain, (spl48_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_115])])).
fof(f4163, plain, (~ (e22 = op2(e20, e23)) | ~ spl48_119), inference(backward_demodulation, [], [f293, f1194])).
fof(f293, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax6)).
fof(f4051, plain, (spl48_219 | ~ spl48_68), inference(avatar_split_clause, [], [f4050, f975, f1930])).
fof(f1930, plain, (spl48_219 <=> (e23 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_219])])).
fof(f975, plain, (spl48_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_68])])).
fof(f4050, plain, ((e23 = h4(e13)) | ~ spl48_68), inference(forward_demodulation, [], [f561, f977])).
fof(f977, plain, ((e23 = op2(e23, e23)) | ~ spl48_68), inference(avatar_component_clause, [], [f975])).
fof(f561, plain, (op2(e23, e23) = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f4048, plain, (~ spl48_67 | ~ spl48_68), inference(avatar_contradiction_clause, [], [f4047])).
fof(f4047, plain, ($false | (~ spl48_67 | ~ spl48_68)), inference(subsumption_resolution, [], [f4046, f323])).
fof(f323, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f4046, plain, ((e22 = e23) | (~ spl48_67 | ~ spl48_68)), inference(forward_demodulation, [], [f977, f973])).
fof(f973, plain, ((e22 = op2(e23, e23)) | ~ spl48_67), inference(avatar_component_clause, [], [f971])).
fof(f971, plain, (spl48_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_67])])).
fof(f4042, plain, (~ spl48_73 | ~ spl48_74), inference(avatar_contradiction_clause, [], [f4041])).
fof(f4041, plain, ($false | (~ spl48_73 | ~ spl48_74)), inference(subsumption_resolution, [], [f4040, f318])).
fof(f4040, plain, ((e20 = e21) | (~ spl48_73 | ~ spl48_74)), inference(forward_demodulation, [], [f1003, f999])).
fof(f999, plain, ((e20 = op2(e23, e21)) | ~ spl48_73), inference(avatar_component_clause, [], [f997])).
fof(f997, plain, (spl48_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_73])])).
fof(f1003, plain, ((e21 = op2(e23, e21)) | ~ spl48_74), inference(avatar_component_clause, [], [f1001])).
fof(f1001, plain, (spl48_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_74])])).
fof(f4018, plain, (~ spl48_100 | ~ spl48_36 | ~ spl48_182 | spl48_230 | ~ spl48_233), inference(avatar_split_clause, [], [f4017, f2039, f2027, f1739, f807, f1111])).
fof(f1111, plain, (spl48_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_100])])).
fof(f807, plain, (spl48_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_36])])).
fof(f4017, plain, (~ (e23 = op2(e21, e23)) | (~ spl48_36 | ~ spl48_182 | spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f4016, f2040])).
fof(f4016, plain, (~ (op2(e21, e23) = h3(e13)) | (~ spl48_36 | ~ spl48_182 | spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f4015, f809])).
fof(f809, plain, ((e13 = op1(e11, e13)) | ~ spl48_36), inference(avatar_component_clause, [], [f807])).
fof(f4015, plain, (~ (op2(e21, e23) = h3(op1(e11, e13))) | (~ spl48_182 | spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f4014, f1740])).
fof(f4014, plain, (~ (h3(op1(e11, e13)) = op2(h3(e11), e23)) | (spl48_230 | ~ spl48_233)), inference(forward_demodulation, [], [f2029, f2040])).
fof(f4010, plain, (spl48_98 | ~ spl48_193 | ~ spl48_256), inference(avatar_contradiction_clause, [], [f4009])).
fof(f4009, plain, ($false | (spl48_98 | ~ spl48_193 | ~ spl48_256)), inference(subsumption_resolution, [], [f4000, f1104])).
fof(f1104, plain, (~ (e21 = op2(e21, e23)) | spl48_98), inference(avatar_component_clause, [], [f1103])).
fof(f4000, plain, ((e21 = op2(e21, e23)) | (~ spl48_193 | ~ spl48_256)), inference(backward_demodulation, [], [f3988, f2164])).
fof(f2164, plain, ((e23 = h2(e13)) | ~ spl48_256), inference(avatar_component_clause, [], [f2163])).
fof(f2163, plain, (spl48_256 <=> (e23 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_256])])).
fof(f3988, plain, ((e21 = op2(e21, h2(e13))) | ~ spl48_193), inference(forward_demodulation, [], [f1624, f1799])).
fof(f1799, plain, ((e21 = h2(e10)) | ~ spl48_193), inference(avatar_component_clause, [], [f1798])).
fof(f1798, plain, (spl48_193 <=> (e21 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_193])])).
fof(f1624, plain, (h2(e10) = op2(e21, h2(e13))), inference(forward_demodulation, [], [f551, f553])).
fof(f553, plain, (op2(e21, e21) = h2(e13)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, e21) = h2(e13)) & (h2(e11) = op2(e21, op2(e21, op2(e21, e21)))) & (h2(e10) = op2(e21, op2(e21, e21))) & (e21 = h2(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax15)).
fof(f551, plain, (h2(e10) = op2(e21, op2(e21, e21))), inference(cnf_transformation, [], [f15])).
fof(f4001, plain, (~ spl48_112 | ~ spl48_256), inference(avatar_split_clause, [], [f3995, f2163, f1162])).
fof(f1162, plain, (spl48_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_112])])).
fof(f3995, plain, (~ (e23 = op2(e21, e20)) | ~ spl48_256), inference(backward_demodulation, [], [f1620, f2164])).
fof(f1620, plain, ~ (op2(e21, e20) = h2(e13)), inference(backward_demodulation, [], [f294, f553])).
fof(f294, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f3969, plain, (~ spl48_125 | ~ spl48_42 | ~ spl48_175 | spl48_214), inference(avatar_split_clause, [], [f3826, f1910, f1702, f833, f1218])).
fof(f1218, plain, (spl48_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_125])])).
fof(f1910, plain, (spl48_214 <=> (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_214])])).
fof(f3826, plain, (~ (e20 = op2(e20, e20)) | (~ spl48_42 | ~ spl48_175 | spl48_214)), inference(forward_demodulation, [], [f3825, f1703])).
fof(f3825, plain, (~ (op2(e20, e20) = h4(e11)) | (~ spl48_42 | ~ spl48_175 | spl48_214)), inference(forward_demodulation, [], [f3824, f835])).
fof(f3824, plain, (~ (op2(e20, e20) = h4(op1(e11, e11))) | (~ spl48_175 | spl48_214)), inference(forward_demodulation, [], [f1912, f1703])).
fof(f1912, plain, (~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | spl48_214), inference(avatar_component_clause, [], [f1910])).
fof(f3963, plain, (~ spl48_120 | ~ spl48_233), inference(avatar_split_clause, [], [f2874, f2039, f1196])).
fof(f1196, plain, (spl48_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_120])])).
fof(f2874, plain, (~ (e23 = op2(e20, e22)) | ~ spl48_233), inference(forward_demodulation, [], [f1626, f2040])).
fof(f1626, plain, ~ (op2(e20, e22) = h3(e13)), inference(backward_demodulation, [], [f277, f557])).
fof(f277, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f3959, plain, (~ spl48_119 | ~ spl48_36 | ~ spl48_166 | ~ spl48_175 | spl48_215), inference(avatar_split_clause, [], [f3822, f1914, f1702, f1656, f807, f1192])).
fof(f1914, plain, (spl48_215 <=> (h4(op1(e11, e13)) = op2(h4(e11), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_215])])).
fof(f3822, plain, (~ (e22 = op2(e20, e22)) | (~ spl48_36 | ~ spl48_166 | ~ spl48_175 | spl48_215)), inference(forward_demodulation, [], [f3821, f1657])).
fof(f3821, plain, (~ (op2(e20, e22) = h4(e13)) | (~ spl48_36 | ~ spl48_166 | ~ spl48_175 | spl48_215)), inference(forward_demodulation, [], [f3820, f809])).
fof(f3820, plain, (~ (op2(e20, e22) = h4(op1(e11, e13))) | (~ spl48_166 | ~ spl48_175 | spl48_215)), inference(forward_demodulation, [], [f3819, f1703])).
fof(f3819, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e22)) | (~ spl48_166 | spl48_215)), inference(forward_demodulation, [], [f1916, f1657])).
fof(f1916, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | spl48_215), inference(avatar_component_clause, [], [f1914])).
fof(f3954, plain, (~ spl48_111 | ~ spl48_60 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_211), inference(avatar_split_clause, [], [f3718, f1898, f1702, f1687, f1656, f909, f1158])).
fof(f1898, plain, (spl48_211 <=> (h4(op1(e10, e11)) = op2(h4(e10), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_211])])).
fof(f3718, plain, (~ (e22 = op2(e21, e20)) | (~ spl48_60 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f3717, f1657])).
fof(f3717, plain, (~ (op2(e21, e20) = h4(e13)) | (~ spl48_60 | ~ spl48_172 | ~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f3716, f911])).
fof(f3716, plain, (~ (op2(e21, e20) = h4(op1(e10, e11))) | (~ spl48_172 | ~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f3715, f1688])).
fof(f3715, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e20)) | (~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f1900, f1703])).
fof(f1900, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | spl48_211), inference(avatar_component_clause, [], [f1898])).
fof(f3932, plain, (~ spl48_108 | ~ spl48_3 | ~ spl48_63 | ~ spl48_88 | ~ spl48_166 | ~ spl48_172 | spl48_210 | ~ spl48_218), inference(avatar_split_clause, [], [f3833, f1926, f1894, f1687, f1656, f1060, f922, f667, f1145])).
fof(f667, plain, (spl48_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_3])])).
fof(f922, plain, (spl48_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl48_63])])).
fof(f1060, plain, (spl48_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_88])])).
fof(f1894, plain, (spl48_210 <=> (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl48_210])])).
fof(f1926, plain, (spl48_218 <=> (h4(op1(e13, e13)) = op2(h4(e13), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_218])])).
fof(f3833, plain, (~ (e23 = op2(e21, e21)) | (~ spl48_3 | ~ spl48_63 | ~ spl48_88 | ~ spl48_166 | ~ spl48_172 | spl48_210 | ~ spl48_218)), inference(forward_demodulation, [], [f3832, f3805])).
fof(f3805, plain, ((e23 = h4(e12)) | (~ spl48_3 | ~ spl48_88 | ~ spl48_166 | ~ spl48_218)), inference(backward_demodulation, [], [f3698, f669])).
fof(f669, plain, ((e12 = op1(e13, e13)) | ~ spl48_3), inference(avatar_component_clause, [], [f667])).
fof(f3698, plain, ((e23 = h4(op1(e13, e13))) | (~ spl48_88 | ~ spl48_166 | ~ spl48_218)), inference(forward_demodulation, [], [f3469, f1062])).
fof(f1062, plain, ((e23 = op2(e22, e22)) | ~ spl48_88), inference(avatar_component_clause, [], [f1060])).
fof(f3469, plain, ((op2(e22, e22) = h4(op1(e13, e13))) | (~ spl48_166 | ~ spl48_218)), inference(forward_demodulation, [], [f1927, f1657])).
fof(f1927, plain, ((h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ spl48_218), inference(avatar_component_clause, [], [f1926])).
fof(f3832, plain, (~ (op2(e21, e21) = h4(e12)) | (~ spl48_63 | ~ spl48_172 | spl48_210)), inference(forward_demodulation, [], [f3831, f924])).
fof(f924, plain, ((op1(e10, e10) = e12) | ~ spl48_63), inference(avatar_component_clause, [], [f922])).
fof(f3831, plain, (~ (op2(e21, e21) = h4(op1(e10, e10))) | (~ spl48_172 | spl48_210)), inference(forward_demodulation, [], [f1896, f1688])).
fof(f1896, plain, (~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10))) | spl48_210), inference(avatar_component_clause, [], [f1894])).
fof(f3930, plain, (~ spl48_97 | ~ spl48_81), inference(avatar_split_clause, [], [f3019, f1031, f1099])).
fof(f1099, plain, (spl48_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_97])])).
fof(f1031, plain, (spl48_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_81])])).
fof(f3019, plain, (~ (e20 = op2(e21, e23)) | ~ spl48_81), inference(forward_demodulation, [], [f285, f1033])).
fof(f1033, plain, ((e20 = op2(e22, e23)) | ~ spl48_81), inference(avatar_component_clause, [], [f1031])).
fof(f285, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3925, plain, (~ spl48_27 | ~ spl48_91 | ~ spl48_182 | spl48_238), inference(avatar_contradiction_clause, [], [f3924])).
fof(f3924, plain, ($false | (~ spl48_27 | ~ spl48_91 | ~ spl48_182 | spl48_238)), inference(subsumption_resolution, [], [f3923, f1075])).
fof(f1075, plain, ((e22 = op2(e22, e21)) | ~ spl48_91), inference(avatar_component_clause, [], [f1073])).
fof(f1073, plain, (spl48_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_91])])).
fof(f3923, plain, (~ (e22 = op2(e22, e21)) | (~ spl48_27 | ~ spl48_182 | spl48_238)), inference(forward_demodulation, [], [f3922, f554])).
fof(f3922, plain, (~ (op2(e22, e21) = h3(e12)) | (~ spl48_27 | ~ spl48_182 | spl48_238)), inference(forward_demodulation, [], [f3921, f771])).
fof(f771, plain, ((e12 = op1(e12, e11)) | ~ spl48_27), inference(avatar_component_clause, [], [f769])).
fof(f769, plain, (spl48_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_27])])).
fof(f3921, plain, (~ (op2(e22, e21) = h3(op1(e12, e11))) | (~ spl48_182 | spl48_238)), inference(forward_demodulation, [], [f2061, f1740])).
fof(f2061, plain, (~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | spl48_238), inference(avatar_component_clause, [], [f2059])).
fof(f2059, plain, (spl48_238 <=> (h3(op1(e12, e11)) = op2(e22, h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_238])])).
fof(f3919, plain, (~ spl48_24 | spl48_237), inference(avatar_contradiction_clause, [], [f3918])).
fof(f3918, plain, ($false | (~ spl48_24 | spl48_237)), inference(trivial_inequality_removal, [], [f3917])).
fof(f3917, plain, (~ (h3(e13) = h3(e13)) | (~ spl48_24 | spl48_237)), inference(forward_demodulation, [], [f2057, f758])).
fof(f758, plain, ((e13 = op1(e12, e12)) | ~ spl48_24), inference(avatar_component_clause, [], [f756])).
fof(f756, plain, (spl48_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_24])])).
fof(f2057, plain, (~ (h3(e13) = h3(op1(e12, e12))) | spl48_237), inference(avatar_component_clause, [], [f2055])).
fof(f2055, plain, (spl48_237 <=> (h3(e13) = h3(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl48_237])])).
fof(f3916, plain, (~ spl48_17 | spl48_236), inference(avatar_contradiction_clause, [], [f3915])).
fof(f3915, plain, ($false | (~ spl48_17 | spl48_236)), inference(subsumption_resolution, [], [f3914, f1641])).
fof(f3914, plain, (~ (e20 = h3(e10)) | (~ spl48_17 | spl48_236)), inference(forward_demodulation, [], [f2053, f729])).
fof(f729, plain, ((e10 = op1(e12, e13)) | ~ spl48_17), inference(avatar_component_clause, [], [f727])).
fof(f727, plain, (spl48_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_17])])).
fof(f2053, plain, (~ (e20 = h3(op1(e12, e13))) | spl48_236), inference(avatar_component_clause, [], [f2051])).
fof(f2051, plain, (spl48_236 <=> (e20 = h3(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_236])])).
fof(f3910, plain, (~ spl48_30 | spl48_239), inference(avatar_contradiction_clause, [], [f3909])).
fof(f3909, plain, ($false | (~ spl48_30 | spl48_239)), inference(trivial_inequality_removal, [], [f3908])).
fof(f3908, plain, (~ (h3(e11) = h3(e11)) | (~ spl48_30 | spl48_239)), inference(forward_demodulation, [], [f2065, f784])).
fof(f784, plain, ((e11 = op1(e12, e10)) | ~ spl48_30), inference(avatar_component_clause, [], [f782])).
fof(f782, plain, (spl48_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_30])])).
fof(f2065, plain, (~ (h3(e11) = h3(op1(e12, e10))) | spl48_239), inference(avatar_component_clause, [], [f2063])).
fof(f2063, plain, (spl48_239 <=> (h3(e11) = h3(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl48_239])])).
fof(f3907, plain, (~ spl48_16 | ~ spl48_80 | ~ spl48_233 | spl48_235), inference(avatar_contradiction_clause, [], [f3906])).
fof(f3906, plain, ($false | (~ spl48_16 | ~ spl48_80 | ~ spl48_233 | spl48_235)), inference(subsumption_resolution, [], [f3905, f1028])).
fof(f1028, plain, ((e23 = op2(e23, e20)) | ~ spl48_80), inference(avatar_component_clause, [], [f1026])).
fof(f1026, plain, (spl48_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_80])])).
fof(f3905, plain, (~ (e23 = op2(e23, e20)) | (~ spl48_16 | ~ spl48_233 | spl48_235)), inference(forward_demodulation, [], [f3904, f2040])).
fof(f3904, plain, (~ (op2(e23, e20) = h3(e13)) | (~ spl48_16 | ~ spl48_233 | spl48_235)), inference(forward_demodulation, [], [f3903, f724])).
fof(f3903, plain, (~ (op2(e23, e20) = h3(op1(e13, e10))) | (~ spl48_233 | spl48_235)), inference(forward_demodulation, [], [f2049, f2040])).
fof(f2049, plain, (~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | spl48_235), inference(avatar_component_clause, [], [f2047])).
fof(f2047, plain, (spl48_235 <=> (h3(op1(e13, e10)) = op2(h3(e13), e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_235])])).
fof(f3896, plain, (~ spl48_39 | ~ spl48_103 | ~ spl48_182 | spl48_240), inference(avatar_contradiction_clause, [], [f3895])).
fof(f3895, plain, ($false | (~ spl48_39 | ~ spl48_103 | ~ spl48_182 | spl48_240)), inference(subsumption_resolution, [], [f3894, f1126])).
fof(f1126, plain, ((e22 = op2(e21, e22)) | ~ spl48_103), inference(avatar_component_clause, [], [f1124])).
fof(f1124, plain, (spl48_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_103])])).
fof(f3894, plain, (~ (e22 = op2(e21, e22)) | (~ spl48_39 | ~ spl48_182 | spl48_240)), inference(forward_demodulation, [], [f3893, f554])).
fof(f3893, plain, (~ (op2(e21, e22) = h3(e12)) | (~ spl48_39 | ~ spl48_182 | spl48_240)), inference(forward_demodulation, [], [f3892, f822])).
fof(f3892, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | (~ spl48_182 | spl48_240)), inference(forward_demodulation, [], [f2069, f1740])).
fof(f3891, plain, (~ spl48_6 | ~ spl48_70 | ~ spl48_182 | ~ spl48_233 | spl48_234), inference(avatar_contradiction_clause, [], [f3890])).
fof(f3890, plain, ($false | (~ spl48_6 | ~ spl48_70 | ~ spl48_182 | ~ spl48_233 | spl48_234)), inference(subsumption_resolution, [], [f3889, f986])).
fof(f986, plain, ((e21 = op2(e23, e22)) | ~ spl48_70), inference(avatar_component_clause, [], [f984])).
fof(f984, plain, (spl48_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_70])])).
fof(f3889, plain, (~ (e21 = op2(e23, e22)) | (~ spl48_6 | ~ spl48_182 | ~ spl48_233 | spl48_234)), inference(forward_demodulation, [], [f3888, f1740])).
fof(f3888, plain, (~ (op2(e23, e22) = h3(e11)) | (~ spl48_6 | ~ spl48_233 | spl48_234)), inference(forward_demodulation, [], [f3887, f682])).
fof(f682, plain, ((e11 = op1(e13, e12)) | ~ spl48_6), inference(avatar_component_clause, [], [f680])).
fof(f680, plain, (spl48_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_6])])).
fof(f3887, plain, (~ (op2(e23, e22) = h3(op1(e13, e12))) | (~ spl48_233 | spl48_234)), inference(forward_demodulation, [], [f2045, f2040])).
fof(f2045, plain, (~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | spl48_234), inference(avatar_component_clause, [], [f2043])).
fof(f2043, plain, (spl48_234 <=> (h3(op1(e13, e12)) = op2(h3(e13), e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_234])])).
fof(f3880, plain, (~ spl48_45 | ~ spl48_109 | ~ spl48_182 | spl48_241), inference(avatar_contradiction_clause, [], [f3879])).
fof(f3879, plain, ($false | (~ spl48_45 | ~ spl48_109 | ~ spl48_182 | spl48_241)), inference(subsumption_resolution, [], [f3878, f1152])).
fof(f1152, plain, ((e20 = op2(e21, e20)) | ~ spl48_109), inference(avatar_component_clause, [], [f1150])).
fof(f1150, plain, (spl48_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_109])])).
fof(f3878, plain, (~ (e20 = op2(e21, e20)) | (~ spl48_45 | ~ spl48_182 | spl48_241)), inference(forward_demodulation, [], [f3877, f1641])).
fof(f3877, plain, (~ (op2(e21, e20) = h3(e10)) | (~ spl48_45 | ~ spl48_182 | spl48_241)), inference(forward_demodulation, [], [f3876, f848])).
fof(f3876, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | (~ spl48_182 | spl48_241)), inference(forward_demodulation, [], [f2073, f1740])).
fof(f3875, plain, (~ spl48_3 | ~ spl48_67 | spl48_232 | ~ spl48_233), inference(avatar_contradiction_clause, [], [f3874])).
fof(f3874, plain, ($false | (~ spl48_3 | ~ spl48_67 | spl48_232 | ~ spl48_233)), inference(subsumption_resolution, [], [f3873, f973])).
fof(f3873, plain, (~ (e22 = op2(e23, e23)) | (~ spl48_3 | spl48_232 | ~ spl48_233)), inference(forward_demodulation, [], [f3872, f554])).
fof(f3872, plain, (~ (op2(e23, e23) = h3(e12)) | (~ spl48_3 | spl48_232 | ~ spl48_233)), inference(forward_demodulation, [], [f3871, f669])).
fof(f3871, plain, (~ (op2(e23, e23) = h3(op1(e13, e13))) | (spl48_232 | ~ spl48_233)), inference(forward_demodulation, [], [f2037, f2040])).
fof(f2037, plain, (~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | spl48_232), inference(avatar_component_clause, [], [f2035])).
fof(f2035, plain, (spl48_232 <=> (h3(op1(e13, e13)) = op2(h3(e13), h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_232])])).
fof(f3863, plain, (~ spl48_50 | ~ spl48_114 | ~ spl48_182 | ~ spl48_233 | spl48_242), inference(avatar_contradiction_clause, [], [f3862])).
fof(f3862, plain, ($false | (~ spl48_50 | ~ spl48_114 | ~ spl48_182 | ~ spl48_233 | spl48_242)), inference(subsumption_resolution, [], [f3861, f1173])).
fof(f1173, plain, ((e21 = op2(e20, e23)) | ~ spl48_114), inference(avatar_component_clause, [], [f1171])).
fof(f1171, plain, (spl48_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_114])])).
fof(f3861, plain, (~ (e21 = op2(e20, e23)) | (~ spl48_50 | ~ spl48_182 | ~ spl48_233 | spl48_242)), inference(forward_demodulation, [], [f3860, f1740])).
fof(f3860, plain, (~ (op2(e20, e23) = h3(e11)) | (~ spl48_50 | ~ spl48_233 | spl48_242)), inference(forward_demodulation, [], [f3859, f869])).
fof(f3859, plain, (~ (op2(e20, e23) = h3(op1(e10, e13))) | (~ spl48_233 | spl48_242)), inference(forward_demodulation, [], [f2077, f2040])).
fof(f3858, plain, (~ spl48_9 | ~ spl48_73 | ~ spl48_182 | spl48_231 | ~ spl48_233), inference(avatar_contradiction_clause, [], [f3857])).
fof(f3857, plain, ($false | (~ spl48_9 | ~ spl48_73 | ~ spl48_182 | spl48_231 | ~ spl48_233)), inference(subsumption_resolution, [], [f3856, f999])).
fof(f3856, plain, (~ (e20 = op2(e23, e21)) | (~ spl48_9 | ~ spl48_182 | spl48_231 | ~ spl48_233)), inference(forward_demodulation, [], [f3855, f1641])).
fof(f3855, plain, (~ (op2(e23, e21) = h3(e10)) | (~ spl48_9 | ~ spl48_182 | spl48_231 | ~ spl48_233)), inference(forward_demodulation, [], [f3854, f695])).
fof(f695, plain, ((e10 = op1(e13, e11)) | ~ spl48_9), inference(avatar_component_clause, [], [f693])).
fof(f693, plain, (spl48_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_9])])).
fof(f3854, plain, (~ (op2(e23, e21) = h3(op1(e13, e11))) | (~ spl48_182 | spl48_231 | ~ spl48_233)), inference(forward_demodulation, [], [f3853, f2040])).
fof(f3853, plain, (~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | (~ spl48_182 | spl48_231)), inference(forward_demodulation, [], [f2033, f1740])).
fof(f2033, plain, (~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | spl48_231), inference(avatar_component_clause, [], [f2031])).
fof(f2031, plain, (spl48_231 <=> (h3(op1(e13, e11)) = op2(h3(e13), h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_231])])).
fof(f3846, plain, (~ spl48_53 | ~ spl48_117 | spl48_243), inference(avatar_contradiction_clause, [], [f3845])).
fof(f3845, plain, ($false | (~ spl48_53 | ~ spl48_117 | spl48_243)), inference(subsumption_resolution, [], [f3844, f1186])).
fof(f1186, plain, ((e20 = op2(e20, e22)) | ~ spl48_117), inference(avatar_component_clause, [], [f1184])).
fof(f1184, plain, (spl48_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_117])])).
fof(f3844, plain, (~ (e20 = op2(e20, e22)) | (~ spl48_53 | spl48_243)), inference(forward_demodulation, [], [f3843, f1641])).
fof(f3843, plain, (~ (op2(e20, e22) = h3(e10)) | (~ spl48_53 | spl48_243)), inference(forward_demodulation, [], [f2081, f882])).
fof(f3812, plain, (~ spl48_63 | ~ spl48_199 | spl48_245), inference(avatar_contradiction_clause, [], [f3811])).
fof(f3811, plain, ($false | (~ spl48_63 | ~ spl48_199 | spl48_245)), inference(subsumption_resolution, [], [f3810, f1828])).
fof(f1828, plain, ((e22 = h1(e13)) | ~ spl48_199), inference(avatar_component_clause, [], [f1827])).
fof(f1827, plain, (spl48_199 <=> (e22 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_199])])).
fof(f3810, plain, (~ (e22 = h1(e13)) | (~ spl48_63 | spl48_245)), inference(forward_demodulation, [], [f3809, f554])).
fof(f3809, plain, (~ (h1(e13) = h3(e12)) | (~ spl48_63 | spl48_245)), inference(forward_demodulation, [], [f2089, f924])).
fof(f3797, plain, (~ spl48_5 | ~ spl48_9), inference(avatar_split_clause, [], [f3790, f693, f676])).
fof(f676, plain, (spl48_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_5])])).
fof(f3790, plain, (~ (e10 = op1(e13, e12)) | ~ spl48_9), inference(backward_demodulation, [], [f261, f695])).
fof(f261, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3789, plain, (~ spl48_4 | ~ spl48_16), inference(avatar_split_clause, [], [f3786, f722, f671])).
fof(f671, plain, (spl48_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_4])])).
fof(f3786, plain, (~ (e13 = op1(e13, e13)) | ~ spl48_16), inference(backward_demodulation, [], [f260, f724])).
fof(f260, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3780, plain, (~ spl48_4 | ~ spl48_36), inference(avatar_split_clause, [], [f3777, f807, f671])).
fof(f3777, plain, (~ (e13 = op1(e13, e13)) | ~ spl48_36), inference(backward_demodulation, [], [f238, f809])).
fof(f238, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3776, plain, (~ spl48_42 | ~ spl48_106 | ~ spl48_182 | spl48_229), inference(avatar_contradiction_clause, [], [f3775])).
fof(f3775, plain, ($false | (~ spl48_42 | ~ spl48_106 | ~ spl48_182 | spl48_229)), inference(subsumption_resolution, [], [f3769, f1740])).
fof(f3769, plain, (~ (e21 = h3(e11)) | (~ spl48_42 | ~ spl48_106 | ~ spl48_182 | spl48_229)), inference(backward_demodulation, [], [f3616, f835])).
fof(f3616, plain, (~ (e21 = h3(op1(e11, e11))) | (~ spl48_106 | ~ spl48_182 | spl48_229)), inference(forward_demodulation, [], [f3615, f1139])).
fof(f1139, plain, ((e21 = op2(e21, e21)) | ~ spl48_106), inference(avatar_component_clause, [], [f1137])).
fof(f1137, plain, (spl48_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_106])])).
fof(f3615, plain, (~ (op2(e21, e21) = h3(op1(e11, e11))) | (~ spl48_182 | spl48_229)), inference(forward_demodulation, [], [f2025, f1740])).
fof(f3771, plain, (~ spl48_34 | ~ spl48_42), inference(avatar_split_clause, [], [f3764, f833, f799])).
fof(f3764, plain, (~ (e11 = op1(e11, e13)) | ~ spl48_42), inference(backward_demodulation, [], [f250, f835])).
fof(f250, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3770, plain, (~ spl48_10 | ~ spl48_42), inference(avatar_split_clause, [], [f3763, f833, f697])).
fof(f697, plain, (spl48_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_10])])).
fof(f3763, plain, (~ (e11 = op1(e13, e11)) | ~ spl48_42), inference(backward_demodulation, [], [f226, f835])).
fof(f3739, plain, (~ spl48_5 | ~ spl48_53), inference(avatar_split_clause, [], [f3732, f880, f676])).
fof(f3732, plain, (~ (e10 = op1(e13, e12)) | ~ spl48_53), inference(backward_demodulation, [], [f230, f882])).
fof(f230, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3700, plain, (~ spl48_35 | ~ spl48_39), inference(avatar_split_clause, [], [f3699, f820, f803])).
fof(f803, plain, (spl48_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_35])])).
fof(f3699, plain, (~ (e12 = op1(e11, e13)) | ~ spl48_39), inference(forward_demodulation, [], [f251, f822])).
fof(f251, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3673, plain, (~ spl48_17 | ~ spl48_19), inference(avatar_contradiction_clause, [], [f3672])).
fof(f3672, plain, ($false | (~ spl48_17 | ~ spl48_19)), inference(subsumption_resolution, [], [f3671, f313])).
fof(f313, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f3671, plain, ((e10 = e12) | (~ spl48_17 | ~ spl48_19)), inference(forward_demodulation, [], [f737, f729])).
fof(f737, plain, ((e12 = op1(e12, e13)) | ~ spl48_19), inference(avatar_component_clause, [], [f735])).
fof(f735, plain, (spl48_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_19])])).
fof(f3643, plain, (~ spl48_44 | ~ spl48_48), inference(avatar_split_clause, [], [f3638, f858, f841])).
fof(f858, plain, (spl48_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_48])])).
fof(f3638, plain, (~ (e13 = op1(e11, e11)) | ~ spl48_48), inference(backward_demodulation, [], [f246, f860])).
fof(f860, plain, ((e13 = op1(e11, e10)) | ~ spl48_48), inference(avatar_component_clause, [], [f858])).
fof(f246, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f3633, plain, (~ spl48_6 | ~ spl48_54), inference(avatar_split_clause, [], [f3628, f884, f680])).
fof(f884, plain, (spl48_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_54])])).
fof(f3628, plain, (~ (e11 = op1(e13, e12)) | ~ spl48_54), inference(backward_demodulation, [], [f230, f886])).
fof(f886, plain, ((e11 = op1(e10, e12)) | ~ spl48_54), inference(avatar_component_clause, [], [f884])).
fof(f3624, plain, (~ spl48_44 | ~ spl48_60), inference(avatar_split_clause, [], [f3618, f909, f841])).
fof(f3618, plain, (~ (e13 = op1(e11, e11)) | ~ spl48_60), inference(backward_demodulation, [], [f222, f911])).
fof(f3567, plain, (~ spl48_61 | ~ spl48_64), inference(avatar_contradiction_clause, [], [f3566])).
fof(f3566, plain, ($false | (~ spl48_61 | ~ spl48_64)), inference(subsumption_resolution, [], [f3565, f314])).
fof(f3565, plain, ((e10 = e13) | (~ spl48_61 | ~ spl48_64)), inference(forward_demodulation, [], [f928, f916])).
fof(f928, plain, ((op1(e10, e10) = e13) | ~ spl48_64), inference(avatar_component_clause, [], [f926])).
fof(f926, plain, (spl48_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl48_64])])).
fof(f3551, plain, (~ spl48_30 | spl48_224), inference(avatar_contradiction_clause, [], [f3550])).
fof(f3550, plain, ($false | (~ spl48_30 | spl48_224)), inference(trivial_inequality_removal, [], [f3549])).
fof(f3549, plain, (~ (h4(e11) = h4(e11)) | (~ spl48_30 | spl48_224)), inference(forward_demodulation, [], [f1952, f784])).
fof(f1952, plain, (~ (h4(e11) = h4(op1(e12, e10))) | spl48_224), inference(avatar_component_clause, [], [f1950])).
fof(f1950, plain, (spl48_224 <=> (h4(e11) = h4(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl48_224])])).
fof(f3542, plain, (~ spl48_37 | ~ spl48_114 | ~ spl48_172 | ~ spl48_175 | spl48_225), inference(avatar_contradiction_clause, [], [f3541])).
fof(f3541, plain, ($false | (~ spl48_37 | ~ spl48_114 | ~ spl48_172 | ~ spl48_175 | spl48_225)), inference(subsumption_resolution, [], [f3540, f1173])).
fof(f3540, plain, (~ (e21 = op2(e20, e23)) | (~ spl48_37 | ~ spl48_172 | ~ spl48_175 | spl48_225)), inference(forward_demodulation, [], [f3539, f1688])).
fof(f3539, plain, (~ (op2(e20, e23) = h4(e10)) | (~ spl48_37 | ~ spl48_175 | spl48_225)), inference(forward_demodulation, [], [f3538, f814])).
fof(f3538, plain, (~ (op2(e20, e23) = h4(op1(e11, e12))) | (~ spl48_175 | spl48_225)), inference(forward_demodulation, [], [f1956, f1703])).
fof(f3537, plain, (~ spl48_27 | ~ spl48_80 | ~ spl48_175 | spl48_223), inference(avatar_contradiction_clause, [], [f3536])).
fof(f3536, plain, ($false | (~ spl48_27 | ~ spl48_80 | ~ spl48_175 | spl48_223)), inference(subsumption_resolution, [], [f3535, f1028])).
fof(f3535, plain, (~ (e23 = op2(e23, e20)) | (~ spl48_27 | ~ spl48_175 | spl48_223)), inference(forward_demodulation, [], [f3534, f558])).
fof(f3534, plain, (~ (op2(e23, e20) = h4(e12)) | (~ spl48_27 | ~ spl48_175 | spl48_223)), inference(forward_demodulation, [], [f3533, f771])).
fof(f3533, plain, (~ (op2(e23, e20) = h4(op1(e12, e11))) | (~ spl48_175 | spl48_223)), inference(forward_demodulation, [], [f1948, f1703])).
fof(f1948, plain, (~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | spl48_223), inference(avatar_component_clause, [], [f1946])).
fof(f1946, plain, (spl48_223 <=> (h4(op1(e12, e11)) = op2(e23, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_223])])).
fof(f3526, plain, (~ spl48_55 | ~ spl48_100 | ~ spl48_172 | spl48_226), inference(avatar_contradiction_clause, [], [f3525])).
fof(f3525, plain, ($false | (~ spl48_55 | ~ spl48_100 | ~ spl48_172 | spl48_226)), inference(subsumption_resolution, [], [f3524, f1113])).
fof(f1113, plain, ((e23 = op2(e21, e23)) | ~ spl48_100), inference(avatar_component_clause, [], [f1111])).
fof(f3524, plain, (~ (e23 = op2(e21, e23)) | (~ spl48_55 | ~ spl48_172 | spl48_226)), inference(forward_demodulation, [], [f3523, f558])).
fof(f3523, plain, (~ (op2(e21, e23) = h4(e12)) | (~ spl48_55 | ~ spl48_172 | spl48_226)), inference(forward_demodulation, [], [f3522, f890])).
fof(f3522, plain, (~ (op2(e21, e23) = h4(op1(e10, e12))) | (~ spl48_172 | spl48_226)), inference(forward_demodulation, [], [f1960, f1688])).
fof(f3512, plain, (~ spl48_6 | ~ spl48_81 | ~ spl48_166 | ~ spl48_175 | spl48_220), inference(avatar_contradiction_clause, [], [f3511])).
fof(f3511, plain, ($false | (~ spl48_6 | ~ spl48_81 | ~ spl48_166 | ~ spl48_175 | spl48_220)), inference(subsumption_resolution, [], [f3510, f1033])).
fof(f3510, plain, (~ (e20 = op2(e22, e23)) | (~ spl48_6 | ~ spl48_166 | ~ spl48_175 | spl48_220)), inference(forward_demodulation, [], [f3509, f1703])).
fof(f3509, plain, (~ (op2(e22, e23) = h4(e11)) | (~ spl48_6 | ~ spl48_166 | spl48_220)), inference(forward_demodulation, [], [f3508, f682])).
fof(f3508, plain, (~ (op2(e22, e23) = h4(op1(e13, e12))) | (~ spl48_166 | spl48_220)), inference(forward_demodulation, [], [f1936, f1657])).
fof(f1936, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | spl48_220), inference(avatar_component_clause, [], [f1934])).
fof(f1934, plain, (spl48_220 <=> (h4(op1(e13, e12)) = op2(h4(e13), e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_220])])).
fof(f3499, plain, (~ spl48_34 | ~ spl48_117 | ~ spl48_166 | ~ spl48_175 | spl48_215), inference(avatar_contradiction_clause, [], [f3498])).
fof(f3498, plain, ($false | (~ spl48_34 | ~ spl48_117 | ~ spl48_166 | ~ spl48_175 | spl48_215)), inference(subsumption_resolution, [], [f3497, f1186])).
fof(f3497, plain, (~ (e20 = op2(e20, e22)) | (~ spl48_34 | ~ spl48_166 | ~ spl48_175 | spl48_215)), inference(forward_demodulation, [], [f3496, f1703])).
fof(f3496, plain, (~ (op2(e20, e22) = h4(e11)) | (~ spl48_34 | ~ spl48_166 | ~ spl48_175 | spl48_215)), inference(forward_demodulation, [], [f3495, f801])).
fof(f3495, plain, (~ (op2(e20, e22) = h4(op1(e11, e13))) | (~ spl48_166 | ~ spl48_175 | spl48_215)), inference(forward_demodulation, [], [f3494, f1703])).
fof(f3494, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e22)) | (~ spl48_166 | spl48_215)), inference(forward_demodulation, [], [f1916, f1657])).
fof(f3485, plain, (~ spl48_24 | spl48_222), inference(avatar_contradiction_clause, [], [f3484])).
fof(f3484, plain, ($false | (~ spl48_24 | spl48_222)), inference(trivial_inequality_removal, [], [f3483])).
fof(f3483, plain, (~ (h4(e13) = h4(e13)) | (~ spl48_24 | spl48_222)), inference(forward_demodulation, [], [f1944, f758])).
fof(f1944, plain, (~ (h4(e13) = h4(op1(e12, e12))) | spl48_222), inference(avatar_component_clause, [], [f1942])).
fof(f1942, plain, (spl48_222 <=> (h4(e13) = h4(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl48_222])])).
fof(f3476, plain, (~ spl48_44 | ~ spl48_127 | ~ spl48_166 | ~ spl48_175 | spl48_214), inference(avatar_contradiction_clause, [], [f3475])).
fof(f3475, plain, ($false | (~ spl48_44 | ~ spl48_127 | ~ spl48_166 | ~ spl48_175 | spl48_214)), inference(subsumption_resolution, [], [f3474, f1228])).
fof(f1228, plain, ((op2(e20, e20) = e22) | ~ spl48_127), inference(avatar_component_clause, [], [f1226])).
fof(f1226, plain, (spl48_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl48_127])])).
fof(f3474, plain, (~ (op2(e20, e20) = e22) | (~ spl48_44 | ~ spl48_166 | ~ spl48_175 | spl48_214)), inference(forward_demodulation, [], [f3473, f1657])).
fof(f3473, plain, (~ (op2(e20, e20) = h4(e13)) | (~ spl48_44 | ~ spl48_175 | spl48_214)), inference(forward_demodulation, [], [f3472, f843])).
fof(f3472, plain, (~ (op2(e20, e20) = h4(op1(e11, e11))) | (~ spl48_175 | spl48_214)), inference(forward_demodulation, [], [f1912, f1703])).
fof(f3455, plain, (~ spl48_3 | ~ spl48_88 | ~ spl48_166 | spl48_218), inference(avatar_contradiction_clause, [], [f3454])).
fof(f3454, plain, ($false | (~ spl48_3 | ~ spl48_88 | ~ spl48_166 | spl48_218)), inference(subsumption_resolution, [], [f3453, f1062])).
fof(f3453, plain, (~ (e23 = op2(e22, e22)) | (~ spl48_3 | ~ spl48_166 | spl48_218)), inference(forward_demodulation, [], [f3452, f558])).
fof(f3452, plain, (~ (op2(e22, e22) = h4(e12)) | (~ spl48_3 | ~ spl48_166 | spl48_218)), inference(forward_demodulation, [], [f3451, f669])).
fof(f3451, plain, (~ (op2(e22, e22) = h4(op1(e13, e13))) | (~ spl48_166 | spl48_218)), inference(forward_demodulation, [], [f1928, f1657])).
fof(f1928, plain, (~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | spl48_218), inference(avatar_component_clause, [], [f1926])).
fof(f3444, plain, (~ spl48_58 | ~ spl48_109 | ~ spl48_172 | ~ spl48_175 | spl48_211), inference(avatar_contradiction_clause, [], [f3443])).
fof(f3443, plain, ($false | (~ spl48_58 | ~ spl48_109 | ~ spl48_172 | ~ spl48_175 | spl48_211)), inference(subsumption_resolution, [], [f3442, f1152])).
fof(f3442, plain, (~ (e20 = op2(e21, e20)) | (~ spl48_58 | ~ spl48_172 | ~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f3441, f1703])).
fof(f3441, plain, (~ (op2(e21, e20) = h4(e11)) | (~ spl48_58 | ~ spl48_172 | ~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f3440, f903])).
fof(f3440, plain, (~ (op2(e21, e20) = h4(op1(e10, e11))) | (~ spl48_172 | ~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f3439, f1688])).
fof(f3439, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e20)) | (~ spl48_175 | spl48_211)), inference(forward_demodulation, [], [f1900, f1703])).
fof(f3431, plain, (~ spl48_47 | ~ spl48_124 | ~ spl48_172 | ~ spl48_175 | spl48_213), inference(avatar_contradiction_clause, [], [f3430])).
fof(f3430, plain, ($false | (~ spl48_47 | ~ spl48_124 | ~ spl48_172 | ~ spl48_175 | spl48_213)), inference(subsumption_resolution, [], [f3429, f1215])).
fof(f1215, plain, ((e23 = op2(e20, e21)) | ~ spl48_124), inference(avatar_component_clause, [], [f1213])).
fof(f1213, plain, (spl48_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_124])])).
fof(f3429, plain, (~ (e23 = op2(e20, e21)) | (~ spl48_47 | ~ spl48_172 | ~ spl48_175 | spl48_213)), inference(forward_demodulation, [], [f3428, f558])).
fof(f3428, plain, (~ (op2(e20, e21) = h4(e12)) | (~ spl48_47 | ~ spl48_172 | ~ spl48_175 | spl48_213)), inference(forward_demodulation, [], [f3427, f856])).
fof(f3427, plain, (~ (op2(e20, e21) = h4(op1(e11, e10))) | (~ spl48_172 | ~ spl48_175 | spl48_213)), inference(forward_demodulation, [], [f3426, f1703])).
fof(f3426, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e21)) | (~ spl48_172 | spl48_213)), inference(forward_demodulation, [], [f1908, f1688])).
fof(f3415, plain, (~ spl48_17 | spl48_221), inference(avatar_contradiction_clause, [], [f3414])).
fof(f3414, plain, ($false | (~ spl48_17 | spl48_221)), inference(trivial_inequality_removal, [], [f3413])).
fof(f3413, plain, (~ (h4(e10) = h4(e10)) | (~ spl48_17 | spl48_221)), inference(forward_demodulation, [], [f1940, f729])).
fof(f1940, plain, (~ (h4(e10) = h4(op1(e12, e13))) | spl48_221), inference(avatar_component_clause, [], [f1938])).
fof(f1938, plain, (spl48_221 <=> (h4(e10) = h4(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl48_221])])).
fof(f3403, plain, (~ spl48_52 | ~ spl48_103 | ~ spl48_166 | ~ spl48_172 | spl48_212), inference(avatar_contradiction_clause, [], [f3402])).
fof(f3402, plain, ($false | (~ spl48_52 | ~ spl48_103 | ~ spl48_166 | ~ spl48_172 | spl48_212)), inference(subsumption_resolution, [], [f3401, f1126])).
fof(f3401, plain, (~ (e22 = op2(e21, e22)) | (~ spl48_52 | ~ spl48_166 | ~ spl48_172 | spl48_212)), inference(forward_demodulation, [], [f3400, f1657])).
fof(f3400, plain, (~ (op2(e21, e22) = h4(e13)) | (~ spl48_52 | ~ spl48_166 | ~ spl48_172 | spl48_212)), inference(forward_demodulation, [], [f3399, f877])).
fof(f3399, plain, (~ (op2(e21, e22) = h4(op1(e10, e13))) | (~ spl48_166 | ~ spl48_172 | spl48_212)), inference(forward_demodulation, [], [f3398, f1688])).
fof(f3398, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e22)) | (~ spl48_166 | spl48_212)), inference(forward_demodulation, [], [f1904, f1657])).
fof(f3383, plain, (~ spl48_9 | ~ spl48_94 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_217), inference(avatar_contradiction_clause, [], [f3382])).
fof(f3382, plain, ($false | (~ spl48_9 | ~ spl48_94 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_217)), inference(subsumption_resolution, [], [f3378, f1088])).
fof(f1088, plain, ((e21 = op2(e22, e20)) | ~ spl48_94), inference(avatar_component_clause, [], [f1086])).
fof(f1086, plain, (spl48_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_94])])).
fof(f3378, plain, (~ (e21 = op2(e22, e20)) | (~ spl48_9 | ~ spl48_166 | ~ spl48_172 | ~ spl48_175 | spl48_217)), inference(backward_demodulation, [], [f3373, f1703])).
fof(f3373, plain, (~ (e21 = op2(e22, h4(e11))) | (~ spl48_9 | ~ spl48_166 | ~ spl48_172 | spl48_217)), inference(forward_demodulation, [], [f3372, f1688])).
fof(f3372, plain, (~ (h4(e10) = op2(e22, h4(e11))) | (~ spl48_9 | ~ spl48_166 | spl48_217)), inference(forward_demodulation, [], [f3371, f695])).
fof(f3371, plain, (~ (h4(op1(e13, e11)) = op2(e22, h4(e11))) | (~ spl48_166 | spl48_217)), inference(forward_demodulation, [], [f1924, f1657])).
fof(f1924, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | spl48_217), inference(avatar_component_clause, [], [f1922])).
fof(f1922, plain, (spl48_217 <=> (h4(op1(e13, e11)) = op2(h4(e13), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_217])])).
fof(f3360, plain, (spl48_175 | ~ spl48_73 | ~ spl48_172), inference(avatar_split_clause, [], [f3339, f1687, f997, f1702])).
fof(f3339, plain, ((e20 = h4(e11)) | (~ spl48_73 | ~ spl48_172)), inference(forward_demodulation, [], [f3338, f999])).
fof(f3338, plain, ((op2(e23, e21) = h4(e11)) | ~ spl48_172), inference(forward_demodulation, [], [f1650, f1688])).
fof(f1650, plain, (h4(e11) = op2(e23, h4(e10))), inference(backward_demodulation, [], [f1648, f1649])).
fof(f1649, plain, (h4(e10) = op2(e23, h4(e13))), inference(forward_demodulation, [], [f559, f561])).
fof(f559, plain, (h4(e10) = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f17])).
fof(f1648, plain, (h4(e11) = op2(e23, op2(e23, h4(e13)))), inference(forward_demodulation, [], [f560, f561])).
fof(f560, plain, (h4(e11) = op2(e23, op2(e23, op2(e23, e23)))), inference(cnf_transformation, [], [f17])).
fof(f3356, plain, (~ spl48_16 | ~ spl48_91 | ~ spl48_166 | ~ spl48_172 | spl48_216), inference(avatar_contradiction_clause, [], [f3355])).
fof(f3355, plain, ($false | (~ spl48_16 | ~ spl48_91 | ~ spl48_166 | ~ spl48_172 | spl48_216)), inference(subsumption_resolution, [], [f3354, f1075])).
fof(f3354, plain, (~ (e22 = op2(e22, e21)) | (~ spl48_16 | ~ spl48_166 | ~ spl48_172 | spl48_216)), inference(forward_demodulation, [], [f3353, f1657])).
fof(f3353, plain, (~ (op2(e22, e21) = h4(e13)) | (~ spl48_16 | ~ spl48_166 | ~ spl48_172 | spl48_216)), inference(forward_demodulation, [], [f3352, f724])).
fof(f3352, plain, (~ (op2(e22, e21) = h4(op1(e13, e10))) | (~ spl48_166 | ~ spl48_172 | spl48_216)), inference(forward_demodulation, [], [f3351, f1657])).
fof(f3351, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e21)) | (~ spl48_172 | spl48_216)), inference(forward_demodulation, [], [f1920, f1688])).
fof(f1920, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | spl48_216), inference(avatar_component_clause, [], [f1918])).
fof(f1918, plain, (spl48_216 <=> (h4(op1(e13, e10)) = op2(h4(e13), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl48_216])])).
fof(f3334, plain, (~ spl48_61 | ~ spl48_106 | ~ spl48_172 | spl48_210), inference(avatar_contradiction_clause, [], [f3333])).
fof(f3333, plain, ($false | (~ spl48_61 | ~ spl48_106 | ~ spl48_172 | spl48_210)), inference(subsumption_resolution, [], [f3330, f1139])).
fof(f3330, plain, (~ (e21 = op2(e21, e21)) | (~ spl48_61 | ~ spl48_172 | spl48_210)), inference(backward_demodulation, [], [f2944, f1688])).
fof(f2944, plain, (~ (h4(e10) = op2(h4(e10), h4(e10))) | (~ spl48_61 | spl48_210)), inference(backward_demodulation, [], [f1896, f916])).
fof(f3319, plain, (spl48_172 | ~ spl48_70 | ~ spl48_166), inference(avatar_split_clause, [], [f3318, f1656, f984, f1687])).
fof(f3318, plain, ((e21 = h4(e10)) | (~ spl48_70 | ~ spl48_166)), inference(forward_demodulation, [], [f3316, f986])).
fof(f3316, plain, ((op2(e23, e22) = h4(e10)) | ~ spl48_166), inference(backward_demodulation, [], [f1649, f1657])).
fof(f3305, plain, (spl48_166 | ~ spl48_67), inference(avatar_split_clause, [], [f3290, f971, f1656])).
fof(f3290, plain, ((e22 = h4(e13)) | ~ spl48_67), inference(forward_demodulation, [], [f561, f973])).
fof(f3283, plain, (~ spl48_174 | ~ spl48_81), inference(avatar_split_clause, [], [f3282, f1031, f1696])).
fof(f1696, plain, (spl48_174 <=> (e20 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_174])])).
fof(f3282, plain, (~ (e20 = h4(e13)) | ~ spl48_81), inference(forward_demodulation, [], [f1644, f1033])).
fof(f1644, plain, ~ (op2(e22, e23) = h4(e13)), inference(backward_demodulation, [], [f287, f561])).
fof(f287, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3237, plain, (~ spl48_9 | ~ spl48_10), inference(avatar_contradiction_clause, [], [f3236])).
fof(f3236, plain, ($false | (~ spl48_9 | ~ spl48_10)), inference(subsumption_resolution, [], [f3235, f312])).
fof(f3235, plain, ((e10 = e11) | (~ spl48_9 | ~ spl48_10)), inference(forward_demodulation, [], [f699, f695])).
fof(f699, plain, ((e11 = op1(e13, e11)) | ~ spl48_10), inference(avatar_component_clause, [], [f697])).
fof(f3234, plain, (~ spl48_2 | ~ spl48_34), inference(avatar_split_clause, [], [f3233, f799, f663])).
fof(f663, plain, (spl48_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_2])])).
fof(f3233, plain, (~ (e11 = op1(e13, e13)) | ~ spl48_34), inference(backward_demodulation, [], [f238, f801])).
fof(f3204, plain, (~ spl48_98 | ~ spl48_114), inference(avatar_split_clause, [], [f3201, f1171, f1103])).
fof(f3201, plain, (~ (e21 = op2(e21, e23)) | ~ spl48_114), inference(backward_demodulation, [], [f282, f1173])).
fof(f282, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3197, plain, (~ spl48_116 | ~ spl48_124), inference(avatar_split_clause, [], [f3195, f1213, f1179])).
fof(f3195, plain, (~ (e23 = op2(e20, e23)) | ~ spl48_124), inference(backward_demodulation, [], [f292, f1215])).
fof(f292, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3137, plain, (~ spl48_59 | ~ spl48_27), inference(avatar_split_clause, [], [f3136, f769, f905])).
fof(f905, plain, (spl48_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_59])])).
fof(f3136, plain, (~ (e12 = op1(e10, e11)) | ~ spl48_27), inference(forward_demodulation, [], [f223, f771])).
fof(f223, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3130, plain, (~ spl48_124 | ~ spl48_60 | ~ spl48_182 | ~ spl48_233 | spl48_244), inference(avatar_split_clause, [], [f3129, f2083, f2039, f1739, f909, f1213])).
fof(f2083, plain, (spl48_244 <=> (h3(op1(e10, e11)) = op2(e20, h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl48_244])])).
fof(f3129, plain, (~ (e23 = op2(e20, e21)) | (~ spl48_60 | ~ spl48_182 | ~ spl48_233 | spl48_244)), inference(forward_demodulation, [], [f3128, f2040])).
fof(f3128, plain, (~ (op2(e20, e21) = h3(e13)) | (~ spl48_60 | ~ spl48_182 | spl48_244)), inference(forward_demodulation, [], [f2694, f911])).
fof(f2694, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | (~ spl48_182 | spl48_244)), inference(backward_demodulation, [], [f2085, f1740])).
fof(f2085, plain, (~ (h3(op1(e10, e11)) = op2(e20, h3(e11))) | spl48_244), inference(avatar_component_clause, [], [f2083])).
fof(f3127, plain, (~ spl48_58 | ~ spl48_54), inference(avatar_split_clause, [], [f3126, f884, f901])).
fof(f3126, plain, (~ (e11 = op1(e10, e11)) | ~ spl48_54), inference(forward_demodulation, [], [f243, f886])).
fof(f243, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3097, plain, (~ spl48_37 | ~ spl48_38), inference(avatar_contradiction_clause, [], [f3096])).
fof(f3096, plain, ($false | (~ spl48_37 | ~ spl48_38)), inference(subsumption_resolution, [], [f3095, f312])).
fof(f3095, plain, ((e10 = e11) | (~ spl48_37 | ~ spl48_38)), inference(backward_demodulation, [], [f818, f814])).
fof(f818, plain, ((e11 = op1(e11, e12)) | ~ spl48_38), inference(avatar_component_clause, [], [f816])).
fof(f816, plain, (spl48_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_38])])).
fof(f3082, plain, (~ spl48_73 | ~ spl48_76), inference(avatar_contradiction_clause, [], [f3081])).
fof(f3081, plain, ($false | (~ spl48_73 | ~ spl48_76)), inference(subsumption_resolution, [], [f3080, f320])).
fof(f320, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f3080, plain, ((e20 = e23) | (~ spl48_73 | ~ spl48_76)), inference(backward_demodulation, [], [f1011, f999])).
fof(f1011, plain, ((e23 = op2(e23, e21)) | ~ spl48_76), inference(avatar_component_clause, [], [f1009])).
fof(f1009, plain, (spl48_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_76])])).
fof(f3072, plain, (~ spl48_81 | ~ spl48_83), inference(avatar_contradiction_clause, [], [f3071])).
fof(f3071, plain, ($false | (~ spl48_81 | ~ spl48_83)), inference(subsumption_resolution, [], [f3070, f319])).
fof(f319, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f3070, plain, ((e20 = e22) | (~ spl48_81 | ~ spl48_83)), inference(forward_demodulation, [], [f1041, f1033])).
fof(f1041, plain, ((e22 = op2(e22, e23)) | ~ spl48_83), inference(avatar_component_clause, [], [f1039])).
fof(f1039, plain, (spl48_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_83])])).
fof(f3065, plain, (~ spl48_71 | ~ spl48_103), inference(avatar_split_clause, [], [f3063, f1124, f988])).
fof(f988, plain, (spl48_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_71])])).
fof(f3063, plain, (~ (e22 = op2(e23, e22)) | ~ spl48_103), inference(backward_demodulation, [], [f280, f1126])).
fof(f280, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3064, plain, (~ spl48_99 | ~ spl48_103), inference(avatar_split_clause, [], [f3062, f1124, f1107])).
fof(f1107, plain, (spl48_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_99])])).
fof(f3062, plain, (~ (e22 = op2(e21, e23)) | ~ spl48_103), inference(backward_demodulation, [], [f299, f1126])).
fof(f299, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3060, plain, (~ spl48_109 | ~ spl48_112), inference(avatar_contradiction_clause, [], [f3059])).
fof(f3059, plain, ($false | (~ spl48_109 | ~ spl48_112)), inference(subsumption_resolution, [], [f3058, f320])).
fof(f3058, plain, ((e20 = e23) | (~ spl48_109 | ~ spl48_112)), inference(backward_demodulation, [], [f1164, f1152])).
fof(f1164, plain, ((e23 = op2(e21, e20)) | ~ spl48_112), inference(avatar_component_clause, [], [f1162])).
fof(f3053, plain, (~ spl48_117 | ~ spl48_118), inference(avatar_contradiction_clause, [], [f3052])).
fof(f3052, plain, ($false | (~ spl48_117 | ~ spl48_118)), inference(subsumption_resolution, [], [f3051, f318])).
fof(f3051, plain, ((e20 = e21) | (~ spl48_117 | ~ spl48_118)), inference(backward_demodulation, [], [f1190, f1186])).
fof(f1190, plain, ((e21 = op2(e20, e22)) | ~ spl48_118), inference(avatar_component_clause, [], [f1188])).
fof(f1188, plain, (spl48_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_118])])).
fof(f3030, plain, (~ spl48_113 | ~ spl48_81), inference(avatar_split_clause, [], [f3029, f1031, f1167])).
fof(f1167, plain, (spl48_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_113])])).
fof(f3029, plain, (~ (e20 = op2(e20, e23)) | ~ spl48_81), inference(forward_demodulation, [], [f283, f1033])).
fof(f283, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3024, plain, (~ spl48_98 | ~ spl48_191), inference(avatar_split_clause, [], [f3023, f1786, f1103])).
fof(f1786, plain, (spl48_191 <=> (e21 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_191])])).
fof(f3023, plain, (~ (e21 = op2(e21, e23)) | ~ spl48_191), inference(forward_demodulation, [], [f1622, f1787])).
fof(f1787, plain, ((e21 = h2(e13)) | ~ spl48_191), inference(avatar_component_clause, [], [f1786])).
fof(f1622, plain, ~ (op2(e21, e23) = h2(e13)), inference(backward_demodulation, [], [f298, f553])).
fof(f298, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3022, plain, (~ spl48_98 | ~ spl48_170), inference(avatar_split_clause, [], [f3021, f1676, f1103])).
fof(f1676, plain, (spl48_170 <=> (e21 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_170])])).
fof(f3021, plain, (~ (e21 = op2(e21, e23)) | ~ spl48_170), inference(forward_demodulation, [], [f1643, f1677])).
fof(f1677, plain, ((e21 = h4(e13)) | ~ spl48_170), inference(avatar_component_clause, [], [f1676])).
fof(f1643, plain, ~ (op2(e21, e23) = h4(e13)), inference(backward_demodulation, [], [f286, f561])).
fof(f286, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3018, plain, (~ spl48_91 | ~ spl48_123), inference(avatar_split_clause, [], [f3017, f1209, f1073])).
fof(f1209, plain, (spl48_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_123])])).
fof(f3017, plain, (~ (e22 = op2(e22, e21)) | ~ spl48_123), inference(forward_demodulation, [], [f271, f1211])).
fof(f1211, plain, ((e22 = op2(e20, e21)) | ~ spl48_123), inference(avatar_component_clause, [], [f1209])).
fof(f271, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3011, plain, (~ spl48_92 | ~ spl48_233), inference(avatar_split_clause, [], [f2850, f2039, f1077])).
fof(f1077, plain, (spl48_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_92])])).
fof(f2850, plain, (~ (e23 = op2(e22, e21)) | ~ spl48_233), inference(forward_demodulation, [], [f1630, f2040])).
fof(f1630, plain, ~ (op2(e22, e21) = h3(e13)), inference(backward_demodulation, [], [f303, f557])).
fof(f303, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f3008, plain, (~ spl48_78 | ~ spl48_182), inference(avatar_split_clause, [], [f2834, f1739, f1018])).
fof(f1018, plain, (spl48_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_78])])).
fof(f2834, plain, (~ (e21 = op2(e23, e20)) | ~ spl48_182), inference(forward_demodulation, [], [f1637, f1740])).
fof(f1637, plain, ~ (op2(e23, e20) = h3(e11)), inference(backward_demodulation, [], [f269, f1634])).
fof(f1634, plain, (op2(e22, e20) = h3(e11)), inference(forward_demodulation, [], [f1633, f1632])).
fof(f1633, plain, (h3(e11) = op2(e22, op2(e22, h3(e13)))), inference(forward_demodulation, [], [f556, f557])).
fof(f556, plain, (op2(e22, op2(e22, op2(e22, e22))) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f269, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3001, plain, (~ spl48_70 | ~ spl48_118), inference(avatar_split_clause, [], [f3000, f1188, f984])).
fof(f3000, plain, (~ (e21 = op2(e23, e22)) | ~ spl48_118), inference(forward_demodulation, [], [f278, f1190])).
fof(f278, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2995, plain, (~ spl48_56 | ~ spl48_24), inference(avatar_split_clause, [], [f2822, f756, f892])).
fof(f892, plain, (spl48_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_56])])).
fof(f2822, plain, (~ (e13 = op1(e10, e12)) | ~ spl48_24), inference(forward_demodulation, [], [f229, f758])).
fof(f229, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2988, plain, (~ spl48_43 | ~ spl48_27), inference(avatar_split_clause, [], [f2987, f769, f837])).
fof(f837, plain, (spl48_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_43])])).
fof(f2987, plain, (~ (e12 = op1(e11, e11)) | ~ spl48_27), inference(forward_demodulation, [], [f225, f771])).
fof(f225, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2967, plain, (~ spl48_9 | ~ spl48_12), inference(avatar_contradiction_clause, [], [f2966])).
fof(f2966, plain, ($false | (~ spl48_9 | ~ spl48_12)), inference(subsumption_resolution, [], [f2965, f314])).
fof(f2965, plain, ((e10 = e13) | (~ spl48_9 | ~ spl48_12)), inference(forward_demodulation, [], [f707, f695])).
fof(f707, plain, ((e13 = op1(e13, e11)) | ~ spl48_12), inference(avatar_component_clause, [], [f705])).
fof(f2963, plain, (~ spl48_17 | ~ spl48_18), inference(avatar_contradiction_clause, [], [f2962])).
fof(f2962, plain, ($false | (~ spl48_17 | ~ spl48_18)), inference(subsumption_resolution, [], [f2961, f312])).
fof(f2961, plain, ((e10 = e11) | (~ spl48_17 | ~ spl48_18)), inference(forward_demodulation, [], [f733, f729])).
fof(f733, plain, ((e11 = op1(e12, e13)) | ~ spl48_18), inference(avatar_component_clause, [], [f731])).
fof(f731, plain, (spl48_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_18])])).
fof(f2946, plain, (~ spl48_45 | ~ spl48_61), inference(avatar_split_clause, [], [f2945, f914, f846])).
fof(f2945, plain, (~ (e10 = op1(e11, e10)) | ~ spl48_61), inference(backward_demodulation, [], [f216, f916])).
fof(f216, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f2933, plain, (~ spl48_94 | ~ spl48_95), inference(avatar_contradiction_clause, [], [f2932])).
fof(f2932, plain, ($false | (~ spl48_94 | ~ spl48_95)), inference(subsumption_resolution, [], [f2931, f321])).
fof(f321, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f2931, plain, ((e21 = e22) | (~ spl48_94 | ~ spl48_95)), inference(forward_demodulation, [], [f1092, f1088])).
fof(f1092, plain, ((e22 = op2(e22, e20)) | ~ spl48_95), inference(avatar_component_clause, [], [f1090])).
fof(f1090, plain, (spl48_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_95])])).
fof(f2927, plain, (~ spl48_105 | ~ spl48_106), inference(avatar_contradiction_clause, [], [f2926])).
fof(f2926, plain, ($false | (~ spl48_105 | ~ spl48_106)), inference(subsumption_resolution, [], [f2925, f318])).
fof(f2925, plain, ((e20 = e21) | (~ spl48_105 | ~ spl48_106)), inference(backward_demodulation, [], [f1139, f1135])).
fof(f1135, plain, ((e20 = op2(e21, e21)) | ~ spl48_105), inference(avatar_component_clause, [], [f1133])).
fof(f1133, plain, (spl48_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_105])])).
fof(f2924, plain, (spl48_193 | ~ spl48_106 | ~ spl48_191), inference(avatar_split_clause, [], [f2922, f1786, f1137, f1798])).
fof(f2922, plain, ((e21 = h2(e10)) | (~ spl48_106 | ~ spl48_191)), inference(backward_demodulation, [], [f2884, f1139])).
fof(f2884, plain, ((op2(e21, e21) = h2(e10)) | ~ spl48_191), inference(backward_demodulation, [], [f1624, f1787])).
fof(f2903, plain, (spl48_199 | ~ spl48_127), inference(avatar_split_clause, [], [f2902, f1226, f1827])).
fof(f2902, plain, ((e22 = h1(e13)) | ~ spl48_127), inference(backward_demodulation, [], [f549, f1228])).
fof(f549, plain, (op2(e20, e20) = h1(e13)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, e20) = h1(e13)) & (h1(e11) = op2(e20, op2(e20, op2(e20, e20)))) & (h1(e10) = op2(e20, op2(e20, e20))) & (e20 = h1(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax14)).
fof(f2891, plain, (~ spl48_102 | ~ spl48_191), inference(avatar_split_clause, [], [f2883, f1786, f1120])).
fof(f2883, plain, (~ (e21 = op2(e21, e22)) | ~ spl48_191), inference(backward_demodulation, [], [f1621, f1787])).
fof(f1621, plain, ~ (op2(e21, e22) = h2(e13)), inference(backward_demodulation, [], [f297, f553])).
fof(f297, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2889, plain, (~ spl48_74 | ~ spl48_191), inference(avatar_split_clause, [], [f2881, f1786, f1001])).
fof(f2881, plain, (~ (e21 = op2(e23, e21)) | ~ spl48_191), inference(backward_demodulation, [], [f1619, f1787])).
fof(f1619, plain, ~ (op2(e23, e21) = h2(e13)), inference(backward_demodulation, [], [f274, f553])).
fof(f274, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2888, plain, (~ spl48_122 | ~ spl48_191), inference(avatar_split_clause, [], [f2880, f1786, f1205])).
fof(f2880, plain, (~ (e21 = op2(e20, e21)) | ~ spl48_191), inference(backward_demodulation, [], [f1617, f1787])).
fof(f1617, plain, ~ (op2(e20, e21) = h2(e13)), inference(backward_demodulation, [], [f270, f553])).
fof(f270, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f2878, plain, (~ spl48_122 | ~ spl48_58 | ~ spl48_182 | spl48_244), inference(avatar_split_clause, [], [f2877, f2083, f1739, f901, f1205])).
fof(f2877, plain, (~ (e21 = op2(e20, e21)) | (~ spl48_58 | ~ spl48_182 | spl48_244)), inference(forward_demodulation, [], [f2876, f1740])).
fof(f2876, plain, (~ (op2(e20, e21) = h3(e11)) | (~ spl48_58 | ~ spl48_182 | spl48_244)), inference(forward_demodulation, [], [f2694, f903])).
fof(f2853, plain, (~ spl48_90 | ~ spl48_182), inference(avatar_split_clause, [], [f2852, f1739, f1069])).
fof(f1069, plain, (spl48_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_90])])).
fof(f2852, plain, (~ (e21 = op2(e22, e21)) | ~ spl48_182), inference(forward_demodulation, [], [f1638, f1740])).
fof(f1638, plain, ~ (op2(e22, e21) = h3(e11)), inference(backward_demodulation, [], [f300, f1634])).
fof(f300, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2847, plain, (~ spl48_75 | ~ spl48_91), inference(avatar_split_clause, [], [f2846, f1073, f1005])).
fof(f1005, plain, (spl48_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_75])])).
fof(f2846, plain, (~ (e22 = op2(e23, e21)) | ~ spl48_91), inference(forward_demodulation, [], [f275, f1075])).
fof(f275, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2809, plain, (~ spl48_46 | ~ spl48_30), inference(avatar_split_clause, [], [f2808, f782, f850])).
fof(f850, plain, (spl48_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_46])])).
fof(f2808, plain, (~ (e11 = op1(e11, e10)) | ~ spl48_30), inference(forward_demodulation, [], [f219, f784])).
fof(f219, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2792, plain, (~ spl48_2 | ~ spl48_6), inference(avatar_split_clause, [], [f2791, f680, f663])).
fof(f2791, plain, (~ (e11 = op1(e13, e13)) | ~ spl48_6), inference(forward_demodulation, [], [f263, f682])).
fof(f263, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2782, plain, (~ spl48_15 | ~ spl48_16), inference(avatar_contradiction_clause, [], [f2781])).
fof(f2781, plain, ($false | (~ spl48_15 | ~ spl48_16)), inference(subsumption_resolution, [], [f2780, f317])).
fof(f317, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2780, plain, ((e12 = e13) | (~ spl48_15 | ~ spl48_16)), inference(backward_demodulation, [], [f724, f720])).
fof(f720, plain, ((e12 = op1(e13, e10)) | ~ spl48_15), inference(avatar_component_clause, [], [f718])).
fof(f718, plain, (spl48_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_15])])).
fof(f2778, plain, (~ spl48_23 | ~ spl48_24), inference(avatar_contradiction_clause, [], [f2777])).
fof(f2777, plain, ($false | (~ spl48_23 | ~ spl48_24)), inference(subsumption_resolution, [], [f2776, f317])).
fof(f2776, plain, ((e12 = e13) | (~ spl48_23 | ~ spl48_24)), inference(backward_demodulation, [], [f758, f754])).
fof(f754, plain, ((e12 = op1(e12, e12)) | ~ spl48_23), inference(avatar_component_clause, [], [f752])).
fof(f752, plain, (spl48_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_23])])).
fof(f2758, plain, (~ spl48_53 | ~ spl48_55), inference(avatar_contradiction_clause, [], [f2757])).
fof(f2757, plain, ($false | (~ spl48_53 | ~ spl48_55)), inference(subsumption_resolution, [], [f2756, f313])).
fof(f2756, plain, ((e10 = e12) | (~ spl48_53 | ~ spl48_55)), inference(forward_demodulation, [], [f890, f882])).
fof(f2741, plain, (spl48_170 | ~ spl48_66), inference(avatar_split_clause, [], [f2740, f967, f1676])).
fof(f967, plain, (spl48_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_66])])).
fof(f2740, plain, ((e21 = h4(e13)) | ~ spl48_66), inference(backward_demodulation, [], [f561, f969])).
fof(f969, plain, ((e21 = op2(e23, e23)) | ~ spl48_66), inference(avatar_component_clause, [], [f967])).
fof(f2730, plain, (~ spl48_87 | ~ spl48_88), inference(avatar_contradiction_clause, [], [f2729])).
fof(f2729, plain, ($false | (~ spl48_87 | ~ spl48_88)), inference(subsumption_resolution, [], [f2728, f323])).
fof(f2728, plain, ((e22 = e23) | (~ spl48_87 | ~ spl48_88)), inference(backward_demodulation, [], [f1062, f1058])).
fof(f1058, plain, ((e22 = op2(e22, e22)) | ~ spl48_87), inference(avatar_component_clause, [], [f1056])).
fof(f1056, plain, (spl48_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_87])])).
fof(f2718, plain, (spl48_256 | ~ spl48_108), inference(avatar_split_clause, [], [f2717, f1145, f2163])).
fof(f2717, plain, ((e23 = h2(e13)) | ~ spl48_108), inference(backward_demodulation, [], [f553, f1147])).
fof(f2709, plain, (~ spl48_117 | ~ spl48_119), inference(avatar_contradiction_clause, [], [f2708])).
fof(f2708, plain, ($false | (~ spl48_117 | ~ spl48_119)), inference(subsumption_resolution, [], [f2707, f319])).
fof(f2707, plain, ((e20 = e22) | (~ spl48_117 | ~ spl48_119)), inference(forward_demodulation, [], [f1194, f1186])).
fof(f2702, plain, (~ spl48_126 | spl48_203), inference(avatar_contradiction_clause, [], [f2701])).
fof(f2701, plain, ($false | (~ spl48_126 | spl48_203)), inference(subsumption_resolution, [], [f2700, f1849])).
fof(f1849, plain, (~ (e21 = h1(e13)) | spl48_203), inference(avatar_component_clause, [], [f1847])).
fof(f1847, plain, (spl48_203 <=> (e21 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_203])])).
fof(f2700, plain, ((e21 = h1(e13)) | ~ spl48_126), inference(backward_demodulation, [], [f549, f1224])).
fof(f1224, plain, ((op2(e20, e20) = e21) | ~ spl48_126), inference(avatar_component_clause, [], [f1222])).
fof(f1222, plain, (spl48_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl48_126])])).
fof(f2696, plain, (~ spl48_110 | ~ spl48_182), inference(avatar_split_clause, [], [f2693, f1739, f1154])).
fof(f1154, plain, (spl48_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl48_110])])).
fof(f2693, plain, (~ (e21 = op2(e21, e20)) | ~ spl48_182), inference(backward_demodulation, [], [f1636, f1740])).
fof(f1636, plain, ~ (op2(e21, e20) = h3(e11)), inference(backward_demodulation, [], [f267, f1634])).
fof(f267, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2692, plain, (spl48_81 | ~ spl48_233), inference(avatar_split_clause, [], [f2689, f2039, f1031])).
fof(f2689, plain, ((e20 = op2(e22, e23)) | ~ spl48_233), inference(backward_demodulation, [], [f1632, f2040])).
fof(f2691, plain, (~ spl48_104 | ~ spl48_233), inference(avatar_split_clause, [], [f2688, f2039, f1128])).
fof(f1128, plain, (spl48_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_104])])).
fof(f2688, plain, (~ (e23 = op2(e21, e22)) | ~ spl48_233), inference(backward_demodulation, [], [f1627, f2040])).
fof(f1627, plain, ~ (op2(e21, e22) = h3(e13)), inference(backward_demodulation, [], [f279, f557])).
fof(f279, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2683, plain, (~ spl48_121 | ~ spl48_117), inference(avatar_split_clause, [], [f2682, f1184, f1201])).
fof(f1201, plain, (spl48_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_121])])).
fof(f2682, plain, (~ (e20 = op2(e20, e21)) | ~ spl48_117), inference(forward_demodulation, [], [f291, f1186])).
fof(f291, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f2648, plain, (~ spl48_62 | ~ spl48_30), inference(avatar_split_clause, [], [f2647, f782, f918])).
fof(f918, plain, (spl48_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl48_62])])).
fof(f2647, plain, (~ (op1(e10, e10) = e11) | ~ spl48_30), inference(forward_demodulation, [], [f217, f784])).
fof(f217, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2640, plain, (~ spl48_57 | ~ spl48_53), inference(avatar_split_clause, [], [f2639, f880, f897])).
fof(f897, plain, (spl48_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_57])])).
fof(f2639, plain, (~ (e10 = op1(e10, e11)) | ~ spl48_53), inference(forward_demodulation, [], [f243, f882])).
fof(f2631, plain, (~ spl48_40 | ~ spl48_24), inference(avatar_split_clause, [], [f2630, f756, f824])).
fof(f824, plain, (spl48_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_40])])).
fof(f2630, plain, (~ (e13 = op1(e11, e12)) | ~ spl48_24), inference(forward_demodulation, [], [f231, f758])).
fof(f231, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2628, plain, (~ spl48_33 | ~ spl48_17), inference(avatar_split_clause, [], [f2627, f727, f795])).
fof(f795, plain, (spl48_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_33])])).
fof(f2627, plain, (~ (e10 = op1(e11, e13)) | ~ spl48_17), inference(forward_demodulation, [], [f237, f729])).
fof(f237, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2624, plain, (~ spl48_14 | ~ spl48_30), inference(avatar_split_clause, [], [f2623, f782, f714])).
fof(f714, plain, (spl48_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_14])])).
fof(f2623, plain, (~ (e11 = op1(e13, e10)) | ~ spl48_30), inference(forward_demodulation, [], [f221, f784])).
fof(f221, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2622, plain, (~ spl48_26 | ~ spl48_30), inference(avatar_split_clause, [], [f2621, f782, f765])).
fof(f765, plain, (spl48_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_26])])).
fof(f2621, plain, (~ (e11 = op1(e12, e11)) | ~ spl48_30), inference(forward_demodulation, [], [f252, f784])).
fof(f252, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2618, plain, (~ spl48_29 | ~ spl48_17), inference(avatar_split_clause, [], [f2617, f727, f778])).
fof(f778, plain, (spl48_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_29])])).
fof(f2617, plain, (~ (e10 = op1(e12, e10)) | ~ spl48_17), inference(forward_demodulation, [], [f254, f729])).
fof(f254, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2616, plain, (~ spl48_27 | ~ spl48_11), inference(avatar_split_clause, [], [f2615, f701, f769])).
fof(f701, plain, (spl48_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_11])])).
fof(f2615, plain, (~ (e12 = op1(e12, e11)) | ~ spl48_11), inference(forward_demodulation, [], [f227, f703])).
fof(f703, plain, ((e12 = op1(e13, e11)) | ~ spl48_11), inference(avatar_component_clause, [], [f701])).
fof(f227, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2612, plain, (~ spl48_25 | ~ spl48_17), inference(avatar_split_clause, [], [f2611, f727, f761])).
fof(f761, plain, (spl48_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl48_25])])).
fof(f2611, plain, (~ (e10 = op1(e12, e11)) | ~ spl48_17), inference(forward_demodulation, [], [f256, f729])).
fof(f256, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2581, plain, (~ spl48_9 | ~ spl48_13), inference(avatar_split_clause, [], [f2578, f710, f693])).
fof(f2578, plain, (~ (e10 = op1(e13, e11)) | ~ spl48_13), inference(backward_demodulation, [], [f258, f712])).
fof(f2577, plain, (~ spl48_1 | ~ spl48_17), inference(avatar_split_clause, [], [f2576, f727, f659])).
fof(f659, plain, (spl48_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_1])])).
fof(f2576, plain, (~ (e10 = op1(e13, e13)) | ~ spl48_17), inference(backward_demodulation, [], [f239, f729])).
fof(f239, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2574, plain, (spl48_17 | ~ spl48_24), inference(avatar_split_clause, [], [f2571, f756, f727])).
fof(f2571, plain, ((e10 = op1(e12, e13)) | ~ spl48_24), inference(backward_demodulation, [], [f540, f758])).
fof(f540, plain, (e10 = op1(e12, op1(e12, e12))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(e12, e12)) & (e11 = op1(e12, op1(e12, op1(e12, e12)))) & (e10 = op1(e12, op1(e12, e12)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax12)).
fof(f2572, plain, (~ spl48_8 | ~ spl48_24), inference(avatar_split_clause, [], [f2569, f756, f688])).
fof(f688, plain, (spl48_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_8])])).
fof(f2569, plain, (~ (e13 = op1(e13, e12)) | ~ spl48_24), inference(backward_demodulation, [], [f233, f758])).
fof(f233, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2562, plain, (~ spl48_30 | ~ spl48_31), inference(avatar_contradiction_clause, [], [f2561])).
fof(f2561, plain, ($false | (~ spl48_30 | ~ spl48_31)), inference(subsumption_resolution, [], [f2560, f315])).
fof(f315, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f2560, plain, ((e11 = e12) | (~ spl48_30 | ~ spl48_31)), inference(backward_demodulation, [], [f788, f784])).
fof(f788, plain, ((e12 = op1(e12, e10)) | ~ spl48_31), inference(avatar_component_clause, [], [f786])).
fof(f786, plain, (spl48_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_31])])).
fof(f2555, plain, (~ spl48_24 | ~ spl48_32), inference(avatar_split_clause, [], [f2551, f790, f756])).
fof(f790, plain, (spl48_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl48_32])])).
fof(f2551, plain, (~ (e13 = op1(e12, e12)) | ~ spl48_32), inference(backward_demodulation, [], [f253, f792])).
fof(f792, plain, ((e13 = op1(e12, e10)) | ~ spl48_32), inference(avatar_component_clause, [], [f790])).
fof(f253, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2542, plain, (~ spl48_7 | ~ spl48_39), inference(avatar_split_clause, [], [f2538, f820, f684])).
fof(f684, plain, (spl48_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl48_7])])).
fof(f2538, plain, (~ (e12 = op1(e13, e12)) | ~ spl48_39), inference(backward_demodulation, [], [f232, f822])).
fof(f232, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2535, plain, (~ spl48_38 | ~ spl48_42), inference(avatar_split_clause, [], [f2529, f833, f816])).
fof(f2529, plain, (~ (e11 = op1(e11, e12)) | ~ spl48_42), inference(backward_demodulation, [], [f249, f835])).
fof(f249, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2525, plain, (~ spl48_37 | ~ spl48_45), inference(avatar_split_clause, [], [f2520, f846, f812])).
fof(f2520, plain, (~ (e10 = op1(e11, e12)) | ~ spl48_45), inference(backward_demodulation, [], [f247, f848])).
fof(f2516, plain, (~ spl48_50 | ~ spl48_51), inference(avatar_contradiction_clause, [], [f2515])).
fof(f2515, plain, ($false | (~ spl48_50 | ~ spl48_51)), inference(subsumption_resolution, [], [f2514, f315])).
fof(f2514, plain, ((e11 = e12) | (~ spl48_50 | ~ spl48_51)), inference(backward_demodulation, [], [f873, f869])).
fof(f873, plain, ((e12 = op1(e10, e13)) | ~ spl48_51), inference(avatar_component_clause, [], [f871])).
fof(f871, plain, (spl48_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_51])])).
fof(f2504, plain, (~ spl48_49 | ~ spl48_53), inference(avatar_split_clause, [], [f2500, f880, f863])).
fof(f863, plain, (spl48_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl48_49])])).
fof(f2500, plain, (~ (e10 = op1(e10, e13)) | ~ spl48_53), inference(backward_demodulation, [], [f245, f882])).
fof(f245, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2501, plain, (~ spl48_37 | ~ spl48_53), inference(avatar_split_clause, [], [f2497, f880, f812])).
fof(f2497, plain, (~ (e10 = op1(e11, e12)) | ~ spl48_53), inference(backward_demodulation, [], [f228, f882])).
fof(f2495, plain, (~ spl48_49 | ~ spl48_57), inference(avatar_split_clause, [], [f2489, f897, f863])).
fof(f2489, plain, (~ (e10 = op1(e10, e13)) | ~ spl48_57), inference(backward_demodulation, [], [f244, f899])).
fof(f899, plain, ((e10 = op1(e10, e11)) | ~ spl48_57), inference(avatar_component_clause, [], [f897])).
fof(f244, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2493, plain, (~ spl48_9 | ~ spl48_57), inference(avatar_split_clause, [], [f2487, f897, f693])).
fof(f2487, plain, (~ (e10 = op1(e13, e11)) | ~ spl48_57), inference(backward_demodulation, [], [f224, f899])).
fof(f2484, plain, (~ spl48_49 | ~ spl48_61), inference(avatar_split_clause, [], [f2474, f914, f863])).
fof(f2474, plain, (~ (e10 = op1(e10, e13)) | ~ spl48_61), inference(backward_demodulation, [], [f242, f916])).
fof(f242, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2482, plain, (~ spl48_57 | ~ spl48_61), inference(avatar_split_clause, [], [f2472, f914, f897])).
fof(f2472, plain, (~ (e10 = op1(e10, e11)) | ~ spl48_61), inference(backward_demodulation, [], [f240, f916])).
fof(f240, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f2468, plain, (spl48_174 | ~ spl48_65), inference(avatar_split_clause, [], [f2467, f963, f1696])).
fof(f963, plain, (spl48_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl48_65])])).
fof(f2467, plain, ((e20 = h4(e13)) | ~ spl48_65), inference(backward_demodulation, [], [f561, f965])).
fof(f965, plain, ((e20 = op2(e23, e23)) | ~ spl48_65), inference(avatar_component_clause, [], [f963])).
fof(f2457, plain, (~ spl48_233 | ~ spl48_72), inference(avatar_split_clause, [], [f2455, f992, f2039])).
fof(f992, plain, (spl48_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_72])])).
fof(f2455, plain, (~ (e23 = h3(e13)) | ~ spl48_72), inference(backward_demodulation, [], [f1628, f994])).
fof(f994, plain, ((e23 = op2(e23, e22)) | ~ spl48_72), inference(avatar_component_clause, [], [f992])).
fof(f1628, plain, ~ (op2(e23, e22) = h3(e13)), inference(backward_demodulation, [], [f281, f557])).
fof(f281, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2448, plain, (~ spl48_219 | ~ spl48_76), inference(avatar_split_clause, [], [f2445, f1009, f1930])).
fof(f2445, plain, (~ (e23 = h4(e13)) | ~ spl48_76), inference(backward_demodulation, [], [f1646, f1011])).
fof(f1646, plain, ~ (op2(e23, e21) = h4(e13)), inference(backward_demodulation, [], [f310, f561])).
fof(f310, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2440, plain, (~ spl48_76 | ~ spl48_80), inference(avatar_split_clause, [], [f2435, f1026, f1009])).
fof(f2435, plain, (~ (e23 = op2(e23, e21)) | ~ spl48_80), inference(backward_demodulation, [], [f306, f1028])).
fof(f306, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2425, plain, (spl48_233 | ~ spl48_88), inference(avatar_split_clause, [], [f2424, f1060, f2039])).
fof(f2424, plain, ((e23 = h3(e13)) | ~ spl48_88), inference(backward_demodulation, [], [f557, f1062])).
fof(f2423, plain, (~ spl48_81 | ~ spl48_89), inference(avatar_split_clause, [], [f2418, f1065, f1031])).
fof(f1065, plain, (spl48_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl48_89])])).
fof(f2418, plain, (~ (e20 = op2(e22, e23)) | ~ spl48_89), inference(backward_demodulation, [], [f304, f1067])).
fof(f1067, plain, ((e20 = op2(e22, e21)) | ~ spl48_89), inference(avatar_component_clause, [], [f1065])).
fof(f304, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2413, plain, (~ spl48_203 | ~ spl48_94), inference(avatar_split_clause, [], [f2411, f1086, f1847])).
fof(f2411, plain, (~ (e21 = h1(e13)) | ~ spl48_94), inference(backward_demodulation, [], [f1609, f1088])).
fof(f1609, plain, ~ (op2(e22, e20) = h1(e13)), inference(backward_demodulation, [], [f265, f549])).
fof(f265, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2412, plain, (spl48_182 | ~ spl48_94), inference(avatar_split_clause, [], [f2410, f1086, f1739])).
fof(f2410, plain, ((e21 = h3(e11)) | ~ spl48_94), inference(backward_demodulation, [], [f1634, f1088])).
fof(f2392, plain, (spl48_191 | ~ spl48_106), inference(avatar_split_clause, [], [f2391, f1137, f1786])).
fof(f2391, plain, ((e21 = h2(e13)) | ~ spl48_106), inference(backward_demodulation, [], [f553, f1139])).
fof(f2385, plain, (~ spl48_207 | ~ spl48_109), inference(avatar_split_clause, [], [f2379, f1150, f1867])).
fof(f2379, plain, (~ (e20 = h1(e13)) | ~ spl48_109), inference(backward_demodulation, [], [f1608, f1152])).
fof(f1608, plain, ~ (op2(e21, e20) = h1(e13)), inference(backward_demodulation, [], [f264, f549])).
fof(f264, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f2376, plain, (~ spl48_114 | ~ spl48_115), inference(avatar_contradiction_clause, [], [f2375])).
fof(f2375, plain, ($false | (~ spl48_114 | ~ spl48_115)), inference(subsumption_resolution, [], [f2374, f321])).
fof(f2374, plain, ((e21 = e22) | (~ spl48_114 | ~ spl48_115)), inference(backward_demodulation, [], [f1177, f1173])).
fof(f1177, plain, ((e22 = op2(e20, e23)) | ~ spl48_115), inference(avatar_component_clause, [], [f1175])).
fof(f2367, plain, (~ spl48_219 | ~ spl48_116), inference(avatar_split_clause, [], [f2363, f1179, f1930])).
fof(f2363, plain, (~ (e23 = h4(e13)) | ~ spl48_116), inference(backward_demodulation, [], [f1642, f1181])).
fof(f1642, plain, ~ (op2(e20, e23) = h4(e13)), inference(backward_demodulation, [], [f284, f561])).
fof(f284, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2361, plain, (~ spl48_69 | ~ spl48_117), inference(avatar_split_clause, [], [f2356, f1184, f980])).
fof(f980, plain, (spl48_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl48_69])])).
fof(f2356, plain, (~ (e20 = op2(e23, e22)) | ~ spl48_117), inference(backward_demodulation, [], [f278, f1186])).
fof(f2360, plain, (~ spl48_101 | ~ spl48_117), inference(avatar_split_clause, [], [f2355, f1184, f1116])).
fof(f2355, plain, (~ (e20 = op2(e21, e22)) | ~ spl48_117), inference(backward_demodulation, [], [f276, f1186])).
fof(f276, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2359, plain, (~ spl48_207 | ~ spl48_117), inference(avatar_split_clause, [], [f2354, f1184, f1867])).
fof(f2354, plain, (~ (e20 = h1(e13)) | ~ spl48_117), inference(backward_demodulation, [], [f1612, f1186])).
fof(f1612, plain, ~ (op2(e20, e22) = h1(e13)), inference(backward_demodulation, [], [f289, f549])).
fof(f289, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f2350, plain, (~ spl48_73 | ~ spl48_121), inference(avatar_split_clause, [], [f2344, f1201, f997])).
fof(f2344, plain, (~ (e20 = op2(e23, e21)) | ~ spl48_121), inference(backward_demodulation, [], [f272, f1203])).
fof(f1203, plain, ((e20 = op2(e20, e21)) | ~ spl48_121), inference(avatar_component_clause, [], [f1201])).
fof(f272, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2348, plain, (~ spl48_207 | ~ spl48_121), inference(avatar_split_clause, [], [f2342, f1201, f1867])).
fof(f2342, plain, (~ (e20 = h1(e13)) | ~ spl48_121), inference(backward_demodulation, [], [f1611, f1203])).
fof(f1611, plain, ~ (op2(e20, e21) = h1(e13)), inference(backward_demodulation, [], [f288, f549])).
fof(f288, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f2340, plain, (spl48_207 | ~ spl48_125), inference(avatar_split_clause, [], [f2339, f1218, f1867])).
fof(f2339, plain, ((e20 = h1(e13)) | ~ spl48_125), inference(backward_demodulation, [], [f549, f1220])).
fof(f1220, plain, ((e20 = op2(e20, e20)) | ~ spl48_125), inference(avatar_component_clause, [], [f1218])).
fof(f2090, plain, (~ spl48_229 | ~ spl48_230 | ~ spl48_231 | ~ spl48_232 | spl48_183 | spl48_180 | spl48_177 | ~ spl48_233 | ~ spl48_234 | ~ spl48_235 | ~ spl48_236 | ~ spl48_237 | ~ spl48_238 | ~ spl48_239 | ~ spl48_240 | ~ spl48_241 | ~ spl48_242 | ~ spl48_243 | ~ spl48_244 | ~ spl48_245), inference(avatar_split_clause, [], [f2021, f2087, f2083, f2079, f2075, f2071, f2067, f2063, f2059, f2055, f2051, f2047, f2043, f2039, f1712, f1729, f1745, f2035, f2031, f2027, f2023])).
fof(f1745, plain, (spl48_183 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl48_183])])).
fof(f1729, plain, (spl48_180 <=> sP43), introduced(avatar_definition, [new_symbols(naming, [spl48_180])])).
fof(f1712, plain, (spl48_177 <=> sP44), introduced(avatar_definition, [new_symbols(naming, [spl48_177])])).
fof(f2021, plain, (~ (h1(e13) = h3(op1(e10, e10))) | ~ (h3(op1(e10, e11)) = op2(e20, h3(e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11)))), inference(forward_demodulation, [], [f2020, f549])).
fof(f2020, plain, (~ (op2(e20, e20) = h3(op1(e10, e10))) | ~ (h3(op1(e10, e11)) = op2(e20, h3(e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11)))), inference(forward_demodulation, [], [f2019, f1641])).
fof(f2019, plain, (~ (h3(op1(e10, e11)) = op2(e20, h3(e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2018, f1641])).
fof(f2018, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2017, f1641])).
fof(f2017, plain, (~ (h3(op1(e10, e12)) = op2(h3(e10), e22)) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2016, f554])).
fof(f2016, plain, (~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2015, f1641])).
fof(f2015, plain, (~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2014, f1641])).
fof(f2014, plain, (~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2013, f554])).
fof(f2013, plain, (~ (h3(e11) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2012, f1634])).
fof(f2012, plain, (~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2011, f554])).
fof(f2011, plain, (~ (h3(op1(e12, e10)) = op2(h3(e12), e20)) | ~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2010, f1641])).
fof(f2010, plain, (~ (h3(op1(e12, e11)) = op2(e22, h3(e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2009, f554])).
fof(f2009, plain, (~ (h3(e13) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2008, f557])).
fof(f2008, plain, (~ (op2(e22, e22) = h3(op1(e12, e12))) | ~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2007, f554])).
fof(f2007, plain, (~ (e20 = h3(op1(e12, e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2006, f1632])).
fof(f2006, plain, (~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2005, f554])).
fof(f2005, plain, (~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2004, f1641])).
fof(f2004, plain, (~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f621, f554])).
fof(f621, plain, (~ (e23 = h3(e13)) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(cnf_transformation, [], [f71])).
fof(f71, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP44 | sP43 | sP42 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP41 | sP40 | sP39 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP38 | sP37 | sP36 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59])).
fof(f59, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP36), inference(usedef, [], [e59])).
fof(e59, plain, (sP36 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f60, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP37), inference(usedef, [], [e60])).
fof(e60, plain, (sP37 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f61, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP38), inference(usedef, [], [e61])).
fof(e61, plain, (sP38 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f62, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP39), inference(usedef, [], [e62])).
fof(e62, plain, (sP39 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f63, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP40), inference(usedef, [], [e63])).
fof(e63, plain, (sP40 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f64, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP41), inference(usedef, [], [e64])).
fof(e64, plain, (sP41 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f65, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP42), inference(usedef, [], [e65])).
fof(e65, plain, (sP42 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f66, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP43), inference(usedef, [], [e66])).
fof(e66, plain, (sP43 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f67, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP44), inference(usedef, [], [e67])).
fof(e67, plain, (sP44 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f68, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP45), inference(usedef, [], [e68])).
fof(e68, plain, (sP45 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f69, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP46), inference(usedef, [], [e69])).
fof(e69, plain, (sP46 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f70, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP47), inference(usedef, [], [e70])).
fof(e70, plain, (sP47 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', co1)).
fof(f1973, plain, (~ spl48_210 | ~ spl48_211 | ~ spl48_212 | ~ spl48_213 | ~ spl48_214 | ~ spl48_215 | ~ spl48_216 | ~ spl48_217 | ~ spl48_218 | spl48_173 | spl48_169 | spl48_165 | ~ spl48_220 | ~ spl48_221 | ~ spl48_222 | ~ spl48_223 | ~ spl48_224 | ~ spl48_225 | ~ spl48_226), inference(avatar_split_clause, [], [f1972, f1958, f1954, f1950, f1946, f1942, f1938, f1934, f1652, f1672, f1692, f1926, f1922, f1918, f1914, f1910, f1906, f1902, f1898, f1894])).
fof(f1692, plain, (spl48_173 <=> sP45), introduced(avatar_definition, [new_symbols(naming, [spl48_173])])).
fof(f1672, plain, (spl48_169 <=> sP46), introduced(avatar_definition, [new_symbols(naming, [spl48_169])])).
fof(f1652, plain, (spl48_165 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl48_165])])).
fof(f1972, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e12)) = op2(h4(e11), e23)) | ~ (h4(e11) = h4(op1(e12, e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e13) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1971, f558])).
fof(f1971, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e23)) | ~ (h4(e11) = h4(op1(e12, e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e13) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1970, f558])).
fof(f1970, plain, (~ (h4(e11) = h4(op1(e12, e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e13) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1969, f1650])).
fof(f1969, plain, (~ (h4(op1(e12, e10)) = op2(e23, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e13) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1968, f558])).
fof(f1968, plain, (~ (h4(op1(e12, e11)) = op2(e23, h4(e11))) | ~ (h4(e13) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1967, f558])).
fof(f1967, plain, (~ (h4(e13) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1966, f561])).
fof(f1966, plain, (~ (op2(e23, e23) = h4(op1(e12, e12))) | ~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1965, f558])).
fof(f1965, plain, (~ (h4(e10) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1964, f1649])).
fof(f1964, plain, (~ (h4(op1(e12, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1963, f558])).
fof(f1963, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), e23)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1962, f558])).
fof(f1962, plain, (sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f624, f558])).
fof(f624, plain, (~ (e23 = h4(e12)) | sP47 | sP46 | sP45 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f71])).
fof(f1760, plain, ~ spl48_183, inference(avatar_split_clause, [], [f1759, f1745])).
fof(f1759, plain, ~ sP42, inference(subsumption_resolution, [], [f582, f1641])).
fof(f582, plain, (~ (e20 = h3(e10)) | ~ sP42), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP42), inference(nnf_transformation, [], [f65])).
fof(f1742, plain, (~ spl48_180 | ~ spl48_182), inference(avatar_split_clause, [], [f579, f1739, f1729])).
fof(f579, plain, (~ (e21 = h3(e11)) | ~ sP43), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP43), inference(nnf_transformation, [], [f66])).
fof(f1721, plain, ~ spl48_177, inference(avatar_split_clause, [], [f1720, f1712])).
fof(f1720, plain, ~ sP44, inference(subsumption_resolution, [], [f576, f554])).
fof(f576, plain, (~ (e22 = h3(e12)) | ~ sP44), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP44), inference(nnf_transformation, [], [f67])).
fof(f1705, plain, (~ spl48_173 | ~ spl48_175), inference(avatar_split_clause, [], [f571, f1702, f1692])).
fof(f571, plain, (~ (e20 = h4(e11)) | ~ sP45), inference(cnf_transformation, [], [f110])).
fof(f110, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP45), inference(nnf_transformation, [], [f68])).
fof(f1690, plain, (~ spl48_169 | ~ spl48_172), inference(avatar_split_clause, [], [f566, f1687, f1672])).
fof(f566, plain, (~ (e21 = h4(e10)) | ~ sP46), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP46), inference(nnf_transformation, [], [f69])).
fof(f1659, plain, (~ spl48_165 | ~ spl48_166), inference(avatar_split_clause, [], [f565, f1656, f1652])).
fof(f565, plain, (~ (e22 = h4(e13)) | ~ sP47), inference(cnf_transformation, [], [f108])).
fof(f108, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP47), inference(nnf_transformation, [], [f70])).
fof(f1607, plain, spl48_94, inference(avatar_split_clause, [], [f1606, f1086])).
fof(f1606, plain, (e21 = op2(e22, e20)), inference(backward_demodulation, [], [f544, f543])).
fof(f544, plain, (e21 = op2(e22, op2(e22, op2(e22, e22)))), inference(cnf_transformation, [], [f13])).
fof(f1605, plain, spl48_88, inference(avatar_split_clause, [], [f545, f1060])).
fof(f545, plain, (e23 = op2(e22, e22)), inference(cnf_transformation, [], [f13])).
fof(f1604, plain, spl48_30, inference(avatar_split_clause, [], [f1603, f782])).
fof(f1603, plain, (e11 = op1(e12, e10)), inference(backward_demodulation, [], [f541, f540])).
fof(f541, plain, (e11 = op1(e12, op1(e12, op1(e12, e12)))), inference(cnf_transformation, [], [f12])).
fof(f1602, plain, spl48_24, inference(avatar_split_clause, [], [f542, f756])).
fof(f542, plain, (e13 = op1(e12, e12)), inference(cnf_transformation, [], [f12])).
fof(f1601, plain, (spl48_164 | spl48_163 | spl48_162 | spl48_161 | spl48_160 | spl48_159 | spl48_158 | spl48_157 | spl48_156 | spl48_155 | spl48_154 | spl48_153 | spl48_152 | spl48_151 | spl48_150 | spl48_68), inference(avatar_split_clause, [], [f530, f975, f1459, f1468, f1477, f1486, f1495, f1504, f1513, f1522, f1531, f1540, f1549, f1558, f1567, f1576, f1585])).
fof(f1585, plain, (spl48_164 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl48_164])])).
fof(f1576, plain, (spl48_163 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl48_163])])).
fof(f1567, plain, (spl48_162 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl48_162])])).
fof(f1558, plain, (spl48_161 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl48_161])])).
fof(f1549, plain, (spl48_160 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl48_160])])).
fof(f1540, plain, (spl48_159 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl48_159])])).
fof(f1531, plain, (spl48_158 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl48_158])])).
fof(f1522, plain, (spl48_157 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl48_157])])).
fof(f1513, plain, (spl48_156 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl48_156])])).
fof(f1504, plain, (spl48_155 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl48_155])])).
fof(f1495, plain, (spl48_154 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl48_154])])).
fof(f1486, plain, (spl48_153 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl48_153])])).
fof(f1477, plain, (spl48_152 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl48_152])])).
fof(f1468, plain, (spl48_151 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl48_151])])).
fof(f1459, plain, (spl48_150 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl48_150])])).
fof(f530, plain, ((e23 = op2(e23, e23)) | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | sP35 | sP34 | sP33) & (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18)), inference(definition_folding, [], [f11, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40])).
fof(f40, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP18), inference(usedef, [], [e40])).
fof(e40, plain, (sP18 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f41, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e20, e21))) | ~ sP19), inference(usedef, [], [e41])).
fof(e41, plain, (sP19 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f42, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e20, e22))) | ~ sP20), inference(usedef, [], [e42])).
fof(e42, plain, (sP20 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f43, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e20, e23))) | ~ sP21), inference(usedef, [], [e43])).
fof(e43, plain, (sP21 <=> ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f44, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e21, e20))) | ~ sP22), inference(usedef, [], [e44])).
fof(e44, plain, (sP22 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f45, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP23), inference(usedef, [], [e45])).
fof(e45, plain, (sP23 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f46, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e21, e22))) | ~ sP24), inference(usedef, [], [e46])).
fof(e46, plain, (sP24 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f47, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e21, e23))) | ~ sP25), inference(usedef, [], [e47])).
fof(e47, plain, (sP25 <=> ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f48, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e22, e20))) | ~ sP26), inference(usedef, [], [e48])).
fof(e48, plain, (sP26 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f49, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e22, e21))) | ~ sP27), inference(usedef, [], [e49])).
fof(e49, plain, (sP27 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f50, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP28), inference(usedef, [], [e50])).
fof(e50, plain, (sP28 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f51, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e22, e23))) | ~ sP29), inference(usedef, [], [e51])).
fof(e51, plain, (sP29 <=> ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f52, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e23, e20))) | ~ sP30), inference(usedef, [], [e52])).
fof(e52, plain, (sP30 <=> ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f53, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e23, e21))) | ~ sP31), inference(usedef, [], [e53])).
fof(e53, plain, (sP31 <=> ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f54, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e23, e22))) | ~ sP32), inference(usedef, [], [e54])).
fof(e54, plain, (sP32 <=> ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f55, plain, ((((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP33), inference(usedef, [], [e55])).
fof(e55, plain, (sP33 <=> (((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f56, plain, ((((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP34), inference(usedef, [], [e56])).
fof(e56, plain, (sP34 <=> (((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f57, plain, ((((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP35), inference(usedef, [], [e57])).
fof(e57, plain, (sP35 <=> (((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f11, plain, (((((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | (((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | (((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | (((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20)))) & (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e23, e22))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e23, e21))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e23, e20))) | ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e22, e23))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e22, e21))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e22, e20))) | ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e21, e23))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e21, e22))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e21, e20))) | ((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e20, e23))) | ((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e20, e22))) | ((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e20, e21))) | ((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax11)).
fof(f1597, plain, (spl48_164 | spl48_163 | spl48_162 | spl48_161 | spl48_160 | spl48_159 | spl48_158 | spl48_157 | spl48_156 | spl48_155 | spl48_154 | spl48_153 | spl48_152 | spl48_151 | spl48_150 | ~ spl48_68), inference(avatar_split_clause, [], [f626, f975, f1459, f1468, f1477, f1486, f1495, f1504, f1513, f1522, f1531, f1540, f1549, f1558, f1567, f1576, f1585])).
fof(f626, plain, (~ (e23 = op2(e23, e23)) | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18), inference(duplicate_literal_removal, [], [f534])).
fof(f534, plain, (~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23)) | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18), inference(cnf_transformation, [], [f58])).
fof(f1593, plain, (spl48_149 | spl48_148 | spl48_147 | ~ spl48_88 | spl48_83), inference(avatar_split_clause, [], [f538, f1039, f1060, f1435, f1443, f1451])).
fof(f1451, plain, (spl48_149 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl48_149])])).
fof(f1443, plain, (spl48_148 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl48_148])])).
fof(f1435, plain, (spl48_147 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl48_147])])).
fof(f538, plain, ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22)) | sP35 | sP34 | sP33), inference(cnf_transformation, [], [f58])).
fof(f1592, plain, (~ spl48_164 | spl48_125), inference(avatar_split_clause, [], [f525, f1218, f1585])).
fof(f525, plain, ((e20 = op2(e20, e20)) | ~ sP18), inference(cnf_transformation, [], [f107])).
fof(f107, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP18), inference(nnf_transformation, [], [f40])).
fof(f1591, plain, (~ spl48_164 | ~ spl48_125), inference(avatar_split_clause, [], [f627, f1218, f1585])).
fof(f627, plain, (~ (e20 = op2(e20, e20)) | ~ sP18), inference(duplicate_literal_removal, [], [f526])).
fof(f526, plain, (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20)) | ~ sP18), inference(cnf_transformation, [], [f107])).
fof(f1583, plain, (~ spl48_163 | spl48_121), inference(avatar_split_clause, [], [f520, f1201, f1576])).
fof(f520, plain, ((e20 = op2(e20, e21)) | ~ sP19), inference(cnf_transformation, [], [f106])).
fof(f106, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e20 = op2(e20, e21))) | ~ sP19), inference(nnf_transformation, [], [f41])).
fof(f1581, plain, (~ spl48_163 | ~ spl48_106), inference(avatar_split_clause, [], [f628, f1137, f1576])).
fof(f628, plain, (~ (e21 = op2(e21, e21)) | ~ sP19), inference(duplicate_literal_removal, [], [f522])).
fof(f522, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP19), inference(cnf_transformation, [], [f106])).
fof(f1574, plain, (~ spl48_162 | spl48_117), inference(avatar_split_clause, [], [f515, f1184, f1567])).
fof(f515, plain, ((e20 = op2(e20, e22)) | ~ sP20), inference(cnf_transformation, [], [f105])).
fof(f105, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e20 = op2(e20, e22))) | ~ sP20), inference(nnf_transformation, [], [f42])).
fof(f1565, plain, (~ spl48_161 | spl48_113), inference(avatar_split_clause, [], [f510, f1167, f1558])).
fof(f510, plain, ((e20 = op2(e20, e23)) | ~ sP21), inference(cnf_transformation, [], [f104])).
fof(f104, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e20 = op2(e20, e23))) | ~ sP21), inference(nnf_transformation, [], [f43])).
fof(f1556, plain, (~ spl48_160 | spl48_110), inference(avatar_split_clause, [], [f505, f1154, f1549])).
fof(f505, plain, ((e21 = op2(e21, e20)) | ~ sP22), inference(cnf_transformation, [], [f103])).
fof(f103, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e21 = op2(e21, e20))) | ~ sP22), inference(nnf_transformation, [], [f44])).
fof(f1545, plain, (~ spl48_159 | ~ spl48_106), inference(avatar_split_clause, [], [f632, f1137, f1540])).
fof(f632, plain, (~ (e21 = op2(e21, e21)) | ~ sP23), inference(duplicate_literal_removal, [], [f502])).
fof(f502, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP23), inference(cnf_transformation, [], [f102])).
fof(f102, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP23), inference(nnf_transformation, [], [f45])).
fof(f1538, plain, (~ spl48_158 | spl48_102), inference(avatar_split_clause, [], [f495, f1120, f1531])).
fof(f495, plain, ((e21 = op2(e21, e22)) | ~ sP24), inference(cnf_transformation, [], [f101])).
fof(f101, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e21 = op2(e21, e22))) | ~ sP24), inference(nnf_transformation, [], [f46])).
fof(f1529, plain, (~ spl48_157 | spl48_98), inference(avatar_split_clause, [], [f490, f1103, f1522])).
fof(f490, plain, ((e21 = op2(e21, e23)) | ~ sP25), inference(cnf_transformation, [], [f100])).
fof(f100, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e21 = op2(e21, e23))) | ~ sP25), inference(nnf_transformation, [], [f47])).
fof(f1520, plain, (~ spl48_156 | spl48_95), inference(avatar_split_clause, [], [f485, f1090, f1513])).
fof(f485, plain, ((e22 = op2(e22, e20)) | ~ sP26), inference(cnf_transformation, [], [f99])).
fof(f99, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e22 = op2(e22, e20))) | ~ sP26), inference(nnf_transformation, [], [f48])).
fof(f1509, plain, (~ spl48_155 | ~ spl48_106), inference(avatar_split_clause, [], [f636, f1137, f1504])).
fof(f636, plain, (~ (e21 = op2(e21, e21)) | ~ sP27), inference(duplicate_literal_removal, [], [f482])).
fof(f482, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP27), inference(cnf_transformation, [], [f98])).
fof(f98, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e22 = op2(e22, e21))) | ~ sP27), inference(nnf_transformation, [], [f49])).
fof(f1502, plain, (~ spl48_154 | spl48_87), inference(avatar_split_clause, [], [f475, f1056, f1495])).
fof(f475, plain, ((e22 = op2(e22, e22)) | ~ sP28), inference(cnf_transformation, [], [f97])).
fof(f97, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP28), inference(nnf_transformation, [], [f50])).
fof(f1493, plain, (~ spl48_153 | spl48_83), inference(avatar_split_clause, [], [f470, f1039, f1486])).
fof(f470, plain, ((e22 = op2(e22, e23)) | ~ sP29), inference(cnf_transformation, [], [f96])).
fof(f96, plain, (((~ (e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & (~ (e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & (~ (e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & (~ (e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e22 = op2(e22, e23))) | ~ sP29), inference(nnf_transformation, [], [f51])).
fof(f1484, plain, (~ spl48_152 | spl48_80), inference(avatar_split_clause, [], [f465, f1026, f1477])).
fof(f465, plain, ((e23 = op2(e23, e20)) | ~ sP30), inference(cnf_transformation, [], [f95])).
fof(f95, plain, (((~ (e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & (~ (e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & (~ (e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e23, e20))) | ~ sP30), inference(nnf_transformation, [], [f52])).
fof(f1483, plain, (~ spl48_152 | ~ spl48_125), inference(avatar_split_clause, [], [f639, f1218, f1477])).
fof(f639, plain, (~ (e20 = op2(e20, e20)) | ~ sP30), inference(duplicate_literal_removal, [], [f466])).
fof(f466, plain, (~ (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20)) | ~ sP30), inference(cnf_transformation, [], [f95])).
fof(f1473, plain, (~ spl48_151 | ~ spl48_106), inference(avatar_split_clause, [], [f640, f1137, f1468])).
fof(f640, plain, (~ (e21 = op2(e21, e21)) | ~ sP31), inference(duplicate_literal_removal, [], [f462])).
fof(f462, plain, (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21)) | ~ sP31), inference(cnf_transformation, [], [f94])).
fof(f94, plain, (((~ (e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & (~ (e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & (~ (e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & (~ (e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e23 = op2(e23, e21))) | ~ sP31), inference(nnf_transformation, [], [f53])).
fof(f1466, plain, (~ spl48_150 | spl48_72), inference(avatar_split_clause, [], [f455, f992, f1459])).
fof(f455, plain, ((e23 = op2(e23, e22)) | ~ sP32), inference(cnf_transformation, [], [f93])).
fof(f93, plain, (((~ (e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & (~ (e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & (~ (e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & (~ (e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e23 = op2(e23, e22))) | ~ sP32), inference(nnf_transformation, [], [f54])).
fof(f1457, plain, (~ spl48_149 | spl48_125), inference(avatar_split_clause, [], [f450, f1218, f1451])).
fof(f450, plain, ((e20 = op2(e20, e20)) | ~ sP33), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP33), inference(nnf_transformation, [], [f55])).
fof(f1456, plain, (~ spl48_149 | ~ spl48_105 | spl48_110), inference(avatar_split_clause, [], [f452, f1154, f1133, f1451])).
fof(f452, plain, ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21)) | ~ sP33), inference(cnf_transformation, [], [f92])).
fof(f1449, plain, (~ spl48_148 | spl48_106), inference(avatar_split_clause, [], [f445, f1137, f1443])).
fof(f445, plain, ((e21 = op2(e21, e21)) | ~ sP34), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ((((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP34), inference(nnf_transformation, [], [f56])).
fof(f1441, plain, (~ spl48_147 | spl48_87), inference(avatar_split_clause, [], [f440, f1056, f1435])).
fof(f440, plain, ((e22 = op2(e22, e22)) | ~ sP35), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ((((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP35), inference(nnf_transformation, [], [f57])).
fof(f1433, plain, (spl48_146 | spl48_145 | spl48_144 | spl48_143 | spl48_142 | spl48_141 | spl48_140 | spl48_139 | spl48_138 | spl48_137 | spl48_136 | spl48_135 | spl48_134 | spl48_133 | spl48_132 | spl48_4), inference(avatar_split_clause, [], [f430, f671, f1291, f1300, f1309, f1318, f1327, f1336, f1345, f1354, f1363, f1372, f1381, f1390, f1399, f1408, f1417])).
fof(f1417, plain, (spl48_146 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl48_146])])).
fof(f1408, plain, (spl48_145 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl48_145])])).
fof(f1399, plain, (spl48_144 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl48_144])])).
fof(f1390, plain, (spl48_143 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl48_143])])).
fof(f1381, plain, (spl48_142 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl48_142])])).
fof(f1372, plain, (spl48_141 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl48_141])])).
fof(f1363, plain, (spl48_140 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl48_140])])).
fof(f1354, plain, (spl48_139 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl48_139])])).
fof(f1345, plain, (spl48_138 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl48_138])])).
fof(f1336, plain, (spl48_137 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl48_137])])).
fof(f1327, plain, (spl48_136 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl48_136])])).
fof(f1318, plain, (spl48_135 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl48_135])])).
fof(f1309, plain, (spl48_134 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl48_134])])).
fof(f1300, plain, (spl48_133 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl48_133])])).
fof(f1291, plain, (spl48_132 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl48_132])])).
fof(f430, plain, ((e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (((((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | sP17 | sP16 | sP15) & (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f10, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e10, e13))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e11, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e11, e12))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e11, e13))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e12, e10))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e12, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e12, e13))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e13, e10))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e13, e11))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e13, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f36, plain, ((((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP15), inference(usedef, [], [e36])).
fof(e36, plain, (sP15 <=> (((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f37, plain, ((((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP16), inference(usedef, [], [e37])).
fof(e37, plain, (sP16 <=> (((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f38, plain, ((((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP17), inference(usedef, [], [e38])).
fof(e38, plain, (sP17 <=> (((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f10, plain, (((((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | (((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | (((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | (((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10)))) & (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e13, e12))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e13, e11))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e13, e10))) | ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e12, e13))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e12, e11))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e12, e10))) | ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e11, e13))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e11, e12))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e11, e10))) | ((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e10, e13))) | ((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e10, e12))) | ((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e10, e11))) | ((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax10)).
fof(f1429, plain, (spl48_146 | spl48_145 | spl48_144 | spl48_143 | spl48_142 | spl48_141 | spl48_140 | spl48_139 | spl48_138 | spl48_137 | spl48_136 | spl48_135 | spl48_134 | spl48_133 | spl48_132 | ~ spl48_4), inference(avatar_split_clause, [], [f642, f671, f1291, f1300, f1309, f1318, f1327, f1336, f1345, f1354, f1363, f1372, f1381, f1390, f1399, f1408, f1417])).
fof(f642, plain, (~ (e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(duplicate_literal_removal, [], [f434])).
fof(f434, plain, (~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f39])).
fof(f1425, plain, (spl48_131 | spl48_130 | spl48_129 | ~ spl48_24 | spl48_19), inference(avatar_split_clause, [], [f438, f735, f756, f1267, f1275, f1283])).
fof(f1283, plain, (spl48_131 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl48_131])])).
fof(f1275, plain, (spl48_130 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl48_130])])).
fof(f1267, plain, (spl48_129 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl48_129])])).
fof(f438, plain, ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12)) | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f39])).
fof(f1424, plain, (~ spl48_146 | spl48_61), inference(avatar_split_clause, [], [f425, f914, f1417])).
fof(f425, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1423, plain, (~ spl48_146 | ~ spl48_61), inference(avatar_split_clause, [], [f643, f914, f1417])).
fof(f643, plain, (~ (e10 = op1(e10, e10)) | ~ sP0), inference(duplicate_literal_removal, [], [f426])).
fof(f426, plain, (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f89])).
fof(f1415, plain, (~ spl48_145 | spl48_57), inference(avatar_split_clause, [], [f420, f897, f1408])).
fof(f420, plain, ((e10 = op1(e10, e11)) | ~ sP1), inference(cnf_transformation, [], [f88])).
fof(f88, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e10 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1413, plain, (~ spl48_145 | ~ spl48_42), inference(avatar_split_clause, [], [f644, f833, f1408])).
fof(f644, plain, (~ (e11 = op1(e11, e11)) | ~ sP1), inference(duplicate_literal_removal, [], [f422])).
fof(f422, plain, (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11)) | ~ sP1), inference(cnf_transformation, [], [f88])).
fof(f1406, plain, (~ spl48_144 | spl48_53), inference(avatar_split_clause, [], [f415, f880, f1399])).
fof(f415, plain, ((e10 = op1(e10, e12)) | ~ sP2), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e10 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1397, plain, (~ spl48_143 | spl48_49), inference(avatar_split_clause, [], [f410, f863, f1390])).
fof(f410, plain, ((e10 = op1(e10, e13)) | ~ sP3), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e10 = op1(e10, e13))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f1388, plain, (~ spl48_142 | spl48_46), inference(avatar_split_clause, [], [f405, f850, f1381])).
fof(f405, plain, ((e11 = op1(e11, e10)) | ~ sP4), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e11 = op1(e11, e10))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f1377, plain, (~ spl48_141 | ~ spl48_42), inference(avatar_split_clause, [], [f648, f833, f1372])).
fof(f648, plain, (~ (e11 = op1(e11, e11)) | ~ sP5), inference(duplicate_literal_removal, [], [f402])).
fof(f402, plain, (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f1370, plain, (~ spl48_140 | spl48_38), inference(avatar_split_clause, [], [f395, f816, f1363])).
fof(f395, plain, ((e11 = op1(e11, e12)) | ~ sP6), inference(cnf_transformation, [], [f83])).
fof(f83, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e11 = op1(e11, e12))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f1361, plain, (~ spl48_139 | spl48_34), inference(avatar_split_clause, [], [f390, f799, f1354])).
fof(f390, plain, ((e11 = op1(e11, e13)) | ~ sP7), inference(cnf_transformation, [], [f82])).
fof(f82, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e11 = op1(e11, e13))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f1352, plain, (~ spl48_138 | spl48_31), inference(avatar_split_clause, [], [f385, f786, f1345])).
fof(f385, plain, ((e12 = op1(e12, e10)) | ~ sP8), inference(cnf_transformation, [], [f81])).
fof(f81, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e12 = op1(e12, e10))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f1341, plain, (~ spl48_137 | ~ spl48_42), inference(avatar_split_clause, [], [f652, f833, f1336])).
fof(f652, plain, (~ (e11 = op1(e11, e11)) | ~ sP9), inference(duplicate_literal_removal, [], [f382])).
fof(f382, plain, (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11)) | ~ sP9), inference(cnf_transformation, [], [f80])).
fof(f80, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e12 = op1(e12, e11))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f1334, plain, (~ spl48_136 | spl48_23), inference(avatar_split_clause, [], [f375, f752, f1327])).
fof(f375, plain, ((e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f79])).
fof(f79, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f1325, plain, (~ spl48_135 | spl48_19), inference(avatar_split_clause, [], [f370, f735, f1318])).
fof(f370, plain, ((e12 = op1(e12, e13)) | ~ sP11), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((~ (e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & (~ (e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & (~ (e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & (~ (e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e12 = op1(e12, e13))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f1316, plain, (~ spl48_134 | spl48_16), inference(avatar_split_clause, [], [f365, f722, f1309])).
fof(f365, plain, ((e13 = op1(e13, e10)) | ~ sP12), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (((~ (e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & (~ (e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & (~ (e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e13, e10))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f1315, plain, (~ spl48_134 | ~ spl48_61), inference(avatar_split_clause, [], [f655, f914, f1309])).
fof(f655, plain, (~ (e10 = op1(e10, e10)) | ~ sP12), inference(duplicate_literal_removal, [], [f366])).
fof(f366, plain, (~ (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10)) | ~ sP12), inference(cnf_transformation, [], [f77])).
fof(f1307, plain, (~ spl48_133 | spl48_12), inference(avatar_split_clause, [], [f360, f705, f1300])).
fof(f360, plain, ((e13 = op1(e13, e11)) | ~ sP13), inference(cnf_transformation, [], [f76])).
fof(f76, plain, (((~ (e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & (~ (e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & (~ (e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e13 = op1(e13, e11))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f1305, plain, (~ spl48_133 | ~ spl48_42), inference(avatar_split_clause, [], [f656, f833, f1300])).
fof(f656, plain, (~ (e11 = op1(e11, e11)) | ~ sP13), inference(duplicate_literal_removal, [], [f362])).
fof(f362, plain, (~ (e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11)) | ~ sP13), inference(cnf_transformation, [], [f76])).
fof(f1298, plain, (~ spl48_132 | spl48_8), inference(avatar_split_clause, [], [f355, f688, f1291])).
fof(f355, plain, ((e13 = op1(e13, e12)) | ~ sP14), inference(cnf_transformation, [], [f75])).
fof(f75, plain, (((~ (e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & (~ (e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & (~ (e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & (~ (e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e13 = op1(e13, e12))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f1289, plain, (~ spl48_131 | spl48_61), inference(avatar_split_clause, [], [f350, f914, f1283])).
fof(f350, plain, ((e10 = op1(e10, e10)) | ~ sP15), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP15), inference(nnf_transformation, [], [f36])).
fof(f1288, plain, (~ spl48_131 | ~ spl48_41 | spl48_46), inference(avatar_split_clause, [], [f352, f850, f829, f1283])).
fof(f352, plain, ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11)) | ~ sP15), inference(cnf_transformation, [], [f74])).
fof(f1281, plain, (~ spl48_130 | spl48_42), inference(avatar_split_clause, [], [f345, f833, f1275])).
fof(f345, plain, ((e11 = op1(e11, e11)) | ~ sP16), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP16), inference(nnf_transformation, [], [f37])).
fof(f1278, plain, (~ spl48_130 | ~ spl48_2 | spl48_12), inference(avatar_split_clause, [], [f349, f705, f663, f1275])).
fof(f349, plain, ((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13)) | ~ sP16), inference(cnf_transformation, [], [f73])).
fof(f1273, plain, (~ spl48_129 | spl48_23), inference(avatar_split_clause, [], [f340, f752, f1267])).
fof(f340, plain, ((e12 = op1(e12, e12)) | ~ sP17), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP17), inference(nnf_transformation, [], [f38])).
fof(f1265, plain, (spl48_125 | spl48_121 | spl48_117 | spl48_113), inference(avatar_split_clause, [], [f184, f1167, f1184, f1201, f1218])).
fof(f184, plain, ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax4)).
fof(f1263, plain, (spl48_126 | spl48_122 | spl48_118 | spl48_114), inference(avatar_split_clause, [], [f186, f1171, f1188, f1205, f1222])).
fof(f186, plain, ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f1261, plain, (spl48_127 | spl48_123 | spl48_119 | spl48_115), inference(avatar_split_clause, [], [f188, f1175, f1192, f1209, f1226])).
fof(f188, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1257, plain, (spl48_109 | spl48_105 | spl48_101 | spl48_97), inference(avatar_split_clause, [], [f192, f1099, f1116, f1133, f1150])).
fof(f192, plain, ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1256, plain, (spl48_121 | spl48_105 | spl48_89 | spl48_73), inference(avatar_split_clause, [], [f193, f997, f1065, f1133, f1201])).
fof(f193, plain, ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1255, plain, (spl48_110 | spl48_106 | spl48_102 | spl48_98), inference(avatar_split_clause, [], [f194, f1103, f1120, f1137, f1154])).
fof(f194, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1254, plain, (spl48_122 | spl48_106 | spl48_90 | spl48_74), inference(avatar_split_clause, [], [f195, f1001, f1069, f1137, f1205])).
fof(f195, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1251, plain, (spl48_112 | spl48_108 | spl48_104 | spl48_100), inference(avatar_split_clause, [], [f198, f1111, f1128, f1145, f1162])).
fof(f198, plain, ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1250, plain, (spl48_124 | spl48_108 | spl48_92 | spl48_76), inference(avatar_split_clause, [], [f199, f1009, f1077, f1145, f1213])).
fof(f199, plain, ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1245, plain, (spl48_95 | spl48_91 | spl48_87 | spl48_83), inference(avatar_split_clause, [], [f204, f1039, f1056, f1073, f1090])).
fof(f204, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1239, plain, (spl48_78 | spl48_74 | spl48_70 | spl48_66), inference(avatar_split_clause, [], [f210, f967, f984, f1001, f1018])).
fof(f210, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1236, plain, (spl48_115 | spl48_99 | spl48_83 | spl48_67), inference(avatar_split_clause, [], [f213, f971, f1039, f1107, f1175])).
fof(f213, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1235, plain, (spl48_80 | spl48_76 | spl48_72 | spl48_68), inference(avatar_split_clause, [], [f214, f975, f992, f1009, f1026])).
fof(f214, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1216, plain, (spl48_121 | spl48_122 | spl48_123 | spl48_124), inference(avatar_split_clause, [], [f169, f1213, f1209, f1205, f1201])).
fof(f169, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax3)).
fof(f1199, plain, (spl48_117 | spl48_118 | spl48_119 | spl48_120), inference(avatar_split_clause, [], [f170, f1196, f1192, f1188, f1184])).
fof(f170, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f1182, plain, (spl48_113 | spl48_114 | spl48_115 | spl48_116), inference(avatar_split_clause, [], [f171, f1179, f1175, f1171, f1167])).
fof(f171, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f1165, plain, (spl48_109 | spl48_110 | spl48_111 | spl48_112), inference(avatar_split_clause, [], [f172, f1162, f1158, f1154, f1150])).
fof(f172, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1131, plain, (spl48_101 | spl48_102 | spl48_103 | spl48_104), inference(avatar_split_clause, [], [f174, f1128, f1124, f1120, f1116])).
fof(f174, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f1012, plain, (spl48_73 | spl48_74 | spl48_75 | spl48_76), inference(avatar_split_clause, [], [f181, f1009, f1005, f1001, f997])).
fof(f181, plain, ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))), inference(cnf_transformation, [], [f3])).
fof(f995, plain, (spl48_69 | spl48_70 | spl48_71 | spl48_72), inference(avatar_split_clause, [], [f182, f992, f988, f984, f980])).
fof(f182, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f978, plain, (spl48_65 | spl48_66 | spl48_67 | spl48_68), inference(avatar_split_clause, [], [f183, f975, f971, f967, f963])).
fof(f183, plain, ((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))), inference(cnf_transformation, [], [f3])).
fof(f961, plain, (spl48_61 | spl48_57 | spl48_53 | spl48_49), inference(avatar_split_clause, [], [f136, f863, f880, f897, f914])).
fof(f136, plain, ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax2)).
fof(f960, plain, (spl48_61 | spl48_45 | spl48_29 | spl48_13), inference(avatar_split_clause, [], [f137, f710, f778, f846, f914])).
fof(f137, plain, ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f959, plain, (spl48_62 | spl48_58 | spl48_54 | spl48_50), inference(avatar_split_clause, [], [f138, f867, f884, f901, f918])).
fof(f138, plain, ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f957, plain, (spl48_63 | spl48_59 | spl48_55 | spl48_51), inference(avatar_split_clause, [], [f140, f871, f888, f905, f922])).
fof(f140, plain, ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f956, plain, (spl48_63 | spl48_47 | spl48_31 | spl48_15), inference(avatar_split_clause, [], [f141, f718, f786, f854, f922])).
fof(f141, plain, ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f955, plain, (spl48_64 | spl48_60 | spl48_56 | spl48_52), inference(avatar_split_clause, [], [f142, f875, f892, f909, f926])).
fof(f142, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f954, plain, (spl48_64 | spl48_48 | spl48_32 | spl48_16), inference(avatar_split_clause, [], [f143, f722, f790, f858, f926])).
fof(f143, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f953, plain, (spl48_45 | spl48_41 | spl48_37 | spl48_33), inference(avatar_split_clause, [], [f144, f795, f812, f829, f846])).
fof(f144, plain, ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f952, plain, (spl48_57 | spl48_41 | spl48_25 | spl48_9), inference(avatar_split_clause, [], [f145, f693, f761, f829, f897])).
fof(f145, plain, ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f951, plain, (spl48_46 | spl48_42 | spl48_38 | spl48_34), inference(avatar_split_clause, [], [f146, f799, f816, f833, f850])).
fof(f146, plain, ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f950, plain, (spl48_58 | spl48_42 | spl48_26 | spl48_10), inference(avatar_split_clause, [], [f147, f697, f765, f833, f901])).
fof(f147, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f941, plain, (spl48_31 | spl48_27 | spl48_23 | spl48_19), inference(avatar_split_clause, [], [f156, f735, f752, f769, f786])).
fof(f156, plain, ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f935, plain, (spl48_14 | spl48_10 | spl48_6 | spl48_2), inference(avatar_split_clause, [], [f162, f663, f680, f697, f714])).
fof(f162, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f934, plain, (spl48_50 | spl48_34 | spl48_18 | spl48_2), inference(avatar_split_clause, [], [f163, f663, f731, f799, f867])).
fof(f163, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f931, plain, (spl48_16 | spl48_12 | spl48_8 | spl48_4), inference(avatar_split_clause, [], [f166, f671, f688, f705, f722])).
fof(f166, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f912, plain, (spl48_57 | spl48_58 | spl48_59 | spl48_60), inference(avatar_split_clause, [], [f121, f909, f905, f901, f897])).
fof(f121, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG127+1.p', ax1)).
fof(f895, plain, (spl48_53 | spl48_54 | spl48_55 | spl48_56), inference(avatar_split_clause, [], [f122, f892, f888, f884, f880])).
fof(f122, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f861, plain, (spl48_45 | spl48_46 | spl48_47 | spl48_48), inference(avatar_split_clause, [], [f124, f858, f854, f850, f846])).
fof(f124, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f844, plain, (spl48_41 | spl48_42 | spl48_43 | spl48_44), inference(avatar_split_clause, [], [f125, f841, f837, f833, f829])).
fof(f125, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f827, plain, (spl48_37 | spl48_38 | spl48_39 | spl48_40), inference(avatar_split_clause, [], [f126, f824, f820, f816, f812])).
fof(f126, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f810, plain, (spl48_33 | spl48_34 | spl48_35 | spl48_36), inference(avatar_split_clause, [], [f127, f807, f803, f799, f795])).
fof(f127, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f708, plain, (spl48_9 | spl48_10 | spl48_11 | spl48_12), inference(avatar_split_clause, [], [f133, f705, f701, f697, f693])).
fof(f133, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))), inference(cnf_transformation, [], [f1])).
fof(f691, plain, (spl48_5 | spl48_6 | spl48_7 | spl48_8), inference(avatar_split_clause, [], [f134, f688, f684, f680, f676])).
fof(f134, plain, ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))), inference(cnf_transformation, [], [f1])).
fof(f674, plain, (spl48_1 | spl48_2 | spl48_3 | spl48_4), inference(avatar_split_clause, [], [f135, f671, f667, f663, f659])).
fof(f135, plain, ((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))), inference(cnf_transformation, [], [f1])).