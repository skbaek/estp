fof(f4910, plain, $false, inference(avatar_sat_refutation, [], [f705, f722, f756, f773, f790, f807, f841, f858, f875, f892, f910, f911, f912, f913, f916, f920, f921, f922, f928, f930, f932, f933, f934, f935, f937, f938, f939, f940, f1009, f1026, f1060, f1077, f1094, f1111, f1145, f1162, f1179, f1196, f1213, f1214, f1215, f1216, f1217, f1224, f1225, f1226, f1229, f1230, f1231, f1235, f1238, f1240, f1241, f1242, f1244, f1271, f1276, f1277, f1281, f1290, f1291, f1316, f1327, f1334, f1336, f1337, f1343, f1346, f1347, f1372, f1378, f1382, f1383, f1388, f1389, f1390, f1393, f1398, f1399, f1402, f1403, f1428, f1439, f1444, f1448, f1449, f1457, f1459, f1461, f1462, f1465, f1468, f1471, f1473, f1474, f1503, f1508, f1513, f1522, f1523, f1548, f1559, f1567, f1568, f1569, f1578, f1579, f1605, f1611, f1614, f1615, f1620, f1621, f1623, f1625, f1631, f1635, f1660, f1671, f1676, f1678, f1680, f1681, f1689, f1691, f1697, f1700, f1703, f1705, f1710, f1711, f1818, f1832, f1846, f2184, f2410, f2419, f2431, f2443, f2454, f2458, f2460, f2467, f2489, f2500, f2520, f2555, f2567, f2571, f2600, f2609, f2618, f2621, f2634, f2645, f2656, f2657, f2658, f2659, f2678, f2686, f2687, f2690, f2693, f2705, f2708, f2714, f2719, f2726, f2731, f2733, f2742, f2744, f2748, f2750, f2755, f2757, f2759, f2761, f2763, f2769, f2777, f2779, f2785, f2792, f2820, f2828, f2836, f2858, f2859, f2860, f2863, f2866, f2880, f2892, f2910, f2921, f2922, f2925, f2931, f2946, f2952, f2956, f2959, f2982, f3011, f3028, f3039, f3043, f3056, f3057, f3062, f3069, f3072, f3088, f3094, f3119, f3123, f3128, f3139, f3145, f3158, f3167, f3171, f3177, f3184, f3237, f3245, f3250, f3264, f3273, f3290, f3305, f3308, f3313, f3319, f3334, f3340, f3354, f3356, f3357, f3405, f3415, f3430, f3438, f3452, f3455, f3460, f3466, f3470, f3475, f3479, f3486, f3515, f3520, f3545, f3546, f3560, f3567, f3581, f3596, f3606, f3636, f3640, f3646, f3666, f3667, f3679, f3687, f3707, f3708, f3709, f3719, f3722, f3723, f3746, f3789, f3826, f3885, f3897, f3917, f3943, f3956, f3962, f3968, f3979, f3994, f4066, f4078, f4091, f4100, f4106, f4147, f4175, f4176, f4201, f4230, f4231, f4243, f4254, f4255, f4267, f4275, f4312, f4371, f4437, f4466, f4470, f4502, f4533, f4559, f4584, f4609, f4627, f4645, f4664, f4733, f4735, f4749, f4750, f4770, f4771, f4786, f4787, f4798, f4814, f4815, f4817, f4855, f4869, f4887])).
fof(f4887, plain, (~ spl42_12 | ~ spl42_16), inference(avatar_split_clause, [], [f4881, f702, f685])).
fof(f685, plain, (spl42_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_12])])).
fof(f702, plain, (spl42_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_16])])).
fof(f4881, plain, (~ (e13 = op1(e13, e11)) | ~ spl42_16), inference(backward_demodulation, [], [f246, f704])).
fof(f704, plain, ((e13 = op1(e13, e10)) | ~ spl42_16), inference(avatar_component_clause, [], [f702])).
fof(f246, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax5)).
fof(f4869, plain, (~ spl42_17 | ~ spl42_25), inference(avatar_split_clause, [], [f4862, f741, f707])).
fof(f707, plain, (spl42_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_17])])).
fof(f741, plain, (spl42_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_25])])).
fof(f4862, plain, (~ (e10 = op1(e12, e13)) | ~ spl42_25), inference(backward_demodulation, [], [f244, f743])).
fof(f743, plain, ((e10 = op1(e12, e11)) | ~ spl42_25), inference(avatar_component_clause, [], [f741])).
fof(f244, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4855, plain, (~ spl42_17 | ~ spl42_33), inference(avatar_split_clause, [], [f4849, f775, f707])).
fof(f775, plain, (spl42_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_33])])).
fof(f4849, plain, (~ (e10 = op1(e12, e13)) | ~ spl42_33), inference(backward_demodulation, [], [f225, f777])).
fof(f777, plain, ((e10 = op1(e11, e13)) | ~ spl42_33), inference(avatar_component_clause, [], [f775])).
fof(f225, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4817, plain, (~ spl42_35 | ~ spl42_47), inference(avatar_split_clause, [], [f4809, f834, f783])).
fof(f783, plain, (spl42_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_35])])).
fof(f834, plain, (spl42_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_47])])).
fof(f4809, plain, (~ (e12 = op1(e11, e13)) | ~ spl42_47), inference(backward_demodulation, [], [f236, f836])).
fof(f836, plain, ((e12 = op1(e11, e10)) | ~ spl42_47), inference(avatar_component_clause, [], [f834])).
fof(f236, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4815, plain, (~ spl42_43 | ~ spl42_47), inference(avatar_split_clause, [], [f4807, f834, f817])).
fof(f817, plain, (spl42_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_43])])).
fof(f4807, plain, (~ (e12 = op1(e11, e11)) | ~ spl42_47), inference(backward_demodulation, [], [f234, f836])).
fof(f234, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4814, plain, (~ spl42_15 | ~ spl42_47), inference(avatar_split_clause, [], [f4806, f834, f698])).
fof(f698, plain, (spl42_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_15])])).
fof(f4806, plain, (~ (e12 = op1(e13, e10)) | ~ spl42_47), inference(backward_demodulation, [], [f208, f836])).
fof(f208, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f4798, plain, (~ spl42_36 | ~ spl42_52), inference(avatar_split_clause, [], [f4791, f855, f787])).
fof(f787, plain, (spl42_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_36])])).
fof(f855, plain, (spl42_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_52])])).
fof(f4791, plain, (~ (e13 = op1(e11, e13)) | ~ spl42_52), inference(backward_demodulation, [], [f222, f857])).
fof(f857, plain, ((e13 = op1(e10, e13)) | ~ spl42_52), inference(avatar_component_clause, [], [f855])).
fof(f222, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4787, plain, (~ spl42_51 | ~ spl42_55), inference(avatar_split_clause, [], [f4782, f868, f851])).
fof(f851, plain, (spl42_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_51])])).
fof(f868, plain, (spl42_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_55])])).
fof(f4782, plain, (~ (e12 = op1(e10, e13)) | ~ spl42_55), inference(backward_demodulation, [], [f233, f870])).
fof(f870, plain, ((e12 = op1(e10, e12)) | ~ spl42_55), inference(avatar_component_clause, [], [f868])).
fof(f233, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f4786, plain, (~ spl42_39 | ~ spl42_55), inference(avatar_split_clause, [], [f4781, f868, f800])).
fof(f800, plain, (spl42_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_39])])).
fof(f4781, plain, (~ (e12 = op1(e11, e12)) | ~ spl42_55), inference(backward_demodulation, [], [f216, f870])).
fof(f216, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4771, plain, (~ spl42_54 | ~ spl42_58), inference(avatar_split_clause, [], [f4761, f881, f864])).
fof(f864, plain, (spl42_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_54])])).
fof(f881, plain, (spl42_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_58])])).
fof(f4761, plain, (~ (e11 = op1(e10, e12)) | ~ spl42_58), inference(backward_demodulation, [], [f231, f883])).
fof(f883, plain, ((e11 = op1(e10, e11)) | ~ spl42_58), inference(avatar_component_clause, [], [f881])).
fof(f231, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f4770, plain, (~ spl42_42 | ~ spl42_58), inference(avatar_split_clause, [], [f4758, f881, f813])).
fof(f813, plain, (spl42_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_42])])).
fof(f4758, plain, (~ (e11 = op1(e11, e11)) | ~ spl42_58), inference(backward_demodulation, [], [f210, f883])).
fof(f210, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4750, plain, (~ spl42_57 | ~ spl42_61), inference(avatar_split_clause, [], [f4738, f894, f877])).
fof(f877, plain, (spl42_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_57])])).
fof(f894, plain, (spl42_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_61])])).
fof(f4738, plain, (~ (e10 = op1(e10, e11)) | ~ spl42_61), inference(backward_demodulation, [], [f228, f896])).
fof(f896, plain, ((e10 = op1(e10, e10)) | ~ spl42_61), inference(avatar_component_clause, [], [f894])).
fof(f228, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f4749, plain, (~ spl42_45 | ~ spl42_61), inference(avatar_split_clause, [], [f4736, f894, f826])).
fof(f826, plain, (spl42_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_45])])).
fof(f4736, plain, (~ (e10 = op1(e11, e10)) | ~ spl42_61), inference(backward_demodulation, [], [f204, f896])).
fof(f204, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f4735, plain, (~ spl42_12 | ~ spl42_2 | spl42_130), inference(avatar_split_clause, [], [f4734, f1251, f643, f685])).
fof(f643, plain, (spl42_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_2])])).
fof(f1251, plain, (spl42_130 <=> (e13 = op1(e13, op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_130])])).
fof(f4734, plain, (~ (e13 = op1(e13, e11)) | (~ spl42_2 | spl42_130)), inference(forward_demodulation, [], [f1253, f645])).
fof(f645, plain, ((e11 = op1(e13, e13)) | ~ spl42_2), inference(avatar_component_clause, [], [f643])).
fof(f1253, plain, (~ (e13 = op1(e13, op1(e13, e13))) | spl42_130), inference(avatar_component_clause, [], [f1251])).
fof(f4733, plain, (~ spl42_15 | ~ spl42_5 | spl42_131), inference(avatar_split_clause, [], [f4732, f1256, f656, f698])).
fof(f656, plain, (spl42_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_5])])).
fof(f1256, plain, (spl42_131 <=> (e12 = op1(e13, op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_131])])).
fof(f4732, plain, (~ (e12 = op1(e13, e10)) | (~ spl42_5 | spl42_131)), inference(forward_demodulation, [], [f1258, f658])).
fof(f658, plain, ((e10 = op1(e13, e12)) | ~ spl42_5), inference(avatar_component_clause, [], [f656])).
fof(f1258, plain, (~ (e12 = op1(e13, op1(e13, e12))) | spl42_131), inference(avatar_component_clause, [], [f1256])).
fof(f4664, plain, (~ spl42_54 | ~ spl42_118 | spl42_266), inference(avatar_contradiction_clause, [], [f4663])).
fof(f4663, plain, ($false | (~ spl42_54 | ~ spl42_118 | spl42_266)), inference(subsumption_resolution, [], [f4662, f1170])).
fof(f1170, plain, ((e21 = op2(e20, e22)) | ~ spl42_118), inference(avatar_component_clause, [], [f1168])).
fof(f1168, plain, (spl42_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_118])])).
fof(f4662, plain, (~ (e21 = op2(e20, e22)) | (~ spl42_54 | spl42_266)), inference(forward_demodulation, [], [f4661, f1737])).
fof(f1737, plain, (e21 = h3(e11)), inference(forward_demodulation, [], [f1736, f1735])).
fof(f1735, plain, (e21 = op2(h3(e13), h3(e13))), inference(backward_demodulation, [], [f556, f569])).
fof(f569, plain, (op2(e22, e22) = h3(e13)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, e22) = h3(e13)) & (op2(op2(e22, e22), op2(e22, e22)) = h3(e11)) & (op2(op2(e22, e22), e22) = h3(e10)) & (e22 = h3(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax16)).
fof(f556, plain, (e21 = op2(op2(e22, e22), op2(e22, e22))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(e22, e22)) & (e21 = op2(op2(e22, e22), op2(e22, e22))) & (e20 = op2(op2(e22, e22), e22))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax13)).
fof(f1736, plain, (h3(e11) = op2(h3(e13), h3(e13))), inference(forward_demodulation, [], [f568, f569])).
fof(f568, plain, (op2(op2(e22, e22), op2(e22, e22)) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f4661, plain, (~ (op2(e20, e22) = h3(e11)) | (~ spl42_54 | spl42_266)), inference(forward_demodulation, [], [f2175, f866])).
fof(f866, plain, ((e11 = op1(e10, e12)) | ~ spl42_54), inference(avatar_component_clause, [], [f864])).
fof(f2175, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | spl42_266), inference(avatar_component_clause, [], [f2173])).
fof(f2173, plain, (spl42_266 <=> (op2(e20, e22) = h3(op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_266])])).
fof(f4645, plain, (~ spl42_45 | ~ spl42_109 | spl42_264), inference(avatar_contradiction_clause, [], [f4644])).
fof(f4644, plain, ($false | (~ spl42_45 | ~ spl42_109 | spl42_264)), inference(subsumption_resolution, [], [f4643, f1132])).
fof(f1132, plain, ((e20 = op2(e21, e20)) | ~ spl42_109), inference(avatar_component_clause, [], [f1130])).
fof(f1130, plain, (spl42_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_109])])).
fof(f4643, plain, (~ (e20 = op2(e21, e20)) | (~ spl42_45 | spl42_264)), inference(forward_demodulation, [], [f4642, f1739])).
fof(f1739, plain, (e20 = h3(e10)), inference(forward_demodulation, [], [f1738, f1734])).
fof(f1734, plain, (e20 = op2(h3(e13), e22)), inference(backward_demodulation, [], [f555, f569])).
fof(f555, plain, (e20 = op2(op2(e22, e22), e22)), inference(cnf_transformation, [], [f13])).
fof(f1738, plain, (h3(e10) = op2(h3(e13), e22)), inference(forward_demodulation, [], [f567, f569])).
fof(f567, plain, (op2(op2(e22, e22), e22) = h3(e10)), inference(cnf_transformation, [], [f16])).
fof(f4642, plain, (~ (op2(e21, e20) = h3(e10)) | (~ spl42_45 | spl42_264)), inference(forward_demodulation, [], [f2167, f828])).
fof(f828, plain, ((e10 = op1(e11, e10)) | ~ spl42_45), inference(avatar_component_clause, [], [f826])).
fof(f2167, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | spl42_264), inference(avatar_component_clause, [], [f2165])).
fof(f2165, plain, (spl42_264 <=> (op2(e21, e20) = h3(op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_264])])).
fof(f4627, plain, (~ spl42_36 | ~ spl42_100 | ~ spl42_252 | spl42_261), inference(avatar_contradiction_clause, [], [f4626])).
fof(f4626, plain, ($false | (~ spl42_36 | ~ spl42_100 | ~ spl42_252 | spl42_261)), inference(subsumption_resolution, [], [f4625, f1093])).
fof(f1093, plain, ((e23 = op2(e21, e23)) | ~ spl42_100), inference(avatar_component_clause, [], [f1091])).
fof(f1091, plain, (spl42_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_100])])).
fof(f4625, plain, (~ (e23 = op2(e21, e23)) | (~ spl42_36 | ~ spl42_252 | spl42_261)), inference(forward_demodulation, [], [f4624, f2118])).
fof(f2118, plain, ((e23 = h3(e13)) | ~ spl42_252), inference(avatar_component_clause, [], [f2117])).
fof(f2117, plain, (spl42_252 <=> (e23 = h3(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_252])])).
fof(f4624, plain, (~ (op2(e21, e23) = h3(e13)) | (~ spl42_36 | ~ spl42_252 | spl42_261)), inference(forward_demodulation, [], [f4623, f789])).
fof(f789, plain, ((e13 = op1(e11, e13)) | ~ spl42_36), inference(avatar_component_clause, [], [f787])).
fof(f4623, plain, (~ (op2(e21, e23) = h3(op1(e11, e13))) | (~ spl42_252 | spl42_261)), inference(forward_demodulation, [], [f2155, f2118])).
fof(f2155, plain, (~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | spl42_261), inference(avatar_component_clause, [], [f2153])).
fof(f2153, plain, (spl42_261 <=> (h3(op1(e11, e13)) = op2(e21, h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_261])])).
fof(f4609, plain, (~ spl42_24 | spl42_258), inference(avatar_contradiction_clause, [], [f4608])).
fof(f4608, plain, ($false | (~ spl42_24 | spl42_258)), inference(trivial_inequality_removal, [], [f4607])).
fof(f4607, plain, (~ (h3(e13) = h3(e13)) | (~ spl42_24 | spl42_258)), inference(forward_demodulation, [], [f2143, f738])).
fof(f738, plain, ((e13 = op1(e12, e12)) | ~ spl42_24), inference(avatar_component_clause, [], [f736])).
fof(f736, plain, (spl42_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_24])])).
fof(f2143, plain, (~ (h3(e13) = h3(op1(e12, e12))) | spl42_258), inference(avatar_component_clause, [], [f2141])).
fof(f2141, plain, (spl42_258 <=> (h3(e13) = h3(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_258])])).
fof(f4584, plain, (~ spl42_57 | ~ spl42_121 | spl42_267), inference(avatar_contradiction_clause, [], [f4583])).
fof(f4583, plain, ($false | (~ spl42_57 | ~ spl42_121 | spl42_267)), inference(subsumption_resolution, [], [f4582, f1183])).
fof(f1183, plain, ((e20 = op2(e20, e21)) | ~ spl42_121), inference(avatar_component_clause, [], [f1181])).
fof(f1181, plain, (spl42_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_121])])).
fof(f4582, plain, (~ (e20 = op2(e20, e21)) | (~ spl42_57 | spl42_267)), inference(forward_demodulation, [], [f4581, f1739])).
fof(f4581, plain, (~ (op2(e20, e21) = h3(e10)) | (~ spl42_57 | spl42_267)), inference(forward_demodulation, [], [f2179, f879])).
fof(f879, plain, ((e10 = op1(e10, e11)) | ~ spl42_57), inference(avatar_component_clause, [], [f877])).
fof(f2179, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | spl42_267), inference(avatar_component_clause, [], [f2177])).
fof(f2177, plain, (spl42_267 <=> (op2(e20, e21) = h3(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_267])])).
fof(f4559, plain, (~ spl42_51 | ~ spl42_115 | ~ spl42_252 | spl42_265), inference(avatar_contradiction_clause, [], [f4558])).
fof(f4558, plain, ($false | (~ spl42_51 | ~ spl42_115 | ~ spl42_252 | spl42_265)), inference(subsumption_resolution, [], [f4557, f1157])).
fof(f1157, plain, ((e22 = op2(e20, e23)) | ~ spl42_115), inference(avatar_component_clause, [], [f1155])).
fof(f1155, plain, (spl42_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_115])])).
fof(f4557, plain, (~ (e22 = op2(e20, e23)) | (~ spl42_51 | ~ spl42_252 | spl42_265)), inference(forward_demodulation, [], [f4556, f566])).
fof(f566, plain, (e22 = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f4556, plain, (~ (op2(e20, e23) = h3(e12)) | (~ spl42_51 | ~ spl42_252 | spl42_265)), inference(forward_demodulation, [], [f4555, f853])).
fof(f853, plain, ((e12 = op1(e10, e13)) | ~ spl42_51), inference(avatar_component_clause, [], [f851])).
fof(f4555, plain, (~ (op2(e20, e23) = h3(op1(e10, e13))) | (~ spl42_252 | spl42_265)), inference(forward_demodulation, [], [f2171, f2118])).
fof(f2171, plain, (~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | spl42_265), inference(avatar_component_clause, [], [f2169])).
fof(f2169, plain, (spl42_265 <=> (h3(op1(e10, e13)) = op2(e20, h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_265])])).
fof(f4533, plain, (~ spl42_39 | ~ spl42_103 | spl42_262), inference(avatar_contradiction_clause, [], [f4532])).
fof(f4532, plain, ($false | (~ spl42_39 | ~ spl42_103 | spl42_262)), inference(subsumption_resolution, [], [f4531, f1106])).
fof(f1106, plain, ((e22 = op2(e21, e22)) | ~ spl42_103), inference(avatar_component_clause, [], [f1104])).
fof(f1104, plain, (spl42_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_103])])).
fof(f4531, plain, (~ (e22 = op2(e21, e22)) | (~ spl42_39 | spl42_262)), inference(forward_demodulation, [], [f4530, f566])).
fof(f4530, plain, (~ (op2(e21, e22) = h3(e12)) | (~ spl42_39 | spl42_262)), inference(forward_demodulation, [], [f2159, f802])).
fof(f802, plain, ((e12 = op1(e11, e12)) | ~ spl42_39), inference(avatar_component_clause, [], [f800])).
fof(f2159, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | spl42_262), inference(avatar_component_clause, [], [f2157])).
fof(f2157, plain, (spl42_262 <=> (op2(e21, e22) = h3(op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_262])])).
fof(f4502, plain, (~ spl42_30 | ~ spl42_94 | spl42_260), inference(avatar_contradiction_clause, [], [f4501])).
fof(f4501, plain, ($false | (~ spl42_30 | ~ spl42_94 | spl42_260)), inference(subsumption_resolution, [], [f4500, f1068])).
fof(f1068, plain, ((e21 = op2(e22, e20)) | ~ spl42_94), inference(avatar_component_clause, [], [f1066])).
fof(f1066, plain, (spl42_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_94])])).
fof(f4500, plain, (~ (e21 = op2(e22, e20)) | (~ spl42_30 | spl42_260)), inference(forward_demodulation, [], [f4499, f1737])).
fof(f4499, plain, (~ (op2(e22, e20) = h3(e11)) | (~ spl42_30 | spl42_260)), inference(forward_demodulation, [], [f2151, f764])).
fof(f764, plain, ((e11 = op1(e12, e10)) | ~ spl42_30), inference(avatar_component_clause, [], [f762])).
fof(f762, plain, (spl42_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_30])])).
fof(f2151, plain, (~ (op2(e22, e20) = h3(op1(e12, e10))) | spl42_260), inference(avatar_component_clause, [], [f2149])).
fof(f2149, plain, (spl42_260 <=> (op2(e22, e20) = h3(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_260])])).
fof(f4470, plain, (~ spl42_75 | ~ spl42_91), inference(avatar_split_clause, [], [f4467, f1053, f985])).
fof(f985, plain, (spl42_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_75])])).
fof(f1053, plain, (spl42_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_91])])).
fof(f4467, plain, (~ (e22 = op2(e23, e21)) | ~ spl42_91), inference(backward_demodulation, [], [f263, f1055])).
fof(f1055, plain, ((e22 = op2(e22, e21)) | ~ spl42_91), inference(avatar_component_clause, [], [f1053])).
fof(f263, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax6)).
fof(f4466, plain, (~ spl42_90 | ~ spl42_94), inference(avatar_split_clause, [], [f4462, f1066, f1049])).
fof(f1049, plain, (spl42_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_90])])).
fof(f4462, plain, (~ (e21 = op2(e22, e21)) | ~ spl42_94), inference(backward_demodulation, [], [f288, f1068])).
fof(f288, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f4437, plain, (~ spl42_90 | ~ spl42_214), inference(avatar_split_clause, [], [f4428, f1872, f1049])).
fof(f1872, plain, (spl42_214 <=> (e21 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_214])])).
fof(f4428, plain, (~ (e21 = op2(e22, e21)) | ~ spl42_214), inference(backward_demodulation, [], [f1721, f1873])).
fof(f1873, plain, ((e21 = h2(e13)) | ~ spl42_214), inference(avatar_component_clause, [], [f1872])).
fof(f1721, plain, ~ (op2(e22, e21) = h2(e13)), inference(backward_demodulation, [], [f261, f565])).
fof(f565, plain, (op2(e21, e21) = h2(e13)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, e21) = h2(e13)) & (h2(e11) = op2(op2(e21, e21), op2(e21, e21))) & (h2(e10) = op2(op2(e21, e21), e21)) & (e21 = h2(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax15)).
fof(f261, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f4371, plain, (~ spl42_76 | ~ spl42_12 | ~ spl42_252 | spl42_255), inference(avatar_split_clause, [], [f4370, f2129, f2117, f685, f989])).
fof(f989, plain, (spl42_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_76])])).
fof(f2129, plain, (spl42_255 <=> (h3(op1(e13, e11)) = op2(h3(e13), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_255])])).
fof(f4370, plain, (~ (e23 = op2(e23, e21)) | (~ spl42_12 | ~ spl42_252 | spl42_255)), inference(forward_demodulation, [], [f4369, f2118])).
fof(f4369, plain, (~ (op2(e23, e21) = h3(e13)) | (~ spl42_12 | ~ spl42_252 | spl42_255)), inference(forward_demodulation, [], [f4368, f687])).
fof(f687, plain, ((e13 = op1(e13, e11)) | ~ spl42_12), inference(avatar_component_clause, [], [f685])).
fof(f4368, plain, (~ (op2(e23, e21) = h3(op1(e13, e11))) | (~ spl42_252 | spl42_255)), inference(forward_demodulation, [], [f2131, f2118])).
fof(f2131, plain, (~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | spl42_255), inference(avatar_component_clause, [], [f2129])).
fof(f4312, plain, (~ spl42_125 | spl42_187 | ~ spl42_230 | ~ spl42_232), inference(avatar_contradiction_clause, [], [f4311])).
fof(f4311, plain, ($false | (~ spl42_125 | spl42_187 | ~ spl42_230 | ~ spl42_232)), inference(subsumption_resolution, [], [f4304, f1200])).
fof(f1200, plain, ((e20 = op2(e20, e20)) | ~ spl42_125), inference(avatar_component_clause, [], [f1198])).
fof(f1198, plain, (spl42_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_125])])).
fof(f4304, plain, (~ (e20 = op2(e20, e20)) | (spl42_187 | ~ spl42_230 | ~ spl42_232)), inference(backward_demodulation, [], [f4214, f1966])).
fof(f1966, plain, ((e20 = h1(e10)) | ~ spl42_232), inference(avatar_component_clause, [], [f1965])).
fof(f1965, plain, (spl42_232 <=> (e20 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_232])])).
fof(f4214, plain, (~ (e20 = op2(e20, h1(e10))) | (spl42_187 | ~ spl42_230)), inference(forward_demodulation, [], [f1658, f4171])).
fof(f4171, plain, ((op2(e20, e20) = h1(e10)) | ~ spl42_230), inference(backward_demodulation, [], [f1719, f1954])).
fof(f1954, plain, ((e20 = h1(e13)) | ~ spl42_230), inference(avatar_component_clause, [], [f1953])).
fof(f1953, plain, (spl42_230 <=> (e20 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_230])])).
fof(f1719, plain, (h1(e10) = op2(h1(e13), e20)), inference(forward_demodulation, [], [f559, f561])).
fof(f561, plain, (op2(e20, e20) = h1(e13)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, e20) = h1(e13)) & (h1(e11) = op2(op2(e20, e20), op2(e20, e20))) & (h1(e10) = op2(op2(e20, e20), e20)) & (e20 = h1(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax14)).
fof(f559, plain, (h1(e10) = op2(op2(e20, e20), e20)), inference(cnf_transformation, [], [f14])).
fof(f1658, plain, (~ (e20 = op2(e20, op2(e20, e20))) | spl42_187), inference(avatar_component_clause, [], [f1656])).
fof(f1656, plain, (spl42_187 <=> (e20 = op2(e20, op2(e20, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl42_187])])).
fof(f4275, plain, (spl42_232 | ~ spl42_125 | ~ spl42_230), inference(avatar_split_clause, [], [f4222, f1953, f1198, f1965])).
fof(f4222, plain, ((e20 = h1(e10)) | (~ spl42_125 | ~ spl42_230)), inference(backward_demodulation, [], [f4171, f1200])).
fof(f4267, plain, (~ spl42_76 | ~ spl42_80), inference(avatar_split_clause, [], [f4265, f1006, f989])).
fof(f1006, plain, (spl42_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_80])])).
fof(f4265, plain, (~ (e23 = op2(e23, e21)) | ~ spl42_80), inference(backward_demodulation, [], [f294, f1008])).
fof(f1008, plain, ((e23 = op2(e23, e20)) | ~ spl42_80), inference(avatar_component_clause, [], [f1006])).
fof(f294, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4255, plain, (~ spl42_83 | ~ spl42_95), inference(avatar_split_clause, [], [f4251, f1070, f1019])).
fof(f1019, plain, (spl42_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_83])])).
fof(f1070, plain, (spl42_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_95])])).
fof(f4251, plain, (~ (e22 = op2(e22, e23)) | ~ spl42_95), inference(backward_demodulation, [], [f290, f1072])).
fof(f1072, plain, ((e22 = op2(e22, e20)) | ~ spl42_95), inference(avatar_component_clause, [], [f1070])).
fof(f290, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4254, plain, (~ spl42_79 | ~ spl42_95), inference(avatar_split_clause, [], [f4250, f1070, f1002])).
fof(f1002, plain, (spl42_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_79])])).
fof(f4250, plain, (~ (e22 = op2(e23, e20)) | ~ spl42_95), inference(backward_demodulation, [], [f257, f1072])).
fof(f257, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f4243, plain, (~ spl42_102 | ~ spl42_110), inference(avatar_split_clause, [], [f4240, f1134, f1100])).
fof(f1100, plain, (spl42_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_102])])).
fof(f1134, plain, (spl42_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_110])])).
fof(f4240, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_110), inference(backward_demodulation, [], [f283, f1136])).
fof(f1136, plain, ((e21 = op2(e21, e20)) | ~ spl42_110), inference(avatar_component_clause, [], [f1134])).
fof(f283, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f4231, plain, (~ spl42_116 | ~ spl42_124), inference(avatar_split_clause, [], [f4228, f1193, f1159])).
fof(f1159, plain, (spl42_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_116])])).
fof(f1193, plain, (spl42_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_124])])).
fof(f4228, plain, (~ (e23 = op2(e20, e23)) | ~ spl42_124), inference(backward_demodulation, [], [f280, f1195])).
fof(f1195, plain, ((e23 = op2(e20, e21)) | ~ spl42_124), inference(avatar_component_clause, [], [f1193])).
fof(f280, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f4230, plain, (~ spl42_76 | ~ spl42_124), inference(avatar_split_clause, [], [f4227, f1193, f989])).
fof(f4227, plain, (~ (e23 = op2(e23, e21)) | ~ spl42_124), inference(backward_demodulation, [], [f260, f1195])).
fof(f260, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4201, plain, (~ spl42_121 | ~ spl42_218), inference(avatar_split_clause, [], [f4187, f1893, f1181])).
fof(f1893, plain, (spl42_218 <=> (e20 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_218])])).
fof(f4187, plain, (~ (e20 = op2(e20, e21)) | ~ spl42_218), inference(backward_demodulation, [], [f1720, f1894])).
fof(f1894, plain, ((e20 = h2(e13)) | ~ spl42_218), inference(avatar_component_clause, [], [f1893])).
fof(f1720, plain, ~ (op2(e20, e21) = h2(e13)), inference(backward_demodulation, [], [f258, f565])).
fof(f258, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4176, plain, (~ spl42_121 | ~ spl42_230), inference(avatar_split_clause, [], [f4168, f1953, f1181])).
fof(f4168, plain, (~ (e20 = op2(e20, e21)) | ~ spl42_230), inference(backward_demodulation, [], [f1715, f1954])).
fof(f1715, plain, ~ (op2(e20, e21) = h1(e13)), inference(backward_demodulation, [], [f276, f561])).
fof(f276, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f4175, plain, (~ spl42_93 | ~ spl42_230), inference(avatar_split_clause, [], [f4166, f1953, f1062])).
fof(f1062, plain, (spl42_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_93])])).
fof(f4166, plain, (~ (e20 = op2(e22, e20)) | ~ spl42_230), inference(backward_demodulation, [], [f1713, f1954])).
fof(f1713, plain, ~ (op2(e22, e20) = h1(e13)), inference(backward_demodulation, [], [f253, f561])).
fof(f253, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f4147, plain, (~ spl42_79 | ~ spl42_15 | ~ spl42_252 | spl42_256), inference(avatar_split_clause, [], [f4146, f2133, f2117, f698, f1002])).
fof(f2133, plain, (spl42_256 <=> (h3(op1(e13, e10)) = op2(h3(e13), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_256])])).
fof(f4146, plain, (~ (e22 = op2(e23, e20)) | (~ spl42_15 | ~ spl42_252 | spl42_256)), inference(forward_demodulation, [], [f4145, f566])).
fof(f4145, plain, (~ (op2(e23, e20) = h3(e12)) | (~ spl42_15 | ~ spl42_252 | spl42_256)), inference(forward_demodulation, [], [f4144, f700])).
fof(f700, plain, ((e12 = op1(e13, e10)) | ~ spl42_15), inference(avatar_component_clause, [], [f698])).
fof(f4144, plain, (~ (op2(e23, e20) = h3(op1(e13, e10))) | (~ spl42_252 | spl42_256)), inference(forward_demodulation, [], [f2135, f2118])).
fof(f2135, plain, (~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | spl42_256), inference(avatar_component_clause, [], [f2133])).
fof(f4106, plain, (~ spl42_102 | ~ spl42_107 | spl42_177), inference(avatar_contradiction_clause, [], [f4105])).
fof(f4105, plain, ($false | (~ spl42_102 | ~ spl42_107 | spl42_177)), inference(subsumption_resolution, [], [f4102, f1123])).
fof(f1123, plain, ((e22 = op2(e21, e21)) | ~ spl42_107), inference(avatar_component_clause, [], [f1121])).
fof(f1121, plain, (spl42_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_107])])).
fof(f4102, plain, (~ (e22 = op2(e21, e21)) | (~ spl42_102 | spl42_177)), inference(backward_demodulation, [], [f1592, f1102])).
fof(f1102, plain, ((e21 = op2(e21, e22)) | ~ spl42_102), inference(avatar_component_clause, [], [f1100])).
fof(f1592, plain, (~ (e22 = op2(e21, op2(e21, e22))) | spl42_177), inference(avatar_component_clause, [], [f1590])).
fof(f1590, plain, (spl42_177 <=> (e22 = op2(e21, op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl42_177])])).
fof(f4100, plain, (~ spl42_107 | ~ spl42_108), inference(avatar_contradiction_clause, [], [f4099])).
fof(f4099, plain, ($false | (~ spl42_107 | ~ spl42_108)), inference(subsumption_resolution, [], [f4098, f311])).
fof(f311, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax8)).
fof(f4098, plain, ((e22 = e23) | (~ spl42_107 | ~ spl42_108)), inference(backward_demodulation, [], [f1127, f1123])).
fof(f1127, plain, ((e23 = op2(e21, e21)) | ~ spl42_108), inference(avatar_component_clause, [], [f1125])).
fof(f1125, plain, (spl42_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_108])])).
fof(f4091, plain, (~ spl42_121 | ~ spl42_126 | spl42_186), inference(avatar_contradiction_clause, [], [f4090])).
fof(f4090, plain, ($false | (~ spl42_121 | ~ spl42_126 | spl42_186)), inference(subsumption_resolution, [], [f4089, f1204])).
fof(f1204, plain, ((op2(e20, e20) = e21) | ~ spl42_126), inference(avatar_component_clause, [], [f1202])).
fof(f1202, plain, (spl42_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl42_126])])).
fof(f4089, plain, (~ (op2(e20, e20) = e21) | (~ spl42_121 | spl42_186)), inference(backward_demodulation, [], [f1653, f1183])).
fof(f1653, plain, (~ (e21 = op2(e20, op2(e20, e21))) | spl42_186), inference(avatar_component_clause, [], [f1651])).
fof(f1651, plain, (spl42_186 <=> (e21 = op2(e20, op2(e20, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl42_186])])).
fof(f4078, plain, (~ spl42_112 | ~ spl42_97 | spl42_176), inference(avatar_split_clause, [], [f4077, f1585, f1079, f1142])).
fof(f1142, plain, (spl42_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_112])])).
fof(f1079, plain, (spl42_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_97])])).
fof(f1585, plain, (spl42_176 <=> (e23 = op2(e21, op2(e21, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl42_176])])).
fof(f4077, plain, (~ (e23 = op2(e21, e20)) | (~ spl42_97 | spl42_176)), inference(forward_demodulation, [], [f1587, f1081])).
fof(f1081, plain, ((e20 = op2(e21, e23)) | ~ spl42_97), inference(avatar_component_clause, [], [f1079])).
fof(f1587, plain, (~ (e23 = op2(e21, op2(e21, e23))) | spl42_176), inference(avatar_component_clause, [], [f1585])).
fof(f4066, plain, (~ spl42_75 | ~ spl42_210), inference(avatar_split_clause, [], [f4057, f1852, f985])).
fof(f1852, plain, (spl42_210 <=> (e22 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_210])])).
fof(f4057, plain, (~ (e22 = op2(e23, e21)) | ~ spl42_210), inference(backward_demodulation, [], [f1722, f1853])).
fof(f1853, plain, ((e22 = h2(e13)) | ~ spl42_210), inference(avatar_component_clause, [], [f1852])).
fof(f1722, plain, ~ (op2(e23, e21) = h2(e13)), inference(backward_demodulation, [], [f262, f565])).
fof(f262, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3994, plain, (~ spl42_102 | ~ spl42_118), inference(avatar_split_clause, [], [f3993, f1168, f1100])).
fof(f3993, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_118), inference(forward_demodulation, [], [f264, f1170])).
fof(f264, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3979, plain, (~ spl42_81 | ~ spl42_93), inference(avatar_split_clause, [], [f3974, f1062, f1011])).
fof(f1011, plain, (spl42_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_81])])).
fof(f3974, plain, (~ (e20 = op2(e22, e23)) | ~ spl42_93), inference(backward_demodulation, [], [f290, f1064])).
fof(f1064, plain, ((e20 = op2(e22, e20)) | ~ spl42_93), inference(avatar_component_clause, [], [f1062])).
fof(f3968, plain, (~ spl42_81 | ~ spl42_97), inference(avatar_split_clause, [], [f3963, f1079, f1011])).
fof(f3963, plain, (~ (e20 = op2(e22, e23)) | ~ spl42_97), inference(backward_demodulation, [], [f273, f1081])).
fof(f273, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3962, plain, (~ spl42_105 | ~ spl42_108), inference(avatar_contradiction_clause, [], [f3961])).
fof(f3961, plain, ($false | (~ spl42_105 | ~ spl42_108)), inference(subsumption_resolution, [], [f3959, f308])).
fof(f308, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f3959, plain, ((e20 = e23) | (~ spl42_105 | ~ spl42_108)), inference(backward_demodulation, [], [f1127, f1115])).
fof(f1115, plain, ((e20 = op2(e21, e21)) | ~ spl42_105), inference(avatar_component_clause, [], [f1113])).
fof(f1113, plain, (spl42_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_105])])).
fof(f3956, plain, (~ spl42_94 | ~ spl42_110), inference(avatar_split_clause, [], [f3950, f1134, f1066])).
fof(f3950, plain, (~ (e21 = op2(e22, e20)) | ~ spl42_110), inference(backward_demodulation, [], [f255, f1136])).
fof(f255, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f3943, plain, (~ spl42_116 | spl42_184), inference(avatar_contradiction_clause, [], [f3942])).
fof(f3942, plain, ($false | (~ spl42_116 | spl42_184)), inference(subsumption_resolution, [], [f3939, f1161])).
fof(f1161, plain, ((e23 = op2(e20, e23)) | ~ spl42_116), inference(avatar_component_clause, [], [f1159])).
fof(f3939, plain, (~ (e23 = op2(e20, e23)) | (~ spl42_116 | spl42_184)), inference(backward_demodulation, [], [f1643, f1161])).
fof(f1643, plain, (~ (e23 = op2(e20, op2(e20, e23))) | spl42_184), inference(avatar_component_clause, [], [f1641])).
fof(f1641, plain, (spl42_184 <=> (e23 = op2(e20, op2(e20, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl42_184])])).
fof(f3917, plain, (~ spl42_109 | ~ spl42_218), inference(avatar_split_clause, [], [f3906, f1893, f1130])).
fof(f3906, plain, (~ (e20 = op2(e21, e20)) | ~ spl42_218), inference(backward_demodulation, [], [f1723, f1894])).
fof(f1723, plain, ~ (op2(e21, e20) = h2(e13)), inference(backward_demodulation, [], [f282, f565])).
fof(f282, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f3897, plain, (~ spl42_79 | ~ spl42_222), inference(avatar_split_clause, [], [f3890, f1913, f1002])).
fof(f1913, plain, (spl42_222 <=> (e22 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_222])])).
fof(f3890, plain, (~ (e22 = op2(e23, e20)) | ~ spl42_222), inference(backward_demodulation, [], [f1714, f1914])).
fof(f1914, plain, ((e22 = h1(e13)) | ~ spl42_222), inference(avatar_component_clause, [], [f1913])).
fof(f1714, plain, ~ (op2(e23, e20) = h1(e13)), inference(backward_demodulation, [], [f254, f561])).
fof(f254, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3885, plain, (~ spl42_81 | ~ spl42_17 | ~ spl42_252 | spl42_257), inference(avatar_split_clause, [], [f3884, f2137, f2117, f707, f1011])).
fof(f2137, plain, (spl42_257 <=> (h3(op1(e12, e13)) = op2(e22, h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_257])])).
fof(f3884, plain, (~ (e20 = op2(e22, e23)) | (~ spl42_17 | ~ spl42_252 | spl42_257)), inference(forward_demodulation, [], [f3883, f1739])).
fof(f3883, plain, (~ (op2(e22, e23) = h3(e10)) | (~ spl42_17 | ~ spl42_252 | spl42_257)), inference(forward_demodulation, [], [f3882, f709])).
fof(f709, plain, ((e10 = op1(e12, e13)) | ~ spl42_17), inference(avatar_component_clause, [], [f707])).
fof(f3882, plain, (~ (op2(e22, e23) = h3(op1(e12, e13))) | (~ spl42_252 | spl42_257)), inference(forward_demodulation, [], [f2139, f2118])).
fof(f2139, plain, (~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | spl42_257), inference(avatar_component_clause, [], [f2137])).
fof(f3826, plain, (~ spl42_2 | spl42_253), inference(avatar_contradiction_clause, [], [f3825])).
fof(f3825, plain, ($false | (~ spl42_2 | spl42_253)), inference(subsumption_resolution, [], [f3824, f1737])).
fof(f3824, plain, (~ (e21 = h3(e11)) | (~ spl42_2 | spl42_253)), inference(forward_demodulation, [], [f2123, f645])).
fof(f2123, plain, (~ (e21 = h3(op1(e13, e13))) | spl42_253), inference(avatar_component_clause, [], [f2121])).
fof(f2121, plain, (spl42_253 <=> (e21 = h3(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_253])])).
fof(f3789, plain, (~ spl42_64 | ~ spl42_252 | spl42_268 | ~ spl42_296), inference(avatar_contradiction_clause, [], [f3788])).
fof(f3788, plain, ($false | (~ spl42_64 | ~ spl42_252 | spl42_268 | ~ spl42_296)), inference(subsumption_resolution, [], [f3787, f2342])).
fof(f2342, plain, ((e23 = h1(e13)) | ~ spl42_296), inference(avatar_component_clause, [], [f2341])).
fof(f2341, plain, (spl42_296 <=> (e23 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_296])])).
fof(f3787, plain, (~ (e23 = h1(e13)) | (~ spl42_64 | ~ spl42_252 | spl42_268)), inference(forward_demodulation, [], [f3786, f2118])).
fof(f3786, plain, (~ (h1(e13) = h3(e13)) | (~ spl42_64 | spl42_268)), inference(forward_demodulation, [], [f2183, f908])).
fof(f908, plain, ((op1(e10, e10) = e13) | ~ spl42_64), inference(avatar_component_clause, [], [f906])).
fof(f906, plain, (spl42_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl42_64])])).
fof(f2183, plain, (~ (h1(e13) = h3(op1(e10, e10))) | spl42_268), inference(avatar_component_clause, [], [f2181])).
fof(f2181, plain, (spl42_268 <=> (h1(e13) = h3(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_268])])).
fof(f3746, plain, (~ spl42_27 | ~ spl42_91 | spl42_259), inference(avatar_contradiction_clause, [], [f3745])).
fof(f3745, plain, ($false | (~ spl42_27 | ~ spl42_91 | spl42_259)), inference(subsumption_resolution, [], [f3744, f1055])).
fof(f3744, plain, (~ (e22 = op2(e22, e21)) | (~ spl42_27 | spl42_259)), inference(forward_demodulation, [], [f3743, f566])).
fof(f3743, plain, (~ (op2(e22, e21) = h3(e12)) | (~ spl42_27 | spl42_259)), inference(forward_demodulation, [], [f2147, f751])).
fof(f751, plain, ((e12 = op1(e12, e11)) | ~ spl42_27), inference(avatar_component_clause, [], [f749])).
fof(f749, plain, (spl42_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_27])])).
fof(f2147, plain, (~ (op2(e22, e21) = h3(op1(e12, e11))) | spl42_259), inference(avatar_component_clause, [], [f2145])).
fof(f2145, plain, (spl42_259 <=> (op2(e22, e21) = h3(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_259])])).
fof(f3723, plain, (~ spl42_62 | ~ spl42_54), inference(avatar_split_clause, [], [f3588, f864, f898])).
fof(f898, plain, (spl42_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl42_62])])).
fof(f3588, plain, (~ (op1(e10, e10) = e11) | ~ spl42_54), inference(forward_demodulation, [], [f229, f866])).
fof(f229, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3722, plain, (~ spl42_62 | ~ spl42_30), inference(avatar_split_clause, [], [f3589, f762, f898])).
fof(f3589, plain, (~ (op1(e10, e10) = e11) | ~ spl42_30), inference(forward_demodulation, [], [f205, f764])).
fof(f205, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f3719, plain, (~ spl42_11 | ~ spl42_15), inference(avatar_split_clause, [], [f3715, f698, f681])).
fof(f681, plain, (spl42_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_11])])).
fof(f3715, plain, (~ (e12 = op1(e13, e11)) | ~ spl42_15), inference(backward_demodulation, [], [f246, f700])).
fof(f3709, plain, (~ spl42_19 | ~ spl42_27), inference(avatar_split_clause, [], [f3706, f749, f715])).
fof(f715, plain, (spl42_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_19])])).
fof(f3706, plain, (~ (e12 = op1(e12, e13)) | ~ spl42_27), inference(backward_demodulation, [], [f244, f751])).
fof(f3708, plain, (~ spl42_11 | ~ spl42_27), inference(avatar_split_clause, [], [f3705, f749, f681])).
fof(f3705, plain, (~ (e12 = op1(e13, e11)) | ~ spl42_27), inference(backward_demodulation, [], [f215, f751])).
fof(f215, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3707, plain, (~ spl42_22 | ~ spl42_27 | spl42_139), inference(avatar_split_clause, [], [f3703, f1307, f749, f728])).
fof(f728, plain, (spl42_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_22])])).
fof(f1307, plain, (spl42_139 <=> (e11 = op1(e12, op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_139])])).
fof(f3703, plain, (~ (e11 = op1(e12, e12)) | (~ spl42_27 | spl42_139)), inference(backward_demodulation, [], [f1309, f751])).
fof(f1309, plain, (~ (e11 = op1(e12, op1(e12, e11))) | spl42_139), inference(avatar_component_clause, [], [f1307])).
fof(f3687, plain, (~ spl42_19 | ~ spl42_51), inference(avatar_split_clause, [], [f3683, f851, f715])).
fof(f3683, plain, (~ (e12 = op1(e12, e13)) | ~ spl42_51), inference(backward_demodulation, [], [f223, f853])).
fof(f223, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f3679, plain, (~ spl42_25 | ~ spl42_57), inference(avatar_split_clause, [], [f3672, f877, f741])).
fof(f3672, plain, (~ (e10 = op1(e12, e11)) | ~ spl42_57), inference(backward_demodulation, [], [f211, f879])).
fof(f211, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3667, plain, (~ spl42_52 | ~ spl42_64), inference(avatar_split_clause, [], [f3662, f906, f855])).
fof(f3662, plain, (~ (e13 = op1(e10, e13)) | ~ spl42_64), inference(backward_demodulation, [], [f230, f908])).
fof(f230, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3666, plain, (~ spl42_16 | ~ spl42_64), inference(avatar_split_clause, [], [f3660, f906, f702])).
fof(f3660, plain, (~ (e13 = op1(e13, e10)) | ~ spl42_64), inference(backward_demodulation, [], [f206, f908])).
fof(f206, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3646, plain, (~ spl42_113 | ~ spl42_121), inference(avatar_split_clause, [], [f3644, f1181, f1147])).
fof(f1147, plain, (spl42_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_113])])).
fof(f3644, plain, (~ (e20 = op2(e20, e23)) | ~ spl42_121), inference(backward_demodulation, [], [f280, f1183])).
fof(f3640, plain, (spl42_12 | ~ spl42_2 | ~ spl42_130), inference(avatar_split_clause, [], [f3639, f1251, f643, f685])).
fof(f3639, plain, ((e13 = op1(e13, e11)) | (~ spl42_2 | ~ spl42_130)), inference(forward_demodulation, [], [f1252, f645])).
fof(f1252, plain, ((e13 = op1(e13, op1(e13, e13))) | ~ spl42_130), inference(avatar_component_clause, [], [f1251])).
fof(f3636, plain, (~ spl42_19 | ~ spl42_24 | spl42_138), inference(avatar_split_clause, [], [f3635, f1302, f736, f715])).
fof(f1302, plain, (spl42_138 <=> (e12 = op1(e12, op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_138])])).
fof(f3635, plain, (~ (e12 = op1(e12, e13)) | (~ spl42_24 | spl42_138)), inference(forward_demodulation, [], [f1304, f738])).
fof(f1304, plain, (~ (e12 = op1(e12, op1(e12, e12))) | spl42_138), inference(avatar_component_clause, [], [f1302])).
fof(f3606, plain, (~ spl42_42 | ~ spl42_214 | spl42_263), inference(avatar_contradiction_clause, [], [f3605])).
fof(f3605, plain, ($false | (~ spl42_42 | ~ spl42_214 | spl42_263)), inference(subsumption_resolution, [], [f3604, f1873])).
fof(f3604, plain, (~ (e21 = h2(e13)) | (~ spl42_42 | spl42_263)), inference(forward_demodulation, [], [f3603, f1737])).
fof(f3603, plain, (~ (h2(e13) = h3(e11)) | (~ spl42_42 | spl42_263)), inference(forward_demodulation, [], [f2163, f815])).
fof(f815, plain, ((e11 = op1(e11, e11)) | ~ spl42_42), inference(avatar_component_clause, [], [f813])).
fof(f2163, plain, (~ (h2(e13) = h3(op1(e11, e11))) | spl42_263), inference(avatar_component_clause, [], [f2161])).
fof(f2161, plain, (spl42_263 <=> (h2(e13) = h3(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_263])])).
fof(f3596, plain, (~ spl42_83 | ~ spl42_91), inference(avatar_split_clause, [], [f3595, f1053, f1019])).
fof(f3595, plain, (~ (e22 = op2(e22, e23)) | ~ spl42_91), inference(backward_demodulation, [], [f292, f1055])).
fof(f292, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3581, plain, (~ spl42_35 | ~ spl42_39), inference(avatar_split_clause, [], [f3580, f800, f783])).
fof(f3580, plain, (~ (e12 = op1(e11, e13)) | ~ spl42_39), inference(forward_demodulation, [], [f239, f802])).
fof(f239, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3567, plain, (~ spl42_61 | ~ spl42_63), inference(avatar_contradiction_clause, [], [f3566])).
fof(f3566, plain, ($false | (~ spl42_61 | ~ spl42_63)), inference(subsumption_resolution, [], [f3565, f301])).
fof(f301, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax7)).
fof(f3565, plain, ((e10 = e12) | (~ spl42_61 | ~ spl42_63)), inference(forward_demodulation, [], [f904, f896])).
fof(f904, plain, ((op1(e10, e10) = e12) | ~ spl42_63), inference(avatar_component_clause, [], [f902])).
fof(f902, plain, (spl42_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl42_63])])).
fof(f3560, plain, (~ spl42_89 | ~ spl42_91), inference(avatar_contradiction_clause, [], [f3559])).
fof(f3559, plain, ($false | (~ spl42_89 | ~ spl42_91)), inference(subsumption_resolution, [], [f3558, f307])).
fof(f307, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f3558, plain, ((e20 = e22) | (~ spl42_89 | ~ spl42_91)), inference(backward_demodulation, [], [f1055, f1047])).
fof(f1047, plain, ((e20 = op2(e22, e21)) | ~ spl42_89), inference(avatar_component_clause, [], [f1045])).
fof(f1045, plain, (spl42_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_89])])).
fof(f3546, plain, (~ spl42_93 | ~ spl42_109), inference(avatar_split_clause, [], [f3541, f1130, f1062])).
fof(f3541, plain, (~ (e20 = op2(e22, e20)) | ~ spl42_109), inference(backward_demodulation, [], [f255, f1132])).
fof(f3545, plain, (~ spl42_109 | spl42_179), inference(avatar_contradiction_clause, [], [f3544])).
fof(f3544, plain, ($false | (~ spl42_109 | spl42_179)), inference(subsumption_resolution, [], [f3540, f1132])).
fof(f3540, plain, (~ (e20 = op2(e21, e20)) | (~ spl42_109 | spl42_179)), inference(backward_demodulation, [], [f1602, f1132])).
fof(f1602, plain, (~ (e20 = op2(e21, op2(e21, e20))) | spl42_179), inference(avatar_component_clause, [], [f1600])).
fof(f1600, plain, (spl42_179 <=> (e20 = op2(e21, op2(e21, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl42_179])])).
fof(f3520, plain, (~ spl42_103 | spl42_177), inference(avatar_contradiction_clause, [], [f3519])).
fof(f3519, plain, ($false | (~ spl42_103 | spl42_177)), inference(subsumption_resolution, [], [f3518, f1106])).
fof(f3518, plain, (~ (e22 = op2(e21, e22)) | (~ spl42_103 | spl42_177)), inference(forward_demodulation, [], [f1592, f1106])).
fof(f3515, plain, (~ spl42_76 | spl42_161 | ~ spl42_196), inference(avatar_contradiction_clause, [], [f3514])).
fof(f3514, plain, ($false | (~ spl42_76 | spl42_161 | ~ spl42_196)), inference(subsumption_resolution, [], [f3509, f991])).
fof(f991, plain, ((e23 = op2(e23, e21)) | ~ spl42_76), inference(avatar_component_clause, [], [f989])).
fof(f3509, plain, (~ (e23 = op2(e23, e21)) | (spl42_161 | ~ spl42_196)), inference(backward_demodulation, [], [f2408, f1774])).
fof(f1774, plain, ((e21 = h4(e13)) | ~ spl42_196), inference(avatar_component_clause, [], [f1773])).
fof(f1773, plain, (spl42_196 <=> (e21 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_196])])).
fof(f2408, plain, (~ (e23 = op2(e23, h4(e13))) | spl42_161), inference(forward_demodulation, [], [f1485, f573])).
fof(f573, plain, (op2(e23, e23) = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, e23) = h4(e13)) & (h4(e11) = op2(op2(e23, e23), op2(e23, e23))) & (h4(e10) = op2(op2(e23, e23), e23)) & (e23 = h4(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax17)).
fof(f1485, plain, (~ (e23 = op2(e23, op2(e23, e23))) | spl42_161), inference(avatar_component_clause, [], [f1483])).
fof(f1483, plain, (spl42_161 <=> (e23 = op2(e23, op2(e23, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl42_161])])).
fof(f3486, plain, (~ spl42_122 | ~ spl42_118), inference(avatar_split_clause, [], [f3485, f1168, f1185])).
fof(f1185, plain, (spl42_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_122])])).
fof(f3485, plain, (~ (e21 = op2(e20, e21)) | ~ spl42_118), inference(forward_demodulation, [], [f279, f1170])).
fof(f279, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3479, plain, (~ spl42_112 | ~ spl42_100), inference(avatar_split_clause, [], [f3478, f1091, f1142])).
fof(f3478, plain, (~ (e23 = op2(e21, e20)) | ~ spl42_100), inference(forward_demodulation, [], [f284, f1093])).
fof(f284, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3475, plain, (~ spl42_210 | ~ spl42_91), inference(avatar_split_clause, [], [f3474, f1053, f1852])).
fof(f3474, plain, (~ (e22 = h2(e13)) | ~ spl42_91), inference(forward_demodulation, [], [f1721, f1055])).
fof(f3470, plain, (~ spl42_95 | ~ spl42_91), inference(avatar_split_clause, [], [f3469, f1053, f1070])).
fof(f3469, plain, (~ (e22 = op2(e22, e20)) | ~ spl42_91), inference(forward_demodulation, [], [f288, f1055])).
fof(f3466, plain, (~ spl42_123 | ~ spl42_91), inference(avatar_split_clause, [], [f3465, f1053, f1189])).
fof(f1189, plain, (spl42_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_123])])).
fof(f3465, plain, (~ (e22 = op2(e20, e21)) | ~ spl42_91), inference(forward_demodulation, [], [f259, f1055])).
fof(f259, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3460, plain, (~ spl42_16 | ~ spl42_48), inference(avatar_split_clause, [], [f3459, f838, f702])).
fof(f838, plain, (spl42_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_48])])).
fof(f3459, plain, (~ (e13 = op1(e13, e10)) | ~ spl42_48), inference(forward_demodulation, [], [f208, f840])).
fof(f840, plain, ((e13 = op1(e11, e10)) | ~ spl42_48), inference(avatar_component_clause, [], [f838])).
fof(f3455, plain, (~ spl42_11 | ~ spl42_59), inference(avatar_split_clause, [], [f3454, f885, f681])).
fof(f885, plain, (spl42_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_59])])).
fof(f3454, plain, (~ (e12 = op1(e13, e11)) | ~ spl42_59), inference(forward_demodulation, [], [f212, f887])).
fof(f887, plain, ((e12 = op1(e10, e11)) | ~ spl42_59), inference(avatar_component_clause, [], [f885])).
fof(f212, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3452, plain, (~ spl42_5 | ~ spl42_7), inference(avatar_contradiction_clause, [], [f3451])).
fof(f3451, plain, ($false | (~ spl42_5 | ~ spl42_7)), inference(subsumption_resolution, [], [f3450, f301])).
fof(f3450, plain, ((e10 = e12) | (~ spl42_5 | ~ spl42_7)), inference(forward_demodulation, [], [f666, f658])).
fof(f666, plain, ((e12 = op1(e13, e12)) | ~ spl42_7), inference(avatar_component_clause, [], [f664])).
fof(f664, plain, (spl42_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_7])])).
fof(f3438, plain, (~ spl42_38 | ~ spl42_54), inference(avatar_split_clause, [], [f3433, f864, f796])).
fof(f796, plain, (spl42_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_38])])).
fof(f3433, plain, (~ (e11 = op1(e11, e12)) | ~ spl42_54), inference(backward_demodulation, [], [f216, f866])).
fof(f3430, plain, (~ spl42_69 | ~ spl42_71), inference(avatar_contradiction_clause, [], [f3429])).
fof(f3429, plain, ($false | (~ spl42_69 | ~ spl42_71)), inference(subsumption_resolution, [], [f3428, f307])).
fof(f3428, plain, ((e20 = e22) | (~ spl42_69 | ~ spl42_71)), inference(forward_demodulation, [], [f970, f962])).
fof(f962, plain, ((e20 = op2(e23, e22)) | ~ spl42_69), inference(avatar_component_clause, [], [f960])).
fof(f960, plain, (spl42_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_69])])).
fof(f970, plain, ((e22 = op2(e23, e22)) | ~ spl42_71), inference(avatar_component_clause, [], [f968])).
fof(f968, plain, (spl42_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_71])])).
fof(f3415, plain, (~ spl42_93 | spl42_171), inference(avatar_contradiction_clause, [], [f3414])).
fof(f3414, plain, ($false | (~ spl42_93 | spl42_171)), inference(subsumption_resolution, [], [f3411, f1064])).
fof(f3411, plain, (~ (e20 = op2(e22, e20)) | (~ spl42_93 | spl42_171)), inference(backward_demodulation, [], [f1546, f1064])).
fof(f1546, plain, (~ (e20 = op2(e22, op2(e22, e20))) | spl42_171), inference(avatar_component_clause, [], [f1544])).
fof(f1544, plain, (spl42_171 <=> (e20 = op2(e22, op2(e22, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl42_171])])).
fof(f3405, plain, (~ spl42_111 | ~ spl42_112), inference(avatar_contradiction_clause, [], [f3404])).
fof(f3404, plain, ($false | (~ spl42_111 | ~ spl42_112)), inference(subsumption_resolution, [], [f3403, f311])).
fof(f3403, plain, ((e22 = e23) | (~ spl42_111 | ~ spl42_112)), inference(backward_demodulation, [], [f1144, f1140])).
fof(f1140, plain, ((e22 = op2(e21, e20)) | ~ spl42_111), inference(avatar_component_clause, [], [f1138])).
fof(f1138, plain, (spl42_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_111])])).
fof(f1144, plain, ((e23 = op2(e21, e20)) | ~ spl42_112), inference(avatar_component_clause, [], [f1142])).
fof(f3357, plain, (spl42_296 | ~ spl42_128), inference(avatar_split_clause, [], [f3222, f1210, f2341])).
fof(f1210, plain, (spl42_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl42_128])])).
fof(f3222, plain, ((e23 = h1(e13)) | ~ spl42_128), inference(backward_demodulation, [], [f561, f1212])).
fof(f1212, plain, ((op2(e20, e20) = e23) | ~ spl42_128), inference(avatar_component_clause, [], [f1210])).
fof(f3356, plain, (spl42_196 | ~ spl42_66), inference(avatar_split_clause, [], [f3355, f947, f1773])).
fof(f947, plain, (spl42_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_66])])).
fof(f3355, plain, ((e21 = h4(e13)) | ~ spl42_66), inference(forward_demodulation, [], [f573, f949])).
fof(f949, plain, ((e21 = op2(e23, e23)) | ~ spl42_66), inference(avatar_component_clause, [], [f947])).
fof(f3354, plain, (~ spl42_196 | ~ spl42_98), inference(avatar_split_clause, [], [f3353, f1083, f1773])).
fof(f1083, plain, (spl42_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_98])])).
fof(f3353, plain, (~ (e21 = h4(e13)) | ~ spl42_98), inference(forward_demodulation, [], [f1741, f1085])).
fof(f1085, plain, ((e21 = op2(e21, e23)) | ~ spl42_98), inference(avatar_component_clause, [], [f1083])).
fof(f1741, plain, ~ (op2(e21, e23) = h4(e13)), inference(backward_demodulation, [], [f274, f573])).
fof(f274, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3340, plain, (~ spl42_119 | ~ spl42_103), inference(avatar_split_clause, [], [f3339, f1104, f1172])).
fof(f1172, plain, (spl42_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_119])])).
fof(f3339, plain, (~ (e22 = op2(e20, e22)) | ~ spl42_103), inference(forward_demodulation, [], [f264, f1106])).
fof(f3334, plain, (~ spl42_111 | ~ spl42_103), inference(avatar_split_clause, [], [f3333, f1104, f1138])).
fof(f3333, plain, (~ (e22 = op2(e21, e20)) | ~ spl42_103), inference(forward_demodulation, [], [f283, f1106])).
fof(f3319, plain, (~ spl42_27 | ~ spl42_59), inference(avatar_split_clause, [], [f3318, f885, f749])).
fof(f3318, plain, (~ (e12 = op1(e12, e11)) | ~ spl42_59), inference(forward_demodulation, [], [f211, f887])).
fof(f3313, plain, (~ spl42_9 | ~ spl42_12), inference(avatar_contradiction_clause, [], [f3312])).
fof(f3312, plain, ($false | (~ spl42_9 | ~ spl42_12)), inference(subsumption_resolution, [], [f3310, f302])).
fof(f302, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f3310, plain, ((e10 = e13) | (~ spl42_9 | ~ spl42_12)), inference(backward_demodulation, [], [f687, f675])).
fof(f675, plain, ((e10 = op1(e13, e11)) | ~ spl42_9), inference(avatar_component_clause, [], [f673])).
fof(f673, plain, (spl42_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_9])])).
fof(f3308, plain, (~ spl42_17 | ~ spl42_19), inference(avatar_contradiction_clause, [], [f3307])).
fof(f3307, plain, ($false | (~ spl42_17 | ~ spl42_19)), inference(subsumption_resolution, [], [f3306, f301])).
fof(f3306, plain, ((e10 = e12) | (~ spl42_17 | ~ spl42_19)), inference(forward_demodulation, [], [f717, f709])).
fof(f717, plain, ((e12 = op1(e12, e13)) | ~ spl42_19), inference(avatar_component_clause, [], [f715])).
fof(f3305, plain, (~ spl42_33 | ~ spl42_48 | spl42_145), inference(avatar_contradiction_clause, [], [f3304])).
fof(f3304, plain, ($false | (~ spl42_33 | ~ spl42_48 | spl42_145)), inference(subsumption_resolution, [], [f3301, f840])).
fof(f3301, plain, (~ (e13 = op1(e11, e10)) | (~ spl42_33 | spl42_145)), inference(backward_demodulation, [], [f1355, f777])).
fof(f1355, plain, (~ (e13 = op1(e11, op1(e11, e13))) | spl42_145), inference(avatar_component_clause, [], [f1353])).
fof(f1353, plain, (spl42_145 <=> (e13 = op1(e11, op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_145])])).
fof(f3290, plain, (~ spl42_52 | spl42_153), inference(avatar_contradiction_clause, [], [f3289])).
fof(f3289, plain, ($false | (~ spl42_52 | spl42_153)), inference(subsumption_resolution, [], [f3286, f857])).
fof(f3286, plain, (~ (e13 = op1(e10, e13)) | (~ spl42_52 | spl42_153)), inference(backward_demodulation, [], [f1411, f857])).
fof(f1411, plain, (~ (e13 = op1(e10, op1(e10, e13))) | spl42_153), inference(avatar_component_clause, [], [f1409])).
fof(f1409, plain, (spl42_153 <=> (e13 = op1(e10, op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_153])])).
fof(f3273, plain, (~ spl42_55 | ~ spl42_59), inference(avatar_split_clause, [], [f3270, f885, f868])).
fof(f3270, plain, (~ (e12 = op1(e10, e12)) | ~ spl42_59), inference(backward_demodulation, [], [f231, f887])).
fof(f3264, plain, (~ spl42_66 | ~ spl42_68), inference(avatar_contradiction_clause, [], [f3263])).
fof(f3263, plain, ($false | (~ spl42_66 | ~ spl42_68)), inference(subsumption_resolution, [], [f3262, f310])).
fof(f310, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f3262, plain, ((e21 = e23) | (~ spl42_66 | ~ spl42_68)), inference(backward_demodulation, [], [f957, f949])).
fof(f957, plain, ((e23 = op2(e23, e23)) | ~ spl42_68), inference(avatar_component_clause, [], [f955])).
fof(f955, plain, (spl42_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_68])])).
fof(f3250, plain, (~ spl42_81 | ~ spl42_83), inference(avatar_contradiction_clause, [], [f3249])).
fof(f3249, plain, ($false | (~ spl42_81 | ~ spl42_83)), inference(subsumption_resolution, [], [f3247, f307])).
fof(f3247, plain, ((e20 = e22) | (~ spl42_81 | ~ spl42_83)), inference(backward_demodulation, [], [f1021, f1013])).
fof(f1013, plain, ((e20 = op2(e22, e23)) | ~ spl42_81), inference(avatar_component_clause, [], [f1011])).
fof(f1021, plain, ((e22 = op2(e22, e23)) | ~ spl42_83), inference(avatar_component_clause, [], [f1019])).
fof(f3245, plain, (~ spl42_86 | ~ spl42_88), inference(avatar_contradiction_clause, [], [f3244])).
fof(f3244, plain, ($false | (~ spl42_86 | ~ spl42_88)), inference(subsumption_resolution, [], [f3243, f310])).
fof(f3243, plain, ((e21 = e23) | (~ spl42_86 | ~ spl42_88)), inference(backward_demodulation, [], [f1042, f1034])).
fof(f1034, plain, ((e21 = op2(e22, e22)) | ~ spl42_86), inference(avatar_component_clause, [], [f1032])).
fof(f1032, plain, (spl42_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_86])])).
fof(f1042, plain, ((e23 = op2(e22, e22)) | ~ spl42_88), inference(avatar_component_clause, [], [f1040])).
fof(f1040, plain, (spl42_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_88])])).
fof(f3237, plain, (~ spl42_99 | ~ spl42_103), inference(avatar_split_clause, [], [f3236, f1104, f1087])).
fof(f1087, plain, (spl42_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_99])])).
fof(f3236, plain, (~ (e22 = op2(e21, e23)) | ~ spl42_103), inference(backward_demodulation, [], [f287, f1106])).
fof(f287, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3184, plain, (~ spl42_104 | ~ spl42_252), inference(avatar_split_clause, [], [f3023, f2117, f1108])).
fof(f1108, plain, (spl42_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_104])])).
fof(f3023, plain, (~ (e23 = op2(e21, e22)) | ~ spl42_252), inference(forward_demodulation, [], [f1729, f2118])).
fof(f1729, plain, ~ (op2(e21, e22) = h3(e13)), inference(backward_demodulation, [], [f267, f569])).
fof(f267, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f3177, plain, (~ spl42_222 | ~ spl42_95), inference(avatar_split_clause, [], [f3176, f1070, f1913])).
fof(f3176, plain, (~ (e22 = h1(e13)) | ~ spl42_95), inference(forward_demodulation, [], [f1713, f1072])).
fof(f3171, plain, (~ spl42_84 | ~ spl42_252), inference(avatar_split_clause, [], [f2996, f2117, f1023])).
fof(f1023, plain, (spl42_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_84])])).
fof(f2996, plain, (~ (e23 = op2(e22, e23)) | ~ spl42_252), inference(forward_demodulation, [], [f1733, f2118])).
fof(f1733, plain, ~ (op2(e22, e23) = h3(e13)), inference(backward_demodulation, [], [f293, f569])).
fof(f293, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3167, plain, (~ spl42_77 | ~ spl42_69), inference(avatar_split_clause, [], [f3166, f960, f994])).
fof(f994, plain, (spl42_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_77])])).
fof(f3166, plain, (~ (e20 = op2(e23, e20)) | ~ spl42_69), inference(forward_demodulation, [], [f295, f962])).
fof(f295, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3158, plain, (spl42_66 | ~ spl42_252), inference(avatar_split_clause, [], [f2857, f2117, f947])).
fof(f2857, plain, ((e21 = op2(e23, e23)) | ~ spl42_252), inference(backward_demodulation, [], [f1735, f2118])).
fof(f3145, plain, (~ spl42_42 | spl42_147), inference(avatar_contradiction_clause, [], [f3144])).
fof(f3144, plain, ($false | (~ spl42_42 | spl42_147)), inference(subsumption_resolution, [], [f3143, f815])).
fof(f3143, plain, (~ (e11 = op1(e11, e11)) | (~ spl42_42 | spl42_147)), inference(forward_demodulation, [], [f1365, f815])).
fof(f1365, plain, (~ (e11 = op1(e11, op1(e11, e11))) | spl42_147), inference(avatar_component_clause, [], [f1363])).
fof(f1363, plain, (spl42_147 <=> (e11 = op1(e11, op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_147])])).
fof(f3139, plain, (~ spl42_36 | ~ spl42_48), inference(avatar_split_clause, [], [f3138, f838, f787])).
fof(f3138, plain, (~ (e13 = op1(e11, e13)) | ~ spl42_48), inference(forward_demodulation, [], [f236, f840])).
fof(f3128, plain, (~ spl42_26 | ~ spl42_30), inference(avatar_split_clause, [], [f3126, f762, f745])).
fof(f745, plain, (spl42_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_26])])).
fof(f3126, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_30), inference(backward_demodulation, [], [f240, f764])).
fof(f240, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3123, plain, (~ spl42_39 | spl42_146), inference(avatar_contradiction_clause, [], [f3122])).
fof(f3122, plain, ($false | (~ spl42_39 | spl42_146)), inference(subsumption_resolution, [], [f3121, f802])).
fof(f3121, plain, (~ (e12 = op1(e11, e12)) | (~ spl42_39 | spl42_146)), inference(backward_demodulation, [], [f1360, f802])).
fof(f1360, plain, (~ (e12 = op1(e11, op1(e11, e12))) | spl42_146), inference(avatar_component_clause, [], [f1358])).
fof(f1358, plain, (spl42_146 <=> (e12 = op1(e11, op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_146])])).
fof(f3119, plain, (~ spl42_41 | ~ spl42_42), inference(avatar_contradiction_clause, [], [f3118])).
fof(f3118, plain, ($false | (~ spl42_41 | ~ spl42_42)), inference(subsumption_resolution, [], [f3117, f300])).
fof(f300, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f3117, plain, ((e10 = e11) | (~ spl42_41 | ~ spl42_42)), inference(forward_demodulation, [], [f815, f811])).
fof(f811, plain, ((e10 = op1(e11, e11)) | ~ spl42_41), inference(avatar_component_clause, [], [f809])).
fof(f809, plain, (spl42_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_41])])).
fof(f3094, plain, (~ spl42_196 | ~ spl42_78), inference(avatar_split_clause, [], [f3090, f998, f1773])).
fof(f998, plain, (spl42_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_78])])).
fof(f3090, plain, (~ (e21 = h4(e13)) | ~ spl42_78), inference(backward_demodulation, [], [f1743, f1000])).
fof(f1000, plain, ((e21 = op2(e23, e20)) | ~ spl42_78), inference(avatar_component_clause, [], [f998])).
fof(f1743, plain, ~ (op2(e23, e20) = h4(e13)), inference(backward_demodulation, [], [f296, f573])).
fof(f296, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3088, plain, (~ spl42_85 | ~ spl42_88), inference(avatar_contradiction_clause, [], [f3087])).
fof(f3087, plain, ($false | (~ spl42_85 | ~ spl42_88)), inference(subsumption_resolution, [], [f3086, f308])).
fof(f3086, plain, ((e20 = e23) | (~ spl42_85 | ~ spl42_88)), inference(backward_demodulation, [], [f1042, f1030])).
fof(f1030, plain, ((e20 = op2(e22, e22)) | ~ spl42_85), inference(avatar_component_clause, [], [f1028])).
fof(f1028, plain, (spl42_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_85])])).
fof(f3072, plain, (~ spl42_107 | spl42_210), inference(avatar_contradiction_clause, [], [f3071])).
fof(f3071, plain, ($false | (~ spl42_107 | spl42_210)), inference(subsumption_resolution, [], [f3070, f1854])).
fof(f1854, plain, (~ (e22 = h2(e13)) | spl42_210), inference(avatar_component_clause, [], [f1852])).
fof(f3070, plain, ((e22 = h2(e13)) | ~ spl42_107), inference(backward_demodulation, [], [f565, f1123])).
fof(f3069, plain, (~ spl42_109 | ~ spl42_110), inference(avatar_contradiction_clause, [], [f3068])).
fof(f3068, plain, ($false | (~ spl42_109 | ~ spl42_110)), inference(subsumption_resolution, [], [f3067, f306])).
fof(f306, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f3067, plain, ((e20 = e21) | (~ spl42_109 | ~ spl42_110)), inference(backward_demodulation, [], [f1136, f1132])).
fof(f3062, plain, (~ spl42_196 | ~ spl42_114), inference(avatar_split_clause, [], [f3060, f1151, f1773])).
fof(f1151, plain, (spl42_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_114])])).
fof(f3060, plain, (~ (e21 = h4(e13)) | ~ spl42_114), inference(backward_demodulation, [], [f1740, f1153])).
fof(f1153, plain, ((e21 = op2(e20, e23)) | ~ spl42_114), inference(avatar_component_clause, [], [f1151])).
fof(f1740, plain, ~ (op2(e20, e23) = h4(e13)), inference(backward_demodulation, [], [f272, f573])).
fof(f272, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3057, plain, (~ spl42_115 | ~ spl42_119), inference(avatar_split_clause, [], [f3055, f1172, f1155])).
fof(f3055, plain, (~ (e22 = op2(e20, e23)) | ~ spl42_119), inference(backward_demodulation, [], [f281, f1174])).
fof(f1174, plain, ((e22 = op2(e20, e22)) | ~ spl42_119), inference(avatar_component_clause, [], [f1172])).
fof(f281, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3056, plain, (~ spl42_222 | ~ spl42_119), inference(avatar_split_clause, [], [f3054, f1172, f1913])).
fof(f3054, plain, (~ (e22 = h1(e13)) | ~ spl42_119), inference(backward_demodulation, [], [f1716, f1174])).
fof(f1716, plain, ~ (op2(e20, e22) = h1(e13)), inference(backward_demodulation, [], [f277, f561])).
fof(f277, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3043, plain, (spl42_222 | ~ spl42_127), inference(avatar_split_clause, [], [f3042, f1206, f1913])).
fof(f1206, plain, (spl42_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl42_127])])).
fof(f3042, plain, ((e22 = h1(e13)) | ~ spl42_127), inference(backward_demodulation, [], [f561, f1208])).
fof(f1208, plain, ((op2(e20, e20) = e22) | ~ spl42_127), inference(avatar_component_clause, [], [f1206])).
fof(f3039, plain, (~ spl42_36 | spl42_145), inference(avatar_contradiction_clause, [], [f3038])).
fof(f3038, plain, ($false | (~ spl42_36 | spl42_145)), inference(subsumption_resolution, [], [f3037, f789])).
fof(f3037, plain, (~ (e13 = op1(e11, e13)) | (~ spl42_36 | spl42_145)), inference(forward_demodulation, [], [f1355, f789])).
fof(f3028, plain, (~ spl42_123 | ~ spl42_75), inference(avatar_split_clause, [], [f3027, f985, f1189])).
fof(f3027, plain, (~ (e22 = op2(e20, e21)) | ~ spl42_75), inference(forward_demodulation, [], [f260, f987])).
fof(f987, plain, ((e22 = op2(e23, e21)) | ~ spl42_75), inference(avatar_component_clause, [], [f985])).
fof(f3011, plain, (~ spl42_96 | ~ spl42_252), inference(avatar_contradiction_clause, [], [f3010])).
fof(f3010, plain, ($false | (~ spl42_96 | ~ spl42_252)), inference(subsumption_resolution, [], [f3009, f1076])).
fof(f1076, plain, ((e23 = op2(e22, e20)) | ~ spl42_96), inference(avatar_component_clause, [], [f1074])).
fof(f1074, plain, (spl42_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_96])])).
fof(f3009, plain, (~ (e23 = op2(e22, e20)) | ~ spl42_252), inference(forward_demodulation, [], [f1731, f2118])).
fof(f1731, plain, ~ (op2(e22, e20) = h3(e13)), inference(backward_demodulation, [], [f289, f569])).
fof(f289, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2982, plain, (~ spl42_72 | ~ spl42_252), inference(avatar_contradiction_clause, [], [f2981])).
fof(f2981, plain, ($false | (~ spl42_72 | ~ spl42_252)), inference(subsumption_resolution, [], [f2980, f974])).
fof(f974, plain, ((e23 = op2(e23, e22)) | ~ spl42_72), inference(avatar_component_clause, [], [f972])).
fof(f972, plain, (spl42_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_72])])).
fof(f2980, plain, (~ (e23 = op2(e23, e22)) | ~ spl42_252), inference(forward_demodulation, [], [f1730, f2118])).
fof(f1730, plain, ~ (op2(e23, e22) = h3(e13)), inference(backward_demodulation, [], [f269, f569])).
fof(f269, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2959, plain, (~ spl42_21 | ~ spl42_24), inference(avatar_contradiction_clause, [], [f2958])).
fof(f2958, plain, ($false | (~ spl42_21 | ~ spl42_24)), inference(subsumption_resolution, [], [f2957, f302])).
fof(f2957, plain, ((e10 = e13) | (~ spl42_21 | ~ spl42_24)), inference(forward_demodulation, [], [f738, f726])).
fof(f726, plain, ((e10 = op1(e12, e12)) | ~ spl42_21), inference(avatar_component_clause, [], [f724])).
fof(f724, plain, (spl42_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_21])])).
fof(f2956, plain, (~ spl42_18 | ~ spl42_2), inference(avatar_split_clause, [], [f2736, f643, f711])).
fof(f711, plain, (spl42_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_18])])).
fof(f2736, plain, (~ (e11 = op1(e12, e13)) | ~ spl42_2), inference(forward_demodulation, [], [f227, f645])).
fof(f227, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2952, plain, (~ spl42_60 | ~ spl42_12), inference(avatar_split_clause, [], [f2949, f685, f889])).
fof(f889, plain, (spl42_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_60])])).
fof(f2949, plain, (~ (e13 = op1(e10, e11)) | ~ spl42_12), inference(backward_demodulation, [], [f212, f687])).
fof(f2946, plain, (~ spl42_1 | ~ spl42_2), inference(avatar_contradiction_clause, [], [f2945])).
fof(f2945, plain, ($false | (~ spl42_1 | ~ spl42_2)), inference(subsumption_resolution, [], [f2944, f300])).
fof(f2944, plain, ((e10 = e11) | (~ spl42_1 | ~ spl42_2)), inference(backward_demodulation, [], [f645, f641])).
fof(f641, plain, ((e10 = op1(e13, e13)) | ~ spl42_1), inference(avatar_component_clause, [], [f639])).
fof(f639, plain, (spl42_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_1])])).
fof(f2931, plain, (~ spl42_23 | ~ spl42_24), inference(avatar_contradiction_clause, [], [f2930])).
fof(f2930, plain, ($false | (~ spl42_23 | ~ spl42_24)), inference(subsumption_resolution, [], [f2929, f305])).
fof(f305, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2929, plain, ((e12 = e13) | (~ spl42_23 | ~ spl42_24)), inference(backward_demodulation, [], [f738, f734])).
fof(f734, plain, ((e12 = op1(e12, e12)) | ~ spl42_23), inference(avatar_component_clause, [], [f732])).
fof(f732, plain, (spl42_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_23])])).
fof(f2925, plain, (~ spl42_41 | ~ spl42_43), inference(avatar_contradiction_clause, [], [f2924])).
fof(f2924, plain, ($false | (~ spl42_41 | ~ spl42_43)), inference(subsumption_resolution, [], [f2923, f301])).
fof(f2923, plain, ((e10 = e12) | (~ spl42_41 | ~ spl42_43)), inference(forward_demodulation, [], [f819, f811])).
fof(f819, plain, ((e12 = op1(e11, e11)) | ~ spl42_43), inference(avatar_component_clause, [], [f817])).
fof(f2922, plain, (~ spl42_38 | ~ spl42_46), inference(avatar_split_clause, [], [f2920, f830, f796])).
fof(f830, plain, (spl42_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_46])])).
fof(f2920, plain, (~ (e11 = op1(e11, e12)) | ~ spl42_46), inference(backward_demodulation, [], [f235, f832])).
fof(f832, plain, ((e11 = op1(e11, e10)) | ~ spl42_46), inference(avatar_component_clause, [], [f830])).
fof(f235, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2921, plain, (~ spl42_30 | ~ spl42_46), inference(avatar_split_clause, [], [f2919, f830, f762])).
fof(f2919, plain, (~ (e11 = op1(e12, e10)) | ~ spl42_46), inference(backward_demodulation, [], [f207, f832])).
fof(f207, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2910, plain, (~ spl42_26 | ~ spl42_58), inference(avatar_split_clause, [], [f2906, f881, f745])).
fof(f2906, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_58), inference(backward_demodulation, [], [f211, f883])).
fof(f2892, plain, (~ spl42_87 | ~ spl42_88), inference(avatar_contradiction_clause, [], [f2891])).
fof(f2891, plain, ($false | (~ spl42_87 | ~ spl42_88)), inference(subsumption_resolution, [], [f2890, f311])).
fof(f2890, plain, ((e22 = e23) | (~ spl42_87 | ~ spl42_88)), inference(backward_demodulation, [], [f1042, f1038])).
fof(f1038, plain, ((e22 = op2(e22, e22)) | ~ spl42_87), inference(avatar_component_clause, [], [f1036])).
fof(f1036, plain, (spl42_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_87])])).
fof(f2880, plain, (spl42_214 | ~ spl42_106), inference(avatar_split_clause, [], [f2879, f1117, f1872])).
fof(f1117, plain, (spl42_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_106])])).
fof(f2879, plain, ((e21 = h2(e13)) | ~ spl42_106), inference(backward_demodulation, [], [f565, f1119])).
fof(f1119, plain, ((e21 = op2(e21, e21)) | ~ spl42_106), inference(avatar_component_clause, [], [f1117])).
fof(f2866, plain, (~ spl42_61 | spl42_156), inference(avatar_contradiction_clause, [], [f2865])).
fof(f2865, plain, ($false | (~ spl42_61 | spl42_156)), inference(subsumption_resolution, [], [f2864, f896])).
fof(f2864, plain, (~ (e10 = op1(e10, e10)) | (~ spl42_61 | spl42_156)), inference(forward_demodulation, [], [f1426, f896])).
fof(f1426, plain, (~ (e10 = op1(e10, op1(e10, e10))) | spl42_156), inference(avatar_component_clause, [], [f1424])).
fof(f1424, plain, (spl42_156 <=> (e10 = op1(e10, op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_156])])).
fof(f2863, plain, (~ spl42_65 | ~ spl42_252), inference(avatar_contradiction_clause, [], [f2862])).
fof(f2862, plain, ($false | (~ spl42_65 | ~ spl42_252)), inference(subsumption_resolution, [], [f2861, f306])).
fof(f2861, plain, ((e20 = e21) | (~ spl42_65 | ~ spl42_252)), inference(forward_demodulation, [], [f2857, f945])).
fof(f945, plain, ((e20 = op2(e23, e23)) | ~ spl42_65), inference(avatar_component_clause, [], [f943])).
fof(f943, plain, (spl42_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_65])])).
fof(f2860, plain, (spl42_69 | ~ spl42_252), inference(avatar_split_clause, [], [f2856, f2117, f960])).
fof(f2856, plain, ((e20 = op2(e23, e22)) | ~ spl42_252), inference(backward_demodulation, [], [f1734, f2118])).
fof(f2859, plain, (~ spl42_92 | ~ spl42_252), inference(avatar_split_clause, [], [f2855, f2117, f1057])).
fof(f1057, plain, (spl42_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_92])])).
fof(f2855, plain, (~ (e23 = op2(e22, e21)) | ~ spl42_252), inference(backward_demodulation, [], [f1732, f2118])).
fof(f1732, plain, ~ (op2(e22, e21) = h3(e13)), inference(backward_demodulation, [], [f291, f569])).
fof(f291, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2858, plain, (~ spl42_120 | ~ spl42_252), inference(avatar_split_clause, [], [f2854, f2117, f1176])).
fof(f1176, plain, (spl42_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_120])])).
fof(f2854, plain, (~ (e23 = op2(e20, e22)) | ~ spl42_252), inference(backward_demodulation, [], [f1728, f2118])).
fof(f1728, plain, ~ (op2(e20, e22) = h3(e13)), inference(backward_demodulation, [], [f265, f569])).
fof(f265, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2836, plain, (~ spl42_111 | ~ spl42_79), inference(avatar_split_clause, [], [f2835, f1002, f1138])).
fof(f2835, plain, (~ (e22 = op2(e21, e20)) | ~ spl42_79), inference(forward_demodulation, [], [f256, f1004])).
fof(f1004, plain, ((e22 = op2(e23, e20)) | ~ spl42_79), inference(avatar_component_clause, [], [f1002])).
fof(f256, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2828, plain, (~ spl42_210 | ~ spl42_99), inference(avatar_split_clause, [], [f2827, f1087, f1852])).
fof(f2827, plain, (~ (e22 = h2(e13)) | ~ spl42_99), inference(forward_demodulation, [], [f1725, f1089])).
fof(f1089, plain, ((e22 = op2(e21, e23)) | ~ spl42_99), inference(avatar_component_clause, [], [f1087])).
fof(f1725, plain, ~ (op2(e21, e23) = h2(e13)), inference(backward_demodulation, [], [f286, f565])).
fof(f286, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2820, plain, (~ spl42_196 | ~ spl42_82), inference(avatar_split_clause, [], [f2819, f1015, f1773])).
fof(f1015, plain, (spl42_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_82])])).
fof(f2819, plain, (~ (e21 = h4(e13)) | ~ spl42_82), inference(forward_demodulation, [], [f1742, f1017])).
fof(f1017, plain, ((e21 = op2(e22, e23)) | ~ spl42_82), inference(avatar_component_clause, [], [f1015])).
fof(f1742, plain, ~ (op2(e22, e23) = h4(e13)), inference(backward_demodulation, [], [f275, f573])).
fof(f275, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2792, plain, (~ spl42_196 | ~ spl42_70), inference(avatar_split_clause, [], [f2791, f964, f1773])).
fof(f964, plain, (spl42_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_70])])).
fof(f2791, plain, (~ (e21 = h4(e13)) | ~ spl42_70), inference(forward_demodulation, [], [f1745, f966])).
fof(f966, plain, ((e21 = op2(e23, e22)) | ~ spl42_70), inference(avatar_component_clause, [], [f964])).
fof(f1745, plain, ~ (op2(e23, e22) = h4(e13)), inference(backward_demodulation, [], [f299, f573])).
fof(f299, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2785, plain, (~ spl42_57 | ~ spl42_41), inference(avatar_split_clause, [], [f2784, f809, f877])).
fof(f2784, plain, (~ (e10 = op1(e10, e11)) | ~ spl42_41), inference(forward_demodulation, [], [f210, f811])).
fof(f2779, plain, (~ spl42_56 | ~ spl42_24), inference(avatar_split_clause, [], [f2778, f736, f872])).
fof(f872, plain, (spl42_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_56])])).
fof(f2778, plain, (~ (e13 = op1(e10, e12)) | ~ spl42_24), inference(forward_demodulation, [], [f217, f738])).
fof(f217, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2777, plain, (~ spl42_53 | ~ spl42_5), inference(avatar_split_clause, [], [f2776, f656, f860])).
fof(f860, plain, (spl42_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_53])])).
fof(f2776, plain, (~ (e10 = op1(e10, e12)) | ~ spl42_5), inference(forward_demodulation, [], [f218, f658])).
fof(f218, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2769, plain, (~ spl42_50 | ~ spl42_2), inference(avatar_split_clause, [], [f2768, f643, f847])).
fof(f847, plain, (spl42_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_50])])).
fof(f2768, plain, (~ (e11 = op1(e10, e13)) | ~ spl42_2), inference(forward_demodulation, [], [f224, f645])).
fof(f224, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2763, plain, (~ spl42_45 | ~ spl42_41), inference(avatar_split_clause, [], [f2762, f809, f826])).
fof(f2762, plain, (~ (e10 = op1(e11, e10)) | ~ spl42_41), inference(forward_demodulation, [], [f234, f811])).
fof(f2761, plain, (~ spl42_46 | ~ spl42_34), inference(avatar_split_clause, [], [f2760, f779, f830])).
fof(f779, plain, (spl42_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_34])])).
fof(f2760, plain, (~ (e11 = op1(e11, e10)) | ~ spl42_34), inference(forward_demodulation, [], [f236, f781])).
fof(f781, plain, ((e11 = op1(e11, e13)) | ~ spl42_34), inference(avatar_component_clause, [], [f779])).
fof(f2759, plain, (~ spl42_40 | ~ spl42_24), inference(avatar_split_clause, [], [f2758, f736, f804])).
fof(f804, plain, (spl42_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_40])])).
fof(f2758, plain, (~ (e13 = op1(e11, e12)) | ~ spl42_24), inference(forward_demodulation, [], [f219, f738])).
fof(f219, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2757, plain, (~ spl42_37 | ~ spl42_5), inference(avatar_split_clause, [], [f2756, f656, f792])).
fof(f792, plain, (spl42_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_37])])).
fof(f2756, plain, (~ (e10 = op1(e11, e12)) | ~ spl42_5), inference(forward_demodulation, [], [f220, f658])).
fof(f220, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2755, plain, (~ spl42_38 | ~ spl42_34), inference(avatar_split_clause, [], [f2754, f779, f796])).
fof(f2754, plain, (~ (e11 = op1(e11, e12)) | ~ spl42_34), inference(forward_demodulation, [], [f239, f781])).
fof(f2750, plain, (~ spl42_31 | ~ spl42_15), inference(avatar_split_clause, [], [f2749, f698, f766])).
fof(f766, plain, (spl42_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_31])])).
fof(f2749, plain, (~ (e12 = op1(e12, e10)) | ~ spl42_15), inference(forward_demodulation, [], [f209, f700])).
fof(f209, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2748, plain, (~ spl42_32 | ~ spl42_24), inference(avatar_split_clause, [], [f2747, f736, f770])).
fof(f770, plain, (spl42_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_32])])).
fof(f2747, plain, (~ (e13 = op1(e12, e10)) | ~ spl42_24), inference(forward_demodulation, [], [f241, f738])).
fof(f241, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2744, plain, (~ spl42_26 | ~ spl42_10), inference(avatar_split_clause, [], [f2743, f677, f745])).
fof(f677, plain, (spl42_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_10])])).
fof(f2743, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_10), inference(forward_demodulation, [], [f215, f679])).
fof(f679, plain, ((e11 = op1(e13, e11)) | ~ spl42_10), inference(avatar_component_clause, [], [f677])).
fof(f2742, plain, (~ spl42_28 | ~ spl42_24), inference(avatar_split_clause, [], [f2741, f736, f753])).
fof(f753, plain, (spl42_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_28])])).
fof(f2741, plain, (~ (e13 = op1(e12, e11)) | ~ spl42_24), inference(forward_demodulation, [], [f243, f738])).
fof(f243, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2733, plain, (~ spl42_13 | ~ spl42_5), inference(avatar_split_clause, [], [f2732, f656, f690])).
fof(f690, plain, (spl42_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_13])])).
fof(f2732, plain, (~ (e10 = op1(e13, e10)) | ~ spl42_5), inference(forward_demodulation, [], [f247, f658])).
fof(f247, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2731, plain, (~ spl42_14 | ~ spl42_2), inference(avatar_split_clause, [], [f2730, f643, f694])).
fof(f694, plain, (spl42_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_14])])).
fof(f2730, plain, (~ (e11 = op1(e13, e10)) | ~ spl42_2), inference(forward_demodulation, [], [f248, f645])).
fof(f248, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2726, plain, (~ spl42_10 | ~ spl42_12), inference(avatar_contradiction_clause, [], [f2725])).
fof(f2725, plain, ($false | (~ spl42_10 | ~ spl42_12)), inference(subsumption_resolution, [], [f2724, f304])).
fof(f304, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f2724, plain, ((e11 = e13) | (~ spl42_10 | ~ spl42_12)), inference(forward_demodulation, [], [f687, f679])).
fof(f2719, plain, (~ spl42_5 | spl42_254), inference(avatar_contradiction_clause, [], [f2718])).
fof(f2718, plain, ($false | (~ spl42_5 | spl42_254)), inference(subsumption_resolution, [], [f2717, f1739])).
fof(f2717, plain, (~ (e20 = h3(e10)) | (~ spl42_5 | spl42_254)), inference(forward_demodulation, [], [f2127, f658])).
fof(f2127, plain, (~ (e20 = h3(op1(e13, e12))) | spl42_254), inference(avatar_component_clause, [], [f2125])).
fof(f2125, plain, (spl42_254 <=> (e20 = h3(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_254])])).
fof(f2714, plain, (~ spl42_2 | ~ spl42_3), inference(avatar_contradiction_clause, [], [f2713])).
fof(f2713, plain, ($false | (~ spl42_2 | ~ spl42_3)), inference(subsumption_resolution, [], [f2712, f303])).
fof(f303, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f2712, plain, ((e11 = e12) | (~ spl42_2 | ~ spl42_3)), inference(backward_demodulation, [], [f649, f645])).
fof(f649, plain, ((e12 = op1(e13, e13)) | ~ spl42_3), inference(avatar_component_clause, [], [f647])).
fof(f647, plain, (spl42_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_3])])).
fof(f2708, plain, (~ spl42_4 | spl42_130), inference(avatar_contradiction_clause, [], [f2707])).
fof(f2707, plain, ($false | (~ spl42_4 | spl42_130)), inference(subsumption_resolution, [], [f2706, f653])).
fof(f653, plain, ((e13 = op1(e13, e13)) | ~ spl42_4), inference(avatar_component_clause, [], [f651])).
fof(f651, plain, (spl42_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_4])])).
fof(f2706, plain, (~ (e13 = op1(e13, e13)) | (~ spl42_4 | spl42_130)), inference(backward_demodulation, [], [f1253, f653])).
fof(f2705, plain, (~ spl42_5 | ~ spl42_6), inference(avatar_contradiction_clause, [], [f2704])).
fof(f2704, plain, ($false | (~ spl42_5 | ~ spl42_6)), inference(subsumption_resolution, [], [f2703, f300])).
fof(f2703, plain, ((e10 = e11) | (~ spl42_5 | ~ spl42_6)), inference(backward_demodulation, [], [f662, f658])).
fof(f662, plain, ((e11 = op1(e13, e12)) | ~ spl42_6), inference(avatar_component_clause, [], [f660])).
fof(f660, plain, (spl42_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_6])])).
fof(f2693, plain, (~ spl42_10 | ~ spl42_11), inference(avatar_contradiction_clause, [], [f2692])).
fof(f2692, plain, ($false | (~ spl42_10 | ~ spl42_11)), inference(subsumption_resolution, [], [f2691, f303])).
fof(f2691, plain, ((e11 = e12) | (~ spl42_10 | ~ spl42_11)), inference(backward_demodulation, [], [f683, f679])).
fof(f683, plain, ((e12 = op1(e13, e11)) | ~ spl42_11), inference(avatar_component_clause, [], [f681])).
fof(f2690, plain, (~ spl42_11 | ~ spl42_12), inference(avatar_contradiction_clause, [], [f2689])).
fof(f2689, plain, ($false | (~ spl42_11 | ~ spl42_12)), inference(subsumption_resolution, [], [f2688, f305])).
fof(f2688, plain, ((e12 = e13) | (~ spl42_11 | ~ spl42_12)), inference(backward_demodulation, [], [f687, f683])).
fof(f2687, plain, (~ spl42_2 | ~ spl42_12 | spl42_132), inference(avatar_split_clause, [], [f2684, f1261, f685, f643])).
fof(f1261, plain, (spl42_132 <=> (e11 = op1(e13, op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_132])])).
fof(f2684, plain, (~ (e11 = op1(e13, e13)) | (~ spl42_12 | spl42_132)), inference(backward_demodulation, [], [f1263, f687])).
fof(f1263, plain, (~ (e11 = op1(e13, op1(e13, e11))) | spl42_132), inference(avatar_component_clause, [], [f1261])).
fof(f2686, plain, (~ spl42_4 | ~ spl42_12), inference(avatar_split_clause, [], [f2683, f685, f651])).
fof(f2683, plain, (~ (e13 = op1(e13, e13)) | ~ spl42_12), inference(backward_demodulation, [], [f250, f687])).
fof(f250, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2678, plain, (~ spl42_15 | ~ spl42_16), inference(avatar_contradiction_clause, [], [f2677])).
fof(f2677, plain, ($false | (~ spl42_15 | ~ spl42_16)), inference(subsumption_resolution, [], [f2676, f305])).
fof(f2676, plain, ((e12 = e13) | (~ spl42_15 | ~ spl42_16)), inference(backward_demodulation, [], [f704, f700])).
fof(f2659, plain, (spl42_2 | ~ spl42_24), inference(avatar_split_clause, [], [f2655, f736, f643])).
fof(f2655, plain, ((e11 = op1(e13, e13)) | ~ spl42_24), inference(backward_demodulation, [], [f553, f738])).
fof(f553, plain, (e11 = op1(op1(e12, e12), op1(e12, e12))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(e12, e12)) & (e11 = op1(op1(e12, e12), op1(e12, e12))) & (e10 = op1(op1(e12, e12), e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax12)).
fof(f2658, plain, (spl42_5 | ~ spl42_24), inference(avatar_split_clause, [], [f2654, f736, f656])).
fof(f2654, plain, ((e10 = op1(e13, e12)) | ~ spl42_24), inference(backward_demodulation, [], [f552, f738])).
fof(f552, plain, (e10 = op1(op1(e12, e12), e12)), inference(cnf_transformation, [], [f12])).
fof(f2657, plain, (~ spl42_20 | ~ spl42_24), inference(avatar_split_clause, [], [f2653, f736, f719])).
fof(f719, plain, (spl42_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_20])])).
fof(f2653, plain, (~ (e13 = op1(e12, e13)) | ~ spl42_24), inference(backward_demodulation, [], [f245, f738])).
fof(f245, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2656, plain, (~ spl42_8 | ~ spl42_24), inference(avatar_split_clause, [], [f2652, f736, f668])).
fof(f668, plain, (spl42_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_8])])).
fof(f2652, plain, (~ (e13 = op1(e13, e12)) | ~ spl42_24), inference(backward_demodulation, [], [f221, f738])).
fof(f221, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2645, plain, (~ spl42_17 | ~ spl42_29), inference(avatar_split_clause, [], [f2641, f758, f707])).
fof(f758, plain, (spl42_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_29])])).
fof(f2641, plain, (~ (e10 = op1(e12, e13)) | ~ spl42_29), inference(backward_demodulation, [], [f242, f760])).
fof(f760, plain, ((e10 = op1(e12, e10)) | ~ spl42_29), inference(avatar_component_clause, [], [f758])).
fof(f242, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2634, plain, (~ spl42_35 | ~ spl42_36), inference(avatar_contradiction_clause, [], [f2633])).
fof(f2633, plain, ($false | (~ spl42_35 | ~ spl42_36)), inference(subsumption_resolution, [], [f2632, f305])).
fof(f2632, plain, ((e12 = e13) | (~ spl42_35 | ~ spl42_36)), inference(backward_demodulation, [], [f789, f785])).
fof(f785, plain, ((e12 = op1(e11, e13)) | ~ spl42_35), inference(avatar_component_clause, [], [f783])).
fof(f2621, plain, (~ spl42_33 | ~ spl42_41), inference(avatar_split_clause, [], [f2617, f809, f775])).
fof(f2617, plain, (~ (e10 = op1(e11, e13)) | ~ spl42_41), inference(backward_demodulation, [], [f238, f811])).
fof(f238, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2618, plain, (~ spl42_25 | ~ spl42_41), inference(avatar_split_clause, [], [f2614, f809, f741])).
fof(f2614, plain, (~ (e10 = op1(e12, e11)) | ~ spl42_41), inference(backward_demodulation, [], [f213, f811])).
fof(f213, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2609, plain, (~ spl42_29 | ~ spl42_45), inference(avatar_split_clause, [], [f2604, f826, f758])).
fof(f2604, plain, (~ (e10 = op1(e12, e10)) | ~ spl42_45), inference(backward_demodulation, [], [f207, f828])).
fof(f2600, plain, (~ spl42_51 | ~ spl42_52), inference(avatar_contradiction_clause, [], [f2599])).
fof(f2599, plain, ($false | (~ spl42_51 | ~ spl42_52)), inference(subsumption_resolution, [], [f2598, f305])).
fof(f2598, plain, ((e12 = e13) | (~ spl42_51 | ~ spl42_52)), inference(backward_demodulation, [], [f857, f853])).
fof(f2571, plain, (~ spl42_49 | ~ spl42_61), inference(avatar_split_clause, [], [f2564, f894, f843])).
fof(f843, plain, (spl42_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_49])])).
fof(f2564, plain, (~ (e10 = op1(e10, e13)) | ~ spl42_61), inference(backward_demodulation, [], [f230, f896])).
fof(f2567, plain, (~ spl42_29 | ~ spl42_61), inference(avatar_split_clause, [], [f2560, f894, f758])).
fof(f2560, plain, (~ (e10 = op1(e12, e10)) | ~ spl42_61), inference(backward_demodulation, [], [f205, f896])).
fof(f2555, plain, (~ spl42_66 | ~ spl42_67), inference(avatar_contradiction_clause, [], [f2554])).
fof(f2554, plain, ($false | (~ spl42_66 | ~ spl42_67)), inference(subsumption_resolution, [], [f2553, f309])).
fof(f309, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f2553, plain, ((e21 = e22) | (~ spl42_66 | ~ spl42_67)), inference(backward_demodulation, [], [f953, f949])).
fof(f953, plain, ((e22 = op2(e23, e23)) | ~ spl42_67), inference(avatar_component_clause, [], [f951])).
fof(f951, plain, (spl42_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_67])])).
fof(f2520, plain, (~ spl42_79 | ~ spl42_80), inference(avatar_contradiction_clause, [], [f2519])).
fof(f2519, plain, ($false | (~ spl42_79 | ~ spl42_80)), inference(subsumption_resolution, [], [f2518, f311])).
fof(f2518, plain, ((e22 = e23) | (~ spl42_79 | ~ spl42_80)), inference(backward_demodulation, [], [f1008, f1004])).
fof(f2500, plain, (spl42_252 | ~ spl42_88), inference(avatar_split_clause, [], [f2499, f1040, f2117])).
fof(f2499, plain, ((e23 = h3(e13)) | ~ spl42_88), inference(backward_demodulation, [], [f569, f1042])).
fof(f2489, plain, (~ spl42_89 | ~ spl42_93), inference(avatar_split_clause, [], [f2484, f1062, f1045])).
fof(f2484, plain, (~ (e20 = op2(e22, e21)) | ~ spl42_93), inference(backward_demodulation, [], [f288, f1064])).
fof(f2467, plain, (~ spl42_69 | ~ spl42_101), inference(avatar_split_clause, [], [f2463, f1096, f960])).
fof(f1096, plain, (spl42_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_101])])).
fof(f2463, plain, (~ (e20 = op2(e23, e22)) | ~ spl42_101), inference(backward_demodulation, [], [f268, f1098])).
fof(f1098, plain, ((e20 = op2(e21, e22)) | ~ spl42_101), inference(avatar_component_clause, [], [f1096])).
fof(f268, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2460, plain, (spl42_218 | ~ spl42_105), inference(avatar_split_clause, [], [f2459, f1113, f1893])).
fof(f2459, plain, ((e20 = h2(e13)) | ~ spl42_105), inference(backward_demodulation, [], [f565, f1115])).
fof(f2458, plain, (~ spl42_97 | ~ spl42_109), inference(avatar_split_clause, [], [f2452, f1130, f1079])).
fof(f2452, plain, (~ (e20 = op2(e21, e23)) | ~ spl42_109), inference(backward_demodulation, [], [f284, f1132])).
fof(f2454, plain, (~ spl42_230 | ~ spl42_109), inference(avatar_split_clause, [], [f2448, f1130, f1953])).
fof(f2448, plain, (~ (e20 = h1(e13)) | ~ spl42_109), inference(backward_demodulation, [], [f1712, f1132])).
fof(f1712, plain, ~ (op2(e21, e20) = h1(e13)), inference(backward_demodulation, [], [f252, f561])).
fof(f252, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f2443, plain, (~ spl42_115 | ~ spl42_116), inference(avatar_contradiction_clause, [], [f2442])).
fof(f2442, plain, ($false | (~ spl42_115 | ~ spl42_116)), inference(subsumption_resolution, [], [f2441, f311])).
fof(f2441, plain, ((e22 = e23) | (~ spl42_115 | ~ spl42_116)), inference(backward_demodulation, [], [f1161, f1157])).
fof(f2431, plain, (~ spl42_69 | ~ spl42_117), inference(avatar_split_clause, [], [f2426, f1164, f960])).
fof(f1164, plain, (spl42_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_117])])).
fof(f2426, plain, (~ (e20 = op2(e23, e22)) | ~ spl42_117), inference(backward_demodulation, [], [f266, f1166])).
fof(f1166, plain, ((e20 = op2(e20, e22)) | ~ spl42_117), inference(avatar_component_clause, [], [f1164])).
fof(f266, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2419, plain, (~ spl42_89 | ~ spl42_121), inference(avatar_split_clause, [], [f2413, f1181, f1045])).
fof(f2413, plain, (~ (e20 = op2(e22, e21)) | ~ spl42_121), inference(backward_demodulation, [], [f259, f1183])).
fof(f2410, plain, (spl42_230 | ~ spl42_125), inference(avatar_split_clause, [], [f2409, f1198, f1953])).
fof(f2409, plain, ((e20 = h1(e13)) | ~ spl42_125), inference(backward_demodulation, [], [f561, f1200])).
fof(f2184, plain, (spl42_207 | spl42_205 | spl42_203 | ~ spl42_252 | ~ spl42_253 | ~ spl42_254 | ~ spl42_255 | ~ spl42_256 | ~ spl42_257 | ~ spl42_258 | ~ spl42_259 | ~ spl42_260 | ~ spl42_261 | ~ spl42_262 | ~ spl42_263 | ~ spl42_264 | ~ spl42_265 | ~ spl42_266 | ~ spl42_267 | ~ spl42_268), inference(avatar_split_clause, [], [f2115, f2181, f2177, f2173, f2169, f2165, f2161, f2157, f2153, f2149, f2145, f2141, f2137, f2133, f2129, f2125, f2121, f2117, f1809, f1822, f1835])).
fof(f1835, plain, (spl42_207 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl42_207])])).
fof(f1822, plain, (spl42_205 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl42_205])])).
fof(f1809, plain, (spl42_203 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl42_203])])).
fof(f2115, plain, (~ (h1(e13) = h3(op1(e10, e10))) | ~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36), inference(forward_demodulation, [], [f2114, f561])).
fof(f2114, plain, (~ (op2(e20, e20) = h3(op1(e10, e10))) | ~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36), inference(forward_demodulation, [], [f2113, f1739])).
fof(f2113, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2112, f1739])).
fof(f2112, plain, (~ (h3(op1(e10, e11)) = op2(h3(e10), e21)) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2111, f1737])).
fof(f2111, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2110, f1739])).
fof(f2110, plain, (~ (h3(op1(e10, e12)) = op2(h3(e10), e22)) | ~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2109, f566])).
fof(f2109, plain, (~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2108, f1739])).
fof(f2108, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2107, f1737])).
fof(f2107, plain, (~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2106, f1739])).
fof(f2106, plain, (~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2105, f565])).
fof(f2105, plain, (~ (op2(e21, e21) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2104, f1737])).
fof(f2104, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2103, f1737])).
fof(f2103, plain, (~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2102, f566])).
fof(f2102, plain, (~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2101, f1737])).
fof(f2101, plain, (~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2100, f566])).
fof(f2100, plain, (~ (h3(op1(e12, e10)) = op2(h3(e12), e20)) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2099, f1739])).
fof(f2099, plain, (~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2098, f566])).
fof(f2098, plain, (~ (h3(op1(e12, e11)) = op2(h3(e12), e21)) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2097, f1737])).
fof(f2097, plain, (~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2096, f569])).
fof(f2096, plain, (~ (op2(e22, e22) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2095, f566])).
fof(f2095, plain, (~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2094, f566])).
fof(f2094, plain, (~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2093, f1739])).
fof(f2093, plain, (~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2092, f1737])).
fof(f2092, plain, (~ (e20 = h3(op1(e13, e12))) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2091, f1734])).
fof(f2091, plain, (~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f2090, f566])).
fof(f2090, plain, (~ (e21 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f633, f1735])).
fof(f633, plain, (~ (e23 = h3(e13)) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(cnf_transformation, [], [f65])).
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
fof(f64, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP41), inference(usedef, [], [e64])).
fof(e64, plain, (sP41 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', co1)).
fof(f1846, plain, ~ spl42_207, inference(avatar_split_clause, [], [f1845, f1835])).
fof(f1845, plain, ~ sP36, inference(subsumption_resolution, [], [f594, f1739])).
fof(f594, plain, (~ (e20 = h3(e10)) | ~ sP36), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP36), inference(nnf_transformation, [], [f59])).
fof(f1832, plain, ~ spl42_205, inference(avatar_split_clause, [], [f1831, f1822])).
fof(f1831, plain, ~ sP37, inference(subsumption_resolution, [], [f591, f1737])).
fof(f591, plain, (~ (e21 = h3(e11)) | ~ sP37), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP37), inference(nnf_transformation, [], [f60])).
fof(f1818, plain, ~ spl42_203, inference(avatar_split_clause, [], [f1817, f1809])).
fof(f1817, plain, ~ sP38, inference(subsumption_resolution, [], [f588, f566])).
fof(f588, plain, (~ (e22 = h3(e12)) | ~ sP38), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP38), inference(nnf_transformation, [], [f61])).
fof(f1711, plain, spl42_88, inference(avatar_split_clause, [], [f557, f1040])).
fof(f557, plain, (e23 = op2(e22, e22)), inference(cnf_transformation, [], [f13])).
fof(f1710, plain, spl42_24, inference(avatar_split_clause, [], [f554, f736])).
fof(f554, plain, (e13 = op1(e12, e12)), inference(cnf_transformation, [], [f12])).
fof(f1705, plain, (~ spl42_102 | spl42_127 | spl42_107 | spl42_87 | spl42_67), inference(avatar_split_clause, [], [f536, f951, f1036, f1121, f1206, f1100])).
fof(f536, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e21 = op2(e21, e22))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e23 = op2(e23, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e23 = op2(e23, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e23 = op2(e23, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e23 = op2(e23, e20))) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e22 = op2(e22, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e22 = op2(e22, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e22 = op2(e22, e20))) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e21 = op2(e21, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e21 = op2(e21, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e21 = op2(e21, e20))) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e20 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e20 = op2(e20, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e20 = op2(e20, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20)))), inference(definition_folding, [], [f11, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37])).
fof(f37, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(usedef, [], [e37])).
fof(e37, plain, (sP15 <=> (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f38, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP16), inference(usedef, [], [e38])).
fof(e38, plain, (sP16 <=> (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f39, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP17), inference(usedef, [], [e39])).
fof(e39, plain, (sP17 <=> (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f40, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP18), inference(usedef, [], [e40])).
fof(e40, plain, (sP18 <=> (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f41, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP19), inference(usedef, [], [e41])).
fof(e41, plain, (sP19 <=> (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f42, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(usedef, [], [e42])).
fof(e42, plain, (sP20 <=> (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f43, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP21), inference(usedef, [], [e43])).
fof(e43, plain, (sP21 <=> (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f44, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP22), inference(usedef, [], [e44])).
fof(e44, plain, (sP22 <=> (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f45, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP23), inference(usedef, [], [e45])).
fof(e45, plain, (sP23 <=> (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f46, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP24), inference(usedef, [], [e46])).
fof(e46, plain, (sP24 <=> (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f47, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(usedef, [], [e47])).
fof(e47, plain, (sP25 <=> (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f48, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP26), inference(usedef, [], [e48])).
fof(e48, plain, (sP26 <=> (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f49, plain, ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP27), inference(usedef, [], [e49])).
fof(e49, plain, (sP27 <=> (~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f50, plain, ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP28), inference(usedef, [], [e50])).
fof(e50, plain, (sP28 <=> (~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f51, plain, ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP29), inference(usedef, [], [e51])).
fof(e51, plain, (sP29 <=> (~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f11, plain, (((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | (~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e23 = op2(e23, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e23 = op2(e23, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e23 = op2(e23, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e23 = op2(e23, e20))) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e22 = op2(e22, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e22 = op2(e22, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e22 = op2(e22, e20))) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e21 = op2(e21, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e21 = op2(e21, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e21 = op2(e21, e20))) & ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | (op2(e20, e20) = e23) | ~ (e20 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | (op2(e20, e20) = e22) | ~ (e20 = op2(e20, e22))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (op2(e20, e20) = e21) | ~ (e20 = op2(e20, e21))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax11)).
fof(f1703, plain, (~ spl42_95 | spl42_125 | spl42_105 | spl42_85 | spl42_65), inference(avatar_split_clause, [], [f538, f943, f1028, f1113, f1198, f1070])).
fof(f538, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f52])).
fof(f1700, plain, (~ spl42_80 | spl42_125 | spl42_105 | spl42_85 | spl42_65), inference(avatar_split_clause, [], [f542, f943, f1028, f1113, f1198, f1006])).
fof(f542, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e20)) | ~ (e23 = op2(e23, e20))), inference(cnf_transformation, [], [f52])).
fof(f1697, plain, (spl42_190 | spl42_189 | spl42_188 | spl42_183 | spl42_182 | spl42_181 | spl42_180 | spl42_175 | spl42_174 | spl42_173 | spl42_172 | spl42_167 | spl42_166 | spl42_165 | spl42_160 | spl42_68), inference(avatar_split_clause, [], [f546, f955, f1479, f1505, f1515, f1525, f1551, f1561, f1571, f1581, f1607, f1617, f1627, f1637, f1663, f1673, f1683])).
fof(f1683, plain, (spl42_190 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl42_190])])).
fof(f1673, plain, (spl42_189 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl42_189])])).
fof(f1663, plain, (spl42_188 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl42_188])])).
fof(f1637, plain, (spl42_183 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl42_183])])).
fof(f1627, plain, (spl42_182 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl42_182])])).
fof(f1617, plain, (spl42_181 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl42_181])])).
fof(f1607, plain, (spl42_180 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl42_180])])).
fof(f1581, plain, (spl42_175 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl42_175])])).
fof(f1571, plain, (spl42_174 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl42_174])])).
fof(f1561, plain, (spl42_173 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl42_173])])).
fof(f1551, plain, (spl42_172 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl42_172])])).
fof(f1525, plain, (spl42_167 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl42_167])])).
fof(f1515, plain, (spl42_166 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl42_166])])).
fof(f1505, plain, (spl42_165 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl42_165])])).
fof(f1479, plain, (spl42_160 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl42_160])])).
fof(f546, plain, ((e23 = op2(e23, e23)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f52])).
fof(f1691, plain, (~ spl42_190 | spl42_125), inference(avatar_split_clause, [], [f524, f1198, f1683])).
fof(f524, plain, ((e20 = op2(e20, e20)) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(nnf_transformation, [], [f37])).
fof(f1689, plain, (~ spl42_190 | ~ spl42_187), inference(avatar_split_clause, [], [f526, f1656, f1683])).
fof(f526, plain, (~ (e20 = op2(e20, op2(e20, e20))) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f1681, plain, (~ spl42_189 | spl42_121), inference(avatar_split_clause, [], [f518, f1181, f1673])).
fof(f518, plain, ((e20 = op2(e20, e21)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP16), inference(nnf_transformation, [], [f38])).
fof(f1680, plain, (~ spl42_189 | spl42_109), inference(avatar_split_clause, [], [f519, f1130, f1673])).
fof(f519, plain, ((e20 = op2(e21, e20)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f1678, plain, (~ spl42_189 | ~ spl42_186), inference(avatar_split_clause, [], [f521, f1651, f1673])).
fof(f521, plain, (~ (e21 = op2(e20, op2(e20, e21))) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f1676, plain, (~ spl42_189 | ~ spl42_184), inference(avatar_split_clause, [], [f523, f1641, f1673])).
fof(f523, plain, (~ (e23 = op2(e20, op2(e20, e23))) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f1671, plain, (~ spl42_188 | spl42_117), inference(avatar_split_clause, [], [f512, f1164, f1663])).
fof(f512, plain, ((e20 = op2(e20, e22)) | ~ sP17), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP17), inference(nnf_transformation, [], [f39])).
fof(f1660, plain, (~ spl42_183 | spl42_77), inference(avatar_split_clause, [], [f507, f994, f1637])).
fof(f507, plain, ((e20 = op2(e23, e20)) | ~ sP18), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((~ (e23 = op2(e20, op2(e20, e23))) & ~ (e22 = op2(e20, op2(e20, e22))) & ~ (e21 = op2(e20, op2(e20, e21))) & ~ (e20 = op2(e20, op2(e20, e20))) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP18), inference(nnf_transformation, [], [f40])).
fof(f1635, plain, (~ spl42_182 | spl42_110), inference(avatar_split_clause, [], [f500, f1134, f1627])).
fof(f500, plain, ((e21 = op2(e21, e20)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP19), inference(nnf_transformation, [], [f41])).
fof(f1631, plain, (~ spl42_182 | ~ spl42_177), inference(avatar_split_clause, [], [f504, f1590, f1627])).
fof(f504, plain, (~ (e22 = op2(e21, op2(e21, e22))) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f1625, plain, (~ spl42_181 | spl42_106), inference(avatar_split_clause, [], [f494, f1117, f1617])).
fof(f494, plain, ((e21 = op2(e21, e21)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(nnf_transformation, [], [f42])).
fof(f1623, plain, (~ spl42_181 | ~ spl42_179), inference(avatar_split_clause, [], [f496, f1600, f1617])).
fof(f496, plain, (~ (e20 = op2(e21, op2(e21, e20))) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1621, plain, (~ spl42_181 | ~ spl42_177), inference(avatar_split_clause, [], [f498, f1590, f1617])).
fof(f498, plain, (~ (e22 = op2(e21, op2(e21, e22))) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1620, plain, (~ spl42_181 | ~ spl42_176), inference(avatar_split_clause, [], [f499, f1585, f1617])).
fof(f499, plain, (~ (e23 = op2(e21, op2(e21, e23))) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1615, plain, (~ spl42_180 | spl42_102), inference(avatar_split_clause, [], [f488, f1100, f1607])).
fof(f488, plain, ((e21 = op2(e21, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP21), inference(nnf_transformation, [], [f43])).
fof(f1614, plain, (~ spl42_180 | spl42_90), inference(avatar_split_clause, [], [f489, f1049, f1607])).
fof(f489, plain, ((e21 = op2(e22, e21)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1611, plain, (~ spl42_180 | ~ spl42_177), inference(avatar_split_clause, [], [f492, f1590, f1607])).
fof(f492, plain, (~ (e22 = op2(e21, op2(e21, e22))) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1605, plain, (~ spl42_175 | spl42_98), inference(avatar_split_clause, [], [f482, f1083, f1581])).
fof(f482, plain, ((e21 = op2(e21, e23)) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ((~ (e23 = op2(e21, op2(e21, e23))) & ~ (e22 = op2(e21, op2(e21, e22))) & ~ (e21 = op2(e21, op2(e21, e21))) & ~ (e20 = op2(e21, op2(e21, e20))) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP22), inference(nnf_transformation, [], [f44])).
fof(f1579, plain, (~ spl42_174 | spl42_95), inference(avatar_split_clause, [], [f476, f1070, f1571])).
fof(f476, plain, ((e22 = op2(e22, e20)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP23), inference(nnf_transformation, [], [f45])).
fof(f1578, plain, (~ spl42_174 | spl42_119), inference(avatar_split_clause, [], [f477, f1172, f1571])).
fof(f477, plain, ((e22 = op2(e20, e22)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1569, plain, (~ spl42_173 | spl42_91), inference(avatar_split_clause, [], [f470, f1053, f1561])).
fof(f470, plain, ((e22 = op2(e22, e21)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f86, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP24), inference(nnf_transformation, [], [f46])).
fof(f1568, plain, (~ spl42_173 | spl42_103), inference(avatar_split_clause, [], [f471, f1104, f1561])).
fof(f471, plain, ((e22 = op2(e21, e22)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f1567, plain, (~ spl42_173 | ~ spl42_171), inference(avatar_split_clause, [], [f472, f1544, f1561])).
fof(f472, plain, (~ (e20 = op2(e22, op2(e22, e20))) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f1559, plain, (~ spl42_172 | spl42_87), inference(avatar_split_clause, [], [f464, f1036, f1551])).
fof(f464, plain, ((e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(nnf_transformation, [], [f47])).
fof(f1548, plain, (~ spl42_167 | spl42_71), inference(avatar_split_clause, [], [f459, f968, f1525])).
fof(f459, plain, ((e22 = op2(e23, e22)) | ~ sP26), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e23 = op2(e22, op2(e22, e23))) & ~ (e22 = op2(e22, op2(e22, e22))) & ~ (e21 = op2(e22, op2(e22, e21))) & ~ (e20 = op2(e22, op2(e22, e20))) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP26), inference(nnf_transformation, [], [f48])).
fof(f1523, plain, (~ spl42_166 | spl42_80), inference(avatar_split_clause, [], [f452, f1006, f1515])).
fof(f452, plain, ((e23 = op2(e23, e20)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP27), inference(nnf_transformation, [], [f49])).
fof(f1522, plain, (~ spl42_166 | spl42_116), inference(avatar_split_clause, [], [f453, f1159, f1515])).
fof(f453, plain, ((e23 = op2(e20, e23)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f1513, plain, (~ spl42_165 | spl42_76), inference(avatar_split_clause, [], [f446, f989, f1505])).
fof(f446, plain, ((e23 = op2(e23, e21)) | ~ sP28), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP28), inference(nnf_transformation, [], [f50])).
fof(f1508, plain, (~ spl42_165 | ~ spl42_161), inference(avatar_split_clause, [], [f451, f1483, f1505])).
fof(f451, plain, (~ (e23 = op2(e23, op2(e23, e23))) | ~ sP28), inference(cnf_transformation, [], [f82])).
fof(f1503, plain, (~ spl42_160 | spl42_72), inference(avatar_split_clause, [], [f440, f972, f1479])).
fof(f440, plain, ((e23 = op2(e23, e22)) | ~ sP29), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e23 = op2(e23, op2(e23, e23))) & ~ (e22 = op2(e23, op2(e23, e22))) & ~ (e21 = op2(e23, op2(e23, e21))) & ~ (e20 = op2(e23, op2(e23, e20))) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP29), inference(nnf_transformation, [], [f51])).
fof(f1474, plain, (~ spl42_46 | spl42_61 | spl42_41 | spl42_21 | spl42_1), inference(avatar_split_clause, [], [f422, f639, f724, f809, f894, f830])).
fof(f422, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e13 = op1(e13, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e13 = op1(e13, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e13 = op1(e13, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e13 = op1(e13, e10))) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e12 = op1(e12, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e12 = op1(e12, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e12 = op1(e12, e10))) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e11 = op1(e11, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e11 = op1(e11, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e11 = op1(e11, e10))) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e10 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e10 = op1(e10, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e10 = op1(e10, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10)))), inference(definition_folding, [], [f10, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> (~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> (~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> (~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f10, plain, (((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | (~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e13 = op1(e13, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e13 = op1(e13, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e13 = op1(e13, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e13 = op1(e13, e10))) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e12 = op1(e12, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e12 = op1(e12, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e12 = op1(e12, e10))) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e11 = op1(e11, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e11 = op1(e11, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e11 = op1(e11, e10))) & ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | (op1(e10, e10) = e13) | ~ (e10 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e10 = op1(e10, e12))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (op1(e10, e10) = e11) | ~ (e10 = op1(e10, e11))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax10)).
fof(f1473, plain, (~ spl42_38 | spl42_63 | spl42_43 | spl42_23 | spl42_3), inference(avatar_split_clause, [], [f424, f647, f732, f817, f902, f796])).
fof(f424, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | (op1(e10, e10) = e12) | ~ (e11 = op1(e11, e12))), inference(cnf_transformation, [], [f36])).
fof(f1471, plain, (~ spl42_31 | spl42_61 | spl42_41 | spl42_21 | spl42_1), inference(avatar_split_clause, [], [f426, f639, f724, f809, f894, f766])).
fof(f426, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f36])).
fof(f1468, plain, (~ spl42_16 | spl42_61 | spl42_41 | spl42_21 | spl42_1), inference(avatar_split_clause, [], [f430, f639, f724, f809, f894, f702])).
fof(f430, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e10)) | ~ (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f36])).
fof(f1465, plain, (spl42_159 | spl42_158 | spl42_157 | spl42_152 | spl42_151 | spl42_150 | spl42_149 | spl42_144 | spl42_143 | spl42_142 | spl42_141 | spl42_136 | spl42_135 | spl42_134 | spl42_129 | spl42_4), inference(avatar_split_clause, [], [f434, f651, f1247, f1273, f1283, f1293, f1319, f1329, f1339, f1349, f1375, f1385, f1395, f1405, f1431, f1441, f1451])).
fof(f1451, plain, (spl42_159 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl42_159])])).
fof(f1441, plain, (spl42_158 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl42_158])])).
fof(f1431, plain, (spl42_157 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl42_157])])).
fof(f1405, plain, (spl42_152 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl42_152])])).
fof(f1395, plain, (spl42_151 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl42_151])])).
fof(f1385, plain, (spl42_150 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl42_150])])).
fof(f1375, plain, (spl42_149 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl42_149])])).
fof(f1349, plain, (spl42_144 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl42_144])])).
fof(f1339, plain, (spl42_143 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl42_143])])).
fof(f1329, plain, (spl42_142 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl42_142])])).
fof(f1319, plain, (spl42_141 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl42_141])])).
fof(f1293, plain, (spl42_136 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl42_136])])).
fof(f1283, plain, (spl42_135 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl42_135])])).
fof(f1273, plain, (spl42_134 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl42_134])])).
fof(f1247, plain, (spl42_129 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl42_129])])).
fof(f434, plain, ((e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1462, plain, (spl42_159 | spl42_158 | spl42_157 | spl42_152 | spl42_151 | spl42_150 | spl42_149 | spl42_144 | spl42_143 | spl42_142 | spl42_141 | spl42_136 | spl42_135 | spl42_134 | spl42_129 | ~ spl42_132), inference(avatar_split_clause, [], [f437, f1261, f1247, f1273, f1283, f1293, f1319, f1329, f1339, f1349, f1375, f1385, f1395, f1405, f1431, f1441, f1451])).
fof(f437, plain, (~ (e11 = op1(e13, op1(e13, e11))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1461, plain, (spl42_159 | spl42_158 | spl42_157 | spl42_152 | spl42_151 | spl42_150 | spl42_149 | spl42_144 | spl42_143 | spl42_142 | spl42_141 | spl42_136 | spl42_135 | spl42_134 | spl42_129 | ~ spl42_131), inference(avatar_split_clause, [], [f438, f1256, f1247, f1273, f1283, f1293, f1319, f1329, f1339, f1349, f1375, f1385, f1395, f1405, f1431, f1441, f1451])).
fof(f438, plain, (~ (e12 = op1(e13, op1(e13, e12))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1459, plain, (~ spl42_159 | spl42_61), inference(avatar_split_clause, [], [f412, f894, f1451])).
fof(f412, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1457, plain, (~ spl42_159 | ~ spl42_156), inference(avatar_split_clause, [], [f414, f1424, f1451])).
fof(f414, plain, (~ (e10 = op1(e10, op1(e10, e10))) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f1449, plain, (~ spl42_158 | spl42_57), inference(avatar_split_clause, [], [f406, f877, f1441])).
fof(f406, plain, ((e10 = op1(e10, e11)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1448, plain, (~ spl42_158 | spl42_45), inference(avatar_split_clause, [], [f407, f826, f1441])).
fof(f407, plain, ((e10 = op1(e11, e10)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f1444, plain, (~ spl42_158 | ~ spl42_153), inference(avatar_split_clause, [], [f411, f1409, f1441])).
fof(f411, plain, (~ (e13 = op1(e10, op1(e10, e13))) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f1439, plain, (~ spl42_157 | spl42_53), inference(avatar_split_clause, [], [f400, f860, f1431])).
fof(f400, plain, ((e10 = op1(e10, e12)) | ~ sP2), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1428, plain, (~ spl42_152 | spl42_13), inference(avatar_split_clause, [], [f395, f690, f1405])).
fof(f395, plain, ((e10 = op1(e13, e10)) | ~ sP3), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e13 = op1(e10, op1(e10, e13))) & ~ (e12 = op1(e10, op1(e10, e12))) & ~ (e11 = op1(e10, op1(e10, e11))) & ~ (e10 = op1(e10, op1(e10, e10))) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f1403, plain, (~ spl42_151 | spl42_46), inference(avatar_split_clause, [], [f388, f830, f1395])).
fof(f388, plain, ((e11 = op1(e11, e10)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f1402, plain, (~ spl42_151 | spl42_58), inference(avatar_split_clause, [], [f389, f881, f1395])).
fof(f389, plain, ((e11 = op1(e10, e11)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f1399, plain, (~ spl42_151 | ~ spl42_146), inference(avatar_split_clause, [], [f392, f1358, f1395])).
fof(f392, plain, (~ (e12 = op1(e11, op1(e11, e12))) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f1398, plain, (~ spl42_151 | ~ spl42_145), inference(avatar_split_clause, [], [f393, f1353, f1395])).
fof(f393, plain, (~ (e13 = op1(e11, op1(e11, e13))) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f1393, plain, (~ spl42_150 | spl42_42), inference(avatar_split_clause, [], [f382, f813, f1385])).
fof(f382, plain, ((e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f1390, plain, (~ spl42_150 | ~ spl42_147), inference(avatar_split_clause, [], [f385, f1363, f1385])).
fof(f385, plain, (~ (e11 = op1(e11, op1(e11, e11))) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f1389, plain, (~ spl42_150 | ~ spl42_146), inference(avatar_split_clause, [], [f386, f1358, f1385])).
fof(f386, plain, (~ (e12 = op1(e11, op1(e11, e12))) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f1388, plain, (~ spl42_150 | ~ spl42_145), inference(avatar_split_clause, [], [f387, f1353, f1385])).
fof(f387, plain, (~ (e13 = op1(e11, op1(e11, e13))) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f1383, plain, (~ spl42_149 | spl42_38), inference(avatar_split_clause, [], [f376, f796, f1375])).
fof(f376, plain, ((e11 = op1(e11, e12)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f1382, plain, (~ spl42_149 | spl42_26), inference(avatar_split_clause, [], [f377, f745, f1375])).
fof(f377, plain, ((e11 = op1(e12, e11)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1378, plain, (~ spl42_149 | ~ spl42_145), inference(avatar_split_clause, [], [f381, f1353, f1375])).
fof(f381, plain, (~ (e13 = op1(e11, op1(e11, e13))) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1372, plain, (~ spl42_144 | spl42_10), inference(avatar_split_clause, [], [f371, f677, f1349])).
fof(f371, plain, ((e11 = op1(e13, e11)) | ~ sP7), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e13 = op1(e11, op1(e11, e13))) & ~ (e12 = op1(e11, op1(e11, e12))) & ~ (e11 = op1(e11, op1(e11, e11))) & ~ (e10 = op1(e11, op1(e11, e10))) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f1347, plain, (~ spl42_143 | spl42_31), inference(avatar_split_clause, [], [f364, f766, f1339])).
fof(f364, plain, ((e12 = op1(e12, e10)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f1346, plain, (~ spl42_143 | spl42_55), inference(avatar_split_clause, [], [f365, f868, f1339])).
fof(f365, plain, ((e12 = op1(e10, e12)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1343, plain, (~ spl42_143 | ~ spl42_138), inference(avatar_split_clause, [], [f368, f1302, f1339])).
fof(f368, plain, (~ (e12 = op1(e12, op1(e12, e12))) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1337, plain, (~ spl42_142 | spl42_27), inference(avatar_split_clause, [], [f358, f749, f1329])).
fof(f358, plain, ((e12 = op1(e12, e11)) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f1336, plain, (~ spl42_142 | spl42_39), inference(avatar_split_clause, [], [f359, f800, f1329])).
fof(f359, plain, ((e12 = op1(e11, e12)) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f1334, plain, (~ spl42_142 | ~ spl42_139), inference(avatar_split_clause, [], [f361, f1307, f1329])).
fof(f361, plain, (~ (e11 = op1(e12, op1(e12, e11))) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f1327, plain, (~ spl42_141 | spl42_23), inference(avatar_split_clause, [], [f352, f732, f1319])).
fof(f352, plain, ((e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f1316, plain, (~ spl42_136 | spl42_7), inference(avatar_split_clause, [], [f347, f664, f1293])).
fof(f347, plain, ((e12 = op1(e13, e12)) | ~ sP11), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ((~ (e13 = op1(e12, op1(e12, e13))) & ~ (e12 = op1(e12, op1(e12, e12))) & ~ (e11 = op1(e12, op1(e12, e11))) & ~ (e10 = op1(e12, op1(e12, e10))) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f1291, plain, (~ spl42_135 | spl42_16), inference(avatar_split_clause, [], [f340, f702, f1283])).
fof(f340, plain, ((e13 = op1(e13, e10)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f1290, plain, (~ spl42_135 | spl42_52), inference(avatar_split_clause, [], [f341, f855, f1283])).
fof(f341, plain, ((e13 = op1(e10, e13)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f1281, plain, (~ spl42_134 | spl42_12), inference(avatar_split_clause, [], [f334, f685, f1273])).
fof(f334, plain, ((e13 = op1(e13, e11)) | ~ sP13), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f1277, plain, (~ spl42_134 | ~ spl42_131), inference(avatar_split_clause, [], [f338, f1256, f1273])).
fof(f338, plain, (~ (e12 = op1(e13, op1(e13, e12))) | ~ sP13), inference(cnf_transformation, [], [f67])).
fof(f1276, plain, (~ spl42_134 | ~ spl42_130), inference(avatar_split_clause, [], [f339, f1251, f1273])).
fof(f339, plain, (~ (e13 = op1(e13, op1(e13, e13))) | ~ sP13), inference(cnf_transformation, [], [f67])).
fof(f1271, plain, (~ spl42_129 | spl42_8), inference(avatar_split_clause, [], [f328, f668, f1247])).
fof(f328, plain, ((e13 = op1(e13, e12)) | ~ sP14), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e13 = op1(e13, op1(e13, e13))) & ~ (e12 = op1(e13, op1(e13, e12))) & ~ (e11 = op1(e13, op1(e13, e11))) & ~ (e10 = op1(e13, op1(e13, e10))) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f1244, plain, (spl42_125 | spl42_109 | spl42_93 | spl42_77), inference(avatar_split_clause, [], [f173, f994, f1062, f1130, f1198])).
fof(f173, plain, ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax4)).
fof(f1242, plain, (spl42_126 | spl42_110 | spl42_94 | spl42_78), inference(avatar_split_clause, [], [f175, f998, f1066, f1134, f1202])).
fof(f175, plain, ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f1241, plain, (spl42_127 | spl42_123 | spl42_119 | spl42_115), inference(avatar_split_clause, [], [f176, f1155, f1172, f1189, f1206])).
fof(f176, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1240, plain, (spl42_127 | spl42_111 | spl42_95 | spl42_79), inference(avatar_split_clause, [], [f177, f1002, f1070, f1138, f1206])).
fof(f177, plain, ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1238, plain, (spl42_128 | spl42_112 | spl42_96 | spl42_80), inference(avatar_split_clause, [], [f179, f1006, f1074, f1142, f1210])).
fof(f179, plain, ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1235, plain, (spl42_110 | spl42_106 | spl42_102 | spl42_98), inference(avatar_split_clause, [], [f182, f1083, f1100, f1117, f1134])).
fof(f182, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1231, plain, (spl42_112 | spl42_108 | spl42_104 | spl42_100), inference(avatar_split_clause, [], [f186, f1091, f1108, f1125, f1142])).
fof(f186, plain, ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1230, plain, (spl42_124 | spl42_108 | spl42_92 | spl42_76), inference(avatar_split_clause, [], [f187, f989, f1057, f1125, f1193])).
fof(f187, plain, ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1229, plain, (spl42_93 | spl42_89 | spl42_85 | spl42_81), inference(avatar_split_clause, [], [f188, f1011, f1028, f1045, f1062])).
fof(f188, plain, ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1226, plain, (spl42_118 | spl42_102 | spl42_86 | spl42_70), inference(avatar_split_clause, [], [f191, f964, f1032, f1100, f1168])).
fof(f191, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1225, plain, (spl42_95 | spl42_91 | spl42_87 | spl42_83), inference(avatar_split_clause, [], [f192, f1019, f1036, f1053, f1070])).
fof(f192, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1224, plain, (spl42_119 | spl42_103 | spl42_87 | spl42_71), inference(avatar_split_clause, [], [f193, f968, f1036, f1104, f1172])).
fof(f193, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1217, plain, (spl42_79 | spl42_75 | spl42_71 | spl42_67), inference(avatar_split_clause, [], [f200, f951, f968, f985, f1002])).
fof(f200, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1216, plain, (spl42_115 | spl42_99 | spl42_83 | spl42_67), inference(avatar_split_clause, [], [f201, f951, f1019, f1087, f1155])).
fof(f201, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1215, plain, (spl42_80 | spl42_76 | spl42_72 | spl42_68), inference(avatar_split_clause, [], [f202, f955, f972, f989, f1006])).
fof(f202, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1214, plain, (spl42_116 | spl42_100 | spl42_84 | spl42_68), inference(avatar_split_clause, [], [f203, f955, f1023, f1091, f1159])).
fof(f203, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1213, plain, (spl42_125 | spl42_126 | spl42_127 | spl42_128), inference(avatar_split_clause, [], [f156, f1210, f1206, f1202, f1198])).
fof(f156, plain, ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax3)).
fof(f1196, plain, (spl42_121 | spl42_122 | spl42_123 | spl42_124), inference(avatar_split_clause, [], [f157, f1193, f1189, f1185, f1181])).
fof(f157, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f1179, plain, (spl42_117 | spl42_118 | spl42_119 | spl42_120), inference(avatar_split_clause, [], [f158, f1176, f1172, f1168, f1164])).
fof(f158, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f1162, plain, (spl42_113 | spl42_114 | spl42_115 | spl42_116), inference(avatar_split_clause, [], [f159, f1159, f1155, f1151, f1147])).
fof(f159, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f1145, plain, (spl42_109 | spl42_110 | spl42_111 | spl42_112), inference(avatar_split_clause, [], [f160, f1142, f1138, f1134, f1130])).
fof(f160, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1111, plain, (spl42_101 | spl42_102 | spl42_103 | spl42_104), inference(avatar_split_clause, [], [f162, f1108, f1104, f1100, f1096])).
fof(f162, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f1094, plain, (spl42_97 | spl42_98 | spl42_99 | spl42_100), inference(avatar_split_clause, [], [f163, f1091, f1087, f1083, f1079])).
fof(f163, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1077, plain, (spl42_93 | spl42_94 | spl42_95 | spl42_96), inference(avatar_split_clause, [], [f164, f1074, f1070, f1066, f1062])).
fof(f164, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f1060, plain, (spl42_89 | spl42_90 | spl42_91 | spl42_92), inference(avatar_split_clause, [], [f165, f1057, f1053, f1049, f1045])).
fof(f165, plain, ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f3])).
fof(f1026, plain, (spl42_81 | spl42_82 | spl42_83 | spl42_84), inference(avatar_split_clause, [], [f167, f1023, f1019, f1015, f1011])).
fof(f167, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f1009, plain, (spl42_77 | spl42_78 | spl42_79 | spl42_80), inference(avatar_split_clause, [], [f168, f1006, f1002, f998, f994])).
fof(f168, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f940, plain, (spl42_61 | spl42_45 | spl42_29 | spl42_13), inference(avatar_split_clause, [], [f125, f690, f758, f826, f894])).
fof(f125, plain, ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax2)).
fof(f939, plain, (spl42_62 | spl42_58 | spl42_54 | spl42_50), inference(avatar_split_clause, [], [f126, f847, f864, f881, f898])).
fof(f126, plain, ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f938, plain, (spl42_62 | spl42_46 | spl42_30 | spl42_14), inference(avatar_split_clause, [], [f127, f694, f762, f830, f898])).
fof(f127, plain, ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f937, plain, (spl42_63 | spl42_59 | spl42_55 | spl42_51), inference(avatar_split_clause, [], [f128, f851, f868, f885, f902])).
fof(f128, plain, ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f935, plain, (spl42_64 | spl42_60 | spl42_56 | spl42_52), inference(avatar_split_clause, [], [f130, f855, f872, f889, f906])).
fof(f130, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f934, plain, (spl42_64 | spl42_48 | spl42_32 | spl42_16), inference(avatar_split_clause, [], [f131, f702, f770, f838, f906])).
fof(f131, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f933, plain, (spl42_45 | spl42_41 | spl42_37 | spl42_33), inference(avatar_split_clause, [], [f132, f775, f792, f809, f826])).
fof(f132, plain, ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f932, plain, (spl42_57 | spl42_41 | spl42_25 | spl42_9), inference(avatar_split_clause, [], [f133, f673, f741, f809, f877])).
fof(f133, plain, ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f930, plain, (spl42_58 | spl42_42 | spl42_26 | spl42_10), inference(avatar_split_clause, [], [f135, f677, f745, f813, f881])).
fof(f135, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f928, plain, (spl42_59 | spl42_43 | spl42_27 | spl42_11), inference(avatar_split_clause, [], [f137, f681, f749, f817, f885])).
fof(f137, plain, ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f922, plain, (spl42_54 | spl42_38 | spl42_22 | spl42_6), inference(avatar_split_clause, [], [f143, f660, f728, f796, f864])).
fof(f143, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f921, plain, (spl42_31 | spl42_27 | spl42_23 | spl42_19), inference(avatar_split_clause, [], [f144, f715, f732, f749, f766])).
fof(f144, plain, ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f920, plain, (spl42_55 | spl42_39 | spl42_23 | spl42_7), inference(avatar_split_clause, [], [f145, f664, f732, f800, f868])).
fof(f145, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f916, plain, (spl42_49 | spl42_33 | spl42_17 | spl42_1), inference(avatar_split_clause, [], [f149, f639, f707, f775, f843])).
fof(f149, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f913, plain, (spl42_15 | spl42_11 | spl42_7 | spl42_3), inference(avatar_split_clause, [], [f152, f647, f664, f681, f698])).
fof(f152, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f912, plain, (spl42_51 | spl42_35 | spl42_19 | spl42_3), inference(avatar_split_clause, [], [f153, f647, f715, f783, f851])).
fof(f153, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f911, plain, (spl42_16 | spl42_12 | spl42_8 | spl42_4), inference(avatar_split_clause, [], [f154, f651, f668, f685, f702])).
fof(f154, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f910, plain, (spl42_52 | spl42_36 | spl42_20 | spl42_4), inference(avatar_split_clause, [], [f155, f651, f719, f787, f855])).
fof(f155, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f892, plain, (spl42_57 | spl42_58 | spl42_59 | spl42_60), inference(avatar_split_clause, [], [f109, f889, f885, f881, f877])).
fof(f109, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG120+1.p', ax1)).
fof(f875, plain, (spl42_53 | spl42_54 | spl42_55 | spl42_56), inference(avatar_split_clause, [], [f110, f872, f868, f864, f860])).
fof(f110, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f858, plain, (spl42_49 | spl42_50 | spl42_51 | spl42_52), inference(avatar_split_clause, [], [f111, f855, f851, f847, f843])).
fof(f111, plain, ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f1])).
fof(f841, plain, (spl42_45 | spl42_46 | spl42_47 | spl42_48), inference(avatar_split_clause, [], [f112, f838, f834, f830, f826])).
fof(f112, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f807, plain, (spl42_37 | spl42_38 | spl42_39 | spl42_40), inference(avatar_split_clause, [], [f114, f804, f800, f796, f792])).
fof(f114, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f790, plain, (spl42_33 | spl42_34 | spl42_35 | spl42_36), inference(avatar_split_clause, [], [f115, f787, f783, f779, f775])).
fof(f115, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f773, plain, (spl42_29 | spl42_30 | spl42_31 | spl42_32), inference(avatar_split_clause, [], [f116, f770, f766, f762, f758])).
fof(f116, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f756, plain, (spl42_25 | spl42_26 | spl42_27 | spl42_28), inference(avatar_split_clause, [], [f117, f753, f749, f745, f741])).
fof(f117, plain, ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f722, plain, (spl42_17 | spl42_18 | spl42_19 | spl42_20), inference(avatar_split_clause, [], [f119, f719, f715, f711, f707])).
fof(f119, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f705, plain, (spl42_13 | spl42_14 | spl42_15 | spl42_16), inference(avatar_split_clause, [], [f120, f702, f698, f694, f690])).
fof(f120, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).