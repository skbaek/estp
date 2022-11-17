fof(f4170, plain, $false, inference(avatar_sat_refutation, [], [f236, f253, f270, f287, f304, f321, f338, f355, f372, f389, f406, f423, f440, f457, f474, f491, f492, f493, f494, f495, f496, f497, f498, f499, f500, f501, f502, f504, f505, f506, f507, f508, f509, f511, f512, f515, f516, f517, f518, f519, f520, f521, f522, f532, f537, f542, f547, f556, f561, f566, f571, f580, f585, f590, f595, f596, f597, f598, f599, f600, f601, f602, f603, f604, f605, f606, f607, f608, f613, f618, f623, f628, f637, f646, f651, f660, f665, f670, f675, f680, f685, f694, f699, f704, f709, f714, f715, f720, f725, f730, f735, f740, f741, f746, f747, f748, f769, f781, f790, f793, f799, f810, f811, f850, f853, f864, f867, f872, f883, f886, f894, f898, f934, f938, f970, f974, f1011, f1048, f1051, f1058, f1085, f1107, f1183, f1207, f1222, f1235, f1251, f1263, f1271, f1292, f1300, f1303, f1333, f1340, f1346, f1355, f1359, f1390, f1394, f1422, f1430, f1442, f1457, f1460, f1476, f1500, f1504, f1518, f1533, f1540, f1569, f1570, f1587, f1612, f1616, f1632, f1642, f1658, f1692, f1698, f1745, f1807, f1812, f1815, f1841, f1857, f1872, f1877, f1906, f1915, f1981, f2030, f2037, f2050, f2053, f2063, f2097, f2110, f2131, f2146, f2150, f2154, f2185, f2199, f2228, f2263, f2278, f2346, f2352, f2355, f2357, f2365, f2373, f2377, f2414, f2435, f2456, f2475, f2477, f2506, f2522, f2553, f2574, f2596, f2604, f2609, f2618, f2656, f2688, f2713, f2720, f2740, f2753, f2754, f2764, f2775, f2778, f2780, f2785, f2812, f2815, f2834, f2848, f2849, f2859, f2860, f2875, f2881, f2914, f2939, f2946, f2947, f2949, f2958, f2963, f2991, f3007, f3009, f3010, f3014, f3045, f3066, f3095, f3100, f3116, f3132, f3133, f3136, f3143, f3144, f3154, f3155, f3163, f3179, f3180, f3195, f3206, f3226, f3231, f3252, f3279, f3280, f3291, f3299, f3307, f3314, f3328, f3338, f3346, f3367, f3369, f3375, f3385, f3396, f3401, f3406, f3413, f3441, f3470, f3513, f3526, f3529, f3539, f3544, f3545, f3576, f3588, f3596, f3603, f3609, f3637, f3647, f3651, f3654, f3662, f3680, f3681, f3686, f3699, f3700, f3701, f3708, f3709, f3724, f3736, f3754, f3755, f3762, f3772, f3779, f3788, f3789, f3791, f3793, f3795, f3801, f3810, f3835, f3854, f3876, f3889, f3890, f3901, f3902, f3903, f3909, f3914, f3915, f3922, f3923, f3935, f3936, f3944, f3950, f3959, f3972, f3984, f3997, f4002, f4010, f4011, f4026, f4033, f4034, f4035, f4040, f4041, f4046, f4053, f4066, f4076, f4081, f4094, f4106, f4107, f4122, f4127, f4128, f4138, f4143, f4149, f4162])).
fof(f4162, plain, (~ spl3_22 | ~ spl3_44 | spl3_105), inference(avatar_contradiction_clause, [], [f4161])).
fof(f4161, plain, ($false | (~ spl3_22 | ~ spl3_44 | spl3_105)), inference(subsumption_resolution, [], [f4160, f405])).
fof(f405, plain, ((e3 = op(e1, e1)) | ~ spl3_44), inference(avatar_component_clause, [], [f403])).
fof(f403, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f4160, plain, (~ (e3 = op(e1, e1)) | (~ spl3_22 | spl3_105)), inference(forward_demodulation, [], [f734, f312])).
fof(f312, plain, ((e1 = op(e2, e2)) | ~ spl3_22), inference(avatar_component_clause, [], [f310])).
fof(f310, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f734, plain, (~ (e3 = op(op(e2, e2), op(e2, e2))) | spl3_105), inference(avatar_component_clause, [], [f732])).
fof(f732, plain, (spl3_105 <=> (e3 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_105])])).
fof(f4149, plain, (~ spl3_6 | ~ spl3_14), inference(avatar_split_clause, [], [f4146, f276, f242])).
fof(f242, plain, (spl3_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_6])])).
fof(f276, plain, (spl3_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_14])])).
fof(f4146, plain, (~ (e1 = op(e3, e2)) | ~ spl3_14), inference(backward_demodulation, [], [f152, f278])).
fof(f278, plain, ((e1 = op(e3, e0)) | ~ spl3_14), inference(avatar_component_clause, [], [f276])).
fof(f152, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax3)).
fof(f4143, plain, (~ spl3_17 | ~ spl3_22 | ~ spl3_44 | spl3_98), inference(avatar_contradiction_clause, [], [f4142])).
fof(f4142, plain, ($false | (~ spl3_17 | ~ spl3_22 | ~ spl3_44 | spl3_98)), inference(subsumption_resolution, [], [f4141, f291])).
fof(f291, plain, ((e0 = op(e2, e3)) | ~ spl3_17), inference(avatar_component_clause, [], [f289])).
fof(f289, plain, (spl3_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_17])])).
fof(f4141, plain, (~ (e0 = op(e2, e3)) | (~ spl3_22 | ~ spl3_44 | spl3_98)), inference(forward_demodulation, [], [f4137, f405])).
fof(f4137, plain, (~ (e0 = op(e2, op(e1, e1))) | (~ spl3_22 | spl3_98)), inference(backward_demodulation, [], [f698, f312])).
fof(f698, plain, (~ (e0 = op(e2, op(op(e2, e2), op(e2, e2)))) | spl3_98), inference(avatar_component_clause, [], [f696])).
fof(f696, plain, (spl3_98 <=> (e0 = op(e2, op(op(e2, e2), op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl3_98])])).
fof(f4138, plain, (~ spl3_6 | ~ spl3_22), inference(avatar_split_clause, [], [f4130, f310, f242])).
fof(f4130, plain, (~ (e1 = op(e3, e2)) | ~ spl3_22), inference(backward_demodulation, [], [f126, f312])).
fof(f126, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f4128, plain, (~ spl3_23 | ~ spl3_27), inference(avatar_split_clause, [], [f4124, f331, f314])).
fof(f314, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f331, plain, (spl3_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_27])])).
fof(f4124, plain, (~ (e2 = op(e2, e2)) | ~ spl3_27), inference(backward_demodulation, [], [f148, f333])).
fof(f333, plain, ((e2 = op(e2, e1)) | ~ spl3_27), inference(avatar_component_clause, [], [f331])).
fof(f148, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f4127, plain, (~ spl3_11 | ~ spl3_27), inference(avatar_split_clause, [], [f4123, f331, f263])).
fof(f263, plain, (spl3_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_11])])).
fof(f4123, plain, (~ (e2 = op(e3, e1)) | ~ spl3_27), inference(backward_demodulation, [], [f120, f333])).
fof(f120, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f4122, plain, (~ spl3_26 | ~ spl3_58), inference(avatar_split_clause, [], [f4117, f463, f327])).
fof(f327, plain, (spl3_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_26])])).
fof(f463, plain, (spl3_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_58])])).
fof(f4117, plain, (~ (e1 = op(e2, e1)) | ~ spl3_58), inference(backward_demodulation, [], [f116, f465])).
fof(f465, plain, ((e1 = op(e0, e1)) | ~ spl3_58), inference(avatar_component_clause, [], [f463])).
fof(f116, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f4107, plain, (~ spl3_57 | ~ spl3_61), inference(avatar_split_clause, [], [f4096, f476, f459])).
fof(f459, plain, (spl3_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_57])])).
fof(f476, plain, (spl3_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f4096, plain, (~ (e0 = op(e0, e1)) | ~ spl3_61), inference(backward_demodulation, [], [f133, f478])).
fof(f478, plain, ((e0 = op(e0, e0)) | ~ spl3_61), inference(avatar_component_clause, [], [f476])).
fof(f133, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f4106, plain, (~ spl3_13 | ~ spl3_61), inference(avatar_split_clause, [], [f4095, f476, f272])).
fof(f272, plain, (spl3_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_13])])).
fof(f4095, plain, (~ (e0 = op(e3, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f111, f478])).
fof(f111, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f4094, plain, (~ spl3_13 | ~ spl3_17 | spl3_66), inference(avatar_split_clause, [], [f4093, f529, f289, f272])).
fof(f529, plain, (spl3_66 <=> (op(e2, e3) = op(e3, op(e2, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_66])])).
fof(f4093, plain, (~ (e0 = op(e3, e0)) | (~ spl3_17 | spl3_66)), inference(forward_demodulation, [], [f531, f291])).
fof(f531, plain, (~ (op(e2, e3) = op(e3, op(e2, e3))) | spl3_66), inference(avatar_component_clause, [], [f529])).
fof(f4081, plain, (~ spl3_24 | ~ spl3_32), inference(avatar_split_clause, [], [f4080, f352, f318])).
fof(f318, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f352, plain, (spl3_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_32])])).
fof(f4080, plain, (~ (e3 = op(e2, e2)) | ~ spl3_32), inference(forward_demodulation, [], [f146, f354])).
fof(f354, plain, ((e3 = op(e2, e0)) | ~ spl3_32), inference(avatar_component_clause, [], [f352])).
fof(f146, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f4076, plain, (~ spl3_44 | ~ spl3_62 | spl3_107), inference(avatar_contradiction_clause, [], [f4075])).
fof(f4075, plain, ($false | (~ spl3_44 | ~ spl3_62 | spl3_107)), inference(subsumption_resolution, [], [f4074, f405])).
fof(f4074, plain, (~ (e3 = op(e1, e1)) | (~ spl3_62 | spl3_107)), inference(forward_demodulation, [], [f745, f482])).
fof(f482, plain, ((op(e0, e0) = e1) | ~ spl3_62), inference(avatar_component_clause, [], [f480])).
fof(f480, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f745, plain, (~ (e3 = op(op(e0, e0), op(e0, e0))) | spl3_107), inference(avatar_component_clause, [], [f743])).
fof(f743, plain, (spl3_107 <=> (e3 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_107])])).
fof(f4066, plain, (~ spl3_5 | ~ spl3_13), inference(avatar_split_clause, [], [f4061, f272, f238])).
fof(f238, plain, (spl3_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_5])])).
fof(f4061, plain, (~ (e0 = op(e3, e2)) | ~ spl3_13), inference(backward_demodulation, [], [f152, f274])).
fof(f274, plain, ((e0 = op(e3, e0)) | ~ spl3_13), inference(avatar_component_clause, [], [f272])).
fof(f4053, plain, (~ spl3_10 | ~ spl3_26), inference(avatar_split_clause, [], [f4048, f327, f259])).
fof(f259, plain, (spl3_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_10])])).
fof(f4048, plain, (~ (e1 = op(e3, e1)) | ~ spl3_26), inference(backward_demodulation, [], [f120, f329])).
fof(f329, plain, ((e1 = op(e2, e1)) | ~ spl3_26), inference(avatar_component_clause, [], [f327])).
fof(f4046, plain, (~ spl3_28 | ~ spl3_32), inference(avatar_split_clause, [], [f4043, f352, f335])).
fof(f335, plain, (spl3_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_28])])).
fof(f4043, plain, (~ (e3 = op(e2, e1)) | ~ spl3_32), inference(backward_demodulation, [], [f145, f354])).
fof(f145, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f4041, plain, (~ spl3_10 | ~ spl3_34 | spl3_71), inference(avatar_split_clause, [], [f4039, f553, f361, f259])).
fof(f361, plain, (spl3_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_34])])).
fof(f553, plain, (spl3_71 <=> (op(e1, e3) = op(e3, op(e1, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_71])])).
fof(f4039, plain, (~ (e1 = op(e3, e1)) | (~ spl3_34 | spl3_71)), inference(backward_demodulation, [], [f555, f363])).
fof(f363, plain, ((e1 = op(e1, e3)) | ~ spl3_34), inference(avatar_component_clause, [], [f361])).
fof(f555, plain, (~ (op(e1, e3) = op(e3, op(e1, e3))) | spl3_71), inference(avatar_component_clause, [], [f553])).
fof(f4040, plain, (~ spl3_18 | ~ spl3_34), inference(avatar_split_clause, [], [f4036, f361, f293])).
fof(f293, plain, (spl3_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_18])])).
fof(f4036, plain, (~ (e1 = op(e2, e3)) | ~ spl3_34), inference(backward_demodulation, [], [f130, f363])).
fof(f130, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f4035, plain, (~ spl3_29 | ~ spl3_37 | spl3_72), inference(avatar_split_clause, [], [f4032, f558, f374, f340])).
fof(f340, plain, (spl3_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_29])])).
fof(f374, plain, (spl3_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_37])])).
fof(f558, plain, (spl3_72 <=> (op(e1, e2) = op(e2, op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_72])])).
fof(f4032, plain, (~ (e0 = op(e2, e0)) | (~ spl3_37 | spl3_72)), inference(backward_demodulation, [], [f560, f376])).
fof(f376, plain, ((e0 = op(e1, e2)) | ~ spl3_37), inference(avatar_component_clause, [], [f374])).
fof(f560, plain, (~ (op(e1, e2) = op(e2, op(e1, e2))) | spl3_72), inference(avatar_component_clause, [], [f558])).
fof(f4034, plain, (~ spl3_33 | ~ spl3_37), inference(avatar_split_clause, [], [f4028, f374, f357])).
fof(f357, plain, (spl3_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_33])])).
fof(f4028, plain, (~ (e0 = op(e1, e3)) | ~ spl3_37), inference(backward_demodulation, [], [f144, f376])).
fof(f144, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f4033, plain, (~ spl3_5 | ~ spl3_37), inference(avatar_split_clause, [], [f4027, f374, f238])).
fof(f4027, plain, (~ (e0 = op(e3, e2)) | ~ spl3_37), inference(backward_demodulation, [], [f125, f376])).
fof(f125, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f4026, plain, (~ spl3_44 | ~ spl3_51 | ~ spl3_62 | spl3_99), inference(avatar_contradiction_clause, [], [f4025])).
fof(f4025, plain, ($false | (~ spl3_44 | ~ spl3_51 | ~ spl3_62 | spl3_99)), inference(subsumption_resolution, [], [f4020, f435])).
fof(f435, plain, ((e2 = op(e0, e3)) | ~ spl3_51), inference(avatar_component_clause, [], [f433])).
fof(f433, plain, (spl3_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_51])])).
fof(f4020, plain, (~ (e2 = op(e0, e3)) | (~ spl3_44 | ~ spl3_62 | spl3_99)), inference(backward_demodulation, [], [f3886, f405])).
fof(f3886, plain, (~ (e2 = op(e0, op(e1, e1))) | (~ spl3_62 | spl3_99)), inference(backward_demodulation, [], [f703, f482])).
fof(f703, plain, (~ (e2 = op(e0, op(op(e0, e0), op(e0, e0)))) | spl3_99), inference(avatar_component_clause, [], [f701])).
fof(f701, plain, (spl3_99 <=> (e2 = op(e0, op(op(e0, e0), op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl3_99])])).
fof(f4011, plain, (~ spl3_43 | ~ spl3_47), inference(avatar_split_clause, [], [f4005, f416, f399])).
fof(f399, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f416, plain, (spl3_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_47])])).
fof(f4005, plain, (~ (e2 = op(e1, e1)) | ~ spl3_47), inference(backward_demodulation, [], [f139, f418])).
fof(f418, plain, ((e2 = op(e1, e0)) | ~ spl3_47), inference(avatar_component_clause, [], [f416])).
fof(f139, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f4010, plain, (~ spl3_15 | ~ spl3_47), inference(avatar_split_clause, [], [f4004, f416, f280])).
fof(f280, plain, (spl3_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_15])])).
fof(f4004, plain, (~ (e2 = op(e3, e0)) | ~ spl3_47), inference(backward_demodulation, [], [f113, f418])).
fof(f113, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f4002, plain, (~ spl3_43 | ~ spl3_62 | spl3_102), inference(avatar_split_clause, [], [f4001, f717, f480, f399])).
fof(f717, plain, (spl3_102 <=> (e2 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_102])])).
fof(f4001, plain, (~ (e2 = op(e1, e1)) | (~ spl3_62 | spl3_102)), inference(forward_demodulation, [], [f719, f482])).
fof(f719, plain, (~ (e2 = op(op(e0, e0), op(e0, e0))) | spl3_102), inference(avatar_component_clause, [], [f717])).
fof(f3997, plain, (~ spl3_43 | ~ spl3_56 | ~ spl3_62 | spl3_97), inference(avatar_contradiction_clause, [], [f3996])).
fof(f3996, plain, ($false | (~ spl3_43 | ~ spl3_56 | ~ spl3_62 | spl3_97)), inference(subsumption_resolution, [], [f3995, f456])).
fof(f456, plain, ((e3 = op(e0, e2)) | ~ spl3_56), inference(avatar_component_clause, [], [f454])).
fof(f454, plain, (spl3_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_56])])).
fof(f3995, plain, (~ (e3 = op(e0, e2)) | (~ spl3_43 | ~ spl3_62 | spl3_97)), inference(forward_demodulation, [], [f3994, f401])).
fof(f401, plain, ((e2 = op(e1, e1)) | ~ spl3_43), inference(avatar_component_clause, [], [f399])).
fof(f3994, plain, (~ (e3 = op(e0, op(e1, e1))) | (~ spl3_62 | spl3_97)), inference(forward_demodulation, [], [f693, f482])).
fof(f693, plain, (~ (e3 = op(e0, op(op(e0, e0), op(e0, e0)))) | spl3_97), inference(avatar_component_clause, [], [f691])).
fof(f691, plain, (spl3_97 <=> (e3 = op(e0, op(op(e0, e0), op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl3_97])])).
fof(f3984, plain, (~ spl3_7 | ~ spl3_15), inference(avatar_split_clause, [], [f3981, f280, f246])).
fof(f246, plain, (spl3_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f3981, plain, (~ (e2 = op(e3, e2)) | ~ spl3_15), inference(backward_demodulation, [], [f152, f282])).
fof(f282, plain, ((e2 = op(e3, e0)) | ~ spl3_15), inference(avatar_component_clause, [], [f280])).
fof(f3972, plain, (~ spl3_7 | ~ spl3_23), inference(avatar_split_clause, [], [f3965, f314, f246])).
fof(f3965, plain, (~ (e2 = op(e3, e2)) | ~ spl3_23), inference(backward_demodulation, [], [f126, f316])).
fof(f316, plain, ((e2 = op(e2, e2)) | ~ spl3_23), inference(avatar_component_clause, [], [f314])).
fof(f3959, plain, (~ spl3_17 | ~ spl3_29), inference(avatar_split_clause, [], [f3954, f340, f289])).
fof(f3954, plain, (~ (e0 = op(e2, e3)) | ~ spl3_29), inference(backward_demodulation, [], [f147, f342])).
fof(f342, plain, ((e0 = op(e2, e0)) | ~ spl3_29), inference(avatar_component_clause, [], [f340])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3950, plain, (~ spl3_17 | ~ spl3_33), inference(avatar_split_clause, [], [f3945, f357, f289])).
fof(f3945, plain, (~ (e0 = op(e2, e3)) | ~ spl3_33), inference(backward_demodulation, [], [f130, f359])).
fof(f359, plain, ((e0 = op(e1, e3)) | ~ spl3_33), inference(avatar_component_clause, [], [f357])).
fof(f3944, plain, (~ spl3_22 | ~ spl3_38), inference(avatar_split_clause, [], [f3938, f378, f310])).
fof(f378, plain, (spl3_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_38])])).
fof(f3938, plain, (~ (e1 = op(e2, e2)) | ~ spl3_38), inference(backward_demodulation, [], [f124, f380])).
fof(f380, plain, ((e1 = op(e1, e2)) | ~ spl3_38), inference(avatar_component_clause, [], [f378])).
fof(f124, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3936, plain, (~ spl3_35 | ~ spl3_43), inference(avatar_split_clause, [], [f3928, f399, f365])).
fof(f365, plain, (spl3_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_35])])).
fof(f3928, plain, (~ (e2 = op(e1, e3)) | ~ spl3_43), inference(backward_demodulation, [], [f143, f401])).
fof(f143, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3935, plain, (~ spl3_27 | ~ spl3_43), inference(avatar_split_clause, [], [f3925, f399, f331])).
fof(f3925, plain, (~ (e2 = op(e2, e1)) | ~ spl3_43), inference(backward_demodulation, [], [f118, f401])).
fof(f118, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3923, plain, (~ spl3_40 | ~ spl3_48), inference(avatar_split_clause, [], [f3919, f420, f386])).
fof(f386, plain, (spl3_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_40])])).
fof(f420, plain, (spl3_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_48])])).
fof(f3919, plain, (~ (e3 = op(e1, e2)) | ~ spl3_48), inference(backward_demodulation, [], [f140, f422])).
fof(f422, plain, ((e3 = op(e1, e0)) | ~ spl3_48), inference(avatar_component_clause, [], [f420])).
fof(f140, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3922, plain, (~ spl3_32 | ~ spl3_48), inference(avatar_split_clause, [], [f3916, f420, f352])).
fof(f3916, plain, (~ (e3 = op(e2, e0)) | ~ spl3_48), inference(backward_demodulation, [], [f112, f422])).
fof(f112, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3915, plain, (~ spl3_7 | ~ spl3_51 | spl3_76), inference(avatar_split_clause, [], [f3913, f577, f433, f246])).
fof(f577, plain, (spl3_76 <=> (op(e0, e3) = op(e3, op(e0, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_76])])).
fof(f3913, plain, (~ (e2 = op(e3, e2)) | (~ spl3_51 | spl3_76)), inference(backward_demodulation, [], [f579, f435])).
fof(f579, plain, (~ (op(e0, e3) = op(e3, op(e0, e3))) | spl3_76), inference(avatar_component_clause, [], [f577])).
fof(f3914, plain, (~ spl3_35 | ~ spl3_51), inference(avatar_split_clause, [], [f3910, f433, f365])).
fof(f3910, plain, (~ (e2 = op(e1, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f127, f435])).
fof(f127, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3909, plain, (~ spl3_40 | ~ spl3_56), inference(avatar_split_clause, [], [f3904, f454, f386])).
fof(f3904, plain, (~ (e3 = op(e1, e2)) | ~ spl3_56), inference(backward_demodulation, [], [f121, f456])).
fof(f121, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3903, plain, (~ spl3_45 | ~ spl3_57 | spl3_78), inference(avatar_split_clause, [], [f3900, f587, f459, f408])).
fof(f408, plain, (spl3_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_45])])).
fof(f587, plain, (spl3_78 <=> (op(e0, e1) = op(e1, op(e0, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_78])])).
fof(f3900, plain, (~ (e0 = op(e1, e0)) | (~ spl3_57 | spl3_78)), inference(backward_demodulation, [], [f589, f461])).
fof(f461, plain, ((e0 = op(e0, e1)) | ~ spl3_57), inference(avatar_component_clause, [], [f459])).
fof(f589, plain, (~ (op(e0, e1) = op(e1, op(e0, e1))) | spl3_78), inference(avatar_component_clause, [], [f587])).
fof(f3902, plain, (~ spl3_53 | ~ spl3_57), inference(avatar_split_clause, [], [f3895, f459, f442])).
fof(f442, plain, (spl3_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_53])])).
fof(f3895, plain, (~ (e0 = op(e0, e2)) | ~ spl3_57), inference(backward_demodulation, [], [f136, f461])).
fof(f136, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3901, plain, (~ spl3_9 | ~ spl3_57), inference(avatar_split_clause, [], [f3894, f459, f255])).
fof(f255, plain, (spl3_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_9])])).
fof(f3894, plain, (~ (e0 = op(e3, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f117, f461])).
fof(f117, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3890, plain, (~ spl3_50 | ~ spl3_62), inference(avatar_split_clause, [], [f3882, f480, f429])).
fof(f429, plain, (spl3_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_50])])).
fof(f3882, plain, (~ (e1 = op(e0, e3)) | ~ spl3_62), inference(backward_demodulation, [], [f135, f482])).
fof(f135, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3889, plain, (~ spl3_14 | ~ spl3_62), inference(avatar_split_clause, [], [f3879, f480, f276])).
fof(f3879, plain, (~ (e1 = op(e3, e0)) | ~ spl3_62), inference(backward_demodulation, [], [f111, f482])).
fof(f3876, plain, (~ spl3_40 | spl3_90 | ~ spl3_101), inference(avatar_split_clause, [], [f3874, f711, f657, f386])).
fof(f657, plain, (spl3_90 <=> (e3 = op(e1, op(op(e1, e1), op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl3_90])])).
fof(f711, plain, (spl3_101 <=> (e2 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_101])])).
fof(f3874, plain, (~ (e3 = op(e1, e2)) | (spl3_90 | ~ spl3_101)), inference(backward_demodulation, [], [f659, f712])).
fof(f712, plain, ((e2 = op(op(e1, e1), op(e1, e1))) | ~ spl3_101), inference(avatar_component_clause, [], [f711])).
fof(f659, plain, (~ (e3 = op(e1, op(op(e1, e1), op(e1, e1)))) | spl3_90), inference(avatar_component_clause, [], [f657])).
fof(f3854, plain, (~ spl3_22 | ~ spl3_60 | ~ spl3_63 | spl3_97), inference(avatar_contradiction_clause, [], [f3853])).
fof(f3853, plain, ($false | (~ spl3_22 | ~ spl3_60 | ~ spl3_63 | spl3_97)), inference(subsumption_resolution, [], [f3846, f473])).
fof(f473, plain, ((e3 = op(e0, e1)) | ~ spl3_60), inference(avatar_component_clause, [], [f471])).
fof(f471, plain, (spl3_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_60])])).
fof(f3846, plain, (~ (e3 = op(e0, e1)) | (~ spl3_22 | ~ spl3_63 | spl3_97)), inference(backward_demodulation, [], [f3799, f312])).
fof(f3799, plain, (~ (e3 = op(e0, op(e2, e2))) | (~ spl3_63 | spl3_97)), inference(forward_demodulation, [], [f693, f486])).
fof(f486, plain, ((op(e0, e0) = e2) | ~ spl3_63), inference(avatar_component_clause, [], [f484])).
fof(f484, plain, (spl3_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f3835, plain, (~ spl3_19 | ~ spl3_27), inference(avatar_split_clause, [], [f3831, f331, f297])).
fof(f297, plain, (spl3_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_19])])).
fof(f3831, plain, (~ (e2 = op(e2, e3)) | ~ spl3_27), inference(backward_demodulation, [], [f149, f333])).
fof(f149, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3810, plain, (~ spl3_13 | ~ spl3_45), inference(avatar_split_clause, [], [f3803, f408, f272])).
fof(f3803, plain, (~ (e0 = op(e3, e0)) | ~ spl3_45), inference(backward_demodulation, [], [f113, f410])).
fof(f410, plain, ((e0 = op(e1, e0)) | ~ spl3_45), inference(avatar_component_clause, [], [f408])).
fof(f3801, plain, (~ spl3_24 | ~ spl3_63 | spl3_107), inference(avatar_split_clause, [], [f3800, f743, f484, f318])).
fof(f3800, plain, (~ (e3 = op(e2, e2)) | (~ spl3_63 | spl3_107)), inference(forward_demodulation, [], [f745, f486])).
fof(f3795, plain, (~ spl3_36 | ~ spl3_4), inference(avatar_split_clause, [], [f3794, f233, f369])).
fof(f369, plain, (spl3_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_36])])).
fof(f233, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f3794, plain, (~ (e3 = op(e1, e3)) | ~ spl3_4), inference(forward_demodulation, [], [f131, f235])).
fof(f235, plain, ((e3 = op(e3, e3)) | ~ spl3_4), inference(avatar_component_clause, [], [f233])).
fof(f131, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3793, plain, (~ spl3_34 | ~ spl3_42), inference(avatar_split_clause, [], [f3792, f395, f361])).
fof(f395, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f3792, plain, (~ (e1 = op(e1, e3)) | ~ spl3_42), inference(forward_demodulation, [], [f143, f397])).
fof(f397, plain, ((e1 = op(e1, e1)) | ~ spl3_42), inference(avatar_component_clause, [], [f395])).
fof(f3791, plain, (~ spl3_34 | ~ spl3_50), inference(avatar_split_clause, [], [f3790, f429, f361])).
fof(f3790, plain, (~ (e1 = op(e1, e3)) | ~ spl3_50), inference(forward_demodulation, [], [f127, f431])).
fof(f431, plain, ((e1 = op(e0, e3)) | ~ spl3_50), inference(avatar_component_clause, [], [f429])).
fof(f3789, plain, (~ spl3_36 | ~ spl3_60 | spl3_78), inference(avatar_split_clause, [], [f3707, f587, f471, f369])).
fof(f3707, plain, (~ (e3 = op(e1, e3)) | (~ spl3_60 | spl3_78)), inference(backward_demodulation, [], [f589, f473])).
fof(f3788, plain, (~ spl3_22 | ~ spl3_63 | spl3_96), inference(avatar_split_clause, [], [f3697, f687, f484, f310])).
fof(f687, plain, (spl3_96 <=> (e1 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_96])])).
fof(f3697, plain, (~ (e1 = op(e2, e2)) | (~ spl3_63 | spl3_96)), inference(backward_demodulation, [], [f689, f486])).
fof(f689, plain, (~ (e1 = op(op(e0, e0), op(e0, e0))) | spl3_96), inference(avatar_component_clause, [], [f687])).
fof(f3779, plain, (~ spl3_9 | ~ spl3_13), inference(avatar_split_clause, [], [f3773, f272, f255])).
fof(f3773, plain, (~ (e0 = op(e3, e1)) | ~ spl3_13), inference(backward_demodulation, [], [f151, f274])).
fof(f151, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3772, plain, (~ spl3_7 | ~ spl3_19 | spl3_66), inference(avatar_split_clause, [], [f3771, f529, f297, f246])).
fof(f3771, plain, (~ (e2 = op(e3, e2)) | (~ spl3_19 | spl3_66)), inference(backward_demodulation, [], [f531, f299])).
fof(f299, plain, ((e2 = op(e2, e3)) | ~ spl3_19), inference(avatar_component_clause, [], [f297])).
fof(f3762, plain, (~ spl3_9 | ~ spl3_25), inference(avatar_split_clause, [], [f3756, f323, f255])).
fof(f323, plain, (spl3_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_25])])).
fof(f3756, plain, (~ (e0 = op(e3, e1)) | ~ spl3_25), inference(backward_demodulation, [], [f120, f325])).
fof(f325, plain, ((e0 = op(e2, e1)) | ~ spl3_25), inference(avatar_component_clause, [], [f323])).
fof(f3755, plain, (~ spl3_18 | ~ spl3_30), inference(avatar_split_clause, [], [f3750, f344, f293])).
fof(f344, plain, (spl3_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_30])])).
fof(f3750, plain, (~ (e1 = op(e2, e3)) | ~ spl3_30), inference(backward_demodulation, [], [f147, f346])).
fof(f346, plain, ((e1 = op(e2, e0)) | ~ spl3_30), inference(avatar_component_clause, [], [f344])).
fof(f3754, plain, (~ spl3_14 | ~ spl3_30), inference(avatar_split_clause, [], [f3747, f344, f276])).
fof(f3747, plain, (~ (e1 = op(e3, e0)) | ~ spl3_30), inference(backward_demodulation, [], [f114, f346])).
fof(f114, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3736, plain, (~ spl3_38 | ~ spl3_42), inference(avatar_split_clause, [], [f3727, f395, f378])).
fof(f3727, plain, (~ (e1 = op(e1, e2)) | ~ spl3_42), inference(backward_demodulation, [], [f142, f397])).
fof(f142, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3724, plain, (~ spl3_18 | ~ spl3_50), inference(avatar_split_clause, [], [f3720, f429, f293])).
fof(f3720, plain, (~ (e1 = op(e2, e3)) | ~ spl3_50), inference(backward_demodulation, [], [f128, f431])).
fof(f128, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3709, plain, (~ spl3_56 | ~ spl3_60), inference(avatar_split_clause, [], [f3705, f471, f454])).
fof(f3705, plain, (~ (e3 = op(e0, e2)) | ~ spl3_60), inference(backward_demodulation, [], [f136, f473])).
fof(f3708, plain, (~ spl3_28 | ~ spl3_60), inference(avatar_split_clause, [], [f3703, f471, f335])).
fof(f3703, plain, (~ (e3 = op(e2, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f116, f473])).
fof(f3701, plain, (spl3_24 | ~ spl3_63 | ~ spl3_107), inference(avatar_split_clause, [], [f3698, f743, f484, f318])).
fof(f3698, plain, ((e3 = op(e2, e2)) | (~ spl3_63 | ~ spl3_107)), inference(backward_demodulation, [], [f744, f486])).
fof(f744, plain, ((e3 = op(op(e0, e0), op(e0, e0))) | ~ spl3_107), inference(avatar_component_clause, [], [f743])).
fof(f3700, plain, (~ spl3_51 | ~ spl3_63), inference(avatar_split_clause, [], [f3694, f484, f433])).
fof(f3694, plain, (~ (e2 = op(e0, e3)) | ~ spl3_63), inference(backward_demodulation, [], [f135, f486])).
fof(f3699, plain, (~ spl3_31 | ~ spl3_63), inference(avatar_split_clause, [], [f3690, f484, f348])).
fof(f348, plain, (spl3_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_31])])).
fof(f3690, plain, (~ (e2 = op(e2, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f110, f486])).
fof(f110, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3686, plain, (~ spl3_50 | spl3_104 | ~ spl3_107), inference(avatar_split_clause, [], [f3685, f743, f727, f429])).
fof(f727, plain, (spl3_104 <=> (e1 = op(e0, op(op(e0, e0), op(e0, e0))))), introduced(avatar_definition, [new_symbols(naming, [spl3_104])])).
fof(f3685, plain, (~ (e1 = op(e0, e3)) | (spl3_104 | ~ spl3_107)), inference(backward_demodulation, [], [f729, f744])).
fof(f729, plain, (~ (e1 = op(e0, op(op(e0, e0), op(e0, e0)))) | spl3_104), inference(avatar_component_clause, [], [f727])).
fof(f3681, plain, (~ spl3_52 | ~ spl3_48 | spl3_74), inference(avatar_split_clause, [], [f3490, f568, f420, f437])).
fof(f437, plain, (spl3_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_52])])).
fof(f568, plain, (spl3_74 <=> (op(e1, e0) = op(e0, op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_74])])).
fof(f3490, plain, (~ (e3 = op(e0, e3)) | (~ spl3_48 | spl3_74)), inference(backward_demodulation, [], [f570, f422])).
fof(f570, plain, (~ (op(e1, e0) = op(e0, op(e1, e0))) | spl3_74), inference(avatar_component_clause, [], [f568])).
fof(f3680, plain, (~ spl3_20 | ~ spl3_4), inference(avatar_split_clause, [], [f3679, f233, f301])).
fof(f301, plain, (spl3_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_20])])).
fof(f3679, plain, (~ (e3 = op(e2, e3)) | ~ spl3_4), inference(forward_demodulation, [], [f132, f235])).
fof(f132, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3662, plain, (~ spl3_6 | ~ spl3_38), inference(avatar_split_clause, [], [f3659, f378, f242])).
fof(f3659, plain, (~ (e1 = op(e3, e2)) | ~ spl3_38), inference(backward_demodulation, [], [f125, f380])).
fof(f3654, plain, (~ spl3_21 | ~ spl3_43 | ~ spl3_48 | spl3_90), inference(avatar_contradiction_clause, [], [f3653])).
fof(f3653, plain, ($false | (~ spl3_21 | ~ spl3_43 | ~ spl3_48 | spl3_90)), inference(subsumption_resolution, [], [f3652, f422])).
fof(f3652, plain, (~ (e3 = op(e1, e0)) | (~ spl3_21 | ~ spl3_43 | spl3_90)), inference(forward_demodulation, [], [f3643, f308])).
fof(f308, plain, ((e0 = op(e2, e2)) | ~ spl3_21), inference(avatar_component_clause, [], [f306])).
fof(f306, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f3643, plain, (~ (e3 = op(e1, op(e2, e2))) | (~ spl3_43 | spl3_90)), inference(backward_demodulation, [], [f659, f401])).
fof(f3651, plain, (~ spl3_21 | ~ spl3_43 | spl3_89), inference(avatar_contradiction_clause, [], [f3650])).
fof(f3650, plain, ($false | (~ spl3_21 | ~ spl3_43 | spl3_89)), inference(subsumption_resolution, [], [f3642, f308])).
fof(f3642, plain, (~ (e0 = op(e2, e2)) | (~ spl3_43 | spl3_89)), inference(backward_demodulation, [], [f655, f401])).
fof(f655, plain, (~ (e0 = op(op(e1, e1), op(e1, e1))) | spl3_89), inference(avatar_component_clause, [], [f653])).
fof(f653, plain, (spl3_89 <=> (e0 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_89])])).
fof(f3647, plain, (~ spl3_11 | ~ spl3_43), inference(avatar_split_clause, [], [f3638, f399, f263])).
fof(f3638, plain, (~ (e2 = op(e3, e1)) | ~ spl3_43), inference(backward_demodulation, [], [f119, f401])).
fof(f119, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3637, plain, (~ spl3_42 | ~ spl3_58), inference(avatar_split_clause, [], [f3635, f463, f395])).
fof(f3635, plain, (~ (e1 = op(e1, e1)) | ~ spl3_58), inference(backward_demodulation, [], [f115, f465])).
fof(f115, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3609, plain, (~ spl3_13 | ~ spl3_33 | spl3_71), inference(avatar_split_clause, [], [f3608, f553, f357, f272])).
fof(f3608, plain, (~ (e0 = op(e3, e0)) | (~ spl3_33 | spl3_71)), inference(forward_demodulation, [], [f555, f359])).
fof(f3603, plain, (~ spl3_62 | ~ spl3_21 | spl3_94), inference(avatar_split_clause, [], [f3602, f677, f306, f480])).
fof(f677, plain, (spl3_94 <=> (e1 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_94])])).
fof(f3602, plain, (~ (op(e0, e0) = e1) | (~ spl3_21 | spl3_94)), inference(forward_demodulation, [], [f679, f308])).
fof(f679, plain, (~ (e1 = op(op(e2, e2), op(e2, e2))) | spl3_94), inference(avatar_component_clause, [], [f677])).
fof(f3596, plain, (~ spl3_64 | ~ spl3_48), inference(avatar_split_clause, [], [f3595, f420, f488])).
fof(f488, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f3595, plain, (~ (op(e0, e0) = e3) | ~ spl3_48), inference(forward_demodulation, [], [f109, f422])).
fof(f109, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f3588, plain, (~ spl3_44 | ~ spl3_48), inference(avatar_split_clause, [], [f3587, f420, f403])).
fof(f3587, plain, (~ (e3 = op(e1, e1)) | ~ spl3_48), inference(forward_demodulation, [], [f139, f422])).
fof(f3576, plain, (~ spl3_2 | ~ spl3_6), inference(avatar_split_clause, [], [f3572, f242, f225])).
fof(f225, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f3572, plain, (~ (e1 = op(e3, e3)) | ~ spl3_6), inference(backward_demodulation, [], [f156, f244])).
fof(f244, plain, ((e1 = op(e3, e2)) | ~ spl3_6), inference(avatar_component_clause, [], [f242])).
fof(f156, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3545, plain, (~ spl3_20 | ~ spl3_28), inference(avatar_split_clause, [], [f3542, f335, f301])).
fof(f3542, plain, (~ (e3 = op(e2, e3)) | ~ spl3_28), inference(backward_demodulation, [], [f149, f337])).
fof(f337, plain, ((e3 = op(e2, e1)) | ~ spl3_28), inference(avatar_component_clause, [], [f335])).
fof(f3544, plain, (~ spl3_12 | ~ spl3_28), inference(avatar_split_clause, [], [f3540, f335, f267])).
fof(f267, plain, (spl3_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_12])])).
fof(f3540, plain, (~ (e3 = op(e3, e1)) | ~ spl3_28), inference(backward_demodulation, [], [f120, f337])).
fof(f3539, plain, (~ spl3_27 | ~ spl3_31), inference(avatar_split_clause, [], [f3533, f348, f331])).
fof(f3533, plain, (~ (e2 = op(e2, e1)) | ~ spl3_31), inference(backward_demodulation, [], [f145, f350])).
fof(f350, plain, ((e2 = op(e2, e0)) | ~ spl3_31), inference(avatar_component_clause, [], [f348])).
fof(f3529, plain, (~ spl3_28 | spl3_87 | ~ spl3_94), inference(avatar_split_clause, [], [f3527, f677, f643, f335])).
fof(f643, plain, (spl3_87 <=> (e3 = op(e2, op(op(e2, e2), op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl3_87])])).
fof(f3527, plain, (~ (e3 = op(e2, e1)) | (spl3_87 | ~ spl3_94)), inference(backward_demodulation, [], [f645, f678])).
fof(f678, plain, ((e1 = op(op(e2, e2), op(e2, e2))) | ~ spl3_94), inference(avatar_component_clause, [], [f677])).
fof(f645, plain, (~ (e3 = op(e2, op(op(e2, e2), op(e2, e2)))) | spl3_87), inference(avatar_component_clause, [], [f643])).
fof(f3526, plain, (~ spl3_23 | ~ spl3_39), inference(avatar_split_clause, [], [f3525, f382, f314])).
fof(f382, plain, (spl3_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_39])])).
fof(f3525, plain, (~ (e2 = op(e2, e2)) | ~ spl3_39), inference(forward_demodulation, [], [f124, f384])).
fof(f384, plain, ((e2 = op(e1, e2)) | ~ spl3_39), inference(avatar_component_clause, [], [f382])).
fof(f3513, plain, (~ spl3_13 | ~ spl3_29), inference(avatar_split_clause, [], [f3507, f340, f272])).
fof(f3507, plain, (~ (e0 = op(e3, e0)) | ~ spl3_29), inference(backward_demodulation, [], [f114, f342])).
fof(f3470, plain, (~ spl3_20 | ~ spl3_56 | spl3_77), inference(avatar_split_clause, [], [f3400, f582, f454, f301])).
fof(f582, plain, (spl3_77 <=> (op(e0, e2) = op(e2, op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_77])])).
fof(f3400, plain, (~ (e3 = op(e2, e3)) | (~ spl3_56 | spl3_77)), inference(backward_demodulation, [], [f584, f456])).
fof(f584, plain, (~ (op(e0, e2) = op(e2, op(e0, e2))) | spl3_77), inference(avatar_component_clause, [], [f582])).
fof(f3441, plain, (~ spl3_1 | ~ spl3_17), inference(avatar_split_clause, [], [f3436, f289, f221])).
fof(f221, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f3436, plain, (~ (e0 = op(e3, e3)) | ~ spl3_17), inference(backward_demodulation, [], [f132, f291])).
fof(f3413, plain, (~ spl3_16 | ~ spl3_32), inference(avatar_split_clause, [], [f3408, f352, f284])).
fof(f284, plain, (spl3_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_16])])).
fof(f3408, plain, (~ (e3 = op(e3, e0)) | ~ spl3_32), inference(backward_demodulation, [], [f114, f354])).
fof(f3406, plain, (~ spl3_19 | ~ spl3_51), inference(avatar_split_clause, [], [f3402, f433, f297])).
fof(f3402, plain, (~ (e2 = op(e2, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f128, f435])).
fof(f3401, plain, (~ spl3_24 | ~ spl3_56), inference(avatar_split_clause, [], [f3397, f454, f318])).
fof(f3397, plain, (~ (e3 = op(e2, e2)) | ~ spl3_56), inference(backward_demodulation, [], [f122, f456])).
fof(f122, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3396, plain, (~ spl3_25 | ~ spl3_57), inference(avatar_split_clause, [], [f3389, f459, f323])).
fof(f3389, plain, (~ (e0 = op(e2, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f116, f461])).
fof(f3385, plain, (~ spl3_30 | ~ spl3_62), inference(avatar_split_clause, [], [f3376, f480, f344])).
fof(f3376, plain, (~ (e1 = op(e2, e0)) | ~ spl3_62), inference(backward_demodulation, [], [f110, f482])).
fof(f3375, plain, (~ spl3_60 | ~ spl3_96 | spl3_97), inference(avatar_split_clause, [], [f3372, f691, f687, f471])).
fof(f3372, plain, (~ (e3 = op(e0, e1)) | (~ spl3_96 | spl3_97)), inference(backward_demodulation, [], [f693, f688])).
fof(f688, plain, ((e1 = op(op(e0, e0), op(e0, e0))) | ~ spl3_96), inference(avatar_component_clause, [], [f687])).
fof(f3369, plain, (~ spl3_55 | ~ spl3_47 | spl3_74), inference(avatar_split_clause, [], [f3312, f568, f416, f450])).
fof(f450, plain, (spl3_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_55])])).
fof(f3312, plain, (~ (e2 = op(e0, e2)) | (~ spl3_47 | spl3_74)), inference(backward_demodulation, [], [f570, f418])).
fof(f3367, plain, (~ spl3_31 | ~ spl3_47), inference(avatar_split_clause, [], [f3366, f416, f348])).
fof(f3366, plain, (~ (e2 = op(e2, e0)) | ~ spl3_47), inference(forward_demodulation, [], [f112, f418])).
fof(f3346, plain, (~ spl3_12 | ~ spl3_16), inference(avatar_split_clause, [], [f3342, f284, f267])).
fof(f3342, plain, (~ (e3 = op(e3, e1)) | ~ spl3_16), inference(backward_demodulation, [], [f151, f286])).
fof(f286, plain, ((e3 = op(e3, e0)) | ~ spl3_16), inference(avatar_component_clause, [], [f284])).
fof(f3338, plain, (~ spl3_1 | ~ spl3_24 | spl3_86), inference(avatar_split_clause, [], [f3332, f639, f318, f221])).
fof(f639, plain, (spl3_86 <=> (e0 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_86])])).
fof(f3332, plain, (~ (e0 = op(e3, e3)) | (~ spl3_24 | spl3_86)), inference(backward_demodulation, [], [f641, f320])).
fof(f320, plain, ((e3 = op(e2, e2)) | ~ spl3_24), inference(avatar_component_clause, [], [f318])).
fof(f641, plain, (~ (e0 = op(op(e2, e2), op(e2, e2))) | spl3_86), inference(avatar_component_clause, [], [f639])).
fof(f3328, plain, (~ spl3_20 | ~ spl3_36), inference(avatar_split_clause, [], [f3325, f369, f301])).
fof(f3325, plain, (~ (e3 = op(e2, e3)) | ~ spl3_36), inference(backward_demodulation, [], [f130, f371])).
fof(f371, plain, ((e3 = op(e1, e3)) | ~ spl3_36), inference(avatar_component_clause, [], [f369])).
fof(f3314, plain, (~ spl3_35 | ~ spl3_47), inference(avatar_split_clause, [], [f3310, f416, f365])).
fof(f3310, plain, (~ (e2 = op(e1, e3)) | ~ spl3_47), inference(backward_demodulation, [], [f141, f418])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3307, plain, (~ spl3_2 | ~ spl3_50), inference(avatar_split_clause, [], [f3303, f429, f225])).
fof(f3303, plain, (~ (e1 = op(e3, e3)) | ~ spl3_50), inference(backward_demodulation, [], [f129, f431])).
fof(f129, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3299, plain, (~ spl3_23 | ~ spl3_55), inference(avatar_split_clause, [], [f3294, f450, f314])).
fof(f3294, plain, (~ (e2 = op(e2, e2)) | ~ spl3_55), inference(backward_demodulation, [], [f122, f452])).
fof(f452, plain, ((e2 = op(e0, e2)) | ~ spl3_55), inference(avatar_component_clause, [], [f450])).
fof(f3291, plain, (~ spl3_12 | ~ spl3_60), inference(avatar_split_clause, [], [f3287, f471, f267])).
fof(f3287, plain, (~ (e3 = op(e3, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f117, f473])).
fof(f3280, plain, (~ spl3_49 | ~ spl3_61), inference(avatar_split_clause, [], [f3270, f476, f425])).
fof(f425, plain, (spl3_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_49])])).
fof(f3270, plain, (~ (e0 = op(e0, e3)) | ~ spl3_61), inference(backward_demodulation, [], [f135, f478])).
fof(f3279, plain, (~ spl3_45 | ~ spl3_61), inference(avatar_split_clause, [], [f3266, f476, f408])).
fof(f3266, plain, (~ (e0 = op(e1, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f109, f478])).
fof(f3252, plain, (~ spl3_2 | ~ spl3_59 | ~ spl3_64 | spl3_99), inference(avatar_contradiction_clause, [], [f3251])).
fof(f3251, plain, ($false | (~ spl3_2 | ~ spl3_59 | ~ spl3_64 | spl3_99)), inference(subsumption_resolution, [], [f3250, f469])).
fof(f469, plain, ((e2 = op(e0, e1)) | ~ spl3_59), inference(avatar_component_clause, [], [f467])).
fof(f467, plain, (spl3_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_59])])).
fof(f3250, plain, (~ (e2 = op(e0, e1)) | (~ spl3_2 | ~ spl3_64 | spl3_99)), inference(forward_demodulation, [], [f3249, f227])).
fof(f227, plain, ((e1 = op(e3, e3)) | ~ spl3_2), inference(avatar_component_clause, [], [f225])).
fof(f3249, plain, (~ (e2 = op(e0, op(e3, e3))) | (~ spl3_64 | spl3_99)), inference(forward_demodulation, [], [f703, f490])).
fof(f490, plain, ((op(e0, e0) = e3) | ~ spl3_64), inference(avatar_component_clause, [], [f488])).
fof(f3231, plain, (~ spl3_8 | ~ spl3_12), inference(avatar_split_clause, [], [f3228, f267, f250])).
fof(f250, plain, (spl3_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_8])])).
fof(f3228, plain, (~ (e3 = op(e3, e2)) | ~ spl3_12), inference(backward_demodulation, [], [f154, f269])).
fof(f269, plain, ((e3 = op(e3, e1)) | ~ spl3_12), inference(avatar_component_clause, [], [f267])).
fof(f154, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3226, plain, (~ spl3_3 | ~ spl3_15), inference(avatar_split_clause, [], [f3223, f280, f229])).
fof(f229, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f3223, plain, (~ (e2 = op(e3, e3)) | ~ spl3_15), inference(backward_demodulation, [], [f153, f282])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3206, plain, (~ spl3_25 | ~ spl3_45 | spl3_68), inference(avatar_contradiction_clause, [], [f3205])).
fof(f3205, plain, ($false | (~ spl3_25 | ~ spl3_45 | spl3_68)), inference(subsumption_resolution, [], [f3202, f410])).
fof(f3202, plain, (~ (e0 = op(e1, e0)) | (~ spl3_25 | spl3_68)), inference(backward_demodulation, [], [f541, f325])).
fof(f541, plain, (~ (op(e2, e1) = op(e1, op(e2, e1))) | spl3_68), inference(avatar_component_clause, [], [f539])).
fof(f539, plain, (spl3_68 <=> (op(e2, e1) = op(e1, op(e2, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_68])])).
fof(f3195, plain, (~ spl3_22 | ~ spl3_30), inference(avatar_split_clause, [], [f3189, f344, f310])).
fof(f3189, plain, (~ (e1 = op(e2, e2)) | ~ spl3_30), inference(backward_demodulation, [], [f146, f346])).
fof(f3180, plain, (~ spl3_36 | ~ spl3_40), inference(avatar_split_clause, [], [f3177, f386, f369])).
fof(f3177, plain, (~ (e3 = op(e1, e3)) | ~ spl3_40), inference(backward_demodulation, [], [f144, f388])).
fof(f388, plain, ((e3 = op(e1, e2)) | ~ spl3_40), inference(avatar_component_clause, [], [f386])).
fof(f3179, plain, (~ spl3_8 | ~ spl3_40), inference(avatar_split_clause, [], [f3176, f386, f250])).
fof(f3176, plain, (~ (e3 = op(e3, e2)) | ~ spl3_40), inference(backward_demodulation, [], [f125, f388])).
fof(f3163, plain, (~ spl3_17 | ~ spl3_49), inference(avatar_split_clause, [], [f3157, f425, f289])).
fof(f3157, plain, (~ (e0 = op(e2, e3)) | ~ spl3_49), inference(backward_demodulation, [], [f128, f427])).
fof(f427, plain, ((e0 = op(e0, e3)) | ~ spl3_49), inference(avatar_component_clause, [], [f425])).
fof(f3155, plain, (~ spl3_50 | ~ spl3_54), inference(avatar_split_clause, [], [f3150, f446, f429])).
fof(f446, plain, (spl3_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_54])])).
fof(f3150, plain, (~ (e1 = op(e0, e3)) | ~ spl3_54), inference(backward_demodulation, [], [f138, f448])).
fof(f448, plain, ((e1 = op(e0, e2)) | ~ spl3_54), inference(avatar_component_clause, [], [f446])).
fof(f138, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3154, plain, (~ spl3_22 | ~ spl3_54), inference(avatar_split_clause, [], [f3148, f446, f310])).
fof(f3148, plain, (~ (e1 = op(e2, e2)) | ~ spl3_54), inference(backward_demodulation, [], [f122, f448])).
fof(f3144, plain, (~ spl3_55 | ~ spl3_59), inference(avatar_split_clause, [], [f3139, f467, f450])).
fof(f3139, plain, (~ (e2 = op(e0, e2)) | ~ spl3_59), inference(backward_demodulation, [], [f136, f469])).
fof(f3143, plain, (~ spl3_27 | ~ spl3_59), inference(avatar_split_clause, [], [f3137, f467, f331])).
fof(f3137, plain, (~ (e2 = op(e2, e1)) | ~ spl3_59), inference(backward_demodulation, [], [f116, f469])).
fof(f3136, plain, (~ spl3_2 | ~ spl3_64 | spl3_96), inference(avatar_split_clause, [], [f3128, f687, f488, f225])).
fof(f3128, plain, (~ (e1 = op(e3, e3)) | (~ spl3_64 | spl3_96)), inference(backward_demodulation, [], [f689, f490])).
fof(f3133, plain, (~ spl3_60 | ~ spl3_64), inference(avatar_split_clause, [], [f3124, f488, f471])).
fof(f3124, plain, (~ (e3 = op(e0, e1)) | ~ spl3_64), inference(backward_demodulation, [], [f133, f490])).
fof(f3132, plain, (~ spl3_32 | ~ spl3_64), inference(avatar_split_clause, [], [f3122, f488, f352])).
fof(f3122, plain, (~ (e3 = op(e2, e0)) | ~ spl3_64), inference(backward_demodulation, [], [f110, f490])).
fof(f3116, plain, (~ spl3_3 | ~ spl3_22 | spl3_93), inference(avatar_contradiction_clause, [], [f3115])).
fof(f3115, plain, ($false | (~ spl3_3 | ~ spl3_22 | spl3_93)), inference(subsumption_resolution, [], [f3114, f312])).
fof(f3114, plain, (~ (e1 = op(e2, e2)) | (~ spl3_3 | spl3_93)), inference(forward_demodulation, [], [f674, f231])).
fof(f231, plain, ((e2 = op(e3, e3)) | ~ spl3_3), inference(avatar_component_clause, [], [f229])).
fof(f674, plain, (~ (e1 = op(op(e3, e3), op(e3, e3))) | spl3_93), inference(avatar_component_clause, [], [f672])).
fof(f672, plain, (spl3_93 <=> (e1 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_93])])).
fof(f3100, plain, (~ spl3_3 | ~ spl3_9 | ~ spl3_22 | spl3_95), inference(avatar_contradiction_clause, [], [f3099])).
fof(f3099, plain, ($false | (~ spl3_3 | ~ spl3_9 | ~ spl3_22 | spl3_95)), inference(subsumption_resolution, [], [f3094, f257])).
fof(f257, plain, ((e0 = op(e3, e1)) | ~ spl3_9), inference(avatar_component_clause, [], [f255])).
fof(f3094, plain, (~ (e0 = op(e3, e1)) | (~ spl3_3 | ~ spl3_22 | spl3_95)), inference(backward_demodulation, [], [f2999, f312])).
fof(f2999, plain, (~ (e0 = op(e3, op(e2, e2))) | (~ spl3_3 | spl3_95)), inference(backward_demodulation, [], [f684, f231])).
fof(f684, plain, (~ (e0 = op(e3, op(op(e3, e3), op(e3, e3)))) | spl3_95), inference(avatar_component_clause, [], [f682])).
fof(f682, plain, (spl3_95 <=> (e0 = op(e3, op(op(e3, e3), op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl3_95])])).
fof(f3095, plain, (~ spl3_18 | ~ spl3_22), inference(avatar_split_clause, [], [f3089, f310, f293])).
fof(f3089, plain, (~ (e1 = op(e2, e3)) | ~ spl3_22), inference(backward_demodulation, [], [f150, f312])).
fof(f150, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3066, plain, (~ spl3_39 | ~ spl3_47), inference(avatar_split_clause, [], [f3061, f416, f382])).
fof(f3061, plain, (~ (e2 = op(e1, e2)) | ~ spl3_47), inference(backward_demodulation, [], [f140, f418])).
fof(f3045, plain, (~ spl3_52 | ~ spl3_60), inference(avatar_split_clause, [], [f3042, f471, f437])).
fof(f3042, plain, (~ (e3 = op(e0, e3)) | ~ spl3_60), inference(backward_demodulation, [], [f137, f473])).
fof(f137, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3014, plain, (~ spl3_32 | ~ spl3_86 | spl3_87), inference(avatar_split_clause, [], [f3013, f643, f639, f352])).
fof(f3013, plain, (~ (e3 = op(e2, e0)) | (~ spl3_86 | spl3_87)), inference(forward_demodulation, [], [f645, f640])).
fof(f640, plain, ((e0 = op(op(e2, e2), op(e2, e2))) | ~ spl3_86), inference(avatar_component_clause, [], [f639])).
fof(f3010, plain, (~ spl3_21 | ~ spl3_3 | spl3_84), inference(avatar_split_clause, [], [f2997, f630, f229, f306])).
fof(f630, plain, (spl3_84 <=> (e0 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_84])])).
fof(f2997, plain, (~ (e0 = op(e2, e2)) | (~ spl3_3 | spl3_84)), inference(backward_demodulation, [], [f632, f231])).
fof(f632, plain, (~ (e0 = op(op(e3, e3), op(e3, e3))) | spl3_84), inference(avatar_component_clause, [], [f630])).
fof(f3009, plain, (~ spl3_23 | ~ spl3_3 | spl3_100), inference(avatar_split_clause, [], [f3000, f706, f229, f314])).
fof(f706, plain, (spl3_100 <=> (e2 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_100])])).
fof(f3000, plain, (~ (e2 = op(e2, e2)) | (~ spl3_3 | spl3_100)), inference(backward_demodulation, [], [f708, f231])).
fof(f708, plain, (~ (e2 = op(op(e3, e3), op(e3, e3))) | spl3_100), inference(avatar_component_clause, [], [f706])).
fof(f3007, plain, (~ spl3_20 | ~ spl3_8 | spl3_81), inference(avatar_split_clause, [], [f2993, f615, f250, f301])).
fof(f615, plain, (spl3_81 <=> (op(e3, e2) = op(e2, op(e3, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_81])])).
fof(f2993, plain, (~ (e3 = op(e2, e3)) | (~ spl3_8 | spl3_81)), inference(backward_demodulation, [], [f617, f252])).
fof(f252, plain, ((e3 = op(e3, e2)) | ~ spl3_8), inference(avatar_component_clause, [], [f250])).
fof(f617, plain, (~ (op(e3, e2) = op(e2, op(e3, e2))) | spl3_81), inference(avatar_component_clause, [], [f615])).
fof(f2991, plain, (~ spl3_1 | ~ spl3_9), inference(avatar_split_clause, [], [f2986, f255, f221])).
fof(f2986, plain, (~ (e0 = op(e3, e3)) | ~ spl3_9), inference(backward_demodulation, [], [f155, f257])).
fof(f155, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2963, plain, (~ spl3_16 | ~ spl3_48), inference(avatar_split_clause, [], [f2959, f420, f284])).
fof(f2959, plain, (~ (e3 = op(e3, e0)) | ~ spl3_48), inference(backward_demodulation, [], [f113, f422])).
fof(f2958, plain, (~ spl3_36 | ~ spl3_52), inference(avatar_split_clause, [], [f2956, f437, f369])).
fof(f2956, plain, (~ (e3 = op(e1, e3)) | ~ spl3_52), inference(backward_demodulation, [], [f127, f439])).
fof(f439, plain, ((e3 = op(e0, e3)) | ~ spl3_52), inference(avatar_component_clause, [], [f437])).
fof(f2949, plain, (~ spl3_39 | ~ spl3_59 | spl3_78), inference(avatar_contradiction_clause, [], [f2948])).
fof(f2948, plain, ($false | (~ spl3_39 | ~ spl3_59 | spl3_78)), inference(subsumption_resolution, [], [f2945, f384])).
fof(f2945, plain, (~ (e2 = op(e1, e2)) | (~ spl3_59 | spl3_78)), inference(backward_demodulation, [], [f589, f469])).
fof(f2947, plain, (~ spl3_51 | ~ spl3_59), inference(avatar_split_clause, [], [f2943, f467, f433])).
fof(f2943, plain, (~ (e2 = op(e0, e3)) | ~ spl3_59), inference(backward_demodulation, [], [f137, f469])).
fof(f2946, plain, (~ spl3_11 | ~ spl3_59), inference(avatar_split_clause, [], [f2941, f467, f263])).
fof(f2941, plain, (~ (e2 = op(e3, e1)) | ~ spl3_59), inference(backward_demodulation, [], [f117, f469])).
fof(f2939, plain, (~ spl3_36 | ~ spl3_28 | spl3_68), inference(avatar_split_clause, [], [f2938, f539, f335, f369])).
fof(f2938, plain, (~ (e3 = op(e1, e3)) | (~ spl3_28 | spl3_68)), inference(forward_demodulation, [], [f541, f337])).
fof(f2914, plain, (~ spl3_11 | ~ spl3_39 | spl3_82), inference(avatar_contradiction_clause, [], [f2913])).
fof(f2913, plain, ($false | (~ spl3_11 | ~ spl3_39 | spl3_82)), inference(subsumption_resolution, [], [f2912, f384])).
fof(f2912, plain, (~ (e2 = op(e1, e2)) | (~ spl3_11 | spl3_82)), inference(backward_demodulation, [], [f622, f265])).
fof(f265, plain, ((e2 = op(e3, e1)) | ~ spl3_11), inference(avatar_component_clause, [], [f263])).
fof(f622, plain, (~ (op(e3, e1) = op(e1, op(e3, e1))) | spl3_82), inference(avatar_component_clause, [], [f620])).
fof(f620, plain, (spl3_82 <=> (op(e3, e1) = op(e1, op(e3, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_82])])).
fof(f2881, plain, (~ spl3_24 | ~ spl3_28), inference(avatar_split_clause, [], [f2877, f335, f318])).
fof(f2877, plain, (~ (e3 = op(e2, e2)) | ~ spl3_28), inference(backward_demodulation, [], [f148, f337])).
fof(f2875, plain, (~ spl3_19 | ~ spl3_31), inference(avatar_split_clause, [], [f2871, f348, f297])).
fof(f2871, plain, (~ (e2 = op(e2, e3)) | ~ spl3_31), inference(backward_demodulation, [], [f147, f350])).
fof(f2860, plain, (~ spl3_33 | ~ spl3_41), inference(avatar_split_clause, [], [f2853, f391, f357])).
fof(f391, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f2853, plain, (~ (e0 = op(e1, e3)) | ~ spl3_41), inference(backward_demodulation, [], [f143, f393])).
fof(f393, plain, ((e0 = op(e1, e1)) | ~ spl3_41), inference(avatar_component_clause, [], [f391])).
fof(f2859, plain, (~ spl3_25 | ~ spl3_41), inference(avatar_split_clause, [], [f2850, f391, f323])).
fof(f2850, plain, (~ (e0 = op(e2, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f118, f393])).
fof(f2849, plain, (~ spl3_38 | ~ spl3_46), inference(avatar_split_clause, [], [f2843, f412, f378])).
fof(f412, plain, (spl3_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_46])])).
fof(f2843, plain, (~ (e1 = op(e1, e2)) | ~ spl3_46), inference(backward_demodulation, [], [f140, f414])).
fof(f414, plain, ((e1 = op(e1, e0)) | ~ spl3_46), inference(avatar_component_clause, [], [f412])).
fof(f2848, plain, (~ spl3_30 | ~ spl3_46), inference(avatar_split_clause, [], [f2840, f412, f344])).
fof(f2840, plain, (~ (e1 = op(e2, e0)) | ~ spl3_46), inference(backward_demodulation, [], [f112, f414])).
fof(f2834, plain, (~ spl3_52 | ~ spl3_56), inference(avatar_split_clause, [], [f2832, f454, f437])).
fof(f2832, plain, (~ (e3 = op(e0, e3)) | ~ spl3_56), inference(backward_demodulation, [], [f138, f456])).
fof(f2815, plain, (~ spl3_2 | ~ spl3_24 | ~ spl3_25 | spl3_98), inference(avatar_contradiction_clause, [], [f2814])).
fof(f2814, plain, ($false | (~ spl3_2 | ~ spl3_24 | ~ spl3_25 | spl3_98)), inference(subsumption_resolution, [], [f2809, f325])).
fof(f2809, plain, (~ (e0 = op(e2, e1)) | (~ spl3_2 | ~ spl3_24 | spl3_98)), inference(backward_demodulation, [], [f2771, f227])).
fof(f2771, plain, (~ (e0 = op(e2, op(e3, e3))) | (~ spl3_24 | spl3_98)), inference(backward_demodulation, [], [f698, f320])).
fof(f2812, plain, (~ spl3_2 | ~ spl3_43 | spl3_100), inference(avatar_contradiction_clause, [], [f2811])).
fof(f2811, plain, ($false | (~ spl3_2 | ~ spl3_43 | spl3_100)), inference(subsumption_resolution, [], [f2807, f401])).
fof(f2807, plain, (~ (e2 = op(e1, e1)) | (~ spl3_2 | spl3_100)), inference(backward_demodulation, [], [f708, f227])).
fof(f2785, plain, (~ spl3_3 | ~ spl3_19), inference(avatar_split_clause, [], [f2784, f297, f229])).
fof(f2784, plain, (~ (e2 = op(e3, e3)) | ~ spl3_19), inference(backward_demodulation, [], [f132, f299])).
fof(f2780, plain, (~ spl3_24 | ~ spl3_33 | ~ spl3_43 | spl3_103), inference(avatar_contradiction_clause, [], [f2779])).
fof(f2779, plain, ($false | (~ spl3_24 | ~ spl3_33 | ~ spl3_43 | spl3_103)), inference(subsumption_resolution, [], [f2773, f359])).
fof(f2773, plain, (~ (e0 = op(e1, e3)) | (~ spl3_24 | ~ spl3_43 | spl3_103)), inference(backward_demodulation, [], [f2718, f320])).
fof(f2718, plain, (~ (e0 = op(e1, op(e2, e2))) | (~ spl3_43 | spl3_103)), inference(forward_demodulation, [], [f724, f401])).
fof(f724, plain, (~ (e0 = op(e1, op(op(e1, e1), op(e1, e1)))) | spl3_103), inference(avatar_component_clause, [], [f722])).
fof(f722, plain, (spl3_103 <=> (e0 = op(e1, op(op(e1, e1), op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl3_103])])).
fof(f2778, plain, (~ spl3_2 | ~ spl3_24 | spl3_94), inference(avatar_split_clause, [], [f2770, f677, f318, f225])).
fof(f2770, plain, (~ (e1 = op(e3, e3)) | (~ spl3_24 | spl3_94)), inference(backward_demodulation, [], [f679, f320])).
fof(f2775, plain, (~ spl3_8 | ~ spl3_24), inference(avatar_split_clause, [], [f2765, f318, f250])).
fof(f2765, plain, (~ (e3 = op(e3, e2)) | ~ spl3_24), inference(backward_demodulation, [], [f126, f320])).
fof(f2764, plain, (~ spl3_20 | ~ spl3_52), inference(avatar_split_clause, [], [f2762, f437, f301])).
fof(f2762, plain, (~ (e3 = op(e2, e3)) | ~ spl3_52), inference(backward_demodulation, [], [f128, f439])).
fof(f2754, plain, (~ spl3_50 | ~ spl3_58), inference(avatar_split_clause, [], [f2749, f463, f429])).
fof(f2749, plain, (~ (e1 = op(e0, e3)) | ~ spl3_58), inference(backward_demodulation, [], [f137, f465])).
fof(f2753, plain, (~ spl3_10 | ~ spl3_58), inference(avatar_split_clause, [], [f2747, f463, f259])).
fof(f2747, plain, (~ (e1 = op(e3, e1)) | ~ spl3_58), inference(backward_demodulation, [], [f117, f465])).
fof(f2740, plain, (~ spl3_53 | ~ spl3_61), inference(avatar_split_clause, [], [f2729, f476, f442])).
fof(f2729, plain, (~ (e0 = op(e0, e2)) | ~ spl3_61), inference(backward_demodulation, [], [f134, f478])).
fof(f134, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2720, plain, (~ spl3_24 | ~ spl3_43 | spl3_106), inference(avatar_split_clause, [], [f2719, f737, f399, f318])).
fof(f737, plain, (spl3_106 <=> (e3 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_106])])).
fof(f2719, plain, (~ (e3 = op(e2, e2)) | (~ spl3_43 | spl3_106)), inference(forward_demodulation, [], [f739, f401])).
fof(f739, plain, (~ (e3 = op(op(e1, e1), op(e1, e1))) | spl3_106), inference(avatar_component_clause, [], [f737])).
fof(f2713, plain, (~ spl3_58 | ~ spl3_30 | spl3_69), inference(avatar_split_clause, [], [f2686, f544, f344, f463])).
fof(f544, plain, (spl3_69 <=> (op(e2, e0) = op(e0, op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_69])])).
fof(f2686, plain, (~ (e1 = op(e0, e1)) | (~ spl3_30 | spl3_69)), inference(backward_demodulation, [], [f546, f346])).
fof(f546, plain, (~ (op(e2, e0) = op(e0, op(e2, e0))) | spl3_69), inference(avatar_component_clause, [], [f544])).
fof(f2688, plain, (~ spl3_26 | ~ spl3_30), inference(avatar_split_clause, [], [f2682, f344, f327])).
fof(f2682, plain, (~ (e1 = op(e2, e1)) | ~ spl3_30), inference(backward_demodulation, [], [f145, f346])).
fof(f2656, plain, (~ spl3_10 | ~ spl3_14), inference(avatar_split_clause, [], [f2655, f276, f259])).
fof(f2655, plain, (~ (e1 = op(e3, e1)) | ~ spl3_14), inference(backward_demodulation, [], [f151, f278])).
fof(f2618, plain, (~ spl3_31 | ~ spl3_23), inference(avatar_split_clause, [], [f2617, f314, f348])).
fof(f2617, plain, (~ (e2 = op(e2, e0)) | ~ spl3_23), inference(forward_demodulation, [], [f146, f316])).
fof(f2609, plain, (~ spl3_26 | ~ spl3_38 | spl3_72), inference(avatar_split_clause, [], [f2552, f558, f378, f327])).
fof(f2552, plain, (~ (e1 = op(e2, e1)) | (~ spl3_38 | spl3_72)), inference(backward_demodulation, [], [f560, f380])).
fof(f2604, plain, (~ spl3_11 | ~ spl3_3), inference(avatar_split_clause, [], [f2603, f229, f263])).
fof(f2603, plain, (~ (e2 = op(e3, e1)) | ~ spl3_3), inference(forward_demodulation, [], [f155, f231])).
fof(f2596, plain, (~ spl3_10 | ~ spl3_50 | spl3_76), inference(avatar_split_clause, [], [f2528, f577, f429, f259])).
fof(f2528, plain, (~ (e1 = op(e3, e1)) | (~ spl3_50 | spl3_76)), inference(backward_demodulation, [], [f579, f431])).
fof(f2574, plain, (~ spl3_19 | ~ spl3_23), inference(avatar_split_clause, [], [f2564, f314, f297])).
fof(f2564, plain, (~ (e2 = op(e2, e3)) | ~ spl3_23), inference(backward_demodulation, [], [f150, f316])).
fof(f2553, plain, (~ spl3_34 | ~ spl3_38), inference(avatar_split_clause, [], [f2549, f378, f361])).
fof(f2549, plain, (~ (e1 = op(e1, e3)) | ~ spl3_38), inference(backward_demodulation, [], [f144, f380])).
fof(f2522, plain, (~ spl3_37 | ~ spl3_53), inference(avatar_split_clause, [], [f2514, f442, f374])).
fof(f2514, plain, (~ (e0 = op(e1, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f121, f444])).
fof(f444, plain, ((e0 = op(e0, e2)) | ~ spl3_53), inference(avatar_component_clause, [], [f442])).
fof(f2506, plain, (~ spl3_59 | ~ spl3_63), inference(avatar_split_clause, [], [f2495, f484, f467])).
fof(f2495, plain, (~ (e2 = op(e0, e1)) | ~ spl3_63), inference(backward_demodulation, [], [f133, f486])).
fof(f2477, plain, (~ spl3_1 | ~ spl3_44 | ~ spl3_47 | spl3_92), inference(avatar_contradiction_clause, [], [f2476])).
fof(f2476, plain, ($false | (~ spl3_1 | ~ spl3_44 | ~ spl3_47 | spl3_92)), inference(subsumption_resolution, [], [f2471, f418])).
fof(f2471, plain, (~ (e2 = op(e1, e0)) | (~ spl3_1 | ~ spl3_44 | spl3_92)), inference(backward_demodulation, [], [f2436, f223])).
fof(f223, plain, ((e0 = op(e3, e3)) | ~ spl3_1), inference(avatar_component_clause, [], [f221])).
fof(f2436, plain, (~ (e2 = op(e1, op(e3, e3))) | (~ spl3_44 | spl3_92)), inference(forward_demodulation, [], [f669, f405])).
fof(f669, plain, (~ (e2 = op(e1, op(op(e1, e1), op(e1, e1)))) | spl3_92), inference(avatar_component_clause, [], [f667])).
fof(f667, plain, (spl3_92 <=> (e2 = op(e1, op(op(e1, e1), op(e1, e1))))), introduced(avatar_definition, [new_symbols(naming, [spl3_92])])).
fof(f2475, plain, (~ spl3_1 | ~ spl3_24 | ~ spl3_30 | spl3_91), inference(avatar_contradiction_clause, [], [f2474])).
fof(f2474, plain, ($false | (~ spl3_1 | ~ spl3_24 | ~ spl3_30 | spl3_91)), inference(subsumption_resolution, [], [f2469, f346])).
fof(f2469, plain, (~ (e1 = op(e2, e0)) | (~ spl3_1 | ~ spl3_24 | spl3_91)), inference(backward_demodulation, [], [f2390, f223])).
fof(f2390, plain, (~ (e1 = op(e2, op(e3, e3))) | (~ spl3_24 | spl3_91)), inference(backward_demodulation, [], [f664, f320])).
fof(f664, plain, (~ (e1 = op(e2, op(op(e2, e2), op(e2, e2)))) | spl3_91), inference(avatar_component_clause, [], [f662])).
fof(f662, plain, (spl3_91 <=> (e1 = op(e2, op(op(e2, e2), op(e2, e2))))), introduced(avatar_definition, [new_symbols(naming, [spl3_91])])).
fof(f2456, plain, (~ spl3_8 | ~ spl3_16), inference(avatar_split_clause, [], [f2453, f284, f250])).
fof(f2453, plain, (~ (e3 = op(e3, e2)) | ~ spl3_16), inference(backward_demodulation, [], [f152, f286])).
fof(f2435, plain, (~ spl3_3 | ~ spl3_44 | spl3_101), inference(avatar_split_clause, [], [f2434, f711, f403, f229])).
fof(f2434, plain, (~ (e2 = op(e3, e3)) | (~ spl3_44 | spl3_101)), inference(forward_demodulation, [], [f713, f405])).
fof(f713, plain, (~ (e2 = op(op(e1, e1), op(e1, e1))) | spl3_101), inference(avatar_component_clause, [], [f711])).
fof(f2414, plain, (~ spl3_5 | ~ spl3_8), inference(avatar_contradiction_clause, [], [f2413])).
fof(f2413, plain, ($false | (~ spl3_5 | ~ spl3_8)), inference(subsumption_resolution, [], [f2412, f159])).
fof(f159, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax4)).
fof(f2412, plain, ((e0 = e3) | (~ spl3_5 | ~ spl3_8)), inference(backward_demodulation, [], [f252, f240])).
fof(f240, plain, ((e0 = op(e3, e2)) | ~ spl3_5), inference(avatar_component_clause, [], [f238])).
fof(f2377, plain, (~ spl3_30 | ~ spl3_32), inference(avatar_contradiction_clause, [], [f2376])).
fof(f2376, plain, ($false | (~ spl3_30 | ~ spl3_32)), inference(subsumption_resolution, [], [f2375, f161])).
fof(f161, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2375, plain, ((e1 = e3) | (~ spl3_30 | ~ spl3_32)), inference(backward_demodulation, [], [f354, f346])).
fof(f2373, plain, (~ spl3_33 | ~ spl3_34), inference(avatar_contradiction_clause, [], [f2372])).
fof(f2372, plain, ($false | (~ spl3_33 | ~ spl3_34)), inference(subsumption_resolution, [], [f2371, f157])).
fof(f157, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f2371, plain, ((e0 = e1) | (~ spl3_33 | ~ spl3_34)), inference(backward_demodulation, [], [f363, f359])).
fof(f2365, plain, (~ spl3_26 | ~ spl3_54 | spl3_77), inference(avatar_split_clause, [], [f2364, f582, f446, f327])).
fof(f2364, plain, (~ (e1 = op(e2, e1)) | (~ spl3_54 | spl3_77)), inference(forward_demodulation, [], [f584, f448])).
fof(f2357, plain, (~ spl3_38 | ~ spl3_54), inference(avatar_split_clause, [], [f2356, f446, f378])).
fof(f2356, plain, (~ (e1 = op(e1, e2)) | ~ spl3_54), inference(forward_demodulation, [], [f121, f448])).
fof(f2355, plain, (~ spl3_40 | ~ spl3_44), inference(avatar_split_clause, [], [f2354, f403, f386])).
fof(f2354, plain, (~ (e3 = op(e1, e2)) | ~ spl3_44), inference(forward_demodulation, [], [f142, f405])).
fof(f2352, plain, (~ spl3_37 | ~ spl3_3 | ~ spl3_44 | spl3_103), inference(avatar_split_clause, [], [f2325, f722, f403, f229, f374])).
fof(f2325, plain, (~ (e0 = op(e1, e2)) | (~ spl3_3 | ~ spl3_44 | spl3_103)), inference(forward_demodulation, [], [f2324, f231])).
fof(f2324, plain, (~ (e0 = op(e1, op(e3, e3))) | (~ spl3_44 | spl3_103)), inference(forward_demodulation, [], [f724, f405])).
fof(f2346, plain, (spl3_23 | ~ spl3_3 | ~ spl3_100), inference(avatar_split_clause, [], [f2313, f706, f229, f314])).
fof(f2313, plain, ((e2 = op(e2, e2)) | (~ spl3_3 | ~ spl3_100)), inference(backward_demodulation, [], [f707, f231])).
fof(f707, plain, ((e2 = op(op(e3, e3), op(e3, e3))) | ~ spl3_100), inference(avatar_component_clause, [], [f706])).
fof(f2278, plain, (~ spl3_32 | ~ spl3_52 | spl3_69), inference(avatar_contradiction_clause, [], [f2277])).
fof(f2277, plain, ($false | (~ spl3_32 | ~ spl3_52 | spl3_69)), inference(subsumption_resolution, [], [f2274, f439])).
fof(f2274, plain, (~ (e3 = op(e0, e3)) | (~ spl3_32 | spl3_69)), inference(backward_demodulation, [], [f546, f354])).
fof(f2263, plain, (~ spl3_1 | ~ spl3_44 | spl3_89), inference(avatar_split_clause, [], [f2256, f653, f403, f221])).
fof(f2256, plain, (~ (e0 = op(e3, e3)) | (~ spl3_44 | spl3_89)), inference(backward_demodulation, [], [f655, f405])).
fof(f2228, plain, (~ spl3_29 | ~ spl3_61), inference(avatar_split_clause, [], [f2218, f476, f340])).
fof(f2218, plain, (~ (e0 = op(e2, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f110, f478])).
fof(f2199, plain, (~ spl3_1 | ~ spl3_62 | spl3_93), inference(avatar_contradiction_clause, [], [f2198])).
fof(f2198, plain, ($false | (~ spl3_1 | ~ spl3_62 | spl3_93)), inference(subsumption_resolution, [], [f2197, f482])).
fof(f2197, plain, (~ (op(e0, e0) = e1) | (~ spl3_1 | spl3_93)), inference(forward_demodulation, [], [f674, f223])).
fof(f2185, plain, (~ spl3_1 | ~ spl3_11 | ~ spl3_62 | spl3_85), inference(avatar_contradiction_clause, [], [f2184])).
fof(f2184, plain, ($false | (~ spl3_1 | ~ spl3_11 | ~ spl3_62 | spl3_85)), inference(subsumption_resolution, [], [f2183, f265])).
fof(f2183, plain, (~ (e2 = op(e3, e1)) | (~ spl3_1 | ~ spl3_62 | spl3_85)), inference(forward_demodulation, [], [f2182, f482])).
fof(f2182, plain, (~ (e2 = op(e3, op(e0, e0))) | (~ spl3_1 | spl3_85)), inference(forward_demodulation, [], [f636, f223])).
fof(f636, plain, (~ (e2 = op(e3, op(op(e3, e3), op(e3, e3)))) | spl3_85), inference(avatar_component_clause, [], [f634])).
fof(f634, plain, (spl3_85 <=> (e2 = op(e3, op(op(e3, e3), op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl3_85])])).
fof(f2154, plain, (~ spl3_3 | ~ spl3_51), inference(avatar_split_clause, [], [f2153, f433, f229])).
fof(f2153, plain, (~ (e2 = op(e3, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f129, f435])).
fof(f2150, plain, (~ spl3_8 | ~ spl3_56), inference(avatar_split_clause, [], [f2147, f454, f250])).
fof(f2147, plain, (~ (e3 = op(e3, e2)) | ~ spl3_56), inference(backward_demodulation, [], [f123, f456])).
fof(f123, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2146, plain, (~ spl3_49 | ~ spl3_57), inference(avatar_split_clause, [], [f2140, f459, f425])).
fof(f2140, plain, (~ (e0 = op(e0, e3)) | ~ spl3_57), inference(backward_demodulation, [], [f137, f461])).
fof(f2131, plain, (~ spl3_54 | ~ spl3_62), inference(avatar_split_clause, [], [f2123, f480, f446])).
fof(f2123, plain, (~ (e1 = op(e0, e2)) | ~ spl3_62), inference(backward_demodulation, [], [f134, f482])).
fof(f2110, plain, (~ spl3_6 | spl3_88 | ~ spl3_100), inference(avatar_split_clause, [], [f2109, f706, f648, f242])).
fof(f648, plain, (spl3_88 <=> (e1 = op(e3, op(op(e3, e3), op(e3, e3))))), introduced(avatar_definition, [new_symbols(naming, [spl3_88])])).
fof(f2109, plain, (~ (e1 = op(e3, e2)) | (spl3_88 | ~ spl3_100)), inference(forward_demodulation, [], [f650, f707])).
fof(f650, plain, (~ (e1 = op(e3, op(op(e3, e3), op(e3, e3)))) | spl3_88), inference(avatar_component_clause, [], [f648])).
fof(f2097, plain, (~ spl3_3 | ~ spl3_64 | spl3_102), inference(avatar_contradiction_clause, [], [f2096])).
fof(f2096, plain, ($false | (~ spl3_3 | ~ spl3_64 | spl3_102)), inference(subsumption_resolution, [], [f2095, f231])).
fof(f2095, plain, (~ (e2 = op(e3, e3)) | (~ spl3_64 | spl3_102)), inference(forward_demodulation, [], [f719, f490])).
fof(f2063, plain, (~ spl3_42 | spl3_73), inference(avatar_contradiction_clause, [], [f2062])).
fof(f2062, plain, ($false | (~ spl3_42 | spl3_73)), inference(subsumption_resolution, [], [f2058, f397])).
fof(f2058, plain, (~ (e1 = op(e1, e1)) | (~ spl3_42 | spl3_73)), inference(backward_demodulation, [], [f565, f397])).
fof(f565, plain, (~ (op(e1, e1) = op(e1, op(e1, e1))) | spl3_73), inference(avatar_component_clause, [], [f563])).
fof(f563, plain, (spl3_73 <=> (op(e1, e1) = op(e1, op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_73])])).
fof(f2053, plain, (~ spl3_47 | ~ spl3_48), inference(avatar_contradiction_clause, [], [f2052])).
fof(f2052, plain, ($false | (~ spl3_47 | ~ spl3_48)), inference(subsumption_resolution, [], [f2051, f162])).
fof(f162, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f2051, plain, ((e2 = e3) | (~ spl3_47 | ~ spl3_48)), inference(backward_demodulation, [], [f422, f418])).
fof(f2050, plain, (~ spl3_33 | ~ spl3_49), inference(avatar_split_clause, [], [f2047, f425, f357])).
fof(f2047, plain, (~ (e0 = op(e1, e3)) | ~ spl3_49), inference(backward_demodulation, [], [f127, f427])).
fof(f2037, plain, (~ spl3_43 | ~ spl3_59), inference(avatar_split_clause, [], [f2032, f467, f399])).
fof(f2032, plain, (~ (e2 = op(e1, e1)) | ~ spl3_59), inference(backward_demodulation, [], [f115, f469])).
fof(f2030, plain, (~ spl3_54 | ~ spl3_3 | ~ spl3_64 | spl3_104), inference(avatar_split_clause, [], [f2029, f727, f488, f229, f446])).
fof(f2029, plain, (~ (e1 = op(e0, e2)) | (~ spl3_3 | ~ spl3_64 | spl3_104)), inference(forward_demodulation, [], [f2025, f231])).
fof(f2025, plain, (~ (e1 = op(e0, op(e3, e3))) | (~ spl3_64 | spl3_104)), inference(backward_demodulation, [], [f729, f490])).
fof(f1981, plain, (~ spl3_58 | ~ spl3_14 | spl3_83), inference(avatar_split_clause, [], [f1980, f625, f276, f463])).
fof(f625, plain, (spl3_83 <=> (op(e3, e0) = op(e0, op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_83])])).
fof(f1980, plain, (~ (e1 = op(e0, e1)) | (~ spl3_14 | spl3_83)), inference(forward_demodulation, [], [f627, f278])).
fof(f627, plain, (~ (op(e3, e0) = op(e0, op(e3, e0))) | spl3_83), inference(avatar_component_clause, [], [f625])).
fof(f1915, plain, (~ spl3_53 | ~ spl3_54), inference(avatar_contradiction_clause, [], [f1914])).
fof(f1914, plain, ($false | (~ spl3_53 | ~ spl3_54)), inference(subsumption_resolution, [], [f1913, f157])).
fof(f1913, plain, ((e0 = e1) | (~ spl3_53 | ~ spl3_54)), inference(forward_demodulation, [], [f448, f444])).
fof(f1906, plain, (~ spl3_14 | ~ spl3_84 | spl3_88), inference(avatar_split_clause, [], [f1905, f648, f630, f276])).
fof(f1905, plain, (~ (e1 = op(e3, e0)) | (~ spl3_84 | spl3_88)), inference(forward_demodulation, [], [f650, f631])).
fof(f631, plain, ((e0 = op(op(e3, e3), op(e3, e3))) | ~ spl3_84), inference(avatar_component_clause, [], [f630])).
fof(f1877, plain, (~ spl3_29 | ~ spl3_53 | spl3_77), inference(avatar_split_clause, [], [f1785, f582, f442, f340])).
fof(f1785, plain, (~ (e0 = op(e2, e0)) | (~ spl3_53 | spl3_77)), inference(backward_demodulation, [], [f584, f444])).
fof(f1872, plain, (~ spl3_45 | ~ spl3_9 | spl3_82), inference(avatar_split_clause, [], [f1871, f620, f255, f408])).
fof(f1871, plain, (~ (e0 = op(e1, e0)) | (~ spl3_9 | spl3_82)), inference(forward_demodulation, [], [f622, f257])).
fof(f1857, plain, (~ spl3_4 | ~ spl3_52), inference(avatar_split_clause, [], [f1856, f437, f233])).
fof(f1856, plain, (~ (e3 = op(e3, e3)) | ~ spl3_52), inference(forward_demodulation, [], [f129, f439])).
fof(f1841, plain, (~ spl3_23 | spl3_67), inference(avatar_contradiction_clause, [], [f1840])).
fof(f1840, plain, ($false | (~ spl3_23 | spl3_67)), inference(subsumption_resolution, [], [f1831, f316])).
fof(f1831, plain, (~ (e2 = op(e2, e2)) | (~ spl3_23 | spl3_67)), inference(backward_demodulation, [], [f536, f316])).
fof(f536, plain, (~ (op(e2, e2) = op(e2, op(e2, e2))) | spl3_67), inference(avatar_component_clause, [], [f534])).
fof(f534, plain, (spl3_67 <=> (op(e2, e2) = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_67])])).
fof(f1815, plain, (~ spl3_34 | ~ spl3_35), inference(avatar_contradiction_clause, [], [f1814])).
fof(f1814, plain, ($false | (~ spl3_34 | ~ spl3_35)), inference(subsumption_resolution, [], [f1813, f160])).
fof(f160, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f1813, plain, ((e1 = e2) | (~ spl3_34 | ~ spl3_35)), inference(backward_demodulation, [], [f367, f363])).
fof(f367, plain, ((e2 = op(e1, e3)) | ~ spl3_35), inference(avatar_component_clause, [], [f365])).
fof(f1812, plain, (~ spl3_38 | ~ spl3_40), inference(avatar_contradiction_clause, [], [f1811])).
fof(f1811, plain, ($false | (~ spl3_38 | ~ spl3_40)), inference(subsumption_resolution, [], [f1809, f161])).
fof(f1809, plain, ((e1 = e3) | (~ spl3_38 | ~ spl3_40)), inference(backward_demodulation, [], [f388, f380])).
fof(f1807, plain, (~ spl3_20 | ~ spl3_40 | spl3_72), inference(avatar_split_clause, [], [f1805, f558, f386, f301])).
fof(f1805, plain, (~ (e3 = op(e2, e3)) | (~ spl3_40 | spl3_72)), inference(backward_demodulation, [], [f560, f388])).
fof(f1745, plain, (~ spl3_63 | ~ spl3_1 | spl3_100), inference(avatar_split_clause, [], [f1578, f706, f221, f484])).
fof(f1578, plain, (~ (op(e0, e0) = e2) | (~ spl3_1 | spl3_100)), inference(backward_demodulation, [], [f708, f223])).
fof(f1698, plain, (~ spl3_46 | ~ spl3_48), inference(avatar_contradiction_clause, [], [f1697])).
fof(f1697, plain, ($false | (~ spl3_46 | ~ spl3_48)), inference(subsumption_resolution, [], [f1696, f161])).
fof(f1696, plain, ((e1 = e3) | (~ spl3_46 | ~ spl3_48)), inference(forward_demodulation, [], [f422, f414])).
fof(f1692, plain, (~ spl3_37 | ~ spl3_41), inference(avatar_split_clause, [], [f1378, f391, f374])).
fof(f1378, plain, (~ (e0 = op(e1, e2)) | ~ spl3_41), inference(forward_demodulation, [], [f142, f393])).
fof(f1658, plain, (~ spl3_7 | ~ spl3_55), inference(avatar_split_clause, [], [f1655, f450, f246])).
fof(f1655, plain, (~ (e2 = op(e3, e2)) | ~ spl3_55), inference(backward_demodulation, [], [f123, f452])).
fof(f1642, plain, (~ spl3_7 | ~ spl3_35 | spl3_71), inference(avatar_split_clause, [], [f1641, f553, f365, f246])).
fof(f1641, plain, (~ (e2 = op(e3, e2)) | (~ spl3_35 | spl3_71)), inference(forward_demodulation, [], [f555, f367])).
fof(f1632, plain, (~ spl3_6 | ~ spl3_10), inference(avatar_split_clause, [], [f1628, f259, f242])).
fof(f1628, plain, (~ (e1 = op(e3, e2)) | ~ spl3_10), inference(backward_demodulation, [], [f154, f261])).
fof(f261, plain, ((e1 = op(e3, e1)) | ~ spl3_10), inference(avatar_component_clause, [], [f259])).
fof(f1616, plain, (~ spl3_55 | ~ spl3_31 | spl3_69), inference(avatar_split_clause, [], [f1615, f544, f348, f450])).
fof(f1615, plain, (~ (e2 = op(e0, e2)) | (~ spl3_31 | spl3_69)), inference(forward_demodulation, [], [f546, f350])).
fof(f1612, plain, (~ spl3_58 | ~ spl3_46 | spl3_74), inference(avatar_split_clause, [], [f1611, f568, f412, f463])).
fof(f1611, plain, (~ (e1 = op(e0, e1)) | (~ spl3_46 | spl3_74)), inference(forward_demodulation, [], [f570, f414])).
fof(f1587, plain, (~ spl3_10 | ~ spl3_18 | spl3_66), inference(avatar_split_clause, [], [f1445, f529, f293, f259])).
fof(f1445, plain, (~ (e1 = op(e3, e1)) | (~ spl3_18 | spl3_66)), inference(backward_demodulation, [], [f531, f295])).
fof(f295, plain, ((e1 = op(e2, e3)) | ~ spl3_18), inference(avatar_component_clause, [], [f293])).
fof(f1570, plain, (~ spl3_4 | ~ spl3_16), inference(avatar_split_clause, [], [f1567, f284, f233])).
fof(f1567, plain, (~ (e3 = op(e3, e3)) | ~ spl3_16), inference(backward_demodulation, [], [f153, f286])).
fof(f1569, plain, (~ spl3_16 | ~ spl3_52 | spl3_83), inference(avatar_contradiction_clause, [], [f1568])).
fof(f1568, plain, ($false | (~ spl3_16 | ~ spl3_52 | spl3_83)), inference(subsumption_resolution, [], [f1566, f439])).
fof(f1566, plain, (~ (e3 = op(e0, e3)) | (~ spl3_16 | spl3_83)), inference(backward_demodulation, [], [f627, f286])).
fof(f1540, plain, (~ spl3_64 | ~ spl3_41 | spl3_106), inference(avatar_split_clause, [], [f1539, f737, f391, f488])).
fof(f1539, plain, (~ (op(e0, e0) = e3) | (~ spl3_41 | spl3_106)), inference(forward_demodulation, [], [f739, f393])).
fof(f1533, plain, (spl3_64 | ~ spl3_21 | ~ spl3_105), inference(avatar_split_clause, [], [f1496, f732, f306, f488])).
fof(f1496, plain, ((op(e0, e0) = e3) | (~ spl3_21 | ~ spl3_105)), inference(forward_demodulation, [], [f733, f308])).
fof(f733, plain, ((e3 = op(op(e2, e2), op(e2, e2))) | ~ spl3_105), inference(avatar_component_clause, [], [f732])).
fof(f1518, plain, (~ spl3_54 | ~ spl3_58), inference(avatar_split_clause, [], [f1515, f463, f446])).
fof(f1515, plain, (~ (e1 = op(e0, e2)) | ~ spl3_58), inference(backward_demodulation, [], [f136, f465])).
fof(f1504, plain, (~ spl3_18 | ~ spl3_21 | ~ spl3_64 | spl3_91), inference(avatar_contradiction_clause, [], [f1503])).
fof(f1503, plain, ($false | (~ spl3_18 | ~ spl3_21 | ~ spl3_64 | spl3_91)), inference(subsumption_resolution, [], [f1502, f295])).
fof(f1502, plain, (~ (e1 = op(e2, e3)) | (~ spl3_21 | ~ spl3_64 | spl3_91)), inference(forward_demodulation, [], [f1501, f490])).
fof(f1501, plain, (~ (e1 = op(e2, op(e0, e0))) | (~ spl3_21 | spl3_91)), inference(forward_demodulation, [], [f664, f308])).
fof(f1500, plain, (~ spl3_35 | ~ spl3_41 | ~ spl3_64 | spl3_92), inference(avatar_contradiction_clause, [], [f1499])).
fof(f1499, plain, ($false | (~ spl3_35 | ~ spl3_41 | ~ spl3_64 | spl3_92)), inference(subsumption_resolution, [], [f1498, f367])).
fof(f1498, plain, (~ (e2 = op(e1, e3)) | (~ spl3_41 | ~ spl3_64 | spl3_92)), inference(forward_demodulation, [], [f1497, f490])).
fof(f1497, plain, (~ (e2 = op(e1, op(e0, e0))) | (~ spl3_41 | spl3_92)), inference(forward_demodulation, [], [f669, f393])).
fof(f1476, plain, (~ spl3_13 | ~ spl3_49 | spl3_76), inference(avatar_contradiction_clause, [], [f1475])).
fof(f1475, plain, ($false | (~ spl3_13 | ~ spl3_49 | spl3_76)), inference(subsumption_resolution, [], [f1474, f274])).
fof(f1474, plain, (~ (e0 = op(e3, e0)) | (~ spl3_49 | spl3_76)), inference(forward_demodulation, [], [f579, f427])).
fof(f1460, plain, (~ spl3_26 | ~ spl3_6 | spl3_81), inference(avatar_split_clause, [], [f1459, f615, f242, f327])).
fof(f1459, plain, (~ (e1 = op(e2, e1)) | (~ spl3_6 | spl3_81)), inference(forward_demodulation, [], [f617, f244])).
fof(f1457, plain, (~ spl3_6 | ~ spl3_7), inference(avatar_contradiction_clause, [], [f1456])).
fof(f1456, plain, ($false | (~ spl3_6 | ~ spl3_7)), inference(subsumption_resolution, [], [f1455, f160])).
fof(f1455, plain, ((e1 = e2) | (~ spl3_6 | ~ spl3_7)), inference(backward_demodulation, [], [f248, f244])).
fof(f248, plain, ((e2 = op(e3, e2)) | ~ spl3_7), inference(avatar_component_clause, [], [f246])).
fof(f1442, plain, (~ spl3_26 | ~ spl3_28), inference(avatar_contradiction_clause, [], [f1441])).
fof(f1441, plain, ($false | (~ spl3_26 | ~ spl3_28)), inference(subsumption_resolution, [], [f1440, f161])).
fof(f1440, plain, ((e1 = e3) | (~ spl3_26 | ~ spl3_28)), inference(forward_demodulation, [], [f337, f329])).
fof(f1430, plain, (~ spl3_49 | ~ spl3_51), inference(avatar_contradiction_clause, [], [f1429])).
fof(f1429, plain, ($false | (~ spl3_49 | ~ spl3_51)), inference(subsumption_resolution, [], [f1428, f158])).
fof(f158, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f1428, plain, ((e0 = e2) | (~ spl3_49 | ~ spl3_51)), inference(backward_demodulation, [], [f435, f427])).
fof(f1422, plain, (~ spl3_4 | ~ spl3_64 | spl3_107), inference(avatar_contradiction_clause, [], [f1421])).
fof(f1421, plain, ($false | (~ spl3_4 | ~ spl3_64 | spl3_107)), inference(subsumption_resolution, [], [f1411, f235])).
fof(f1411, plain, (~ (e3 = op(e3, e3)) | (~ spl3_64 | spl3_107)), inference(backward_demodulation, [], [f745, f490])).
fof(f1394, plain, (~ spl3_63 | ~ spl3_41 | spl3_101), inference(avatar_split_clause, [], [f1240, f711, f391, f484])).
fof(f1240, plain, (~ (op(e0, e0) = e2) | (~ spl3_41 | spl3_101)), inference(backward_demodulation, [], [f713, f393])).
fof(f1390, plain, (~ spl3_64 | ~ spl3_21 | spl3_105), inference(avatar_split_clause, [], [f1320, f732, f306, f488])).
fof(f1320, plain, (~ (op(e0, e0) = e3) | (~ spl3_21 | spl3_105)), inference(backward_demodulation, [], [f734, f308])).
fof(f1359, plain, (~ spl3_17 | ~ spl3_21), inference(avatar_split_clause, [], [f1358, f306, f289])).
fof(f1358, plain, (~ (e0 = op(e2, e3)) | ~ spl3_21), inference(forward_demodulation, [], [f150, f308])).
fof(f1355, plain, (~ spl3_15 | ~ spl3_11), inference(avatar_split_clause, [], [f1354, f263, f280])).
fof(f1354, plain, (~ (e2 = op(e3, e0)) | ~ spl3_11), inference(forward_demodulation, [], [f151, f265])).
fof(f1346, plain, (~ spl3_8 | ~ spl3_4), inference(avatar_split_clause, [], [f1345, f233, f250])).
fof(f1345, plain, (~ (e3 = op(e3, e2)) | ~ spl3_4), inference(forward_demodulation, [], [f156, f235])).
fof(f1340, plain, (~ spl3_4 | spl3_80), inference(avatar_contradiction_clause, [], [f1339])).
fof(f1339, plain, ($false | (~ spl3_4 | spl3_80)), inference(subsumption_resolution, [], [f1337, f235])).
fof(f1337, plain, (~ (e3 = op(e3, e3)) | (~ spl3_4 | spl3_80)), inference(backward_demodulation, [], [f612, f235])).
fof(f612, plain, (~ (op(e3, e3) = op(e3, op(e3, e3))) | spl3_80), inference(avatar_component_clause, [], [f610])).
fof(f610, plain, (spl3_80 <=> (op(e3, e3) = op(e3, op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_80])])).
fof(f1333, plain, (~ spl3_2 | ~ spl3_14), inference(avatar_split_clause, [], [f1332, f276, f225])).
fof(f1332, plain, (~ (e1 = op(e3, e3)) | ~ spl3_14), inference(backward_demodulation, [], [f153, f278])).
fof(f1303, plain, (~ spl3_31 | ~ spl3_32), inference(avatar_contradiction_clause, [], [f1302])).
fof(f1302, plain, ($false | (~ spl3_31 | ~ spl3_32)), inference(subsumption_resolution, [], [f1301, f162])).
fof(f1301, plain, ((e2 = e3) | (~ spl3_31 | ~ spl3_32)), inference(backward_demodulation, [], [f354, f350])).
fof(f1300, plain, (~ spl3_2 | ~ spl3_34), inference(avatar_split_clause, [], [f1298, f361, f225])).
fof(f1298, plain, (~ (e1 = op(e3, e3)) | ~ spl3_34), inference(backward_demodulation, [], [f131, f363])).
fof(f1292, plain, (~ spl3_54 | ~ spl3_56), inference(avatar_contradiction_clause, [], [f1291])).
fof(f1291, plain, ($false | (~ spl3_54 | ~ spl3_56)), inference(subsumption_resolution, [], [f1290, f161])).
fof(f1290, plain, ((e1 = e3) | (~ spl3_54 | ~ spl3_56)), inference(backward_demodulation, [], [f456, f448])).
fof(f1271, plain, (~ spl3_34 | ~ spl3_46), inference(avatar_split_clause, [], [f1270, f412, f361])).
fof(f1270, plain, (~ (e1 = op(e1, e3)) | ~ spl3_46), inference(forward_demodulation, [], [f141, f414])).
fof(f1263, plain, (~ spl3_14 | ~ spl3_46), inference(avatar_split_clause, [], [f1262, f412, f276])).
fof(f1262, plain, (~ (e1 = op(e3, e0)) | ~ spl3_46), inference(forward_demodulation, [], [f113, f414])).
fof(f1251, plain, (~ spl3_2 | ~ spl3_15 | ~ spl3_41 | spl3_85), inference(avatar_contradiction_clause, [], [f1250])).
fof(f1250, plain, ($false | (~ spl3_2 | ~ spl3_15 | ~ spl3_41 | spl3_85)), inference(subsumption_resolution, [], [f1245, f282])).
fof(f1245, plain, (~ (e2 = op(e3, e0)) | (~ spl3_2 | ~ spl3_41 | spl3_85)), inference(backward_demodulation, [], [f1208, f393])).
fof(f1208, plain, (~ (e2 = op(e3, op(e1, e1))) | (~ spl3_2 | spl3_85)), inference(forward_demodulation, [], [f636, f227])).
fof(f1235, plain, (~ spl3_42 | ~ spl3_46), inference(avatar_split_clause, [], [f1232, f412, f395])).
fof(f1232, plain, (~ (e1 = op(e1, e1)) | ~ spl3_46), inference(backward_demodulation, [], [f139, f414])).
fof(f1222, plain, (~ spl3_61 | spl3_79), inference(avatar_contradiction_clause, [], [f1221])).
fof(f1221, plain, ($false | (~ spl3_61 | spl3_79)), inference(subsumption_resolution, [], [f1216, f478])).
fof(f1216, plain, (~ (e0 = op(e0, e0)) | (~ spl3_61 | spl3_79)), inference(backward_demodulation, [], [f594, f478])).
fof(f594, plain, (~ (op(e0, e0) = op(e0, op(e0, e0))) | spl3_79), inference(avatar_component_clause, [], [f592])).
fof(f592, plain, (spl3_79 <=> (op(e0, e0) = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_79])])).
fof(f1207, plain, (~ spl3_41 | ~ spl3_22 | spl3_86), inference(avatar_split_clause, [], [f1206, f639, f310, f391])).
fof(f1206, plain, (~ (e0 = op(e1, e1)) | (~ spl3_22 | spl3_86)), inference(forward_demodulation, [], [f641, f312])).
fof(f1183, plain, (~ spl3_27 | ~ spl3_39 | spl3_68), inference(avatar_contradiction_clause, [], [f1182])).
fof(f1182, plain, ($false | (~ spl3_27 | ~ spl3_39 | spl3_68)), inference(subsumption_resolution, [], [f1181, f384])).
fof(f1181, plain, (~ (e2 = op(e1, e2)) | (~ spl3_27 | spl3_68)), inference(forward_demodulation, [], [f541, f333])).
fof(f1107, plain, (~ spl3_36 | ~ spl3_12 | spl3_82), inference(avatar_split_clause, [], [f870, f620, f267, f369])).
fof(f870, plain, (~ (e3 = op(e1, e3)) | (~ spl3_12 | spl3_82)), inference(backward_demodulation, [], [f622, f269])).
fof(f1085, plain, (~ spl3_2 | ~ spl3_5 | ~ spl3_43 | spl3_95), inference(avatar_contradiction_clause, [], [f1084])).
fof(f1084, plain, ($false | (~ spl3_2 | ~ spl3_5 | ~ spl3_43 | spl3_95)), inference(subsumption_resolution, [], [f1080, f240])).
fof(f1080, plain, (~ (e0 = op(e3, e2)) | (~ spl3_2 | ~ spl3_43 | spl3_95)), inference(backward_demodulation, [], [f889, f401])).
fof(f889, plain, (~ (e0 = op(e3, op(e1, e1))) | (~ spl3_2 | spl3_95)), inference(backward_demodulation, [], [f684, f227])).
fof(f1058, plain, (~ spl3_17 | ~ spl3_20), inference(avatar_contradiction_clause, [], [f1057])).
fof(f1057, plain, ($false | (~ spl3_17 | ~ spl3_20)), inference(subsumption_resolution, [], [f1055, f159])).
fof(f1055, plain, ((e0 = e3) | (~ spl3_17 | ~ spl3_20)), inference(backward_demodulation, [], [f303, f291])).
fof(f303, plain, ((e3 = op(e2, e3)) | ~ spl3_20), inference(avatar_component_clause, [], [f301])).
fof(f1051, plain, (~ spl3_39 | ~ spl3_40), inference(avatar_contradiction_clause, [], [f1050])).
fof(f1050, plain, ($false | (~ spl3_39 | ~ spl3_40)), inference(subsumption_resolution, [], [f1049, f162])).
fof(f1049, plain, ((e2 = e3) | (~ spl3_39 | ~ spl3_40)), inference(backward_demodulation, [], [f388, f384])).
fof(f1048, plain, (~ spl3_22 | ~ spl3_32 | ~ spl3_41 | spl3_87), inference(avatar_contradiction_clause, [], [f1047])).
fof(f1047, plain, ($false | (~ spl3_22 | ~ spl3_32 | ~ spl3_41 | spl3_87)), inference(subsumption_resolution, [], [f1043, f354])).
fof(f1043, plain, (~ (e3 = op(e2, e0)) | (~ spl3_22 | ~ spl3_41 | spl3_87)), inference(backward_demodulation, [], [f988, f393])).
fof(f988, plain, (~ (e3 = op(e2, op(e1, e1))) | (~ spl3_22 | spl3_87)), inference(forward_demodulation, [], [f645, f312])).
fof(f1011, plain, (~ spl3_41 | ~ spl3_2 | spl3_84), inference(avatar_split_clause, [], [f888, f630, f225, f391])).
fof(f888, plain, (~ (e0 = op(e1, e1)) | (~ spl3_2 | spl3_84)), inference(backward_demodulation, [], [f632, f227])).
fof(f974, plain, (~ spl3_22 | ~ spl3_23), inference(avatar_contradiction_clause, [], [f973])).
fof(f973, plain, ($false | (~ spl3_22 | ~ spl3_23)), inference(subsumption_resolution, [], [f972, f160])).
fof(f972, plain, ((e1 = e2) | (~ spl3_22 | ~ spl3_23)), inference(backward_demodulation, [], [f316, f312])).
fof(f970, plain, (~ spl3_25 | ~ spl3_27), inference(avatar_contradiction_clause, [], [f969])).
fof(f969, plain, ($false | (~ spl3_25 | ~ spl3_27)), inference(subsumption_resolution, [], [f968, f158])).
fof(f968, plain, ((e0 = e2) | (~ spl3_25 | ~ spl3_27)), inference(forward_demodulation, [], [f333, f325])).
fof(f938, plain, (~ spl3_53 | ~ spl3_5), inference(avatar_split_clause, [], [f937, f238, f442])).
fof(f937, plain, (~ (e0 = op(e0, e2)) | ~ spl3_5), inference(forward_demodulation, [], [f123, f240])).
fof(f934, plain, (~ spl3_50 | ~ spl3_52), inference(avatar_contradiction_clause, [], [f933])).
fof(f933, plain, ($false | (~ spl3_50 | ~ spl3_52)), inference(subsumption_resolution, [], [f932, f161])).
fof(f932, plain, ((e1 = e3) | (~ spl3_50 | ~ spl3_52)), inference(forward_demodulation, [], [f439, f431])).
fof(f898, plain, (~ spl3_55 | ~ spl3_15 | spl3_83), inference(avatar_split_clause, [], [f897, f625, f280, f450])).
fof(f897, plain, (~ (e2 = op(e0, e2)) | (~ spl3_15 | spl3_83)), inference(forward_demodulation, [], [f627, f282])).
fof(f894, plain, (~ spl3_29 | ~ spl3_5 | spl3_81), inference(avatar_split_clause, [], [f893, f615, f238, f340])).
fof(f893, plain, (~ (e0 = op(e2, e0)) | (~ spl3_5 | spl3_81)), inference(forward_demodulation, [], [f617, f240])).
fof(f886, plain, (~ spl3_5 | ~ spl3_6), inference(avatar_contradiction_clause, [], [f885])).
fof(f885, plain, ($false | (~ spl3_5 | ~ spl3_6)), inference(subsumption_resolution, [], [f884, f157])).
fof(f884, plain, ((e0 = e1) | (~ spl3_5 | ~ spl3_6)), inference(backward_demodulation, [], [f244, f240])).
fof(f883, plain, (~ spl3_6 | ~ spl3_8), inference(avatar_contradiction_clause, [], [f882])).
fof(f882, plain, ($false | (~ spl3_6 | ~ spl3_8)), inference(subsumption_resolution, [], [f881, f161])).
fof(f881, plain, ((e1 = e3) | (~ spl3_6 | ~ spl3_8)), inference(backward_demodulation, [], [f252, f244])).
fof(f872, plain, (~ spl3_4 | ~ spl3_12), inference(avatar_split_clause, [], [f869, f267, f233])).
fof(f869, plain, (~ (e3 = op(e3, e3)) | ~ spl3_12), inference(backward_demodulation, [], [f155, f269])).
fof(f867, plain, (~ spl3_14 | ~ spl3_15), inference(avatar_contradiction_clause, [], [f866])).
fof(f866, plain, ($false | (~ spl3_14 | ~ spl3_15)), inference(subsumption_resolution, [], [f865, f160])).
fof(f865, plain, ((e1 = e2) | (~ spl3_14 | ~ spl3_15)), inference(backward_demodulation, [], [f282, f278])).
fof(f864, plain, (~ spl3_15 | ~ spl3_16), inference(avatar_contradiction_clause, [], [f863])).
fof(f863, plain, ($false | (~ spl3_15 | ~ spl3_16)), inference(subsumption_resolution, [], [f862, f162])).
fof(f862, plain, ((e2 = e3) | (~ spl3_15 | ~ spl3_16)), inference(backward_demodulation, [], [f286, f282])).
fof(f853, plain, (~ spl3_17 | ~ spl3_18), inference(avatar_contradiction_clause, [], [f852])).
fof(f852, plain, ($false | (~ spl3_17 | ~ spl3_18)), inference(subsumption_resolution, [], [f851, f157])).
fof(f851, plain, ((e0 = e1) | (~ spl3_17 | ~ spl3_18)), inference(backward_demodulation, [], [f295, f291])).
fof(f850, plain, (~ spl3_18 | ~ spl3_20), inference(avatar_contradiction_clause, [], [f849])).
fof(f849, plain, ($false | (~ spl3_18 | ~ spl3_20)), inference(subsumption_resolution, [], [f848, f161])).
fof(f848, plain, ((e1 = e3) | (~ spl3_18 | ~ spl3_20)), inference(backward_demodulation, [], [f303, f295])).
fof(f811, plain, (~ spl3_10 | ~ spl3_42), inference(avatar_split_clause, [], [f807, f395, f259])).
fof(f807, plain, (~ (e1 = op(e3, e1)) | ~ spl3_42), inference(backward_demodulation, [], [f119, f397])).
fof(f810, plain, (~ spl3_26 | ~ spl3_42), inference(avatar_split_clause, [], [f806, f395, f327])).
fof(f806, plain, (~ (e1 = op(e2, e1)) | ~ spl3_42), inference(backward_demodulation, [], [f118, f397])).
fof(f799, plain, (~ spl3_29 | ~ spl3_45), inference(avatar_split_clause, [], [f794, f408, f340])).
fof(f794, plain, (~ (e0 = op(e2, e0)) | ~ spl3_45), inference(backward_demodulation, [], [f112, f410])).
fof(f793, plain, (~ spl3_50 | ~ spl3_51), inference(avatar_contradiction_clause, [], [f792])).
fof(f792, plain, ($false | (~ spl3_50 | ~ spl3_51)), inference(subsumption_resolution, [], [f791, f160])).
fof(f791, plain, ((e1 = e2) | (~ spl3_50 | ~ spl3_51)), inference(backward_demodulation, [], [f435, f431])).
fof(f790, plain, (~ spl3_51 | ~ spl3_52), inference(avatar_contradiction_clause, [], [f789])).
fof(f789, plain, ($false | (~ spl3_51 | ~ spl3_52)), inference(subsumption_resolution, [], [f788, f162])).
fof(f788, plain, ((e2 = e3) | (~ spl3_51 | ~ spl3_52)), inference(backward_demodulation, [], [f439, f435])).
fof(f781, plain, (~ spl3_49 | ~ spl3_53), inference(avatar_split_clause, [], [f777, f442, f425])).
fof(f777, plain, (~ (e0 = op(e0, e3)) | ~ spl3_53), inference(backward_demodulation, [], [f138, f444])).
fof(f769, plain, (~ spl3_41 | ~ spl3_57), inference(avatar_split_clause, [], [f764, f459, f391])).
fof(f764, plain, (~ (e0 = op(e1, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f115, f461])).
fof(f748, plain, (~ spl3_107 | ~ spl3_63 | ~ spl3_104), inference(avatar_split_clause, [], [f219, f727, f484, f743])).
fof(f219, plain, (~ (e1 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e1 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e2) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e1 = op(e0, op(op(e0, e0), op(e0, e0)))) & (op(e0, e0) = e2) & (e3 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax29)).
fof(f747, plain, (~ spl3_106 | ~ spl3_43 | ~ spl3_103), inference(avatar_split_clause, [], [f218, f722, f399, f737])).
fof(f218, plain, (~ (e0 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e0 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e0 = op(e1, op(op(e1, e1), op(e1, e1)))) & (e2 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax28)).
fof(f746, plain, (~ spl3_107 | ~ spl3_62 | ~ spl3_99), inference(avatar_split_clause, [], [f217, f701, f480, f743])).
fof(f217, plain, (~ (e2 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e2 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e1) | ~ (e3 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e2 = op(e0, op(op(e0, e0), op(e0, e0)))) & (op(e0, e0) = e1) & (e3 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax27)).
fof(f741, plain, (~ spl3_105 | ~ spl3_22 | ~ spl3_98), inference(avatar_split_clause, [], [f216, f696, f310, f732])).
fof(f216, plain, (~ (e0 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e0 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e0 = op(e2, op(op(e2, e2), op(e2, e2)))) & (e1 = op(e2, e2)) & (e3 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax26)).
fof(f740, plain, (~ spl3_106 | ~ spl3_41 | ~ spl3_92), inference(avatar_split_clause, [], [f215, f667, f391, f737])).
fof(f215, plain, (~ (e2 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e2 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e0 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e2 = op(e1, op(op(e1, e1), op(e1, e1)))) & (e0 = op(e1, e1)) & (e3 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax25)).
fof(f735, plain, (~ spl3_105 | ~ spl3_21 | ~ spl3_91), inference(avatar_split_clause, [], [f214, f662, f306, f732])).
fof(f214, plain, (~ (e1 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e1 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e0 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e1 = op(e2, op(op(e2, e2), op(e2, e2)))) & (e0 = op(e2, e2)) & (e3 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax24)).
fof(f730, plain, (~ spl3_102 | ~ spl3_64 | ~ spl3_104), inference(avatar_split_clause, [], [f213, f727, f488, f717])).
fof(f213, plain, (~ (e1 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e1 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e3) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e1 = op(e0, op(op(e0, e0), op(e0, e0)))) & (op(e0, e0) = e3) & (e2 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax23)).
fof(f725, plain, (~ spl3_101 | ~ spl3_44 | ~ spl3_103), inference(avatar_split_clause, [], [f212, f722, f403, f711])).
fof(f212, plain, (~ (e0 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e0 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e3 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e0 = op(e1, op(op(e1, e1), op(e1, e1)))) & (e3 = op(e1, e1)) & (e2 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax22)).
fof(f720, plain, (~ spl3_102 | ~ spl3_62 | ~ spl3_97), inference(avatar_split_clause, [], [f211, f691, f480, f717])).
fof(f211, plain, (~ (e3 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e3 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e3 = op(e0, op(op(e0, e0), op(e0, e0)))) & (op(e0, e0) = e1) & (e2 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax21)).
fof(f715, plain, (~ spl3_100 | ~ spl3_2 | ~ spl3_95), inference(avatar_split_clause, [], [f210, f682, f225, f706])).
fof(f210, plain, (~ (e0 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e0 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e0 = op(e3, op(op(e3, e3), op(e3, e3)))) & (e1 = op(e3, e3)) & (e2 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax20)).
fof(f714, plain, (~ spl3_101 | ~ spl3_41 | ~ spl3_90), inference(avatar_split_clause, [], [f209, f657, f391, f711])).
fof(f209, plain, (~ (e3 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e0 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e3 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e0 = op(e1, e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e3 = op(e1, op(op(e1, e1), op(e1, e1)))) & (e0 = op(e1, e1)) & (e2 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax19)).
fof(f709, plain, (~ spl3_100 | ~ spl3_1 | ~ spl3_88), inference(avatar_split_clause, [], [f208, f648, f221, f706])).
fof(f208, plain, (~ (e1 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e1 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e1 = op(e3, op(op(e3, e3), op(e3, e3)))) & (e0 = op(e3, e3)) & (e2 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax18)).
fof(f704, plain, (~ spl3_96 | ~ spl3_64 | ~ spl3_99), inference(avatar_split_clause, [], [f207, f701, f488, f687])).
fof(f207, plain, (~ (e2 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e2 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e3) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e2 = op(e0, op(op(e0, e0), op(e0, e0)))) & (op(e0, e0) = e3) & (e1 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax17)).
fof(f699, plain, (~ spl3_94 | ~ spl3_24 | ~ spl3_98), inference(avatar_split_clause, [], [f206, f696, f318, f677])).
fof(f206, plain, (~ (e0 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e0 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e0 = op(e2, op(op(e2, e2), op(e2, e2)))) & (e3 = op(e2, e2)) & (e1 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax16)).
fof(f694, plain, (~ spl3_96 | ~ spl3_63 | ~ spl3_97), inference(avatar_split_clause, [], [f205, f691, f484, f687])).
fof(f205, plain, (~ (e3 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e3 = op(e0, op(op(e0, e0), op(e0, e0)))) | ~ (op(e0, e0) = e2) | ~ (e1 = op(op(e0, e0), op(e0, e0)))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e3 = op(e0, op(op(e0, e0), op(e0, e0)))) & (op(e0, e0) = e2) & (e1 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax15)).
fof(f685, plain, (~ spl3_93 | ~ spl3_3 | ~ spl3_95), inference(avatar_split_clause, [], [f204, f682, f229, f672])).
fof(f204, plain, (~ (e0 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e0 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e2 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e0 = op(e3, op(op(e3, e3), op(e3, e3)))) & (e2 = op(e3, e3)) & (e1 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax14)).
fof(f680, plain, (~ spl3_94 | ~ spl3_21 | ~ spl3_87), inference(avatar_split_clause, [], [f203, f643, f306, f677])).
fof(f203, plain, (~ (e3 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e3 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e3 = op(e2, op(op(e2, e2), op(e2, e2)))) & (e0 = op(e2, e2)) & (e1 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax13)).
fof(f675, plain, (~ spl3_93 | ~ spl3_1 | ~ spl3_85), inference(avatar_split_clause, [], [f202, f634, f221, f672])).
fof(f202, plain, (~ (e2 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e2 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e0 = op(e3, e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e2 = op(e3, op(op(e3, e3), op(e3, e3)))) & (e0 = op(e3, e3)) & (e1 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax12)).
fof(f670, plain, (~ spl3_89 | ~ spl3_44 | ~ spl3_92), inference(avatar_split_clause, [], [f201, f667, f403, f653])).
fof(f201, plain, (~ (e2 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e2 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e2 = op(e1, op(op(e1, e1), op(e1, e1)))) & (e3 = op(e1, e1)) & (e0 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax11)).
fof(f665, plain, (~ spl3_86 | ~ spl3_24 | ~ spl3_91), inference(avatar_split_clause, [], [f200, f662, f318, f639])).
fof(f200, plain, (~ (e1 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e1 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e3 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e1 = op(e2, op(op(e2, e2), op(e2, e2)))) & (e3 = op(e2, e2)) & (e0 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax10)).
fof(f660, plain, (~ spl3_89 | ~ spl3_43 | ~ spl3_90), inference(avatar_split_clause, [], [f199, f657, f399, f653])).
fof(f199, plain, (~ (e3 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e3 = op(e1, op(op(e1, e1), op(e1, e1)))) | ~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1)))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e3 = op(e1, op(op(e1, e1), op(e1, e1)))) & (e2 = op(e1, e1)) & (e0 = op(op(e1, e1), op(e1, e1)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax9)).
fof(f651, plain, (~ spl3_84 | ~ spl3_3 | ~ spl3_88), inference(avatar_split_clause, [], [f198, f648, f229, f630])).
fof(f198, plain, (~ (e1 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e2 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f32])).
fof(f32, plain, (~ (e1 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e2 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ ((e1 = op(e3, op(op(e3, e3), op(e3, e3)))) & (e2 = op(e3, e3)) & (e0 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax8)).
fof(f646, plain, (~ spl3_86 | ~ spl3_22 | ~ spl3_87), inference(avatar_split_clause, [], [f197, f643, f310, f639])).
fof(f197, plain, (~ (e3 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e1 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e3 = op(e2, op(op(e2, e2), op(e2, e2)))) | ~ (e1 = op(e2, e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e3 = op(e2, op(op(e2, e2), op(e2, e2)))) & (e1 = op(e2, e2)) & (e0 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax7)).
fof(f637, plain, (~ spl3_84 | ~ spl3_2 | ~ spl3_85), inference(avatar_split_clause, [], [f196, f634, f225, f630])).
fof(f196, plain, (~ (e2 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e2 = op(e3, op(op(e3, e3), op(e3, e3)))) | ~ (e1 = op(e3, e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3)))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e2 = op(e3, op(op(e3, e3), op(e3, e3)))) & (e1 = op(e3, e3)) & (e0 = op(op(e3, e3), op(e3, e3)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax6)).
fof(f628, plain, (spl3_75 | spl3_70 | spl3_65 | ~ spl3_83), inference(avatar_split_clause, [], [f175, f625, f525, f549, f573])).
fof(f573, plain, (spl3_75 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_75])])).
fof(f549, plain, (spl3_70 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_70])])).
fof(f525, plain, (spl3_65 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_65])])).
fof(f175, plain, (~ (op(e3, e0) = op(e0, op(e3, e0))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f57, plain, (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))) & ((~ (op(e3, e3) = op(e3, op(e3, e3))) & ~ (op(e3, e2) = op(e2, op(e3, e2))) & ~ (op(e3, e1) = op(e1, op(e3, e1))) & ~ (op(e3, e0) = op(e0, op(e3, e0)))) | sP2 | sP1 | sP0)), inference(definition_folding, [], [f5, e56, e55, e54])).
fof(f54, plain, ((~ (op(e0, e3) = op(e3, op(e0, e3))) & ~ (op(e0, e2) = op(e2, op(e0, e2))) & ~ (op(e0, e1) = op(e1, op(e0, e1))) & ~ (op(e0, e0) = op(e0, op(e0, e0)))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (op(e0, e3) = op(e3, op(e0, e3))) & ~ (op(e0, e2) = op(e2, op(e0, e2))) & ~ (op(e0, e1) = op(e1, op(e0, e1))) & ~ (op(e0, e0) = op(e0, op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (op(e1, e3) = op(e3, op(e1, e3))) & ~ (op(e1, e2) = op(e2, op(e1, e2))) & ~ (op(e1, e1) = op(e1, op(e1, e1))) & ~ (op(e1, e0) = op(e0, op(e1, e0)))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (op(e1, e3) = op(e3, op(e1, e3))) & ~ (op(e1, e2) = op(e2, op(e1, e2))) & ~ (op(e1, e1) = op(e1, op(e1, e1))) & ~ (op(e1, e0) = op(e0, op(e1, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (op(e2, e3) = op(e3, op(e2, e3))) & ~ (op(e2, e2) = op(e2, op(e2, e2))) & ~ (op(e2, e1) = op(e1, op(e2, e1))) & ~ (op(e2, e0) = op(e0, op(e2, e0)))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (op(e2, e3) = op(e3, op(e2, e3))) & ~ (op(e2, e2) = op(e2, op(e2, e2))) & ~ (op(e2, e1) = op(e1, op(e2, e1))) & ~ (op(e2, e0) = op(e0, op(e2, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f5, plain, (((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0))) & ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))) & ((~ (op(e3, e3) = op(e3, op(e3, e3))) & ~ (op(e3, e2) = op(e2, op(e3, e2))) & ~ (op(e3, e1) = op(e1, op(e3, e1))) & ~ (op(e3, e0) = op(e0, op(e3, e0)))) | (~ (op(e2, e3) = op(e3, op(e2, e3))) & ~ (op(e2, e2) = op(e2, op(e2, e2))) & ~ (op(e2, e1) = op(e1, op(e2, e1))) & ~ (op(e2, e0) = op(e0, op(e2, e0)))) | (~ (op(e1, e3) = op(e3, op(e1, e3))) & ~ (op(e1, e2) = op(e2, op(e1, e2))) & ~ (op(e1, e1) = op(e1, op(e1, e1))) & ~ (op(e1, e0) = op(e0, op(e1, e0)))) | (~ (op(e0, e3) = op(e3, op(e0, e3))) & ~ (op(e0, e2) = op(e2, op(e0, e2))) & ~ (op(e0, e1) = op(e1, op(e0, e1))) & ~ (op(e0, e0) = op(e0, op(e0, e0)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax5)).
fof(f623, plain, (spl3_75 | spl3_70 | spl3_65 | ~ spl3_82), inference(avatar_split_clause, [], [f176, f620, f525, f549, f573])).
fof(f176, plain, (~ (op(e3, e1) = op(e1, op(e3, e1))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f618, plain, (spl3_75 | spl3_70 | spl3_65 | ~ spl3_81), inference(avatar_split_clause, [], [f177, f615, f525, f549, f573])).
fof(f177, plain, (~ (op(e3, e2) = op(e2, op(e3, e2))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f613, plain, (spl3_75 | spl3_70 | spl3_65 | ~ spl3_80), inference(avatar_split_clause, [], [f178, f610, f525, f549, f573])).
fof(f178, plain, (~ (op(e3, e3) = op(e3, op(e3, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f608, plain, (spl3_61 | spl3_42 | spl3_23 | spl3_4), inference(avatar_split_clause, [], [f179, f233, f314, f395, f476])).
fof(f179, plain, ((e3 = op(e3, e3)) | (e2 = op(e2, e2)) | (e1 = op(e1, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f57])).
fof(f607, plain, (~ spl3_62 | spl3_57), inference(avatar_split_clause, [], [f181, f459, f480])).
fof(f181, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f57])).
fof(f606, plain, (~ spl3_63 | spl3_53), inference(avatar_split_clause, [], [f182, f442, f484])).
fof(f182, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f57])).
fof(f605, plain, (~ spl3_64 | spl3_49), inference(avatar_split_clause, [], [f183, f425, f488])).
fof(f183, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f57])).
fof(f604, plain, (~ spl3_41 | spl3_46), inference(avatar_split_clause, [], [f184, f412, f391])).
fof(f184, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f57])).
fof(f603, plain, (~ spl3_43 | spl3_38), inference(avatar_split_clause, [], [f186, f378, f399])).
fof(f186, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f57])).
fof(f602, plain, (~ spl3_44 | spl3_34), inference(avatar_split_clause, [], [f187, f361, f403])).
fof(f187, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f57])).
fof(f601, plain, (~ spl3_21 | spl3_31), inference(avatar_split_clause, [], [f188, f348, f306])).
fof(f188, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f57])).
fof(f600, plain, (~ spl3_22 | spl3_27), inference(avatar_split_clause, [], [f189, f331, f310])).
fof(f189, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f57])).
fof(f599, plain, (~ spl3_24 | spl3_19), inference(avatar_split_clause, [], [f191, f297, f318])).
fof(f191, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f57])).
fof(f598, plain, (~ spl3_1 | spl3_16), inference(avatar_split_clause, [], [f192, f284, f221])).
fof(f192, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f57])).
fof(f597, plain, (~ spl3_2 | spl3_12), inference(avatar_split_clause, [], [f193, f267, f225])).
fof(f193, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f57])).
fof(f596, plain, (~ spl3_3 | spl3_8), inference(avatar_split_clause, [], [f194, f250, f229])).
fof(f194, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f57])).
fof(f595, plain, (~ spl3_75 | ~ spl3_79), inference(avatar_split_clause, [], [f171, f592, f573])).
fof(f171, plain, (~ (op(e0, e0) = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (op(e0, e3) = op(e3, op(e0, e3))) & ~ (op(e0, e2) = op(e2, op(e0, e2))) & ~ (op(e0, e1) = op(e1, op(e0, e1))) & ~ (op(e0, e0) = op(e0, op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f590, plain, (~ spl3_75 | ~ spl3_78), inference(avatar_split_clause, [], [f172, f587, f573])).
fof(f172, plain, (~ (op(e0, e1) = op(e1, op(e0, e1))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f585, plain, (~ spl3_75 | ~ spl3_77), inference(avatar_split_clause, [], [f173, f582, f573])).
fof(f173, plain, (~ (op(e0, e2) = op(e2, op(e0, e2))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f580, plain, (~ spl3_75 | ~ spl3_76), inference(avatar_split_clause, [], [f174, f577, f573])).
fof(f174, plain, (~ (op(e0, e3) = op(e3, op(e0, e3))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f571, plain, (~ spl3_70 | ~ spl3_74), inference(avatar_split_clause, [], [f167, f568, f549])).
fof(f167, plain, (~ (op(e1, e0) = op(e0, op(e1, e0))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (op(e1, e3) = op(e3, op(e1, e3))) & ~ (op(e1, e2) = op(e2, op(e1, e2))) & ~ (op(e1, e1) = op(e1, op(e1, e1))) & ~ (op(e1, e0) = op(e0, op(e1, e0)))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f566, plain, (~ spl3_70 | ~ spl3_73), inference(avatar_split_clause, [], [f168, f563, f549])).
fof(f168, plain, (~ (op(e1, e1) = op(e1, op(e1, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f561, plain, (~ spl3_70 | ~ spl3_72), inference(avatar_split_clause, [], [f169, f558, f549])).
fof(f169, plain, (~ (op(e1, e2) = op(e2, op(e1, e2))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f556, plain, (~ spl3_70 | ~ spl3_71), inference(avatar_split_clause, [], [f170, f553, f549])).
fof(f170, plain, (~ (op(e1, e3) = op(e3, op(e1, e3))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f547, plain, (~ spl3_65 | ~ spl3_69), inference(avatar_split_clause, [], [f163, f544, f525])).
fof(f163, plain, (~ (op(e2, e0) = op(e0, op(e2, e0))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (op(e2, e3) = op(e3, op(e2, e3))) & ~ (op(e2, e2) = op(e2, op(e2, e2))) & ~ (op(e2, e1) = op(e1, op(e2, e1))) & ~ (op(e2, e0) = op(e0, op(e2, e0)))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f542, plain, (~ spl3_65 | ~ spl3_68), inference(avatar_split_clause, [], [f164, f539, f525])).
fof(f164, plain, (~ (op(e2, e1) = op(e1, op(e2, e1))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f537, plain, (~ spl3_65 | ~ spl3_67), inference(avatar_split_clause, [], [f165, f534, f525])).
fof(f165, plain, (~ (op(e2, e2) = op(e2, op(e2, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f532, plain, (~ spl3_65 | ~ spl3_66), inference(avatar_split_clause, [], [f166, f529, f525])).
fof(f166, plain, (~ (op(e2, e3) = op(e3, op(e2, e3))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f522, plain, (spl3_61 | spl3_45 | spl3_29 | spl3_13), inference(avatar_split_clause, [], [f78, f272, f340, f408, f476])).
fof(f78, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax2)).
fof(f521, plain, (spl3_62 | spl3_58 | spl3_54 | spl3_50), inference(avatar_split_clause, [], [f79, f429, f446, f463, f480])).
fof(f79, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f520, plain, (spl3_62 | spl3_46 | spl3_30 | spl3_14), inference(avatar_split_clause, [], [f80, f276, f344, f412, f480])).
fof(f80, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f519, plain, (spl3_63 | spl3_59 | spl3_55 | spl3_51), inference(avatar_split_clause, [], [f81, f433, f450, f467, f484])).
fof(f81, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f518, plain, (spl3_63 | spl3_47 | spl3_31 | spl3_15), inference(avatar_split_clause, [], [f82, f280, f348, f416, f484])).
fof(f82, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f517, plain, (spl3_64 | spl3_60 | spl3_56 | spl3_52), inference(avatar_split_clause, [], [f83, f437, f454, f471, f488])).
fof(f83, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f516, plain, (spl3_64 | spl3_48 | spl3_32 | spl3_16), inference(avatar_split_clause, [], [f84, f284, f352, f420, f488])).
fof(f84, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f515, plain, (spl3_45 | spl3_41 | spl3_37 | spl3_33), inference(avatar_split_clause, [], [f85, f357, f374, f391, f408])).
fof(f85, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f512, plain, (spl3_58 | spl3_42 | spl3_26 | spl3_10), inference(avatar_split_clause, [], [f88, f259, f327, f395, f463])).
fof(f88, plain, ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f511, plain, (spl3_47 | spl3_43 | spl3_39 | spl3_35), inference(avatar_split_clause, [], [f89, f365, f382, f399, f416])).
fof(f89, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f509, plain, (spl3_48 | spl3_44 | spl3_40 | spl3_36), inference(avatar_split_clause, [], [f91, f369, f386, f403, f420])).
fof(f91, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f508, plain, (spl3_60 | spl3_44 | spl3_28 | spl3_12), inference(avatar_split_clause, [], [f92, f267, f335, f403, f471])).
fof(f92, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f507, plain, (spl3_29 | spl3_25 | spl3_21 | spl3_17), inference(avatar_split_clause, [], [f93, f289, f306, f323, f340])).
fof(f93, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f506, plain, (spl3_53 | spl3_37 | spl3_21 | spl3_5), inference(avatar_split_clause, [], [f94, f238, f306, f374, f442])).
fof(f94, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f505, plain, (spl3_30 | spl3_26 | spl3_22 | spl3_18), inference(avatar_split_clause, [], [f95, f293, f310, f327, f344])).
fof(f95, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f504, plain, (spl3_54 | spl3_38 | spl3_22 | spl3_6), inference(avatar_split_clause, [], [f96, f242, f310, f378, f446])).
fof(f96, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f502, plain, (spl3_55 | spl3_39 | spl3_23 | spl3_7), inference(avatar_split_clause, [], [f98, f246, f314, f382, f450])).
fof(f98, plain, ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f501, plain, (spl3_32 | spl3_28 | spl3_24 | spl3_20), inference(avatar_split_clause, [], [f99, f301, f318, f335, f352])).
fof(f99, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f500, plain, (spl3_56 | spl3_40 | spl3_24 | spl3_8), inference(avatar_split_clause, [], [f100, f250, f318, f386, f454])).
fof(f100, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f499, plain, (spl3_13 | spl3_9 | spl3_5 | spl3_1), inference(avatar_split_clause, [], [f101, f221, f238, f255, f272])).
fof(f101, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f498, plain, (spl3_49 | spl3_33 | spl3_17 | spl3_1), inference(avatar_split_clause, [], [f102, f221, f289, f357, f425])).
fof(f102, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f497, plain, (spl3_14 | spl3_10 | spl3_6 | spl3_2), inference(avatar_split_clause, [], [f103, f225, f242, f259, f276])).
fof(f103, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f496, plain, (spl3_50 | spl3_34 | spl3_18 | spl3_2), inference(avatar_split_clause, [], [f104, f225, f293, f361, f429])).
fof(f104, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f495, plain, (spl3_15 | spl3_11 | spl3_7 | spl3_3), inference(avatar_split_clause, [], [f105, f229, f246, f263, f280])).
fof(f105, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f494, plain, (spl3_51 | spl3_35 | spl3_19 | spl3_3), inference(avatar_split_clause, [], [f106, f229, f297, f365, f433])).
fof(f106, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f493, plain, (spl3_16 | spl3_12 | spl3_8 | spl3_4), inference(avatar_split_clause, [], [f107, f233, f250, f267, f284])).
fof(f107, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f492, plain, (spl3_52 | spl3_36 | spl3_20 | spl3_4), inference(avatar_split_clause, [], [f108, f233, f301, f369, f437])).
fof(f108, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f491, plain, (spl3_61 | spl3_62 | spl3_63 | spl3_64), inference(avatar_split_clause, [], [f61, f488, f484, f480, f476])).
fof(f61, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG150+1.p', ax1)).
fof(f474, plain, (spl3_57 | spl3_58 | spl3_59 | spl3_60), inference(avatar_split_clause, [], [f62, f471, f467, f463, f459])).
fof(f62, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f457, plain, (spl3_53 | spl3_54 | spl3_55 | spl3_56), inference(avatar_split_clause, [], [f63, f454, f450, f446, f442])).
fof(f63, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f440, plain, (spl3_49 | spl3_50 | spl3_51 | spl3_52), inference(avatar_split_clause, [], [f64, f437, f433, f429, f425])).
fof(f64, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f423, plain, (spl3_45 | spl3_46 | spl3_47 | spl3_48), inference(avatar_split_clause, [], [f65, f420, f416, f412, f408])).
fof(f65, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f406, plain, (spl3_41 | spl3_42 | spl3_43 | spl3_44), inference(avatar_split_clause, [], [f66, f403, f399, f395, f391])).
fof(f66, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f389, plain, (spl3_37 | spl3_38 | spl3_39 | spl3_40), inference(avatar_split_clause, [], [f67, f386, f382, f378, f374])).
fof(f67, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f372, plain, (spl3_33 | spl3_34 | spl3_35 | spl3_36), inference(avatar_split_clause, [], [f68, f369, f365, f361, f357])).
fof(f68, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f355, plain, (spl3_29 | spl3_30 | spl3_31 | spl3_32), inference(avatar_split_clause, [], [f69, f352, f348, f344, f340])).
fof(f69, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f338, plain, (spl3_25 | spl3_26 | spl3_27 | spl3_28), inference(avatar_split_clause, [], [f70, f335, f331, f327, f323])).
fof(f70, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f321, plain, (spl3_21 | spl3_22 | spl3_23 | spl3_24), inference(avatar_split_clause, [], [f71, f318, f314, f310, f306])).
fof(f71, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f304, plain, (spl3_17 | spl3_18 | spl3_19 | spl3_20), inference(avatar_split_clause, [], [f72, f301, f297, f293, f289])).
fof(f72, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f287, plain, (spl3_13 | spl3_14 | spl3_15 | spl3_16), inference(avatar_split_clause, [], [f73, f284, f280, f276, f272])).
fof(f73, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f270, plain, (spl3_9 | spl3_10 | spl3_11 | spl3_12), inference(avatar_split_clause, [], [f74, f267, f263, f259, f255])).
fof(f74, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f253, plain, (spl3_5 | spl3_6 | spl3_7 | spl3_8), inference(avatar_split_clause, [], [f75, f250, f246, f242, f238])).
fof(f75, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f236, plain, (spl3_1 | spl3_2 | spl3_3 | spl3_4), inference(avatar_split_clause, [], [f76, f233, f229, f225, f221])).
fof(f76, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).