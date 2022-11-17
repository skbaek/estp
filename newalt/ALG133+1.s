fof(f3776, plain, $false, inference(avatar_sat_refutation, [], [f327, f344, f361, f378, f395, f412, f429, f446, f463, f480, f497, f514, f531, f548, f565, f582, f583, f584, f585, f586, f587, f588, f590, f591, f592, f594, f595, f596, f598, f599, f600, f601, f602, f604, f606, f607, f608, f611, f612, f613, f614, f619, f620, f623, f624, f629, f630, f631, f632, f633, f634, f639, f640, f641, f642, f643, f644, f649, f650, f651, f653, f654, f660, f664, f669, f670, f671, f672, f673, f674, f680, f681, f682, f683, f684, f689, f690, f691, f693, f694, f699, f700, f701, f703, f704, f711, f714, f721, f722, f723, f724, f729, f730, f731, f732, f733, f734, f739, f740, f742, f743, f744, f749, f751, f752, f753, f754, f762, f764, f765, f770, f787, f804, f821, f838, f847, f856, f874, f883, f892, f897, f902, f907, f916, f921, f930, f931, f936, f937, f942, f947, f952, f953, f954, f955, f956, f957, f958, f972, f974, f976, f985, f986, f988, f994, f997, f998, f1003, f1005, f1009, f1029, f1046, f1049, f1082, f1094, f1101, f1104, f1109, f1112, f1122, f1162, f1193, f1201, f1225, f1249, f1280, f1284, f1289, f1316, f1323, f1327, f1347, f1386, f1388, f1390, f1398, f1402, f1405, f1418, f1419, f1422, f1423, f1451, f1459, f1470, f1490, f1494, f1499, f1530, f1537, f1539, f1555, f1559, f1567, f1579, f1591, f1612, f1631, f1632, f1654, f1658, f1662, f1673, f1706, f1713, f1720, f1726, f1752, f1755, f1759, f1769, f1780, f1793, f1808, f1825, f1834, f1836, f1860, f1882, f1889, f1893, f1918, f1932, f1938, f1965, f1978, f1995, f2006, f2029, f2054, f2065, f2080, f2085, f2089, f2105, f2117, f2122, f2123, f2139, f2149, f2162, f2168, f2188, f2193, f2203, f2236, f2249, f2257, f2273, f2278, f2282, f2288, f2309, f2319, f2321, f2325, f2352, f2391, f2403, f2409, f2423, f2436, f2454, f2476, f2481, f2507, f2529, f2550, f2571, f2573, f2581, f2584, f2603, f2613, f2625, f2635, f2641, f2651, f2652, f2661, f2683, f2686, f2720, f2722, f2739, f2763, f2764, f2766, f2779, f2800, f2842, f2846, f2855, f2856, f2879, f2883, f2892, f2906, f2909, f2912, f2918, f2922, f2936, f2942, f2957, f2967, f2992, f2997, f3005, f3023, f3054, f3067, f3069, f3073, f3084, f3091, f3100, f3124, f3131, f3137, f3144, f3148, f3155, f3160, f3161, f3166, f3174, f3184, f3212, f3213, f3214, f3216, f3217, f3225, f3234, f3242, f3252, f3262, f3263, f3274, f3275, f3283, f3295, f3315, f3331, f3346, f3347, f3352, f3353, f3360, f3361, f3366, f3367, f3368, f3383, f3388, f3395, f3423, f3437, f3438, f3444, f3451, f3469, f3472, f3484, f3489, f3550, f3551, f3572, f3573, f3581, f3591, f3594, f3600, f3606, f3613, f3624, f3626, f3656, f3663, f3680, f3691, f3692, f3699, f3700, f3706, f3707, f3734, f3736, f3743, f3759, f3761, f3770])).
fof(f3770, plain, (~ spl15_8 | ~ spl15_12), inference(avatar_split_clause, [], [f3768, f358, f341])).
fof(f341, plain, (spl15_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f358, plain, (spl15_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_12])])).
fof(f3768, plain, (~ (e3 = op(e3, e2)) | ~ spl15_12), inference(backward_demodulation, [], [f178, f360])).
fof(f360, plain, ((e3 = op(e3, e1)) | ~ spl15_12), inference(avatar_component_clause, [], [f358])).
fof(f178, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax3)).
fof(f3761, plain, (~ spl15_2 | ~ spl15_24 | spl15_98), inference(avatar_contradiction_clause, [], [f3760])).
fof(f3760, plain, ($false | (~ spl15_2 | ~ spl15_24 | spl15_98)), inference(subsumption_resolution, [], [f3755, f318])).
fof(f318, plain, ((e1 = op(e3, e3)) | ~ spl15_2), inference(avatar_component_clause, [], [f316])).
fof(f316, plain, (spl15_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_2])])).
fof(f3755, plain, (~ (e1 = op(e3, e3)) | (~ spl15_24 | spl15_98)), inference(backward_demodulation, [], [f851, f411])).
fof(f411, plain, ((e3 = op(e2, e2)) | ~ spl15_24), inference(avatar_component_clause, [], [f409])).
fof(f409, plain, (spl15_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_24])])).
fof(f851, plain, (~ (e1 = op(op(e2, e2), op(e2, e2))) | spl15_98), inference(avatar_component_clause, [], [f849])).
fof(f849, plain, (spl15_98 <=> (e1 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_98])])).
fof(f3759, plain, (~ spl15_8 | ~ spl15_24), inference(avatar_split_clause, [], [f3753, f409, f341])).
fof(f3753, plain, (~ (e3 = op(e3, e2)) | ~ spl15_24), inference(backward_demodulation, [], [f150, f411])).
fof(f150, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3743, plain, (~ spl15_22 | ~ spl15_30), inference(avatar_split_clause, [], [f3738, f435, f401])).
fof(f401, plain, (spl15_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_22])])).
fof(f435, plain, (spl15_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_30])])).
fof(f3738, plain, (~ (e1 = op(e2, e2)) | ~ spl15_30), inference(backward_demodulation, [], [f170, f437])).
fof(f437, plain, ((e1 = op(e2, e0)) | ~ spl15_30), inference(avatar_component_clause, [], [f435])).
fof(f170, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3736, plain, (~ spl15_33 | spl15_57 | ~ spl15_81), inference(avatar_contradiction_clause, [], [f3735])).
fof(f3735, plain, ($false | (~ spl15_33 | spl15_57 | ~ spl15_81)), inference(subsumption_resolution, [], [f3733, f551])).
fof(f551, plain, (~ (e0 = op(e0, e1)) | spl15_57), inference(avatar_component_clause, [], [f550])).
fof(f550, plain, (spl15_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_57])])).
fof(f3733, plain, ((e0 = op(e0, e1)) | (~ spl15_33 | ~ spl15_81)), inference(backward_demodulation, [], [f778, f450])).
fof(f450, plain, ((e0 = op(e1, e3)) | ~ spl15_33), inference(avatar_component_clause, [], [f448])).
fof(f448, plain, (spl15_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_33])])).
fof(f778, plain, ((op(e1, e3) = op(op(e1, e3), e1)) | ~ spl15_81), inference(avatar_component_clause, [], [f776])).
fof(f776, plain, (spl15_81 <=> (op(e1, e3) = op(op(e1, e3), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_81])])).
fof(f3734, plain, (~ spl15_17 | ~ spl15_33), inference(avatar_split_clause, [], [f3729, f448, f380])).
fof(f380, plain, (spl15_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_17])])).
fof(f3729, plain, (~ (e0 = op(e2, e3)) | ~ spl15_33), inference(backward_demodulation, [], [f154, f450])).
fof(f154, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3707, plain, (~ spl15_44 | ~ spl15_48), inference(avatar_split_clause, [], [f3702, f511, f494])).
fof(f494, plain, (spl15_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_44])])).
fof(f511, plain, (spl15_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_48])])).
fof(f3702, plain, (~ (e3 = op(e1, e1)) | ~ spl15_48), inference(backward_demodulation, [], [f163, f513])).
fof(f513, plain, ((e3 = op(e1, e0)) | ~ spl15_48), inference(avatar_component_clause, [], [f511])).
fof(f163, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3706, plain, (~ spl15_32 | ~ spl15_48), inference(avatar_split_clause, [], [f3701, f511, f443])).
fof(f443, plain, (spl15_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_32])])).
fof(f3701, plain, (~ (e3 = op(e2, e0)) | ~ spl15_48), inference(backward_demodulation, [], [f136, f513])).
fof(f136, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3700, plain, (spl15_46 | ~ spl15_54 | ~ spl15_84), inference(avatar_split_clause, [], [f3698, f789, f537, f503])).
fof(f503, plain, (spl15_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_46])])).
fof(f537, plain, (spl15_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_54])])).
fof(f789, plain, (spl15_84 <=> (op(e0, e2) = op(op(e0, e2), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_84])])).
fof(f3698, plain, ((e1 = op(e1, e0)) | (~ spl15_54 | ~ spl15_84)), inference(backward_demodulation, [], [f791, f539])).
fof(f539, plain, ((e1 = op(e0, e2)) | ~ spl15_54), inference(avatar_component_clause, [], [f537])).
fof(f791, plain, ((op(e0, e2) = op(op(e0, e2), e0)) | ~ spl15_84), inference(avatar_component_clause, [], [f789])).
fof(f3699, plain, (~ spl15_22 | ~ spl15_54), inference(avatar_split_clause, [], [f3694, f537, f401])).
fof(f3694, plain, (~ (e1 = op(e2, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f146, f539])).
fof(f146, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3692, plain, (~ spl15_55 | ~ spl15_59), inference(avatar_split_clause, [], [f3688, f558, f541])).
fof(f541, plain, (spl15_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_55])])).
fof(f558, plain, (spl15_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_59])])).
fof(f3688, plain, (~ (e2 = op(e0, e2)) | ~ spl15_59), inference(backward_demodulation, [], [f160, f560])).
fof(f560, plain, ((e2 = op(e0, e1)) | ~ spl15_59), inference(avatar_component_clause, [], [f558])).
fof(f160, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3691, plain, (~ spl15_27 | ~ spl15_59), inference(avatar_split_clause, [], [f3686, f558, f422])).
fof(f422, plain, (spl15_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_27])])).
fof(f3686, plain, (~ (e2 = op(e2, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f140, f560])).
fof(f140, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3680, plain, (spl15_19 | ~ spl15_15 | ~ spl15_95), inference(avatar_split_clause, [], [f3679, f835, f371, f388])).
fof(f388, plain, (spl15_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_19])])).
fof(f371, plain, (spl15_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_15])])).
fof(f835, plain, (spl15_95 <=> (op(e3, e0) = op(op(e3, e0), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_95])])).
fof(f3679, plain, ((e2 = op(e2, e3)) | (~ spl15_15 | ~ spl15_95)), inference(forward_demodulation, [], [f837, f373])).
fof(f373, plain, ((e2 = op(e3, e0)) | ~ spl15_15), inference(avatar_component_clause, [], [f371])).
fof(f837, plain, ((op(e3, e0) = op(op(e3, e0), e3)) | ~ spl15_95), inference(avatar_component_clause, [], [f835])).
fof(f3663, plain, (~ spl15_22 | ~ spl15_37 | ~ spl15_86), inference(avatar_contradiction_clause, [], [f3662])).
fof(f3662, plain, ($false | (~ spl15_22 | ~ spl15_37 | ~ spl15_86)), inference(subsumption_resolution, [], [f3661, f181])).
fof(f181, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax4)).
fof(f3661, plain, ((e0 = e1) | (~ spl15_22 | ~ spl15_37 | ~ spl15_86)), inference(forward_demodulation, [], [f3660, f467])).
fof(f467, plain, ((e0 = op(e1, e2)) | ~ spl15_37), inference(avatar_component_clause, [], [f465])).
fof(f465, plain, (spl15_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_37])])).
fof(f3660, plain, ((e1 = op(e1, e2)) | (~ spl15_22 | ~ spl15_86)), inference(forward_demodulation, [], [f799, f403])).
fof(f403, plain, ((e1 = op(e2, e2)) | ~ spl15_22), inference(avatar_component_clause, [], [f401])).
fof(f799, plain, ((op(e2, e2) = op(op(e2, e2), e2)) | ~ spl15_86), inference(avatar_component_clause, [], [f797])).
fof(f797, plain, (spl15_86 <=> (op(e2, e2) = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_86])])).
fof(f3656, plain, (~ spl15_22 | ~ spl15_37 | spl15_113), inference(avatar_contradiction_clause, [], [f3655])).
fof(f3655, plain, ($false | (~ spl15_22 | ~ spl15_37 | spl15_113)), inference(subsumption_resolution, [], [f3654, f467])).
fof(f3654, plain, (~ (e0 = op(e1, e2)) | (~ spl15_22 | spl15_113)), inference(forward_demodulation, [], [f920, f403])).
fof(f920, plain, (~ (e0 = op(op(e2, e2), e2)) | spl15_113), inference(avatar_component_clause, [], [f918])).
fof(f918, plain, (spl15_113 <=> (e0 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_113])])).
fof(f3626, plain, (~ spl15_22 | ~ spl15_44 | spl15_104), inference(avatar_contradiction_clause, [], [f3625])).
fof(f3625, plain, ($false | (~ spl15_22 | ~ spl15_44 | spl15_104)), inference(subsumption_resolution, [], [f3620, f496])).
fof(f496, plain, ((e3 = op(e1, e1)) | ~ spl15_44), inference(avatar_component_clause, [], [f494])).
fof(f3620, plain, (~ (e3 = op(e1, e1)) | (~ spl15_22 | spl15_104)), inference(backward_demodulation, [], [f878, f403])).
fof(f878, plain, (~ (e3 = op(op(e2, e2), op(e2, e2))) | spl15_104), inference(avatar_component_clause, [], [f876])).
fof(f876, plain, (spl15_104 <=> (e3 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_104])])).
fof(f3624, plain, (~ spl15_22 | ~ spl15_37 | ~ spl15_99), inference(avatar_contradiction_clause, [], [f3623])).
fof(f3623, plain, ($false | (~ spl15_22 | ~ spl15_37 | ~ spl15_99)), inference(subsumption_resolution, [], [f3622, f183])).
fof(f183, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f3622, plain, ((e0 = e3) | (~ spl15_22 | ~ spl15_37 | ~ spl15_99)), inference(forward_demodulation, [], [f3619, f467])).
fof(f3619, plain, ((e3 = op(e1, e2)) | (~ spl15_22 | ~ spl15_99)), inference(backward_demodulation, [], [f854, f403])).
fof(f854, plain, ((e3 = op(op(e2, e2), e2)) | ~ spl15_99), inference(avatar_component_clause, [], [f853])).
fof(f853, plain, (spl15_99 <=> (e3 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_99])])).
fof(f3613, plain, (~ spl15_19 | ~ spl15_27), inference(avatar_split_clause, [], [f3610, f422, f388])).
fof(f3610, plain, (~ (e2 = op(e2, e3)) | ~ spl15_27), inference(backward_demodulation, [], [f173, f424])).
fof(f424, plain, ((e2 = op(e2, e1)) | ~ spl15_27), inference(avatar_component_clause, [], [f422])).
fof(f173, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3606, plain, (~ spl15_28 | ~ spl15_32), inference(avatar_split_clause, [], [f3602, f443, f426])).
fof(f426, plain, (spl15_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_28])])).
fof(f3602, plain, (~ (e3 = op(e2, e1)) | ~ spl15_32), inference(backward_demodulation, [], [f169, f445])).
fof(f445, plain, ((e3 = op(e2, e0)) | ~ spl15_32), inference(avatar_component_clause, [], [f443])).
fof(f169, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3600, plain, (spl15_27 | ~ spl15_35 | ~ spl15_81), inference(avatar_split_clause, [], [f3598, f776, f456, f422])).
fof(f456, plain, (spl15_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_35])])).
fof(f3598, plain, ((e2 = op(e2, e1)) | (~ spl15_35 | ~ spl15_81)), inference(backward_demodulation, [], [f778, f458])).
fof(f458, plain, ((e2 = op(e1, e3)) | ~ spl15_35), inference(avatar_component_clause, [], [f456])).
fof(f3594, plain, (~ spl15_37 | spl15_57 | ~ spl15_85), inference(avatar_contradiction_clause, [], [f3593])).
fof(f3593, plain, ($false | (~ spl15_37 | spl15_57 | ~ spl15_85)), inference(subsumption_resolution, [], [f3590, f551])).
fof(f3590, plain, ((e0 = op(e0, e1)) | (~ spl15_37 | ~ spl15_85)), inference(backward_demodulation, [], [f795, f467])).
fof(f795, plain, ((op(e1, e2) = op(op(e1, e2), e1)) | ~ spl15_85), inference(avatar_component_clause, [], [f793])).
fof(f793, plain, (spl15_85 <=> (op(e1, e2) = op(op(e1, e2), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_85])])).
fof(f3591, plain, (~ spl15_21 | ~ spl15_37), inference(avatar_split_clause, [], [f3584, f465, f397])).
fof(f397, plain, (spl15_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_21])])).
fof(f3584, plain, (~ (e0 = op(e2, e2)) | ~ spl15_37), inference(backward_demodulation, [], [f148, f467])).
fof(f148, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3581, plain, (~ spl15_28 | ~ spl15_44), inference(avatar_split_clause, [], [f3574, f494, f426])).
fof(f3574, plain, (~ (e3 = op(e2, e1)) | ~ spl15_44), inference(backward_demodulation, [], [f142, f496])).
fof(f142, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3573, plain, (~ spl15_38 | ~ spl15_46), inference(avatar_split_clause, [], [f3567, f503, f469])).
fof(f469, plain, (spl15_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_38])])).
fof(f3567, plain, (~ (e1 = op(e1, e2)) | ~ spl15_46), inference(backward_demodulation, [], [f164, f505])).
fof(f505, plain, ((e1 = op(e1, e0)) | ~ spl15_46), inference(avatar_component_clause, [], [f503])).
fof(f164, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3572, plain, (~ spl15_30 | ~ spl15_46), inference(avatar_split_clause, [], [f3564, f503, f435])).
fof(f3564, plain, (~ (e1 = op(e2, e0)) | ~ spl15_46), inference(backward_demodulation, [], [f136, f505])).
fof(f3551, plain, (spl15_46 | ~ spl15_58 | ~ spl15_88), inference(avatar_split_clause, [], [f3548, f806, f554, f503])).
fof(f554, plain, (spl15_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_58])])).
fof(f806, plain, (spl15_88 <=> (op(e0, e1) = op(op(e0, e1), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_88])])).
fof(f3548, plain, ((e1 = op(e1, e0)) | (~ spl15_58 | ~ spl15_88)), inference(backward_demodulation, [], [f808, f556])).
fof(f556, plain, ((e1 = op(e0, e1)) | ~ spl15_58), inference(avatar_component_clause, [], [f554])).
fof(f808, plain, ((op(e0, e1) = op(op(e0, e1), e0)) | ~ spl15_88), inference(avatar_component_clause, [], [f806])).
fof(f3550, plain, (~ spl15_50 | ~ spl15_58), inference(avatar_split_clause, [], [f3545, f554, f520])).
fof(f520, plain, (spl15_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_50])])).
fof(f3545, plain, (~ (e1 = op(e0, e3)) | ~ spl15_58), inference(backward_demodulation, [], [f161, f556])).
fof(f161, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3489, plain, (~ spl15_14 | ~ spl15_30), inference(avatar_split_clause, [], [f3485, f435, f367])).
fof(f367, plain, (spl15_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f3485, plain, (~ (e1 = op(e3, e0)) | ~ spl15_30), inference(backward_demodulation, [], [f138, f437])).
fof(f138, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3484, plain, (~ spl15_1 | ~ spl15_33), inference(avatar_split_clause, [], [f3479, f448, f312])).
fof(f312, plain, (spl15_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f3479, plain, (~ (e0 = op(e3, e3)) | ~ spl15_33), inference(backward_demodulation, [], [f155, f450])).
fof(f155, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3472, plain, (~ spl15_21 | ~ spl15_43 | spl15_116), inference(avatar_contradiction_clause, [], [f3471])).
fof(f3471, plain, ($false | (~ spl15_21 | ~ spl15_43 | spl15_116)), inference(subsumption_resolution, [], [f3467, f399])).
fof(f399, plain, ((e0 = op(e2, e2)) | ~ spl15_21), inference(avatar_component_clause, [], [f397])).
fof(f3467, plain, (~ (e0 = op(e2, e2)) | (~ spl15_43 | spl15_116)), inference(backward_demodulation, [], [f935, f492])).
fof(f492, plain, ((e2 = op(e1, e1)) | ~ spl15_43), inference(avatar_component_clause, [], [f490])).
fof(f490, plain, (spl15_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_43])])).
fof(f935, plain, (~ (e0 = op(op(e1, e1), op(e1, e1))) | spl15_116), inference(avatar_component_clause, [], [f933])).
fof(f933, plain, (spl15_116 <=> (e0 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_116])])).
fof(f3469, plain, (~ spl15_39 | ~ spl15_43), inference(avatar_split_clause, [], [f3462, f490, f473])).
fof(f473, plain, (spl15_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_39])])).
fof(f3462, plain, (~ (e2 = op(e1, e2)) | ~ spl15_43), inference(backward_demodulation, [], [f166, f492])).
fof(f166, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f3451, plain, (~ spl15_18 | ~ spl15_50), inference(avatar_split_clause, [], [f3446, f520, f384])).
fof(f384, plain, (spl15_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_18])])).
fof(f3446, plain, (~ (e1 = op(e2, e3)) | ~ spl15_50), inference(backward_demodulation, [], [f152, f522])).
fof(f522, plain, ((e1 = op(e0, e3)) | ~ spl15_50), inference(avatar_component_clause, [], [f520])).
fof(f152, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3444, plain, (~ spl15_8 | ~ spl15_56), inference(avatar_split_clause, [], [f3442, f545, f341])).
fof(f545, plain, (spl15_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_56])])).
fof(f3442, plain, (~ (e3 = op(e3, e2)) | ~ spl15_56), inference(backward_demodulation, [], [f147, f547])).
fof(f547, plain, ((e3 = op(e0, e2)) | ~ spl15_56), inference(avatar_component_clause, [], [f545])).
fof(f147, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3438, plain, (~ spl15_51 | ~ spl15_63), inference(avatar_split_clause, [], [f3433, f575, f524])).
fof(f524, plain, (spl15_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_51])])).
fof(f575, plain, (spl15_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl15_63])])).
fof(f3433, plain, (~ (e2 = op(e0, e3)) | ~ spl15_63), inference(backward_demodulation, [], [f159, f577])).
fof(f577, plain, ((op(e0, e0) = e2) | ~ spl15_63), inference(avatar_component_clause, [], [f575])).
fof(f159, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3437, plain, (~ spl15_31 | ~ spl15_63), inference(avatar_split_clause, [], [f3430, f575, f439])).
fof(f439, plain, (spl15_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_31])])).
fof(f3430, plain, (~ (e2 = op(e2, e0)) | ~ spl15_63), inference(backward_demodulation, [], [f134, f577])).
fof(f134, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f3423, plain, (~ spl15_64 | ~ spl15_21 | spl15_104), inference(avatar_split_clause, [], [f3422, f876, f397, f579])).
fof(f579, plain, (spl15_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl15_64])])).
fof(f3422, plain, (~ (op(e0, e0) = e3) | (~ spl15_21 | spl15_104)), inference(forward_demodulation, [], [f878, f399])).
fof(f3395, plain, (~ spl15_2 | ~ spl15_14), inference(avatar_split_clause, [], [f3391, f367, f316])).
fof(f3391, plain, (~ (e1 = op(e3, e3)) | ~ spl15_14), inference(backward_demodulation, [], [f177, f369])).
fof(f369, plain, ((e1 = op(e3, e0)) | ~ spl15_14), inference(avatar_component_clause, [], [f367])).
fof(f177, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3388, plain, (~ spl15_2 | ~ spl15_18), inference(avatar_split_clause, [], [f3384, f384, f316])).
fof(f3384, plain, (~ (e1 = op(e3, e3)) | ~ spl15_18), inference(backward_demodulation, [], [f156, f386])).
fof(f386, plain, ((e1 = op(e2, e3)) | ~ spl15_18), inference(avatar_component_clause, [], [f384])).
fof(f156, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3383, plain, (~ spl15_21 | ~ spl15_54 | spl15_105), inference(avatar_contradiction_clause, [], [f3382])).
fof(f3382, plain, ($false | (~ spl15_21 | ~ spl15_54 | spl15_105)), inference(subsumption_resolution, [], [f3378, f539])).
fof(f3378, plain, (~ (e1 = op(e0, e2)) | (~ spl15_21 | spl15_105)), inference(backward_demodulation, [], [f882, f399])).
fof(f882, plain, (~ (e1 = op(op(e2, e2), e2)) | spl15_105), inference(avatar_component_clause, [], [f880])).
fof(f880, plain, (spl15_105 <=> (e1 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_105])])).
fof(f3368, plain, (~ spl15_8 | ~ spl15_28 | spl15_90), inference(avatar_split_clause, [], [f3365, f814, f426, f341])).
fof(f814, plain, (spl15_90 <=> (op(e2, e1) = op(op(e2, e1), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_90])])).
fof(f3365, plain, (~ (e3 = op(e3, e2)) | (~ spl15_28 | spl15_90)), inference(backward_demodulation, [], [f815, f428])).
fof(f428, plain, ((e3 = op(e2, e1)) | ~ spl15_28), inference(avatar_component_clause, [], [f426])).
fof(f815, plain, (~ (op(e2, e1) = op(op(e2, e1), e2)) | spl15_90), inference(avatar_component_clause, [], [f814])).
fof(f3367, plain, (~ spl15_24 | ~ spl15_28), inference(avatar_split_clause, [], [f3363, f426, f409])).
fof(f3363, plain, (~ (e3 = op(e2, e2)) | ~ spl15_28), inference(backward_demodulation, [], [f172, f428])).
fof(f172, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3366, plain, (~ spl15_12 | ~ spl15_28), inference(avatar_split_clause, [], [f3362, f426, f358])).
fof(f3362, plain, (~ (e3 = op(e3, e1)) | ~ spl15_28), inference(backward_demodulation, [], [f144, f428])).
fof(f144, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3361, plain, (~ spl15_19 | ~ spl15_31), inference(avatar_split_clause, [], [f3357, f439, f388])).
fof(f3357, plain, (~ (e2 = op(e2, e3)) | ~ spl15_31), inference(backward_demodulation, [], [f171, f441])).
fof(f441, plain, ((e2 = op(e2, e0)) | ~ spl15_31), inference(avatar_component_clause, [], [f439])).
fof(f171, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3360, plain, (~ spl15_15 | ~ spl15_31), inference(avatar_split_clause, [], [f3354, f439, f371])).
fof(f3354, plain, (~ (e2 = op(e3, e0)) | ~ spl15_31), inference(backward_demodulation, [], [f138, f441])).
fof(f3353, plain, (~ spl15_19 | ~ spl15_51), inference(avatar_split_clause, [], [f3349, f524, f388])).
fof(f3349, plain, (~ (e2 = op(e2, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f152, f526])).
fof(f526, plain, ((e2 = op(e0, e3)) | ~ spl15_51), inference(avatar_component_clause, [], [f524])).
fof(f3352, plain, (spl15_31 | ~ spl15_51 | ~ spl15_80), inference(avatar_split_clause, [], [f3348, f772, f524, f439])).
fof(f772, plain, (spl15_80 <=> (op(e0, e3) = op(op(e0, e3), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_80])])).
fof(f3348, plain, ((e2 = op(e2, e0)) | (~ spl15_51 | ~ spl15_80)), inference(backward_demodulation, [], [f774, f526])).
fof(f774, plain, ((op(e0, e3) = op(op(e0, e3), e0)) | ~ spl15_80), inference(avatar_component_clause, [], [f772])).
fof(f3347, plain, (~ spl15_49 | ~ spl15_57), inference(avatar_split_clause, [], [f3341, f550, f516])).
fof(f516, plain, (spl15_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_49])])).
fof(f3341, plain, (~ (e0 = op(e0, e3)) | ~ spl15_57), inference(backward_demodulation, [], [f161, f552])).
fof(f552, plain, ((e0 = op(e0, e1)) | ~ spl15_57), inference(avatar_component_clause, [], [f550])).
fof(f3346, plain, (~ spl15_25 | ~ spl15_57), inference(avatar_split_clause, [], [f3339, f550, f414])).
fof(f414, plain, (spl15_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_25])])).
fof(f3339, plain, (~ (e0 = op(e2, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f140, f552])).
fof(f3331, plain, (~ spl15_2 | ~ spl15_64 | spl15_117), inference(avatar_split_clause, [], [f3330, f939, f579, f316])).
fof(f939, plain, (spl15_117 <=> (e1 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_117])])).
fof(f3330, plain, (~ (e1 = op(e3, e3)) | (~ spl15_64 | spl15_117)), inference(forward_demodulation, [], [f941, f581])).
fof(f581, plain, ((op(e0, e0) = e3) | ~ spl15_64), inference(avatar_component_clause, [], [f579])).
fof(f941, plain, (~ (e1 = op(op(e0, e0), op(e0, e0))) | spl15_117), inference(avatar_component_clause, [], [f939])).
fof(f3315, plain, (~ spl15_1 | ~ spl15_5), inference(avatar_split_clause, [], [f3310, f329, f312])).
fof(f329, plain, (spl15_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_5])])).
fof(f3310, plain, (~ (e0 = op(e3, e3)) | ~ spl15_5), inference(backward_demodulation, [], [f180, f331])).
fof(f331, plain, ((e0 = op(e3, e2)) | ~ spl15_5), inference(avatar_component_clause, [], [f329])).
fof(f180, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3295, plain, (~ spl15_20 | ~ spl15_24), inference(avatar_split_clause, [], [f3292, f409, f392])).
fof(f392, plain, (spl15_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_20])])).
fof(f3292, plain, (~ (e3 = op(e2, e3)) | ~ spl15_24), inference(backward_demodulation, [], [f174, f411])).
fof(f174, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f3283, plain, (~ spl15_26 | ~ spl15_30), inference(avatar_split_clause, [], [f3277, f435, f418])).
fof(f418, plain, (spl15_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_26])])).
fof(f3277, plain, (~ (e1 = op(e2, e1)) | ~ spl15_30), inference(backward_demodulation, [], [f169, f437])).
fof(f3275, plain, (spl15_12 | ~ spl15_36 | ~ spl15_81), inference(avatar_split_clause, [], [f3273, f776, f460, f358])).
fof(f460, plain, (spl15_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_36])])).
fof(f3273, plain, ((e3 = op(e3, e1)) | (~ spl15_36 | ~ spl15_81)), inference(backward_demodulation, [], [f778, f462])).
fof(f462, plain, ((e3 = op(e1, e3)) | ~ spl15_36), inference(avatar_component_clause, [], [f460])).
fof(f3274, plain, (~ spl15_20 | ~ spl15_36), inference(avatar_split_clause, [], [f3271, f460, f392])).
fof(f3271, plain, (~ (e3 = op(e2, e3)) | ~ spl15_36), inference(backward_demodulation, [], [f154, f462])).
fof(f3263, plain, (~ spl15_34 | ~ spl15_42), inference(avatar_split_clause, [], [f3257, f486, f452])).
fof(f452, plain, (spl15_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_34])])).
fof(f486, plain, (spl15_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_42])])).
fof(f3257, plain, (~ (e1 = op(e1, e3)) | ~ spl15_42), inference(backward_demodulation, [], [f167, f488])).
fof(f488, plain, ((e1 = op(e1, e1)) | ~ spl15_42), inference(avatar_component_clause, [], [f486])).
fof(f167, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3262, plain, (~ spl15_26 | ~ spl15_42), inference(avatar_split_clause, [], [f3254, f486, f418])).
fof(f3254, plain, (~ (e1 = op(e2, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f142, f488])).
fof(f3252, plain, (~ spl15_29 | ~ spl15_45), inference(avatar_split_clause, [], [f3243, f499, f431])).
fof(f431, plain, (spl15_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_29])])).
fof(f499, plain, (spl15_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_45])])).
fof(f3243, plain, (~ (e0 = op(e2, e0)) | ~ spl15_45), inference(backward_demodulation, [], [f136, f501])).
fof(f501, plain, ((e0 = op(e1, e0)) | ~ spl15_45), inference(avatar_component_clause, [], [f499])).
fof(f3242, plain, (~ spl15_1 | ~ spl15_49), inference(avatar_split_clause, [], [f3237, f516, f312])).
fof(f3237, plain, (~ (e0 = op(e3, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f153, f518])).
fof(f518, plain, ((e0 = op(e0, e3)) | ~ spl15_49), inference(avatar_component_clause, [], [f516])).
fof(f153, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3234, plain, (~ spl15_6 | ~ spl15_54), inference(avatar_split_clause, [], [f3229, f537, f333])).
fof(f333, plain, (spl15_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_6])])).
fof(f3229, plain, (~ (e1 = op(e3, e2)) | ~ spl15_54), inference(backward_demodulation, [], [f147, f539])).
fof(f3225, plain, (~ spl15_11 | ~ spl15_59), inference(avatar_split_clause, [], [f3220, f558, f354])).
fof(f354, plain, (spl15_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f3220, plain, (~ (e2 = op(e3, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f141, f560])).
fof(f141, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3217, plain, (spl15_2 | ~ spl15_64 | ~ spl15_117), inference(avatar_split_clause, [], [f3210, f939, f579, f316])).
fof(f3210, plain, ((e1 = op(e3, e3)) | (~ spl15_64 | ~ spl15_117)), inference(backward_demodulation, [], [f940, f581])).
fof(f940, plain, ((e1 = op(op(e0, e0), op(e0, e0))) | ~ spl15_117), inference(avatar_component_clause, [], [f939])).
fof(f3216, plain, (~ spl15_15 | ~ spl15_64 | spl15_115), inference(avatar_split_clause, [], [f3209, f927, f579, f371])).
fof(f927, plain, (spl15_115 <=> (e2 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_115])])).
fof(f3209, plain, (~ (e2 = op(e3, e0)) | (~ spl15_64 | spl15_115)), inference(backward_demodulation, [], [f929, f581])).
fof(f929, plain, (~ (e2 = op(op(e0, e0), e0)) | spl15_115), inference(avatar_component_clause, [], [f927])).
fof(f3214, plain, (~ spl15_16 | ~ spl15_64 | spl15_92), inference(avatar_split_clause, [], [f3207, f823, f579, f375])).
fof(f375, plain, (spl15_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f823, plain, (spl15_92 <=> (op(e0, e0) = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_92])])).
fof(f3207, plain, (~ (e3 = op(e3, e0)) | (~ spl15_64 | spl15_92)), inference(backward_demodulation, [], [f824, f581])).
fof(f824, plain, (~ (op(e0, e0) = op(op(e0, e0), e0)) | spl15_92), inference(avatar_component_clause, [], [f823])).
fof(f3213, plain, (~ spl15_60 | ~ spl15_64), inference(avatar_split_clause, [], [f3204, f579, f562])).
fof(f562, plain, (spl15_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_60])])).
fof(f3204, plain, (~ (e3 = op(e0, e1)) | ~ spl15_64), inference(backward_demodulation, [], [f157, f581])).
fof(f157, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f3212, plain, (~ spl15_16 | ~ spl15_64), inference(avatar_split_clause, [], [f3203, f579, f375])).
fof(f3203, plain, (~ (e3 = op(e3, e0)) | ~ spl15_64), inference(backward_demodulation, [], [f135, f581])).
fof(f135, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3184, plain, (~ spl15_7 | ~ spl15_23), inference(avatar_split_clause, [], [f3176, f405, f337])).
fof(f337, plain, (spl15_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f405, plain, (spl15_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_23])])).
fof(f3176, plain, (~ (e2 = op(e3, e2)) | ~ spl15_23), inference(backward_demodulation, [], [f150, f407])).
fof(f407, plain, ((e2 = op(e2, e2)) | ~ spl15_23), inference(avatar_component_clause, [], [f405])).
fof(f3174, plain, (~ spl15_10 | ~ spl15_26), inference(avatar_split_clause, [], [f3168, f418, f350])).
fof(f350, plain, (spl15_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_10])])).
fof(f3168, plain, (~ (e1 = op(e3, e1)) | ~ spl15_26), inference(backward_demodulation, [], [f144, f420])).
fof(f420, plain, ((e1 = op(e2, e1)) | ~ spl15_26), inference(avatar_component_clause, [], [f418])).
fof(f3166, plain, (~ spl15_18 | ~ spl15_34), inference(avatar_split_clause, [], [f3162, f452, f384])).
fof(f3162, plain, (~ (e1 = op(e2, e3)) | ~ spl15_34), inference(backward_demodulation, [], [f154, f454])).
fof(f454, plain, ((e1 = op(e1, e3)) | ~ spl15_34), inference(avatar_component_clause, [], [f452])).
fof(f3161, plain, (~ spl15_36 | ~ spl15_40), inference(avatar_split_clause, [], [f3158, f477, f460])).
fof(f477, plain, (spl15_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_40])])).
fof(f3158, plain, (~ (e3 = op(e1, e3)) | ~ spl15_40), inference(backward_demodulation, [], [f168, f479])).
fof(f479, plain, ((e3 = op(e1, e2)) | ~ spl15_40), inference(avatar_component_clause, [], [f477])).
fof(f168, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3160, plain, (~ spl15_24 | ~ spl15_40), inference(avatar_split_clause, [], [f3156, f477, f409])).
fof(f3156, plain, (~ (e3 = op(e2, e2)) | ~ spl15_40), inference(backward_demodulation, [], [f148, f479])).
fof(f3155, plain, (~ spl15_1 | spl15_49 | ~ spl15_83), inference(avatar_contradiction_clause, [], [f3154])).
fof(f3154, plain, ($false | (~ spl15_1 | spl15_49 | ~ spl15_83)), inference(subsumption_resolution, [], [f3153, f517])).
fof(f517, plain, (~ (e0 = op(e0, e3)) | spl15_49), inference(avatar_component_clause, [], [f516])).
fof(f3153, plain, ((e0 = op(e0, e3)) | (~ spl15_1 | ~ spl15_83)), inference(forward_demodulation, [], [f786, f314])).
fof(f314, plain, ((e0 = op(e3, e3)) | ~ spl15_1), inference(avatar_component_clause, [], [f312])).
fof(f786, plain, ((op(e3, e3) = op(op(e3, e3), e3)) | ~ spl15_83), inference(avatar_component_clause, [], [f784])).
fof(f784, plain, (spl15_83 <=> (op(e3, e3) = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_83])])).
fof(f3148, plain, (~ spl15_1 | ~ spl15_51 | spl15_97), inference(avatar_contradiction_clause, [], [f3147])).
fof(f3147, plain, ($false | (~ spl15_1 | ~ spl15_51 | spl15_97)), inference(subsumption_resolution, [], [f3146, f526])).
fof(f3146, plain, (~ (e2 = op(e0, e3)) | (~ spl15_1 | spl15_97)), inference(forward_demodulation, [], [f846, f314])).
fof(f846, plain, (~ (e2 = op(op(e3, e3), e3)) | spl15_97), inference(avatar_component_clause, [], [f844])).
fof(f844, plain, (spl15_97 <=> (e2 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_97])])).
fof(f3144, plain, (~ spl15_28 | ~ spl15_60), inference(avatar_split_clause, [], [f3143, f562, f426])).
fof(f3143, plain, (~ (e3 = op(e2, e1)) | ~ spl15_60), inference(forward_demodulation, [], [f140, f564])).
fof(f564, plain, ((e3 = op(e0, e1)) | ~ spl15_60), inference(avatar_component_clause, [], [f562])).
fof(f3137, plain, (~ spl15_6 | ~ spl15_10), inference(avatar_split_clause, [], [f3133, f350, f333])).
fof(f3133, plain, (~ (e1 = op(e3, e2)) | ~ spl15_10), inference(backward_demodulation, [], [f178, f352])).
fof(f352, plain, ((e1 = op(e3, e1)) | ~ spl15_10), inference(avatar_component_clause, [], [f350])).
fof(f3131, plain, (~ spl15_12 | ~ spl15_16), inference(avatar_split_clause, [], [f3128, f375, f358])).
fof(f3128, plain, (~ (e3 = op(e3, e1)) | ~ spl15_16), inference(backward_demodulation, [], [f175, f377])).
fof(f377, plain, ((e3 = op(e3, e0)) | ~ spl15_16), inference(avatar_component_clause, [], [f375])).
fof(f175, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3124, plain, (~ spl15_6 | ~ spl15_24 | spl15_105), inference(avatar_split_clause, [], [f3118, f880, f409, f333])).
fof(f3118, plain, (~ (e1 = op(e3, e2)) | (~ spl15_24 | spl15_105)), inference(backward_demodulation, [], [f882, f411])).
fof(f3100, plain, (~ spl15_21 | ~ spl15_53), inference(avatar_split_clause, [], [f3094, f533, f397])).
fof(f533, plain, (spl15_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_53])])).
fof(f3094, plain, (~ (e0 = op(e2, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f146, f535])).
fof(f535, plain, ((e0 = op(e0, e2)) | ~ spl15_53), inference(avatar_component_clause, [], [f533])).
fof(f3091, plain, (~ spl15_12 | ~ spl15_60), inference(avatar_split_clause, [], [f3088, f562, f358])).
fof(f3088, plain, (~ (e3 = op(e3, e1)) | ~ spl15_60), inference(backward_demodulation, [], [f141, f564])).
fof(f3084, plain, (~ spl15_58 | ~ spl15_62), inference(avatar_split_clause, [], [f3075, f571, f554])).
fof(f571, plain, (spl15_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl15_62])])).
fof(f3075, plain, (~ (e1 = op(e0, e1)) | ~ spl15_62), inference(backward_demodulation, [], [f157, f573])).
fof(f573, plain, ((op(e0, e0) = e1) | ~ spl15_62), inference(avatar_component_clause, [], [f571])).
fof(f3073, plain, (~ spl15_62 | ~ spl15_1 | spl15_96), inference(avatar_split_clause, [], [f3072, f840, f312, f571])).
fof(f840, plain, (spl15_96 <=> (e1 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_96])])).
fof(f3072, plain, (~ (op(e0, e0) = e1) | (~ spl15_1 | spl15_96)), inference(forward_demodulation, [], [f842, f314])).
fof(f842, plain, (~ (e1 = op(op(e3, e3), op(e3, e3))) | spl15_96), inference(avatar_component_clause, [], [f840])).
fof(f3069, plain, (~ spl15_64 | ~ spl15_41 | spl15_106), inference(avatar_split_clause, [], [f3068, f885, f482, f579])).
fof(f482, plain, (spl15_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_41])])).
fof(f885, plain, (spl15_106 <=> (e3 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_106])])).
fof(f3068, plain, (~ (op(e0, e0) = e3) | (~ spl15_41 | spl15_106)), inference(forward_demodulation, [], [f887, f484])).
fof(f484, plain, ((e0 = op(e1, e1)) | ~ spl15_41), inference(avatar_component_clause, [], [f482])).
fof(f887, plain, (~ (e3 = op(op(e1, e1), op(e1, e1))) | spl15_106), inference(avatar_component_clause, [], [f885])).
fof(f3067, plain, (~ spl15_104 | ~ spl15_109), inference(avatar_contradiction_clause, [], [f3066])).
fof(f3066, plain, ($false | (~ spl15_104 | ~ spl15_109)), inference(subsumption_resolution, [], [f3065, f183])).
fof(f3065, plain, ((e0 = e3) | (~ spl15_104 | ~ spl15_109)), inference(backward_demodulation, [], [f877, f900])).
fof(f900, plain, ((e0 = op(op(e2, e2), op(e2, e2))) | ~ spl15_109), inference(avatar_component_clause, [], [f899])).
fof(f899, plain, (spl15_109 <=> (e0 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl15_109])])).
fof(f877, plain, ((e3 = op(op(e2, e2), op(e2, e2))) | ~ spl15_104), inference(avatar_component_clause, [], [f876])).
fof(f3054, plain, (~ spl15_55 | ~ spl15_51), inference(avatar_split_clause, [], [f3053, f524, f541])).
fof(f3053, plain, (~ (e2 = op(e0, e2)) | ~ spl15_51), inference(forward_demodulation, [], [f162, f526])).
fof(f162, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3023, plain, (spl15_4 | ~ spl15_12 | ~ spl15_91), inference(avatar_split_clause, [], [f3021, f818, f358, f324])).
fof(f324, plain, (spl15_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_4])])).
fof(f818, plain, (spl15_91 <=> (op(e3, e1) = op(op(e3, e1), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_91])])).
fof(f3021, plain, ((e3 = op(e3, e3)) | (~ spl15_12 | ~ spl15_91)), inference(backward_demodulation, [], [f820, f360])).
fof(f820, plain, ((op(e3, e1) = op(op(e3, e1), e3)) | ~ spl15_91), inference(avatar_component_clause, [], [f818])).
fof(f3005, plain, (~ spl15_29 | ~ spl15_30), inference(avatar_contradiction_clause, [], [f3004])).
fof(f3004, plain, ($false | (~ spl15_29 | ~ spl15_30)), inference(subsumption_resolution, [], [f3003, f181])).
fof(f3003, plain, ((e0 = e1) | (~ spl15_29 | ~ spl15_30)), inference(forward_demodulation, [], [f437, f433])).
fof(f433, plain, ((e0 = op(e2, e0)) | ~ spl15_29), inference(avatar_component_clause, [], [f431])).
fof(f2997, plain, (~ spl15_35 | ~ spl15_47), inference(avatar_split_clause, [], [f2995, f507, f456])).
fof(f507, plain, (spl15_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_47])])).
fof(f2995, plain, (~ (e2 = op(e1, e3)) | ~ spl15_47), inference(backward_demodulation, [], [f165, f509])).
fof(f509, plain, ((e2 = op(e1, e0)) | ~ spl15_47), inference(avatar_component_clause, [], [f507])).
fof(f165, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2992, plain, (~ spl15_49 | ~ spl15_51), inference(avatar_contradiction_clause, [], [f2991])).
fof(f2991, plain, ($false | (~ spl15_49 | ~ spl15_51)), inference(subsumption_resolution, [], [f2990, f182])).
fof(f182, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f2990, plain, ((e0 = e2) | (~ spl15_49 | ~ spl15_51)), inference(forward_demodulation, [], [f526, f518])).
fof(f2967, plain, (~ spl15_60 | ~ spl15_41 | spl15_103), inference(avatar_split_clause, [], [f2966, f871, f482, f562])).
fof(f871, plain, (spl15_103 <=> (e3 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_103])])).
fof(f2966, plain, (~ (e3 = op(e0, e1)) | (~ spl15_41 | spl15_103)), inference(forward_demodulation, [], [f873, f484])).
fof(f873, plain, (~ (e3 = op(op(e1, e1), e1)) | spl15_103), inference(avatar_component_clause, [], [f871])).
fof(f2957, plain, (~ spl15_63 | ~ spl15_41 | spl15_102), inference(avatar_split_clause, [], [f2865, f867, f482, f575])).
fof(f867, plain, (spl15_102 <=> (e2 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl15_102])])).
fof(f2865, plain, (~ (op(e0, e0) = e2) | (~ spl15_41 | spl15_102)), inference(backward_demodulation, [], [f869, f484])).
fof(f869, plain, (~ (e2 = op(op(e1, e1), op(e1, e1))) | spl15_102), inference(avatar_component_clause, [], [f867])).
fof(f2942, plain, (~ spl15_8 | ~ spl15_40), inference(avatar_split_clause, [], [f2941, f477, f341])).
fof(f2941, plain, (~ (e3 = op(e3, e2)) | ~ spl15_40), inference(forward_demodulation, [], [f149, f479])).
fof(f149, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2936, plain, (~ spl15_8 | ~ spl15_20 | spl15_82), inference(avatar_split_clause, [], [f2911, f780, f392, f341])).
fof(f780, plain, (spl15_82 <=> (op(e2, e3) = op(op(e2, e3), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_82])])).
fof(f2911, plain, (~ (e3 = op(e3, e2)) | (~ spl15_20 | spl15_82)), inference(backward_demodulation, [], [f781, f394])).
fof(f394, plain, ((e3 = op(e2, e3)) | ~ spl15_20), inference(avatar_component_clause, [], [f392])).
fof(f781, plain, (~ (op(e2, e3) = op(op(e2, e3), e2)) | spl15_82), inference(avatar_component_clause, [], [f780])).
fof(f2922, plain, (~ spl15_15 | ~ spl15_16), inference(avatar_contradiction_clause, [], [f2921])).
fof(f2921, plain, ($false | (~ spl15_15 | ~ spl15_16)), inference(subsumption_resolution, [], [f2920, f186])).
fof(f186, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f2920, plain, ((e2 = e3) | (~ spl15_15 | ~ spl15_16)), inference(backward_demodulation, [], [f377, f373])).
fof(f2918, plain, (~ spl15_4 | ~ spl15_16), inference(avatar_split_clause, [], [f2915, f375, f324])).
fof(f2915, plain, (~ (e3 = op(e3, e3)) | ~ spl15_16), inference(backward_demodulation, [], [f177, f377])).
fof(f2912, plain, (~ spl15_4 | ~ spl15_20), inference(avatar_split_clause, [], [f2910, f392, f324])).
fof(f2910, plain, (~ (e3 = op(e3, e3)) | ~ spl15_20), inference(backward_demodulation, [], [f156, f394])).
fof(f2909, plain, (~ spl15_22 | ~ spl15_41 | spl15_109), inference(avatar_contradiction_clause, [], [f2908])).
fof(f2908, plain, ($false | (~ spl15_22 | ~ spl15_41 | spl15_109)), inference(subsumption_resolution, [], [f2904, f484])).
fof(f2904, plain, (~ (e0 = op(e1, e1)) | (~ spl15_22 | spl15_109)), inference(backward_demodulation, [], [f901, f403])).
fof(f901, plain, (~ (e0 = op(op(e2, e2), op(e2, e2))) | spl15_109), inference(avatar_component_clause, [], [f899])).
fof(f2906, plain, (~ spl15_18 | ~ spl15_22), inference(avatar_split_clause, [], [f2898, f401, f384])).
fof(f2898, plain, (~ (e1 = op(e2, e3)) | ~ spl15_22), inference(backward_demodulation, [], [f174, f403])).
fof(f2892, plain, (~ spl15_25 | ~ spl15_29), inference(avatar_split_clause, [], [f2885, f431, f414])).
fof(f2885, plain, (~ (e0 = op(e2, e1)) | ~ spl15_29), inference(backward_demodulation, [], [f169, f433])).
fof(f2883, plain, (~ spl15_3 | ~ spl15_35), inference(avatar_split_clause, [], [f2881, f456, f320])).
fof(f320, plain, (spl15_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_3])])).
fof(f2881, plain, (~ (e2 = op(e3, e3)) | ~ spl15_35), inference(backward_demodulation, [], [f155, f458])).
fof(f2879, plain, (~ spl15_12 | ~ spl15_40 | spl15_85), inference(avatar_split_clause, [], [f2876, f793, f477, f358])).
fof(f2876, plain, (~ (e3 = op(e3, e1)) | (~ spl15_40 | spl15_85)), inference(backward_demodulation, [], [f794, f479])).
fof(f794, plain, (~ (op(e1, e2) = op(op(e1, e2), e1)) | spl15_85), inference(avatar_component_clause, [], [f793])).
fof(f2856, plain, (~ spl15_42 | ~ spl15_46), inference(avatar_split_clause, [], [f2849, f503, f486])).
fof(f2849, plain, (~ (e1 = op(e1, e1)) | ~ spl15_46), inference(backward_demodulation, [], [f163, f505])).
fof(f2855, plain, (~ spl15_14 | ~ spl15_46), inference(avatar_split_clause, [], [f2848, f503, f367])).
fof(f2848, plain, (~ (e1 = op(e3, e0)) | ~ spl15_46), inference(backward_demodulation, [], [f137, f505])).
fof(f137, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f2846, plain, (~ spl15_49 | ~ spl15_50), inference(avatar_contradiction_clause, [], [f2845])).
fof(f2845, plain, ($false | (~ spl15_49 | ~ spl15_50)), inference(subsumption_resolution, [], [f2844, f181])).
fof(f2844, plain, ((e0 = e1) | (~ spl15_49 | ~ spl15_50)), inference(forward_demodulation, [], [f522, f518])).
fof(f2842, plain, (~ spl15_39 | ~ spl15_55), inference(avatar_split_clause, [], [f2838, f541, f473])).
fof(f2838, plain, (~ (e2 = op(e1, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f145, f543])).
fof(f543, plain, ((e2 = op(e0, e2)) | ~ spl15_55), inference(avatar_component_clause, [], [f541])).
fof(f145, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2800, plain, (~ spl15_61 | ~ spl15_49), inference(avatar_split_clause, [], [f2799, f516, f567])).
fof(f567, plain, (spl15_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_61])])).
fof(f2799, plain, (~ (e0 = op(e0, e0)) | ~ spl15_49), inference(forward_demodulation, [], [f159, f518])).
fof(f2779, plain, (~ spl15_27 | ~ spl15_31), inference(avatar_split_clause, [], [f2776, f439, f422])).
fof(f2776, plain, (~ (e2 = op(e2, e1)) | ~ spl15_31), inference(backward_demodulation, [], [f169, f441])).
fof(f2766, plain, (~ spl15_42 | spl15_89), inference(avatar_contradiction_clause, [], [f2765])).
fof(f2765, plain, ($false | (~ spl15_42 | spl15_89)), inference(subsumption_resolution, [], [f2759, f488])).
fof(f2759, plain, (~ (e1 = op(e1, e1)) | (~ spl15_42 | spl15_89)), inference(backward_demodulation, [], [f811, f488])).
fof(f811, plain, (~ (op(e1, e1) = op(op(e1, e1), e1)) | spl15_89), inference(avatar_component_clause, [], [f810])).
fof(f810, plain, (spl15_89 <=> (op(e1, e1) = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_89])])).
fof(f2764, plain, (~ spl15_38 | ~ spl15_42), inference(avatar_split_clause, [], [f2755, f486, f469])).
fof(f2755, plain, (~ (e1 = op(e1, e2)) | ~ spl15_42), inference(backward_demodulation, [], [f166, f488])).
fof(f2763, plain, (~ spl15_10 | ~ spl15_42), inference(avatar_split_clause, [], [f2754, f486, f350])).
fof(f2754, plain, (~ (e1 = op(e3, e1)) | ~ spl15_42), inference(backward_demodulation, [], [f143, f488])).
fof(f143, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2739, plain, (~ spl15_33 | ~ spl15_49), inference(avatar_split_clause, [], [f2736, f516, f448])).
fof(f2736, plain, (~ (e0 = op(e1, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f151, f518])).
fof(f151, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2722, plain, (~ spl15_3 | ~ spl15_64 | spl15_111), inference(avatar_contradiction_clause, [], [f2721])).
fof(f2721, plain, ($false | (~ spl15_3 | ~ spl15_64 | spl15_111)), inference(subsumption_resolution, [], [f2715, f322])).
fof(f322, plain, ((e2 = op(e3, e3)) | ~ spl15_3), inference(avatar_component_clause, [], [f320])).
fof(f2715, plain, (~ (e2 = op(e3, e3)) | (~ spl15_64 | spl15_111)), inference(backward_demodulation, [], [f911, f581])).
fof(f911, plain, (~ (e2 = op(op(e0, e0), op(e0, e0))) | spl15_111), inference(avatar_component_clause, [], [f909])).
fof(f909, plain, (spl15_111 <=> (e2 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_111])])).
fof(f2720, plain, (~ spl15_52 | ~ spl15_64), inference(avatar_split_clause, [], [f2714, f579, f528])).
fof(f528, plain, (spl15_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_52])])).
fof(f2714, plain, (~ (e3 = op(e0, e3)) | ~ spl15_64), inference(backward_demodulation, [], [f159, f581])).
fof(f2686, plain, (~ spl15_3 | ~ spl15_44 | spl15_102), inference(avatar_contradiction_clause, [], [f2685])).
fof(f2685, plain, ($false | (~ spl15_3 | ~ spl15_44 | spl15_102)), inference(subsumption_resolution, [], [f2678, f322])).
fof(f2678, plain, (~ (e2 = op(e3, e3)) | (~ spl15_44 | spl15_102)), inference(backward_demodulation, [], [f869, f496])).
fof(f2683, plain, (~ spl15_36 | ~ spl15_44), inference(avatar_split_clause, [], [f2676, f494, f460])).
fof(f2676, plain, (~ (e3 = op(e1, e3)) | ~ spl15_44), inference(backward_demodulation, [], [f167, f496])).
fof(f2661, plain, (~ spl15_41 | ~ spl15_57), inference(avatar_split_clause, [], [f2655, f550, f482])).
fof(f2655, plain, (~ (e0 = op(e1, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f139, f552])).
fof(f139, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2652, plain, (~ spl15_44 | ~ spl15_62 | spl15_114), inference(avatar_split_clause, [], [f2647, f923, f571, f494])).
fof(f923, plain, (spl15_114 <=> (e3 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl15_114])])).
fof(f2647, plain, (~ (e3 = op(e1, e1)) | (~ spl15_62 | spl15_114)), inference(backward_demodulation, [], [f925, f573])).
fof(f925, plain, (~ (e3 = op(op(e0, e0), op(e0, e0))) | spl15_114), inference(avatar_component_clause, [], [f923])).
fof(f2651, plain, (~ spl15_46 | ~ spl15_62), inference(avatar_split_clause, [], [f2642, f571, f503])).
fof(f2642, plain, (~ (e1 = op(e1, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f133, f573])).
fof(f133, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2641, plain, (~ spl15_46 | ~ spl15_54 | spl15_84), inference(avatar_split_clause, [], [f2640, f789, f537, f503])).
fof(f2640, plain, (~ (e1 = op(e1, e0)) | (~ spl15_54 | spl15_84)), inference(forward_demodulation, [], [f790, f539])).
fof(f790, plain, (~ (op(e0, e2) = op(op(e0, e2), e0)) | spl15_84), inference(avatar_component_clause, [], [f789])).
fof(f2635, plain, (spl15_23 | ~ spl15_27 | ~ spl15_90), inference(avatar_contradiction_clause, [], [f2634])).
fof(f2634, plain, ($false | (spl15_23 | ~ spl15_27 | ~ spl15_90)), inference(subsumption_resolution, [], [f2633, f406])).
fof(f406, plain, (~ (e2 = op(e2, e2)) | spl15_23), inference(avatar_component_clause, [], [f405])).
fof(f2633, plain, ((e2 = op(e2, e2)) | (~ spl15_27 | ~ spl15_90)), inference(forward_demodulation, [], [f816, f424])).
fof(f816, plain, ((op(e2, e1) = op(op(e2, e1), e2)) | ~ spl15_90), inference(avatar_component_clause, [], [f814])).
fof(f2625, plain, (~ spl15_62 | ~ spl15_54), inference(avatar_split_clause, [], [f2624, f537, f571])).
fof(f2624, plain, (~ (op(e0, e0) = e1) | ~ spl15_54), inference(forward_demodulation, [], [f158, f539])).
fof(f158, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f2613, plain, (~ spl15_38 | ~ spl15_54), inference(avatar_split_clause, [], [f2612, f537, f469])).
fof(f2612, plain, (~ (e1 = op(e1, e2)) | ~ spl15_54), inference(forward_demodulation, [], [f145, f539])).
fof(f2603, plain, (~ spl15_10 | spl15_34 | ~ spl15_91), inference(avatar_contradiction_clause, [], [f2602])).
fof(f2602, plain, ($false | (~ spl15_10 | spl15_34 | ~ spl15_91)), inference(subsumption_resolution, [], [f2600, f453])).
fof(f453, plain, (~ (e1 = op(e1, e3)) | spl15_34), inference(avatar_component_clause, [], [f452])).
fof(f2600, plain, ((e1 = op(e1, e3)) | (~ spl15_10 | ~ spl15_91)), inference(backward_demodulation, [], [f820, f352])).
fof(f2584, plain, (~ spl15_29 | spl15_53 | ~ spl15_94), inference(avatar_contradiction_clause, [], [f2583])).
fof(f2583, plain, ($false | (~ spl15_29 | spl15_53 | ~ spl15_94)), inference(subsumption_resolution, [], [f2580, f534])).
fof(f534, plain, (~ (e0 = op(e0, e2)) | spl15_53), inference(avatar_component_clause, [], [f533])).
fof(f2580, plain, ((e0 = op(e0, e2)) | (~ spl15_29 | ~ spl15_94)), inference(backward_demodulation, [], [f833, f433])).
fof(f833, plain, ((op(e2, e0) = op(op(e2, e0), e2)) | ~ spl15_94), inference(avatar_component_clause, [], [f831])).
fof(f831, plain, (spl15_94 <=> (op(e2, e0) = op(op(e2, e0), e2))), introduced(avatar_definition, [new_symbols(naming, [spl15_94])])).
fof(f2581, plain, (~ spl15_13 | ~ spl15_29), inference(avatar_split_clause, [], [f2574, f431, f363])).
fof(f363, plain, (spl15_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_13])])).
fof(f2574, plain, (~ (e0 = op(e3, e0)) | ~ spl15_29), inference(backward_demodulation, [], [f138, f433])).
fof(f2573, plain, (~ spl15_49 | spl15_61 | ~ spl15_80), inference(avatar_contradiction_clause, [], [f2572])).
fof(f2572, plain, ($false | (~ spl15_49 | spl15_61 | ~ spl15_80)), inference(subsumption_resolution, [], [f2570, f568])).
fof(f568, plain, (~ (e0 = op(e0, e0)) | spl15_61), inference(avatar_component_clause, [], [f567])).
fof(f2570, plain, ((e0 = op(e0, e0)) | (~ spl15_49 | ~ spl15_80)), inference(backward_demodulation, [], [f774, f518])).
fof(f2571, plain, (~ spl15_17 | ~ spl15_49), inference(avatar_split_clause, [], [f2566, f516, f380])).
fof(f2566, plain, (~ (e0 = op(e2, e3)) | ~ spl15_49), inference(backward_demodulation, [], [f152, f518])).
fof(f2550, plain, (~ spl15_59 | ~ spl15_63), inference(avatar_split_clause, [], [f2540, f575, f558])).
fof(f2540, plain, (~ (e2 = op(e0, e1)) | ~ spl15_63), inference(backward_demodulation, [], [f157, f577])).
fof(f2529, plain, (~ spl15_17 | ~ spl15_3 | spl15_110), inference(avatar_split_clause, [], [f2528, f904, f320, f380])).
fof(f904, plain, (spl15_110 <=> (e0 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_110])])).
fof(f2528, plain, (~ (e0 = op(e2, e3)) | (~ spl15_3 | spl15_110)), inference(forward_demodulation, [], [f906, f322])).
fof(f906, plain, (~ (e0 = op(op(e3, e3), e3)) | spl15_110), inference(avatar_component_clause, [], [f904])).
fof(f2507, plain, (~ spl15_5 | ~ spl15_13), inference(avatar_split_clause, [], [f2502, f363, f329])).
fof(f2502, plain, (~ (e0 = op(e3, e2)) | ~ spl15_13), inference(backward_demodulation, [], [f176, f365])).
fof(f365, plain, ((e0 = op(e3, e0)) | ~ spl15_13), inference(avatar_component_clause, [], [f363])).
fof(f176, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2481, plain, (~ spl15_41 | ~ spl15_59 | spl15_107), inference(avatar_contradiction_clause, [], [f2480])).
fof(f2480, plain, ($false | (~ spl15_41 | ~ spl15_59 | spl15_107)), inference(subsumption_resolution, [], [f2473, f560])).
fof(f2473, plain, (~ (e2 = op(e0, e1)) | (~ spl15_41 | spl15_107)), inference(backward_demodulation, [], [f891, f484])).
fof(f891, plain, (~ (e2 = op(op(e1, e1), e1)) | spl15_107), inference(avatar_component_clause, [], [f889])).
fof(f889, plain, (spl15_107 <=> (e2 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_107])])).
fof(f2476, plain, (~ spl15_33 | ~ spl15_41), inference(avatar_split_clause, [], [f2466, f482, f448])).
fof(f2466, plain, (~ (e0 = op(e1, e3)) | ~ spl15_41), inference(backward_demodulation, [], [f167, f484])).
fof(f2454, plain, (spl15_46 | ~ spl15_50 | ~ spl15_80), inference(avatar_split_clause, [], [f2452, f772, f520, f503])).
fof(f2452, plain, ((e1 = op(e1, e0)) | (~ spl15_50 | ~ spl15_80)), inference(backward_demodulation, [], [f774, f522])).
fof(f2436, plain, (~ spl15_43 | ~ spl15_59), inference(avatar_split_clause, [], [f2429, f558, f490])).
fof(f2429, plain, (~ (e2 = op(e1, e1)) | ~ spl15_59), inference(backward_demodulation, [], [f139, f560])).
fof(f2423, plain, (~ spl15_48 | ~ spl15_64), inference(avatar_split_clause, [], [f2413, f579, f511])).
fof(f2413, plain, (~ (e3 = op(e1, e0)) | ~ spl15_64), inference(backward_demodulation, [], [f133, f581])).
fof(f2409, plain, (~ spl15_18 | ~ spl15_3 | spl15_101), inference(avatar_split_clause, [], [f2408, f862, f320, f384])).
fof(f862, plain, (spl15_101 <=> (e1 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_101])])).
fof(f2408, plain, (~ (e1 = op(e2, e3)) | (~ spl15_3 | spl15_101)), inference(forward_demodulation, [], [f864, f322])).
fof(f864, plain, (~ (e1 = op(op(e3, e3), e3)) | spl15_101), inference(avatar_component_clause, [], [f862])).
fof(f2403, plain, (~ spl15_5 | ~ spl15_24 | spl15_113), inference(avatar_split_clause, [], [f2402, f918, f409, f329])).
fof(f2402, plain, (~ (e0 = op(e3, e2)) | (~ spl15_24 | spl15_113)), inference(forward_demodulation, [], [f920, f411])).
fof(f2391, plain, (~ spl15_19 | ~ spl15_3), inference(avatar_split_clause, [], [f2390, f320, f388])).
fof(f2390, plain, (~ (e2 = op(e2, e3)) | ~ spl15_3), inference(forward_demodulation, [], [f156, f322])).
fof(f2352, plain, (~ spl15_29 | ~ spl15_31), inference(avatar_contradiction_clause, [], [f2351])).
fof(f2351, plain, ($false | (~ spl15_29 | ~ spl15_31)), inference(subsumption_resolution, [], [f2350, f182])).
fof(f2350, plain, ((e0 = e2) | (~ spl15_29 | ~ spl15_31)), inference(backward_demodulation, [], [f441, f433])).
fof(f2325, plain, (~ spl15_57 | ~ spl15_58), inference(avatar_contradiction_clause, [], [f2324])).
fof(f2324, plain, ($false | (~ spl15_57 | ~ spl15_58)), inference(subsumption_resolution, [], [f2323, f181])).
fof(f2323, plain, ((e0 = e1) | (~ spl15_57 | ~ spl15_58)), inference(backward_demodulation, [], [f556, f552])).
fof(f2321, plain, (~ spl15_43 | ~ spl15_62 | spl15_111), inference(avatar_contradiction_clause, [], [f2320])).
fof(f2320, plain, ($false | (~ spl15_43 | ~ spl15_62 | spl15_111)), inference(subsumption_resolution, [], [f2314, f492])).
fof(f2314, plain, (~ (e2 = op(e1, e1)) | (~ spl15_62 | spl15_111)), inference(backward_demodulation, [], [f911, f573])).
fof(f2319, plain, (~ spl15_30 | ~ spl15_62), inference(avatar_split_clause, [], [f2310, f571, f435])).
fof(f2310, plain, (~ (e1 = op(e2, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f134, f573])).
fof(f2309, plain, (spl15_31 | ~ spl15_55 | ~ spl15_84), inference(avatar_split_clause, [], [f2308, f789, f541, f439])).
fof(f2308, plain, ((e2 = op(e2, e0)) | (~ spl15_55 | ~ spl15_84)), inference(forward_demodulation, [], [f791, f543])).
fof(f2288, plain, (~ spl15_32 | ~ spl15_24), inference(avatar_split_clause, [], [f2287, f409, f443])).
fof(f2287, plain, (~ (e3 = op(e2, e0)) | ~ spl15_24), inference(forward_demodulation, [], [f170, f411])).
fof(f2282, plain, (~ spl15_25 | ~ spl15_43 | spl15_118), inference(avatar_split_clause, [], [f2205, f944, f490, f414])).
fof(f944, plain, (spl15_118 <=> (e0 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_118])])).
fof(f2205, plain, (~ (e0 = op(e2, e1)) | (~ spl15_43 | spl15_118)), inference(forward_demodulation, [], [f946, f492])).
fof(f946, plain, (~ (e0 = op(op(e1, e1), e1)) | spl15_118), inference(avatar_component_clause, [], [f944])).
fof(f2278, plain, (~ spl15_16 | ~ spl15_48), inference(avatar_split_clause, [], [f2277, f511, f375])).
fof(f2277, plain, (~ (e3 = op(e3, e0)) | ~ spl15_48), inference(forward_demodulation, [], [f137, f513])).
fof(f2273, plain, (~ spl15_12 | ~ spl15_48 | spl15_93), inference(avatar_split_clause, [], [f2217, f827, f511, f358])).
fof(f827, plain, (spl15_93 <=> (op(e1, e0) = op(op(e1, e0), e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_93])])).
fof(f2217, plain, (~ (e3 = op(e3, e1)) | (~ spl15_48 | spl15_93)), inference(backward_demodulation, [], [f828, f513])).
fof(f828, plain, (~ (op(e1, e0) = op(op(e1, e0), e1)) | spl15_93), inference(avatar_component_clause, [], [f827])).
fof(f2257, plain, (~ spl15_2 | ~ spl15_6), inference(avatar_split_clause, [], [f2254, f333, f316])).
fof(f2254, plain, (~ (e1 = op(e3, e3)) | ~ spl15_6), inference(backward_demodulation, [], [f180, f335])).
fof(f335, plain, ((e1 = op(e3, e2)) | ~ spl15_6), inference(avatar_component_clause, [], [f333])).
fof(f2249, plain, (~ spl15_1 | ~ spl15_24 | spl15_109), inference(avatar_split_clause, [], [f2246, f899, f409, f312])).
fof(f2246, plain, (~ (e0 = op(e3, e3)) | (~ spl15_24 | spl15_109)), inference(backward_demodulation, [], [f901, f411])).
fof(f2236, plain, (~ spl15_2 | ~ spl15_34), inference(avatar_split_clause, [], [f2235, f452, f316])).
fof(f2235, plain, (~ (e1 = op(e3, e3)) | ~ spl15_34), inference(backward_demodulation, [], [f155, f454])).
fof(f2203, plain, (~ spl15_47 | ~ spl15_43), inference(avatar_split_clause, [], [f2202, f490, f507])).
fof(f2202, plain, (~ (e2 = op(e1, e0)) | ~ spl15_43), inference(forward_demodulation, [], [f163, f492])).
fof(f2193, plain, (spl15_23 | ~ spl15_19 | ~ spl15_82), inference(avatar_split_clause, [], [f2180, f780, f388, f405])).
fof(f2180, plain, ((e2 = op(e2, e2)) | (~ spl15_19 | ~ spl15_82)), inference(backward_demodulation, [], [f782, f390])).
fof(f390, plain, ((e2 = op(e2, e3)) | ~ spl15_19), inference(avatar_component_clause, [], [f388])).
fof(f782, plain, ((op(e2, e3) = op(op(e2, e3), e2)) | ~ spl15_82), inference(avatar_component_clause, [], [f780])).
fof(f2188, plain, (~ spl15_3 | ~ spl15_15), inference(avatar_split_clause, [], [f2187, f371, f320])).
fof(f2187, plain, (~ (e2 = op(e3, e3)) | ~ spl15_15), inference(forward_demodulation, [], [f177, f373])).
fof(f2168, plain, (~ spl15_23 | ~ spl15_55), inference(avatar_split_clause, [], [f2166, f541, f405])).
fof(f2166, plain, (~ (e2 = op(e2, e2)) | ~ spl15_55), inference(backward_demodulation, [], [f146, f543])).
fof(f2162, plain, (~ spl15_54 | ~ spl15_58), inference(avatar_split_clause, [], [f2160, f554, f537])).
fof(f2160, plain, (~ (e1 = op(e0, e2)) | ~ spl15_58), inference(backward_demodulation, [], [f160, f556])).
fof(f2149, plain, (~ spl15_2 | ~ spl15_33 | spl15_110), inference(avatar_contradiction_clause, [], [f2148])).
fof(f2148, plain, ($false | (~ spl15_2 | ~ spl15_33 | spl15_110)), inference(subsumption_resolution, [], [f2147, f450])).
fof(f2147, plain, (~ (e0 = op(e1, e3)) | (~ spl15_2 | spl15_110)), inference(forward_demodulation, [], [f906, f318])).
fof(f2139, plain, (~ spl15_56 | ~ spl15_40), inference(avatar_split_clause, [], [f2138, f477, f545])).
fof(f2138, plain, (~ (e3 = op(e0, e2)) | ~ spl15_40), inference(forward_demodulation, [], [f145, f479])).
fof(f2123, plain, (~ spl15_24 | ~ spl15_43 | spl15_106), inference(avatar_split_clause, [], [f2104, f885, f490, f409])).
fof(f2104, plain, (~ (e3 = op(e2, e2)) | (~ spl15_43 | spl15_106)), inference(backward_demodulation, [], [f887, f492])).
fof(f2122, plain, (~ spl15_18 | spl15_38 | ~ spl15_82), inference(avatar_contradiction_clause, [], [f2121])).
fof(f2121, plain, ($false | (~ spl15_18 | spl15_38 | ~ spl15_82)), inference(subsumption_resolution, [], [f2120, f470])).
fof(f470, plain, (~ (e1 = op(e1, e2)) | spl15_38), inference(avatar_component_clause, [], [f469])).
fof(f2120, plain, ((e1 = op(e1, e2)) | (~ spl15_18 | ~ spl15_82)), inference(backward_demodulation, [], [f782, f386])).
fof(f2117, plain, (~ spl15_25 | ~ spl15_26), inference(avatar_contradiction_clause, [], [f2116])).
fof(f2116, plain, ($false | (~ spl15_25 | ~ spl15_26)), inference(subsumption_resolution, [], [f2115, f181])).
fof(f2115, plain, ((e0 = e1) | (~ spl15_25 | ~ spl15_26)), inference(backward_demodulation, [], [f420, f416])).
fof(f416, plain, ((e0 = op(e2, e1)) | ~ spl15_25), inference(avatar_component_clause, [], [f414])).
fof(f2105, plain, (~ spl15_35 | ~ spl15_43), inference(avatar_split_clause, [], [f2100, f490, f456])).
fof(f2100, plain, (~ (e2 = op(e1, e3)) | ~ spl15_43), inference(backward_demodulation, [], [f167, f492])).
fof(f2089, plain, (~ spl15_43 | ~ spl15_2 | spl15_100), inference(avatar_split_clause, [], [f1898, f858, f316, f490])).
fof(f858, plain, (spl15_100 <=> (e2 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_100])])).
fof(f1898, plain, (~ (e2 = op(e1, e1)) | (~ spl15_2 | spl15_100)), inference(backward_demodulation, [], [f860, f318])).
fof(f860, plain, (~ (e2 = op(op(e3, e3), op(e3, e3))) | spl15_100), inference(avatar_component_clause, [], [f858])).
fof(f2085, plain, (spl15_42 | ~ spl15_46 | ~ spl15_93), inference(avatar_split_clause, [], [f2016, f827, f503, f486])).
fof(f2016, plain, ((e1 = op(e1, e1)) | (~ spl15_46 | ~ spl15_93)), inference(forward_demodulation, [], [f829, f505])).
fof(f829, plain, ((op(e1, e0) = op(op(e1, e0), e1)) | ~ spl15_93), inference(avatar_component_clause, [], [f827])).
fof(f2080, plain, (~ spl15_35 | ~ spl15_2 | spl15_97), inference(avatar_split_clause, [], [f2033, f844, f316, f456])).
fof(f2033, plain, (~ (e2 = op(e1, e3)) | (~ spl15_2 | spl15_97)), inference(forward_demodulation, [], [f846, f318])).
fof(f2065, plain, (~ spl15_17 | spl15_53 | ~ spl15_82), inference(avatar_contradiction_clause, [], [f2064])).
fof(f2064, plain, ($false | (~ spl15_17 | spl15_53 | ~ spl15_82)), inference(subsumption_resolution, [], [f2063, f534])).
fof(f2063, plain, ((e0 = op(e0, e2)) | (~ spl15_17 | ~ spl15_82)), inference(backward_demodulation, [], [f782, f382])).
fof(f382, plain, ((e0 = op(e2, e3)) | ~ spl15_17), inference(avatar_component_clause, [], [f380])).
fof(f2054, plain, (~ spl15_19 | ~ spl15_23), inference(avatar_split_clause, [], [f2047, f405, f388])).
fof(f2047, plain, (~ (e2 = op(e2, e3)) | ~ spl15_23), inference(backward_demodulation, [], [f174, f407])).
fof(f2029, plain, (~ spl15_61 | spl15_92), inference(avatar_contradiction_clause, [], [f2028])).
fof(f2028, plain, ($false | (~ spl15_61 | spl15_92)), inference(subsumption_resolution, [], [f2027, f569])).
fof(f569, plain, ((e0 = op(e0, e0)) | ~ spl15_61), inference(avatar_component_clause, [], [f567])).
fof(f2027, plain, (~ (e0 = op(e0, e0)) | (~ spl15_61 | spl15_92)), inference(forward_demodulation, [], [f824, f569])).
fof(f2006, plain, (~ spl15_1 | ~ spl15_2), inference(avatar_contradiction_clause, [], [f2005])).
fof(f2005, plain, ($false | (~ spl15_1 | ~ spl15_2)), inference(subsumption_resolution, [], [f2004, f181])).
fof(f2004, plain, ((e0 = e1) | (~ spl15_1 | ~ spl15_2)), inference(backward_demodulation, [], [f318, f314])).
fof(f1995, plain, (~ spl15_9 | ~ spl15_13), inference(avatar_split_clause, [], [f1989, f363, f346])).
fof(f346, plain, (spl15_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl15_9])])).
fof(f1989, plain, (~ (e0 = op(e3, e1)) | ~ spl15_13), inference(backward_demodulation, [], [f175, f365])).
fof(f1978, plain, (~ spl15_9 | ~ spl15_41), inference(avatar_split_clause, [], [f1969, f482, f346])).
fof(f1969, plain, (~ (e0 = op(e3, e1)) | ~ spl15_41), inference(backward_demodulation, [], [f143, f484])).
fof(f1965, plain, (~ spl15_26 | ~ spl15_47 | ~ spl15_93), inference(avatar_contradiction_clause, [], [f1964])).
fof(f1964, plain, ($false | (~ spl15_26 | ~ spl15_47 | ~ spl15_93)), inference(subsumption_resolution, [], [f1963, f184])).
fof(f184, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f1963, plain, ((e1 = e2) | (~ spl15_26 | ~ spl15_47 | ~ spl15_93)), inference(forward_demodulation, [], [f1960, f420])).
fof(f1960, plain, ((e2 = op(e2, e1)) | (~ spl15_47 | ~ spl15_93)), inference(backward_demodulation, [], [f829, f509])).
fof(f1938, plain, (spl15_34 | ~ spl15_2 | ~ spl15_83), inference(avatar_split_clause, [], [f1937, f784, f316, f452])).
fof(f1937, plain, ((e1 = op(e1, e3)) | (~ spl15_2 | ~ spl15_83)), inference(forward_demodulation, [], [f786, f318])).
fof(f1932, plain, (~ spl15_64 | ~ spl15_32), inference(avatar_split_clause, [], [f1931, f443, f579])).
fof(f1931, plain, (~ (op(e0, e0) = e3) | ~ spl15_32), inference(forward_demodulation, [], [f134, f445])).
fof(f1918, plain, (~ spl15_41 | ~ spl15_2 | spl15_108), inference(avatar_split_clause, [], [f1900, f894, f316, f482])).
fof(f894, plain, (spl15_108 <=> (e0 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl15_108])])).
fof(f1900, plain, (~ (e0 = op(e1, e1)) | (~ spl15_2 | spl15_108)), inference(backward_demodulation, [], [f896, f318])).
fof(f896, plain, (~ (e0 = op(op(e3, e3), op(e3, e3))) | spl15_108), inference(avatar_component_clause, [], [f894])).
fof(f1893, plain, (spl15_4 | ~ spl15_8 | ~ spl15_87), inference(avatar_contradiction_clause, [], [f1892])).
fof(f1892, plain, ($false | (spl15_4 | ~ spl15_8 | ~ spl15_87)), inference(subsumption_resolution, [], [f1891, f325])).
fof(f325, plain, (~ (e3 = op(e3, e3)) | spl15_4), inference(avatar_component_clause, [], [f324])).
fof(f1891, plain, ((e3 = op(e3, e3)) | (~ spl15_8 | ~ spl15_87)), inference(backward_demodulation, [], [f803, f343])).
fof(f343, plain, ((e3 = op(e3, e2)) | ~ spl15_8), inference(avatar_component_clause, [], [f341])).
fof(f803, plain, ((op(e3, e2) = op(op(e3, e2), e3)) | ~ spl15_87), inference(avatar_component_clause, [], [f801])).
fof(f801, plain, (spl15_87 <=> (op(e3, e2) = op(op(e3, e2), e3))), introduced(avatar_definition, [new_symbols(naming, [spl15_87])])).
fof(f1889, plain, (~ spl15_9 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f1888])).
fof(f1888, plain, ($false | (~ spl15_9 | ~ spl15_11)), inference(subsumption_resolution, [], [f1886, f182])).
fof(f1886, plain, ((e0 = e2) | (~ spl15_9 | ~ spl15_11)), inference(backward_demodulation, [], [f356, f348])).
fof(f348, plain, ((e0 = op(e3, e1)) | ~ spl15_9), inference(avatar_component_clause, [], [f346])).
fof(f356, plain, ((e2 = op(e3, e1)) | ~ spl15_11), inference(avatar_component_clause, [], [f354])).
fof(f1882, plain, (~ spl15_6 | ~ spl15_14), inference(avatar_split_clause, [], [f1877, f367, f333])).
fof(f1877, plain, (~ (e1 = op(e3, e2)) | ~ spl15_14), inference(backward_demodulation, [], [f176, f369])).
fof(f1860, plain, (~ spl15_16 | ~ spl15_32), inference(avatar_split_clause, [], [f1856, f443, f375])).
fof(f1856, plain, (~ (e3 = op(e3, e0)) | ~ spl15_32), inference(backward_demodulation, [], [f138, f445])).
fof(f1836, plain, (~ spl15_1 | ~ spl15_44 | spl15_116), inference(avatar_split_clause, [], [f1835, f933, f494, f312])).
fof(f1835, plain, (~ (e0 = op(e3, e3)) | (~ spl15_44 | spl15_116)), inference(forward_demodulation, [], [f935, f496])).
fof(f1834, plain, (~ spl15_30 | ~ spl15_63 | spl15_119), inference(avatar_split_clause, [], [f1833, f949, f575, f435])).
fof(f949, plain, (spl15_119 <=> (e1 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_119])])).
fof(f1833, plain, (~ (e1 = op(e2, e0)) | (~ spl15_63 | spl15_119)), inference(forward_demodulation, [], [f951, f577])).
fof(f951, plain, (~ (e1 = op(op(e0, e0), e0)) | spl15_119), inference(avatar_component_clause, [], [f949])).
fof(f1825, plain, (~ spl15_9 | ~ spl15_44 | spl15_118), inference(avatar_split_clause, [], [f1629, f944, f494, f346])).
fof(f1629, plain, (~ (e0 = op(e3, e1)) | (~ spl15_44 | spl15_118)), inference(backward_demodulation, [], [f946, f496])).
fof(f1808, plain, (~ spl15_21 | ~ spl15_25), inference(avatar_split_clause, [], [f1807, f414, f397])).
fof(f1807, plain, (~ (e0 = op(e2, e2)) | ~ spl15_25), inference(backward_demodulation, [], [f172, f416])).
fof(f1793, plain, (~ spl15_26 | ~ spl15_58), inference(avatar_split_clause, [], [f1787, f554, f418])).
fof(f1787, plain, (~ (e1 = op(e2, e1)) | ~ spl15_58), inference(backward_demodulation, [], [f140, f556])).
fof(f1780, plain, (~ spl15_11 | ~ spl15_44 | spl15_107), inference(avatar_contradiction_clause, [], [f1779])).
fof(f1779, plain, ($false | (~ spl15_11 | ~ spl15_44 | spl15_107)), inference(subsumption_resolution, [], [f1778, f356])).
fof(f1778, plain, (~ (e2 = op(e3, e1)) | (~ spl15_44 | spl15_107)), inference(forward_demodulation, [], [f891, f496])).
fof(f1769, plain, (~ spl15_50 | ~ spl15_34), inference(avatar_split_clause, [], [f1768, f452, f520])).
fof(f1768, plain, (~ (e1 = op(e0, e3)) | ~ spl15_34), inference(forward_demodulation, [], [f151, f454])).
fof(f1759, plain, (~ spl15_22 | ~ spl15_6), inference(avatar_split_clause, [], [f1758, f333, f401])).
fof(f1758, plain, (~ (e1 = op(e2, e2)) | ~ spl15_6), inference(forward_demodulation, [], [f150, f335])).
fof(f1755, plain, (~ spl15_22 | ~ spl15_63 | spl15_117), inference(avatar_split_clause, [], [f1602, f939, f575, f401])).
fof(f1602, plain, (~ (e1 = op(e2, e2)) | (~ spl15_63 | spl15_117)), inference(backward_demodulation, [], [f941, f577])).
fof(f1752, plain, (~ spl15_24 | ~ spl15_63 | spl15_114), inference(avatar_split_clause, [], [f1684, f923, f575, f409])).
fof(f1684, plain, (~ (e3 = op(e2, e2)) | (~ spl15_63 | spl15_114)), inference(forward_demodulation, [], [f925, f577])).
fof(f1726, plain, (~ spl15_3 | ~ spl15_11), inference(avatar_split_clause, [], [f1724, f354, f320])).
fof(f1724, plain, (~ (e2 = op(e3, e3)) | ~ spl15_11), inference(backward_demodulation, [], [f179, f356])).
fof(f179, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f1720, plain, (spl15_4 | ~ spl15_16 | ~ spl15_95), inference(avatar_contradiction_clause, [], [f1719])).
fof(f1719, plain, ($false | (spl15_4 | ~ spl15_16 | ~ spl15_95)), inference(subsumption_resolution, [], [f1717, f325])).
fof(f1717, plain, ((e3 = op(e3, e3)) | (~ spl15_16 | ~ spl15_95)), inference(backward_demodulation, [], [f837, f377])).
fof(f1713, plain, (~ spl15_19 | ~ spl15_20), inference(avatar_contradiction_clause, [], [f1712])).
fof(f1712, plain, ($false | (~ spl15_19 | ~ spl15_20)), inference(subsumption_resolution, [], [f1711, f186])).
fof(f1711, plain, ((e2 = e3) | (~ spl15_19 | ~ spl15_20)), inference(backward_demodulation, [], [f394, f390])).
fof(f1706, plain, (~ spl15_34 | spl15_42 | ~ spl15_81), inference(avatar_contradiction_clause, [], [f1705])).
fof(f1705, plain, ($false | (~ spl15_34 | spl15_42 | ~ spl15_81)), inference(subsumption_resolution, [], [f1701, f487])).
fof(f487, plain, (~ (e1 = op(e1, e1)) | spl15_42), inference(avatar_component_clause, [], [f486])).
fof(f1701, plain, ((e1 = op(e1, e1)) | (~ spl15_34 | ~ spl15_81)), inference(backward_demodulation, [], [f778, f454])).
fof(f1673, plain, (~ spl15_7 | ~ spl15_39), inference(avatar_split_clause, [], [f1672, f473, f337])).
fof(f1672, plain, (~ (e2 = op(e3, e2)) | ~ spl15_39), inference(forward_demodulation, [], [f149, f475])).
fof(f475, plain, ((e2 = op(e1, e2)) | ~ spl15_39), inference(avatar_component_clause, [], [f473])).
fof(f1662, plain, (~ spl15_32 | ~ spl15_20), inference(avatar_split_clause, [], [f1661, f392, f443])).
fof(f1661, plain, (~ (e3 = op(e2, e0)) | ~ spl15_20), inference(forward_demodulation, [], [f171, f394])).
fof(f1658, plain, (~ spl15_32 | ~ spl15_63 | spl15_112), inference(avatar_split_clause, [], [f1600, f913, f575, f443])).
fof(f913, plain, (spl15_112 <=> (e3 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl15_112])])).
fof(f1600, plain, (~ (e3 = op(e2, e0)) | (~ spl15_63 | spl15_112)), inference(backward_demodulation, [], [f915, f577])).
fof(f915, plain, (~ (e3 = op(op(e0, e0), e0)) | spl15_112), inference(avatar_component_clause, [], [f913])).
fof(f1654, plain, (~ spl15_16 | ~ spl15_56 | spl15_84), inference(avatar_split_clause, [], [f1615, f789, f545, f375])).
fof(f1615, plain, (~ (e3 = op(e3, e0)) | (~ spl15_56 | spl15_84)), inference(backward_demodulation, [], [f790, f547])).
fof(f1632, plain, (spl15_12 | ~ spl15_44 | ~ spl15_89), inference(avatar_split_clause, [], [f1625, f810, f494, f358])).
fof(f1625, plain, ((e3 = op(e3, e1)) | (~ spl15_44 | ~ spl15_89)), inference(backward_demodulation, [], [f812, f496])).
fof(f812, plain, ((op(e1, e1) = op(op(e1, e1), e1)) | ~ spl15_89), inference(avatar_component_clause, [], [f810])).
fof(f1631, plain, (~ spl15_12 | ~ spl15_44), inference(avatar_split_clause, [], [f1624, f494, f358])).
fof(f1624, plain, (~ (e3 = op(e3, e1)) | ~ spl15_44), inference(backward_demodulation, [], [f143, f496])).
fof(f1612, plain, (~ spl15_57 | ~ spl15_59), inference(avatar_contradiction_clause, [], [f1611])).
fof(f1611, plain, ($false | (~ spl15_57 | ~ spl15_59)), inference(subsumption_resolution, [], [f1610, f182])).
fof(f1610, plain, ((e0 = e2) | (~ spl15_57 | ~ spl15_59)), inference(forward_demodulation, [], [f560, f552])).
fof(f1591, plain, (~ spl15_62 | ~ spl15_21 | spl15_98), inference(avatar_split_clause, [], [f1590, f849, f397, f571])).
fof(f1590, plain, (~ (op(e0, e0) = e1) | (~ spl15_21 | spl15_98)), inference(forward_demodulation, [], [f851, f399])).
fof(f1579, plain, (~ spl15_52 | ~ spl15_20), inference(avatar_split_clause, [], [f1578, f392, f528])).
fof(f1578, plain, (~ (e3 = op(e0, e3)) | ~ spl15_20), inference(forward_demodulation, [], [f152, f394])).
fof(f1567, plain, (~ spl15_28 | ~ spl15_20), inference(avatar_split_clause, [], [f1566, f392, f426])).
fof(f1566, plain, (~ (e3 = op(e2, e1)) | ~ spl15_20), inference(forward_demodulation, [], [f173, f394])).
fof(f1559, plain, (spl15_8 | ~ spl15_20 | ~ spl15_82), inference(avatar_split_clause, [], [f1558, f780, f392, f341])).
fof(f1558, plain, ((e3 = op(e3, e2)) | (~ spl15_20 | ~ spl15_82)), inference(backward_demodulation, [], [f782, f394])).
fof(f1555, plain, (~ spl15_30 | ~ spl15_31), inference(avatar_contradiction_clause, [], [f1554])).
fof(f1554, plain, ($false | (~ spl15_30 | ~ spl15_31)), inference(subsumption_resolution, [], [f1553, f184])).
fof(f1553, plain, ((e1 = e2) | (~ spl15_30 | ~ spl15_31)), inference(backward_demodulation, [], [f441, f437])).
fof(f1539, plain, (~ spl15_28 | ~ spl15_43 | spl15_103), inference(avatar_split_clause, [], [f1535, f871, f490, f426])).
fof(f1535, plain, (~ (e3 = op(e2, e1)) | (~ spl15_43 | spl15_103)), inference(backward_demodulation, [], [f873, f492])).
fof(f1537, plain, (~ spl15_27 | ~ spl15_43), inference(avatar_split_clause, [], [f1531, f490, f422])).
fof(f1531, plain, (~ (e2 = op(e2, e1)) | ~ spl15_43), inference(backward_demodulation, [], [f142, f492])).
fof(f1530, plain, (~ spl15_46 | ~ spl15_48), inference(avatar_contradiction_clause, [], [f1529])).
fof(f1529, plain, ($false | (~ spl15_46 | ~ spl15_48)), inference(subsumption_resolution, [], [f1528, f185])).
fof(f185, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f1528, plain, ((e1 = e3) | (~ spl15_46 | ~ spl15_48)), inference(backward_demodulation, [], [f513, f505])).
fof(f1499, plain, (~ spl15_48 | ~ spl15_62 | spl15_112), inference(avatar_split_clause, [], [f1414, f913, f571, f511])).
fof(f1414, plain, (~ (e3 = op(e1, e0)) | (~ spl15_62 | spl15_112)), inference(backward_demodulation, [], [f915, f573])).
fof(f1494, plain, (spl15_46 | ~ spl15_62 | ~ spl15_92), inference(avatar_split_clause, [], [f1413, f823, f571, f503])).
fof(f1413, plain, ((e1 = op(e1, e0)) | (~ spl15_62 | ~ spl15_92)), inference(backward_demodulation, [], [f825, f573])).
fof(f825, plain, ((op(e0, e0) = op(op(e0, e0), e0)) | ~ spl15_92), inference(avatar_component_clause, [], [f823])).
fof(f1490, plain, (spl15_42 | ~ spl15_38 | ~ spl15_85), inference(avatar_split_clause, [], [f1458, f793, f469, f486])).
fof(f1458, plain, ((e1 = op(e1, e1)) | (~ spl15_38 | ~ spl15_85)), inference(backward_demodulation, [], [f795, f471])).
fof(f471, plain, ((e1 = op(e1, e2)) | ~ spl15_38), inference(avatar_component_clause, [], [f469])).
fof(f1470, plain, (~ spl15_17 | ~ spl15_21), inference(avatar_split_clause, [], [f1463, f397, f380])).
fof(f1463, plain, (~ (e0 = op(e2, e3)) | ~ spl15_21), inference(backward_demodulation, [], [f174, f399])).
fof(f1459, plain, (~ spl15_22 | ~ spl15_38), inference(avatar_split_clause, [], [f1455, f469, f401])).
fof(f1455, plain, (~ (e1 = op(e2, e2)) | ~ spl15_38), inference(backward_demodulation, [], [f148, f471])).
fof(f1451, plain, (~ spl15_41 | spl15_57 | ~ spl15_89), inference(avatar_contradiction_clause, [], [f1450])).
fof(f1450, plain, ($false | (~ spl15_41 | spl15_57 | ~ spl15_89)), inference(subsumption_resolution, [], [f1446, f551])).
fof(f1446, plain, ((e0 = op(e0, e1)) | (~ spl15_41 | ~ spl15_89)), inference(backward_demodulation, [], [f812, f484])).
fof(f1423, plain, (~ spl15_42 | ~ spl15_62 | spl15_117), inference(avatar_split_clause, [], [f1416, f939, f571, f486])).
fof(f1416, plain, (~ (e1 = op(e1, e1)) | (~ spl15_62 | spl15_117)), inference(backward_demodulation, [], [f941, f573])).
fof(f1422, plain, (~ spl15_47 | ~ spl15_62 | spl15_115), inference(avatar_split_clause, [], [f1415, f927, f571, f507])).
fof(f1415, plain, (~ (e2 = op(e1, e0)) | (~ spl15_62 | spl15_115)), inference(backward_demodulation, [], [f929, f573])).
fof(f1419, plain, (~ spl15_50 | ~ spl15_62), inference(avatar_split_clause, [], [f1410, f571, f520])).
fof(f1410, plain, (~ (e1 = op(e0, e3)) | ~ spl15_62), inference(backward_demodulation, [], [f159, f573])).
fof(f1418, plain, (~ spl15_14 | ~ spl15_62), inference(avatar_split_clause, [], [f1408, f571, f367])).
fof(f1408, plain, (~ (e1 = op(e3, e0)) | ~ spl15_62), inference(backward_demodulation, [], [f135, f573])).
fof(f1405, plain, (~ spl15_3 | spl15_19 | ~ spl15_83), inference(avatar_contradiction_clause, [], [f1404])).
fof(f1404, plain, ($false | (~ spl15_3 | spl15_19 | ~ spl15_83)), inference(subsumption_resolution, [], [f1403, f389])).
fof(f389, plain, (~ (e2 = op(e2, e3)) | spl15_19), inference(avatar_component_clause, [], [f388])).
fof(f1403, plain, ((e2 = op(e2, e3)) | (~ spl15_3 | ~ spl15_83)), inference(forward_demodulation, [], [f786, f322])).
fof(f1402, plain, (~ spl15_22 | ~ spl15_3 | spl15_96), inference(avatar_split_clause, [], [f1401, f840, f320, f401])).
fof(f1401, plain, (~ (e1 = op(e2, e2)) | (~ spl15_3 | spl15_96)), inference(forward_demodulation, [], [f842, f322])).
fof(f1398, plain, (~ spl15_21 | ~ spl15_3 | spl15_108), inference(avatar_split_clause, [], [f1397, f894, f320, f397])).
fof(f1397, plain, (~ (e0 = op(e2, e2)) | (~ spl15_3 | spl15_108)), inference(forward_demodulation, [], [f896, f322])).
fof(f1390, plain, (~ spl15_44 | ~ spl15_60), inference(avatar_split_clause, [], [f1389, f562, f494])).
fof(f1389, plain, (~ (e3 = op(e1, e1)) | ~ spl15_60), inference(forward_demodulation, [], [f139, f564])).
fof(f1388, plain, (~ spl15_16 | ~ spl15_8), inference(avatar_split_clause, [], [f1387, f341, f375])).
fof(f1387, plain, (~ (e3 = op(e3, e0)) | ~ spl15_8), inference(forward_demodulation, [], [f176, f343])).
fof(f1386, plain, (~ spl15_16 | ~ spl15_60 | spl15_88), inference(avatar_split_clause, [], [f1355, f806, f562, f375])).
fof(f1355, plain, (~ (e3 = op(e3, e0)) | (~ spl15_60 | spl15_88)), inference(forward_demodulation, [], [f807, f564])).
fof(f807, plain, (~ (op(e0, e1) = op(op(e0, e1), e0)) | spl15_88), inference(avatar_component_clause, [], [f806])).
fof(f1347, plain, (spl15_31 | ~ spl15_63 | ~ spl15_92), inference(avatar_contradiction_clause, [], [f1346])).
fof(f1346, plain, ($false | (spl15_31 | ~ spl15_63 | ~ spl15_92)), inference(subsumption_resolution, [], [f1345, f440])).
fof(f440, plain, (~ (e2 = op(e2, e0)) | spl15_31), inference(avatar_component_clause, [], [f439])).
fof(f1345, plain, ((e2 = op(e2, e0)) | (~ spl15_63 | ~ spl15_92)), inference(forward_demodulation, [], [f825, f577])).
fof(f1327, plain, (~ spl15_1 | ~ spl15_3), inference(avatar_contradiction_clause, [], [f1326])).
fof(f1326, plain, ($false | (~ spl15_1 | ~ spl15_3)), inference(subsumption_resolution, [], [f1325, f182])).
fof(f1325, plain, ((e0 = e2) | (~ spl15_1 | ~ spl15_3)), inference(backward_demodulation, [], [f322, f314])).
fof(f1323, plain, (~ spl15_6 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1322])).
fof(f1322, plain, ($false | (~ spl15_6 | ~ spl15_8)), inference(subsumption_resolution, [], [f1320, f185])).
fof(f1320, plain, ((e1 = e3) | (~ spl15_6 | ~ spl15_8)), inference(backward_demodulation, [], [f343, f335])).
fof(f1316, plain, (~ spl15_21 | ~ spl15_56 | spl15_99), inference(avatar_contradiction_clause, [], [f1315])).
fof(f1315, plain, ($false | (~ spl15_21 | ~ spl15_56 | spl15_99)), inference(subsumption_resolution, [], [f1311, f547])).
fof(f1311, plain, (~ (e3 = op(e0, e2)) | (~ spl15_21 | spl15_99)), inference(backward_demodulation, [], [f855, f399])).
fof(f855, plain, (~ (e3 = op(op(e2, e2), e2)) | spl15_99), inference(avatar_component_clause, [], [f853])).
fof(f1289, plain, (~ spl15_58 | ~ spl15_42), inference(avatar_split_clause, [], [f1288, f486, f554])).
fof(f1288, plain, (~ (e1 = op(e0, e1)) | ~ spl15_42), inference(forward_demodulation, [], [f139, f488])).
fof(f1284, plain, (~ spl15_56 | ~ spl15_60), inference(avatar_split_clause, [], [f1283, f562, f545])).
fof(f1283, plain, (~ (e3 = op(e0, e2)) | ~ spl15_60), inference(forward_demodulation, [], [f160, f564])).
fof(f1280, plain, (~ spl15_14 | ~ spl15_60 | ~ spl15_88), inference(avatar_contradiction_clause, [], [f1279])).
fof(f1279, plain, ($false | (~ spl15_14 | ~ spl15_60 | ~ spl15_88)), inference(subsumption_resolution, [], [f1278, f185])).
fof(f1278, plain, ((e1 = e3) | (~ spl15_14 | ~ spl15_60 | ~ spl15_88)), inference(forward_demodulation, [], [f1277, f369])).
fof(f1277, plain, ((e3 = op(e3, e0)) | (~ spl15_60 | ~ spl15_88)), inference(forward_demodulation, [], [f808, f564])).
fof(f1249, plain, (~ spl15_11 | ~ spl15_27), inference(avatar_split_clause, [], [f1248, f422, f354])).
fof(f1248, plain, (~ (e2 = op(e3, e1)) | ~ spl15_27), inference(forward_demodulation, [], [f144, f424])).
fof(f1225, plain, (~ spl15_22 | ~ spl15_40 | spl15_99), inference(avatar_contradiction_clause, [], [f1224])).
fof(f1224, plain, ($false | (~ spl15_22 | ~ spl15_40 | spl15_99)), inference(subsumption_resolution, [], [f1221, f479])).
fof(f1221, plain, (~ (e3 = op(e1, e2)) | (~ spl15_22 | spl15_99)), inference(backward_demodulation, [], [f855, f403])).
fof(f1201, plain, (~ spl15_50 | ~ spl15_52), inference(avatar_contradiction_clause, [], [f1200])).
fof(f1200, plain, ($false | (~ spl15_50 | ~ spl15_52)), inference(subsumption_resolution, [], [f1199, f185])).
fof(f1199, plain, ((e1 = e3) | (~ spl15_50 | ~ spl15_52)), inference(forward_demodulation, [], [f530, f522])).
fof(f530, plain, ((e3 = op(e0, e3)) | ~ spl15_52), inference(avatar_component_clause, [], [f528])).
fof(f1193, plain, (~ spl15_57 | ~ spl15_60), inference(avatar_contradiction_clause, [], [f1192])).
fof(f1192, plain, ($false | (~ spl15_57 | ~ spl15_60)), inference(subsumption_resolution, [], [f1191, f183])).
fof(f1191, plain, ((e0 = e3) | (~ spl15_57 | ~ spl15_60)), inference(forward_demodulation, [], [f564, f552])).
fof(f1162, plain, (~ spl15_45 | ~ spl15_50 | ~ spl15_80), inference(avatar_contradiction_clause, [], [f1161])).
fof(f1161, plain, ($false | (~ spl15_45 | ~ spl15_50 | ~ spl15_80)), inference(subsumption_resolution, [], [f1160, f181])).
fof(f1160, plain, ((e0 = e1) | (~ spl15_45 | ~ spl15_50 | ~ spl15_80)), inference(forward_demodulation, [], [f1159, f501])).
fof(f1159, plain, ((e1 = op(e1, e0)) | (~ spl15_50 | ~ spl15_80)), inference(forward_demodulation, [], [f774, f522])).
fof(f1122, plain, (~ spl15_5 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1121])).
fof(f1121, plain, ($false | (~ spl15_5 | ~ spl15_8)), inference(subsumption_resolution, [], [f1120, f183])).
fof(f1120, plain, ((e0 = e3) | (~ spl15_5 | ~ spl15_8)), inference(forward_demodulation, [], [f343, f331])).
fof(f1112, plain, (~ spl15_6 | ~ spl15_7), inference(avatar_contradiction_clause, [], [f1111])).
fof(f1111, plain, ($false | (~ spl15_6 | ~ spl15_7)), inference(subsumption_resolution, [], [f1110, f184])).
fof(f1110, plain, ((e1 = e2) | (~ spl15_6 | ~ spl15_7)), inference(backward_demodulation, [], [f339, f335])).
fof(f339, plain, ((e2 = op(e3, e2)) | ~ spl15_7), inference(avatar_component_clause, [], [f337])).
fof(f1109, plain, (~ spl15_7 | ~ spl15_8), inference(avatar_contradiction_clause, [], [f1108])).
fof(f1108, plain, ($false | (~ spl15_7 | ~ spl15_8)), inference(subsumption_resolution, [], [f1107, f186])).
fof(f1107, plain, ((e2 = e3) | (~ spl15_7 | ~ spl15_8)), inference(backward_demodulation, [], [f343, f339])).
fof(f1104, plain, (~ spl15_10 | ~ spl15_11), inference(avatar_contradiction_clause, [], [f1103])).
fof(f1103, plain, ($false | (~ spl15_10 | ~ spl15_11)), inference(subsumption_resolution, [], [f1102, f184])).
fof(f1102, plain, ((e1 = e2) | (~ spl15_10 | ~ spl15_11)), inference(backward_demodulation, [], [f356, f352])).
fof(f1101, plain, (~ spl15_11 | ~ spl15_12), inference(avatar_contradiction_clause, [], [f1100])).
fof(f1100, plain, ($false | (~ spl15_11 | ~ spl15_12)), inference(subsumption_resolution, [], [f1099, f186])).
fof(f1099, plain, ((e2 = e3) | (~ spl15_11 | ~ spl15_12)), inference(backward_demodulation, [], [f360, f356])).
fof(f1094, plain, (~ spl15_14 | ~ spl15_15), inference(avatar_contradiction_clause, [], [f1093])).
fof(f1093, plain, ($false | (~ spl15_14 | ~ spl15_15)), inference(subsumption_resolution, [], [f1092, f184])).
fof(f1092, plain, ((e1 = e2) | (~ spl15_14 | ~ spl15_15)), inference(backward_demodulation, [], [f373, f369])).
fof(f1082, plain, (~ spl15_18 | ~ spl15_19), inference(avatar_contradiction_clause, [], [f1081])).
fof(f1081, plain, ($false | (~ spl15_18 | ~ spl15_19)), inference(subsumption_resolution, [], [f1080, f184])).
fof(f1080, plain, ((e1 = e2) | (~ spl15_18 | ~ spl15_19)), inference(backward_demodulation, [], [f390, f386])).
fof(f1049, plain, (~ spl15_34 | ~ spl15_35), inference(avatar_contradiction_clause, [], [f1048])).
fof(f1048, plain, ($false | (~ spl15_34 | ~ spl15_35)), inference(subsumption_resolution, [], [f1047, f184])).
fof(f1047, plain, ((e1 = e2) | (~ spl15_34 | ~ spl15_35)), inference(backward_demodulation, [], [f458, f454])).
fof(f1046, plain, (~ spl15_35 | ~ spl15_36), inference(avatar_contradiction_clause, [], [f1045])).
fof(f1045, plain, ($false | (~ spl15_35 | ~ spl15_36)), inference(subsumption_resolution, [], [f1044, f186])).
fof(f1044, plain, ((e2 = e3) | (~ spl15_35 | ~ spl15_36)), inference(backward_demodulation, [], [f462, f458])).
fof(f1029, plain, (~ spl15_37 | ~ spl15_41), inference(avatar_split_clause, [], [f1022, f482, f465])).
fof(f1022, plain, (~ (e0 = op(e1, e2)) | ~ spl15_41), inference(backward_demodulation, [], [f166, f484])).
fof(f1009, plain, (~ spl15_50 | ~ spl15_51), inference(avatar_contradiction_clause, [], [f1008])).
fof(f1008, plain, ($false | (~ spl15_50 | ~ spl15_51)), inference(subsumption_resolution, [], [f1007, f184])).
fof(f1007, plain, ((e1 = e2) | (~ spl15_50 | ~ spl15_51)), inference(backward_demodulation, [], [f526, f522])).
fof(f1005, plain, (~ spl15_3 | ~ spl15_51), inference(avatar_split_clause, [], [f1001, f524, f320])).
fof(f1001, plain, (~ (e2 = op(e3, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f153, f526])).
fof(f1003, plain, (~ spl15_35 | ~ spl15_51), inference(avatar_split_clause, [], [f999, f524, f456])).
fof(f999, plain, (~ (e2 = op(e1, e3)) | ~ spl15_51), inference(backward_demodulation, [], [f151, f526])).
fof(f998, plain, (spl15_61 | ~ spl15_53 | ~ spl15_84), inference(avatar_split_clause, [], [f993, f789, f533, f567])).
fof(f993, plain, ((e0 = op(e0, e0)) | (~ spl15_53 | ~ spl15_84)), inference(backward_demodulation, [], [f791, f535])).
fof(f997, plain, (~ spl15_49 | ~ spl15_53), inference(avatar_split_clause, [], [f992, f533, f516])).
fof(f992, plain, (~ (e0 = op(e0, e3)) | ~ spl15_53), inference(backward_demodulation, [], [f162, f535])).
fof(f994, plain, (~ spl15_37 | ~ spl15_53), inference(avatar_split_clause, [], [f989, f533, f465])).
fof(f989, plain, (~ (e0 = op(e1, e2)) | ~ spl15_53), inference(backward_demodulation, [], [f145, f535])).
fof(f988, plain, (spl15_61 | ~ spl15_57 | ~ spl15_88), inference(avatar_split_clause, [], [f982, f806, f550, f567])).
fof(f982, plain, ((e0 = op(e0, e0)) | (~ spl15_57 | ~ spl15_88)), inference(backward_demodulation, [], [f808, f552])).
fof(f986, plain, (~ spl15_53 | ~ spl15_57), inference(avatar_split_clause, [], [f980, f550, f533])).
fof(f980, plain, (~ (e0 = op(e0, e2)) | ~ spl15_57), inference(backward_demodulation, [], [f160, f552])).
fof(f985, plain, (~ spl15_9 | ~ spl15_57), inference(avatar_split_clause, [], [f979, f550, f346])).
fof(f979, plain, (~ (e0 = op(e3, e1)) | ~ spl15_57), inference(backward_demodulation, [], [f141, f552])).
fof(f976, plain, (~ spl15_14 | ~ spl15_64 | spl15_119), inference(avatar_split_clause, [], [f967, f949, f579, f367])).
fof(f967, plain, (~ (e1 = op(e3, e0)) | (~ spl15_64 | spl15_119)), inference(backward_demodulation, [], [f951, f581])).
fof(f974, plain, (spl15_16 | ~ spl15_64 | ~ spl15_92), inference(avatar_split_clause, [], [f965, f823, f579, f375])).
fof(f965, plain, ((e3 = op(e3, e0)) | (~ spl15_64 | ~ spl15_92)), inference(backward_demodulation, [], [f825, f581])).
fof(f972, plain, (~ spl15_56 | ~ spl15_64), inference(avatar_split_clause, [], [f963, f579, f545])).
fof(f963, plain, (~ (e3 = op(e0, e2)) | ~ spl15_64), inference(backward_demodulation, [], [f158, f581])).
fof(f958, plain, (~ spl15_64 | ~ spl15_111 | ~ spl15_119), inference(avatar_split_clause, [], [f310, f949, f909, f579])).
fof(f310, plain, (~ (e1 = op(op(e0, e0), e0)) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e1 = op(op(e0, e0), e0)) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e3)), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e1 = op(op(e0, e0), e0)) & (e2 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e3)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax29)).
fof(f957, plain, (~ spl15_44 | ~ spl15_102 | ~ spl15_118), inference(avatar_split_clause, [], [f309, f944, f867, f494])).
fof(f309, plain, (~ (e0 = op(op(e1, e1), e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (~ (e0 = op(op(e1, e1), e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ~ ((e0 = op(op(e1, e1), e1)) & (e2 = op(op(e1, e1), op(e1, e1))) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax28)).
fof(f956, plain, (~ spl15_64 | ~ spl15_117 | ~ spl15_115), inference(avatar_split_clause, [], [f308, f927, f939, f579])).
fof(f308, plain, (~ (e2 = op(op(e0, e0), e0)) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e2 = op(op(e0, e0), e0)) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e3)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e2 = op(op(e0, e0), e0)) & (e1 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e3)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax27)).
fof(f955, plain, (~ spl15_24 | ~ spl15_98 | ~ spl15_113), inference(avatar_split_clause, [], [f307, f918, f849, f409])).
fof(f307, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f50])).
fof(f50, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ~ ((e0 = op(op(e2, e2), e2)) & (e1 = op(op(e2, e2), op(e2, e2))) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax26)).
fof(f954, plain, (~ spl15_44 | ~ spl15_116 | ~ spl15_107), inference(avatar_split_clause, [], [f306, f889, f933, f494])).
fof(f306, plain, (~ (e2 = op(op(e1, e1), e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, (~ (e2 = op(op(e1, e1), e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e3 = op(e1, e1))), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ ((e2 = op(op(e1, e1), e1)) & (e0 = op(op(e1, e1), op(e1, e1))) & (e3 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax25)).
fof(f953, plain, (~ spl15_24 | ~ spl15_109 | ~ spl15_105), inference(avatar_split_clause, [], [f305, f880, f899, f409])).
fof(f305, plain, (~ (e1 = op(op(e2, e2), e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f48])).
fof(f48, plain, (~ (e1 = op(op(e2, e2), e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e3 = op(e2, e2))), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ~ ((e1 = op(op(e2, e2), e2)) & (e0 = op(op(e2, e2), op(e2, e2))) & (e3 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax24)).
fof(f952, plain, (~ spl15_63 | ~ spl15_114 | ~ spl15_119), inference(avatar_split_clause, [], [f304, f949, f923, f575])).
fof(f304, plain, (~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e1 = op(op(e0, e0), e0)) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e1 = op(op(e0, e0), e0)) & (e3 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e2)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax23)).
fof(f947, plain, (~ spl15_43 | ~ spl15_106 | ~ spl15_118), inference(avatar_split_clause, [], [f303, f944, f885, f490])).
fof(f303, plain, (~ (e0 = op(op(e1, e1), e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e0 = op(op(e1, e1), e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(e1, e1))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e0 = op(op(e1, e1), e1)) & (e3 = op(op(e1, e1), op(e1, e1))) & (e2 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax22)).
fof(f942, plain, (~ spl15_63 | ~ spl15_117 | ~ spl15_112), inference(avatar_split_clause, [], [f302, f913, f939, f575])).
fof(f302, plain, (~ (e3 = op(op(e0, e0), e0)) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (~ (e3 = op(op(e0, e0), e0)) | ~ (e1 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e2)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((e3 = op(op(e0, e0), e0)) & (e1 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e2)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax21)).
fof(f937, plain, (~ spl15_3 | ~ spl15_96 | ~ spl15_110), inference(avatar_split_clause, [], [f301, f904, f840, f320])).
fof(f301, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ((e0 = op(op(e3, e3), e3)) & (e1 = op(op(e3, e3), op(e3, e3))) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax20)).
fof(f936, plain, (~ spl15_43 | ~ spl15_116 | ~ spl15_103), inference(avatar_split_clause, [], [f300, f871, f933, f490])).
fof(f300, plain, (~ (e3 = op(op(e1, e1), e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (~ (e3 = op(op(e1, e1), e1)) | ~ (e0 = op(op(e1, e1), op(e1, e1))) | ~ (e2 = op(e1, e1))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((e3 = op(op(e1, e1), e1)) & (e0 = op(op(e1, e1), op(e1, e1))) & (e2 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax19)).
fof(f931, plain, (~ spl15_3 | ~ spl15_108 | ~ spl15_101), inference(avatar_split_clause, [], [f299, f862, f894, f320])).
fof(f299, plain, (~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e1 = op(op(e3, e3), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e2 = op(e3, e3))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e1 = op(op(e3, e3), e3)) & (e0 = op(op(e3, e3), op(e3, e3))) & (e2 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax18)).
fof(f930, plain, (~ spl15_62 | ~ spl15_114 | ~ spl15_115), inference(avatar_split_clause, [], [f298, f927, f923, f571])).
fof(f298, plain, (~ (e2 = op(op(e0, e0), e0)) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (~ (e2 = op(op(e0, e0), e0)) | ~ (e3 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ ((e2 = op(op(e0, e0), e0)) & (e3 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax17)).
fof(f921, plain, (~ spl15_22 | ~ spl15_104 | ~ spl15_113), inference(avatar_split_clause, [], [f297, f918, f876, f401])).
fof(f297, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (e0 = op(op(e2, e2), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(e2, e2))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ~ ((e0 = op(op(e2, e2), e2)) & (e3 = op(op(e2, e2), op(e2, e2))) & (e1 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax16)).
fof(f916, plain, (~ spl15_62 | ~ spl15_111 | ~ spl15_112), inference(avatar_split_clause, [], [f296, f913, f909, f571])).
fof(f296, plain, (~ (e3 = op(op(e0, e0), e0)) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e3 = op(op(e0, e0), e0)) | ~ (e2 = op(op(e0, e0), op(e0, e0))) | ~ (op(e0, e0) = e1)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e3 = op(op(e0, e0), e0)) & (e2 = op(op(e0, e0), op(e0, e0))) & (op(e0, e0) = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax15)).
fof(f907, plain, (~ spl15_2 | ~ spl15_100 | ~ spl15_110), inference(avatar_split_clause, [], [f295, f904, f858, f316])).
fof(f295, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e0 = op(op(e3, e3), e3)) | ~ (e2 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e0 = op(op(e3, e3), e3)) & (e2 = op(op(e3, e3), op(e3, e3))) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax14)).
fof(f902, plain, (~ spl15_22 | ~ spl15_109 | ~ spl15_99), inference(avatar_split_clause, [], [f294, f853, f899, f401])).
fof(f294, plain, (~ (e3 = op(op(e2, e2), e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (~ (e3 = op(op(e2, e2), e2)) | ~ (e0 = op(op(e2, e2), op(e2, e2))) | ~ (e1 = op(e2, e2))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ ((e3 = op(op(e2, e2), e2)) & (e0 = op(op(e2, e2), op(e2, e2))) & (e1 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax13)).
fof(f897, plain, (~ spl15_2 | ~ spl15_108 | ~ spl15_97), inference(avatar_split_clause, [], [f293, f844, f894, f316])).
fof(f293, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e0 = op(op(e3, e3), op(e3, e3))) | ~ (e1 = op(e3, e3))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ~ ((e2 = op(op(e3, e3), e3)) & (e0 = op(op(e3, e3), op(e3, e3))) & (e1 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax12)).
fof(f892, plain, (~ spl15_41 | ~ spl15_106 | ~ spl15_107), inference(avatar_split_clause, [], [f292, f889, f885, f482])).
fof(f292, plain, (~ (e2 = op(op(e1, e1), e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, (~ (e2 = op(op(e1, e1), e1)) | ~ (e3 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ~ ((e2 = op(op(e1, e1), e1)) & (e3 = op(op(e1, e1), op(e1, e1))) & (e0 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax11)).
fof(f883, plain, (~ spl15_21 | ~ spl15_104 | ~ spl15_105), inference(avatar_split_clause, [], [f291, f880, f876, f397])).
fof(f291, plain, (~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (~ (e1 = op(op(e2, e2), e2)) | ~ (e3 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(e2, e2))), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ~ ((e1 = op(op(e2, e2), e2)) & (e3 = op(op(e2, e2), op(e2, e2))) & (e0 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax10)).
fof(f874, plain, (~ spl15_41 | ~ spl15_102 | ~ spl15_103), inference(avatar_split_clause, [], [f290, f871, f867, f482])).
fof(f290, plain, (~ (e3 = op(op(e1, e1), e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, (~ (e3 = op(op(e1, e1), e1)) | ~ (e2 = op(op(e1, e1), op(e1, e1))) | ~ (e0 = op(e1, e1))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ~ ((e3 = op(op(e1, e1), e1)) & (e2 = op(op(e1, e1), op(e1, e1))) & (e0 = op(e1, e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax9)).
fof(f856, plain, (~ spl15_21 | ~ spl15_98 | ~ spl15_99), inference(avatar_split_clause, [], [f288, f853, f849, f397])).
fof(f288, plain, (~ (e3 = op(op(e2, e2), e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e3 = op(op(e2, e2), e2)) | ~ (e1 = op(op(e2, e2), op(e2, e2))) | ~ (e0 = op(e2, e2))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e3 = op(op(e2, e2), e2)) & (e1 = op(op(e2, e2), op(e2, e2))) & (e0 = op(e2, e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax7)).
fof(f847, plain, (~ spl15_1 | ~ spl15_96 | ~ spl15_97), inference(avatar_split_clause, [], [f287, f844, f840, f312])).
fof(f287, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e2 = op(op(e3, e3), e3)) | ~ (e1 = op(op(e3, e3), op(e3, e3))) | ~ (e0 = op(e3, e3))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e2 = op(op(e3, e3), e3)) & (e1 = op(op(e3, e3), op(e3, e3))) & (e0 = op(e3, e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax6)).
fof(f838, plain, (spl15_92 | spl15_93 | spl15_94 | spl15_95), inference(avatar_split_clause, [], [f277, f835, f831, f827, f823])).
fof(f277, plain, ((op(e3, e0) = op(op(e3, e0), e3)) | (op(e2, e0) = op(op(e2, e0), e2)) | (op(e1, e0) = op(op(e1, e0), e1)) | (op(e0, e0) = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((op(e3, e3) = op(op(e3, e3), e3)) | (op(e2, e3) = op(op(e2, e3), e2)) | (op(e1, e3) = op(op(e1, e3), e1)) | (op(e0, e3) = op(op(e0, e3), e0))) & ((op(e3, e2) = op(op(e3, e2), e3)) | (op(e2, e2) = op(op(e2, e2), e2)) | (op(e1, e2) = op(op(e1, e2), e1)) | (op(e0, e2) = op(op(e0, e2), e0))) & ((op(e3, e1) = op(op(e3, e1), e3)) | (op(e2, e1) = op(op(e2, e1), e2)) | (op(e1, e1) = op(op(e1, e1), e1)) | (op(e0, e1) = op(op(e0, e1), e0))) & ((op(e3, e0) = op(op(e3, e0), e3)) | (op(e2, e0) = op(op(e2, e0), e2)) | (op(e1, e0) = op(op(e1, e0), e1)) | (op(e0, e0) = op(op(e0, e0), e0)))), inference(definition_folding, [], [f5, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54])).
fof(f54, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e0 = op(e0, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e0 = op(e0, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f57, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(usedef, [], [e57])).
fof(e57, plain, (sP3 <=> (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e0 = op(e0, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f58, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | ~ sP4), inference(usedef, [], [e58])).
fof(e58, plain, (sP4 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e1 = op(e1, e0)) & (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f59, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e59])).
fof(e59, plain, (sP5 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f60, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | ~ sP6), inference(usedef, [], [e60])).
fof(e60, plain, (sP6 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e1 = op(e1, e2)) & (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f61, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | ~ sP7), inference(usedef, [], [e61])).
fof(e61, plain, (sP7 <=> (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e1 = op(e1, e3)) & (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f62, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | ~ sP8), inference(usedef, [], [e62])).
fof(e62, plain, (sP8 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e2 = op(e2, e0)) & (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f63, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | ~ sP9), inference(usedef, [], [e63])).
fof(e63, plain, (sP9 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e2 = op(e2, e1)) & (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f64, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e64])).
fof(e64, plain, (sP10 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f65, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | ~ sP11), inference(usedef, [], [e65])).
fof(e65, plain, (sP11 <=> (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e2 = op(e2, e3)) & (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f66, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | ~ sP12), inference(usedef, [], [e66])).
fof(e66, plain, (sP12 <=> (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e3 = op(e3, e0)) & (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f67, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | ~ sP13), inference(usedef, [], [e67])).
fof(e67, plain, (sP13 <=> (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e3 = op(e3, e1)) & (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f68, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | ~ sP14), inference(usedef, [], [e68])).
fof(e68, plain, (sP14 <=> (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e3 = op(e3, e2)) & (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f5, plain, (((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | (~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | (~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | (~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | (~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((op(e3, e3) = op(op(e3, e3), e3)) | (op(e2, e3) = op(op(e2, e3), e2)) | (op(e1, e3) = op(op(e1, e3), e1)) | (op(e0, e3) = op(op(e0, e3), e0))) & ((op(e3, e2) = op(op(e3, e2), e3)) | (op(e2, e2) = op(op(e2, e2), e2)) | (op(e1, e2) = op(op(e1, e2), e1)) | (op(e0, e2) = op(op(e0, e2), e0))) & ((op(e3, e1) = op(op(e3, e1), e3)) | (op(e2, e1) = op(op(e2, e1), e2)) | (op(e1, e1) = op(op(e1, e1), e1)) | (op(e0, e1) = op(op(e0, e1), e0))) & ((op(e3, e0) = op(op(e3, e0), e3)) | (op(e2, e0) = op(op(e2, e0), e2)) | (op(e1, e0) = op(op(e1, e0), e1)) | (op(e0, e0) = op(op(e0, e0), e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax5)).
fof(f821, plain, (spl15_88 | spl15_89 | spl15_90 | spl15_91), inference(avatar_split_clause, [], [f278, f818, f814, f810, f806])).
fof(f278, plain, ((op(e3, e1) = op(op(e3, e1), e3)) | (op(e2, e1) = op(op(e2, e1), e2)) | (op(e1, e1) = op(op(e1, e1), e1)) | (op(e0, e1) = op(op(e0, e1), e0))), inference(cnf_transformation, [], [f69])).
fof(f804, plain, (spl15_84 | spl15_85 | spl15_86 | spl15_87), inference(avatar_split_clause, [], [f279, f801, f797, f793, f789])).
fof(f279, plain, ((op(e3, e2) = op(op(e3, e2), e3)) | (op(e2, e2) = op(op(e2, e2), e2)) | (op(e1, e2) = op(op(e1, e2), e1)) | (op(e0, e2) = op(op(e0, e2), e0))), inference(cnf_transformation, [], [f69])).
fof(f787, plain, (spl15_80 | spl15_81 | spl15_82 | spl15_83), inference(avatar_split_clause, [], [f280, f784, f780, f776, f772])).
fof(f280, plain, ((op(e3, e3) = op(op(e3, e3), e3)) | (op(e2, e3) = op(op(e2, e3), e2)) | (op(e1, e3) = op(op(e1, e3), e1)) | (op(e0, e3) = op(op(e0, e3), e0))), inference(cnf_transformation, [], [f69])).
fof(f770, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | spl15_4), inference(avatar_split_clause, [], [f281, f324, f616, f626, f636, f646, f656, f666, f676, f686, f696, f706, f716, f726, f736, f746, f756])).
fof(f756, plain, (spl15_79 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl15_79])])).
fof(f746, plain, (spl15_78 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl15_78])])).
fof(f736, plain, (spl15_77 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl15_77])])).
fof(f726, plain, (spl15_76 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl15_76])])).
fof(f716, plain, (spl15_75 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl15_75])])).
fof(f706, plain, (spl15_74 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl15_74])])).
fof(f696, plain, (spl15_73 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl15_73])])).
fof(f686, plain, (spl15_72 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl15_72])])).
fof(f676, plain, (spl15_71 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl15_71])])).
fof(f666, plain, (spl15_70 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl15_70])])).
fof(f656, plain, (spl15_69 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl15_69])])).
fof(f646, plain, (spl15_68 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl15_68])])).
fof(f636, plain, (spl15_67 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl15_67])])).
fof(f626, plain, (spl15_66 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl15_66])])).
fof(f616, plain, (spl15_65 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl15_65])])).
fof(f281, plain, ((e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f765, plain, (spl15_79 | spl15_78 | spl15_77 | spl15_76 | spl15_75 | spl15_74 | spl15_73 | spl15_72 | spl15_71 | spl15_70 | spl15_69 | spl15_68 | spl15_67 | spl15_66 | spl15_65 | ~ spl15_4), inference(avatar_split_clause, [], [f286, f324, f616, f626, f636, f646, f656, f666, f676, f686, f696, f706, f716, f726, f736, f746, f756])).
fof(f286, plain, (~ (e3 = op(e3, e3)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f69])).
fof(f764, plain, (~ spl15_79 | spl15_61), inference(avatar_split_clause, [], [f271, f567, f756])).
fof(f271, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f762, plain, (~ spl15_79 | ~ spl15_61), inference(avatar_split_clause, [], [f273, f567, f756])).
fof(f273, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f84])).
fof(f754, plain, (~ spl15_78 | spl15_62), inference(avatar_split_clause, [], [f265, f571, f746])).
fof(f265, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e0 = op(e0, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f753, plain, (~ spl15_78 | spl15_57), inference(avatar_split_clause, [], [f266, f550, f746])).
fof(f266, plain, ((e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f752, plain, (~ spl15_78 | ~ spl15_45), inference(avatar_split_clause, [], [f267, f499, f746])).
fof(f267, plain, (~ (e0 = op(e1, e0)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f751, plain, (~ spl15_78 | ~ spl15_42), inference(avatar_split_clause, [], [f268, f486, f746])).
fof(f268, plain, (~ (e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f749, plain, (~ spl15_78 | ~ spl15_36), inference(avatar_split_clause, [], [f270, f460, f746])).
fof(f270, plain, (~ (e3 = op(e1, e3)) | ~ sP1), inference(cnf_transformation, [], [f83])).
fof(f744, plain, (~ spl15_77 | spl15_63), inference(avatar_split_clause, [], [f259, f575, f736])).
fof(f259, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e0 = op(e0, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f743, plain, (~ spl15_77 | spl15_53), inference(avatar_split_clause, [], [f260, f533, f736])).
fof(f260, plain, ((e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f742, plain, (~ spl15_77 | ~ spl15_29), inference(avatar_split_clause, [], [f261, f431, f736])).
fof(f261, plain, (~ (e0 = op(e2, e0)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f740, plain, (~ spl15_77 | ~ spl15_23), inference(avatar_split_clause, [], [f263, f405, f736])).
fof(f263, plain, (~ (e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f739, plain, (~ spl15_77 | ~ spl15_20), inference(avatar_split_clause, [], [f264, f392, f736])).
fof(f264, plain, (~ (e3 = op(e2, e3)) | ~ sP2), inference(cnf_transformation, [], [f82])).
fof(f734, plain, (~ spl15_76 | spl15_64), inference(avatar_split_clause, [], [f253, f579, f726])).
fof(f253, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e0 = op(e0, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f57])).
fof(f733, plain, (~ spl15_76 | spl15_49), inference(avatar_split_clause, [], [f254, f516, f726])).
fof(f254, plain, ((e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f732, plain, (~ spl15_76 | ~ spl15_13), inference(avatar_split_clause, [], [f255, f363, f726])).
fof(f255, plain, (~ (e0 = op(e3, e0)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f731, plain, (~ spl15_76 | ~ spl15_10), inference(avatar_split_clause, [], [f256, f350, f726])).
fof(f256, plain, (~ (e1 = op(e3, e1)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f730, plain, (~ spl15_76 | ~ spl15_7), inference(avatar_split_clause, [], [f257, f337, f726])).
fof(f257, plain, (~ (e2 = op(e3, e2)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f729, plain, (~ spl15_76 | ~ spl15_4), inference(avatar_split_clause, [], [f258, f324, f726])).
fof(f258, plain, (~ (e3 = op(e3, e3)) | ~ sP3), inference(cnf_transformation, [], [f81])).
fof(f724, plain, (~ spl15_75 | spl15_41), inference(avatar_split_clause, [], [f247, f482, f716])).
fof(f247, plain, ((e0 = op(e1, e1)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e1 = op(e1, e0)) & (e0 = op(e1, e1))) | ~ sP4), inference(nnf_transformation, [], [f58])).
fof(f723, plain, (~ spl15_75 | spl15_46), inference(avatar_split_clause, [], [f248, f503, f716])).
fof(f248, plain, ((e1 = op(e1, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f722, plain, (~ spl15_75 | ~ spl15_61), inference(avatar_split_clause, [], [f249, f567, f716])).
fof(f249, plain, (~ (e0 = op(e0, e0)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f721, plain, (~ spl15_75 | ~ spl15_58), inference(avatar_split_clause, [], [f250, f554, f716])).
fof(f250, plain, (~ (e1 = op(e0, e1)) | ~ sP4), inference(cnf_transformation, [], [f80])).
fof(f714, plain, (~ spl15_74 | spl15_42), inference(avatar_split_clause, [], [f241, f486, f706])).
fof(f241, plain, ((e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f59])).
fof(f711, plain, (~ spl15_74 | ~ spl15_42), inference(avatar_split_clause, [], [f244, f486, f706])).
fof(f244, plain, (~ (e1 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f79])).
fof(f704, plain, (~ spl15_73 | spl15_43), inference(avatar_split_clause, [], [f235, f490, f696])).
fof(f235, plain, ((e2 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e1 = op(e1, e2)) & (e2 = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f60])).
fof(f703, plain, (~ spl15_73 | spl15_38), inference(avatar_split_clause, [], [f236, f469, f696])).
fof(f236, plain, ((e1 = op(e1, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f701, plain, (~ spl15_73 | ~ spl15_26), inference(avatar_split_clause, [], [f238, f418, f696])).
fof(f238, plain, (~ (e1 = op(e2, e1)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f700, plain, (~ spl15_73 | ~ spl15_23), inference(avatar_split_clause, [], [f239, f405, f696])).
fof(f239, plain, (~ (e2 = op(e2, e2)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f699, plain, (~ spl15_73 | ~ spl15_20), inference(avatar_split_clause, [], [f240, f392, f696])).
fof(f240, plain, (~ (e3 = op(e2, e3)) | ~ sP6), inference(cnf_transformation, [], [f78])).
fof(f694, plain, (~ spl15_72 | spl15_44), inference(avatar_split_clause, [], [f229, f494, f686])).
fof(f229, plain, ((e3 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e1 = op(e1, e3)) & (e3 = op(e1, e1))) | ~ sP7), inference(nnf_transformation, [], [f61])).
fof(f693, plain, (~ spl15_72 | spl15_34), inference(avatar_split_clause, [], [f230, f452, f686])).
fof(f230, plain, ((e1 = op(e1, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f691, plain, (~ spl15_72 | ~ spl15_10), inference(avatar_split_clause, [], [f232, f350, f686])).
fof(f232, plain, (~ (e1 = op(e3, e1)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f690, plain, (~ spl15_72 | ~ spl15_7), inference(avatar_split_clause, [], [f233, f337, f686])).
fof(f233, plain, (~ (e2 = op(e3, e2)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f689, plain, (~ spl15_72 | ~ spl15_4), inference(avatar_split_clause, [], [f234, f324, f686])).
fof(f234, plain, (~ (e3 = op(e3, e3)) | ~ sP7), inference(cnf_transformation, [], [f77])).
fof(f684, plain, (~ spl15_71 | spl15_21), inference(avatar_split_clause, [], [f223, f397, f676])).
fof(f223, plain, ((e0 = op(e2, e2)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e2 = op(e2, e0)) & (e0 = op(e2, e2))) | ~ sP8), inference(nnf_transformation, [], [f62])).
fof(f683, plain, (~ spl15_71 | spl15_31), inference(avatar_split_clause, [], [f224, f439, f676])).
fof(f224, plain, ((e2 = op(e2, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f682, plain, (~ spl15_71 | ~ spl15_61), inference(avatar_split_clause, [], [f225, f567, f676])).
fof(f225, plain, (~ (e0 = op(e0, e0)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f681, plain, (~ spl15_71 | ~ spl15_58), inference(avatar_split_clause, [], [f226, f554, f676])).
fof(f226, plain, (~ (e1 = op(e0, e1)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f680, plain, (~ spl15_71 | ~ spl15_55), inference(avatar_split_clause, [], [f227, f541, f676])).
fof(f227, plain, (~ (e2 = op(e0, e2)) | ~ sP8), inference(cnf_transformation, [], [f76])).
fof(f674, plain, (~ spl15_70 | spl15_22), inference(avatar_split_clause, [], [f217, f401, f666])).
fof(f217, plain, ((e1 = op(e2, e2)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e2 = op(e2, e1)) & (e1 = op(e2, e2))) | ~ sP9), inference(nnf_transformation, [], [f63])).
fof(f673, plain, (~ spl15_70 | spl15_27), inference(avatar_split_clause, [], [f218, f422, f666])).
fof(f218, plain, ((e2 = op(e2, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f672, plain, (~ spl15_70 | ~ spl15_45), inference(avatar_split_clause, [], [f219, f499, f666])).
fof(f219, plain, (~ (e0 = op(e1, e0)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f671, plain, (~ spl15_70 | ~ spl15_42), inference(avatar_split_clause, [], [f220, f486, f666])).
fof(f220, plain, (~ (e1 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f670, plain, (~ spl15_70 | ~ spl15_39), inference(avatar_split_clause, [], [f221, f473, f666])).
fof(f221, plain, (~ (e2 = op(e1, e2)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f669, plain, (~ spl15_70 | ~ spl15_36), inference(avatar_split_clause, [], [f222, f460, f666])).
fof(f222, plain, (~ (e3 = op(e1, e3)) | ~ sP9), inference(cnf_transformation, [], [f75])).
fof(f664, plain, (~ spl15_69 | spl15_23), inference(avatar_split_clause, [], [f211, f405, f656])).
fof(f211, plain, ((e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f64])).
fof(f660, plain, (~ spl15_69 | ~ spl15_23), inference(avatar_split_clause, [], [f215, f405, f656])).
fof(f215, plain, (~ (e2 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f74])).
fof(f654, plain, (~ spl15_68 | spl15_24), inference(avatar_split_clause, [], [f205, f409, f646])).
fof(f205, plain, ((e3 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)) & (e2 = op(e2, e3)) & (e3 = op(e2, e2))) | ~ sP11), inference(nnf_transformation, [], [f65])).
fof(f653, plain, (~ spl15_68 | spl15_19), inference(avatar_split_clause, [], [f206, f388, f646])).
fof(f206, plain, ((e2 = op(e2, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f651, plain, (~ spl15_68 | ~ spl15_10), inference(avatar_split_clause, [], [f208, f350, f646])).
fof(f208, plain, (~ (e1 = op(e3, e1)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f650, plain, (~ spl15_68 | ~ spl15_7), inference(avatar_split_clause, [], [f209, f337, f646])).
fof(f209, plain, (~ (e2 = op(e3, e2)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f649, plain, (~ spl15_68 | ~ spl15_4), inference(avatar_split_clause, [], [f210, f324, f646])).
fof(f210, plain, (~ (e3 = op(e3, e3)) | ~ sP11), inference(cnf_transformation, [], [f73])).
fof(f644, plain, (~ spl15_67 | spl15_1), inference(avatar_split_clause, [], [f199, f312, f636])).
fof(f199, plain, ((e0 = op(e3, e3)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)) & (e3 = op(e3, e0)) & (e0 = op(e3, e3))) | ~ sP12), inference(nnf_transformation, [], [f66])).
fof(f643, plain, (~ spl15_67 | spl15_16), inference(avatar_split_clause, [], [f200, f375, f636])).
fof(f200, plain, ((e3 = op(e3, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f642, plain, (~ spl15_67 | ~ spl15_61), inference(avatar_split_clause, [], [f201, f567, f636])).
fof(f201, plain, (~ (e0 = op(e0, e0)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f641, plain, (~ spl15_67 | ~ spl15_58), inference(avatar_split_clause, [], [f202, f554, f636])).
fof(f202, plain, (~ (e1 = op(e0, e1)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f640, plain, (~ spl15_67 | ~ spl15_55), inference(avatar_split_clause, [], [f203, f541, f636])).
fof(f203, plain, (~ (e2 = op(e0, e2)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f639, plain, (~ spl15_67 | ~ spl15_52), inference(avatar_split_clause, [], [f204, f528, f636])).
fof(f204, plain, (~ (e3 = op(e0, e3)) | ~ sP12), inference(cnf_transformation, [], [f72])).
fof(f634, plain, (~ spl15_66 | spl15_2), inference(avatar_split_clause, [], [f193, f316, f626])).
fof(f193, plain, ((e1 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)) & (e3 = op(e3, e1)) & (e1 = op(e3, e3))) | ~ sP13), inference(nnf_transformation, [], [f67])).
fof(f633, plain, (~ spl15_66 | spl15_12), inference(avatar_split_clause, [], [f194, f358, f626])).
fof(f194, plain, ((e3 = op(e3, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f632, plain, (~ spl15_66 | ~ spl15_45), inference(avatar_split_clause, [], [f195, f499, f626])).
fof(f195, plain, (~ (e0 = op(e1, e0)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f631, plain, (~ spl15_66 | ~ spl15_42), inference(avatar_split_clause, [], [f196, f486, f626])).
fof(f196, plain, (~ (e1 = op(e1, e1)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f630, plain, (~ spl15_66 | ~ spl15_39), inference(avatar_split_clause, [], [f197, f473, f626])).
fof(f197, plain, (~ (e2 = op(e1, e2)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f629, plain, (~ spl15_66 | ~ spl15_36), inference(avatar_split_clause, [], [f198, f460, f626])).
fof(f198, plain, (~ (e3 = op(e1, e3)) | ~ sP13), inference(cnf_transformation, [], [f71])).
fof(f624, plain, (~ spl15_65 | spl15_3), inference(avatar_split_clause, [], [f187, f320, f616])).
fof(f187, plain, ((e2 = op(e3, e3)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)) & (e3 = op(e3, e2)) & (e2 = op(e3, e3))) | ~ sP14), inference(nnf_transformation, [], [f68])).
fof(f623, plain, (~ spl15_65 | spl15_8), inference(avatar_split_clause, [], [f188, f341, f616])).
fof(f188, plain, ((e3 = op(e3, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f620, plain, (~ spl15_65 | ~ spl15_23), inference(avatar_split_clause, [], [f191, f405, f616])).
fof(f191, plain, (~ (e2 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f619, plain, (~ spl15_65 | ~ spl15_20), inference(avatar_split_clause, [], [f192, f392, f616])).
fof(f192, plain, (~ (e3 = op(e2, e3)) | ~ sP14), inference(cnf_transformation, [], [f70])).
fof(f614, plain, (spl15_61 | spl15_57 | spl15_53 | spl15_49), inference(avatar_split_clause, [], [f101, f516, f533, f550, f567])).
fof(f101, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax2)).
fof(f613, plain, (spl15_61 | spl15_45 | spl15_29 | spl15_13), inference(avatar_split_clause, [], [f102, f363, f431, f499, f567])).
fof(f102, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f612, plain, (spl15_62 | spl15_58 | spl15_54 | spl15_50), inference(avatar_split_clause, [], [f103, f520, f537, f554, f571])).
fof(f103, plain, ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f611, plain, (spl15_62 | spl15_46 | spl15_30 | spl15_14), inference(avatar_split_clause, [], [f104, f367, f435, f503, f571])).
fof(f104, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f608, plain, (spl15_64 | spl15_60 | spl15_56 | spl15_52), inference(avatar_split_clause, [], [f107, f528, f545, f562, f579])).
fof(f107, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f607, plain, (spl15_64 | spl15_48 | spl15_32 | spl15_16), inference(avatar_split_clause, [], [f108, f375, f443, f511, f579])).
fof(f108, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f606, plain, (spl15_45 | spl15_41 | spl15_37 | spl15_33), inference(avatar_split_clause, [], [f109, f448, f465, f482, f499])).
fof(f109, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f604, plain, (spl15_46 | spl15_42 | spl15_38 | spl15_34), inference(avatar_split_clause, [], [f111, f452, f469, f486, f503])).
fof(f111, plain, ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f602, plain, (spl15_47 | spl15_43 | spl15_39 | spl15_35), inference(avatar_split_clause, [], [f113, f456, f473, f490, f507])).
fof(f113, plain, ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f601, plain, (spl15_59 | spl15_43 | spl15_27 | spl15_11), inference(avatar_split_clause, [], [f114, f354, f422, f490, f558])).
fof(f114, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f600, plain, (spl15_48 | spl15_44 | spl15_40 | spl15_36), inference(avatar_split_clause, [], [f115, f460, f477, f494, f511])).
fof(f115, plain, ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f599, plain, (spl15_60 | spl15_44 | spl15_28 | spl15_12), inference(avatar_split_clause, [], [f116, f358, f426, f494, f562])).
fof(f116, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f598, plain, (spl15_29 | spl15_25 | spl15_21 | spl15_17), inference(avatar_split_clause, [], [f117, f380, f397, f414, f431])).
fof(f117, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f596, plain, (spl15_30 | spl15_26 | spl15_22 | spl15_18), inference(avatar_split_clause, [], [f119, f384, f401, f418, f435])).
fof(f119, plain, ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f595, plain, (spl15_54 | spl15_38 | spl15_22 | spl15_6), inference(avatar_split_clause, [], [f120, f333, f401, f469, f537])).
fof(f120, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f594, plain, (spl15_31 | spl15_27 | spl15_23 | spl15_19), inference(avatar_split_clause, [], [f121, f388, f405, f422, f439])).
fof(f121, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f592, plain, (spl15_32 | spl15_28 | spl15_24 | spl15_20), inference(avatar_split_clause, [], [f123, f392, f409, f426, f443])).
fof(f123, plain, ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f591, plain, (spl15_56 | spl15_40 | spl15_24 | spl15_8), inference(avatar_split_clause, [], [f124, f341, f409, f477, f545])).
fof(f124, plain, ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f590, plain, (spl15_13 | spl15_9 | spl15_5 | spl15_1), inference(avatar_split_clause, [], [f125, f312, f329, f346, f363])).
fof(f125, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f588, plain, (spl15_14 | spl15_10 | spl15_6 | spl15_2), inference(avatar_split_clause, [], [f127, f316, f333, f350, f367])).
fof(f127, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f587, plain, (spl15_50 | spl15_34 | spl15_18 | spl15_2), inference(avatar_split_clause, [], [f128, f316, f384, f452, f520])).
fof(f128, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f586, plain, (spl15_15 | spl15_11 | spl15_7 | spl15_3), inference(avatar_split_clause, [], [f129, f320, f337, f354, f371])).
fof(f129, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f585, plain, (spl15_51 | spl15_35 | spl15_19 | spl15_3), inference(avatar_split_clause, [], [f130, f320, f388, f456, f524])).
fof(f130, plain, ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f584, plain, (spl15_16 | spl15_12 | spl15_8 | spl15_4), inference(avatar_split_clause, [], [f131, f324, f341, f358, f375])).
fof(f131, plain, ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f583, plain, (spl15_52 | spl15_36 | spl15_20 | spl15_4), inference(avatar_split_clause, [], [f132, f324, f392, f460, f528])).
fof(f132, plain, ((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f582, plain, (spl15_61 | spl15_62 | spl15_63 | spl15_64), inference(avatar_split_clause, [], [f85, f579, f575, f571, f567])).
fof(f85, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG133+1.p', ax1)).
fof(f565, plain, (spl15_57 | spl15_58 | spl15_59 | spl15_60), inference(avatar_split_clause, [], [f86, f562, f558, f554, f550])).
fof(f86, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f548, plain, (spl15_53 | spl15_54 | spl15_55 | spl15_56), inference(avatar_split_clause, [], [f87, f545, f541, f537, f533])).
fof(f87, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f531, plain, (spl15_49 | spl15_50 | spl15_51 | spl15_52), inference(avatar_split_clause, [], [f88, f528, f524, f520, f516])).
fof(f88, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f514, plain, (spl15_45 | spl15_46 | spl15_47 | spl15_48), inference(avatar_split_clause, [], [f89, f511, f507, f503, f499])).
fof(f89, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f497, plain, (spl15_41 | spl15_42 | spl15_43 | spl15_44), inference(avatar_split_clause, [], [f90, f494, f490, f486, f482])).
fof(f90, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f480, plain, (spl15_37 | spl15_38 | spl15_39 | spl15_40), inference(avatar_split_clause, [], [f91, f477, f473, f469, f465])).
fof(f91, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f463, plain, (spl15_33 | spl15_34 | spl15_35 | spl15_36), inference(avatar_split_clause, [], [f92, f460, f456, f452, f448])).
fof(f92, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f446, plain, (spl15_29 | spl15_30 | spl15_31 | spl15_32), inference(avatar_split_clause, [], [f93, f443, f439, f435, f431])).
fof(f93, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f429, plain, (spl15_25 | spl15_26 | spl15_27 | spl15_28), inference(avatar_split_clause, [], [f94, f426, f422, f418, f414])).
fof(f94, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f412, plain, (spl15_21 | spl15_22 | spl15_23 | spl15_24), inference(avatar_split_clause, [], [f95, f409, f405, f401, f397])).
fof(f95, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f395, plain, (spl15_17 | spl15_18 | spl15_19 | spl15_20), inference(avatar_split_clause, [], [f96, f392, f388, f384, f380])).
fof(f96, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f378, plain, (spl15_13 | spl15_14 | spl15_15 | spl15_16), inference(avatar_split_clause, [], [f97, f375, f371, f367, f363])).
fof(f97, plain, ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f361, plain, (spl15_9 | spl15_10 | spl15_11 | spl15_12), inference(avatar_split_clause, [], [f98, f358, f354, f350, f346])).
fof(f98, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f344, plain, (spl15_5 | spl15_6 | spl15_7 | spl15_8), inference(avatar_split_clause, [], [f99, f341, f337, f333, f329])).
fof(f99, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f327, plain, (spl15_1 | spl15_2 | spl15_3 | spl15_4), inference(avatar_split_clause, [], [f100, f324, f320, f316, f312])).
fof(f100, plain, ((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).