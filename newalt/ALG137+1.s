fof(f3369, plain, $false, inference(avatar_sat_refutation, [], [f236, f253, f287, f304, f321, f338, f355, f372, f389, f406, f423, f440, f457, f474, f478, f479, f480, f481, f482, f486, f487, f489, f490, f491, f493, f497, f498, f499, f500, f501, f502, f503, f505, f506, f515, f520, f525, f530, f539, f544, f549, f554, f563, f568, f573, f578, f583, f588, f593, f598, f607, f612, f659, f664, f679, f699, f700, f712, f718, f780, f788, f800, f805, f822, f827, f828, f840, f854, f880, f915, f924, f943, f962, f979, f1002, f1018, f1030, f1067, f1082, f1088, f1125, f1126, f1152, f1160, f1175, f1178, f1203, f1206, f1215, f1216, f1287, f1304, f1307, f1321, f1339, f1355, f1366, f1371, f1397, f1407, f1417, f1421, f1440, f1448, f1452, f1462, f1463, f1481, f1486, f1514, f1525, f1536, f1538, f1573, f1614, f1616, f1623, f1624, f1631, f1656, f1686, f1705, f1728, f1736, f1748, f1762, f1772, f1798, f1801, f1817, f1834, f1862, f1881, f1895, f1919, f1924, f1936, f1981, f1984, f1993, f2001, f2018, f2043, f2057, f2065, f2075, f2081, f2093, f2094, f2095, f2109, f2129, f2149, f2158, f2161, f2184, f2187, f2211, f2222, f2223, f2231, f2244, f2256, f2262, f2263, f2268, f2277, f2319, f2338, f2345, f2382, f2411, f2460, f2463, f2469, f2486, f2487, f2488, f2496, f2506, f2512, f2524, f2539, f2541, f2542, f2549, f2550, f2561, f2606, f2637, f2638, f2669, f2677, f2689, f2716, f2718, f2735, f2744, f2773, f2774, f2785, f2787, f2811, f2826, f2840, f2844, f2856, f2858, f2860, f2868, f2877, f2892, f2914, f2916, f2934, f2939, f2963, f2966, f2976, f2981, f2991, f3007, f3033, f3035, f3059, f3064, f3076, f3080, f3096, f3111, f3145, f3146, f3148, f3156, f3165, f3172, f3173, f3194, f3195, f3200, f3207, f3236, f3249, f3252, f3257, f3267, f3277, f3290, f3295, f3308, f3325, f3339, f3342, f3348, f3351, f3368])).
fof(f3368, plain, (~ spl3_1 | ~ spl3_51 | spl3_104), inference(avatar_contradiction_clause, [], [f3367])).
fof(f3367, plain, ($false | (~ spl3_1 | ~ spl3_51 | spl3_104)), inference(subsumption_resolution, [], [f3363, f418])).
fof(f418, plain, ((e2 = op(e0, e3)) | ~ spl3_51), inference(avatar_component_clause, [], [f416])).
fof(f416, plain, (spl3_51 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_51])])).
fof(f3363, plain, (~ (e2 = op(e0, e3)) | (~ spl3_1 | spl3_104)), inference(backward_demodulation, [], [f698, f206])).
fof(f206, plain, ((e0 = op(e3, e3)) | ~ spl3_1), inference(avatar_component_clause, [], [f204])).
fof(f204, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f698, plain, (~ (e2 = op(op(e3, e3), e3)) | spl3_104), inference(avatar_component_clause, [], [f696])).
fof(f696, plain, (spl3_104 <=> (e2 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_104])])).
fof(f3351, plain, (~ spl3_14 | ~ spl3_41 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f3350])).
fof(f3350, plain, ($false | (~ spl3_14 | ~ spl3_41 | ~ spl3_76)), inference(subsumption_resolution, [], [f3349, f159])).
fof(f159, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (e2 = e3) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax4)).
fof(f3349, plain, ((e0 = e3) | (~ spl3_14 | ~ spl3_41 | ~ spl3_76)), inference(forward_demodulation, [], [f3347, f376])).
fof(f376, plain, ((e0 = op(e1, e1)) | ~ spl3_41), inference(avatar_component_clause, [], [f374])).
fof(f374, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f3347, plain, ((e3 = op(e1, e1)) | (~ spl3_14 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f261])).
fof(f261, plain, ((e1 = op(e3, e0)) | ~ spl3_14), inference(avatar_component_clause, [], [f259])).
fof(f259, plain, (spl3_14 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_14])])).
fof(f562, plain, ((e3 = op(op(e3, e0), op(e3, e0))) | ~ spl3_76), inference(avatar_component_clause, [], [f560])).
fof(f560, plain, (spl3_76 <=> (e3 = op(op(e3, e0), op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_76])])).
fof(f3348, plain, (~ spl3_10 | ~ spl3_14), inference(avatar_split_clause, [], [f3343, f259, f242])).
fof(f242, plain, (spl3_10 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_10])])).
fof(f3343, plain, (~ (e1 = op(e3, e1)) | ~ spl3_14), inference(backward_demodulation, [], [f151, f261])).
fof(f151, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax3)).
fof(f3342, plain, (~ spl3_4 | ~ spl3_20), inference(avatar_split_clause, [], [f3341, f284, f216])).
fof(f216, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f284, plain, (spl3_20 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_20])])).
fof(f3341, plain, (~ (e3 = op(e3, e3)) | ~ spl3_20), inference(backward_demodulation, [], [f132, f286])).
fof(f286, plain, ((e3 = op(e2, e3)) | ~ spl3_20), inference(avatar_component_clause, [], [f284])).
fof(f132, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3339, plain, (~ spl3_21 | ~ spl3_22), inference(avatar_contradiction_clause, [], [f3338])).
fof(f3338, plain, ($false | (~ spl3_21 | ~ spl3_22)), inference(subsumption_resolution, [], [f3337, f157])).
fof(f157, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f4])).
fof(f3337, plain, ((e0 = e1) | (~ spl3_21 | ~ spl3_22)), inference(backward_demodulation, [], [f295, f291])).
fof(f291, plain, ((e0 = op(e2, e2)) | ~ spl3_21), inference(avatar_component_clause, [], [f289])).
fof(f289, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f295, plain, ((e1 = op(e2, e2)) | ~ spl3_22), inference(avatar_component_clause, [], [f293])).
fof(f293, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f3325, plain, (~ spl3_35 | ~ spl3_51), inference(avatar_split_clause, [], [f3321, f416, f348])).
fof(f348, plain, (spl3_35 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_35])])).
fof(f3321, plain, (~ (e2 = op(e1, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f127, f418])).
fof(f127, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3308, plain, (~ spl3_53 | ~ spl3_61), inference(avatar_split_clause, [], [f3298, f459, f425])).
fof(f425, plain, (spl3_53 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_53])])).
fof(f459, plain, (spl3_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f3298, plain, (~ (e0 = op(e0, e2)) | ~ spl3_61), inference(backward_demodulation, [], [f134, f461])).
fof(f461, plain, ((e0 = op(e0, e0)) | ~ spl3_61), inference(avatar_component_clause, [], [f459])).
fof(f134, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3295, plain, (spl3_3 | ~ spl3_32 | ~ spl3_77), inference(avatar_contradiction_clause, [], [f3294])).
fof(f3294, plain, ($false | (spl3_3 | ~ spl3_32 | ~ spl3_77)), inference(subsumption_resolution, [], [f3293, f213])).
fof(f213, plain, (~ (e2 = op(e3, e3)) | spl3_3), inference(avatar_component_clause, [], [f212])).
fof(f212, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f3293, plain, ((e2 = op(e3, e3)) | (~ spl3_32 | ~ spl3_77)), inference(forward_demodulation, [], [f567, f337])).
fof(f337, plain, ((e3 = op(e2, e0)) | ~ spl3_32), inference(avatar_component_clause, [], [f335])).
fof(f335, plain, (spl3_32 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_32])])).
fof(f567, plain, ((e2 = op(op(e2, e0), op(e2, e0))) | ~ spl3_77), inference(avatar_component_clause, [], [f565])).
fof(f565, plain, (spl3_77 <=> (e2 = op(op(e2, e0), op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_77])])).
fof(f3290, plain, (~ spl3_17 | ~ spl3_22 | ~ spl3_40 | spl3_97), inference(avatar_split_clause, [], [f3289, f661, f369, f293, f272])).
fof(f272, plain, (spl3_17 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_17])])).
fof(f369, plain, (spl3_40 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_40])])).
fof(f661, plain, (spl3_97 <=> (e0 = op(e2, op(op(e2, e2), e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_97])])).
fof(f3289, plain, (~ (e0 = op(e2, e3)) | (~ spl3_22 | ~ spl3_40 | spl3_97)), inference(forward_demodulation, [], [f3288, f371])).
fof(f371, plain, ((e3 = op(e1, e2)) | ~ spl3_40), inference(avatar_component_clause, [], [f369])).
fof(f3288, plain, (~ (e0 = op(e2, op(e1, e2))) | (~ spl3_22 | spl3_97)), inference(forward_demodulation, [], [f663, f295])).
fof(f663, plain, (~ (e0 = op(e2, op(op(e2, e2), e2))) | spl3_97), inference(avatar_component_clause, [], [f661])).
fof(f3277, plain, (~ spl3_56 | ~ spl3_60), inference(avatar_split_clause, [], [f3152, f454, f437])).
fof(f437, plain, (spl3_56 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_56])])).
fof(f454, plain, (spl3_60 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_60])])).
fof(f3152, plain, (~ (e3 = op(e0, e2)) | ~ spl3_60), inference(backward_demodulation, [], [f136, f456])).
fof(f456, plain, ((e3 = op(e0, e1)) | ~ spl3_60), inference(avatar_component_clause, [], [f454])).
fof(f136, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f3])).
fof(f3267, plain, (~ spl3_12 | ~ spl3_60), inference(avatar_split_clause, [], [f3151, f454, f250])).
fof(f250, plain, (spl3_12 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_12])])).
fof(f3151, plain, (~ (e3 = op(e3, e1)) | ~ spl3_60), inference(backward_demodulation, [], [f117, f456])).
fof(f117, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3257, plain, (~ spl3_2 | ~ spl3_10), inference(avatar_split_clause, [], [f3254, f242, f208])).
fof(f208, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f3254, plain, (~ (e1 = op(e3, e3)) | ~ spl3_10), inference(backward_demodulation, [], [f155, f244])).
fof(f244, plain, ((e1 = op(e3, e1)) | ~ spl3_10), inference(avatar_component_clause, [], [f242])).
fof(f155, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3252, plain, (~ spl3_13 | ~ spl3_63 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f3251])).
fof(f3251, plain, ($false | (~ spl3_13 | ~ spl3_63 | ~ spl3_76)), inference(subsumption_resolution, [], [f3250, f162])).
fof(f162, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f4])).
fof(f3250, plain, ((e2 = e3) | (~ spl3_13 | ~ spl3_63 | ~ spl3_76)), inference(forward_demodulation, [], [f3248, f469])).
fof(f469, plain, ((op(e0, e0) = e2) | ~ spl3_63), inference(avatar_component_clause, [], [f467])).
fof(f467, plain, (spl3_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f3248, plain, ((op(e0, e0) = e3) | (~ spl3_13 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f257])).
fof(f257, plain, ((e0 = op(e3, e0)) | ~ spl3_13), inference(avatar_component_clause, [], [f255])).
fof(f255, plain, (spl3_13 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_13])])).
fof(f3249, plain, (~ spl3_9 | ~ spl3_13), inference(avatar_split_clause, [], [f3242, f255, f238])).
fof(f238, plain, (spl3_9 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_9])])).
fof(f3242, plain, (~ (e0 = op(e3, e1)) | ~ spl3_13), inference(backward_demodulation, [], [f151, f257])).
fof(f3236, plain, (~ spl3_22 | ~ spl3_40 | spl3_107), inference(avatar_contradiction_clause, [], [f3235])).
fof(f3235, plain, ($false | (~ spl3_22 | ~ spl3_40 | spl3_107)), inference(subsumption_resolution, [], [f3231, f371])).
fof(f3231, plain, (~ (e3 = op(e1, e2)) | (~ spl3_22 | spl3_107)), inference(backward_demodulation, [], [f716, f295])).
fof(f716, plain, (~ (e3 = op(op(e2, e2), e2)) | spl3_107), inference(avatar_component_clause, [], [f714])).
fof(f714, plain, (spl3_107 <=> (e3 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_107])])).
fof(f3207, plain, (spl3_22 | ~ spl3_35 | ~ spl3_82), inference(avatar_split_clause, [], [f3205, f590, f348, f293])).
fof(f590, plain, (spl3_82 <=> (e1 = op(op(e1, e3), op(e1, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_82])])).
fof(f3205, plain, ((e1 = op(e2, e2)) | (~ spl3_35 | ~ spl3_82)), inference(backward_demodulation, [], [f592, f350])).
fof(f350, plain, ((e2 = op(e1, e3)) | ~ spl3_35), inference(avatar_component_clause, [], [f348])).
fof(f592, plain, ((e1 = op(op(e1, e3), op(e1, e3))) | ~ spl3_82), inference(avatar_component_clause, [], [f590])).
fof(f3200, plain, (~ spl3_8 | ~ spl3_40), inference(avatar_split_clause, [], [f3197, f369, f233])).
fof(f233, plain, (spl3_8 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_8])])).
fof(f3197, plain, (~ (e3 = op(e3, e2)) | ~ spl3_40), inference(backward_demodulation, [], [f125, f371])).
fof(f125, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f3195, plain, (~ spl3_33 | ~ spl3_41), inference(avatar_split_clause, [], [f3187, f374, f340])).
fof(f340, plain, (spl3_33 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_33])])).
fof(f3187, plain, (~ (e0 = op(e1, e3)) | ~ spl3_41), inference(backward_demodulation, [], [f143, f376])).
fof(f143, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f3194, plain, (~ spl3_9 | ~ spl3_41), inference(avatar_split_clause, [], [f3185, f374, f238])).
fof(f3185, plain, (~ (e0 = op(e3, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f119, f376])).
fof(f119, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f3173, plain, (spl3_41 | ~ spl3_50 | ~ spl3_83), inference(avatar_split_clause, [], [f3171, f595, f412, f374])).
fof(f412, plain, (spl3_50 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_50])])).
fof(f595, plain, (spl3_83 <=> (e0 = op(op(e0, e3), op(e0, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_83])])).
fof(f3171, plain, ((e0 = op(e1, e1)) | (~ spl3_50 | ~ spl3_83)), inference(backward_demodulation, [], [f597, f414])).
fof(f414, plain, ((e1 = op(e0, e3)) | ~ spl3_50), inference(avatar_component_clause, [], [f412])).
fof(f597, plain, ((e0 = op(op(e0, e3), op(e0, e3))) | ~ spl3_83), inference(avatar_component_clause, [], [f595])).
fof(f3172, plain, (~ spl3_2 | ~ spl3_50), inference(avatar_split_clause, [], [f3168, f412, f208])).
fof(f3168, plain, (~ (e1 = op(e3, e3)) | ~ spl3_50), inference(backward_demodulation, [], [f129, f414])).
fof(f129, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f3165, plain, (~ spl3_21 | ~ spl3_53), inference(avatar_split_clause, [], [f3158, f425, f289])).
fof(f3158, plain, (~ (e0 = op(e2, e2)) | ~ spl3_53), inference(backward_demodulation, [], [f122, f427])).
fof(f427, plain, ((e0 = op(e0, e2)) | ~ spl3_53), inference(avatar_component_clause, [], [f425])).
fof(f122, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f3156, plain, (~ spl3_52 | ~ spl3_60), inference(avatar_split_clause, [], [f3153, f454, f420])).
fof(f420, plain, (spl3_52 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_52])])).
fof(f3153, plain, (~ (e3 = op(e0, e3)) | ~ spl3_60), inference(backward_demodulation, [], [f137, f456])).
fof(f137, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3148, plain, (~ spl3_30 | ~ spl3_63 | spl3_93), inference(avatar_split_clause, [], [f3142, f642, f467, f327])).
fof(f327, plain, (spl3_30 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_30])])).
fof(f642, plain, (spl3_93 <=> (e1 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_93])])).
fof(f3142, plain, (~ (e1 = op(e2, e0)) | (~ spl3_63 | spl3_93)), inference(backward_demodulation, [], [f644, f469])).
fof(f644, plain, (~ (e1 = op(op(e0, e0), e0)) | spl3_93), inference(avatar_component_clause, [], [f642])).
fof(f3146, plain, (~ spl3_55 | ~ spl3_63), inference(avatar_split_clause, [], [f3138, f467, f433])).
fof(f433, plain, (spl3_55 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_55])])).
fof(f3138, plain, (~ (e2 = op(e0, e2)) | ~ spl3_63), inference(backward_demodulation, [], [f134, f469])).
fof(f3145, plain, (~ spl3_15 | ~ spl3_63), inference(avatar_split_clause, [], [f3136, f467, f263])).
fof(f263, plain, (spl3_15 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_15])])).
fof(f3136, plain, (~ (e2 = op(e3, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f111, f469])).
fof(f111, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f3])).
fof(f3111, plain, (~ spl3_27 | ~ spl3_43), inference(avatar_split_clause, [], [f3104, f382, f314])).
fof(f314, plain, (spl3_27 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_27])])).
fof(f382, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f3104, plain, (~ (e2 = op(e2, e1)) | ~ spl3_43), inference(backward_demodulation, [], [f118, f384])).
fof(f384, plain, ((e2 = op(e1, e1)) | ~ spl3_43), inference(avatar_component_clause, [], [f382])).
fof(f118, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f3096, plain, (~ spl3_51 | ~ spl3_55), inference(avatar_split_clause, [], [f3091, f433, f416])).
fof(f3091, plain, (~ (e2 = op(e0, e3)) | ~ spl3_55), inference(backward_demodulation, [], [f138, f435])).
fof(f435, plain, ((e2 = op(e0, e2)) | ~ spl3_55), inference(avatar_component_clause, [], [f433])).
fof(f138, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f3080, plain, (~ spl3_15 | ~ spl3_21 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f3079])).
fof(f3079, plain, ($false | (~ spl3_15 | ~ spl3_21 | ~ spl3_76)), inference(subsumption_resolution, [], [f3078, f159])).
fof(f3078, plain, ((e0 = e3) | (~ spl3_15 | ~ spl3_21 | ~ spl3_76)), inference(forward_demodulation, [], [f3077, f291])).
fof(f3077, plain, ((e3 = op(e2, e2)) | (~ spl3_15 | ~ spl3_76)), inference(forward_demodulation, [], [f562, f265])).
fof(f265, plain, ((e2 = op(e3, e0)) | ~ spl3_15), inference(avatar_component_clause, [], [f263])).
fof(f3076, plain, (spl3_43 | ~ spl3_30 | ~ spl3_77), inference(avatar_split_clause, [], [f3075, f565, f327, f382])).
fof(f3075, plain, ((e2 = op(e1, e1)) | (~ spl3_30 | ~ spl3_77)), inference(forward_demodulation, [], [f567, f329])).
fof(f329, plain, ((e1 = op(e2, e0)) | ~ spl3_30), inference(avatar_component_clause, [], [f327])).
fof(f3064, plain, (~ spl3_54 | ~ spl3_21 | spl3_96), inference(avatar_split_clause, [], [f3063, f656, f289, f429])).
fof(f429, plain, (spl3_54 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_54])])).
fof(f656, plain, (spl3_96 <=> (e1 = op(op(e2, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_96])])).
fof(f3063, plain, (~ (e1 = op(e0, e2)) | (~ spl3_21 | spl3_96)), inference(forward_demodulation, [], [f658, f291])).
fof(f658, plain, (~ (e1 = op(op(e2, e2), e2)) | spl3_96), inference(avatar_component_clause, [], [f656])).
fof(f3059, plain, (~ spl3_33 | spl3_103 | ~ spl3_106), inference(avatar_contradiction_clause, [], [f3058])).
fof(f3058, plain, ($false | (~ spl3_33 | spl3_103 | ~ spl3_106)), inference(subsumption_resolution, [], [f3055, f342])).
fof(f342, plain, ((e0 = op(e1, e3)) | ~ spl3_33), inference(avatar_component_clause, [], [f340])).
fof(f3055, plain, (~ (e0 = op(e1, e3)) | (spl3_103 | ~ spl3_106)), inference(backward_demodulation, [], [f693, f709])).
fof(f709, plain, ((e3 = op(op(e1, e1), e1)) | ~ spl3_106), inference(avatar_component_clause, [], [f708])).
fof(f708, plain, (spl3_106 <=> (e3 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_106])])).
fof(f693, plain, (~ (e0 = op(e1, op(op(e1, e1), e1))) | spl3_103), inference(avatar_component_clause, [], [f691])).
fof(f691, plain, (spl3_103 <=> (e0 = op(e1, op(op(e1, e1), e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_103])])).
fof(f3035, plain, (~ spl3_44 | ~ spl3_48), inference(avatar_split_clause, [], [f3034, f403, f386])).
fof(f386, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f403, plain, (spl3_48 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_48])])).
fof(f3034, plain, (~ (e3 = op(e1, e1)) | ~ spl3_48), inference(forward_demodulation, [], [f139, f405])).
fof(f405, plain, ((e3 = op(e1, e0)) | ~ spl3_48), inference(avatar_component_clause, [], [f403])).
fof(f139, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f3033, plain, (spl3_44 | ~ spl3_2 | ~ spl3_80), inference(avatar_split_clause, [], [f3015, f580, f208, f386])).
fof(f580, plain, (spl3_80 <=> (e3 = op(op(e3, e3), op(e3, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_80])])).
fof(f3015, plain, ((e3 = op(e1, e1)) | (~ spl3_2 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f210])).
fof(f210, plain, ((e1 = op(e3, e3)) | ~ spl3_2), inference(avatar_component_clause, [], [f208])).
fof(f582, plain, ((e3 = op(op(e3, e3), op(e3, e3))) | ~ spl3_80), inference(avatar_component_clause, [], [f580])).
fof(f3007, plain, (spl3_3 | ~ spl3_20 | ~ spl3_81), inference(avatar_split_clause, [], [f3006, f585, f284, f212])).
fof(f585, plain, (spl3_81 <=> (e2 = op(op(e2, e3), op(e2, e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_81])])).
fof(f3006, plain, ((e2 = op(e3, e3)) | (~ spl3_20 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f286])).
fof(f587, plain, ((e2 = op(op(e2, e3), op(e2, e3))) | ~ spl3_81), inference(avatar_component_clause, [], [f585])).
fof(f2991, plain, (~ spl3_33 | ~ spl3_61 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f2990])).
fof(f2990, plain, ($false | (~ spl3_33 | ~ spl3_61 | ~ spl3_82)), inference(subsumption_resolution, [], [f2989, f157])).
fof(f2989, plain, ((e0 = e1) | (~ spl3_33 | ~ spl3_61 | ~ spl3_82)), inference(forward_demodulation, [], [f2988, f461])).
fof(f2988, plain, ((op(e0, e0) = e1) | (~ spl3_33 | ~ spl3_82)), inference(backward_demodulation, [], [f592, f342])).
fof(f2981, plain, (~ spl3_36 | ~ spl3_48), inference(avatar_split_clause, [], [f2979, f403, f352])).
fof(f352, plain, (spl3_36 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_36])])).
fof(f2979, plain, (~ (e3 = op(e1, e3)) | ~ spl3_48), inference(backward_demodulation, [], [f141, f405])).
fof(f141, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2976, plain, (~ spl3_3 | ~ spl3_51), inference(avatar_split_clause, [], [f2973, f416, f212])).
fof(f2973, plain, (~ (e2 = op(e3, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f129, f418])).
fof(f2966, plain, (~ spl3_61 | ~ spl3_93), inference(avatar_contradiction_clause, [], [f2965])).
fof(f2965, plain, ($false | (~ spl3_61 | ~ spl3_93)), inference(subsumption_resolution, [], [f2964, f157])).
fof(f2964, plain, ((e0 = e1) | (~ spl3_61 | ~ spl3_93)), inference(forward_demodulation, [], [f2961, f461])).
fof(f2961, plain, ((op(e0, e0) = e1) | (~ spl3_61 | ~ spl3_93)), inference(backward_demodulation, [], [f643, f461])).
fof(f643, plain, ((e1 = op(op(e0, e0), e0)) | ~ spl3_93), inference(avatar_component_clause, [], [f642])).
fof(f2963, plain, (~ spl3_49 | ~ spl3_61), inference(avatar_split_clause, [], [f2956, f459, f408])).
fof(f408, plain, (spl3_49 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_49])])).
fof(f2956, plain, (~ (e0 = op(e0, e3)) | ~ spl3_61), inference(backward_demodulation, [], [f135, f461])).
fof(f135, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f3])).
fof(f2939, plain, (~ spl3_28 | ~ spl3_21 | ~ spl3_54 | spl3_88), inference(avatar_split_clause, [], [f2917, f618, f429, f289, f318])).
fof(f318, plain, (spl3_28 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_28])])).
fof(f618, plain, (spl3_88 <=> (e3 = op(e2, op(op(e2, e2), e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_88])])).
fof(f2917, plain, (~ (e3 = op(e2, e1)) | (~ spl3_21 | ~ spl3_54 | spl3_88)), inference(forward_demodulation, [], [f2910, f431])).
fof(f431, plain, ((e1 = op(e0, e2)) | ~ spl3_54), inference(avatar_component_clause, [], [f429])).
fof(f2910, plain, (~ (e3 = op(e2, op(e0, e2))) | (~ spl3_21 | spl3_88)), inference(backward_demodulation, [], [f620, f291])).
fof(f620, plain, (~ (e3 = op(e2, op(op(e2, e2), e2))) | spl3_88), inference(avatar_component_clause, [], [f618])).
fof(f2934, plain, (~ spl3_2 | ~ spl3_14), inference(avatar_split_clause, [], [f2930, f259, f208])).
fof(f2930, plain, (~ (e1 = op(e3, e3)) | ~ spl3_14), inference(backward_demodulation, [], [f153, f261])).
fof(f153, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2916, plain, (~ spl3_21 | spl3_63 | ~ spl3_67), inference(avatar_contradiction_clause, [], [f2915])).
fof(f2915, plain, ($false | (~ spl3_21 | spl3_63 | ~ spl3_67)), inference(subsumption_resolution, [], [f2909, f468])).
fof(f468, plain, (~ (op(e0, e0) = e2) | spl3_63), inference(avatar_component_clause, [], [f467])).
fof(f2909, plain, ((op(e0, e0) = e2) | (~ spl3_21 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f291])).
fof(f519, plain, ((e2 = op(op(e2, e2), op(e2, e2))) | ~ spl3_67), inference(avatar_component_clause, [], [f517])).
fof(f517, plain, (spl3_67 <=> (e2 = op(op(e2, e2), op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_67])])).
fof(f2914, plain, (~ spl3_17 | ~ spl3_21), inference(avatar_split_clause, [], [f2905, f289, f272])).
fof(f2905, plain, (~ (e0 = op(e2, e3)) | ~ spl3_21), inference(backward_demodulation, [], [f150, f291])).
fof(f150, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2892, plain, (~ spl3_35 | ~ spl3_39), inference(avatar_split_clause, [], [f2890, f365, f348])).
fof(f365, plain, (spl3_39 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_39])])).
fof(f2890, plain, (~ (e2 = op(e1, e3)) | ~ spl3_39), inference(backward_demodulation, [], [f144, f367])).
fof(f367, plain, ((e2 = op(e1, e2)) | ~ spl3_39), inference(avatar_component_clause, [], [f365])).
fof(f144, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f3])).
fof(f2877, plain, (~ spl3_17 | ~ spl3_49), inference(avatar_split_clause, [], [f2871, f408, f272])).
fof(f2871, plain, (~ (e0 = op(e2, e3)) | ~ spl3_49), inference(backward_demodulation, [], [f128, f410])).
fof(f410, plain, ((e0 = op(e0, e3)) | ~ spl3_49), inference(avatar_component_clause, [], [f408])).
fof(f128, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2868, plain, (~ spl3_52 | ~ spl3_64), inference(avatar_split_clause, [], [f2864, f471, f420])).
fof(f471, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f2864, plain, (~ (e3 = op(e0, e3)) | ~ spl3_64), inference(backward_demodulation, [], [f135, f473])).
fof(f473, plain, ((op(e0, e0) = e3) | ~ spl3_64), inference(avatar_component_clause, [], [f471])).
fof(f2860, plain, (spl3_64 | ~ spl3_9 | ~ spl3_71), inference(avatar_split_clause, [], [f2859, f536, f238, f471])).
fof(f536, plain, (spl3_71 <=> (e3 = op(op(e3, e1), op(e3, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_71])])).
fof(f2859, plain, ((op(e0, e0) = e3) | (~ spl3_9 | ~ spl3_71)), inference(forward_demodulation, [], [f538, f240])).
fof(f240, plain, ((e0 = op(e3, e1)) | ~ spl3_9), inference(avatar_component_clause, [], [f238])).
fof(f538, plain, ((e3 = op(op(e3, e1), op(e3, e1))) | ~ spl3_71), inference(avatar_component_clause, [], [f536])).
fof(f2858, plain, (spl3_3 | ~ spl3_28 | ~ spl3_72), inference(avatar_split_clause, [], [f2857, f541, f318, f212])).
fof(f541, plain, (spl3_72 <=> (e2 = op(op(e2, e1), op(e2, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_72])])).
fof(f2857, plain, ((e2 = op(e3, e3)) | (~ spl3_28 | ~ spl3_72)), inference(forward_demodulation, [], [f543, f320])).
fof(f320, plain, ((e3 = op(e2, e1)) | ~ spl3_28), inference(avatar_component_clause, [], [f318])).
fof(f543, plain, ((e2 = op(op(e2, e1), op(e2, e1))) | ~ spl3_72), inference(avatar_component_clause, [], [f541])).
fof(f2856, plain, (spl3_21 | ~ spl3_59 | ~ spl3_74), inference(avatar_split_clause, [], [f2855, f551, f450, f289])).
fof(f450, plain, (spl3_59 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_59])])).
fof(f551, plain, (spl3_74 <=> (e0 = op(op(e0, e1), op(e0, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_74])])).
fof(f2855, plain, ((e0 = op(e2, e2)) | (~ spl3_59 | ~ spl3_74)), inference(forward_demodulation, [], [f553, f452])).
fof(f452, plain, ((e2 = op(e0, e1)) | ~ spl3_59), inference(avatar_component_clause, [], [f450])).
fof(f553, plain, ((e0 = op(op(e0, e1), op(e0, e1))) | ~ spl3_74), inference(avatar_component_clause, [], [f551])).
fof(f2844, plain, (~ spl3_98 | ~ spl3_104), inference(avatar_contradiction_clause, [], [f2843])).
fof(f2843, plain, ($false | (~ spl3_98 | ~ spl3_104)), inference(subsumption_resolution, [], [f2842, f160])).
fof(f160, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f4])).
fof(f2842, plain, ((e1 = e2) | (~ spl3_98 | ~ spl3_104)), inference(backward_demodulation, [], [f697, f667])).
fof(f667, plain, ((e1 = op(op(e3, e3), e3)) | ~ spl3_98), inference(avatar_component_clause, [], [f666])).
fof(f666, plain, (spl3_98 <=> (e1 = op(op(e3, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_98])])).
fof(f697, plain, ((e2 = op(op(e3, e3), e3)) | ~ spl3_104), inference(avatar_component_clause, [], [f696])).
fof(f2840, plain, (~ spl3_63 | ~ spl3_59), inference(avatar_split_clause, [], [f2839, f450, f467])).
fof(f2839, plain, (~ (op(e0, e0) = e2) | ~ spl3_59), inference(forward_demodulation, [], [f133, f452])).
fof(f133, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f3])).
fof(f2826, plain, (~ spl3_1 | ~ spl3_9), inference(avatar_split_clause, [], [f2821, f238, f204])).
fof(f2821, plain, (~ (e0 = op(e3, e3)) | ~ spl3_9), inference(backward_demodulation, [], [f155, f240])).
fof(f2811, plain, (~ spl3_1 | ~ spl3_17), inference(avatar_split_clause, [], [f2806, f272, f204])).
fof(f2806, plain, (~ (e0 = op(e3, e3)) | ~ spl3_17), inference(backward_demodulation, [], [f132, f274])).
fof(f274, plain, ((e0 = op(e2, e3)) | ~ spl3_17), inference(avatar_component_clause, [], [f272])).
fof(f2787, plain, (spl3_2 | ~ spl3_48 | ~ spl3_78), inference(avatar_split_clause, [], [f2784, f570, f403, f208])).
fof(f570, plain, (spl3_78 <=> (e1 = op(op(e1, e0), op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_78])])).
fof(f2784, plain, ((e1 = op(e3, e3)) | (~ spl3_48 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f405])).
fof(f572, plain, ((e1 = op(op(e1, e0), op(e1, e0))) | ~ spl3_78), inference(avatar_component_clause, [], [f570])).
fof(f2785, plain, (~ spl3_32 | ~ spl3_48), inference(avatar_split_clause, [], [f2781, f403, f335])).
fof(f2781, plain, (~ (e3 = op(e2, e0)) | ~ spl3_48), inference(backward_demodulation, [], [f112, f405])).
fof(f112, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2774, plain, (~ spl3_51 | ~ spl3_59), inference(avatar_split_clause, [], [f2770, f450, f416])).
fof(f2770, plain, (~ (e2 = op(e0, e3)) | ~ spl3_59), inference(backward_demodulation, [], [f137, f452])).
fof(f2773, plain, (~ spl3_11 | ~ spl3_59), inference(avatar_split_clause, [], [f2769, f450, f246])).
fof(f246, plain, (spl3_11 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_11])])).
fof(f2769, plain, (~ (e2 = op(e3, e1)) | ~ spl3_59), inference(backward_demodulation, [], [f117, f452])).
fof(f2744, plain, (~ spl3_19 | ~ spl3_23), inference(avatar_split_clause, [], [f2743, f297, f280])).
fof(f280, plain, (spl3_19 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_19])])).
fof(f297, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f2743, plain, (~ (e2 = op(e2, e3)) | ~ spl3_23), inference(forward_demodulation, [], [f150, f299])).
fof(f299, plain, ((e2 = op(e2, e2)) | ~ spl3_23), inference(avatar_component_clause, [], [f297])).
fof(f2735, plain, (~ spl3_11 | ~ spl3_23 | ~ spl3_71), inference(avatar_contradiction_clause, [], [f2734])).
fof(f2734, plain, ($false | (~ spl3_11 | ~ spl3_23 | ~ spl3_71)), inference(subsumption_resolution, [], [f2733, f162])).
fof(f2733, plain, ((e2 = e3) | (~ spl3_11 | ~ spl3_23 | ~ spl3_71)), inference(forward_demodulation, [], [f2731, f299])).
fof(f2731, plain, ((e3 = op(e2, e2)) | (~ spl3_11 | ~ spl3_71)), inference(backward_demodulation, [], [f538, f248])).
fof(f248, plain, ((e2 = op(e3, e1)) | ~ spl3_11), inference(avatar_component_clause, [], [f246])).
fof(f2718, plain, (~ spl3_23 | spl3_67), inference(avatar_contradiction_clause, [], [f2717])).
fof(f2717, plain, ($false | (~ spl3_23 | spl3_67)), inference(subsumption_resolution, [], [f2711, f299])).
fof(f2711, plain, (~ (e2 = op(e2, e2)) | (~ spl3_23 | spl3_67)), inference(backward_demodulation, [], [f518, f299])).
fof(f518, plain, (~ (e2 = op(op(e2, e2), op(e2, e2))) | spl3_67), inference(avatar_component_clause, [], [f517])).
fof(f2716, plain, (~ spl3_7 | ~ spl3_23), inference(avatar_split_clause, [], [f2709, f297, f229])).
fof(f229, plain, (spl3_7 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f2709, plain, (~ (e2 = op(e3, e2)) | ~ spl3_23), inference(backward_demodulation, [], [f126, f299])).
fof(f126, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f2689, plain, (~ spl3_38 | ~ spl3_42), inference(avatar_split_clause, [], [f2679, f378, f361])).
fof(f361, plain, (spl3_38 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_38])])).
fof(f378, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f2679, plain, (~ (e1 = op(e1, e2)) | ~ spl3_42), inference(backward_demodulation, [], [f142, f380])).
fof(f380, plain, ((e1 = op(e1, e1)) | ~ spl3_42), inference(avatar_component_clause, [], [f378])).
fof(f142, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2677, plain, (~ spl3_43 | ~ spl3_47), inference(avatar_split_clause, [], [f2672, f399, f382])).
fof(f399, plain, (spl3_47 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_47])])).
fof(f2672, plain, (~ (e2 = op(e1, e1)) | ~ spl3_47), inference(backward_demodulation, [], [f139, f401])).
fof(f401, plain, ((e2 = op(e1, e0)) | ~ spl3_47), inference(avatar_component_clause, [], [f399])).
fof(f2669, plain, (~ spl3_38 | ~ spl3_54), inference(avatar_split_clause, [], [f2663, f429, f361])).
fof(f2663, plain, (~ (e1 = op(e1, e2)) | ~ spl3_54), inference(backward_demodulation, [], [f121, f431])).
fof(f121, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f2638, plain, (spl3_63 | ~ spl3_25 | ~ spl3_72), inference(avatar_split_clause, [], [f2568, f541, f306, f467])).
fof(f306, plain, (spl3_25 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_25])])).
fof(f2568, plain, ((op(e0, e0) = e2) | (~ spl3_25 | ~ spl3_72)), inference(backward_demodulation, [], [f543, f308])).
fof(f308, plain, ((e0 = op(e2, e1)) | ~ spl3_25), inference(avatar_component_clause, [], [f306])).
fof(f2637, plain, (~ spl3_64 | ~ spl3_1 | spl3_80), inference(avatar_split_clause, [], [f2615, f580, f204, f471])).
fof(f2615, plain, (~ (op(e0, e0) = e3) | (~ spl3_1 | spl3_80)), inference(backward_demodulation, [], [f581, f206])).
fof(f581, plain, (~ (e3 = op(op(e3, e3), op(e3, e3))) | spl3_80), inference(avatar_component_clause, [], [f580])).
fof(f2606, plain, (~ spl3_10 | ~ spl3_43 | ~ spl3_71), inference(avatar_contradiction_clause, [], [f2605])).
fof(f2605, plain, ($false | (~ spl3_10 | ~ spl3_43 | ~ spl3_71)), inference(subsumption_resolution, [], [f2604, f162])).
fof(f2604, plain, ((e2 = e3) | (~ spl3_10 | ~ spl3_43 | ~ spl3_71)), inference(forward_demodulation, [], [f2603, f384])).
fof(f2603, plain, ((e3 = op(e1, e1)) | (~ spl3_10 | ~ spl3_71)), inference(backward_demodulation, [], [f538, f244])).
fof(f2561, plain, (~ spl3_19 | ~ spl3_31), inference(avatar_split_clause, [], [f2558, f331, f280])).
fof(f331, plain, (spl3_31 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_31])])).
fof(f2558, plain, (~ (e2 = op(e2, e3)) | ~ spl3_31), inference(backward_demodulation, [], [f147, f333])).
fof(f333, plain, ((e2 = op(e2, e0)) | ~ spl3_31), inference(avatar_component_clause, [], [f331])).
fof(f147, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2550, plain, (~ spl3_34 | ~ spl3_38), inference(avatar_split_clause, [], [f2545, f361, f344])).
fof(f344, plain, (spl3_34 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_34])])).
fof(f2545, plain, (~ (e1 = op(e1, e3)) | ~ spl3_38), inference(backward_demodulation, [], [f144, f363])).
fof(f363, plain, ((e1 = op(e1, e2)) | ~ spl3_38), inference(avatar_component_clause, [], [f361])).
fof(f2549, plain, (~ spl3_22 | ~ spl3_38), inference(avatar_split_clause, [], [f2543, f361, f293])).
fof(f2543, plain, (~ (e1 = op(e2, e2)) | ~ spl3_38), inference(backward_demodulation, [], [f124, f363])).
fof(f124, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f2542, plain, (~ spl3_28 | ~ spl3_43 | spl3_106), inference(avatar_split_clause, [], [f2538, f708, f382, f318])).
fof(f2538, plain, (~ (e3 = op(e2, e1)) | (~ spl3_43 | spl3_106)), inference(backward_demodulation, [], [f710, f384])).
fof(f710, plain, (~ (e3 = op(op(e1, e1), e1)) | spl3_106), inference(avatar_component_clause, [], [f708])).
fof(f2541, plain, (~ spl3_25 | ~ spl3_43 | spl3_84), inference(avatar_split_clause, [], [f2534, f600, f382, f306])).
fof(f600, plain, (spl3_84 <=> (e0 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_84])])).
fof(f2534, plain, (~ (e0 = op(e2, e1)) | (~ spl3_43 | spl3_84)), inference(backward_demodulation, [], [f602, f384])).
fof(f602, plain, (~ (e0 = op(op(e1, e1), e1)) | spl3_84), inference(avatar_component_clause, [], [f600])).
fof(f2539, plain, (~ spl3_11 | ~ spl3_43), inference(avatar_split_clause, [], [f2529, f382, f246])).
fof(f2529, plain, (~ (e2 = op(e3, e1)) | ~ spl3_43), inference(backward_demodulation, [], [f119, f384])).
fof(f2524, plain, (~ spl3_41 | ~ spl3_45), inference(avatar_split_clause, [], [f2515, f391, f374])).
fof(f391, plain, (spl3_45 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_45])])).
fof(f2515, plain, (~ (e0 = op(e1, e1)) | ~ spl3_45), inference(backward_demodulation, [], [f139, f393])).
fof(f393, plain, ((e0 = op(e1, e0)) | ~ spl3_45), inference(avatar_component_clause, [], [f391])).
fof(f2512, plain, (~ spl3_19 | ~ spl3_51), inference(avatar_split_clause, [], [f2508, f416, f280])).
fof(f2508, plain, (~ (e2 = op(e2, e3)) | ~ spl3_51), inference(backward_demodulation, [], [f128, f418])).
fof(f2506, plain, (~ spl3_49 | ~ spl3_53), inference(avatar_split_clause, [], [f2500, f425, f408])).
fof(f2500, plain, (~ (e0 = op(e0, e3)) | ~ spl3_53), inference(backward_demodulation, [], [f138, f427])).
fof(f2496, plain, (spl3_1 | ~ spl3_60 | ~ spl3_74), inference(avatar_split_clause, [], [f2494, f551, f454, f204])).
fof(f2494, plain, ((e0 = op(e3, e3)) | (~ spl3_60 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f456])).
fof(f2488, plain, (~ spl3_41 | ~ spl3_62 | spl3_79), inference(avatar_split_clause, [], [f2481, f575, f463, f374])).
fof(f463, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f575, plain, (spl3_79 <=> (e0 = op(op(e0, e0), op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_79])])).
fof(f2481, plain, (~ (e0 = op(e1, e1)) | (~ spl3_62 | spl3_79)), inference(backward_demodulation, [], [f576, f465])).
fof(f465, plain, ((op(e0, e0) = e1) | ~ spl3_62), inference(avatar_component_clause, [], [f463])).
fof(f576, plain, (~ (e0 = op(op(e0, e0), op(e0, e0))) | spl3_79), inference(avatar_component_clause, [], [f575])).
fof(f2487, plain, (~ spl3_58 | ~ spl3_62), inference(avatar_split_clause, [], [f2476, f463, f446])).
fof(f446, plain, (spl3_58 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_58])])).
fof(f2476, plain, (~ (e1 = op(e0, e1)) | ~ spl3_62), inference(backward_demodulation, [], [f133, f465])).
fof(f2486, plain, (~ spl3_14 | ~ spl3_62), inference(avatar_split_clause, [], [f2475, f463, f259])).
fof(f2475, plain, (~ (e1 = op(e3, e0)) | ~ spl3_62), inference(backward_demodulation, [], [f111, f465])).
fof(f2469, plain, (~ spl3_4 | spl3_80), inference(avatar_contradiction_clause, [], [f2468])).
fof(f2468, plain, ($false | (~ spl3_4 | spl3_80)), inference(subsumption_resolution, [], [f2464, f218])).
fof(f218, plain, ((e3 = op(e3, e3)) | ~ spl3_4), inference(avatar_component_clause, [], [f216])).
fof(f2464, plain, (~ (e3 = op(e3, e3)) | (~ spl3_4 | spl3_80)), inference(backward_demodulation, [], [f581, f218])).
fof(f2463, plain, (~ spl3_11 | ~ spl3_22 | ~ spl3_71), inference(avatar_contradiction_clause, [], [f2462])).
fof(f2462, plain, ($false | (~ spl3_11 | ~ spl3_22 | ~ spl3_71)), inference(subsumption_resolution, [], [f2461, f161])).
fof(f161, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f4])).
fof(f2461, plain, ((e1 = e3) | (~ spl3_11 | ~ spl3_22 | ~ spl3_71)), inference(forward_demodulation, [], [f2459, f295])).
fof(f2459, plain, ((e3 = op(e2, e2)) | (~ spl3_11 | ~ spl3_71)), inference(backward_demodulation, [], [f538, f248])).
fof(f2460, plain, (~ spl3_3 | ~ spl3_11), inference(avatar_split_clause, [], [f2457, f246, f212])).
fof(f2457, plain, (~ (e2 = op(e3, e3)) | ~ spl3_11), inference(backward_demodulation, [], [f155, f248])).
fof(f2411, plain, (~ spl3_41 | spl3_62 | ~ spl3_73), inference(avatar_contradiction_clause, [], [f2410])).
fof(f2410, plain, ($false | (~ spl3_41 | spl3_62 | ~ spl3_73)), inference(subsumption_resolution, [], [f2404, f464])).
fof(f464, plain, (~ (op(e0, e0) = e1) | spl3_62), inference(avatar_component_clause, [], [f463])).
fof(f2404, plain, ((op(e0, e0) = e1) | (~ spl3_41 | ~ spl3_73)), inference(backward_demodulation, [], [f548, f376])).
fof(f548, plain, ((e1 = op(op(e1, e1), op(e1, e1))) | ~ spl3_73), inference(avatar_component_clause, [], [f546])).
fof(f546, plain, (spl3_73 <=> (e1 = op(op(e1, e1), op(e1, e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_73])])).
fof(f2382, plain, (spl3_41 | ~ spl3_58 | ~ spl3_74), inference(avatar_split_clause, [], [f2379, f551, f446, f374])).
fof(f2379, plain, ((e0 = op(e1, e1)) | (~ spl3_58 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f448])).
fof(f448, plain, ((e1 = op(e0, e1)) | ~ spl3_58), inference(avatar_component_clause, [], [f446])).
fof(f2345, plain, (~ spl3_44 | ~ spl3_40), inference(avatar_split_clause, [], [f2344, f369, f386])).
fof(f2344, plain, (~ (e3 = op(e1, e1)) | ~ spl3_40), inference(forward_demodulation, [], [f142, f371])).
fof(f2338, plain, (~ spl3_18 | ~ spl3_34), inference(avatar_split_clause, [], [f2337, f344, f276])).
fof(f276, plain, (spl3_18 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_18])])).
fof(f2337, plain, (~ (e1 = op(e2, e3)) | ~ spl3_34), inference(forward_demodulation, [], [f130, f346])).
fof(f346, plain, ((e1 = op(e1, e3)) | ~ spl3_34), inference(avatar_component_clause, [], [f344])).
fof(f130, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2319, plain, (~ spl3_1 | ~ spl3_5), inference(avatar_split_clause, [], [f2314, f221, f204])).
fof(f221, plain, (spl3_5 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_5])])).
fof(f2314, plain, (~ (e0 = op(e3, e3)) | ~ spl3_5), inference(backward_demodulation, [], [f156, f223])).
fof(f223, plain, ((e0 = op(e3, e2)) | ~ spl3_5), inference(avatar_component_clause, [], [f221])).
fof(f156, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2277, plain, (~ spl3_26 | ~ spl3_30), inference(avatar_split_clause, [], [f2270, f327, f310])).
fof(f310, plain, (spl3_26 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_26])])).
fof(f2270, plain, (~ (e1 = op(e2, e1)) | ~ spl3_30), inference(backward_demodulation, [], [f145, f329])).
fof(f145, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f2268, plain, (~ spl3_2 | ~ spl3_34), inference(avatar_split_clause, [], [f2264, f344, f208])).
fof(f2264, plain, (~ (e1 = op(e3, e3)) | ~ spl3_34), inference(backward_demodulation, [], [f131, f346])).
fof(f131, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f3])).
fof(f2263, plain, (spl3_2 | ~ spl3_40 | ~ spl3_68), inference(avatar_split_clause, [], [f2261, f522, f369, f208])).
fof(f522, plain, (spl3_68 <=> (e1 = op(op(e1, e2), op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_68])])).
fof(f2261, plain, ((e1 = op(e3, e3)) | (~ spl3_40 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f371])).
fof(f524, plain, ((e1 = op(op(e1, e2), op(e1, e2))) | ~ spl3_68), inference(avatar_component_clause, [], [f522])).
fof(f2262, plain, (~ spl3_24 | ~ spl3_40), inference(avatar_split_clause, [], [f2258, f369, f301])).
fof(f301, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f2258, plain, (~ (e3 = op(e2, e2)) | ~ spl3_40), inference(backward_demodulation, [], [f124, f371])).
fof(f2256, plain, (~ spl3_35 | ~ spl3_43), inference(avatar_split_clause, [], [f2248, f382, f348])).
fof(f2248, plain, (~ (e2 = op(e1, e3)) | ~ spl3_43), inference(backward_demodulation, [], [f143, f384])).
fof(f2244, plain, (~ spl3_45 | spl3_62 | ~ spl3_78), inference(avatar_contradiction_clause, [], [f2243])).
fof(f2243, plain, ($false | (~ spl3_45 | spl3_62 | ~ spl3_78)), inference(subsumption_resolution, [], [f2240, f464])).
fof(f2240, plain, ((op(e0, e0) = e1) | (~ spl3_45 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f393])).
fof(f2231, plain, (spl3_41 | ~ spl3_54 | ~ spl3_69), inference(avatar_split_clause, [], [f2229, f527, f429, f374])).
fof(f527, plain, (spl3_69 <=> (e0 = op(op(e0, e2), op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_69])])).
fof(f2229, plain, ((e0 = op(e1, e1)) | (~ spl3_54 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f431])).
fof(f529, plain, ((e0 = op(op(e0, e2), op(e0, e2))) | ~ spl3_69), inference(avatar_component_clause, [], [f527])).
fof(f2223, plain, (~ spl3_53 | ~ spl3_57), inference(avatar_split_clause, [], [f2217, f442, f425])).
fof(f442, plain, (spl3_57 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_57])])).
fof(f2217, plain, (~ (e0 = op(e0, e2)) | ~ spl3_57), inference(backward_demodulation, [], [f136, f444])).
fof(f444, plain, ((e0 = op(e0, e1)) | ~ spl3_57), inference(avatar_component_clause, [], [f442])).
fof(f2222, plain, (~ spl3_41 | ~ spl3_57), inference(avatar_split_clause, [], [f2214, f442, f374])).
fof(f2214, plain, (~ (e0 = op(e1, e1)) | ~ spl3_57), inference(backward_demodulation, [], [f115, f444])).
fof(f115, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f3])).
fof(f2211, plain, (~ spl3_31 | ~ spl3_63), inference(avatar_split_clause, [], [f2201, f467, f331])).
fof(f2201, plain, (~ (e2 = op(e2, e0)) | ~ spl3_63), inference(backward_demodulation, [], [f110, f469])).
fof(f110, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f3])).
fof(f2187, plain, (~ spl3_1 | ~ spl3_2), inference(avatar_contradiction_clause, [], [f2186])).
fof(f2186, plain, ($false | (~ spl3_1 | ~ spl3_2)), inference(subsumption_resolution, [], [f2185, f157])).
fof(f2185, plain, ((e0 = e1) | (~ spl3_1 | ~ spl3_2)), inference(backward_demodulation, [], [f210, f206])).
fof(f2184, plain, (~ spl3_2 | ~ spl3_35 | spl3_104), inference(avatar_contradiction_clause, [], [f2183])).
fof(f2183, plain, ($false | (~ spl3_2 | ~ spl3_35 | spl3_104)), inference(subsumption_resolution, [], [f2178, f350])).
fof(f2178, plain, (~ (e2 = op(e1, e3)) | (~ spl3_2 | spl3_104)), inference(backward_demodulation, [], [f698, f210])).
fof(f2161, plain, (~ spl3_13 | ~ spl3_62 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f2160])).
fof(f2160, plain, ($false | (~ spl3_13 | ~ spl3_62 | ~ spl3_76)), inference(subsumption_resolution, [], [f2159, f161])).
fof(f2159, plain, ((e1 = e3) | (~ spl3_13 | ~ spl3_62 | ~ spl3_76)), inference(forward_demodulation, [], [f2156, f465])).
fof(f2156, plain, ((op(e0, e0) = e3) | (~ spl3_13 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f257])).
fof(f2158, plain, (~ spl3_1 | ~ spl3_13), inference(avatar_split_clause, [], [f2152, f255, f204])).
fof(f2152, plain, (~ (e0 = op(e3, e3)) | ~ spl3_13), inference(backward_demodulation, [], [f153, f257])).
fof(f2149, plain, (~ spl3_6 | ~ spl3_24 | spl3_96), inference(avatar_split_clause, [], [f2147, f656, f301, f225])).
fof(f225, plain, (spl3_6 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_6])])).
fof(f2147, plain, (~ (e1 = op(e3, e2)) | (~ spl3_24 | spl3_96)), inference(backward_demodulation, [], [f658, f303])).
fof(f303, plain, ((e3 = op(e2, e2)) | ~ spl3_24), inference(avatar_component_clause, [], [f301])).
fof(f2129, plain, (~ spl3_37 | ~ spl3_41), inference(avatar_split_clause, [], [f2122, f374, f357])).
fof(f357, plain, (spl3_37 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_37])])).
fof(f2122, plain, (~ (e0 = op(e1, e2)) | ~ spl3_41), inference(backward_demodulation, [], [f142, f376])).
fof(f2109, plain, (spl3_1 | ~ spl3_52 | ~ spl3_83), inference(avatar_split_clause, [], [f2107, f595, f420, f204])).
fof(f2107, plain, ((e0 = op(e3, e3)) | (~ spl3_52 | ~ spl3_83)), inference(backward_demodulation, [], [f597, f422])).
fof(f422, plain, ((e3 = op(e0, e3)) | ~ spl3_52), inference(avatar_component_clause, [], [f420])).
fof(f2095, plain, (spl3_41 | ~ spl3_62 | ~ spl3_79), inference(avatar_split_clause, [], [f2088, f575, f463, f374])).
fof(f2088, plain, ((e0 = op(e1, e1)) | (~ spl3_62 | ~ spl3_79)), inference(backward_demodulation, [], [f577, f465])).
fof(f577, plain, ((e0 = op(op(e0, e0), op(e0, e0))) | ~ spl3_79), inference(avatar_component_clause, [], [f575])).
fof(f2094, plain, (~ spl3_50 | ~ spl3_62), inference(avatar_split_clause, [], [f2085, f463, f412])).
fof(f2085, plain, (~ (e1 = op(e0, e3)) | ~ spl3_62), inference(backward_demodulation, [], [f135, f465])).
fof(f2093, plain, (~ spl3_46 | ~ spl3_62), inference(avatar_split_clause, [], [f2082, f463, f395])).
fof(f395, plain, (spl3_46 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_46])])).
fof(f2082, plain, (~ (e1 = op(e1, e0)) | ~ spl3_62), inference(backward_demodulation, [], [f109, f465])).
fof(f109, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f3])).
fof(f2081, plain, (spl3_23 | ~ spl3_31 | ~ spl3_77), inference(avatar_contradiction_clause, [], [f2080])).
fof(f2080, plain, ($false | (spl3_23 | ~ spl3_31 | ~ spl3_77)), inference(subsumption_resolution, [], [f2079, f298])).
fof(f298, plain, (~ (e2 = op(e2, e2)) | spl3_23), inference(avatar_component_clause, [], [f297])).
fof(f2079, plain, ((e2 = op(e2, e2)) | (~ spl3_31 | ~ spl3_77)), inference(forward_demodulation, [], [f567, f333])).
fof(f2075, plain, (~ spl3_48 | ~ spl3_84 | spl3_85), inference(avatar_split_clause, [], [f2074, f604, f600, f403])).
fof(f604, plain, (spl3_85 <=> (e3 = op(e1, op(op(e1, e1), e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_85])])).
fof(f2074, plain, (~ (e3 = op(e1, e0)) | (~ spl3_84 | spl3_85)), inference(forward_demodulation, [], [f606, f601])).
fof(f601, plain, ((e0 = op(op(e1, e1), e1)) | ~ spl3_84), inference(avatar_component_clause, [], [f600])).
fof(f606, plain, (~ (e3 = op(e1, op(op(e1, e1), e1))) | spl3_85), inference(avatar_component_clause, [], [f604])).
fof(f2065, plain, (~ spl3_55 | ~ spl3_59), inference(avatar_split_clause, [], [f2064, f450, f433])).
fof(f2064, plain, (~ (e2 = op(e0, e2)) | ~ spl3_59), inference(forward_demodulation, [], [f136, f452])).
fof(f2057, plain, (~ spl3_47 | ~ spl3_84 | spl3_86), inference(avatar_split_clause, [], [f2056, f609, f600, f399])).
fof(f609, plain, (spl3_86 <=> (e2 = op(e1, op(op(e1, e1), e1)))), introduced(avatar_definition, [new_symbols(naming, [spl3_86])])).
fof(f2056, plain, (~ (e2 = op(e1, e0)) | (~ spl3_84 | spl3_86)), inference(forward_demodulation, [], [f611, f601])).
fof(f611, plain, (~ (e2 = op(e1, op(op(e1, e1), e1))) | spl3_86), inference(avatar_component_clause, [], [f609])).
fof(f2043, plain, (~ spl3_10 | ~ spl3_26), inference(avatar_split_clause, [], [f2042, f310, f242])).
fof(f2042, plain, (~ (e1 = op(e3, e1)) | ~ spl3_26), inference(forward_demodulation, [], [f120, f312])).
fof(f312, plain, ((e1 = op(e2, e1)) | ~ spl3_26), inference(avatar_component_clause, [], [f310])).
fof(f120, plain, ~ (op(e2, e1) = op(e3, e1)), inference(cnf_transformation, [], [f3])).
fof(f2018, plain, (~ spl3_18 | ~ spl3_26), inference(avatar_split_clause, [], [f2013, f310, f276])).
fof(f2013, plain, (~ (e1 = op(e2, e3)) | ~ spl3_26), inference(backward_demodulation, [], [f149, f312])).
fof(f149, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f3])).
fof(f2001, plain, (~ spl3_18 | ~ spl3_50), inference(avatar_split_clause, [], [f1999, f412, f276])).
fof(f1999, plain, (~ (e1 = op(e2, e3)) | ~ spl3_50), inference(backward_demodulation, [], [f128, f414])).
fof(f1993, plain, (~ spl3_27 | ~ spl3_59), inference(avatar_split_clause, [], [f1991, f450, f314])).
fof(f1991, plain, (~ (e2 = op(e2, e1)) | ~ spl3_59), inference(backward_demodulation, [], [f116, f452])).
fof(f116, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f3])).
fof(f1984, plain, (~ spl3_61 | spl3_79), inference(avatar_contradiction_clause, [], [f1983])).
fof(f1983, plain, ($false | (~ spl3_61 | spl3_79)), inference(subsumption_resolution, [], [f1977, f461])).
fof(f1977, plain, (~ (e0 = op(e0, e0)) | (~ spl3_61 | spl3_79)), inference(backward_demodulation, [], [f576, f461])).
fof(f1981, plain, (~ spl3_29 | ~ spl3_61), inference(avatar_split_clause, [], [f1971, f459, f323])).
fof(f323, plain, (spl3_29 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_29])])).
fof(f1971, plain, (~ (e0 = op(e2, e0)) | ~ spl3_61), inference(backward_demodulation, [], [f110, f461])).
fof(f1936, plain, (~ spl3_18 | spl3_43 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f1935])).
fof(f1935, plain, ($false | (~ spl3_18 | spl3_43 | ~ spl3_81)), inference(subsumption_resolution, [], [f1934, f383])).
fof(f383, plain, (~ (e2 = op(e1, e1)) | spl3_43), inference(avatar_component_clause, [], [f382])).
fof(f1934, plain, ((e2 = op(e1, e1)) | (~ spl3_18 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f278])).
fof(f278, plain, ((e1 = op(e2, e3)) | ~ spl3_18), inference(avatar_component_clause, [], [f276])).
fof(f1924, plain, (~ spl3_23 | ~ spl3_27), inference(avatar_split_clause, [], [f1920, f314, f297])).
fof(f1920, plain, (~ (e2 = op(e2, e2)) | ~ spl3_27), inference(backward_demodulation, [], [f148, f316])).
fof(f316, plain, ((e2 = op(e2, e1)) | ~ spl3_27), inference(avatar_component_clause, [], [f314])).
fof(f148, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1919, plain, (~ spl3_3 | ~ spl3_35), inference(avatar_split_clause, [], [f1916, f348, f212])).
fof(f1916, plain, (~ (e2 = op(e3, e3)) | ~ spl3_35), inference(backward_demodulation, [], [f131, f350])).
fof(f1895, plain, (~ spl3_56 | ~ spl3_64), inference(avatar_split_clause, [], [f1889, f471, f437])).
fof(f1889, plain, (~ (e3 = op(e0, e2)) | ~ spl3_64), inference(backward_demodulation, [], [f134, f473])).
fof(f1881, plain, (~ spl3_49 | spl3_61 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1880])).
fof(f1880, plain, ($false | (~ spl3_49 | spl3_61 | ~ spl3_83)), inference(subsumption_resolution, [], [f1879, f460])).
fof(f460, plain, (~ (e0 = op(e0, e0)) | spl3_61), inference(avatar_component_clause, [], [f459])).
fof(f1879, plain, ((e0 = op(e0, e0)) | (~ spl3_49 | ~ spl3_83)), inference(forward_demodulation, [], [f597, f410])).
fof(f1862, plain, (~ spl3_36 | ~ spl3_44), inference(avatar_split_clause, [], [f1861, f386, f352])).
fof(f1861, plain, (~ (e3 = op(e1, e3)) | ~ spl3_44), inference(forward_demodulation, [], [f143, f388])).
fof(f388, plain, ((e3 = op(e1, e1)) | ~ spl3_44), inference(avatar_component_clause, [], [f386])).
fof(f1834, plain, (~ spl3_1 | ~ spl3_49), inference(avatar_split_clause, [], [f1829, f408, f204])).
fof(f1829, plain, (~ (e0 = op(e3, e3)) | ~ spl3_49), inference(backward_demodulation, [], [f129, f410])).
fof(f1817, plain, (~ spl3_47 | ~ spl3_62 | spl3_100), inference(avatar_contradiction_clause, [], [f1816])).
fof(f1816, plain, ($false | (~ spl3_47 | ~ spl3_62 | spl3_100)), inference(subsumption_resolution, [], [f1815, f401])).
fof(f1815, plain, (~ (e2 = op(e1, e0)) | (~ spl3_62 | spl3_100)), inference(forward_demodulation, [], [f678, f465])).
fof(f678, plain, (~ (e2 = op(op(e0, e0), e0)) | spl3_100), inference(avatar_component_clause, [], [f676])).
fof(f676, plain, (spl3_100 <=> (e2 = op(op(e0, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_100])])).
fof(f1801, plain, (spl3_11 | ~ spl3_44 | ~ spl3_102), inference(avatar_split_clause, [], [f1733, f686, f386, f246])).
fof(f686, plain, (spl3_102 <=> (e2 = op(op(e1, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_102])])).
fof(f1733, plain, ((e2 = op(e3, e1)) | (~ spl3_44 | ~ spl3_102)), inference(forward_demodulation, [], [f687, f388])).
fof(f687, plain, ((e2 = op(op(e1, e1), e1)) | ~ spl3_102), inference(avatar_component_clause, [], [f686])).
fof(f1798, plain, (spl3_1 | ~ spl3_56 | ~ spl3_69), inference(avatar_split_clause, [], [f1760, f527, f437, f204])).
fof(f1760, plain, ((e0 = op(e3, e3)) | (~ spl3_56 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f439])).
fof(f439, plain, ((e3 = op(e0, e2)) | ~ spl3_56), inference(avatar_component_clause, [], [f437])).
fof(f1772, plain, (~ spl3_1 | ~ spl3_6 | ~ spl3_51 | spl3_92), inference(avatar_contradiction_clause, [], [f1771])).
fof(f1771, plain, ($false | (~ spl3_1 | ~ spl3_6 | ~ spl3_51 | spl3_92)), inference(subsumption_resolution, [], [f1767, f227])).
fof(f227, plain, ((e1 = op(e3, e2)) | ~ spl3_6), inference(avatar_component_clause, [], [f225])).
fof(f1767, plain, (~ (e1 = op(e3, e2)) | (~ spl3_1 | ~ spl3_51 | spl3_92)), inference(backward_demodulation, [], [f1701, f418])).
fof(f1701, plain, (~ (e1 = op(e3, op(e0, e3))) | (~ spl3_1 | spl3_92)), inference(backward_demodulation, [], [f639, f206])).
fof(f639, plain, (~ (e1 = op(e3, op(op(e3, e3), e3))) | spl3_92), inference(avatar_component_clause, [], [f637])).
fof(f637, plain, (spl3_92 <=> (e1 = op(e3, op(op(e3, e3), e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_92])])).
fof(f1762, plain, (~ spl3_52 | ~ spl3_56), inference(avatar_split_clause, [], [f1759, f437, f420])).
fof(f1759, plain, (~ (e3 = op(e0, e3)) | ~ spl3_56), inference(backward_demodulation, [], [f138, f439])).
fof(f1748, plain, (~ spl3_56 | ~ spl3_47 | ~ spl3_62 | spl3_94), inference(avatar_split_clause, [], [f1747, f646, f463, f399, f437])).
fof(f646, plain, (spl3_94 <=> (e3 = op(e0, op(op(e0, e0), e0)))), introduced(avatar_definition, [new_symbols(naming, [spl3_94])])).
fof(f1747, plain, (~ (e3 = op(e0, e2)) | (~ spl3_47 | ~ spl3_62 | spl3_94)), inference(forward_demodulation, [], [f1743, f401])).
fof(f1743, plain, (~ (e3 = op(e0, op(e1, e0))) | (~ spl3_62 | spl3_94)), inference(backward_demodulation, [], [f648, f465])).
fof(f648, plain, (~ (e3 = op(e0, op(op(e0, e0), e0))) | spl3_94), inference(avatar_component_clause, [], [f646])).
fof(f1736, plain, (spl3_62 | ~ spl3_37 | ~ spl3_68), inference(avatar_split_clause, [], [f1735, f522, f357, f463])).
fof(f1735, plain, ((op(e0, e0) = e1) | (~ spl3_37 | ~ spl3_68)), inference(forward_demodulation, [], [f524, f359])).
fof(f359, plain, ((e0 = op(e1, e2)) | ~ spl3_37), inference(avatar_component_clause, [], [f357])).
fof(f1728, plain, (spl3_64 | ~ spl3_1 | ~ spl3_80), inference(avatar_split_clause, [], [f1699, f580, f204, f471])).
fof(f1699, plain, ((op(e0, e0) = e3) | (~ spl3_1 | ~ spl3_80)), inference(backward_demodulation, [], [f582, f206])).
fof(f1705, plain, (~ spl3_1 | ~ spl3_61 | ~ spl3_80), inference(avatar_contradiction_clause, [], [f1704])).
fof(f1704, plain, ($false | (~ spl3_1 | ~ spl3_61 | ~ spl3_80)), inference(subsumption_resolution, [], [f1703, f159])).
fof(f1703, plain, ((e0 = e3) | (~ spl3_1 | ~ spl3_61 | ~ spl3_80)), inference(forward_demodulation, [], [f1699, f461])).
fof(f1686, plain, (~ spl3_19 | spl3_23 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f1685])).
fof(f1685, plain, ($false | (~ spl3_19 | spl3_23 | ~ spl3_81)), inference(subsumption_resolution, [], [f1683, f298])).
fof(f1683, plain, ((e2 = op(e2, e2)) | (~ spl3_19 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f282])).
fof(f282, plain, ((e2 = op(e2, e3)) | ~ spl3_19), inference(avatar_component_clause, [], [f280])).
fof(f1656, plain, (~ spl3_57 | ~ spl3_61), inference(avatar_split_clause, [], [f1650, f459, f442])).
fof(f1650, plain, (~ (e0 = op(e0, e1)) | ~ spl3_61), inference(backward_demodulation, [], [f133, f461])).
fof(f1631, plain, (~ spl3_42 | ~ spl3_34), inference(avatar_split_clause, [], [f1630, f344, f378])).
fof(f1630, plain, (~ (e1 = op(e1, e1)) | ~ spl3_34), inference(forward_demodulation, [], [f143, f346])).
fof(f1624, plain, (spl3_42 | ~ spl3_34 | ~ spl3_82), inference(avatar_split_clause, [], [f1555, f590, f344, f378])).
fof(f1555, plain, ((e1 = op(e1, e1)) | (~ spl3_34 | ~ spl3_82)), inference(backward_demodulation, [], [f592, f346])).
fof(f1623, plain, (spl3_44 | ~ spl3_6 | ~ spl3_66), inference(avatar_split_clause, [], [f1596, f512, f225, f386])).
fof(f512, plain, (spl3_66 <=> (e3 = op(op(e3, e2), op(e3, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl3_66])])).
fof(f1596, plain, ((e3 = op(e1, e1)) | (~ spl3_6 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f227])).
fof(f514, plain, ((e3 = op(op(e3, e2), op(e3, e2))) | ~ spl3_66), inference(avatar_component_clause, [], [f512])).
fof(f1616, plain, (~ spl3_25 | ~ spl3_6 | ~ spl3_24 | spl3_97), inference(avatar_split_clause, [], [f1598, f661, f301, f225, f306])).
fof(f1598, plain, (~ (e0 = op(e2, e1)) | (~ spl3_6 | ~ spl3_24 | spl3_97)), inference(backward_demodulation, [], [f1099, f227])).
fof(f1099, plain, (~ (e0 = op(e2, op(e3, e2))) | (~ spl3_24 | spl3_97)), inference(backward_demodulation, [], [f663, f303])).
fof(f1614, plain, (~ spl3_10 | ~ spl3_6), inference(avatar_split_clause, [], [f1613, f225, f242])).
fof(f1613, plain, (~ (e1 = op(e3, e1)) | ~ spl3_6), inference(forward_demodulation, [], [f154, f227])).
fof(f154, plain, ~ (op(e3, e1) = op(e3, e2)), inference(cnf_transformation, [], [f3])).
fof(f1573, plain, (~ spl3_17 | ~ spl3_62 | ~ spl3_81), inference(avatar_contradiction_clause, [], [f1572])).
fof(f1572, plain, ($false | (~ spl3_17 | ~ spl3_62 | ~ spl3_81)), inference(subsumption_resolution, [], [f1571, f160])).
fof(f1571, plain, ((e1 = e2) | (~ spl3_17 | ~ spl3_62 | ~ spl3_81)), inference(forward_demodulation, [], [f1568, f465])).
fof(f1568, plain, ((op(e0, e0) = e2) | (~ spl3_17 | ~ spl3_81)), inference(backward_demodulation, [], [f587, f274])).
fof(f1538, plain, (~ spl3_57 | spl3_61 | ~ spl3_74), inference(avatar_contradiction_clause, [], [f1537])).
fof(f1537, plain, ($false | (~ spl3_57 | spl3_61 | ~ spl3_74)), inference(subsumption_resolution, [], [f1534, f460])).
fof(f1534, plain, ((e0 = op(e0, e0)) | (~ spl3_57 | ~ spl3_74)), inference(backward_demodulation, [], [f553, f444])).
fof(f1536, plain, (~ spl3_49 | ~ spl3_57), inference(avatar_split_clause, [], [f1530, f442, f408])).
fof(f1530, plain, (~ (e0 = op(e0, e3)) | ~ spl3_57), inference(backward_demodulation, [], [f137, f444])).
fof(f1525, plain, (~ spl3_54 | ~ spl3_62), inference(avatar_split_clause, [], [f1518, f463, f429])).
fof(f1518, plain, (~ (e1 = op(e0, e2)) | ~ spl3_62), inference(backward_demodulation, [], [f134, f465])).
fof(f1514, plain, (~ spl3_42 | spl3_73), inference(avatar_contradiction_clause, [], [f1513])).
fof(f1513, plain, ($false | (~ spl3_42 | spl3_73)), inference(subsumption_resolution, [], [f1512, f380])).
fof(f1512, plain, (~ (e1 = op(e1, e1)) | (~ spl3_42 | spl3_73)), inference(forward_demodulation, [], [f547, f380])).
fof(f547, plain, (~ (e1 = op(op(e1, e1), op(e1, e1))) | spl3_73), inference(avatar_component_clause, [], [f546])).
fof(f1486, plain, (~ spl3_9 | ~ spl3_12), inference(avatar_contradiction_clause, [], [f1485])).
fof(f1485, plain, ($false | (~ spl3_9 | ~ spl3_12)), inference(subsumption_resolution, [], [f1484, f159])).
fof(f1484, plain, ((e0 = e3) | (~ spl3_9 | ~ spl3_12)), inference(forward_demodulation, [], [f252, f240])).
fof(f252, plain, ((e3 = op(e3, e1)) | ~ spl3_12), inference(avatar_component_clause, [], [f250])).
fof(f1481, plain, (~ spl3_14 | ~ spl3_42 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f1480])).
fof(f1480, plain, ($false | (~ spl3_14 | ~ spl3_42 | ~ spl3_76)), inference(subsumption_resolution, [], [f1479, f161])).
fof(f1479, plain, ((e1 = e3) | (~ spl3_14 | ~ spl3_42 | ~ spl3_76)), inference(forward_demodulation, [], [f1474, f380])).
fof(f1474, plain, ((e3 = op(e1, e1)) | (~ spl3_14 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f261])).
fof(f1463, plain, (~ spl3_39 | ~ spl3_47), inference(avatar_split_clause, [], [f1459, f399, f365])).
fof(f1459, plain, (~ (e2 = op(e1, e2)) | ~ spl3_47), inference(backward_demodulation, [], [f140, f401])).
fof(f140, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f3])).
fof(f1462, plain, (spl3_22 | ~ spl3_47 | ~ spl3_78), inference(avatar_contradiction_clause, [], [f1461])).
fof(f1461, plain, ($false | (spl3_22 | ~ spl3_47 | ~ spl3_78)), inference(subsumption_resolution, [], [f1458, f294])).
fof(f294, plain, (~ (e1 = op(e2, e2)) | spl3_22), inference(avatar_component_clause, [], [f293])).
fof(f1458, plain, ((e1 = op(e2, e2)) | (~ spl3_47 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f401])).
fof(f1452, plain, (spl3_21 | ~ spl3_51 | ~ spl3_83), inference(avatar_contradiction_clause, [], [f1451])).
fof(f1451, plain, ($false | (spl3_21 | ~ spl3_51 | ~ spl3_83)), inference(subsumption_resolution, [], [f1449, f290])).
fof(f290, plain, (~ (e0 = op(e2, e2)) | spl3_21), inference(avatar_component_clause, [], [f289])).
fof(f1449, plain, ((e0 = op(e2, e2)) | (~ spl3_51 | ~ spl3_83)), inference(backward_demodulation, [], [f597, f418])).
fof(f1448, plain, (~ spl3_50 | ~ spl3_54), inference(avatar_split_clause, [], [f1443, f429, f412])).
fof(f1443, plain, (~ (e1 = op(e0, e3)) | ~ spl3_54), inference(backward_demodulation, [], [f138, f431])).
fof(f1440, plain, (spl3_1 | ~ spl3_64 | ~ spl3_79), inference(avatar_split_clause, [], [f1435, f575, f471, f204])).
fof(f1435, plain, ((e0 = op(e3, e3)) | (~ spl3_64 | ~ spl3_79)), inference(backward_demodulation, [], [f577, f473])).
fof(f1421, plain, (spl3_2 | ~ spl3_36 | ~ spl3_82), inference(avatar_contradiction_clause, [], [f1420])).
fof(f1420, plain, ($false | (spl3_2 | ~ spl3_36 | ~ spl3_82)), inference(subsumption_resolution, [], [f1419, f209])).
fof(f209, plain, (~ (e1 = op(e3, e3)) | spl3_2), inference(avatar_component_clause, [], [f208])).
fof(f1419, plain, ((e1 = op(e3, e3)) | (~ spl3_36 | ~ spl3_82)), inference(forward_demodulation, [], [f592, f354])).
fof(f354, plain, ((e3 = op(e1, e3)) | ~ spl3_36), inference(avatar_component_clause, [], [f352])).
fof(f1417, plain, (~ spl3_64 | ~ spl3_60), inference(avatar_split_clause, [], [f1416, f454, f471])).
fof(f1416, plain, (~ (op(e0, e0) = e3) | ~ spl3_60), inference(forward_demodulation, [], [f133, f456])).
fof(f1407, plain, (~ spl3_46 | ~ spl3_42), inference(avatar_split_clause, [], [f1406, f378, f395])).
fof(f1406, plain, (~ (e1 = op(e1, e0)) | ~ spl3_42), inference(forward_demodulation, [], [f139, f380])).
fof(f1397, plain, (~ spl3_40 | ~ spl3_36), inference(avatar_split_clause, [], [f1396, f352, f369])).
fof(f1396, plain, (~ (e3 = op(e1, e2)) | ~ spl3_36), inference(forward_demodulation, [], [f144, f354])).
fof(f1371, plain, (~ spl3_1 | ~ spl3_3), inference(avatar_contradiction_clause, [], [f1370])).
fof(f1370, plain, ($false | (~ spl3_1 | ~ spl3_3)), inference(subsumption_resolution, [], [f1369, f158])).
fof(f158, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f4])).
fof(f1369, plain, ((e0 = e2) | (~ spl3_1 | ~ spl3_3)), inference(forward_demodulation, [], [f214, f206])).
fof(f214, plain, ((e2 = op(e3, e3)) | ~ spl3_3), inference(avatar_component_clause, [], [f212])).
fof(f1366, plain, (~ spl3_1 | ~ spl3_16 | ~ spl3_76), inference(avatar_contradiction_clause, [], [f1365])).
fof(f1365, plain, ($false | (~ spl3_1 | ~ spl3_16 | ~ spl3_76)), inference(subsumption_resolution, [], [f1364, f159])).
fof(f1364, plain, ((e0 = e3) | (~ spl3_1 | ~ spl3_16 | ~ spl3_76)), inference(forward_demodulation, [], [f1363, f206])).
fof(f1363, plain, ((e3 = op(e3, e3)) | (~ spl3_16 | ~ spl3_76)), inference(backward_demodulation, [], [f562, f269])).
fof(f269, plain, ((e3 = op(e3, e0)) | ~ spl3_16), inference(avatar_component_clause, [], [f267])).
fof(f267, plain, (spl3_16 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_16])])).
fof(f1355, plain, (spl3_22 | ~ spl3_39 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f1354])).
fof(f1354, plain, ($false | (spl3_22 | ~ spl3_39 | ~ spl3_68)), inference(subsumption_resolution, [], [f1353, f294])).
fof(f1353, plain, ((e1 = op(e2, e2)) | (~ spl3_39 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f367])).
fof(f1339, plain, (spl3_42 | ~ spl3_46 | ~ spl3_78), inference(avatar_split_clause, [], [f1335, f570, f395, f378])).
fof(f1335, plain, ((e1 = op(e1, e1)) | (~ spl3_46 | ~ spl3_78)), inference(backward_demodulation, [], [f572, f397])).
fof(f397, plain, ((e1 = op(e1, e0)) | ~ spl3_46), inference(avatar_component_clause, [], [f395])).
fof(f1321, plain, (spl3_21 | ~ spl3_63 | ~ spl3_79), inference(avatar_contradiction_clause, [], [f1320])).
fof(f1320, plain, ($false | (spl3_21 | ~ spl3_63 | ~ spl3_79)), inference(subsumption_resolution, [], [f1313, f290])).
fof(f1313, plain, ((e0 = op(e2, e2)) | (~ spl3_63 | ~ spl3_79)), inference(backward_demodulation, [], [f577, f469])).
fof(f1307, plain, (spl3_63 | ~ spl3_29 | ~ spl3_77), inference(avatar_split_clause, [], [f1306, f565, f323, f467])).
fof(f1306, plain, ((op(e0, e0) = e2) | (~ spl3_29 | ~ spl3_77)), inference(forward_demodulation, [], [f567, f325])).
fof(f325, plain, ((e0 = op(e2, e0)) | ~ spl3_29), inference(avatar_component_clause, [], [f323])).
fof(f1304, plain, (~ spl3_1 | ~ spl3_50 | spl3_98), inference(avatar_contradiction_clause, [], [f1303])).
fof(f1303, plain, ($false | (~ spl3_1 | ~ spl3_50 | spl3_98)), inference(subsumption_resolution, [], [f1302, f414])).
fof(f1302, plain, (~ (e1 = op(e0, e3)) | (~ spl3_1 | spl3_98)), inference(forward_demodulation, [], [f668, f206])).
fof(f668, plain, (~ (e1 = op(op(e3, e3), e3)) | spl3_98), inference(avatar_component_clause, [], [f666])).
fof(f1287, plain, (~ spl3_1 | ~ spl3_33), inference(avatar_contradiction_clause, [], [f1286])).
fof(f1286, plain, ($false | (~ spl3_1 | ~ spl3_33)), inference(subsumption_resolution, [], [f1285, f342])).
fof(f1285, plain, (~ (e0 = op(e1, e3)) | ~ spl3_1), inference(forward_demodulation, [], [f131, f206])).
fof(f1216, plain, (~ spl3_9 | ~ spl3_44 | spl3_84), inference(avatar_split_clause, [], [f1212, f600, f386, f238])).
fof(f1212, plain, (~ (e0 = op(e3, e1)) | (~ spl3_44 | spl3_84)), inference(backward_demodulation, [], [f602, f388])).
fof(f1215, plain, (spl3_2 | ~ spl3_44 | ~ spl3_73), inference(avatar_split_clause, [], [f1211, f546, f386, f208])).
fof(f1211, plain, ((e1 = op(e3, e3)) | (~ spl3_44 | ~ spl3_73)), inference(backward_demodulation, [], [f548, f388])).
fof(f1206, plain, (~ spl3_53 | ~ spl3_55), inference(avatar_contradiction_clause, [], [f1205])).
fof(f1205, plain, ($false | (~ spl3_53 | ~ spl3_55)), inference(subsumption_resolution, [], [f1204, f158])).
fof(f1204, plain, ((e0 = e2) | (~ spl3_53 | ~ spl3_55)), inference(forward_demodulation, [], [f435, f427])).
fof(f1203, plain, (~ spl3_53 | ~ spl3_56), inference(avatar_contradiction_clause, [], [f1202])).
fof(f1202, plain, ($false | (~ spl3_53 | ~ spl3_56)), inference(subsumption_resolution, [], [f1201, f159])).
fof(f1201, plain, ((e0 = e3) | (~ spl3_53 | ~ spl3_56)), inference(forward_demodulation, [], [f439, f427])).
fof(f1178, plain, (spl3_4 | ~ spl3_12 | ~ spl3_71), inference(avatar_contradiction_clause, [], [f1177])).
fof(f1177, plain, ($false | (spl3_4 | ~ spl3_12 | ~ spl3_71)), inference(subsumption_resolution, [], [f1176, f217])).
fof(f217, plain, (~ (e3 = op(e3, e3)) | spl3_4), inference(avatar_component_clause, [], [f216])).
fof(f1176, plain, ((e3 = op(e3, e3)) | (~ spl3_12 | ~ spl3_71)), inference(forward_demodulation, [], [f538, f252])).
fof(f1175, plain, (spl3_23 | ~ spl3_27 | ~ spl3_72), inference(avatar_split_clause, [], [f1174, f541, f314, f297])).
fof(f1174, plain, ((e2 = op(e2, e2)) | (~ spl3_27 | ~ spl3_72)), inference(forward_demodulation, [], [f543, f316])).
fof(f1160, plain, (~ spl3_44 | ~ spl3_12), inference(avatar_split_clause, [], [f1159, f250, f386])).
fof(f1159, plain, (~ (e3 = op(e1, e1)) | ~ spl3_12), inference(forward_demodulation, [], [f119, f252])).
fof(f1152, plain, (spl3_42 | ~ spl3_38 | ~ spl3_68), inference(avatar_split_clause, [], [f1078, f522, f361, f378])).
fof(f1078, plain, ((e1 = op(e1, e1)) | (~ spl3_38 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f363])).
fof(f1126, plain, (spl3_3 | ~ spl3_24 | ~ spl3_67), inference(avatar_split_clause, [], [f1095, f517, f301, f212])).
fof(f1095, plain, ((e2 = op(e3, e3)) | (~ spl3_24 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f303])).
fof(f1125, plain, (~ spl3_13 | ~ spl3_15), inference(avatar_contradiction_clause, [], [f1124])).
fof(f1124, plain, ($false | (~ spl3_13 | ~ spl3_15)), inference(subsumption_resolution, [], [f1123, f158])).
fof(f1123, plain, ((e0 = e2) | (~ spl3_13 | ~ spl3_15)), inference(backward_demodulation, [], [f265, f257])).
fof(f1088, plain, (~ spl3_22 | ~ spl3_30), inference(avatar_split_clause, [], [f1085, f327, f293])).
fof(f1085, plain, (~ (e1 = op(e2, e2)) | ~ spl3_30), inference(backward_demodulation, [], [f146, f329])).
fof(f146, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f3])).
fof(f1082, plain, (~ spl3_38 | ~ spl3_41 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f1081])).
fof(f1081, plain, ($false | (~ spl3_38 | ~ spl3_41 | ~ spl3_68)), inference(subsumption_resolution, [], [f1080, f157])).
fof(f1080, plain, ((e0 = e1) | (~ spl3_38 | ~ spl3_41 | ~ spl3_68)), inference(forward_demodulation, [], [f1078, f376])).
fof(f1067, plain, (~ spl3_53 | spl3_61 | ~ spl3_69), inference(avatar_contradiction_clause, [], [f1066])).
fof(f1066, plain, ($false | (~ spl3_53 | spl3_61 | ~ spl3_69)), inference(subsumption_resolution, [], [f1064, f460])).
fof(f1064, plain, ((e0 = op(e0, e0)) | (~ spl3_53 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f427])).
fof(f1030, plain, (spl3_24 | ~ spl3_7 | ~ spl3_66), inference(avatar_split_clause, [], [f1025, f512, f229, f301])).
fof(f1025, plain, ((e3 = op(e2, e2)) | (~ spl3_7 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f231])).
fof(f231, plain, ((e2 = op(e3, e2)) | ~ spl3_7), inference(avatar_component_clause, [], [f229])).
fof(f1018, plain, (~ spl3_5 | ~ spl3_2 | ~ spl3_35 | spl3_99), inference(avatar_split_clause, [], [f935, f671, f348, f208, f221])).
fof(f671, plain, (spl3_99 <=> (e0 = op(e3, op(op(e3, e3), e3)))), introduced(avatar_definition, [new_symbols(naming, [spl3_99])])).
fof(f935, plain, (~ (e0 = op(e3, e2)) | (~ spl3_2 | ~ spl3_35 | spl3_99)), inference(backward_demodulation, [], [f892, f350])).
fof(f892, plain, (~ (e0 = op(e3, op(e1, e3))) | (~ spl3_2 | spl3_99)), inference(backward_demodulation, [], [f673, f210])).
fof(f673, plain, (~ (e0 = op(e3, op(op(e3, e3), e3))) | spl3_99), inference(avatar_component_clause, [], [f671])).
fof(f1002, plain, (~ spl3_22 | ~ spl3_41 | ~ spl3_67), inference(avatar_contradiction_clause, [], [f1001])).
fof(f1001, plain, ($false | (~ spl3_22 | ~ spl3_41 | ~ spl3_67)), inference(subsumption_resolution, [], [f1000, f158])).
fof(f1000, plain, ((e0 = e2) | (~ spl3_22 | ~ spl3_41 | ~ spl3_67)), inference(forward_demodulation, [], [f995, f376])).
fof(f995, plain, ((e2 = op(e1, e1)) | (~ spl3_22 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f295])).
fof(f979, plain, (spl3_21 | ~ spl3_55 | ~ spl3_69), inference(avatar_split_clause, [], [f976, f527, f433, f289])).
fof(f976, plain, ((e0 = op(e2, e2)) | (~ spl3_55 | ~ spl3_69)), inference(backward_demodulation, [], [f529, f435])).
fof(f962, plain, (~ spl3_59 | ~ spl3_41 | spl3_102), inference(avatar_split_clause, [], [f786, f686, f374, f450])).
fof(f786, plain, (~ (e2 = op(e0, e1)) | (~ spl3_41 | spl3_102)), inference(backward_demodulation, [], [f688, f376])).
fof(f688, plain, (~ (e2 = op(op(e1, e1), e1)) | spl3_102), inference(avatar_component_clause, [], [f686])).
fof(f943, plain, (~ spl3_33 | ~ spl3_17), inference(avatar_split_clause, [], [f942, f272, f340])).
fof(f942, plain, (~ (e0 = op(e1, e3)) | ~ spl3_17), inference(forward_demodulation, [], [f130, f274])).
fof(f924, plain, (~ spl3_21 | ~ spl3_5), inference(avatar_split_clause, [], [f923, f221, f289])).
fof(f923, plain, (~ (e0 = op(e2, e2)) | ~ spl3_5), inference(forward_demodulation, [], [f126, f223])).
fof(f915, plain, (~ spl3_14 | ~ spl3_16), inference(avatar_contradiction_clause, [], [f914])).
fof(f914, plain, ($false | (~ spl3_14 | ~ spl3_16)), inference(subsumption_resolution, [], [f913, f161])).
fof(f913, plain, ((e1 = e3) | (~ spl3_14 | ~ spl3_16)), inference(forward_demodulation, [], [f269, f261])).
fof(f880, plain, (spl3_4 | ~ spl3_8 | ~ spl3_66), inference(avatar_split_clause, [], [f878, f512, f233, f216])).
fof(f878, plain, ((e3 = op(e3, e3)) | (~ spl3_8 | ~ spl3_66)), inference(backward_demodulation, [], [f514, f235])).
fof(f235, plain, ((e3 = op(e3, e2)) | ~ spl3_8), inference(avatar_component_clause, [], [f233])).
fof(f854, plain, (~ spl3_17 | ~ spl3_18), inference(avatar_contradiction_clause, [], [f853])).
fof(f853, plain, ($false | (~ spl3_17 | ~ spl3_18)), inference(subsumption_resolution, [], [f852, f157])).
fof(f852, plain, ((e0 = e1) | (~ spl3_17 | ~ spl3_18)), inference(backward_demodulation, [], [f278, f274])).
fof(f840, plain, (~ spl3_21 | ~ spl3_61 | ~ spl3_67), inference(avatar_contradiction_clause, [], [f839])).
fof(f839, plain, ($false | (~ spl3_21 | ~ spl3_61 | ~ spl3_67)), inference(subsumption_resolution, [], [f838, f158])).
fof(f838, plain, ((e0 = e2) | (~ spl3_21 | ~ spl3_61 | ~ spl3_67)), inference(forward_demodulation, [], [f833, f461])).
fof(f833, plain, ((op(e0, e0) = e2) | (~ spl3_21 | ~ spl3_67)), inference(backward_demodulation, [], [f519, f291])).
fof(f828, plain, (~ spl3_21 | ~ spl3_25), inference(avatar_split_clause, [], [f825, f306, f289])).
fof(f825, plain, (~ (e0 = op(e2, e2)) | ~ spl3_25), inference(backward_demodulation, [], [f148, f308])).
fof(f827, plain, (~ spl3_9 | ~ spl3_25), inference(avatar_split_clause, [], [f824, f306, f238])).
fof(f824, plain, (~ (e0 = op(e3, e1)) | ~ spl3_25), inference(backward_demodulation, [], [f120, f308])).
fof(f822, plain, (~ spl3_21 | ~ spl3_29), inference(avatar_split_clause, [], [f818, f323, f289])).
fof(f818, plain, (~ (e0 = op(e2, e2)) | ~ spl3_29), inference(backward_demodulation, [], [f146, f325])).
fof(f805, plain, (~ spl3_37 | ~ spl3_61 | ~ spl3_68), inference(avatar_contradiction_clause, [], [f804])).
fof(f804, plain, ($false | (~ spl3_37 | ~ spl3_61 | ~ spl3_68)), inference(subsumption_resolution, [], [f803, f157])).
fof(f803, plain, ((e0 = e1) | (~ spl3_37 | ~ spl3_61 | ~ spl3_68)), inference(forward_demodulation, [], [f799, f461])).
fof(f799, plain, ((op(e0, e0) = e1) | (~ spl3_37 | ~ spl3_68)), inference(backward_demodulation, [], [f524, f359])).
fof(f800, plain, (~ spl3_21 | ~ spl3_37), inference(avatar_split_clause, [], [f796, f357, f289])).
fof(f796, plain, (~ (e0 = op(e2, e2)) | ~ spl3_37), inference(backward_demodulation, [], [f124, f359])).
fof(f788, plain, (~ spl3_25 | ~ spl3_41), inference(avatar_split_clause, [], [f781, f374, f306])).
fof(f781, plain, (~ (e0 = op(e2, e1)) | ~ spl3_41), inference(backward_demodulation, [], [f118, f376])).
fof(f780, plain, (~ spl3_33 | ~ spl3_45), inference(avatar_split_clause, [], [f775, f391, f340])).
fof(f775, plain, (~ (e0 = op(e1, e3)) | ~ spl3_45), inference(backward_demodulation, [], [f141, f393])).
fof(f718, plain, (~ spl3_107 | ~ spl3_22 | ~ spl3_97), inference(avatar_split_clause, [], [f202, f661, f293, f714])).
fof(f202, plain, (~ (e0 = op(e2, op(op(e2, e2), e2))) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2))), inference(cnf_transformation, [], [f53])).
fof(f53, plain, (~ (e0 = op(e2, op(op(e2, e2), e2))) | ~ (e1 = op(e2, e2)) | ~ (e3 = op(op(e2, e2), e2))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ~ ((e0 = op(e2, op(op(e2, e2), e2))) & (e1 = op(e2, e2)) & (e3 = op(op(e2, e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax29)).
fof(f712, plain, (~ spl3_106 | ~ spl3_43 | ~ spl3_103), inference(avatar_split_clause, [], [f200, f691, f382, f708])).
fof(f200, plain, (~ (e0 = op(e1, op(op(e1, e1), e1))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f51])).
fof(f51, plain, (~ (e0 = op(e1, op(op(e1, e1), e1))) | ~ (e2 = op(e1, e1)) | ~ (e3 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((e0 = op(e1, op(op(e1, e1), e1))) & (e2 = op(e1, e1)) & (e3 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax27)).
fof(f700, plain, (~ spl3_104 | ~ spl3_2 | ~ spl3_99), inference(avatar_split_clause, [], [f196, f671, f208, f696])).
fof(f196, plain, (~ (e0 = op(e3, op(op(e3, e3), e3))) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (~ (e0 = op(e3, op(op(e3, e3), e3))) | ~ (e1 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ((e0 = op(e3, op(op(e3, e3), e3))) & (e1 = op(e3, e3)) & (e2 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax23)).
fof(f699, plain, (~ spl3_104 | ~ spl3_1 | ~ spl3_92), inference(avatar_split_clause, [], [f195, f637, f204, f696])).
fof(f195, plain, (~ (e1 = op(e3, op(op(e3, e3), e3))) | ~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3))), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (~ (e1 = op(e3, op(op(e3, e3), e3))) | ~ (e0 = op(e3, e3)) | ~ (e2 = op(op(e3, e3), e3))), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ~ ((e1 = op(e3, op(op(e3, e3), e3))) & (e0 = op(e3, e3)) & (e2 = op(op(e3, e3), e3))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax22)).
fof(f679, plain, (~ spl3_100 | ~ spl3_62 | ~ spl3_94), inference(avatar_split_clause, [], [f191, f646, f463, f676])).
fof(f191, plain, (~ (e3 = op(e0, op(op(e0, e0), e0))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), e0))), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (~ (e3 = op(e0, op(op(e0, e0), e0))) | ~ (op(e0, e0) = e1) | ~ (e2 = op(op(e0, e0), e0))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ~ ((e3 = op(e0, op(op(e0, e0), e0))) & (op(e0, e0) = e1) & (e2 = op(op(e0, e0), e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax18)).
fof(f664, plain, (~ spl3_96 | ~ spl3_24 | ~ spl3_97), inference(avatar_split_clause, [], [f188, f661, f301, f656])).
fof(f188, plain, (~ (e0 = op(e2, op(op(e2, e2), e2))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2))), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (~ (e0 = op(e2, op(op(e2, e2), e2))) | ~ (e3 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ~ ((e0 = op(e2, op(op(e2, e2), e2))) & (e3 = op(e2, e2)) & (e1 = op(op(e2, e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax15)).
fof(f659, plain, (~ spl3_96 | ~ spl3_21 | ~ spl3_88), inference(avatar_split_clause, [], [f187, f618, f289, f656])).
fof(f187, plain, (~ (e3 = op(e2, op(op(e2, e2), e2))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2))), inference(cnf_transformation, [], [f38])).
fof(f38, plain, (~ (e3 = op(e2, op(op(e2, e2), e2))) | ~ (e0 = op(e2, e2)) | ~ (e1 = op(op(e2, e2), e2))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ~ ((e3 = op(e2, op(op(e2, e2), e2))) & (e0 = op(e2, e2)) & (e1 = op(op(e2, e2), e2))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax14)).
fof(f612, plain, (~ spl3_84 | ~ spl3_44 | ~ spl3_86), inference(avatar_split_clause, [], [f180, f609, f386, f600])).
fof(f180, plain, (~ (e2 = op(e1, op(op(e1, e1), e1))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, (~ (e2 = op(e1, op(op(e1, e1), e1))) | ~ (e3 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((e2 = op(e1, op(op(e1, e1), e1))) & (e3 = op(e1, e1)) & (e0 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax7)).
fof(f607, plain, (~ spl3_84 | ~ spl3_43 | ~ spl3_85), inference(avatar_split_clause, [], [f179, f604, f382, f600])).
fof(f179, plain, (~ (e3 = op(e1, op(op(e1, e1), e1))) | ~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1))), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (~ (e3 = op(e1, op(op(e1, e1), e1))) | ~ (e2 = op(e1, e1)) | ~ (e0 = op(op(e1, e1), e1))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ~ ((e3 = op(e1, op(op(e1, e1), e1))) & (e2 = op(e1, e1)) & (e0 = op(op(e1, e1), e1))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax6)).
fof(f598, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_83), inference(avatar_split_clause, [], [f175, f595, f508, f532, f556])).
fof(f556, plain, (spl3_75 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_75])])).
fof(f532, plain, (spl3_70 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_70])])).
fof(f508, plain, (spl3_65 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_65])])).
fof(f175, plain, ((e0 = op(op(e0, e3), op(e0, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f57, plain, (((e3 = op(op(e3, e3), op(e3, e3))) & (e2 = op(op(e2, e3), op(e2, e3))) & (e1 = op(op(e1, e3), op(e1, e3))) & (e0 = op(op(e0, e3), op(e0, e3)))) | sP2 | sP1 | sP0), inference(definition_folding, [], [f5, e56, e55, e54])).
fof(f54, plain, (((e3 = op(op(e3, e0), op(e3, e0))) & (e2 = op(op(e2, e0), op(e2, e0))) & (e1 = op(op(e1, e0), op(e1, e0))) & (e0 = op(op(e0, e0), op(e0, e0)))) | ~ sP0), inference(usedef, [], [e54])).
fof(e54, plain, (sP0 <=> ((e3 = op(op(e3, e0), op(e3, e0))) & (e2 = op(op(e2, e0), op(e2, e0))) & (e1 = op(op(e1, e0), op(e1, e0))) & (e0 = op(op(e0, e0), op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f55, plain, (((e3 = op(op(e3, e1), op(e3, e1))) & (e2 = op(op(e2, e1), op(e2, e1))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e0, e1), op(e0, e1)))) | ~ sP1), inference(usedef, [], [e55])).
fof(e55, plain, (sP1 <=> ((e3 = op(op(e3, e1), op(e3, e1))) & (e2 = op(op(e2, e1), op(e2, e1))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e0, e1), op(e0, e1))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f56, plain, (((e3 = op(op(e3, e2), op(e3, e2))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e1, e2), op(e1, e2))) & (e0 = op(op(e0, e2), op(e0, e2)))) | ~ sP2), inference(usedef, [], [e56])).
fof(e56, plain, (sP2 <=> ((e3 = op(op(e3, e2), op(e3, e2))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e1, e2), op(e1, e2))) & (e0 = op(op(e0, e2), op(e0, e2))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f5, plain, (((e3 = op(op(e3, e3), op(e3, e3))) & (e2 = op(op(e2, e3), op(e2, e3))) & (e1 = op(op(e1, e3), op(e1, e3))) & (e0 = op(op(e0, e3), op(e0, e3)))) | ((e3 = op(op(e3, e2), op(e3, e2))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e1, e2), op(e1, e2))) & (e0 = op(op(e0, e2), op(e0, e2)))) | ((e3 = op(op(e3, e1), op(e3, e1))) & (e2 = op(op(e2, e1), op(e2, e1))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e0, e1), op(e0, e1)))) | ((e3 = op(op(e3, e0), op(e3, e0))) & (e2 = op(op(e2, e0), op(e2, e0))) & (e1 = op(op(e1, e0), op(e1, e0))) & (e0 = op(op(e0, e0), op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax5)).
fof(f593, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_82), inference(avatar_split_clause, [], [f176, f590, f508, f532, f556])).
fof(f176, plain, ((e1 = op(op(e1, e3), op(e1, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f588, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_81), inference(avatar_split_clause, [], [f177, f585, f508, f532, f556])).
fof(f177, plain, ((e2 = op(op(e2, e3), op(e2, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f583, plain, (spl3_75 | spl3_70 | spl3_65 | spl3_80), inference(avatar_split_clause, [], [f178, f580, f508, f532, f556])).
fof(f178, plain, ((e3 = op(op(e3, e3), op(e3, e3))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f57])).
fof(f578, plain, (~ spl3_75 | spl3_79), inference(avatar_split_clause, [], [f171, f575, f556])).
fof(f171, plain, ((e0 = op(op(e0, e0), op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f60, plain, (((e3 = op(op(e3, e0), op(e3, e0))) & (e2 = op(op(e2, e0), op(e2, e0))) & (e1 = op(op(e1, e0), op(e1, e0))) & (e0 = op(op(e0, e0), op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f54])).
fof(f573, plain, (~ spl3_75 | spl3_78), inference(avatar_split_clause, [], [f172, f570, f556])).
fof(f172, plain, ((e1 = op(op(e1, e0), op(e1, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f568, plain, (~ spl3_75 | spl3_77), inference(avatar_split_clause, [], [f173, f565, f556])).
fof(f173, plain, ((e2 = op(op(e2, e0), op(e2, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f563, plain, (~ spl3_75 | spl3_76), inference(avatar_split_clause, [], [f174, f560, f556])).
fof(f174, plain, ((e3 = op(op(e3, e0), op(e3, e0))) | ~ sP0), inference(cnf_transformation, [], [f60])).
fof(f554, plain, (~ spl3_70 | spl3_74), inference(avatar_split_clause, [], [f167, f551, f532])).
fof(f167, plain, ((e0 = op(op(e0, e1), op(e0, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f59, plain, (((e3 = op(op(e3, e1), op(e3, e1))) & (e2 = op(op(e2, e1), op(e2, e1))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e0 = op(op(e0, e1), op(e0, e1)))) | ~ sP1), inference(nnf_transformation, [], [f55])).
fof(f549, plain, (~ spl3_70 | spl3_73), inference(avatar_split_clause, [], [f168, f546, f532])).
fof(f168, plain, ((e1 = op(op(e1, e1), op(e1, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f544, plain, (~ spl3_70 | spl3_72), inference(avatar_split_clause, [], [f169, f541, f532])).
fof(f169, plain, ((e2 = op(op(e2, e1), op(e2, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f539, plain, (~ spl3_70 | spl3_71), inference(avatar_split_clause, [], [f170, f536, f532])).
fof(f170, plain, ((e3 = op(op(e3, e1), op(e3, e1))) | ~ sP1), inference(cnf_transformation, [], [f59])).
fof(f530, plain, (~ spl3_65 | spl3_69), inference(avatar_split_clause, [], [f163, f527, f508])).
fof(f163, plain, ((e0 = op(op(e0, e2), op(e0, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((e3 = op(op(e3, e2), op(e3, e2))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e1 = op(op(e1, e2), op(e1, e2))) & (e0 = op(op(e0, e2), op(e0, e2)))) | ~ sP2), inference(nnf_transformation, [], [f56])).
fof(f525, plain, (~ spl3_65 | spl3_68), inference(avatar_split_clause, [], [f164, f522, f508])).
fof(f164, plain, ((e1 = op(op(e1, e2), op(e1, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f520, plain, (~ spl3_65 | spl3_67), inference(avatar_split_clause, [], [f165, f517, f508])).
fof(f165, plain, ((e2 = op(op(e2, e2), op(e2, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f515, plain, (~ spl3_65 | spl3_66), inference(avatar_split_clause, [], [f166, f512, f508])).
fof(f166, plain, ((e3 = op(op(e3, e2), op(e3, e2))) | ~ sP2), inference(cnf_transformation, [], [f58])).
fof(f506, plain, (spl3_61 | spl3_57 | spl3_53 | spl3_49), inference(avatar_split_clause, [], [f77, f408, f425, f442, f459])).
fof(f77, plain, ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax2)).
fof(f505, plain, (spl3_61 | spl3_45 | spl3_29 | spl3_13), inference(avatar_split_clause, [], [f78, f255, f323, f391, f459])).
fof(f78, plain, ((e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f503, plain, (spl3_62 | spl3_46 | spl3_30 | spl3_14), inference(avatar_split_clause, [], [f80, f259, f327, f395, f463])).
fof(f80, plain, ((e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f2])).
fof(f502, plain, (spl3_63 | spl3_59 | spl3_55 | spl3_51), inference(avatar_split_clause, [], [f81, f416, f433, f450, f467])).
fof(f81, plain, ((e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f501, plain, (spl3_63 | spl3_47 | spl3_31 | spl3_15), inference(avatar_split_clause, [], [f82, f263, f331, f399, f467])).
fof(f82, plain, ((e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f2])).
fof(f500, plain, (spl3_64 | spl3_60 | spl3_56 | spl3_52), inference(avatar_split_clause, [], [f83, f420, f437, f454, f471])).
fof(f83, plain, ((e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f499, plain, (spl3_64 | spl3_48 | spl3_32 | spl3_16), inference(avatar_split_clause, [], [f84, f267, f335, f403, f471])).
fof(f84, plain, ((e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f2])).
fof(f498, plain, (spl3_45 | spl3_41 | spl3_37 | spl3_33), inference(avatar_split_clause, [], [f85, f340, f357, f374, f391])).
fof(f85, plain, ((e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f497, plain, (spl3_57 | spl3_41 | spl3_25 | spl3_9), inference(avatar_split_clause, [], [f86, f238, f306, f374, f442])).
fof(f86, plain, ((e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f493, plain, (spl3_59 | spl3_43 | spl3_27 | spl3_11), inference(avatar_split_clause, [], [f90, f246, f314, f382, f450])).
fof(f90, plain, ((e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f491, plain, (spl3_60 | spl3_44 | spl3_28 | spl3_12), inference(avatar_split_clause, [], [f92, f250, f318, f386, f454])).
fof(f92, plain, ((e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f490, plain, (spl3_29 | spl3_25 | spl3_21 | spl3_17), inference(avatar_split_clause, [], [f93, f272, f289, f306, f323])).
fof(f93, plain, ((e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f489, plain, (spl3_53 | spl3_37 | spl3_21 | spl3_5), inference(avatar_split_clause, [], [f94, f221, f289, f357, f425])).
fof(f94, plain, ((e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f487, plain, (spl3_54 | spl3_38 | spl3_22 | spl3_6), inference(avatar_split_clause, [], [f96, f225, f293, f361, f429])).
fof(f96, plain, ((e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f2])).
fof(f486, plain, (spl3_31 | spl3_27 | spl3_23 | spl3_19), inference(avatar_split_clause, [], [f97, f280, f297, f314, f331])).
fof(f97, plain, ((e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f482, plain, (spl3_13 | spl3_9 | spl3_5 | spl3_1), inference(avatar_split_clause, [], [f101, f204, f221, f238, f255])).
fof(f101, plain, ((e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f481, plain, (spl3_49 | spl3_33 | spl3_17 | spl3_1), inference(avatar_split_clause, [], [f102, f204, f272, f340, f408])).
fof(f102, plain, ((e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f480, plain, (spl3_14 | spl3_10 | spl3_6 | spl3_2), inference(avatar_split_clause, [], [f103, f208, f225, f242, f259])).
fof(f103, plain, ((e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f479, plain, (spl3_50 | spl3_34 | spl3_18 | spl3_2), inference(avatar_split_clause, [], [f104, f208, f276, f344, f412])).
fof(f104, plain, ((e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f478, plain, (spl3_15 | spl3_11 | spl3_7 | spl3_3), inference(avatar_split_clause, [], [f105, f212, f229, f246, f263])).
fof(f105, plain, ((e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f2])).
fof(f474, plain, (spl3_61 | spl3_62 | spl3_63 | spl3_64), inference(avatar_split_clause, [], [f61, f471, f467, f463, f459])).
fof(f61, plain, ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG137+1.p', ax1)).
fof(f457, plain, (spl3_57 | spl3_58 | spl3_59 | spl3_60), inference(avatar_split_clause, [], [f62, f454, f450, f446, f442])).
fof(f62, plain, ((e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f440, plain, (spl3_53 | spl3_54 | spl3_55 | spl3_56), inference(avatar_split_clause, [], [f63, f437, f433, f429, f425])).
fof(f63, plain, ((e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f423, plain, (spl3_49 | spl3_50 | spl3_51 | spl3_52), inference(avatar_split_clause, [], [f64, f420, f416, f412, f408])).
fof(f64, plain, ((e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f406, plain, (spl3_45 | spl3_46 | spl3_47 | spl3_48), inference(avatar_split_clause, [], [f65, f403, f399, f395, f391])).
fof(f65, plain, ((e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f389, plain, (spl3_41 | spl3_42 | spl3_43 | spl3_44), inference(avatar_split_clause, [], [f66, f386, f382, f378, f374])).
fof(f66, plain, ((e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))), inference(cnf_transformation, [], [f1])).
fof(f372, plain, (spl3_37 | spl3_38 | spl3_39 | spl3_40), inference(avatar_split_clause, [], [f67, f369, f365, f361, f357])).
fof(f67, plain, ((e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f355, plain, (spl3_33 | spl3_34 | spl3_35 | spl3_36), inference(avatar_split_clause, [], [f68, f352, f348, f344, f340])).
fof(f68, plain, ((e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f338, plain, (spl3_29 | spl3_30 | spl3_31 | spl3_32), inference(avatar_split_clause, [], [f69, f335, f331, f327, f323])).
fof(f69, plain, ((e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f321, plain, (spl3_25 | spl3_26 | spl3_27 | spl3_28), inference(avatar_split_clause, [], [f70, f318, f314, f310, f306])).
fof(f70, plain, ((e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f304, plain, (spl3_21 | spl3_22 | spl3_23 | spl3_24), inference(avatar_split_clause, [], [f71, f301, f297, f293, f289])).
fof(f71, plain, ((e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f287, plain, (spl3_17 | spl3_18 | spl3_19 | spl3_20), inference(avatar_split_clause, [], [f72, f284, f280, f276, f272])).
fof(f72, plain, ((e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f253, plain, (spl3_9 | spl3_10 | spl3_11 | spl3_12), inference(avatar_split_clause, [], [f74, f250, f246, f242, f238])).
fof(f74, plain, ((e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).
fof(f236, plain, (spl3_5 | spl3_6 | spl3_7 | spl3_8), inference(avatar_split_clause, [], [f75, f233, f229, f225, f221])).
fof(f75, plain, ((e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).