fof(f6848, plain, $false, inference(avatar_sat_refutation, [], [f664, f681, f698, f715, f732, f749, f766, f800, f817, f889, f890, f891, f896, f897, f898, f900, f902, f904, f906, f907, f909, f911, f912, f951, f968, f985, f1002, f1053, f1070, f1104, f1121, f1155, f1172, f1194, f1195, f1199, f1200, f1201, f1203, f1204, f1205, f1206, f1208, f1209, f1210, f1211, f1212, f1213, f1214, f1215, f1221, f1231, f1241, f1250, f1260, f1267, f1271, f1277, f1281, f1290, f1298, f1300, f1307, f1311, f1318, f1321, f1330, f1341, f1351, f1356, f1357, f1371, f1372, f1374, f1377, f1394, f1411, f1428, f1445, f1455, f1465, f1474, f1484, f1491, f1493, f1495, f1500, f1501, f1502, f1504, f1505, f1514, f1515, f1522, f1524, f1525, f1531, f1534, f1535, f1542, f1543, f1545, f1554, f1565, f1575, f1582, f1584, f1595, f1596, f1598, f1601, f1618, f1635, f1652, f1669, f1670, f1671, f1722, f1939, f2347, f2369, f2373, f2378, f2400, f2410, f2414, f2429, f2433, f2443, f2452, f2474, f2475, f2478, f2488, f2493, f2494, f2508, f2558, f2572, f2585, f2588, f2592, f2594, f2596, f2601, f2612, f2622, f2636, f2695, f2714, f2718, f2727, f2735, f2755, f2800, f2805, f2832, f2835, f2849, f2852, f2860, f2893, f2901, f2904, f2908, f2917, f2919, f2935, f2938, f2941, f2974, f2976, f2988, f3007, f3012, f3018, f3026, f3029, f3032, f3057, f3066, f3071, f3087, f3091, f3093, f3107, f3112, f3117, f3134, f3185, f3204, f3234, f3249, f3250, f3258, f3259, f3301, f3302, f3306, f3326, f3336, f3344, f3354, f3364, f3369, f3373, f3412, f3424, f3430, f3446, f3472, f3489, f3515, f3517, f3519, f3534, f3549, f3568, f3573, f3577, f3581, f3585, f3590, f3662, f3687, f3693, f3695, f3698, f3709, f3720, f3730, f3750, f3785, f3791, f3817, f3830, f3849, f3850, f3855, f3886, f3999, f4054, f4115, f4211, f4219, f4342, f4380, f4394, f4496, f4512, f4586, f4614, f4654, f4720, f4737, f4740, f4856, f4879, f4887, f4910, f4987, f5009, f5013, f5021, f5039, f5051, f5053, f5087, f5118, f5121, f5149, f5189, f5197, f5202, f5209, f5338, f5384, f5394, f5449, f5472, f5484, f5493, f5495, f5553, f5556, f5593, f5599, f5602, f5621, f5706, f5712, f5715, f5722, f5787, f5788, f5804, f5813, f5834, f5837, f5844, f5870, f5936, f5955, f5958, f5988, f5999, f6028, f6045, f6096, f6139, f6164, f6181, f6189, f6190, f6213, f6316, f6339, f6345, f6406, f6407, f6455, f6467, f6617, f6627, f6635, f6640, f6683, f6689, f6732, f6749, f6771, f6772, f6786, f6817, f6829])).
fof(f6829, plain, (~ spl42_20 | ~ spl42_28), inference(avatar_split_clause, [], [f6824, f729, f695])).
fof(f695, plain, (spl42_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_20])])).
fof(f729, plain, (spl42_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_28])])).
fof(f6824, plain, (~ (e13 = op1(e12, e13)) | ~ spl42_28), inference(backward_demodulation, [], [f244, f731])).
fof(f731, plain, ((e13 = op1(e12, e11)) | ~ spl42_28), inference(avatar_component_clause, [], [f729])).
fof(f244, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax5)).
fof(f6817, plain, (~ spl42_23 | ~ spl42_31), inference(avatar_split_clause, [], [f6811, f742, f708])).
fof(f708, plain, (spl42_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_23])])).
fof(f742, plain, (spl42_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_31])])).
fof(f6811, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_31), inference(backward_demodulation, [], [f241, f744])).
fof(f744, plain, ((e12 = op1(e12, e10)) | ~ spl42_31), inference(avatar_component_clause, [], [f742])).
fof(f241, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f6786, plain, (~ spl42_34 | ~ spl42_42), inference(avatar_split_clause, [], [f6778, f789, f755])).
fof(f755, plain, (spl42_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_34])])).
fof(f789, plain, (spl42_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_42])])).
fof(f6778, plain, (~ (e11 = op1(e11, e13)) | ~ spl42_42), inference(backward_demodulation, [], [f238, f791])).
fof(f791, plain, ((e11 = op1(e11, e11)) | ~ spl42_42), inference(avatar_component_clause, [], [f789])).
fof(f238, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f6772, plain, (~ spl42_41 | ~ spl42_45), inference(avatar_split_clause, [], [f6761, f802, f785])).
fof(f785, plain, (spl42_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_41])])).
fof(f802, plain, (spl42_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_45])])).
fof(f6761, plain, (~ (e10 = op1(e11, e11)) | ~ spl42_45), inference(backward_demodulation, [], [f234, f804])).
fof(f804, plain, ((e10 = op1(e11, e10)) | ~ spl42_45), inference(avatar_component_clause, [], [f802])).
fof(f234, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f6771, plain, (~ spl42_29 | ~ spl42_45), inference(avatar_split_clause, [], [f6760, f802, f734])).
fof(f734, plain, (spl42_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_29])])).
fof(f6760, plain, (~ (e10 = op1(e12, e10)) | ~ spl42_45), inference(backward_demodulation, [], [f207, f804])).
fof(f207, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f6749, plain, (~ spl42_53 | ~ spl42_57), inference(avatar_split_clause, [], [f6739, f853, f836])).
fof(f836, plain, (spl42_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_53])])).
fof(f853, plain, (spl42_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_57])])).
fof(f6739, plain, (~ (e10 = op1(e10, e12)) | ~ spl42_57), inference(backward_demodulation, [], [f231, f855])).
fof(f855, plain, ((e10 = op1(e10, e11)) | ~ spl42_57), inference(avatar_component_clause, [], [f853])).
fof(f231, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f6732, plain, (~ spl42_87 | spl42_181), inference(avatar_contradiction_clause, [], [f6731])).
fof(f6731, plain, ($false | (~ spl42_87 | spl42_181)), inference(subsumption_resolution, [], [f6716, f1014])).
fof(f1014, plain, ((e22 = op2(e22, e22)) | ~ spl42_87), inference(avatar_component_clause, [], [f1012])).
fof(f1012, plain, (spl42_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_87])])).
fof(f6716, plain, (~ (e22 = op2(e22, e22)) | (~ spl42_87 | spl42_181)), inference(backward_demodulation, [], [f6109, f6709])).
fof(f6709, plain, ((e22 = h3(e10)) | ~ spl42_87), inference(backward_demodulation, [], [f543, f1014])).
fof(f543, plain, (op2(e22, e22) = h3(e10)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(op2(e22, e22), e22) = h3(e12)) & (h3(e11) = op2(op2(e22, e22), op2(e22, e22))) & (op2(e22, e22) = h3(e10)) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax16)).
fof(f6109, plain, (~ (h3(e10) = op2(h3(e10), e22)) | spl42_181), inference(backward_demodulation, [], [f1629, f543])).
fof(f1629, plain, (~ (op2(e22, e22) = op2(op2(e22, e22), e22)) | spl42_181), inference(avatar_component_clause, [], [f1628])).
fof(f1628, plain, (spl42_181 <=> (op2(e22, e22) = op2(op2(e22, e22), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_181])])).
fof(f6689, plain, (spl42_31 | ~ spl42_51 | ~ spl42_144), inference(avatar_split_clause, [], [f6688, f1379, f827, f742])).
fof(f827, plain, (spl42_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_51])])).
fof(f1379, plain, (spl42_144 <=> (op1(e10, e13) = op1(op1(e10, e13), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_144])])).
fof(f6688, plain, ((e12 = op1(e12, e10)) | (~ spl42_51 | ~ spl42_144)), inference(forward_demodulation, [], [f1381, f829])).
fof(f829, plain, ((e12 = op1(e10, e13)) | ~ spl42_51), inference(avatar_component_clause, [], [f827])).
fof(f1381, plain, ((op1(e10, e13) = op1(op1(e10, e13), e10)) | ~ spl42_144), inference(avatar_component_clause, [], [f1379])).
fof(f6683, plain, (~ spl42_82 | spl42_102 | ~ spl42_177), inference(avatar_contradiction_clause, [], [f6682])).
fof(f6682, plain, ($false | (~ spl42_82 | spl42_102 | ~ spl42_177)), inference(subsumption_resolution, [], [f6681, f1077])).
fof(f1077, plain, (~ (e21 = op2(e21, e22)) | spl42_102), inference(avatar_component_clause, [], [f1076])).
fof(f1076, plain, (spl42_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_102])])).
fof(f6681, plain, ((e21 = op2(e21, e22)) | (~ spl42_82 | ~ spl42_177)), inference(forward_demodulation, [], [f1613, f993])).
fof(f993, plain, ((e21 = op2(e22, e23)) | ~ spl42_82), inference(avatar_component_clause, [], [f991])).
fof(f991, plain, (spl42_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_82])])).
fof(f1613, plain, ((op2(e22, e23) = op2(op2(e22, e23), e22)) | ~ spl42_177), inference(avatar_component_clause, [], [f1611])).
fof(f1611, plain, (spl42_177 <=> (op2(e22, e23) = op2(op2(e22, e23), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_177])])).
fof(f6640, plain, (~ spl42_96 | ~ spl42_80), inference(avatar_split_clause, [], [f6639, f982, f1050])).
fof(f1050, plain, (spl42_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_96])])).
fof(f982, plain, (spl42_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_80])])).
fof(f6639, plain, (~ (e23 = op2(e22, e20)) | ~ spl42_80), inference(forward_demodulation, [], [f257, f984])).
fof(f984, plain, ((e23 = op2(e23, e20)) | ~ spl42_80), inference(avatar_component_clause, [], [f982])).
fof(f257, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax6)).
fof(f6635, plain, (~ spl42_6 | ~ spl42_20 | ~ spl42_146), inference(avatar_contradiction_clause, [], [f6634])).
fof(f6634, plain, ($false | (~ spl42_6 | ~ spl42_20 | ~ spl42_146)), inference(subsumption_resolution, [], [f6633, f304])).
fof(f304, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax7)).
fof(f6633, plain, ((e11 = e13) | (~ spl42_6 | ~ spl42_20 | ~ spl42_146)), inference(forward_demodulation, [], [f6630, f638])).
fof(f638, plain, ((e11 = op1(e13, e12)) | ~ spl42_6), inference(avatar_component_clause, [], [f636])).
fof(f636, plain, (spl42_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_6])])).
fof(f6630, plain, ((e13 = op1(e13, e12)) | (~ spl42_20 | ~ spl42_146)), inference(backward_demodulation, [], [f1389, f697])).
fof(f697, plain, ((e13 = op1(e12, e13)) | ~ spl42_20), inference(avatar_component_clause, [], [f695])).
fof(f1389, plain, ((op1(e12, e13) = op1(op1(e12, e13), e12)) | ~ spl42_146), inference(avatar_component_clause, [], [f1387])).
fof(f1387, plain, (spl42_146 <=> (op1(e12, e13) = op1(op1(e12, e13), e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_146])])).
fof(f6627, plain, (~ spl42_18 | ~ spl42_26), inference(avatar_split_clause, [], [f6620, f721, f687])).
fof(f687, plain, (spl42_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_18])])).
fof(f721, plain, (spl42_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_26])])).
fof(f6620, plain, (~ (e11 = op1(e12, e13)) | ~ spl42_26), inference(backward_demodulation, [], [f244, f723])).
fof(f723, plain, ((e11 = op1(e12, e11)) | ~ spl42_26), inference(avatar_component_clause, [], [f721])).
fof(f6617, plain, (~ spl42_18 | ~ spl42_34), inference(avatar_split_clause, [], [f6611, f755, f687])).
fof(f6611, plain, (~ (e11 = op1(e12, e13)) | ~ spl42_34), inference(backward_demodulation, [], [f225, f757])).
fof(f757, plain, ((e11 = op1(e11, e13)) | ~ spl42_34), inference(avatar_component_clause, [], [f755])).
fof(f225, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f6467, plain, (~ spl42_21 | ~ spl42_29), inference(avatar_split_clause, [], [f6459, f734, f700])).
fof(f700, plain, (spl42_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_21])])).
fof(f6459, plain, (~ (e10 = op1(e12, e12)) | ~ spl42_29), inference(backward_demodulation, [], [f241, f736])).
fof(f736, plain, ((e10 = op1(e12, e10)) | ~ spl42_29), inference(avatar_component_clause, [], [f734])).
fof(f6455, plain, (~ spl42_11 | ~ spl42_36 | ~ spl42_145), inference(avatar_contradiction_clause, [], [f6454])).
fof(f6454, plain, ($false | (~ spl42_11 | ~ spl42_36 | ~ spl42_145)), inference(subsumption_resolution, [], [f6453, f305])).
fof(f305, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f6453, plain, ((e12 = e13) | (~ spl42_11 | ~ spl42_36 | ~ spl42_145)), inference(forward_demodulation, [], [f6448, f659])).
fof(f659, plain, ((e12 = op1(e13, e11)) | ~ spl42_11), inference(avatar_component_clause, [], [f657])).
fof(f657, plain, (spl42_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_11])])).
fof(f6448, plain, ((e13 = op1(e13, e11)) | (~ spl42_36 | ~ spl42_145)), inference(backward_demodulation, [], [f1385, f765])).
fof(f765, plain, ((e13 = op1(e11, e13)) | ~ spl42_36), inference(avatar_component_clause, [], [f763])).
fof(f763, plain, (spl42_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_36])])).
fof(f1385, plain, ((op1(e11, e13) = op1(op1(e11, e13), e11)) | ~ spl42_145), inference(avatar_component_clause, [], [f1383])).
fof(f1383, plain, (spl42_145 <=> (op1(e11, e13) = op1(op1(e11, e13), e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_145])])).
fof(f6407, plain, (~ spl42_39 | ~ spl42_47), inference(avatar_split_clause, [], [f6399, f810, f776])).
fof(f776, plain, (spl42_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_39])])).
fof(f810, plain, (spl42_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_47])])).
fof(f6399, plain, (~ (e12 = op1(e11, e12)) | ~ spl42_47), inference(backward_demodulation, [], [f235, f812])).
fof(f812, plain, ((e12 = op1(e11, e10)) | ~ spl42_47), inference(avatar_component_clause, [], [f810])).
fof(f235, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f6406, plain, (~ spl42_31 | ~ spl42_47), inference(avatar_split_clause, [], [f6397, f810, f742])).
fof(f6397, plain, (~ (e12 = op1(e12, e10)) | ~ spl42_47), inference(backward_demodulation, [], [f207, f812])).
fof(f6345, plain, (~ spl42_26 | spl42_38 | ~ spl42_154), inference(avatar_contradiction_clause, [], [f6344])).
fof(f6344, plain, ($false | (~ spl42_26 | spl42_38 | ~ spl42_154)), inference(subsumption_resolution, [], [f6343, f773])).
fof(f773, plain, (~ (e11 = op1(e11, e12)) | spl42_38), inference(avatar_component_clause, [], [f772])).
fof(f772, plain, (spl42_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_38])])).
fof(f6343, plain, ((e11 = op1(e11, e12)) | (~ spl42_26 | ~ spl42_154)), inference(forward_demodulation, [], [f1423, f723])).
fof(f1423, plain, ((op1(e12, e11) = op1(op1(e12, e11), e12)) | ~ spl42_154), inference(avatar_component_clause, [], [f1421])).
fof(f1421, plain, (spl42_154 <=> (op1(e12, e11) = op1(op1(e12, e11), e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_154])])).
fof(f6339, plain, (~ spl42_11 | spl42_19 | ~ spl42_155), inference(avatar_contradiction_clause, [], [f6338])).
fof(f6338, plain, ($false | (~ spl42_11 | spl42_19 | ~ spl42_155)), inference(subsumption_resolution, [], [f6337, f692])).
fof(f692, plain, (~ (e12 = op1(e12, e13)) | spl42_19), inference(avatar_component_clause, [], [f691])).
fof(f691, plain, (spl42_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_19])])).
fof(f6337, plain, ((e12 = op1(e12, e13)) | (~ spl42_11 | ~ spl42_155)), inference(forward_demodulation, [], [f1427, f659])).
fof(f1427, plain, ((op1(e13, e11) = op1(op1(e13, e11), e13)) | ~ spl42_155), inference(avatar_component_clause, [], [f1425])).
fof(f1425, plain, (spl42_155 <=> (op1(e13, e11) = op1(op1(e13, e11), e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_155])])).
fof(f6316, plain, (~ spl42_11 | ~ spl42_44 | ~ spl42_153), inference(avatar_contradiction_clause, [], [f6315])).
fof(f6315, plain, ($false | (~ spl42_11 | ~ spl42_44 | ~ spl42_153)), inference(subsumption_resolution, [], [f6314, f305])).
fof(f6314, plain, ((e12 = e13) | (~ spl42_11 | ~ spl42_44 | ~ spl42_153)), inference(forward_demodulation, [], [f6313, f659])).
fof(f6313, plain, ((e13 = op1(e13, e11)) | (~ spl42_44 | ~ spl42_153)), inference(forward_demodulation, [], [f1419, f799])).
fof(f799, plain, ((e13 = op1(e11, e11)) | ~ spl42_44), inference(avatar_component_clause, [], [f797])).
fof(f797, plain, (spl42_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_44])])).
fof(f1419, plain, ((op1(e11, e11) = op1(op1(e11, e11), e11)) | ~ spl42_153), inference(avatar_component_clause, [], [f1417])).
fof(f1417, plain, (spl42_153 <=> (op1(e11, e11) = op1(op1(e11, e11), e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_153])])).
fof(f6213, plain, (~ spl42_39 | ~ spl42_103 | spl42_226), inference(avatar_contradiction_clause, [], [f6212])).
fof(f6212, plain, ($false | (~ spl42_39 | ~ spl42_103 | spl42_226)), inference(subsumption_resolution, [], [f6211, f1082])).
fof(f1082, plain, ((e22 = op2(e21, e22)) | ~ spl42_103), inference(avatar_component_clause, [], [f1080])).
fof(f1080, plain, (spl42_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_103])])).
fof(f6211, plain, (~ (e22 = op2(e21, e22)) | (~ spl42_39 | spl42_226)), inference(forward_demodulation, [], [f6210, f1696])).
fof(f1696, plain, (e22 = h4(e12)), inference(forward_demodulation, [], [f549, f533])).
fof(f533, plain, (e22 = op2(op2(e23, e23), e23)), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(op2(e23, e23), e23)) & (e21 = op2(op2(e23, e23), op2(e23, e23))) & (e20 = op2(e23, e23))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax13)).
fof(f549, plain, (op2(op2(e23, e23), e23) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(op2(e23, e23), e23) = h4(e12)) & (op2(op2(e23, e23), op2(e23, e23)) = h4(e11)) & (op2(e23, e23) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax17)).
fof(f6210, plain, (~ (op2(e21, e22) = h4(e12)) | (~ spl42_39 | spl42_226)), inference(forward_demodulation, [], [f1914, f778])).
fof(f778, plain, ((e12 = op1(e11, e12)) | ~ spl42_39), inference(avatar_component_clause, [], [f776])).
fof(f1914, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | spl42_226), inference(avatar_component_clause, [], [f1912])).
fof(f1912, plain, (spl42_226 <=> (op2(e21, e22) = h4(op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_226])])).
fof(f6190, plain, (~ spl42_117 | ~ spl42_121), inference(avatar_split_clause, [], [f6186, f1157, f1140])).
fof(f1140, plain, (spl42_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_117])])).
fof(f1157, plain, (spl42_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_121])])).
fof(f6186, plain, (~ (e20 = op2(e20, e22)) | ~ spl42_121), inference(backward_demodulation, [], [f279, f1159])).
fof(f1159, plain, ((e20 = op2(e20, e21)) | ~ spl42_121), inference(avatar_component_clause, [], [f1157])).
fof(f279, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f6189, plain, (~ spl42_89 | ~ spl42_121), inference(avatar_split_clause, [], [f6185, f1157, f1021])).
fof(f1021, plain, (spl42_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_89])])).
fof(f6185, plain, (~ (e20 = op2(e22, e21)) | ~ spl42_121), inference(backward_demodulation, [], [f259, f1159])).
fof(f259, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f6181, plain, (spl42_121 | ~ spl42_109 | ~ spl42_188), inference(avatar_split_clause, [], [f6180, f1658, f1106, f1157])).
fof(f1106, plain, (spl42_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_109])])).
fof(f1658, plain, (spl42_188 <=> (op2(e21, e20) = op2(op2(e21, e20), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_188])])).
fof(f6180, plain, ((e20 = op2(e20, e21)) | (~ spl42_109 | ~ spl42_188)), inference(forward_demodulation, [], [f1660, f1108])).
fof(f1108, plain, ((e20 = op2(e21, e20)) | ~ spl42_109), inference(avatar_component_clause, [], [f1106])).
fof(f1660, plain, ((op2(e21, e20) = op2(op2(e21, e20), e21)) | ~ spl42_188), inference(avatar_component_clause, [], [f1658])).
fof(f6164, plain, (~ spl42_117 | ~ spl42_200), inference(avatar_split_clause, [], [f6151, f1760, f1140])).
fof(f1760, plain, (spl42_200 <=> (e20 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_200])])).
fof(f6151, plain, (~ (e20 = op2(e20, e22)) | ~ spl42_200), inference(backward_demodulation, [], [f1688, f1761])).
fof(f1761, plain, ((e20 = h3(e10)) | ~ spl42_200), inference(avatar_component_clause, [], [f1760])).
fof(f1688, plain, ~ (op2(e20, e22) = h3(e10)), inference(backward_demodulation, [], [f265, f543])).
fof(f265, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f6139, plain, (~ spl42_121 | ~ spl42_57 | ~ spl42_192 | spl42_231), inference(avatar_split_clause, [], [f6138, f1932, f1719, f853, f1157])).
fof(f1719, plain, (spl42_192 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_192])])).
fof(f1932, plain, (spl42_231 <=> (h4(op1(e10, e11)) = op2(h4(e10), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_231])])).
fof(f6138, plain, (~ (e20 = op2(e20, e21)) | (~ spl42_57 | ~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f6137, f1720])).
fof(f1720, plain, ((e20 = h4(e10)) | ~ spl42_192), inference(avatar_component_clause, [], [f1719])).
fof(f6137, plain, (~ (op2(e20, e21) = h4(e10)) | (~ spl42_57 | ~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f6136, f855])).
fof(f6136, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | (~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f1934, f1720])).
fof(f1934, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e21)) | spl42_231), inference(avatar_component_clause, [], [f1932])).
fof(f6096, plain, (~ spl42_65 | ~ spl42_80 | ~ spl42_190), inference(avatar_contradiction_clause, [], [f6095])).
fof(f6095, plain, ($false | (~ spl42_65 | ~ spl42_80 | ~ spl42_190)), inference(subsumption_resolution, [], [f6094, f308])).
fof(f308, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax8)).
fof(f6094, plain, ((e20 = e23) | (~ spl42_65 | ~ spl42_80 | ~ spl42_190)), inference(forward_demodulation, [], [f6093, f921])).
fof(f921, plain, ((e20 = op2(e23, e23)) | ~ spl42_65), inference(avatar_component_clause, [], [f919])).
fof(f919, plain, (spl42_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_65])])).
fof(f6093, plain, ((e23 = op2(e23, e23)) | (~ spl42_80 | ~ spl42_190)), inference(forward_demodulation, [], [f1668, f984])).
fof(f1668, plain, ((op2(e23, e20) = op2(op2(e23, e20), e23)) | ~ spl42_190), inference(avatar_component_clause, [], [f1666])).
fof(f1666, plain, (spl42_190 <=> (op2(e23, e20) = op2(op2(e23, e20), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_190])])).
fof(f6045, plain, (~ spl42_72 | ~ spl42_80), inference(avatar_split_clause, [], [f6044, f982, f948])).
fof(f948, plain, (spl42_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_72])])).
fof(f6044, plain, (~ (e23 = op2(e23, e22)) | ~ spl42_80), inference(backward_demodulation, [], [f295, f984])).
fof(f295, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f6028, plain, (~ spl42_92 | ~ spl42_124), inference(avatar_split_clause, [], [f6025, f1169, f1033])).
fof(f1033, plain, (spl42_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_92])])).
fof(f1169, plain, (spl42_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_124])])).
fof(f6025, plain, (~ (e23 = op2(e22, e21)) | ~ spl42_124), inference(backward_demodulation, [], [f259, f1171])).
fof(f1171, plain, ((e23 = op2(e20, e21)) | ~ spl42_124), inference(avatar_component_clause, [], [f1169])).
fof(f5999, plain, (spl42_87 | ~ spl42_95 | ~ spl42_189), inference(avatar_contradiction_clause, [], [f5998])).
fof(f5998, plain, ($false | (spl42_87 | ~ spl42_95 | ~ spl42_189)), inference(subsumption_resolution, [], [f5997, f1013])).
fof(f1013, plain, (~ (e22 = op2(e22, e22)) | spl42_87), inference(avatar_component_clause, [], [f1012])).
fof(f5997, plain, ((e22 = op2(e22, e22)) | (~ spl42_95 | ~ spl42_189)), inference(forward_demodulation, [], [f1664, f1048])).
fof(f1048, plain, ((e22 = op2(e22, e20)) | ~ spl42_95), inference(avatar_component_clause, [], [f1046])).
fof(f1046, plain, (spl42_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_95])])).
fof(f1664, plain, ((op2(e22, e20) = op2(op2(e22, e20), e22)) | ~ spl42_189), inference(avatar_component_clause, [], [f1662])).
fof(f1662, plain, (spl42_189 <=> (op2(e22, e20) = op2(op2(e22, e20), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_189])])).
fof(f5988, plain, (spl42_108 | ~ spl42_215 | ~ spl42_216 | ~ spl42_261), inference(avatar_contradiction_clause, [], [f5987])).
fof(f5987, plain, ($false | (spl42_108 | ~ spl42_215 | ~ spl42_216 | ~ spl42_261)), inference(subsumption_resolution, [], [f5977, f1102])).
fof(f1102, plain, (~ (e23 = op2(e21, e21)) | spl42_108), inference(avatar_component_clause, [], [f1101])).
fof(f1101, plain, (spl42_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_108])])).
fof(f5977, plain, ((e23 = op2(e21, e21)) | (~ spl42_215 | ~ spl42_216 | ~ spl42_261)), inference(backward_demodulation, [], [f5886, f1837])).
fof(f1837, plain, ((e21 = h1(e11)) | ~ spl42_215), inference(avatar_component_clause, [], [f1836])).
fof(f1836, plain, (spl42_215 <=> (e21 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_215])])).
fof(f5886, plain, ((e23 = op2(h1(e11), e21)) | (~ spl42_216 | ~ spl42_261)), inference(forward_demodulation, [], [f3343, f2140])).
fof(f2140, plain, ((e23 = h2(e12)) | ~ spl42_261), inference(avatar_component_clause, [], [f2139])).
fof(f2139, plain, (spl42_261 <=> (e23 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_261])])).
fof(f3343, plain, ((h2(e12) = op2(h1(e11), e21)) | ~ spl42_216), inference(backward_demodulation, [], [f1687, f3337])).
fof(f3337, plain, ((h1(e11) = h2(e10)) | ~ spl42_216), inference(backward_demodulation, [], [f539, f3331])).
fof(f3331, plain, ((op2(e21, e21) = h1(e11)) | ~ spl42_216), inference(backward_demodulation, [], [f1678, f1842])).
fof(f1842, plain, ((e21 = h1(e10)) | ~ spl42_216), inference(avatar_component_clause, [], [f1841])).
fof(f1841, plain, (spl42_216 <=> (e21 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_216])])).
fof(f1678, plain, (h1(e11) = op2(h1(e10), h1(e10))), inference(backward_demodulation, [], [f536, f535])).
fof(f535, plain, (op2(e20, e20) = h1(e10)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(op2(e20, e20), e20) = h1(e12)) & (h1(e11) = op2(op2(e20, e20), op2(e20, e20))) & (op2(e20, e20) = h1(e10)) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax14)).
fof(f536, plain, (h1(e11) = op2(op2(e20, e20), op2(e20, e20))), inference(cnf_transformation, [], [f14])).
fof(f539, plain, (op2(e21, e21) = h2(e10)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(op2(e21, e21), e21) = h2(e12)) & (h2(e11) = op2(op2(e21, e21), op2(e21, e21))) & (op2(e21, e21) = h2(e10)) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax15)).
fof(f1687, plain, (h2(e12) = op2(h2(e10), e21)), inference(backward_demodulation, [], [f541, f539])).
fof(f541, plain, (op2(op2(e21, e21), e21) = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f5958, plain, (~ spl42_200 | ~ spl42_21 | ~ spl42_192 | spl42_222), inference(avatar_split_clause, [], [f5957, f1896, f1719, f700, f1760])).
fof(f1896, plain, (spl42_222 <=> (h3(e10) = h4(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_222])])).
fof(f5957, plain, (~ (e20 = h3(e10)) | (~ spl42_21 | ~ spl42_192 | spl42_222)), inference(forward_demodulation, [], [f5956, f1720])).
fof(f5956, plain, (~ (h3(e10) = h4(e10)) | (~ spl42_21 | spl42_222)), inference(forward_demodulation, [], [f1898, f702])).
fof(f702, plain, ((e10 = op1(e12, e12)) | ~ spl42_21), inference(avatar_component_clause, [], [f700])).
fof(f1898, plain, (~ (h3(e10) = h4(op1(e12, e12))) | spl42_222), inference(avatar_component_clause, [], [f1896])).
fof(f5955, plain, (~ spl42_92 | ~ spl42_28 | spl42_223), inference(avatar_split_clause, [], [f5954, f1900, f729, f1033])).
fof(f1900, plain, (spl42_223 <=> (op2(e22, e21) = h4(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_223])])).
fof(f5954, plain, (~ (e23 = op2(e22, e21)) | (~ spl42_28 | spl42_223)), inference(forward_demodulation, [], [f5953, f546])).
fof(f546, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f5953, plain, (~ (op2(e22, e21) = h4(e13)) | (~ spl42_28 | spl42_223)), inference(forward_demodulation, [], [f1902, f731])).
fof(f1902, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | spl42_223), inference(avatar_component_clause, [], [f1900])).
fof(f5936, plain, (spl42_72 | ~ spl42_181 | ~ spl42_252), inference(avatar_split_clause, [], [f5927, f2089, f1628, f948])).
fof(f2089, plain, (spl42_252 <=> (e23 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_252])])).
fof(f5927, plain, ((e23 = op2(e23, e22)) | (~ spl42_181 | ~ spl42_252)), inference(backward_demodulation, [], [f5875, f2090])).
fof(f2090, plain, ((e23 = h3(e10)) | ~ spl42_252), inference(avatar_component_clause, [], [f2089])).
fof(f5875, plain, ((h3(e10) = op2(h3(e10), e22)) | ~ spl42_181), inference(backward_demodulation, [], [f1630, f543])).
fof(f1630, plain, ((op2(e22, e22) = op2(op2(e22, e22), e22)) | ~ spl42_181), inference(avatar_component_clause, [], [f1628])).
fof(f5870, plain, (~ spl42_289 | ~ spl42_100 | ~ spl42_216), inference(avatar_split_clause, [], [f5869, f1841, f1067, f2301])).
fof(f2301, plain, (spl42_289 <=> (e23 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_289])])).
fof(f1067, plain, (spl42_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_100])])).
fof(f5869, plain, (~ (e23 = h1(e11)) | (~ spl42_100 | ~ spl42_216)), inference(forward_demodulation, [], [f3442, f1069])).
fof(f1069, plain, ((e23 = op2(e21, e23)) | ~ spl42_100), inference(avatar_component_clause, [], [f1067])).
fof(f3442, plain, (~ (op2(e21, e23) = h1(e11)) | ~ spl42_216), inference(forward_demodulation, [], [f1685, f3337])).
fof(f1685, plain, ~ (op2(e21, e23) = h2(e10)), inference(backward_demodulation, [], [f286, f539])).
fof(f286, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f5844, plain, (~ spl42_85 | spl42_117 | ~ spl42_181), inference(avatar_contradiction_clause, [], [f5843])).
fof(f5843, plain, ($false | (~ spl42_85 | spl42_117 | ~ spl42_181)), inference(subsumption_resolution, [], [f5839, f1141])).
fof(f1141, plain, (~ (e20 = op2(e20, e22)) | spl42_117), inference(avatar_component_clause, [], [f1140])).
fof(f5839, plain, ((e20 = op2(e20, e22)) | (~ spl42_85 | ~ spl42_181)), inference(backward_demodulation, [], [f1630, f1006])).
fof(f1006, plain, ((e20 = op2(e22, e22)) | ~ spl42_85), inference(avatar_component_clause, [], [f1004])).
fof(f1004, plain, (spl42_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_85])])).
fof(f5837, plain, (spl42_72 | ~ spl42_92 | ~ spl42_185), inference(avatar_split_clause, [], [f5836, f1645, f1033, f948])).
fof(f1645, plain, (spl42_185 <=> (op2(e22, e21) = op2(op2(e22, e21), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_185])])).
fof(f5836, plain, ((e23 = op2(e23, e22)) | (~ spl42_92 | ~ spl42_185)), inference(backward_demodulation, [], [f1647, f1035])).
fof(f1035, plain, ((e23 = op2(e22, e21)) | ~ spl42_92), inference(avatar_component_clause, [], [f1033])).
fof(f1647, plain, ((op2(e22, e21) = op2(op2(e22, e21), e22)) | ~ spl42_185), inference(avatar_component_clause, [], [f1645])).
fof(f5834, plain, (~ spl42_84 | ~ spl42_100), inference(avatar_split_clause, [], [f5831, f1067, f999])).
fof(f999, plain, (spl42_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_84])])).
fof(f5831, plain, (~ (e23 = op2(e22, e23)) | ~ spl42_100), inference(backward_demodulation, [], [f273, f1069])).
fof(f273, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f5813, plain, (~ spl42_126 | ~ spl42_128), inference(avatar_contradiction_clause, [], [f5812])).
fof(f5812, plain, ($false | (~ spl42_126 | ~ spl42_128)), inference(subsumption_resolution, [], [f5811, f310])).
fof(f310, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f5811, plain, ((e21 = e23) | (~ spl42_126 | ~ spl42_128)), inference(forward_demodulation, [], [f1188, f1180])).
fof(f1180, plain, ((op2(e20, e20) = e21) | ~ spl42_126), inference(avatar_component_clause, [], [f1178])).
fof(f1178, plain, (spl42_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl42_126])])).
fof(f1188, plain, ((op2(e20, e20) = e23) | ~ spl42_128), inference(avatar_component_clause, [], [f1186])).
fof(f1186, plain, (spl42_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl42_128])])).
fof(f5804, plain, (~ spl42_80 | ~ spl42_16 | ~ spl42_192 | spl42_220), inference(avatar_split_clause, [], [f5803, f1888, f1719, f678, f982])).
fof(f678, plain, (spl42_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_16])])).
fof(f1888, plain, (spl42_220 <=> (h4(op1(e13, e10)) = op2(e23, h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_220])])).
fof(f5803, plain, (~ (e23 = op2(e23, e20)) | (~ spl42_16 | ~ spl42_192 | spl42_220)), inference(forward_demodulation, [], [f5802, f546])).
fof(f5802, plain, (~ (op2(e23, e20) = h4(e13)) | (~ spl42_16 | ~ spl42_192 | spl42_220)), inference(forward_demodulation, [], [f5801, f680])).
fof(f680, plain, ((e13 = op1(e13, e10)) | ~ spl42_16), inference(avatar_component_clause, [], [f678])).
fof(f5801, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | (~ spl42_192 | spl42_220)), inference(forward_demodulation, [], [f1890, f1720])).
fof(f1890, plain, (~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | spl42_220), inference(avatar_component_clause, [], [f1888])).
fof(f5788, plain, (~ spl42_84 | ~ spl42_211 | ~ spl42_216 | ~ spl42_261), inference(avatar_split_clause, [], [f5745, f2139, f1841, f1816, f999])).
fof(f1816, plain, (spl42_211 <=> (e22 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_211])])).
fof(f5745, plain, (~ (e23 = op2(e22, e23)) | (~ spl42_211 | ~ spl42_216 | ~ spl42_261)), inference(backward_demodulation, [], [f5717, f2140])).
fof(f5717, plain, (~ (op2(e22, e23) = h2(e12)) | (~ spl42_211 | ~ spl42_216)), inference(backward_demodulation, [], [f292, f5610])).
fof(f5610, plain, ((op2(e22, e21) = h2(e12)) | (~ spl42_211 | ~ spl42_216)), inference(backward_demodulation, [], [f3343, f1817])).
fof(f1817, plain, ((e22 = h1(e11)) | ~ spl42_211), inference(avatar_component_clause, [], [f1816])).
fof(f292, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f5787, plain, (~ spl42_124 | ~ spl42_211 | ~ spl42_216 | ~ spl42_261), inference(avatar_split_clause, [], [f5744, f2139, f1841, f1816, f1169])).
fof(f5744, plain, (~ (e23 = op2(e20, e21)) | (~ spl42_211 | ~ spl42_216 | ~ spl42_261)), inference(backward_demodulation, [], [f5633, f2140])).
fof(f5633, plain, (~ (op2(e20, e21) = h2(e12)) | (~ spl42_211 | ~ spl42_216)), inference(backward_demodulation, [], [f259, f5610])).
fof(f5722, plain, (~ spl42_119 | ~ spl42_115), inference(avatar_split_clause, [], [f3710, f1131, f1148])).
fof(f1148, plain, (spl42_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_119])])).
fof(f1131, plain, (spl42_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_115])])).
fof(f3710, plain, (~ (e22 = op2(e20, e22)) | ~ spl42_115), inference(forward_demodulation, [], [f281, f1133])).
fof(f1133, plain, ((e22 = op2(e20, e23)) | ~ spl42_115), inference(avatar_component_clause, [], [f1131])).
fof(f281, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f5715, plain, (~ spl42_79 | ~ spl42_95), inference(avatar_split_clause, [], [f5714, f1046, f978])).
fof(f978, plain, (spl42_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_79])])).
fof(f5714, plain, (~ (e22 = op2(e23, e20)) | ~ spl42_95), inference(forward_demodulation, [], [f257, f1048])).
fof(f5712, plain, (~ spl42_71 | spl42_83 | ~ spl42_182), inference(avatar_contradiction_clause, [], [f5711])).
fof(f5711, plain, ($false | (~ spl42_71 | spl42_83 | ~ spl42_182)), inference(subsumption_resolution, [], [f5710, f996])).
fof(f996, plain, (~ (e22 = op2(e22, e23)) | spl42_83), inference(avatar_component_clause, [], [f995])).
fof(f995, plain, (spl42_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_83])])).
fof(f5710, plain, ((e22 = op2(e22, e23)) | (~ spl42_71 | ~ spl42_182)), inference(backward_demodulation, [], [f1634, f946])).
fof(f946, plain, ((e22 = op2(e23, e22)) | ~ spl42_71), inference(avatar_component_clause, [], [f944])).
fof(f944, plain, (spl42_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_71])])).
fof(f1634, plain, ((op2(e23, e22) = op2(op2(e23, e22), e23)) | ~ spl42_182), inference(avatar_component_clause, [], [f1632])).
fof(f1632, plain, (spl42_182 <=> (op2(e23, e22) = op2(op2(e23, e22), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_182])])).
fof(f5706, plain, (~ spl42_70 | ~ spl42_74), inference(avatar_split_clause, [], [f5701, f957, f940])).
fof(f940, plain, (spl42_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_70])])).
fof(f957, plain, (spl42_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_74])])).
fof(f5701, plain, (~ (e21 = op2(e23, e22)) | ~ spl42_74), inference(backward_demodulation, [], [f297, f959])).
fof(f959, plain, ((e21 = op2(e23, e21)) | ~ spl42_74), inference(avatar_component_clause, [], [f957])).
fof(f297, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f5621, plain, (~ spl42_75 | ~ spl42_211 | ~ spl42_216), inference(avatar_split_clause, [], [f5608, f1841, f1816, f961])).
fof(f961, plain, (spl42_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_75])])).
fof(f5608, plain, (~ (e22 = op2(e23, e21)) | (~ spl42_211 | ~ spl42_216)), inference(backward_demodulation, [], [f3340, f1817])).
fof(f3340, plain, (~ (op2(e23, e21) = h1(e11)) | ~ spl42_216), inference(backward_demodulation, [], [f1682, f3337])).
fof(f1682, plain, ~ (op2(e23, e21) = h2(e10)), inference(backward_demodulation, [], [f262, f539])).
fof(f262, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f5602, plain, (~ spl42_70 | ~ spl42_6 | spl42_218), inference(avatar_split_clause, [], [f5601, f1880, f636, f940])).
fof(f1880, plain, (spl42_218 <=> (op2(e23, e22) = h4(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_218])])).
fof(f5601, plain, (~ (e21 = op2(e23, e22)) | (~ spl42_6 | spl42_218)), inference(forward_demodulation, [], [f5600, f1697])).
fof(f1697, plain, (e21 = h4(e11)), inference(forward_demodulation, [], [f548, f532])).
fof(f532, plain, (e21 = op2(op2(e23, e23), op2(e23, e23))), inference(cnf_transformation, [], [f13])).
fof(f548, plain, (op2(op2(e23, e23), op2(e23, e23)) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f5600, plain, (~ (op2(e23, e22) = h4(e11)) | (~ spl42_6 | spl42_218)), inference(forward_demodulation, [], [f1882, f638])).
fof(f1882, plain, (~ (op2(e23, e22) = h4(op1(e13, e12))) | spl42_218), inference(avatar_component_clause, [], [f1880])).
fof(f5599, plain, (~ spl42_75 | ~ spl42_11 | spl42_219), inference(avatar_split_clause, [], [f5598, f1884, f657, f961])).
fof(f1884, plain, (spl42_219 <=> (op2(e23, e21) = h4(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_219])])).
fof(f5598, plain, (~ (e22 = op2(e23, e21)) | (~ spl42_11 | spl42_219)), inference(forward_demodulation, [], [f5597, f1696])).
fof(f5597, plain, (~ (op2(e23, e21) = h4(e12)) | (~ spl42_11 | spl42_219)), inference(forward_demodulation, [], [f1886, f659])).
fof(f1886, plain, (~ (op2(e23, e21) = h4(op1(e13, e11))) | spl42_219), inference(avatar_component_clause, [], [f1884])).
fof(f5593, plain, (~ spl42_120 | ~ spl42_56 | ~ spl42_192 | spl42_230), inference(avatar_split_clause, [], [f5592, f1928, f1719, f848, f1152])).
fof(f1152, plain, (spl42_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_120])])).
fof(f848, plain, (spl42_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_56])])).
fof(f1928, plain, (spl42_230 <=> (h4(op1(e10, e12)) = op2(h4(e10), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_230])])).
fof(f5592, plain, (~ (e23 = op2(e20, e22)) | (~ spl42_56 | ~ spl42_192 | spl42_230)), inference(forward_demodulation, [], [f5591, f546])).
fof(f5591, plain, (~ (op2(e20, e22) = h4(e13)) | (~ spl42_56 | ~ spl42_192 | spl42_230)), inference(forward_demodulation, [], [f5590, f850])).
fof(f850, plain, ((e13 = op1(e10, e12)) | ~ spl42_56), inference(avatar_component_clause, [], [f848])).
fof(f5590, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl42_192 | spl42_230)), inference(forward_demodulation, [], [f1930, f1720])).
fof(f1930, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | spl42_230), inference(avatar_component_clause, [], [f1928])).
fof(f5556, plain, (~ spl42_125 | ~ spl42_126), inference(avatar_contradiction_clause, [], [f5555])).
fof(f5555, plain, ($false | (~ spl42_125 | ~ spl42_126)), inference(subsumption_resolution, [], [f5554, f306])).
fof(f306, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f5554, plain, ((e20 = e21) | (~ spl42_125 | ~ spl42_126)), inference(backward_demodulation, [], [f1180, f1176])).
fof(f1176, plain, ((e20 = op2(e20, e20)) | ~ spl42_125), inference(avatar_component_clause, [], [f1174])).
fof(f1174, plain, (spl42_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_125])])).
fof(f5553, plain, (spl42_125 | ~ spl42_121 | ~ spl42_183), inference(avatar_split_clause, [], [f5552, f1637, f1157, f1174])).
fof(f1637, plain, (spl42_183 <=> (op2(e20, e21) = op2(op2(e20, e21), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_183])])).
fof(f5552, plain, ((e20 = op2(e20, e20)) | (~ spl42_121 | ~ spl42_183)), inference(forward_demodulation, [], [f1639, f1159])).
fof(f1639, plain, ((op2(e20, e21) = op2(op2(e20, e21), e20)) | ~ spl42_183), inference(avatar_component_clause, [], [f1637])).
fof(f5495, plain, (~ spl42_90 | spl42_102 | ~ spl42_185), inference(avatar_contradiction_clause, [], [f5494])).
fof(f5494, plain, ($false | (~ spl42_90 | spl42_102 | ~ spl42_185)), inference(subsumption_resolution, [], [f5492, f1077])).
fof(f5492, plain, ((e21 = op2(e21, e22)) | (~ spl42_90 | ~ spl42_185)), inference(backward_demodulation, [], [f1647, f1027])).
fof(f1027, plain, ((e21 = op2(e22, e21)) | ~ spl42_90), inference(avatar_component_clause, [], [f1025])).
fof(f1025, plain, (spl42_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_90])])).
fof(f5493, plain, (~ spl42_82 | ~ spl42_90), inference(avatar_split_clause, [], [f5489, f1025, f991])).
fof(f5489, plain, (~ (e21 = op2(e22, e23)) | ~ spl42_90), inference(backward_demodulation, [], [f292, f1027])).
fof(f5484, plain, (~ spl42_82 | ~ spl42_98), inference(avatar_split_clause, [], [f5480, f1059, f991])).
fof(f1059, plain, (spl42_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_98])])).
fof(f5480, plain, (~ (e21 = op2(e22, e23)) | ~ spl42_98), inference(backward_demodulation, [], [f273, f1061])).
fof(f1061, plain, ((e21 = op2(e21, e23)) | ~ spl42_98), inference(avatar_component_clause, [], [f1059])).
fof(f5472, plain, (~ spl42_93 | ~ spl42_109 | ~ spl42_216), inference(avatar_split_clause, [], [f5468, f1841, f1106, f1038])).
fof(f1038, plain, (spl42_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_93])])).
fof(f5468, plain, (~ (e20 = op2(e22, e20)) | (~ spl42_109 | ~ spl42_216)), inference(backward_demodulation, [], [f4625, f5461])).
fof(f5461, plain, ((e20 = h1(e12)) | (~ spl42_109 | ~ spl42_216)), inference(backward_demodulation, [], [f3450, f1108])).
fof(f3450, plain, ((op2(e21, e20) = h1(e12)) | ~ spl42_216), inference(forward_demodulation, [], [f1679, f1842])).
fof(f1679, plain, (h1(e12) = op2(h1(e10), e20)), inference(backward_demodulation, [], [f537, f535])).
fof(f537, plain, (op2(op2(e20, e20), e20) = h1(e12)), inference(cnf_transformation, [], [f14])).
fof(f4625, plain, (~ (op2(e22, e20) = h1(e12)) | ~ spl42_216), inference(backward_demodulation, [], [f255, f3450])).
fof(f255, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f5449, plain, (spl42_95 | ~ spl42_115 | ~ spl42_175), inference(avatar_split_clause, [], [f5448, f1603, f1131, f1046])).
fof(f1603, plain, (spl42_175 <=> (op2(e20, e23) = op2(op2(e20, e23), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_175])])).
fof(f5448, plain, ((e22 = op2(e22, e20)) | (~ spl42_115 | ~ spl42_175)), inference(forward_demodulation, [], [f1605, f1133])).
fof(f1605, plain, ((op2(e20, e23) = op2(op2(e20, e23), e20)) | ~ spl42_175), inference(avatar_component_clause, [], [f1603])).
fof(f5394, plain, (~ spl42_82 | ~ spl42_18 | spl42_221), inference(avatar_split_clause, [], [f5393, f1892, f687, f991])).
fof(f1892, plain, (spl42_221 <=> (op2(e22, e23) = h4(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_221])])).
fof(f5393, plain, (~ (e21 = op2(e22, e23)) | (~ spl42_18 | spl42_221)), inference(forward_demodulation, [], [f5392, f1697])).
fof(f5392, plain, (~ (op2(e22, e23) = h4(e11)) | (~ spl42_18 | spl42_221)), inference(forward_demodulation, [], [f1894, f689])).
fof(f689, plain, ((e11 = op1(e12, e13)) | ~ spl42_18), inference(avatar_component_clause, [], [f687])).
fof(f1894, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | spl42_221), inference(avatar_component_clause, [], [f1892])).
fof(f5384, plain, (~ spl42_100 | ~ spl42_36 | spl42_225), inference(avatar_split_clause, [], [f5383, f1908, f763, f1067])).
fof(f1908, plain, (spl42_225 <=> (op2(e21, e23) = h4(op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_225])])).
fof(f5383, plain, (~ (e23 = op2(e21, e23)) | (~ spl42_36 | spl42_225)), inference(forward_demodulation, [], [f5382, f546])).
fof(f5382, plain, (~ (op2(e21, e23) = h4(e13)) | (~ spl42_36 | spl42_225)), inference(forward_demodulation, [], [f1910, f765])).
fof(f1910, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | spl42_225), inference(avatar_component_clause, [], [f1908])).
fof(f5338, plain, (~ spl42_124 | ~ spl42_216 | ~ spl42_289), inference(avatar_split_clause, [], [f5322, f2301, f1841, f1169])).
fof(f5322, plain, (~ (e23 = op2(e20, e21)) | (~ spl42_216 | ~ spl42_289)), inference(backward_demodulation, [], [f3338, f2302])).
fof(f2302, plain, ((e23 = h1(e11)) | ~ spl42_289), inference(avatar_component_clause, [], [f2301])).
fof(f3338, plain, (~ (op2(e20, e21) = h1(e11)) | ~ spl42_216), inference(backward_demodulation, [], [f1680, f3337])).
fof(f1680, plain, ~ (op2(e20, e21) = h2(e10)), inference(backward_demodulation, [], [f258, f539])).
fof(f258, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f5209, plain, (~ spl42_71 | ~ spl42_88 | ~ spl42_181), inference(avatar_contradiction_clause, [], [f5208])).
fof(f5208, plain, ($false | (~ spl42_71 | ~ spl42_88 | ~ spl42_181)), inference(subsumption_resolution, [], [f5207, f311])).
fof(f311, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f5207, plain, ((e22 = e23) | (~ spl42_71 | ~ spl42_88 | ~ spl42_181)), inference(forward_demodulation, [], [f5206, f946])).
fof(f5206, plain, ((e23 = op2(e23, e22)) | (~ spl42_88 | ~ spl42_181)), inference(forward_demodulation, [], [f1630, f1018])).
fof(f1018, plain, ((e23 = op2(e22, e22)) | ~ spl42_88), inference(avatar_component_clause, [], [f1016])).
fof(f1016, plain, (spl42_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_88])])).
fof(f5202, plain, (~ spl42_76 | ~ spl42_80), inference(avatar_split_clause, [], [f5201, f982, f965])).
fof(f965, plain, (spl42_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_76])])).
fof(f5201, plain, (~ (e23 = op2(e23, e21)) | ~ spl42_80), inference(backward_demodulation, [], [f294, f984])).
fof(f294, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f5197, plain, (~ spl42_86 | ~ spl42_88), inference(avatar_contradiction_clause, [], [f5196])).
fof(f5196, plain, ($false | (~ spl42_86 | ~ spl42_88)), inference(subsumption_resolution, [], [f5195, f310])).
fof(f5195, plain, ((e21 = e23) | (~ spl42_86 | ~ spl42_88)), inference(forward_demodulation, [], [f1018, f1010])).
fof(f1010, plain, ((e21 = op2(e22, e22)) | ~ spl42_86), inference(avatar_component_clause, [], [f1008])).
fof(f1008, plain, (spl42_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_86])])).
fof(f5189, plain, (~ spl42_98 | ~ spl42_102), inference(avatar_split_clause, [], [f5185, f1076, f1059])).
fof(f5185, plain, (~ (e21 = op2(e21, e23)) | ~ spl42_102), inference(backward_demodulation, [], [f287, f1078])).
fof(f1078, plain, ((e21 = op2(e21, e22)) | ~ spl42_102), inference(avatar_component_clause, [], [f1076])).
fof(f287, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f5149, plain, (spl42_124 | ~ spl42_208 | ~ spl42_216 | ~ spl42_261), inference(avatar_split_clause, [], [f5142, f2139, f1841, f1801, f1169])).
fof(f1801, plain, (spl42_208 <=> (e20 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_208])])).
fof(f5142, plain, ((e23 = op2(e20, e21)) | (~ spl42_208 | ~ spl42_216 | ~ spl42_261)), inference(backward_demodulation, [], [f4976, f2140])).
fof(f4976, plain, ((op2(e20, e21) = h2(e12)) | (~ spl42_208 | ~ spl42_216)), inference(backward_demodulation, [], [f3343, f4964])).
fof(f4964, plain, ((e20 = h1(e11)) | (~ spl42_208 | ~ spl42_216)), inference(backward_demodulation, [], [f3337, f1802])).
fof(f1802, plain, ((e20 = h2(e10)) | ~ spl42_208), inference(avatar_component_clause, [], [f1801])).
fof(f5121, plain, (~ spl42_93 | ~ spl42_96), inference(avatar_contradiction_clause, [], [f5120])).
fof(f5120, plain, ($false | (~ spl42_93 | ~ spl42_96)), inference(subsumption_resolution, [], [f5119, f308])).
fof(f5119, plain, ((e20 = e23) | (~ spl42_93 | ~ spl42_96)), inference(forward_demodulation, [], [f1052, f1040])).
fof(f1040, plain, ((e20 = op2(e22, e20)) | ~ spl42_93), inference(avatar_component_clause, [], [f1038])).
fof(f1052, plain, ((e23 = op2(e22, e20)) | ~ spl42_96), inference(avatar_component_clause, [], [f1050])).
fof(f5118, plain, (~ spl42_115 | ~ spl42_116), inference(avatar_contradiction_clause, [], [f5117])).
fof(f5117, plain, ($false | (~ spl42_115 | ~ spl42_116)), inference(subsumption_resolution, [], [f5116, f311])).
fof(f5116, plain, ((e22 = e23) | (~ spl42_115 | ~ spl42_116)), inference(forward_demodulation, [], [f1137, f1133])).
fof(f1137, plain, ((e23 = op2(e20, e23)) | ~ spl42_116), inference(avatar_component_clause, [], [f1135])).
fof(f1135, plain, (spl42_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_116])])).
fof(f5087, plain, (~ spl42_122 | ~ spl42_216), inference(avatar_split_clause, [], [f3330, f1841, f1161])).
fof(f1161, plain, (spl42_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_122])])).
fof(f3330, plain, (~ (e21 = op2(e20, e21)) | ~ spl42_216), inference(backward_demodulation, [], [f1675, f1842])).
fof(f1675, plain, ~ (op2(e20, e21) = h1(e10)), inference(backward_demodulation, [], [f276, f535])).
fof(f276, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f5053, plain, (~ spl42_81 | ~ spl42_192), inference(avatar_split_clause, [], [f3096, f1719, f987])).
fof(f987, plain, (spl42_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_81])])).
fof(f3096, plain, (~ (e20 = op2(e22, e23)) | ~ spl42_192), inference(forward_demodulation, [], [f1700, f1720])).
fof(f1700, plain, ~ (op2(e22, e23) = h4(e10)), inference(backward_demodulation, [], [f275, f547])).
fof(f547, plain, (op2(e23, e23) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f275, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f5051, plain, (~ spl42_78 | ~ spl42_216), inference(avatar_split_clause, [], [f4772, f1841, f974])).
fof(f974, plain, (spl42_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_78])])).
fof(f4772, plain, (~ (e21 = op2(e23, e20)) | ~ spl42_216), inference(forward_demodulation, [], [f1674, f1842])).
fof(f1674, plain, ~ (op2(e23, e20) = h1(e10)), inference(backward_demodulation, [], [f254, f535])).
fof(f254, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f5039, plain, (~ spl42_86 | ~ spl42_87), inference(avatar_contradiction_clause, [], [f5038])).
fof(f5038, plain, ($false | (~ spl42_86 | ~ spl42_87)), inference(subsumption_resolution, [], [f5037, f309])).
fof(f309, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f5037, plain, ((e21 = e22) | (~ spl42_86 | ~ spl42_87)), inference(backward_demodulation, [], [f1014, f1010])).
fof(f5021, plain, (~ spl42_101 | ~ spl42_117), inference(avatar_split_clause, [], [f5014, f1140, f1072])).
fof(f1072, plain, (spl42_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_101])])).
fof(f5014, plain, (~ (e20 = op2(e21, e22)) | ~ spl42_117), inference(backward_demodulation, [], [f264, f1142])).
fof(f1142, plain, ((e20 = op2(e20, e22)) | ~ spl42_117), inference(avatar_component_clause, [], [f1140])).
fof(f264, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f5013, plain, (spl42_261 | ~ spl42_124 | ~ spl42_208 | ~ spl42_216), inference(avatar_split_clause, [], [f5012, f1841, f1801, f1169, f2139])).
fof(f5012, plain, ((e23 = h2(e12)) | (~ spl42_124 | ~ spl42_208 | ~ spl42_216)), inference(backward_demodulation, [], [f4976, f1171])).
fof(f5009, plain, (spl42_72 | ~ spl42_84 | ~ spl42_177), inference(avatar_contradiction_clause, [], [f5008])).
fof(f5008, plain, ($false | (spl42_72 | ~ spl42_84 | ~ spl42_177)), inference(subsumption_resolution, [], [f5007, f949])).
fof(f949, plain, (~ (e23 = op2(e23, e22)) | spl42_72), inference(avatar_component_clause, [], [f948])).
fof(f5007, plain, ((e23 = op2(e23, e22)) | (~ spl42_84 | ~ spl42_177)), inference(forward_demodulation, [], [f1613, f1001])).
fof(f1001, plain, ((e23 = op2(e22, e23)) | ~ spl42_84), inference(avatar_component_clause, [], [f999])).
fof(f4987, plain, (~ spl42_121 | ~ spl42_208 | ~ spl42_216), inference(avatar_split_clause, [], [f4971, f1841, f1801, f1157])).
fof(f4971, plain, (~ (e20 = op2(e20, e21)) | (~ spl42_208 | ~ spl42_216)), inference(backward_demodulation, [], [f3338, f4964])).
fof(f4910, plain, (~ spl42_71 | ~ spl42_87), inference(avatar_split_clause, [], [f4901, f1012, f944])).
fof(f4901, plain, (~ (e22 = op2(e23, e22)) | ~ spl42_87), inference(backward_demodulation, [], [f1690, f4898])).
fof(f4898, plain, ((e22 = h3(e10)) | ~ spl42_87), inference(forward_demodulation, [], [f543, f1014])).
fof(f1690, plain, ~ (op2(e23, e22) = h3(e10)), inference(backward_demodulation, [], [f269, f543])).
fof(f269, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4887, plain, (~ spl42_75 | ~ spl42_79), inference(avatar_split_clause, [], [f4885, f978, f961])).
fof(f4885, plain, (~ (e22 = op2(e23, e21)) | ~ spl42_79), inference(backward_demodulation, [], [f294, f980])).
fof(f980, plain, ((e22 = op2(e23, e20)) | ~ spl42_79), inference(avatar_component_clause, [], [f978])).
fof(f4879, plain, (~ spl42_101 | ~ spl42_121 | spl42_180), inference(avatar_contradiction_clause, [], [f4878])).
fof(f4878, plain, ($false | (~ spl42_101 | ~ spl42_121 | spl42_180)), inference(subsumption_resolution, [], [f4876, f1159])).
fof(f4876, plain, (~ (e20 = op2(e20, e21)) | (~ spl42_101 | spl42_180)), inference(backward_demodulation, [], [f1625, f1074])).
fof(f1074, plain, ((e20 = op2(e21, e22)) | ~ spl42_101), inference(avatar_component_clause, [], [f1072])).
fof(f1625, plain, (~ (op2(e21, e22) = op2(op2(e21, e22), e21)) | spl42_180), inference(avatar_component_clause, [], [f1624])).
fof(f1624, plain, (spl42_180 <=> (op2(e21, e22) = op2(op2(e21, e22), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_180])])).
fof(f4856, plain, (~ spl42_93 | ~ spl42_115 | ~ spl42_175), inference(avatar_contradiction_clause, [], [f4855])).
fof(f4855, plain, ($false | (~ spl42_93 | ~ spl42_115 | ~ spl42_175)), inference(subsumption_resolution, [], [f4854, f307])).
fof(f307, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f4854, plain, ((e20 = e22) | (~ spl42_93 | ~ spl42_115 | ~ spl42_175)), inference(forward_demodulation, [], [f4853, f1040])).
fof(f4853, plain, ((e22 = op2(e22, e20)) | (~ spl42_115 | ~ spl42_175)), inference(forward_demodulation, [], [f1605, f1133])).
fof(f4740, plain, (~ spl42_65 | spl42_113 | ~ spl42_178), inference(avatar_contradiction_clause, [], [f4739])).
fof(f4739, plain, ($false | (~ spl42_65 | spl42_113 | ~ spl42_178)), inference(subsumption_resolution, [], [f4738, f1124])).
fof(f1124, plain, (~ (e20 = op2(e20, e23)) | spl42_113), inference(avatar_component_clause, [], [f1123])).
fof(f1123, plain, (spl42_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_113])])).
fof(f4738, plain, ((e20 = op2(e20, e23)) | (~ spl42_65 | ~ spl42_178)), inference(forward_demodulation, [], [f1617, f921])).
fof(f1617, plain, ((op2(e23, e23) = op2(op2(e23, e23), e23)) | ~ spl42_178), inference(avatar_component_clause, [], [f1615])).
fof(f1615, plain, (spl42_178 <=> (op2(e23, e23) = op2(op2(e23, e23), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_178])])).
fof(f4737, plain, (spl42_123 | ~ spl42_202 | ~ spl42_208 | ~ spl42_216), inference(avatar_contradiction_clause, [], [f4736])).
fof(f4736, plain, ($false | (spl42_123 | ~ spl42_202 | ~ spl42_208 | ~ spl42_216)), inference(subsumption_resolution, [], [f4716, f1166])).
fof(f1166, plain, (~ (e22 = op2(e20, e21)) | spl42_123), inference(avatar_component_clause, [], [f1165])).
fof(f1165, plain, (spl42_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_123])])).
fof(f4716, plain, ((e22 = op2(e20, e21)) | (~ spl42_202 | ~ spl42_208 | ~ spl42_216)), inference(backward_demodulation, [], [f4678, f4693])).
fof(f4693, plain, ((e20 = h1(e11)) | (~ spl42_208 | ~ spl42_216)), inference(backward_demodulation, [], [f3337, f1802])).
fof(f4678, plain, ((e22 = op2(h1(e11), e21)) | (~ spl42_202 | ~ spl42_216)), inference(forward_demodulation, [], [f3343, f1771])).
fof(f1771, plain, ((e22 = h2(e12)) | ~ spl42_202), inference(avatar_component_clause, [], [f1770])).
fof(f1770, plain, (spl42_202 <=> (e22 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_202])])).
fof(f4720, plain, (~ spl42_109 | ~ spl42_208 | ~ spl42_216), inference(avatar_split_clause, [], [f4700, f1841, f1801, f1106])).
fof(f4700, plain, (~ (e20 = op2(e21, e20)) | (~ spl42_208 | ~ spl42_216)), inference(backward_demodulation, [], [f3341, f4693])).
fof(f3341, plain, (~ (op2(e21, e20) = h1(e11)) | ~ spl42_216), inference(backward_demodulation, [], [f1683, f3337])).
fof(f1683, plain, ~ (op2(e21, e20) = h2(e10)), inference(backward_demodulation, [], [f282, f539])).
fof(f282, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4654, plain, (~ spl42_75 | spl42_83 | ~ spl42_186), inference(avatar_contradiction_clause, [], [f4653])).
fof(f4653, plain, ($false | (~ spl42_75 | spl42_83 | ~ spl42_186)), inference(subsumption_resolution, [], [f4652, f996])).
fof(f4652, plain, ((e22 = op2(e22, e23)) | (~ spl42_75 | ~ spl42_186)), inference(forward_demodulation, [], [f1651, f963])).
fof(f963, plain, ((e22 = op2(e23, e21)) | ~ spl42_75), inference(avatar_component_clause, [], [f961])).
fof(f1651, plain, ((op2(e23, e21) = op2(op2(e23, e21), e23)) | ~ spl42_186), inference(avatar_component_clause, [], [f1649])).
fof(f1649, plain, (spl42_186 <=> (op2(e23, e21) = op2(op2(e23, e21), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_186])])).
fof(f4614, plain, (~ spl42_96 | ~ spl42_84), inference(avatar_split_clause, [], [f4613, f999, f1050])).
fof(f4613, plain, (~ (e23 = op2(e22, e20)) | ~ spl42_84), inference(forward_demodulation, [], [f290, f1001])).
fof(f290, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4586, plain, (spl42_202 | ~ spl42_75 | ~ spl42_216 | ~ spl42_289), inference(avatar_split_clause, [], [f4584, f2301, f1841, f961, f1770])).
fof(f4584, plain, ((e22 = h2(e12)) | (~ spl42_75 | ~ spl42_216 | ~ spl42_289)), inference(backward_demodulation, [], [f4520, f963])).
fof(f4520, plain, ((op2(e23, e21) = h2(e12)) | (~ spl42_216 | ~ spl42_289)), inference(backward_demodulation, [], [f3343, f2302])).
fof(f4512, plain, (~ spl42_74 | spl42_98 | ~ spl42_186), inference(avatar_contradiction_clause, [], [f4511])).
fof(f4511, plain, ($false | (~ spl42_74 | spl42_98 | ~ spl42_186)), inference(subsumption_resolution, [], [f4507, f1060])).
fof(f1060, plain, (~ (e21 = op2(e21, e23)) | spl42_98), inference(avatar_component_clause, [], [f1059])).
fof(f4507, plain, ((e21 = op2(e21, e23)) | (~ spl42_74 | ~ spl42_186)), inference(backward_demodulation, [], [f1651, f959])).
fof(f4496, plain, (~ spl42_70 | ~ spl42_102), inference(avatar_split_clause, [], [f4493, f1076, f940])).
fof(f4493, plain, (~ (e21 = op2(e23, e22)) | ~ spl42_102), inference(backward_demodulation, [], [f268, f1078])).
fof(f268, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4394, plain, (~ spl42_75 | ~ spl42_184 | ~ spl42_216 | ~ spl42_289), inference(avatar_contradiction_clause, [], [f4393])).
fof(f4393, plain, ($false | (~ spl42_75 | ~ spl42_184 | ~ spl42_216 | ~ spl42_289)), inference(subsumption_resolution, [], [f4392, f311])).
fof(f4392, plain, ((e22 = e23) | (~ spl42_75 | ~ spl42_184 | ~ spl42_216 | ~ spl42_289)), inference(forward_demodulation, [], [f4384, f963])).
fof(f4384, plain, ((e23 = op2(e23, e21)) | (~ spl42_184 | ~ spl42_216 | ~ spl42_289)), inference(backward_demodulation, [], [f4263, f2302])).
fof(f4263, plain, ((h1(e11) = op2(h1(e11), e21)) | (~ spl42_184 | ~ spl42_216)), inference(backward_demodulation, [], [f1643, f3331])).
fof(f1643, plain, ((op2(e21, e21) = op2(op2(e21, e21), e21)) | ~ spl42_184), inference(avatar_component_clause, [], [f1641])).
fof(f1641, plain, (spl42_184 <=> (op2(e21, e21) = op2(op2(e21, e21), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_184])])).
fof(f4380, plain, (spl42_289 | ~ spl42_108 | ~ spl42_216), inference(avatar_split_clause, [], [f4322, f1841, f1101, f2301])).
fof(f4322, plain, ((e23 = h1(e11)) | (~ spl42_108 | ~ spl42_216)), inference(backward_demodulation, [], [f3331, f1103])).
fof(f1103, plain, ((e23 = op2(e21, e21)) | ~ spl42_108), inference(avatar_component_clause, [], [f1101])).
fof(f4342, plain, (~ spl42_74 | ~ spl42_90), inference(avatar_split_clause, [], [f4336, f1025, f957])).
fof(f4336, plain, (~ (e21 = op2(e23, e21)) | ~ spl42_90), inference(backward_demodulation, [], [f263, f1027])).
fof(f263, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4219, plain, (spl42_91 | ~ spl42_107 | ~ spl42_184), inference(avatar_contradiction_clause, [], [f4218])).
fof(f4218, plain, ($false | (spl42_91 | ~ spl42_107 | ~ spl42_184)), inference(subsumption_resolution, [], [f4217, f1030])).
fof(f1030, plain, (~ (e22 = op2(e22, e21)) | spl42_91), inference(avatar_component_clause, [], [f1029])).
fof(f1029, plain, (spl42_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_91])])).
fof(f4217, plain, ((e22 = op2(e22, e21)) | (~ spl42_107 | ~ spl42_184)), inference(forward_demodulation, [], [f1643, f1099])).
fof(f1099, plain, ((e22 = op2(e21, e21)) | ~ spl42_107), inference(avatar_component_clause, [], [f1097])).
fof(f1097, plain, (spl42_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_107])])).
fof(f4211, plain, (spl42_211 | ~ spl42_107 | ~ spl42_216), inference(avatar_split_clause, [], [f4210, f1841, f1097, f1816])).
fof(f4210, plain, ((e22 = h1(e11)) | (~ spl42_107 | ~ spl42_216)), inference(forward_demodulation, [], [f3331, f1099])).
fof(f4115, plain, (~ spl42_42 | ~ spl42_215 | ~ spl42_216 | spl42_227), inference(avatar_contradiction_clause, [], [f4114])).
fof(f4114, plain, ($false | (~ spl42_42 | ~ spl42_215 | ~ spl42_216 | spl42_227)), inference(subsumption_resolution, [], [f4113, f3458])).
fof(f3458, plain, ((e21 = h2(e10)) | (~ spl42_215 | ~ spl42_216)), inference(backward_demodulation, [], [f3337, f1837])).
fof(f4113, plain, (~ (e21 = h2(e10)) | (~ spl42_42 | spl42_227)), inference(forward_demodulation, [], [f4112, f1697])).
fof(f4112, plain, (~ (h2(e10) = h4(e11)) | (~ spl42_42 | spl42_227)), inference(forward_demodulation, [], [f1918, f791])).
fof(f1918, plain, (~ (h2(e10) = h4(op1(e11, e11))) | spl42_227), inference(avatar_component_clause, [], [f1916])).
fof(f1916, plain, (spl42_227 <=> (h2(e10) = h4(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_227])])).
fof(f4054, plain, (~ spl42_31 | ~ spl42_95 | ~ spl42_192 | spl42_224), inference(avatar_contradiction_clause, [], [f4053])).
fof(f4053, plain, ($false | (~ spl42_31 | ~ spl42_95 | ~ spl42_192 | spl42_224)), inference(subsumption_resolution, [], [f4052, f1048])).
fof(f4052, plain, (~ (e22 = op2(e22, e20)) | (~ spl42_31 | ~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f4051, f1696])).
fof(f4051, plain, (~ (op2(e22, e20) = h4(e12)) | (~ spl42_31 | ~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f4050, f744])).
fof(f4050, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f1906, f1720])).
fof(f1906, plain, (~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | spl42_224), inference(avatar_component_clause, [], [f1904])).
fof(f1904, plain, (spl42_224 <=> (h4(op1(e12, e10)) = op2(e22, h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_224])])).
fof(f3999, plain, (~ spl42_45 | ~ spl42_109 | ~ spl42_192 | spl42_228), inference(avatar_contradiction_clause, [], [f3998])).
fof(f3998, plain, ($false | (~ spl42_45 | ~ spl42_109 | ~ spl42_192 | spl42_228)), inference(subsumption_resolution, [], [f3997, f1108])).
fof(f3997, plain, (~ (e20 = op2(e21, e20)) | (~ spl42_45 | ~ spl42_192 | spl42_228)), inference(forward_demodulation, [], [f3996, f1720])).
fof(f3996, plain, (~ (op2(e21, e20) = h4(e10)) | (~ spl42_45 | ~ spl42_192 | spl42_228)), inference(forward_demodulation, [], [f3995, f804])).
fof(f3995, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | (~ spl42_192 | spl42_228)), inference(forward_demodulation, [], [f1922, f1720])).
fof(f1922, plain, (~ (h4(op1(e11, e10)) = op2(e21, h4(e10))) | spl42_228), inference(avatar_component_clause, [], [f1920])).
fof(f1920, plain, (spl42_228 <=> (h4(op1(e11, e10)) = op2(e21, h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_228])])).
fof(f3886, plain, (~ spl42_7 | ~ spl42_11), inference(avatar_split_clause, [], [f3880, f657, f640])).
fof(f640, plain, (spl42_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_7])])).
fof(f3880, plain, (~ (e12 = op1(e13, e12)) | ~ spl42_11), inference(backward_demodulation, [], [f249, f659])).
fof(f249, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3855, plain, (~ spl42_72 | ~ spl42_120), inference(avatar_split_clause, [], [f3854, f1152, f948])).
fof(f3854, plain, (~ (e23 = op2(e23, e22)) | ~ spl42_120), inference(forward_demodulation, [], [f266, f1154])).
fof(f1154, plain, ((e23 = op2(e20, e22)) | ~ spl42_120), inference(avatar_component_clause, [], [f1152])).
fof(f266, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3850, plain, (spl42_38 | ~ spl42_18 | ~ spl42_146), inference(avatar_split_clause, [], [f3690, f1387, f687, f772])).
fof(f3690, plain, ((e11 = op1(e11, e12)) | (~ spl42_18 | ~ spl42_146)), inference(backward_demodulation, [], [f1389, f689])).
fof(f3849, plain, (~ spl42_39 | ~ spl42_43), inference(avatar_split_clause, [], [f3848, f793, f776])).
fof(f793, plain, (spl42_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_43])])).
fof(f3848, plain, (~ (e12 = op1(e11, e12)) | ~ spl42_43), inference(forward_demodulation, [], [f237, f795])).
fof(f795, plain, ((e12 = op1(e11, e11)) | ~ spl42_43), inference(avatar_component_clause, [], [f793])).
fof(f237, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f3830, plain, (~ spl42_7 | ~ spl42_28 | ~ spl42_154), inference(avatar_contradiction_clause, [], [f3829])).
fof(f3829, plain, ($false | (~ spl42_7 | ~ spl42_28 | ~ spl42_154)), inference(subsumption_resolution, [], [f3828, f305])).
fof(f3828, plain, ((e12 = e13) | (~ spl42_7 | ~ spl42_28 | ~ spl42_154)), inference(forward_demodulation, [], [f3827, f642])).
fof(f642, plain, ((e12 = op1(e13, e12)) | ~ spl42_7), inference(avatar_component_clause, [], [f640])).
fof(f3827, plain, ((e13 = op1(e13, e12)) | (~ spl42_28 | ~ spl42_154)), inference(forward_demodulation, [], [f1423, f731])).
fof(f3817, plain, (~ spl42_62 | spl42_232), inference(avatar_contradiction_clause, [], [f3816])).
fof(f3816, plain, ($false | (~ spl42_62 | spl42_232)), inference(subsumption_resolution, [], [f3815, f1697])).
fof(f3815, plain, (~ (e21 = h4(e11)) | (~ spl42_62 | spl42_232)), inference(forward_demodulation, [], [f1938, f876])).
fof(f876, plain, ((op1(e10, e10) = e11) | ~ spl42_62), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl42_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl42_62])])).
fof(f1938, plain, (~ (e21 = h4(op1(e10, e10))) | spl42_232), inference(avatar_component_clause, [], [f1936])).
fof(f1936, plain, (spl42_232 <=> (e21 = h4(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_232])])).
fof(f3791, plain, (~ spl42_27 | ~ spl42_31), inference(avatar_split_clause, [], [f3788, f742, f725])).
fof(f725, plain, (spl42_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_27])])).
fof(f3788, plain, (~ (e12 = op1(e12, e11)) | ~ spl42_31), inference(backward_demodulation, [], [f240, f744])).
fof(f240, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3785, plain, (spl42_27 | ~ spl42_43 | ~ spl42_153), inference(avatar_split_clause, [], [f3783, f1417, f793, f725])).
fof(f3783, plain, ((e12 = op1(e12, e11)) | (~ spl42_43 | ~ spl42_153)), inference(backward_demodulation, [], [f1419, f795])).
fof(f3750, plain, (~ spl42_37 | ~ spl42_38), inference(avatar_contradiction_clause, [], [f3749])).
fof(f3749, plain, ($false | (~ spl42_37 | ~ spl42_38)), inference(subsumption_resolution, [], [f3748, f300])).
fof(f300, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f3748, plain, ((e10 = e11) | (~ spl42_37 | ~ spl42_38)), inference(backward_demodulation, [], [f774, f770])).
fof(f770, plain, ((e10 = op1(e11, e12)) | ~ spl42_37), inference(avatar_component_clause, [], [f768])).
fof(f768, plain, (spl42_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_37])])).
fof(f774, plain, ((e11 = op1(e11, e12)) | ~ spl42_38), inference(avatar_component_clause, [], [f772])).
fof(f3730, plain, (~ spl42_85 | ~ spl42_88), inference(avatar_contradiction_clause, [], [f3729])).
fof(f3729, plain, ($false | (~ spl42_85 | ~ spl42_88)), inference(subsumption_resolution, [], [f3728, f308])).
fof(f3728, plain, ((e20 = e23) | (~ spl42_85 | ~ spl42_88)), inference(forward_demodulation, [], [f1018, f1006])).
fof(f3720, plain, (~ spl42_121 | ~ spl42_124), inference(avatar_contradiction_clause, [], [f3719])).
fof(f3719, plain, ($false | (~ spl42_121 | ~ spl42_124)), inference(subsumption_resolution, [], [f3718, f308])).
fof(f3718, plain, ((e20 = e23) | (~ spl42_121 | ~ spl42_124)), inference(forward_demodulation, [], [f1171, f1159])).
fof(f3709, plain, (~ spl42_118 | ~ spl42_216), inference(avatar_split_clause, [], [f3708, f1841, f1144])).
fof(f1144, plain, (spl42_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_118])])).
fof(f3708, plain, (~ (e21 = op2(e20, e22)) | ~ spl42_216), inference(forward_demodulation, [], [f1676, f1842])).
fof(f1676, plain, ~ (op2(e20, e22) = h1(e10)), inference(backward_demodulation, [], [f277, f535])).
fof(f277, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3698, plain, (~ spl42_42 | ~ spl42_10), inference(avatar_split_clause, [], [f3425, f653, f789])).
fof(f653, plain, (spl42_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_10])])).
fof(f3425, plain, (~ (e11 = op1(e11, e11)) | ~ spl42_10), inference(forward_demodulation, [], [f214, f655])).
fof(f655, plain, ((e11 = op1(e13, e11)) | ~ spl42_10), inference(avatar_component_clause, [], [f653])).
fof(f214, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3695, plain, (~ spl42_42 | ~ spl42_38), inference(avatar_split_clause, [], [f3694, f772, f789])).
fof(f3694, plain, (~ (e11 = op1(e11, e11)) | ~ spl42_38), inference(forward_demodulation, [], [f237, f774])).
fof(f3693, plain, (~ spl42_44 | ~ spl42_36), inference(avatar_split_clause, [], [f3692, f763, f797])).
fof(f3692, plain, (~ (e13 = op1(e11, e11)) | ~ spl42_36), inference(forward_demodulation, [], [f238, f765])).
fof(f3687, plain, (~ spl42_7 | ~ spl42_24 | ~ spl42_150), inference(avatar_contradiction_clause, [], [f3686])).
fof(f3686, plain, ($false | (~ spl42_7 | ~ spl42_24 | ~ spl42_150)), inference(subsumption_resolution, [], [f3685, f305])).
fof(f3685, plain, ((e12 = e13) | (~ spl42_7 | ~ spl42_24 | ~ spl42_150)), inference(forward_demodulation, [], [f3683, f642])).
fof(f3683, plain, ((e13 = op1(e13, e12)) | (~ spl42_24 | ~ spl42_150)), inference(backward_demodulation, [], [f1406, f714])).
fof(f714, plain, ((e13 = op1(e12, e12)) | ~ spl42_24), inference(avatar_component_clause, [], [f712])).
fof(f712, plain, (spl42_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_24])])).
fof(f1406, plain, ((op1(e12, e12) = op1(op1(e12, e12), e12)) | ~ spl42_150), inference(avatar_component_clause, [], [f1404])).
fof(f1404, plain, (spl42_150 <=> (op1(e12, e12) = op1(op1(e12, e12), e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_150])])).
fof(f3662, plain, (~ spl42_34 | ~ spl42_38), inference(avatar_split_clause, [], [f3658, f772, f755])).
fof(f3658, plain, (~ (e11 = op1(e11, e13)) | ~ spl42_38), inference(backward_demodulation, [], [f239, f774])).
fof(f239, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3590, plain, (~ spl42_57 | ~ spl42_60), inference(avatar_contradiction_clause, [], [f3589])).
fof(f3589, plain, ($false | (~ spl42_57 | ~ spl42_60)), inference(subsumption_resolution, [], [f3586, f302])).
fof(f302, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f3586, plain, ((e10 = e13) | (~ spl42_57 | ~ spl42_60)), inference(backward_demodulation, [], [f867, f855])).
fof(f867, plain, ((e13 = op1(e10, e11)) | ~ spl42_60), inference(avatar_component_clause, [], [f865])).
fof(f865, plain, (spl42_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_60])])).
fof(f3585, plain, (~ spl42_61 | ~ spl42_62), inference(avatar_contradiction_clause, [], [f3584])).
fof(f3584, plain, ($false | (~ spl42_61 | ~ spl42_62)), inference(subsumption_resolution, [], [f3583, f300])).
fof(f3583, plain, ((e10 = e11) | (~ spl42_61 | ~ spl42_62)), inference(backward_demodulation, [], [f876, f872])).
fof(f872, plain, ((e10 = op1(e10, e10)) | ~ spl42_61), inference(avatar_component_clause, [], [f870])).
fof(f870, plain, (spl42_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_61])])).
fof(f3581, plain, (spl42_23 | ~ spl42_31 | ~ spl42_158), inference(avatar_contradiction_clause, [], [f3580])).
fof(f3580, plain, ($false | (spl42_23 | ~ spl42_31 | ~ spl42_158)), inference(subsumption_resolution, [], [f3579, f709])).
fof(f709, plain, (~ (e12 = op1(e12, e12)) | spl42_23), inference(avatar_component_clause, [], [f708])).
fof(f3579, plain, ((e12 = op1(e12, e12)) | (~ spl42_31 | ~ spl42_158)), inference(forward_demodulation, [], [f1440, f744])).
fof(f1440, plain, ((op1(e12, e10) = op1(op1(e12, e10), e12)) | ~ spl42_158), inference(avatar_component_clause, [], [f1438])).
fof(f1438, plain, (spl42_158 <=> (op1(e12, e10) = op1(op1(e12, e10), e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_158])])).
fof(f3577, plain, (~ spl42_54 | ~ spl42_62), inference(avatar_split_clause, [], [f3576, f874, f840])).
fof(f840, plain, (spl42_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_54])])).
fof(f3576, plain, (~ (e11 = op1(e10, e12)) | ~ spl42_62), inference(forward_demodulation, [], [f229, f876])).
fof(f229, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3573, plain, (~ spl42_48 | ~ spl42_16), inference(avatar_split_clause, [], [f3524, f678, f814])).
fof(f814, plain, (spl42_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_48])])).
fof(f3524, plain, (~ (e13 = op1(e11, e10)) | ~ spl42_16), inference(forward_demodulation, [], [f208, f680])).
fof(f208, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3568, plain, (~ spl42_26 | ~ spl42_10), inference(avatar_split_clause, [], [f3514, f653, f721])).
fof(f3514, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_10), inference(forward_demodulation, [], [f215, f655])).
fof(f215, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3549, plain, (~ spl42_45 | spl42_57 | ~ spl42_157), inference(avatar_contradiction_clause, [], [f3548])).
fof(f3548, plain, ($false | (~ spl42_45 | spl42_57 | ~ spl42_157)), inference(subsumption_resolution, [], [f3544, f854])).
fof(f854, plain, (~ (e10 = op1(e10, e11)) | spl42_57), inference(avatar_component_clause, [], [f853])).
fof(f3544, plain, ((e10 = op1(e10, e11)) | (~ spl42_45 | ~ spl42_157)), inference(backward_demodulation, [], [f1436, f804])).
fof(f1436, plain, ((op1(e11, e10) = op1(op1(e11, e10), e11)) | ~ spl42_157), inference(avatar_component_clause, [], [f1434])).
fof(f1434, plain, (spl42_157 <=> (op1(e11, e10) = op1(op1(e11, e10), e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_157])])).
fof(f3534, plain, (~ spl42_7 | spl42_19 | ~ spl42_151), inference(avatar_contradiction_clause, [], [f3533])).
fof(f3533, plain, ($false | (~ spl42_7 | spl42_19 | ~ spl42_151)), inference(subsumption_resolution, [], [f3532, f692])).
fof(f3532, plain, ((e12 = op1(e12, e13)) | (~ spl42_7 | ~ spl42_151)), inference(forward_demodulation, [], [f1410, f642])).
fof(f1410, plain, ((op1(e13, e12) = op1(op1(e13, e12), e13)) | ~ spl42_151), inference(avatar_component_clause, [], [f1408])).
fof(f1408, plain, (spl42_151 <=> (op1(e13, e12) = op1(op1(e13, e12), e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_151])])).
fof(f3519, plain, (~ spl42_32 | ~ spl42_16), inference(avatar_split_clause, [], [f3518, f678, f746])).
fof(f746, plain, (spl42_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_32])])).
fof(f3518, plain, (~ (e13 = op1(e12, e10)) | ~ spl42_16), inference(forward_demodulation, [], [f209, f680])).
fof(f209, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3517, plain, (~ spl42_30 | ~ spl42_62), inference(avatar_split_clause, [], [f3516, f874, f738])).
fof(f738, plain, (spl42_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_30])])).
fof(f3516, plain, (~ (e11 = op1(e12, e10)) | ~ spl42_62), inference(forward_demodulation, [], [f205, f876])).
fof(f205, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f3515, plain, (~ spl42_32 | ~ spl42_20), inference(avatar_split_clause, [], [f3080, f695, f746])).
fof(f3080, plain, (~ (e13 = op1(e12, e10)) | ~ spl42_20), inference(forward_demodulation, [], [f242, f697])).
fof(f242, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f3489, plain, (~ spl42_7 | ~ spl42_20 | ~ spl42_146), inference(avatar_contradiction_clause, [], [f3488])).
fof(f3488, plain, ($false | (~ spl42_7 | ~ spl42_20 | ~ spl42_146)), inference(subsumption_resolution, [], [f3487, f305])).
fof(f3487, plain, ((e12 = e13) | (~ spl42_7 | ~ spl42_20 | ~ spl42_146)), inference(forward_demodulation, [], [f3486, f642])).
fof(f3486, plain, ((e13 = op1(e13, e12)) | (~ spl42_20 | ~ spl42_146)), inference(forward_demodulation, [], [f1389, f697])).
fof(f3472, plain, (~ spl42_98 | ~ spl42_215 | ~ spl42_216), inference(avatar_split_clause, [], [f3464, f1841, f1836, f1059])).
fof(f3464, plain, (~ (e21 = op2(e21, e23)) | (~ spl42_215 | ~ spl42_216)), inference(backward_demodulation, [], [f3442, f1837])).
fof(f3446, plain, (~ spl42_99 | ~ spl42_115), inference(avatar_split_clause, [], [f3445, f1131, f1063])).
fof(f1063, plain, (spl42_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_99])])).
fof(f3445, plain, (~ (e22 = op2(e21, e23)) | ~ spl42_115), inference(forward_demodulation, [], [f270, f1133])).
fof(f270, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3430, plain, (~ spl42_55 | ~ spl42_51), inference(avatar_split_clause, [], [f3277, f827, f844])).
fof(f844, plain, (spl42_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_55])])).
fof(f3277, plain, (~ (e12 = op1(e10, e12)) | ~ spl42_51), inference(forward_demodulation, [], [f233, f829])).
fof(f233, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3424, plain, (spl42_34 | ~ spl42_10 | ~ spl42_155), inference(avatar_split_clause, [], [f3416, f1425, f653, f755])).
fof(f3416, plain, ((e11 = op1(e11, e13)) | (~ spl42_10 | ~ spl42_155)), inference(backward_demodulation, [], [f1427, f655])).
fof(f3412, plain, (~ spl42_12 | ~ spl42_16), inference(avatar_split_clause, [], [f3410, f678, f661])).
fof(f661, plain, (spl42_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_12])])).
fof(f3410, plain, (~ (e13 = op1(e13, e11)) | ~ spl42_16), inference(backward_demodulation, [], [f246, f680])).
fof(f246, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3373, plain, (spl42_215 | ~ spl42_106 | ~ spl42_216), inference(avatar_split_clause, [], [f3371, f1841, f1093, f1836])).
fof(f1093, plain, (spl42_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_106])])).
fof(f3371, plain, ((e21 = h1(e11)) | (~ spl42_106 | ~ spl42_216)), inference(backward_demodulation, [], [f3331, f1095])).
fof(f1095, plain, ((e21 = op2(e21, e21)) | ~ spl42_106), inference(avatar_component_clause, [], [f1093])).
fof(f3369, plain, (~ spl42_109 | ~ spl42_112), inference(avatar_contradiction_clause, [], [f3368])).
fof(f3368, plain, ($false | (~ spl42_109 | ~ spl42_112)), inference(subsumption_resolution, [], [f3367, f308])).
fof(f3367, plain, ((e20 = e23) | (~ spl42_109 | ~ spl42_112)), inference(backward_demodulation, [], [f1120, f1108])).
fof(f1120, plain, ((e23 = op2(e21, e20)) | ~ spl42_112), inference(avatar_component_clause, [], [f1118])).
fof(f1118, plain, (spl42_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_112])])).
fof(f3364, plain, (~ spl42_289 | ~ spl42_112 | ~ spl42_216), inference(avatar_split_clause, [], [f3363, f1841, f1118, f2301])).
fof(f3363, plain, (~ (e23 = h1(e11)) | (~ spl42_112 | ~ spl42_216)), inference(backward_demodulation, [], [f3341, f1120])).
fof(f3354, plain, (spl42_46 | ~ spl42_62 | ~ spl42_156), inference(avatar_contradiction_clause, [], [f3353])).
fof(f3353, plain, ($false | (spl42_46 | ~ spl42_62 | ~ spl42_156)), inference(subsumption_resolution, [], [f3352, f807])).
fof(f807, plain, (~ (e11 = op1(e11, e10)) | spl42_46), inference(avatar_component_clause, [], [f806])).
fof(f806, plain, (spl42_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_46])])).
fof(f3352, plain, ((e11 = op1(e11, e10)) | (~ spl42_62 | ~ spl42_156)), inference(forward_demodulation, [], [f1432, f876])).
fof(f1432, plain, ((op1(e10, e10) = op1(op1(e10, e10), e10)) | ~ spl42_156), inference(avatar_component_clause, [], [f1430])).
fof(f1430, plain, (spl42_156 <=> (op1(e10, e10) = op1(op1(e10, e10), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_156])])).
fof(f3344, plain, (spl42_110 | ~ spl42_187 | ~ spl42_216), inference(avatar_split_clause, [], [f3334, f1841, f1654, f1110])).
fof(f1110, plain, (spl42_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_110])])).
fof(f1654, plain, (spl42_187 <=> (op2(e20, e20) = op2(op2(e20, e20), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_187])])).
fof(f3334, plain, ((e21 = op2(e21, e20)) | (~ spl42_187 | ~ spl42_216)), inference(backward_demodulation, [], [f2321, f1842])).
fof(f2321, plain, ((h1(e10) = op2(h1(e10), e20)) | ~ spl42_187), inference(forward_demodulation, [], [f1656, f535])).
fof(f1656, plain, ((op2(e20, e20) = op2(op2(e20, e20), e20)) | ~ spl42_187), inference(avatar_component_clause, [], [f1654])).
fof(f3336, plain, (~ spl42_110 | ~ spl42_216), inference(avatar_split_clause, [], [f3329, f1841, f1110])).
fof(f3329, plain, (~ (e21 = op2(e21, e20)) | ~ spl42_216), inference(backward_demodulation, [], [f1672, f1842])).
fof(f1672, plain, ~ (op2(e21, e20) = h1(e10)), inference(backward_demodulation, [], [f252, f535])).
fof(f252, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f3326, plain, (spl42_216 | ~ spl42_126), inference(avatar_split_clause, [], [f3142, f1178, f1841])).
fof(f3142, plain, ((e21 = h1(e10)) | ~ spl42_126), inference(forward_demodulation, [], [f535, f1180])).
fof(f3306, plain, (~ spl42_123 | ~ spl42_115), inference(avatar_split_clause, [], [f3305, f1131, f1165])).
fof(f3305, plain, (~ (e22 = op2(e20, e21)) | ~ spl42_115), inference(forward_demodulation, [], [f280, f1133])).
fof(f280, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3302, plain, (~ spl42_112 | ~ spl42_80), inference(avatar_split_clause, [], [f3126, f982, f1118])).
fof(f3126, plain, (~ (e23 = op2(e21, e20)) | ~ spl42_80), inference(forward_demodulation, [], [f256, f984])).
fof(f256, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3301, plain, (~ spl42_111 | ~ spl42_95), inference(avatar_split_clause, [], [f3300, f1046, f1114])).
fof(f1114, plain, (spl42_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_111])])).
fof(f3300, plain, (~ (e22 = op2(e21, e20)) | ~ spl42_95), inference(forward_demodulation, [], [f255, f1048])).
fof(f3259, plain, (spl42_42 | ~ spl42_34 | ~ spl42_145), inference(avatar_split_clause, [], [f3166, f1383, f755, f789])).
fof(f3166, plain, ((e11 = op1(e11, e11)) | (~ spl42_34 | ~ spl42_145)), inference(forward_demodulation, [], [f1385, f757])).
fof(f3258, plain, (~ spl42_33 | ~ spl42_1), inference(avatar_split_clause, [], [f2930, f615, f751])).
fof(f751, plain, (spl42_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_33])])).
fof(f615, plain, (spl42_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_1])])).
fof(f2930, plain, (~ (e10 = op1(e11, e13)) | ~ spl42_1), inference(forward_demodulation, [], [f226, f617])).
fof(f617, plain, ((e10 = op1(e13, e13)) | ~ spl42_1), inference(avatar_component_clause, [], [f615])).
fof(f226, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3250, plain, (~ spl42_7 | ~ spl42_15), inference(avatar_split_clause, [], [f3247, f674, f640])).
fof(f674, plain, (spl42_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_15])])).
fof(f3247, plain, (~ (e12 = op1(e13, e12)) | ~ spl42_15), inference(backward_demodulation, [], [f247, f676])).
fof(f676, plain, ((e12 = op1(e13, e10)) | ~ spl42_15), inference(avatar_component_clause, [], [f674])).
fof(f247, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3249, plain, (~ spl42_15 | spl42_19 | ~ spl42_159), inference(avatar_contradiction_clause, [], [f3248])).
fof(f3248, plain, ($false | (~ spl42_15 | spl42_19 | ~ spl42_159)), inference(subsumption_resolution, [], [f3246, f692])).
fof(f3246, plain, ((e12 = op1(e12, e13)) | (~ spl42_15 | ~ spl42_159)), inference(backward_demodulation, [], [f1444, f676])).
fof(f1444, plain, ((op1(e13, e10) = op1(op1(e13, e10), e13)) | ~ spl42_159), inference(avatar_component_clause, [], [f1442])).
fof(f1442, plain, (spl42_159 <=> (op1(e13, e10) = op1(op1(e13, e10), e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_159])])).
fof(f3234, plain, (~ spl42_51 | ~ spl42_52), inference(avatar_contradiction_clause, [], [f3233])).
fof(f3233, plain, ($false | (~ spl42_51 | ~ spl42_52)), inference(subsumption_resolution, [], [f3232, f305])).
fof(f3232, plain, ((e12 = e13) | (~ spl42_51 | ~ spl42_52)), inference(forward_demodulation, [], [f833, f829])).
fof(f833, plain, ((e13 = op1(e10, e13)) | ~ spl42_52), inference(avatar_component_clause, [], [f831])).
fof(f831, plain, (spl42_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_52])])).
fof(f3204, plain, (~ spl42_85 | ~ spl42_86), inference(avatar_contradiction_clause, [], [f3203])).
fof(f3203, plain, ($false | (~ spl42_85 | ~ spl42_86)), inference(subsumption_resolution, [], [f3202, f306])).
fof(f3202, plain, ((e20 = e21) | (~ spl42_85 | ~ spl42_86)), inference(backward_demodulation, [], [f1010, f1006])).
fof(f3185, plain, (~ spl42_83 | ~ spl42_115), inference(avatar_split_clause, [], [f3183, f1131, f995])).
fof(f3183, plain, (~ (e22 = op2(e22, e23)) | ~ spl42_115), inference(backward_demodulation, [], [f271, f1133])).
fof(f271, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3134, plain, (~ spl42_113 | ~ spl42_192), inference(avatar_split_clause, [], [f2981, f1719, f1123])).
fof(f2981, plain, (~ (e20 = op2(e20, e23)) | ~ spl42_192), inference(forward_demodulation, [], [f1698, f1720])).
fof(f1698, plain, ~ (op2(e20, e23) = h4(e10)), inference(backward_demodulation, [], [f272, f547])).
fof(f272, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3117, plain, (spl42_106 | ~ spl42_98 | ~ spl42_176), inference(avatar_split_clause, [], [f3116, f1607, f1059, f1093])).
fof(f1607, plain, (spl42_176 <=> (op2(e21, e23) = op2(op2(e21, e23), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_176])])).
fof(f3116, plain, ((e21 = op2(e21, e21)) | (~ spl42_98 | ~ spl42_176)), inference(forward_demodulation, [], [f1609, f1061])).
fof(f1609, plain, ((op2(e21, e23) = op2(op2(e21, e23), e21)) | ~ spl42_176), inference(avatar_component_clause, [], [f1607])).
fof(f3112, plain, (~ spl42_97 | ~ spl42_192), inference(avatar_split_clause, [], [f2824, f1719, f1055])).
fof(f1055, plain, (spl42_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_97])])).
fof(f2824, plain, (~ (e20 = op2(e21, e23)) | ~ spl42_192), inference(backward_demodulation, [], [f1699, f1720])).
fof(f1699, plain, ~ (op2(e21, e23) = h4(e10)), inference(backward_demodulation, [], [f274, f547])).
fof(f274, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3107, plain, (~ spl42_204 | ~ spl42_91), inference(avatar_split_clause, [], [f3106, f1029, f1780])).
fof(f1780, plain, (spl42_204 <=> (e22 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_204])])).
fof(f3106, plain, (~ (e22 = h2(e10)) | ~ spl42_91), inference(forward_demodulation, [], [f1681, f1031])).
fof(f1031, plain, ((e22 = op2(e22, e21)) | ~ spl42_91), inference(avatar_component_clause, [], [f1029])).
fof(f1681, plain, ~ (op2(e22, e21) = h2(e10)), inference(backward_demodulation, [], [f261, f539])).
fof(f261, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3093, plain, (~ spl42_73 | ~ spl42_192), inference(avatar_split_clause, [], [f2825, f1719, f953])).
fof(f953, plain, (spl42_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_73])])).
fof(f2825, plain, (~ (e20 = op2(e23, e21)) | ~ spl42_192), inference(backward_demodulation, [], [f1702, f1720])).
fof(f1702, plain, ~ (op2(e23, e21) = h4(e10)), inference(backward_demodulation, [], [f298, f547])).
fof(f298, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3091, plain, (~ spl42_69 | ~ spl42_192), inference(avatar_split_clause, [], [f3090, f1719, f936])).
fof(f936, plain, (spl42_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_69])])).
fof(f3090, plain, (~ (e20 = op2(e23, e22)) | ~ spl42_192), inference(forward_demodulation, [], [f1703, f1720])).
fof(f1703, plain, ~ (op2(e23, e22) = h4(e10)), inference(backward_demodulation, [], [f299, f547])).
fof(f299, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3087, plain, (~ spl42_58 | ~ spl42_62), inference(avatar_split_clause, [], [f2942, f874, f857])).
fof(f857, plain, (spl42_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_58])])).
fof(f2942, plain, (~ (e11 = op1(e10, e11)) | ~ spl42_62), inference(forward_demodulation, [], [f228, f876])).
fof(f228, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f3071, plain, (~ spl42_15 | ~ spl42_47), inference(avatar_split_clause, [], [f3040, f810, f674])).
fof(f3040, plain, (~ (e12 = op1(e13, e10)) | ~ spl42_47), inference(backward_demodulation, [], [f208, f812])).
fof(f3066, plain, (~ spl42_1 | ~ spl42_2), inference(avatar_contradiction_clause, [], [f3065])).
fof(f3065, plain, ($false | (~ spl42_1 | ~ spl42_2)), inference(subsumption_resolution, [], [f3064, f300])).
fof(f3064, plain, ((e10 = e11) | (~ spl42_1 | ~ spl42_2)), inference(forward_demodulation, [], [f621, f617])).
fof(f621, plain, ((e11 = op1(e13, e13)) | ~ spl42_2), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl42_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_2])])).
fof(f3057, plain, (~ spl42_22 | ~ spl42_23), inference(avatar_contradiction_clause, [], [f3056])).
fof(f3056, plain, ($false | (~ spl42_22 | ~ spl42_23)), inference(subsumption_resolution, [], [f3055, f303])).
fof(f303, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f3055, plain, ((e11 = e12) | (~ spl42_22 | ~ spl42_23)), inference(backward_demodulation, [], [f710, f706])).
fof(f706, plain, ((e11 = op1(e12, e12)) | ~ spl42_22), inference(avatar_component_clause, [], [f704])).
fof(f704, plain, (spl42_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_22])])).
fof(f710, plain, ((e12 = op1(e12, e12)) | ~ spl42_23), inference(avatar_component_clause, [], [f708])).
fof(f3032, plain, (~ spl42_62 | ~ spl42_64), inference(avatar_contradiction_clause, [], [f3031])).
fof(f3031, plain, ($false | (~ spl42_62 | ~ spl42_64)), inference(subsumption_resolution, [], [f3030, f304])).
fof(f3030, plain, ((e11 = e13) | (~ spl42_62 | ~ spl42_64)), inference(forward_demodulation, [], [f884, f876])).
fof(f884, plain, ((op1(e10, e10) = e13) | ~ spl42_64), inference(avatar_component_clause, [], [f882])).
fof(f882, plain, (spl42_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl42_64])])).
fof(f3029, plain, (~ spl42_74 | ~ spl42_76), inference(avatar_contradiction_clause, [], [f3028])).
fof(f3028, plain, ($false | (~ spl42_74 | ~ spl42_76)), inference(subsumption_resolution, [], [f3027, f310])).
fof(f3027, plain, ((e21 = e23) | (~ spl42_74 | ~ spl42_76)), inference(forward_demodulation, [], [f967, f959])).
fof(f967, plain, ((e23 = op2(e23, e21)) | ~ spl42_76), inference(avatar_component_clause, [], [f965])).
fof(f3026, plain, (~ spl42_89 | ~ spl42_91), inference(avatar_contradiction_clause, [], [f3025])).
fof(f3025, plain, ($false | (~ spl42_89 | ~ spl42_91)), inference(subsumption_resolution, [], [f3024, f307])).
fof(f3024, plain, ((e20 = e22) | (~ spl42_89 | ~ spl42_91)), inference(forward_demodulation, [], [f1031, f1023])).
fof(f1023, plain, ((e20 = op2(e22, e21)) | ~ spl42_89), inference(avatar_component_clause, [], [f1021])).
fof(f3018, plain, (~ spl42_109 | ~ spl42_111), inference(avatar_contradiction_clause, [], [f3017])).
fof(f3017, plain, ($false | (~ spl42_109 | ~ spl42_111)), inference(subsumption_resolution, [], [f3016, f307])).
fof(f3016, plain, ((e20 = e22) | (~ spl42_109 | ~ spl42_111)), inference(forward_demodulation, [], [f1116, f1108])).
fof(f1116, plain, ((e22 = op2(e21, e20)) | ~ spl42_111), inference(avatar_component_clause, [], [f1114])).
fof(f3012, plain, (~ spl42_126 | ~ spl42_127), inference(avatar_contradiction_clause, [], [f3011])).
fof(f3011, plain, ($false | (~ spl42_126 | ~ spl42_127)), inference(subsumption_resolution, [], [f3010, f309])).
fof(f3010, plain, ((e21 = e22) | (~ spl42_126 | ~ spl42_127)), inference(backward_demodulation, [], [f1184, f1180])).
fof(f1184, plain, ((op2(e20, e20) = e22) | ~ spl42_127), inference(avatar_component_clause, [], [f1182])).
fof(f1182, plain, (spl42_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl42_127])])).
fof(f3007, plain, (~ spl42_1 | spl42_49 | ~ spl42_147), inference(avatar_contradiction_clause, [], [f3006])).
fof(f3006, plain, ($false | (~ spl42_1 | spl42_49 | ~ spl42_147)), inference(subsumption_resolution, [], [f3005, f820])).
fof(f820, plain, (~ (e10 = op1(e10, e13)) | spl42_49), inference(avatar_component_clause, [], [f819])).
fof(f819, plain, (spl42_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_49])])).
fof(f3005, plain, ((e10 = op1(e10, e13)) | (~ spl42_1 | ~ spl42_147)), inference(forward_demodulation, [], [f1393, f617])).
fof(f1393, plain, ((op1(e13, e13) = op1(op1(e13, e13), e13)) | ~ spl42_147), inference(avatar_component_clause, [], [f1391])).
fof(f1391, plain, (spl42_147 <=> (op1(e13, e13) = op1(op1(e13, e13), e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_147])])).
fof(f2988, plain, (~ spl42_51 | spl42_229), inference(avatar_contradiction_clause, [], [f2987])).
fof(f2987, plain, ($false | (~ spl42_51 | spl42_229)), inference(subsumption_resolution, [], [f2986, f1696])).
fof(f2986, plain, (~ (e22 = h4(e12)) | (~ spl42_51 | spl42_229)), inference(forward_demodulation, [], [f1926, f829])).
fof(f1926, plain, (~ (e22 = h4(op1(e10, e13))) | spl42_229), inference(avatar_component_clause, [], [f1924])).
fof(f1924, plain, (spl42_229 <=> (e22 = h4(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_229])])).
fof(f2976, plain, (spl42_115 | ~ spl42_192), inference(avatar_split_clause, [], [f2827, f1719, f1131])).
fof(f2827, plain, ((e22 = op2(e20, e23)) | ~ spl42_192), inference(backward_demodulation, [], [f1705, f1720])).
fof(f1705, plain, (e22 = op2(h4(e10), e23)), inference(backward_demodulation, [], [f533, f547])).
fof(f2974, plain, (~ spl42_74 | ~ spl42_106), inference(avatar_contradiction_clause, [], [f2973])).
fof(f2973, plain, ($false | (~ spl42_74 | ~ spl42_106)), inference(subsumption_resolution, [], [f2972, f2871])).
fof(f2871, plain, (~ (e21 = h2(e10)) | ~ spl42_74), inference(backward_demodulation, [], [f1682, f959])).
fof(f2972, plain, ((e21 = h2(e10)) | ~ spl42_106), inference(forward_demodulation, [], [f539, f1095])).
fof(f2941, plain, (~ spl42_14 | ~ spl42_62), inference(avatar_split_clause, [], [f2940, f874, f670])).
fof(f670, plain, (spl42_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_14])])).
fof(f2940, plain, (~ (e11 = op1(e13, e10)) | ~ spl42_62), inference(forward_demodulation, [], [f206, f876])).
fof(f206, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2938, plain, (~ spl42_59 | ~ spl42_51), inference(avatar_split_clause, [], [f2937, f827, f861])).
fof(f861, plain, (spl42_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_59])])).
fof(f2937, plain, (~ (e12 = op1(e10, e11)) | ~ spl42_51), inference(forward_demodulation, [], [f232, f829])).
fof(f232, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2935, plain, (~ spl42_46 | ~ spl42_62), inference(avatar_split_clause, [], [f2934, f874, f806])).
fof(f2934, plain, (~ (e11 = op1(e11, e10)) | ~ spl42_62), inference(forward_demodulation, [], [f204, f876])).
fof(f204, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f2919, plain, (~ spl42_23 | ~ spl42_7), inference(avatar_split_clause, [], [f2918, f640, f708])).
fof(f2918, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_7), inference(forward_demodulation, [], [f221, f642])).
fof(f221, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2917, plain, (~ spl42_22 | ~ spl42_18), inference(avatar_split_clause, [], [f2916, f687, f704])).
fof(f2916, plain, (~ (e11 = op1(e12, e12)) | ~ spl42_18), inference(forward_demodulation, [], [f245, f689])).
fof(f245, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2908, plain, (~ spl42_17 | ~ spl42_1), inference(avatar_split_clause, [], [f2779, f615, f683])).
fof(f683, plain, (spl42_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_17])])).
fof(f2779, plain, (~ (e10 = op1(e12, e13)) | ~ spl42_1), inference(forward_demodulation, [], [f227, f617])).
fof(f227, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2904, plain, (~ spl42_6 | ~ spl42_7), inference(avatar_contradiction_clause, [], [f2903])).
fof(f2903, plain, ($false | (~ spl42_6 | ~ spl42_7)), inference(subsumption_resolution, [], [f2902, f303])).
fof(f2902, plain, ((e11 = e12) | (~ spl42_6 | ~ spl42_7)), inference(backward_demodulation, [], [f642, f638])).
fof(f2901, plain, (~ spl42_1 | ~ spl42_16 | ~ spl42_159), inference(avatar_contradiction_clause, [], [f2900])).
fof(f2900, plain, ($false | (~ spl42_1 | ~ spl42_16 | ~ spl42_159)), inference(subsumption_resolution, [], [f2899, f302])).
fof(f2899, plain, ((e10 = e13) | (~ spl42_1 | ~ spl42_16 | ~ spl42_159)), inference(forward_demodulation, [], [f2898, f617])).
fof(f2898, plain, ((e13 = op1(e13, e13)) | (~ spl42_16 | ~ spl42_159)), inference(backward_demodulation, [], [f1444, f680])).
fof(f2893, plain, (~ spl42_26 | ~ spl42_27), inference(avatar_contradiction_clause, [], [f2892])).
fof(f2892, plain, ($false | (~ spl42_26 | ~ spl42_27)), inference(subsumption_resolution, [], [f2891, f303])).
fof(f2891, plain, ((e11 = e12) | (~ spl42_26 | ~ spl42_27)), inference(backward_demodulation, [], [f727, f723])).
fof(f727, plain, ((e12 = op1(e12, e11)) | ~ spl42_27), inference(avatar_component_clause, [], [f725])).
fof(f2860, plain, (spl42_76 | ~ spl42_100 | ~ spl42_176), inference(avatar_contradiction_clause, [], [f2859])).
fof(f2859, plain, ($false | (spl42_76 | ~ spl42_100 | ~ spl42_176)), inference(subsumption_resolution, [], [f2857, f966])).
fof(f966, plain, (~ (e23 = op2(e23, e21)) | spl42_76), inference(avatar_component_clause, [], [f965])).
fof(f2857, plain, ((e23 = op2(e23, e21)) | (~ spl42_100 | ~ spl42_176)), inference(backward_demodulation, [], [f1609, f1069])).
fof(f2852, plain, (~ spl42_106 | ~ spl42_107), inference(avatar_contradiction_clause, [], [f2851])).
fof(f2851, plain, ($false | (~ spl42_106 | ~ spl42_107)), inference(subsumption_resolution, [], [f2850, f309])).
fof(f2850, plain, ((e21 = e22) | (~ spl42_106 | ~ spl42_107)), inference(backward_demodulation, [], [f1099, f1095])).
fof(f2849, plain, (spl42_204 | ~ spl42_107), inference(avatar_split_clause, [], [f2848, f1097, f1780])).
fof(f2848, plain, ((e22 = h2(e10)) | ~ spl42_107), inference(backward_demodulation, [], [f539, f1099])).
fof(f2835, plain, (~ spl42_114 | ~ spl42_192), inference(avatar_contradiction_clause, [], [f2834])).
fof(f2834, plain, ($false | (~ spl42_114 | ~ spl42_192)), inference(subsumption_resolution, [], [f2833, f309])).
fof(f2833, plain, ((e21 = e22) | (~ spl42_114 | ~ spl42_192)), inference(forward_demodulation, [], [f2827, f1129])).
fof(f1129, plain, ((e21 = op2(e20, e23)) | ~ spl42_114), inference(avatar_component_clause, [], [f1127])).
fof(f1127, plain, (spl42_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_114])])).
fof(f2832, plain, (spl42_126 | ~ spl42_192), inference(avatar_split_clause, [], [f2826, f1719, f1178])).
fof(f2826, plain, ((op2(e20, e20) = e21) | ~ spl42_192), inference(backward_demodulation, [], [f1704, f1720])).
fof(f1704, plain, (e21 = op2(h4(e10), h4(e10))), inference(backward_demodulation, [], [f532, f547])).
fof(f2805, plain, (~ spl42_51 | ~ spl42_63), inference(avatar_split_clause, [], [f2804, f878, f827])).
fof(f878, plain, (spl42_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl42_63])])).
fof(f2804, plain, (~ (e12 = op1(e10, e13)) | ~ spl42_63), inference(forward_demodulation, [], [f230, f880])).
fof(f880, plain, ((op1(e10, e10) = e12) | ~ spl42_63), inference(avatar_component_clause, [], [f878])).
fof(f230, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2800, plain, (spl42_51 | ~ spl42_1), inference(avatar_split_clause, [], [f2589, f615, f827])).
fof(f2589, plain, ((e12 = op1(e10, e13)) | ~ spl42_1), inference(forward_demodulation, [], [f530, f617])).
fof(f530, plain, (e12 = op1(op1(e13, e13), e13)), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(op1(e13, e13), e13)) & (e11 = op1(op1(e13, e13), op1(e13, e13))) & (e10 = op1(e13, e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax12)).
fof(f2755, plain, (~ spl42_38 | ~ spl42_39), inference(avatar_contradiction_clause, [], [f2754])).
fof(f2754, plain, ($false | (~ spl42_38 | ~ spl42_39)), inference(subsumption_resolution, [], [f2753, f303])).
fof(f2753, plain, ((e11 = e12) | (~ spl42_38 | ~ spl42_39)), inference(forward_demodulation, [], [f778, f774])).
fof(f2735, plain, (~ spl42_65 | ~ spl42_66), inference(avatar_contradiction_clause, [], [f2734])).
fof(f2734, plain, ($false | (~ spl42_65 | ~ spl42_66)), inference(subsumption_resolution, [], [f2733, f306])).
fof(f2733, plain, ((e20 = e21) | (~ spl42_65 | ~ spl42_66)), inference(forward_demodulation, [], [f925, f921])).
fof(f925, plain, ((e21 = op2(e23, e23)) | ~ spl42_66), inference(avatar_component_clause, [], [f923])).
fof(f923, plain, (spl42_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_66])])).
fof(f2727, plain, (~ spl42_85 | spl42_200), inference(avatar_contradiction_clause, [], [f2726])).
fof(f2726, plain, ($false | (~ spl42_85 | spl42_200)), inference(subsumption_resolution, [], [f2724, f1762])).
fof(f1762, plain, (~ (e20 = h3(e10)) | spl42_200), inference(avatar_component_clause, [], [f1760])).
fof(f2724, plain, ((e20 = h3(e10)) | ~ spl42_85), inference(backward_demodulation, [], [f543, f1006])).
fof(f2718, plain, (spl42_106 | ~ spl42_102 | ~ spl42_180), inference(avatar_split_clause, [], [f2715, f1624, f1076, f1093])).
fof(f2715, plain, ((e21 = op2(e21, e21)) | (~ spl42_102 | ~ spl42_180)), inference(backward_demodulation, [], [f1626, f1078])).
fof(f1626, plain, ((op2(e21, e22) = op2(op2(e21, e22), e21)) | ~ spl42_180), inference(avatar_component_clause, [], [f1624])).
fof(f2714, plain, (spl42_271 | ~ spl42_108), inference(avatar_split_clause, [], [f2713, f1101, f2203])).
fof(f2203, plain, (spl42_271 <=> (e23 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_271])])).
fof(f2713, plain, ((e23 = h2(e10)) | ~ spl42_108), inference(backward_demodulation, [], [f539, f1103])).
fof(f2695, plain, (~ spl42_38 | ~ spl42_41 | ~ spl42_149), inference(avatar_contradiction_clause, [], [f2694])).
fof(f2694, plain, ($false | (~ spl42_38 | ~ spl42_41 | ~ spl42_149)), inference(subsumption_resolution, [], [f2693, f300])).
fof(f2693, plain, ((e10 = e11) | (~ spl42_38 | ~ spl42_41 | ~ spl42_149)), inference(forward_demodulation, [], [f2692, f787])).
fof(f787, plain, ((e10 = op1(e11, e11)) | ~ spl42_41), inference(avatar_component_clause, [], [f785])).
fof(f2692, plain, ((e11 = op1(e11, e11)) | (~ spl42_38 | ~ spl42_149)), inference(forward_demodulation, [], [f1402, f774])).
fof(f1402, plain, ((op1(e11, e12) = op1(op1(e11, e12), e11)) | ~ spl42_149), inference(avatar_component_clause, [], [f1400])).
fof(f1400, plain, (spl42_149 <=> (op1(e11, e12) = op1(op1(e11, e12), e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_149])])).
fof(f2636, plain, (spl42_192 | ~ spl42_65), inference(avatar_split_clause, [], [f2635, f919, f1719])).
fof(f2635, plain, ((e20 = h4(e10)) | ~ spl42_65), inference(forward_demodulation, [], [f547, f921])).
fof(f2622, plain, (~ spl42_49 | ~ spl42_1), inference(avatar_split_clause, [], [f2621, f615, f819])).
fof(f2621, plain, (~ (e10 = op1(e10, e13)) | ~ spl42_1), inference(forward_demodulation, [], [f224, f617])).
fof(f224, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2612, plain, (~ spl42_48 | ~ spl42_36), inference(avatar_split_clause, [], [f2611, f763, f814])).
fof(f2611, plain, (~ (e13 = op1(e11, e10)) | ~ spl42_36), inference(forward_demodulation, [], [f236, f765])).
fof(f236, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2601, plain, (~ spl42_13 | ~ spl42_1), inference(avatar_split_clause, [], [f2600, f615, f666])).
fof(f666, plain, (spl42_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_13])])).
fof(f2600, plain, (~ (e10 = op1(e13, e10)) | ~ spl42_1), inference(forward_demodulation, [], [f248, f617])).
fof(f248, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2596, plain, (~ spl42_5 | ~ spl42_1), inference(avatar_split_clause, [], [f2595, f615, f632])).
fof(f632, plain, (spl42_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_5])])).
fof(f2595, plain, (~ (e10 = op1(e13, e12)) | ~ spl42_1), inference(forward_demodulation, [], [f251, f617])).
fof(f251, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2594, plain, (spl42_62 | ~ spl42_1), inference(avatar_split_clause, [], [f2593, f615, f874])).
fof(f2593, plain, ((op1(e10, e10) = e11) | ~ spl42_1), inference(forward_demodulation, [], [f529, f617])).
fof(f529, plain, (e11 = op1(op1(e13, e13), op1(e13, e13))), inference(cnf_transformation, [], [f12])).
fof(f2592, plain, (~ spl42_1 | ~ spl42_50), inference(avatar_contradiction_clause, [], [f2591])).
fof(f2591, plain, ($false | (~ spl42_1 | ~ spl42_50)), inference(subsumption_resolution, [], [f2590, f303])).
fof(f2590, plain, ((e11 = e12) | (~ spl42_1 | ~ spl42_50)), inference(forward_demodulation, [], [f2589, f825])).
fof(f825, plain, ((e11 = op1(e10, e13)) | ~ spl42_50), inference(avatar_component_clause, [], [f823])).
fof(f823, plain, (spl42_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_50])])).
fof(f2588, plain, (~ spl42_1 | spl42_217), inference(avatar_contradiction_clause, [], [f2587])).
fof(f2587, plain, ($false | (~ spl42_1 | spl42_217)), inference(trivial_inequality_removal, [], [f2586])).
fof(f2586, plain, (~ (h4(e10) = h4(e10)) | (~ spl42_1 | spl42_217)), inference(forward_demodulation, [], [f1878, f617])).
fof(f1878, plain, (~ (h4(e10) = h4(op1(e13, e13))) | spl42_217), inference(avatar_component_clause, [], [f1876])).
fof(f1876, plain, (spl42_217 <=> (h4(e10) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_217])])).
fof(f2585, plain, (~ spl42_1 | ~ spl42_3), inference(avatar_contradiction_clause, [], [f2584])).
fof(f2584, plain, ($false | (~ spl42_1 | ~ spl42_3)), inference(subsumption_resolution, [], [f2583, f301])).
fof(f301, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2583, plain, ((e10 = e12) | (~ spl42_1 | ~ spl42_3)), inference(backward_demodulation, [], [f625, f617])).
fof(f625, plain, ((e12 = op1(e13, e13)) | ~ spl42_3), inference(avatar_component_clause, [], [f623])).
fof(f623, plain, (spl42_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_3])])).
fof(f2572, plain, (~ spl42_10 | ~ spl42_11), inference(avatar_contradiction_clause, [], [f2571])).
fof(f2571, plain, ($false | (~ spl42_10 | ~ spl42_11)), inference(subsumption_resolution, [], [f2570, f303])).
fof(f2570, plain, ((e11 = e12) | (~ spl42_10 | ~ spl42_11)), inference(backward_demodulation, [], [f659, f655])).
fof(f2558, plain, (~ spl42_11 | ~ spl42_15), inference(avatar_split_clause, [], [f2554, f674, f657])).
fof(f2554, plain, (~ (e12 = op1(e13, e11)) | ~ spl42_15), inference(backward_demodulation, [], [f246, f676])).
fof(f2508, plain, (~ spl42_37 | ~ spl42_45), inference(avatar_split_clause, [], [f2503, f802, f768])).
fof(f2503, plain, (~ (e10 = op1(e11, e12)) | ~ spl42_45), inference(backward_demodulation, [], [f235, f804])).
fof(f2494, plain, (~ spl42_19 | ~ spl42_51), inference(avatar_split_clause, [], [f2490, f827, f691])).
fof(f2490, plain, (~ (e12 = op1(e12, e13)) | ~ spl42_51), inference(backward_demodulation, [], [f223, f829])).
fof(f223, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2493, plain, (~ spl42_35 | ~ spl42_51), inference(avatar_split_clause, [], [f2489, f827, f759])).
fof(f759, plain, (spl42_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_35])])).
fof(f2489, plain, (~ (e12 = op1(e11, e13)) | ~ spl42_51), inference(backward_demodulation, [], [f222, f829])).
fof(f222, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2488, plain, (spl42_61 | ~ spl42_53 | ~ spl42_148), inference(avatar_split_clause, [], [f2483, f1396, f836, f870])).
fof(f1396, plain, (spl42_148 <=> (op1(e10, e12) = op1(op1(e10, e12), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_148])])).
fof(f2483, plain, ((e10 = op1(e10, e10)) | (~ spl42_53 | ~ spl42_148)), inference(backward_demodulation, [], [f1398, f838])).
fof(f838, plain, ((e10 = op1(e10, e12)) | ~ spl42_53), inference(avatar_component_clause, [], [f836])).
fof(f1398, plain, ((op1(e10, e12) = op1(op1(e10, e12), e10)) | ~ spl42_148), inference(avatar_component_clause, [], [f1396])).
fof(f2478, plain, (spl42_61 | ~ spl42_57 | ~ spl42_152), inference(avatar_split_clause, [], [f2472, f1413, f853, f870])).
fof(f1413, plain, (spl42_152 <=> (op1(e10, e11) = op1(op1(e10, e11), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_152])])).
fof(f2472, plain, ((e10 = op1(e10, e10)) | (~ spl42_57 | ~ spl42_152)), inference(backward_demodulation, [], [f1415, f855])).
fof(f1415, plain, ((op1(e10, e11) = op1(op1(e10, e11), e10)) | ~ spl42_152), inference(avatar_component_clause, [], [f1413])).
fof(f2475, plain, (~ spl42_9 | ~ spl42_57), inference(avatar_split_clause, [], [f2469, f853, f649])).
fof(f649, plain, (spl42_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_9])])).
fof(f2469, plain, (~ (e10 = op1(e13, e11)) | ~ spl42_57), inference(backward_demodulation, [], [f212, f855])).
fof(f212, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2474, plain, (~ spl42_25 | ~ spl42_57), inference(avatar_split_clause, [], [f2468, f853, f717])).
fof(f717, plain, (spl42_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_25])])).
fof(f2468, plain, (~ (e10 = op1(e12, e11)) | ~ spl42_57), inference(backward_demodulation, [], [f211, f855])).
fof(f211, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2452, plain, (~ spl42_65 | ~ spl42_67), inference(avatar_contradiction_clause, [], [f2451])).
fof(f2451, plain, ($false | (~ spl42_65 | ~ spl42_67)), inference(subsumption_resolution, [], [f2450, f307])).
fof(f2450, plain, ((e20 = e22) | (~ spl42_65 | ~ spl42_67)), inference(backward_demodulation, [], [f929, f921])).
fof(f929, plain, ((e22 = op2(e23, e23)) | ~ spl42_67), inference(avatar_component_clause, [], [f927])).
fof(f927, plain, (spl42_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_67])])).
fof(f2443, plain, (~ spl42_71 | ~ spl42_72), inference(avatar_contradiction_clause, [], [f2442])).
fof(f2442, plain, ($false | (~ spl42_71 | ~ spl42_72)), inference(subsumption_resolution, [], [f2441, f311])).
fof(f2441, plain, ((e22 = e23) | (~ spl42_71 | ~ spl42_72)), inference(backward_demodulation, [], [f950, f946])).
fof(f950, plain, ((e23 = op2(e23, e22)) | ~ spl42_72), inference(avatar_component_clause, [], [f948])).
fof(f2433, plain, (~ spl42_75 | ~ spl42_76), inference(avatar_contradiction_clause, [], [f2432])).
fof(f2432, plain, ($false | (~ spl42_75 | ~ spl42_76)), inference(subsumption_resolution, [], [f2431, f311])).
fof(f2431, plain, ((e22 = e23) | (~ spl42_75 | ~ spl42_76)), inference(backward_demodulation, [], [f967, f963])).
fof(f2429, plain, (~ spl42_271 | ~ spl42_76), inference(avatar_split_clause, [], [f2426, f965, f2203])).
fof(f2426, plain, (~ (e23 = h2(e10)) | ~ spl42_76), inference(backward_demodulation, [], [f1682, f967])).
fof(f2414, plain, (spl42_252 | ~ spl42_88), inference(avatar_split_clause, [], [f2413, f1016, f2089])).
fof(f2413, plain, ((e23 = h3(e10)) | ~ spl42_88), inference(backward_demodulation, [], [f543, f1018])).
fof(f2410, plain, (~ spl42_208 | ~ spl42_89), inference(avatar_split_clause, [], [f2406, f1021, f1801])).
fof(f2406, plain, (~ (e20 = h2(e10)) | ~ spl42_89), inference(backward_demodulation, [], [f1681, f1023])).
fof(f2400, plain, (~ spl42_216 | ~ spl42_94), inference(avatar_split_clause, [], [f2395, f1042, f1841])).
fof(f1042, plain, (spl42_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_94])])).
fof(f2395, plain, (~ (e21 = h1(e10)) | ~ spl42_94), inference(backward_demodulation, [], [f1673, f1044])).
fof(f1044, plain, ((e21 = op2(e22, e20)) | ~ spl42_94), inference(avatar_component_clause, [], [f1042])).
fof(f1673, plain, ~ (op2(e22, e20) = h1(e10)), inference(backward_demodulation, [], [f253, f535])).
fof(f253, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2378, plain, (~ spl42_200 | ~ spl42_101), inference(avatar_split_clause, [], [f2374, f1072, f1760])).
fof(f2374, plain, (~ (e20 = h3(e10)) | ~ spl42_101), inference(backward_demodulation, [], [f1689, f1074])).
fof(f1689, plain, ~ (op2(e21, e22) = h3(e10)), inference(backward_demodulation, [], [f267, f543])).
fof(f267, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2373, plain, (spl42_208 | ~ spl42_105), inference(avatar_split_clause, [], [f2372, f1089, f1801])).
fof(f1089, plain, (spl42_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_105])])).
fof(f2372, plain, ((e20 = h2(e10)) | ~ spl42_105), inference(backward_demodulation, [], [f539, f1091])).
fof(f1091, plain, ((e20 = op2(e21, e21)) | ~ spl42_105), inference(avatar_component_clause, [], [f1089])).
fof(f2369, plain, (~ spl42_77 | ~ spl42_109), inference(avatar_split_clause, [], [f2364, f1106, f970])).
fof(f970, plain, (spl42_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_77])])).
fof(f2364, plain, (~ (e20 = op2(e23, e20)) | ~ spl42_109), inference(backward_demodulation, [], [f256, f1108])).
fof(f2347, plain, (spl42_125 | ~ spl42_117 | ~ spl42_179), inference(avatar_split_clause, [], [f2341, f1620, f1140, f1174])).
fof(f1620, plain, (spl42_179 <=> (op2(e20, e22) = op2(op2(e20, e22), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_179])])).
fof(f2341, plain, ((e20 = op2(e20, e20)) | (~ spl42_117 | ~ spl42_179)), inference(backward_demodulation, [], [f1622, f1142])).
fof(f1622, plain, ((op2(e20, e22) = op2(op2(e20, e22), e20)) | ~ spl42_179), inference(avatar_component_clause, [], [f1620])).
fof(f1939, plain, (spl42_191 | ~ spl42_217 | ~ spl42_218 | ~ spl42_219 | ~ spl42_220 | ~ spl42_221 | ~ spl42_222 | ~ spl42_223 | ~ spl42_224 | ~ spl42_225 | ~ spl42_226 | ~ spl42_227 | ~ spl42_228 | ~ spl42_229 | ~ spl42_230 | ~ spl42_231 | ~ spl42_232), inference(avatar_split_clause, [], [f1874, f1936, f1932, f1928, f1924, f1920, f1916, f1912, f1908, f1904, f1900, f1896, f1892, f1888, f1884, f1880, f1876, f1715])).
fof(f1715, plain, (spl42_191 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl42_191])])).
fof(f1874, plain, (~ (e21 = h4(op1(e10, e10))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), e21)) | ~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | ~ (e22 = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(e21, h4(e10))) | ~ (h2(e10) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39), inference(forward_demodulation, [], [f1873, f1704])).
fof(f1873, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e21)) | ~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | ~ (e22 = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(e21, h4(e10))) | ~ (h2(e10) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1872, f1697])).
fof(f1872, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | ~ (e22 = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(e21, h4(e10))) | ~ (h2(e10) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1871, f1696])).
fof(f1871, plain, (~ (e22 = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(e21, h4(e10))) | ~ (h2(e10) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1870, f1705])).
fof(f1870, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e10)) = op2(e21, h4(e10))) | ~ (h2(e10) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1869, f546])).
fof(f1869, plain, (~ (h4(op1(e11, e10)) = op2(e21, h4(e10))) | ~ (h2(e10) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1868, f1697])).
fof(f1868, plain, (~ (h2(e10) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1867, f539])).
fof(f1867, plain, (~ (op2(e21, e21) = h4(op1(e11, e11))) | ~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1866, f1697])).
fof(f1866, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1865, f1697])).
fof(f1865, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1864, f1696])).
fof(f1864, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1863, f1697])).
fof(f1863, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1862, f546])).
fof(f1862, plain, (~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1861, f1696])).
fof(f1861, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1860, f1696])).
fof(f1860, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1859, f1697])).
fof(f1859, plain, (~ (h3(e10) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1858, f543])).
fof(f1858, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1857, f1696])).
fof(f1857, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1856, f1696])).
fof(f1856, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1855, f546])).
fof(f1855, plain, (~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1854, f546])).
fof(f1854, plain, (~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1853, f546])).
fof(f1853, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), e21)) | ~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1852, f1697])).
fof(f1852, plain, (~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1851, f546])).
fof(f1851, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), e22)) | ~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1850, f1696])).
fof(f1850, plain, (~ (h4(e10) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1849, f547])).
fof(f1849, plain, (~ (op2(e23, e23) = h4(op1(e13, e13))) | sP39 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1848, f546])).
fof(f1848, plain, (sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f1847, f1710])).
fof(f1710, plain, ~ sP40, inference(subsumption_resolution, [], [f555, f1697])).
fof(f555, plain, (~ (e21 = h4(e11)) | ~ sP40), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP40), inference(nnf_transformation, [], [f63])).
fof(f63, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP40), inference(usedef, [], [e63])).
fof(e63, plain, (sP40 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f1847, plain, (sP40 | sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f1846, f1707])).
fof(f1707, plain, ~ sP41, inference(subsumption_resolution, [], [f552, f1696])).
fof(f552, plain, (~ (e22 = h4(e12)) | ~ sP41), inference(cnf_transformation, [], [f96])).
fof(f96, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP41), inference(nnf_transformation, [], [f64])).
fof(f64, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP41), inference(usedef, [], [e64])).
fof(e64, plain, (sP41 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f1846, plain, (sP41 | sP40 | sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f613, f546])).
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
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', co1)).
fof(f1722, plain, (~ spl42_191 | ~ spl42_192), inference(avatar_split_clause, [], [f558, f1719, f1715])).
fof(f558, plain, (~ (e20 = h4(e10)) | ~ sP39), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP39), inference(nnf_transformation, [], [f62])).
fof(f1671, plain, spl42_65, inference(avatar_split_clause, [], [f531, f919])).
fof(f531, plain, (e20 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f1670, plain, spl42_1, inference(avatar_split_clause, [], [f528, f615])).
fof(f528, plain, (e10 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f1669, plain, (spl42_187 | spl42_188 | spl42_189 | spl42_190), inference(avatar_split_clause, [], [f518, f1666, f1662, f1658, f1654])).
fof(f518, plain, ((op2(e23, e20) = op2(op2(e23, e20), e23)) | (op2(e22, e20) = op2(op2(e22, e20), e22)) | (op2(e21, e20) = op2(op2(e21, e20), e21)) | (op2(e20, e20) = op2(op2(e20, e20), e20))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15) & ((op2(e23, e23) = op2(op2(e23, e23), e23)) | (op2(e22, e23) = op2(op2(e22, e23), e22)) | (op2(e21, e23) = op2(op2(e21, e23), e21)) | (op2(e20, e23) = op2(op2(e20, e23), e20))) & ((op2(e23, e22) = op2(op2(e23, e22), e23)) | (op2(e22, e22) = op2(op2(e22, e22), e22)) | (op2(e21, e22) = op2(op2(e21, e22), e21)) | (op2(e20, e22) = op2(op2(e20, e22), e20))) & ((op2(e23, e21) = op2(op2(e23, e21), e23)) | (op2(e22, e21) = op2(op2(e22, e21), e22)) | (op2(e21, e21) = op2(op2(e21, e21), e21)) | (op2(e20, e21) = op2(op2(e20, e21), e20))) & ((op2(e23, e20) = op2(op2(e23, e20), e23)) | (op2(e22, e20) = op2(op2(e22, e20), e22)) | (op2(e21, e20) = op2(op2(e21, e20), e21)) | (op2(e20, e20) = op2(op2(e20, e20), e20)))), inference(definition_folding, [], [f11, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37])).
fof(f37, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(usedef, [], [e37])).
fof(e37, plain, (sP15 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f38, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)) & (op2(e20, e20) = e21)) | ~ sP16), inference(usedef, [], [e38])).
fof(e38, plain, (sP16 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)) & (op2(e20, e20) = e21))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f39, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)) & (op2(e20, e20) = e22)) | ~ sP17), inference(usedef, [], [e39])).
fof(e39, plain, (sP17 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)) & (op2(e20, e20) = e22))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f40, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)) & (op2(e20, e20) = e23)) | ~ sP18), inference(usedef, [], [e40])).
fof(e40, plain, (sP18 <=> (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)) & (op2(e20, e20) = e23))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f41, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e21, e20)) & (e20 = op2(e21, e21))) | ~ sP19), inference(usedef, [], [e41])).
fof(e41, plain, (sP19 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f42, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(usedef, [], [e42])).
fof(e42, plain, (sP20 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f43, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e21 = op2(e21, e22)) & (e22 = op2(e21, e21))) | ~ sP21), inference(usedef, [], [e43])).
fof(e43, plain, (sP21 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f44, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e21 = op2(e21, e23)) & (e23 = op2(e21, e21))) | ~ sP22), inference(usedef, [], [e44])).
fof(e44, plain, (sP22 <=> (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f45, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e22, e20)) & (e20 = op2(e22, e22))) | ~ sP23), inference(usedef, [], [e45])).
fof(e45, plain, (sP23 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f46, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e22 = op2(e22, e21)) & (e21 = op2(e22, e22))) | ~ sP24), inference(usedef, [], [e46])).
fof(e46, plain, (sP24 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f47, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(usedef, [], [e47])).
fof(e47, plain, (sP25 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f48, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e22 = op2(e22, e23)) & (e23 = op2(e22, e22))) | ~ sP26), inference(usedef, [], [e48])).
fof(e48, plain, (sP26 <=> (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f49, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e23, e20)) & (e20 = op2(e23, e23))) | ~ sP27), inference(usedef, [], [e49])).
fof(e49, plain, (sP27 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f50, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e23 = op2(e23, e21)) & (e21 = op2(e23, e23))) | ~ sP28), inference(usedef, [], [e50])).
fof(e50, plain, (sP28 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f51, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e23 = op2(e23, e22)) & (e22 = op2(e23, e23))) | ~ sP29), inference(usedef, [], [e51])).
fof(e51, plain, (sP29 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e23 = op2(e23, e22)) & (e22 = op2(e23, e23))) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e23 = op2(e23, e21)) & (e21 = op2(e23, e23))) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e23, e20)) & (e20 = op2(e23, e23))) | (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e22 = op2(e22, e23)) & (e23 = op2(e22, e22))) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e22 = op2(e22, e21)) & (e21 = op2(e22, e22))) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e22, e20)) & (e20 = op2(e22, e22))) | (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e21 = op2(e21, e23)) & (e23 = op2(e21, e21))) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e21 = op2(e21, e22)) & (e22 = op2(e21, e21))) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e21, e20)) & (e20 = op2(e21, e21))) | (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)) & (op2(e20, e20) = e23)) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)) & (op2(e20, e20) = e22)) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)) & (op2(e20, e20) = e21)) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))) & ((op2(e23, e23) = op2(op2(e23, e23), e23)) | (op2(e22, e23) = op2(op2(e22, e23), e22)) | (op2(e21, e23) = op2(op2(e21, e23), e21)) | (op2(e20, e23) = op2(op2(e20, e23), e20))) & ((op2(e23, e22) = op2(op2(e23, e22), e23)) | (op2(e22, e22) = op2(op2(e22, e22), e22)) | (op2(e21, e22) = op2(op2(e21, e22), e21)) | (op2(e20, e22) = op2(op2(e20, e22), e20))) & ((op2(e23, e21) = op2(op2(e23, e21), e23)) | (op2(e22, e21) = op2(op2(e22, e21), e22)) | (op2(e21, e21) = op2(op2(e21, e21), e21)) | (op2(e20, e21) = op2(op2(e20, e21), e20))) & ((op2(e23, e20) = op2(op2(e23, e20), e23)) | (op2(e22, e20) = op2(op2(e22, e20), e22)) | (op2(e21, e20) = op2(op2(e21, e20), e21)) | (op2(e20, e20) = op2(op2(e20, e20), e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax11)).
fof(f1652, plain, (spl42_183 | spl42_184 | spl42_185 | spl42_186), inference(avatar_split_clause, [], [f519, f1649, f1645, f1641, f1637])).
fof(f519, plain, ((op2(e23, e21) = op2(op2(e23, e21), e23)) | (op2(e22, e21) = op2(op2(e22, e21), e22)) | (op2(e21, e21) = op2(op2(e21, e21), e21)) | (op2(e20, e21) = op2(op2(e20, e21), e20))), inference(cnf_transformation, [], [f52])).
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
fof(f931, plain, (spl42_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_68])])).
fof(f522, plain, ((e23 = op2(e23, e23)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f52])).
fof(f1598, plain, (spl42_174 | spl42_173 | spl42_172 | spl42_171 | spl42_170 | spl42_169 | spl42_168 | spl42_167 | spl42_166 | spl42_165 | spl42_164 | spl42_163 | spl42_162 | spl42_161 | spl42_160 | ~ spl42_74), inference(avatar_split_clause, [], [f525, f957, f1447, f1457, f1467, f1477, f1487, f1497, f1507, f1517, f1527, f1537, f1547, f1557, f1567, f1577, f1587])).
fof(f525, plain, (~ (e21 = op2(e23, e21)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f52])).
fof(f1596, plain, (spl42_174 | spl42_173 | spl42_172 | spl42_171 | spl42_170 | spl42_169 | spl42_168 | spl42_167 | spl42_166 | spl42_165 | spl42_164 | spl42_163 | spl42_162 | spl42_161 | spl42_160 | ~ spl42_68), inference(avatar_split_clause, [], [f527, f931, f1447, f1457, f1467, f1477, f1487, f1497, f1507, f1517, f1527, f1537, f1547, f1557, f1567, f1577, f1587])).
fof(f527, plain, (~ (e23 = op2(e23, e23)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f52])).
fof(f1595, plain, (~ spl42_174 | spl42_125), inference(avatar_split_clause, [], [f512, f1174, f1587])).
fof(f512, plain, ((e20 = op2(e20, e20)) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(nnf_transformation, [], [f37])).
fof(f1584, plain, (~ spl42_173 | spl42_121), inference(avatar_split_clause, [], [f507, f1157, f1577])).
fof(f507, plain, ((e20 = op2(e20, e21)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)) & (op2(e20, e20) = e21)) | ~ sP16), inference(nnf_transformation, [], [f38])).
fof(f1582, plain, (~ spl42_173 | ~ spl42_106), inference(avatar_split_clause, [], [f509, f1093, f1577])).
fof(f509, plain, (~ (e21 = op2(e21, e21)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f1575, plain, (~ spl42_172 | spl42_127), inference(avatar_split_clause, [], [f500, f1182, f1567])).
fof(f500, plain, ((op2(e20, e20) = e22) | ~ sP17), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)) & (op2(e20, e20) = e22)) | ~ sP17), inference(nnf_transformation, [], [f39])).
fof(f1565, plain, (~ spl42_171 | spl42_128), inference(avatar_split_clause, [], [f494, f1186, f1557])).
fof(f494, plain, ((op2(e20, e20) = e23) | ~ sP18), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)) & (op2(e20, e20) = e23)) | ~ sP18), inference(nnf_transformation, [], [f40])).
fof(f1554, plain, (~ spl42_170 | spl42_110), inference(avatar_split_clause, [], [f489, f1110, f1547])).
fof(f489, plain, ((e21 = op2(e21, e20)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e21 = op2(e21, e20)) & (e20 = op2(e21, e21))) | ~ sP19), inference(nnf_transformation, [], [f41])).
fof(f1545, plain, (~ spl42_169 | spl42_106), inference(avatar_split_clause, [], [f482, f1093, f1537])).
fof(f482, plain, ((e21 = op2(e21, e21)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(nnf_transformation, [], [f42])).
fof(f1543, plain, (~ spl42_169 | ~ spl42_109), inference(avatar_split_clause, [], [f484, f1106, f1537])).
fof(f484, plain, (~ (e20 = op2(e21, e20)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1542, plain, (~ spl42_169 | ~ spl42_106), inference(avatar_split_clause, [], [f485, f1093, f1537])).
fof(f485, plain, (~ (e21 = op2(e21, e21)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1535, plain, (~ spl42_168 | spl42_107), inference(avatar_split_clause, [], [f476, f1097, f1527])).
fof(f476, plain, ((e22 = op2(e21, e21)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e21 = op2(e21, e22)) & (e22 = op2(e21, e21))) | ~ sP21), inference(nnf_transformation, [], [f43])).
fof(f1534, plain, (~ spl42_168 | spl42_102), inference(avatar_split_clause, [], [f477, f1076, f1527])).
fof(f477, plain, ((e21 = op2(e21, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1531, plain, (~ spl42_168 | ~ spl42_87), inference(avatar_split_clause, [], [f480, f1012, f1527])).
fof(f480, plain, (~ (e22 = op2(e22, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1525, plain, (~ spl42_167 | spl42_108), inference(avatar_split_clause, [], [f470, f1101, f1517])).
fof(f470, plain, ((e23 = op2(e21, e21)) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e21 = op2(e21, e23)) & (e23 = op2(e21, e21))) | ~ sP22), inference(nnf_transformation, [], [f44])).
fof(f1524, plain, (~ spl42_167 | spl42_98), inference(avatar_split_clause, [], [f471, f1059, f1517])).
fof(f471, plain, ((e21 = op2(e21, e23)) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f1522, plain, (~ spl42_167 | ~ spl42_74), inference(avatar_split_clause, [], [f473, f957, f1517])).
fof(f473, plain, (~ (e21 = op2(e23, e21)) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f1515, plain, (~ spl42_166 | spl42_85), inference(avatar_split_clause, [], [f464, f1004, f1507])).
fof(f464, plain, ((e20 = op2(e22, e22)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e22 = op2(e22, e20)) & (e20 = op2(e22, e22))) | ~ sP23), inference(nnf_transformation, [], [f45])).
fof(f1514, plain, (~ spl42_166 | spl42_95), inference(avatar_split_clause, [], [f465, f1046, f1507])).
fof(f465, plain, ((e22 = op2(e22, e20)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1505, plain, (~ spl42_165 | spl42_86), inference(avatar_split_clause, [], [f458, f1008, f1497])).
fof(f458, plain, ((e21 = op2(e22, e22)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f86, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e22 = op2(e22, e21)) & (e21 = op2(e22, e22))) | ~ sP24), inference(nnf_transformation, [], [f46])).
fof(f1504, plain, (~ spl42_165 | spl42_91), inference(avatar_split_clause, [], [f459, f1029, f1497])).
fof(f459, plain, ((e22 = op2(e22, e21)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f1502, plain, (~ spl42_165 | ~ spl42_106), inference(avatar_split_clause, [], [f461, f1093, f1497])).
fof(f461, plain, (~ (e21 = op2(e21, e21)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f1501, plain, (~ spl42_165 | ~ spl42_103), inference(avatar_split_clause, [], [f462, f1080, f1497])).
fof(f462, plain, (~ (e22 = op2(e21, e22)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f1500, plain, (~ spl42_165 | ~ spl42_100), inference(avatar_split_clause, [], [f463, f1067, f1497])).
fof(f463, plain, (~ (e23 = op2(e21, e23)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f1495, plain, (~ spl42_164 | spl42_87), inference(avatar_split_clause, [], [f452, f1012, f1487])).
fof(f452, plain, ((e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(nnf_transformation, [], [f47])).
fof(f1493, plain, (~ spl42_164 | ~ spl42_93), inference(avatar_split_clause, [], [f454, f1038, f1487])).
fof(f454, plain, (~ (e20 = op2(e22, e20)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f1491, plain, (~ spl42_164 | ~ spl42_87), inference(avatar_split_clause, [], [f456, f1012, f1487])).
fof(f456, plain, (~ (e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f1484, plain, (~ spl42_163 | spl42_83), inference(avatar_split_clause, [], [f447, f995, f1477])).
fof(f447, plain, ((e22 = op2(e22, e23)) | ~ sP26), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & (e22 = op2(e22, e23)) & (e23 = op2(e22, e22))) | ~ sP26), inference(nnf_transformation, [], [f48])).
fof(f1474, plain, (~ spl42_162 | spl42_80), inference(avatar_split_clause, [], [f441, f982, f1467])).
fof(f441, plain, ((e23 = op2(e23, e20)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & (e23 = op2(e23, e20)) & (e20 = op2(e23, e23))) | ~ sP27), inference(nnf_transformation, [], [f49])).
fof(f1465, plain, (~ spl42_161 | spl42_66), inference(avatar_split_clause, [], [f434, f923, f1457])).
fof(f434, plain, ((e21 = op2(e23, e23)) | ~ sP28), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & (e23 = op2(e23, e21)) & (e21 = op2(e23, e23))) | ~ sP28), inference(nnf_transformation, [], [f50])).
fof(f1455, plain, (~ spl42_160 | spl42_67), inference(avatar_split_clause, [], [f428, f927, f1447])).
fof(f428, plain, ((e22 = op2(e23, e23)) | ~ sP29), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & (e23 = op2(e23, e22)) & (e22 = op2(e23, e23))) | ~ sP29), inference(nnf_transformation, [], [f51])).
fof(f1445, plain, (spl42_156 | spl42_157 | spl42_158 | spl42_159), inference(avatar_split_clause, [], [f418, f1442, f1438, f1434, f1430])).
fof(f418, plain, ((op1(e13, e10) = op1(op1(e13, e10), e13)) | (op1(e12, e10) = op1(op1(e12, e10), e12)) | (op1(e11, e10) = op1(op1(e11, e10), e11)) | (op1(e10, e10) = op1(op1(e10, e10), e10))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((op1(e13, e13) = op1(op1(e13, e13), e13)) | (op1(e12, e13) = op1(op1(e12, e13), e12)) | (op1(e11, e13) = op1(op1(e11, e13), e11)) | (op1(e10, e13) = op1(op1(e10, e13), e10))) & ((op1(e13, e12) = op1(op1(e13, e12), e13)) | (op1(e12, e12) = op1(op1(e12, e12), e12)) | (op1(e11, e12) = op1(op1(e11, e12), e11)) | (op1(e10, e12) = op1(op1(e10, e12), e10))) & ((op1(e13, e11) = op1(op1(e13, e11), e13)) | (op1(e12, e11) = op1(op1(e12, e11), e12)) | (op1(e11, e11) = op1(op1(e11, e11), e11)) | (op1(e10, e11) = op1(op1(e10, e11), e10))) & ((op1(e13, e10) = op1(op1(e13, e10), e13)) | (op1(e12, e10) = op1(op1(e12, e10), e12)) | (op1(e11, e10) = op1(op1(e11, e10), e11)) | (op1(e10, e10) = op1(op1(e10, e10), e10)))), inference(definition_folding, [], [f10, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)) & (op1(e10, e10) = e11)) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)) & (op1(e10, e10) = e11))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)) & (op1(e10, e10) = e12)) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)) & (op1(e10, e10) = e12))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)) & (op1(e10, e10) = e13)) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)) & (op1(e10, e10) = e13))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e11, e10)) & (e10 = op1(e11, e11))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e11 = op1(e11, e12)) & (e12 = op1(e11, e11))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e11 = op1(e11, e13)) & (e13 = op1(e11, e11))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e12, e10)) & (e10 = op1(e12, e12))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e12 = op1(e12, e11)) & (e11 = op1(e12, e12))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e12 = op1(e12, e13)) & (e13 = op1(e12, e12))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e13, e10)) & (e10 = op1(e13, e13))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e13 = op1(e13, e11)) & (e11 = op1(e13, e13))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e13 = op1(e13, e12)) & (e12 = op1(e13, e13))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e13 = op1(e13, e12)) & (e12 = op1(e13, e13))) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e13 = op1(e13, e11)) & (e11 = op1(e13, e13))) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e13, e10)) & (e10 = op1(e13, e13))) | (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e12 = op1(e12, e13)) & (e13 = op1(e12, e12))) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e12 = op1(e12, e11)) & (e11 = op1(e12, e12))) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e12, e10)) & (e10 = op1(e12, e12))) | (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e11 = op1(e11, e13)) & (e13 = op1(e11, e11))) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e11 = op1(e11, e12)) & (e12 = op1(e11, e11))) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e11, e10)) & (e10 = op1(e11, e11))) | (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)) & (op1(e10, e10) = e13)) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)) & (op1(e10, e10) = e12)) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)) & (op1(e10, e10) = e11)) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))) & ((op1(e13, e13) = op1(op1(e13, e13), e13)) | (op1(e12, e13) = op1(op1(e12, e13), e12)) | (op1(e11, e13) = op1(op1(e11, e13), e11)) | (op1(e10, e13) = op1(op1(e10, e13), e10))) & ((op1(e13, e12) = op1(op1(e13, e12), e13)) | (op1(e12, e12) = op1(op1(e12, e12), e12)) | (op1(e11, e12) = op1(op1(e11, e12), e11)) | (op1(e10, e12) = op1(op1(e10, e12), e10))) & ((op1(e13, e11) = op1(op1(e13, e11), e13)) | (op1(e12, e11) = op1(op1(e12, e11), e12)) | (op1(e11, e11) = op1(op1(e11, e11), e11)) | (op1(e10, e11) = op1(op1(e10, e11), e10))) & ((op1(e13, e10) = op1(op1(e13, e10), e13)) | (op1(e12, e10) = op1(op1(e12, e10), e12)) | (op1(e11, e10) = op1(op1(e11, e10), e11)) | (op1(e10, e10) = op1(op1(e10, e10), e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax10)).
fof(f1428, plain, (spl42_152 | spl42_153 | spl42_154 | spl42_155), inference(avatar_split_clause, [], [f419, f1425, f1421, f1417, f1413])).
fof(f419, plain, ((op1(e13, e11) = op1(op1(e13, e11), e13)) | (op1(e12, e11) = op1(op1(e12, e11), e12)) | (op1(e11, e11) = op1(op1(e11, e11), e11)) | (op1(e10, e11) = op1(op1(e10, e11), e10))), inference(cnf_transformation, [], [f36])).
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
fof(f1374, plain, (spl42_143 | spl42_142 | spl42_141 | spl42_140 | spl42_139 | spl42_138 | spl42_137 | spl42_136 | spl42_135 | spl42_134 | spl42_133 | spl42_132 | spl42_131 | spl42_130 | spl42_129 | ~ spl42_10), inference(avatar_split_clause, [], [f425, f653, f1223, f1233, f1243, f1253, f1263, f1273, f1283, f1293, f1303, f1313, f1323, f1333, f1343, f1353, f1363])).
fof(f425, plain, (~ (e11 = op1(e13, e11)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1372, plain, (spl42_143 | spl42_142 | spl42_141 | spl42_140 | spl42_139 | spl42_138 | spl42_137 | spl42_136 | spl42_135 | spl42_134 | spl42_133 | spl42_132 | spl42_131 | spl42_130 | spl42_129 | ~ spl42_4), inference(avatar_split_clause, [], [f427, f627, f1223, f1233, f1243, f1253, f1263, f1273, f1283, f1293, f1303, f1313, f1323, f1333, f1343, f1353, f1363])).
fof(f427, plain, (~ (e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1371, plain, (~ spl42_143 | spl42_61), inference(avatar_split_clause, [], [f412, f870, f1363])).
fof(f412, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1357, plain, (~ spl42_142 | ~ spl42_39), inference(avatar_split_clause, [], [f410, f776, f1353])).
fof(f410, plain, (~ (e12 = op1(e11, e12)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)) & (op1(e10, e10) = e11)) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1356, plain, (~ spl42_142 | ~ spl42_36), inference(avatar_split_clause, [], [f411, f763, f1353])).
fof(f411, plain, (~ (e13 = op1(e11, e13)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f1351, plain, (~ spl42_141 | spl42_63), inference(avatar_split_clause, [], [f400, f878, f1343])).
fof(f400, plain, ((op1(e10, e10) = e12) | ~ sP2), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)) & (op1(e10, e10) = e12)) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1341, plain, (~ spl42_140 | spl42_64), inference(avatar_split_clause, [], [f394, f882, f1333])).
fof(f394, plain, ((op1(e10, e10) = e13) | ~ sP3), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)) & (op1(e10, e10) = e13)) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f1330, plain, (~ spl42_139 | spl42_46), inference(avatar_split_clause, [], [f389, f806, f1323])).
fof(f389, plain, ((e11 = op1(e11, e10)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e11 = op1(e11, e10)) & (e10 = op1(e11, e11))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f1321, plain, (~ spl42_138 | spl42_42), inference(avatar_split_clause, [], [f382, f789, f1313])).
fof(f382, plain, ((e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f1318, plain, (~ spl42_138 | ~ spl42_42), inference(avatar_split_clause, [], [f385, f789, f1313])).
fof(f385, plain, (~ (e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f1311, plain, (~ spl42_137 | spl42_43), inference(avatar_split_clause, [], [f376, f793, f1303])).
fof(f376, plain, ((e12 = op1(e11, e11)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e11 = op1(e11, e12)) & (e12 = op1(e11, e11))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f1307, plain, (~ spl42_137 | ~ spl42_23), inference(avatar_split_clause, [], [f380, f708, f1303])).
fof(f380, plain, (~ (e12 = op1(e12, e12)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1300, plain, (~ spl42_136 | spl42_34), inference(avatar_split_clause, [], [f371, f755, f1293])).
fof(f371, plain, ((e11 = op1(e11, e13)) | ~ sP7), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e11 = op1(e11, e13)) & (e13 = op1(e11, e11))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f1298, plain, (~ spl42_136 | ~ spl42_10), inference(avatar_split_clause, [], [f373, f653, f1293])).
fof(f373, plain, (~ (e11 = op1(e13, e11)) | ~ sP7), inference(cnf_transformation, [], [f73])).
fof(f1290, plain, (~ spl42_135 | spl42_31), inference(avatar_split_clause, [], [f365, f742, f1283])).
fof(f365, plain, ((e12 = op1(e12, e10)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e12 = op1(e12, e10)) & (e10 = op1(e12, e12))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f1281, plain, (~ spl42_134 | spl42_22), inference(avatar_split_clause, [], [f358, f704, f1273])).
fof(f358, plain, ((e11 = op1(e12, e12)) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e12 = op1(e12, e11)) & (e11 = op1(e12, e12))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f1277, plain, (~ spl42_134 | ~ spl42_39), inference(avatar_split_clause, [], [f362, f776, f1273])).
fof(f362, plain, (~ (e12 = op1(e11, e12)) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f1271, plain, (~ spl42_133 | spl42_23), inference(avatar_split_clause, [], [f352, f708, f1263])).
fof(f352, plain, ((e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f1267, plain, (~ spl42_133 | ~ spl42_23), inference(avatar_split_clause, [], [f356, f708, f1263])).
fof(f356, plain, (~ (e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f1260, plain, (~ spl42_132 | spl42_19), inference(avatar_split_clause, [], [f347, f691, f1253])).
fof(f347, plain, ((e12 = op1(e12, e13)) | ~ sP11), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & (e12 = op1(e12, e13)) & (e13 = op1(e12, e12))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f1250, plain, (~ spl42_131 | spl42_16), inference(avatar_split_clause, [], [f341, f678, f1243])).
fof(f341, plain, ((e13 = op1(e13, e10)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & (e13 = op1(e13, e10)) & (e10 = op1(e13, e13))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f1241, plain, (~ spl42_130 | spl42_2), inference(avatar_split_clause, [], [f334, f619, f1233])).
fof(f334, plain, ((e11 = op1(e13, e13)) | ~ sP13), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & (e13 = op1(e13, e11)) & (e11 = op1(e13, e13))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f1231, plain, (~ spl42_129 | spl42_3), inference(avatar_split_clause, [], [f328, f623, f1223])).
fof(f328, plain, ((e12 = op1(e13, e13)) | ~ sP14), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & (e13 = op1(e13, e12)) & (e12 = op1(e13, e13))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f1221, plain, (spl42_125 | spl42_121 | spl42_117 | spl42_113), inference(avatar_split_clause, [], [f172, f1123, f1140, f1157, f1174])).
fof(f172, plain, ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax4)).
fof(f1215, plain, (spl42_128 | spl42_124 | spl42_120 | spl42_116), inference(avatar_split_clause, [], [f178, f1135, f1152, f1169, f1186])).
fof(f178, plain, ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1214, plain, (spl42_128 | spl42_112 | spl42_96 | spl42_80), inference(avatar_split_clause, [], [f179, f982, f1050, f1118, f1186])).
fof(f179, plain, ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
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
fof(f1208, plain, (spl42_123 | spl42_107 | spl42_91 | spl42_75), inference(avatar_split_clause, [], [f185, f961, f1029, f1097, f1165])).
fof(f185, plain, ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1206, plain, (spl42_124 | spl42_108 | spl42_92 | spl42_76), inference(avatar_split_clause, [], [f187, f965, f1033, f1101, f1169])).
fof(f187, plain, ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1205, plain, (spl42_93 | spl42_89 | spl42_85 | spl42_81), inference(avatar_split_clause, [], [f188, f987, f1004, f1021, f1038])).
fof(f188, plain, ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1204, plain, (spl42_117 | spl42_101 | spl42_85 | spl42_69), inference(avatar_split_clause, [], [f189, f936, f1004, f1072, f1140])).
fof(f189, plain, ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1203, plain, (spl42_94 | spl42_90 | spl42_86 | spl42_82), inference(avatar_split_clause, [], [f190, f991, f1008, f1025, f1042])).
fof(f190, plain, ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1201, plain, (spl42_95 | spl42_91 | spl42_87 | spl42_83), inference(avatar_split_clause, [], [f192, f995, f1012, f1029, f1046])).
fof(f192, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1200, plain, (spl42_119 | spl42_103 | spl42_87 | spl42_71), inference(avatar_split_clause, [], [f193, f944, f1012, f1080, f1148])).
fof(f193, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1199, plain, (spl42_96 | spl42_92 | spl42_88 | spl42_84), inference(avatar_split_clause, [], [f194, f999, f1016, f1033, f1050])).
fof(f194, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1195, plain, (spl42_78 | spl42_74 | spl42_70 | spl42_66), inference(avatar_split_clause, [], [f198, f923, f940, f957, f974])).
fof(f198, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1194, plain, (spl42_114 | spl42_98 | spl42_82 | spl42_66), inference(avatar_split_clause, [], [f199, f923, f991, f1059, f1127])).
fof(f199, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1172, plain, (spl42_121 | spl42_122 | spl42_123 | spl42_124), inference(avatar_split_clause, [], [f157, f1169, f1165, f1161, f1157])).
fof(f157, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax3)).
fof(f1155, plain, (spl42_117 | spl42_118 | spl42_119 | spl42_120), inference(avatar_split_clause, [], [f158, f1152, f1148, f1144, f1140])).
fof(f158, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f1121, plain, (spl42_109 | spl42_110 | spl42_111 | spl42_112), inference(avatar_split_clause, [], [f160, f1118, f1114, f1110, f1106])).
fof(f160, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1104, plain, (spl42_105 | spl42_106 | spl42_107 | spl42_108), inference(avatar_split_clause, [], [f161, f1101, f1097, f1093, f1089])).
fof(f161, plain, ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))), inference(cnf_transformation, [], [f3])).
fof(f1070, plain, (spl42_97 | spl42_98 | spl42_99 | spl42_100), inference(avatar_split_clause, [], [f163, f1067, f1063, f1059, f1055])).
fof(f163, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1053, plain, (spl42_93 | spl42_94 | spl42_95 | spl42_96), inference(avatar_split_clause, [], [f164, f1050, f1046, f1042, f1038])).
fof(f164, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f1002, plain, (spl42_81 | spl42_82 | spl42_83 | spl42_84), inference(avatar_split_clause, [], [f167, f999, f995, f991, f987])).
fof(f167, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f985, plain, (spl42_77 | spl42_78 | spl42_79 | spl42_80), inference(avatar_split_clause, [], [f168, f982, f978, f974, f970])).
fof(f168, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f968, plain, (spl42_73 | spl42_74 | spl42_75 | spl42_76), inference(avatar_split_clause, [], [f169, f965, f961, f957, f953])).
fof(f169, plain, ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))), inference(cnf_transformation, [], [f3])).
fof(f951, plain, (spl42_69 | spl42_70 | spl42_71 | spl42_72), inference(avatar_split_clause, [], [f170, f948, f944, f940, f936])).
fof(f170, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f912, plain, (spl42_63 | spl42_47 | spl42_31 | spl42_15), inference(avatar_split_clause, [], [f129, f674, f742, f810, f878])).
fof(f129, plain, ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax2)).
fof(f911, plain, (spl42_64 | spl42_60 | spl42_56 | spl42_52), inference(avatar_split_clause, [], [f130, f831, f848, f865, f882])).
fof(f130, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f909, plain, (spl42_45 | spl42_41 | spl42_37 | spl42_33), inference(avatar_split_clause, [], [f132, f751, f768, f785, f802])).
fof(f132, plain, ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f907, plain, (spl42_46 | spl42_42 | spl42_38 | spl42_34), inference(avatar_split_clause, [], [f134, f755, f772, f789, f806])).
fof(f134, plain, ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f906, plain, (spl42_58 | spl42_42 | spl42_26 | spl42_10), inference(avatar_split_clause, [], [f135, f653, f721, f789, f857])).
fof(f135, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f904, plain, (spl42_59 | spl42_43 | spl42_27 | spl42_11), inference(avatar_split_clause, [], [f137, f657, f725, f793, f861])).
fof(f137, plain, ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f902, plain, (spl42_60 | spl42_44 | spl42_28 | spl42_12), inference(avatar_split_clause, [], [f139, f661, f729, f797, f865])).
fof(f139, plain, ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f900, plain, (spl42_53 | spl42_37 | spl42_21 | spl42_5), inference(avatar_split_clause, [], [f141, f632, f700, f768, f836])).
fof(f141, plain, ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f898, plain, (spl42_54 | spl42_38 | spl42_22 | spl42_6), inference(avatar_split_clause, [], [f143, f636, f704, f772, f840])).
fof(f143, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f897, plain, (spl42_31 | spl42_27 | spl42_23 | spl42_19), inference(avatar_split_clause, [], [f144, f691, f708, f725, f742])).
fof(f144, plain, ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f896, plain, (spl42_55 | spl42_39 | spl42_23 | spl42_7), inference(avatar_split_clause, [], [f145, f640, f708, f776, f844])).
fof(f145, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f891, plain, (spl42_14 | spl42_10 | spl42_6 | spl42_2), inference(avatar_split_clause, [], [f150, f619, f636, f653, f670])).
fof(f150, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f890, plain, (spl42_50 | spl42_34 | spl42_18 | spl42_2), inference(avatar_split_clause, [], [f151, f619, f687, f755, f823])).
fof(f151, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f889, plain, (spl42_15 | spl42_11 | spl42_7 | spl42_3), inference(avatar_split_clause, [], [f152, f623, f640, f657, f674])).
fof(f152, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f817, plain, (spl42_45 | spl42_46 | spl42_47 | spl42_48), inference(avatar_split_clause, [], [f112, f814, f810, f806, f802])).
fof(f112, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG108+1.p', ax1)).
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
fof(f664, plain, (spl42_9 | spl42_10 | spl42_11 | spl42_12), inference(avatar_split_clause, [], [f121, f661, f657, f653, f649])).
fof(f121, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))), inference(cnf_transformation, [], [f1])).