fof(f7415, plain, $false, inference(avatar_sat_refutation, [], [f1220, f1254, f1271, f1305, f1322, f1339, f1356, f1373, f1390, f1407, f1424, f1441, f1446, f1448, f1449, f1451, f1453, f1456, f1459, f1460, f1461, f1463, f1466, f1467, f1471, f1473, f1558, f1575, f1609, f1626, f1643, f1694, f1711, f1728, f1745, f1747, f1752, f1753, f1754, f1757, f1764, f1765, f1766, f1770, f1771, f1773, f1774, f1777, f2287, f2294, f2304, f2343, f2360, f2377, f2381, f2891, f2898, f2908, f2947, f2964, f2981, f2985, f2986, f2988, f3039, f3053, f3067, f3282, f3673, f3686, f3702, f3724, f3740, f3748, f3760, f3763, f3778, f3804, f3820, f3830, f3865, f3898, f3904, f3955, f3956, f3959, f3965, f3966, f4015, f4022, f4027, f4056, f4058, f4059, f4061, f4126, f4134, f4162, f4163, f4171, f4174, f4225, f4237, f4256, f4281, f4298, f4306, f4315, f4320, f4323, f4324, f4347, f4353, f4355, f4359, f4369, f4373, f4388, f4392, f4394, f4395, f4398, f4407, f4410, f4423, f4491, f4496, f4512, f4514, f4517, f4525, f4535, f4562, f4604, f4614, f4618, f4620, f4633, f4638, f4665, f4670, f4681, f4694, f4736, f4741, f4760, f4770, f4781, f4809, f4844, f4845, f4849, f4875, f4902, f4905, f4910, f4950, f4995, f5002, f5003, f5015, f5042, f5079, f5150, f5168, f5204, f5229, f5241, f5257, f5269, f5295, f5302, f5304, f5347, f5351, f5365, f5403, f5404, f5426, f5451, f5473, f5495, f5552, f5568, f5610, f5662, f5691, f5696, f5743, f5858, f5899, f5906, f5912, f5936, f5954, f6048, f6052, f6158, f6159, f6220, f6227, f6231, f6238, f6251, f6394, f6414, f6428, f6448, f6454, f6465, f6500, f6503, f6516, f6537, f6562, f6636, f6710, f6725, f6739, f6742, f6746, f6748, f6750, f6821, f6822, f6877, f6882, f6912, f6925, f6926, f6936, f6947, f7042, f7043, f7080, f7081, f7105, f7344, f7384, f7388, f7410])).
fof(f7410, plain, (spl144_102 | ~ spl144_107 | ~ spl144_286), inference(avatar_split_clause, [], [f7409, f2953, f1653, f1632])).
fof(f1632, plain, (spl144_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_102])])).
fof(f1653, plain, (spl144_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_107])])).
fof(f2953, plain, (spl144_286 <=> (e21 = op2(e21, op2(e21, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl144_286])])).
fof(f7409, plain, ((e21 = op2(e21, e22)) | (~ spl144_107 | ~ spl144_286)), inference(forward_demodulation, [], [f2955, f1655])).
fof(f1655, plain, ((e22 = op2(e21, e21)) | ~ spl144_107), inference(avatar_component_clause, [], [f1653])).
fof(f2955, plain, ((e21 = op2(e21, op2(e21, e21))) | ~ spl144_286), inference(avatar_component_clause, [], [f2953])).
fof(f7388, plain, (~ spl144_102 | ~ spl144_70), inference(avatar_split_clause, [], [f4239, f1496, f1632])).
fof(f1496, plain, (spl144_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_70])])).
fof(f4239, plain, (~ (e21 = op2(e21, e22)) | ~ spl144_70), inference(forward_demodulation, [], [f472, f1498])).
fof(f1498, plain, ((e21 = op2(e23, e22)) | ~ spl144_70), inference(avatar_component_clause, [], [f1496])).
fof(f472, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax6)).
fof(f7384, plain, (~ spl144_97 | ~ spl144_110 | ~ spl144_288), inference(avatar_contradiction_clause, [], [f7383])).
fof(f7383, plain, ($false | (~ spl144_97 | ~ spl144_110 | ~ spl144_288)), inference(subsumption_resolution, [], [f7382, f514])).
fof(f514, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax8)).
fof(f7382, plain, ((e21 = e23) | (~ spl144_97 | ~ spl144_110 | ~ spl144_288)), inference(forward_demodulation, [], [f7381, f1668])).
fof(f1668, plain, ((e21 = op2(e21, e20)) | ~ spl144_110), inference(avatar_component_clause, [], [f1666])).
fof(f1666, plain, (spl144_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_110])])).
fof(f7381, plain, ((e23 = op2(e21, e20)) | (~ spl144_97 | ~ spl144_288)), inference(forward_demodulation, [], [f2963, f1613])).
fof(f1613, plain, ((e20 = op2(e21, e23)) | ~ spl144_97), inference(avatar_component_clause, [], [f1611])).
fof(f1611, plain, (spl144_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_97])])).
fof(f2963, plain, ((e23 = op2(e21, op2(e21, e23))) | ~ spl144_288), inference(avatar_component_clause, [], [f2961])).
fof(f2961, plain, (spl144_288 <=> (e23 = op2(e21, op2(e21, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl144_288])])).
fof(f7344, plain, (~ spl144_97 | ~ spl144_104 | ~ spl144_287), inference(avatar_contradiction_clause, [], [f7343])).
fof(f7343, plain, ($false | (~ spl144_97 | ~ spl144_104 | ~ spl144_287)), inference(subsumption_resolution, [], [f7342, f511])).
fof(f511, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f7342, plain, ((e20 = e22) | (~ spl144_97 | ~ spl144_104 | ~ spl144_287)), inference(forward_demodulation, [], [f7341, f1613])).
fof(f7341, plain, ((e22 = op2(e21, e23)) | (~ spl144_104 | ~ spl144_287)), inference(forward_demodulation, [], [f2959, f1642])).
fof(f1642, plain, ((e23 = op2(e21, e22)) | ~ spl144_104), inference(avatar_component_clause, [], [f1640])).
fof(f1640, plain, (spl144_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_104])])).
fof(f2959, plain, ((e22 = op2(e21, op2(e21, e22))) | ~ spl144_287), inference(avatar_component_clause, [], [f2957])).
fof(f2957, plain, (spl144_287 <=> (e22 = op2(e21, op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_287])])).
fof(f7105, plain, (~ spl144_43 | ~ spl144_47), inference(avatar_split_clause, [], [f7098, f1366, f1349])).
fof(f1349, plain, (spl144_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_43])])).
fof(f1366, plain, (spl144_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_47])])).
fof(f7098, plain, (~ (e12 = op1(e11, e11)) | ~ spl144_47), inference(backward_demodulation, [], [f438, f1368])).
fof(f1368, plain, ((e12 = op1(e11, e10)) | ~ spl144_47), inference(avatar_component_clause, [], [f1366])).
fof(f438, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax5)).
fof(f7081, plain, (~ spl144_58 | ~ spl144_62), inference(avatar_split_clause, [], [f7073, f1430, f1413])).
fof(f1413, plain, (spl144_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_58])])).
fof(f1430, plain, (spl144_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl144_62])])).
fof(f7073, plain, (~ (e11 = op1(e10, e11)) | ~ spl144_62), inference(backward_demodulation, [], [f432, f1432])).
fof(f1432, plain, ((op1(e10, e10) = e11) | ~ spl144_62), inference(avatar_component_clause, [], [f1430])).
fof(f432, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f7080, plain, (~ spl144_46 | ~ spl144_62), inference(avatar_split_clause, [], [f7072, f1430, f1362])).
fof(f1362, plain, (spl144_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_46])])).
fof(f7072, plain, (~ (e11 = op1(e11, e10)) | ~ spl144_62), inference(backward_demodulation, [], [f408, f1432])).
fof(f408, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f7043, plain, (~ spl144_123 | ~ spl144_316), inference(avatar_split_clause, [], [f7030, f3156, f1721])).
fof(f1721, plain, (spl144_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_123])])).
fof(f3156, plain, (spl144_316 <=> (e22 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_316])])).
fof(f7030, plain, (~ (e22 = op2(e20, e21)) | ~ spl144_316), inference(backward_demodulation, [], [f2993, f3157])).
fof(f3157, plain, ((e22 = h1(e12)) | ~ spl144_316), inference(avatar_component_clause, [], [f3156])).
fof(f2993, plain, ~ (op2(e20, e21) = h1(e12)), inference(backward_demodulation, [], [f480, f1093])).
fof(f1093, plain, (op2(e20, e20) = h1(e12)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, e20) = h1(e12)) & (op2(e20, op2(e20, e20)) = h1(e11)) & (h1(e10) = op2(op2(e20, e20), op2(e20, op2(e20, e20)))) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax14)).
fof(f480, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f7042, plain, (~ spl144_111 | ~ spl144_316), inference(avatar_split_clause, [], [f7029, f3156, f1670])).
fof(f1670, plain, (spl144_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_111])])).
fof(f7029, plain, (~ (e22 = op2(e21, e20)) | ~ spl144_316), inference(backward_demodulation, [], [f2990, f3157])).
fof(f2990, plain, ~ (op2(e21, e20) = h1(e12)), inference(backward_demodulation, [], [f456, f1093])).
fof(f456, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f6947, plain, (spl144_41 | ~ spl144_46 | ~ spl144_203), inference(avatar_contradiction_clause, [], [f6946])).
fof(f6946, plain, ($false | (spl144_41 | ~ spl144_46 | ~ spl144_203)), inference(subsumption_resolution, [], [f6940, f1342])).
fof(f1342, plain, (~ (e10 = op1(e11, e11)) | spl144_41), inference(avatar_component_clause, [], [f1341])).
fof(f1341, plain, (spl144_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_41])])).
fof(f6940, plain, ((e10 = op1(e11, e11)) | (~ spl144_46 | ~ spl144_203)), inference(backward_demodulation, [], [f2347, f1364])).
fof(f1364, plain, ((e11 = op1(e11, e10)) | ~ spl144_46), inference(avatar_component_clause, [], [f1362])).
fof(f2347, plain, ((e10 = op1(e11, op1(e11, e10))) | ~ spl144_203), inference(avatar_component_clause, [], [f2345])).
fof(f2345, plain, (spl144_203 <=> (e10 = op1(e11, op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_203])])).
fof(f6936, plain, (~ spl144_58 | spl144_208), inference(avatar_contradiction_clause, [], [f6935])).
fof(f6935, plain, ($false | (~ spl144_58 | spl144_208)), inference(subsumption_resolution, [], [f6930, f1415])).
fof(f1415, plain, ((e11 = op1(e10, e11)) | ~ spl144_58), inference(avatar_component_clause, [], [f1413])).
fof(f6930, plain, (~ (e11 = op1(e10, e11)) | (~ spl144_58 | spl144_208)), inference(backward_demodulation, [], [f2367, f1415])).
fof(f2367, plain, (~ (e11 = op1(e10, op1(e10, e11))) | spl144_208), inference(avatar_component_clause, [], [f2366])).
fof(f2366, plain, (spl144_208 <=> (e11 = op1(e10, op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_208])])).
fof(f6926, plain, (~ spl144_59 | ~ spl144_63), inference(avatar_split_clause, [], [f6919, f1434, f1417])).
fof(f1417, plain, (spl144_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_59])])).
fof(f1434, plain, (spl144_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl144_63])])).
fof(f6919, plain, (~ (e12 = op1(e10, e11)) | ~ spl144_63), inference(backward_demodulation, [], [f432, f1436])).
fof(f1436, plain, ((op1(e10, e10) = e12) | ~ spl144_63), inference(avatar_component_clause, [], [f1434])).
fof(f6925, plain, (~ spl144_47 | ~ spl144_63), inference(avatar_split_clause, [], [f6918, f1434, f1366])).
fof(f6918, plain, (~ (e12 = op1(e11, e10)) | ~ spl144_63), inference(backward_demodulation, [], [f408, f1436])).
fof(f6912, plain, (~ spl144_84 | ~ spl144_96), inference(avatar_split_clause, [], [f6910, f1606, f1555])).
fof(f1555, plain, (spl144_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_84])])).
fof(f1606, plain, (spl144_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_96])])).
fof(f6910, plain, (~ (e23 = op2(e22, e23)) | ~ spl144_96), inference(backward_demodulation, [], [f494, f1608])).
fof(f1608, plain, ((e23 = op2(e22, e20)) | ~ spl144_96), inference(avatar_component_clause, [], [f1606])).
fof(f494, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f6882, plain, (~ spl144_84 | ~ spl144_116), inference(avatar_split_clause, [], [f6880, f1691, f1555])).
fof(f1691, plain, (spl144_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_116])])).
fof(f6880, plain, (~ (e23 = op2(e22, e23)) | ~ spl144_116), inference(backward_demodulation, [], [f475, f1693])).
fof(f1693, plain, ((e23 = op2(e20, e23)) | ~ spl144_116), inference(avatar_component_clause, [], [f1691])).
fof(f475, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f6877, plain, (~ spl144_113 | ~ spl144_117), inference(avatar_split_clause, [], [f6871, f1696, f1679])).
fof(f1679, plain, (spl144_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_113])])).
fof(f1696, plain, (spl144_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_117])])).
fof(f6871, plain, (~ (e20 = op2(e20, e23)) | ~ spl144_117), inference(backward_demodulation, [], [f485, f1698])).
fof(f1698, plain, ((e20 = op2(e20, e22)) | ~ spl144_117), inference(avatar_component_clause, [], [f1696])).
fof(f485, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f6822, plain, (~ spl144_122 | ~ spl144_320), inference(avatar_split_clause, [], [f6810, f3176, f1717])).
fof(f1717, plain, (spl144_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_122])])).
fof(f3176, plain, (spl144_320 <=> (e21 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_320])])).
fof(f6810, plain, (~ (e21 = op2(e20, e21)) | ~ spl144_320), inference(backward_demodulation, [], [f2993, f3177])).
fof(f3177, plain, ((e21 = h1(e12)) | ~ spl144_320), inference(avatar_component_clause, [], [f3176])).
fof(f6821, plain, (~ spl144_94 | ~ spl144_320), inference(avatar_split_clause, [], [f6809, f3176, f1598])).
fof(f1598, plain, (spl144_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_94])])).
fof(f6809, plain, (~ (e21 = op2(e22, e20)) | ~ spl144_320), inference(backward_demodulation, [], [f2991, f3177])).
fof(f2991, plain, ~ (op2(e22, e20) = h1(e12)), inference(backward_demodulation, [], [f457, f1093])).
fof(f457, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f6750, plain, (~ spl144_124 | ~ spl144_76), inference(avatar_split_clause, [], [f6117, f1521, f1725])).
fof(f1725, plain, (spl144_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_124])])).
fof(f1521, plain, (spl144_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_76])])).
fof(f6117, plain, (~ (e23 = op2(e20, e21)) | ~ spl144_76), inference(forward_demodulation, [], [f464, f1523])).
fof(f1523, plain, ((e23 = op2(e23, e21)) | ~ spl144_76), inference(avatar_component_clause, [], [f1521])).
fof(f464, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f6748, plain, (~ spl144_103 | ~ spl144_87), inference(avatar_split_clause, [], [f6747, f1568, f1636])).
fof(f1636, plain, (spl144_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_103])])).
fof(f1568, plain, (spl144_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_87])])).
fof(f6747, plain, (~ (e22 = op2(e21, e22)) | ~ spl144_87), inference(forward_demodulation, [], [f3009, f6456])).
fof(f6456, plain, ((e22 = h3(e12)) | ~ spl144_87), inference(backward_demodulation, [], [f1101, f1570])).
fof(f1570, plain, ((e22 = op2(e22, e22)) | ~ spl144_87), inference(avatar_component_clause, [], [f1568])).
fof(f1101, plain, (op2(e22, e22) = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, e22) = h3(e12)) & (op2(e22, op2(e22, e22)) = h3(e11)) & (h3(e10) = op2(op2(e22, e22), op2(e22, op2(e22, e22)))) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax16)).
fof(f3009, plain, ~ (op2(e21, e22) = h3(e12)), inference(backward_demodulation, [], [f471, f1101])).
fof(f471, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f6746, plain, (~ spl144_95 | ~ spl144_87), inference(avatar_split_clause, [], [f6745, f1568, f1602])).
fof(f1602, plain, (spl144_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_95])])).
fof(f6745, plain, (~ (e22 = op2(e22, e20)) | ~ spl144_87), inference(forward_demodulation, [], [f3011, f6456])).
fof(f3011, plain, ~ (op2(e22, e20) = h3(e12)), inference(backward_demodulation, [], [f493, f1101])).
fof(f493, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f6742, plain, (~ spl144_64 | ~ spl144_52), inference(avatar_split_clause, [], [f6741, f1387, f1438])).
fof(f1438, plain, (spl144_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl144_64])])).
fof(f1387, plain, (spl144_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_52])])).
fof(f6741, plain, (~ (op1(e10, e10) = e13) | ~ spl144_52), inference(forward_demodulation, [], [f434, f1389])).
fof(f1389, plain, ((e13 = op1(e10, e13)) | ~ spl144_52), inference(avatar_component_clause, [], [f1387])).
fof(f434, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f6739, plain, (~ spl144_63 | ~ spl144_53 | spl144_209), inference(avatar_split_clause, [], [f5399, f2370, f1392, f1434])).
fof(f1392, plain, (spl144_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_53])])).
fof(f2370, plain, (spl144_209 <=> (e12 = op1(e10, op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_209])])).
fof(f5399, plain, (~ (op1(e10, e10) = e12) | (~ spl144_53 | spl144_209)), inference(backward_demodulation, [], [f2371, f1394])).
fof(f1394, plain, ((e10 = op1(e10, e12)) | ~ spl144_53), inference(avatar_component_clause, [], [f1392])).
fof(f2371, plain, (~ (e12 = op1(e10, op1(e10, e12))) | spl144_209), inference(avatar_component_clause, [], [f2370])).
fof(f6725, plain, (~ spl144_48 | ~ spl144_40), inference(avatar_split_clause, [], [f6724, f1336, f1370])).
fof(f1370, plain, (spl144_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_48])])).
fof(f1336, plain, (spl144_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_40])])).
fof(f6724, plain, (~ (e13 = op1(e11, e10)) | ~ spl144_40), inference(forward_demodulation, [], [f439, f1338])).
fof(f1338, plain, ((e13 = op1(e11, e12)) | ~ spl144_40), inference(avatar_component_clause, [], [f1336])).
fof(f439, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f6710, plain, (~ spl144_98 | ~ spl144_107 | ~ spl144_288), inference(avatar_contradiction_clause, [], [f6709])).
fof(f6709, plain, ($false | (~ spl144_98 | ~ spl144_107 | ~ spl144_288)), inference(subsumption_resolution, [], [f6708, f515])).
fof(f515, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f6708, plain, ((e22 = e23) | (~ spl144_98 | ~ spl144_107 | ~ spl144_288)), inference(forward_demodulation, [], [f6707, f1655])).
fof(f6707, plain, ((e23 = op2(e21, e21)) | (~ spl144_98 | ~ spl144_288)), inference(forward_demodulation, [], [f2963, f1617])).
fof(f1617, plain, ((e21 = op2(e21, e23)) | ~ spl144_98), inference(avatar_component_clause, [], [f1615])).
fof(f1615, plain, (spl144_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_98])])).
fof(f6636, plain, (~ spl144_101 | spl144_111 | ~ spl144_287), inference(avatar_contradiction_clause, [], [f6635])).
fof(f6635, plain, ($false | (~ spl144_101 | spl144_111 | ~ spl144_287)), inference(subsumption_resolution, [], [f6634, f1671])).
fof(f1671, plain, (~ (e22 = op2(e21, e20)) | spl144_111), inference(avatar_component_clause, [], [f1670])).
fof(f6634, plain, ((e22 = op2(e21, e20)) | (~ spl144_101 | ~ spl144_287)), inference(forward_demodulation, [], [f2959, f1630])).
fof(f1630, plain, ((e20 = op2(e21, e22)) | ~ spl144_101), inference(avatar_component_clause, [], [f1628])).
fof(f1628, plain, (spl144_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_101])])).
fof(f6562, plain, (spl144_97 | ~ spl144_112 | ~ spl144_285), inference(avatar_contradiction_clause, [], [f6561])).
fof(f6561, plain, ($false | (spl144_97 | ~ spl144_112 | ~ spl144_285)), inference(subsumption_resolution, [], [f6560, f1612])).
fof(f1612, plain, (~ (e20 = op2(e21, e23)) | spl144_97), inference(avatar_component_clause, [], [f1611])).
fof(f6560, plain, ((e20 = op2(e21, e23)) | (~ spl144_112 | ~ spl144_285)), inference(backward_demodulation, [], [f2951, f1676])).
fof(f1676, plain, ((e23 = op2(e21, e20)) | ~ spl144_112), inference(avatar_component_clause, [], [f1674])).
fof(f1674, plain, (spl144_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_112])])).
fof(f2951, plain, ((e20 = op2(e21, op2(e21, e20))) | ~ spl144_285), inference(avatar_component_clause, [], [f2949])).
fof(f2949, plain, (spl144_285 <=> (e20 = op2(e21, op2(e21, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_285])])).
fof(f6537, plain, (~ spl144_23 | ~ spl144_87 | ~ spl144_316 | spl144_385), inference(avatar_contradiction_clause, [], [f6536])).
fof(f6536, plain, ($false | (~ spl144_23 | ~ spl144_87 | ~ spl144_316 | spl144_385)), inference(subsumption_resolution, [], [f6529, f1570])).
fof(f6529, plain, (~ (e22 = op2(e22, e22)) | (~ spl144_23 | ~ spl144_316 | spl144_385)), inference(backward_demodulation, [], [f6511, f3157])).
fof(f6511, plain, (~ (h1(e12) = op2(h1(e12), h1(e12))) | (~ spl144_23 | spl144_385)), inference(forward_demodulation, [], [f3588, f1266])).
fof(f1266, plain, ((e12 = op1(e12, e12)) | ~ spl144_23), inference(avatar_component_clause, [], [f1264])).
fof(f1264, plain, (spl144_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_23])])).
fof(f3588, plain, (~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | spl144_385), inference(avatar_component_clause, [], [f3586])).
fof(f3586, plain, (spl144_385 <=> (h1(op1(e12, e12)) = op2(h1(e12), h1(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_385])])).
fof(f6516, plain, (~ spl144_111 | ~ spl144_47 | ~ spl144_294 | spl144_334), inference(avatar_split_clause, [], [f6515, f3263, f3036, f1366, f1670])).
fof(f3036, plain, (spl144_294 <=> (e22 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_294])])).
fof(f3263, plain, (spl144_334 <=> (op2(e21, e20) = h4(op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_334])])).
fof(f6515, plain, (~ (e22 = op2(e21, e20)) | (~ spl144_47 | ~ spl144_294 | spl144_334)), inference(forward_demodulation, [], [f6514, f3037])).
fof(f3037, plain, ((e22 = h4(e12)) | ~ spl144_294), inference(avatar_component_clause, [], [f3036])).
fof(f6514, plain, (~ (op2(e21, e20) = h4(e12)) | (~ spl144_47 | spl144_334)), inference(forward_demodulation, [], [f3265, f1368])).
fof(f3265, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | spl144_334), inference(avatar_component_clause, [], [f3263])).
fof(f6503, plain, (~ spl144_300 | ~ spl144_70), inference(avatar_split_clause, [], [f4100, f1496, f3075])).
fof(f3075, plain, (spl144_300 <=> (e21 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_300])])).
fof(f4100, plain, (~ (e21 = h3(e12)) | ~ spl144_70), inference(backward_demodulation, [], [f3010, f1498])).
fof(f3010, plain, ~ (op2(e23, e22) = h3(e12)), inference(backward_demodulation, [], [f473, f1101])).
fof(f473, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f6500, plain, (~ spl144_119 | ~ spl144_87), inference(avatar_split_clause, [], [f6499, f1568, f1704])).
fof(f1704, plain, (spl144_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_119])])).
fof(f6499, plain, (~ (e22 = op2(e20, e22)) | ~ spl144_87), inference(forward_demodulation, [], [f3008, f6456])).
fof(f3008, plain, ~ (op2(e20, e22) = h3(e12)), inference(backward_demodulation, [], [f469, f1101])).
fof(f469, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f6465, plain, (~ spl144_70 | ~ spl144_72), inference(avatar_contradiction_clause, [], [f6464])).
fof(f6464, plain, ($false | (~ spl144_70 | ~ spl144_72)), inference(subsumption_resolution, [], [f6463, f514])).
fof(f6463, plain, ((e21 = e23) | (~ spl144_70 | ~ spl144_72)), inference(forward_demodulation, [], [f1506, f1498])).
fof(f1506, plain, ((e23 = op2(e23, e22)) | ~ spl144_72), inference(avatar_component_clause, [], [f1504])).
fof(f1504, plain, (spl144_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_72])])).
fof(f6454, plain, (~ spl144_82 | ~ spl144_94), inference(avatar_split_clause, [], [f6449, f1598, f1547])).
fof(f1547, plain, (spl144_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_82])])).
fof(f6449, plain, (~ (e21 = op2(e22, e23)) | ~ spl144_94), inference(backward_demodulation, [], [f494, f1600])).
fof(f1600, plain, ((e21 = op2(e22, e20)) | ~ spl144_94), inference(avatar_component_clause, [], [f1598])).
fof(f6448, plain, (~ spl144_82 | ~ spl144_98), inference(avatar_split_clause, [], [f6446, f1615, f1547])).
fof(f6446, plain, (~ (e21 = op2(e22, e23)) | ~ spl144_98), inference(backward_demodulation, [], [f477, f1617])).
fof(f477, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f6428, plain, (~ spl144_82 | ~ spl144_18 | ~ spl144_294 | spl144_328), inference(avatar_split_clause, [], [f6427, f3239, f3036, f1243, f1547])).
fof(f1243, plain, (spl144_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_18])])).
fof(f3239, plain, (spl144_328 <=> (h4(op1(e12, e13)) = op2(h4(e12), e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_328])])).
fof(f6427, plain, (~ (e21 = op2(e22, e23)) | (~ spl144_18 | ~ spl144_294 | spl144_328)), inference(forward_demodulation, [], [f6426, f3026])).
fof(f3026, plain, (e21 = h4(e11)), inference(forward_demodulation, [], [f3025, f3023])).
fof(f3023, plain, (e21 = op2(e23, h4(e12))), inference(backward_demodulation, [], [f1088, f1105])).
fof(f1105, plain, (op2(e23, e23) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, e23) = h4(e12)) & (op2(e23, op2(e23, e23)) = h4(e11)) & (op2(op2(e23, e23), op2(e23, op2(e23, e23))) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax17)).
fof(f1088, plain, (e21 = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(e23, e23)) & (e21 = op2(e23, op2(e23, e23))) & (e20 = op2(op2(e23, e23), op2(e23, op2(e23, e23))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax13)).
fof(f3025, plain, (h4(e11) = op2(e23, h4(e12))), inference(forward_demodulation, [], [f1104, f1105])).
fof(f1104, plain, (op2(e23, op2(e23, e23)) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f6426, plain, (~ (op2(e22, e23) = h4(e11)) | (~ spl144_18 | ~ spl144_294 | spl144_328)), inference(forward_demodulation, [], [f6425, f1245])).
fof(f1245, plain, ((e11 = op1(e12, e13)) | ~ spl144_18), inference(avatar_component_clause, [], [f1243])).
fof(f6425, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | (~ spl144_294 | spl144_328)), inference(forward_demodulation, [], [f3241, f3037])).
fof(f3241, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | spl144_328), inference(avatar_component_clause, [], [f3239])).
fof(f6414, plain, (~ spl144_312 | ~ spl144_89), inference(avatar_split_clause, [], [f6413, f1577, f3136])).
fof(f3136, plain, (spl144_312 <=> (e20 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_312])])).
fof(f1577, plain, (spl144_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_89])])).
fof(f6413, plain, (~ (e20 = h2(e12)) | ~ spl144_89), inference(forward_demodulation, [], [f3000, f1579])).
fof(f1579, plain, ((e20 = op2(e22, e21)) | ~ spl144_89), inference(avatar_component_clause, [], [f1577])).
fof(f3000, plain, ~ (op2(e22, e21) = h2(e12)), inference(backward_demodulation, [], [f465, f1097])).
fof(f1097, plain, (op2(e21, e21) = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, e21) = h2(e12)) & (op2(e21, op2(e21, e21)) = h2(e11)) & (h2(e10) = op2(op2(e21, e21), op2(e21, op2(e21, e21)))) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax15)).
fof(f465, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f6394, plain, (spl144_85 | ~ spl144_95 | ~ spl144_281), inference(avatar_contradiction_clause, [], [f6393])).
fof(f6393, plain, ($false | (spl144_85 | ~ spl144_95 | ~ spl144_281)), inference(subsumption_resolution, [], [f6392, f1561])).
fof(f1561, plain, (~ (e20 = op2(e22, e22)) | spl144_85), inference(avatar_component_clause, [], [f1560])).
fof(f1560, plain, (spl144_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_85])])).
fof(f6392, plain, ((e20 = op2(e22, e22)) | (~ spl144_95 | ~ spl144_281)), inference(forward_demodulation, [], [f2934, f1604])).
fof(f1604, plain, ((e22 = op2(e22, e20)) | ~ spl144_95), inference(avatar_component_clause, [], [f1602])).
fof(f2934, plain, ((e20 = op2(e22, op2(e22, e20))) | ~ spl144_281), inference(avatar_component_clause, [], [f2932])).
fof(f2932, plain, (spl144_281 <=> (e20 = op2(e22, op2(e22, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_281])])).
fof(f6251, plain, (spl144_105 | ~ spl144_110 | ~ spl144_285), inference(avatar_split_clause, [], [f6250, f2949, f1666, f1645])).
fof(f1645, plain, (spl144_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_105])])).
fof(f6250, plain, ((e20 = op2(e21, e21)) | (~ spl144_110 | ~ spl144_285)), inference(backward_demodulation, [], [f2951, f1668])).
fof(f6238, plain, (~ spl144_89 | spl144_94 | ~ spl144_282), inference(avatar_contradiction_clause, [], [f6237])).
fof(f6237, plain, ($false | (~ spl144_89 | spl144_94 | ~ spl144_282)), inference(subsumption_resolution, [], [f6236, f1599])).
fof(f1599, plain, (~ (e21 = op2(e22, e20)) | spl144_94), inference(avatar_component_clause, [], [f1598])).
fof(f6236, plain, ((e21 = op2(e22, e20)) | (~ spl144_89 | ~ spl144_282)), inference(forward_demodulation, [], [f2938, f1579])).
fof(f2938, plain, ((e21 = op2(e22, op2(e22, e21))) | ~ spl144_282), inference(avatar_component_clause, [], [f2936])).
fof(f2936, plain, (spl144_282 <=> (e21 = op2(e22, op2(e22, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl144_282])])).
fof(f6231, plain, (~ spl144_87 | ~ spl144_23 | ~ spl144_294 | spl144_323), inference(avatar_split_clause, [], [f6230, f3219, f3036, f1264, f1568])).
fof(f3219, plain, (spl144_323 <=> (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_323])])).
fof(f6230, plain, (~ (e22 = op2(e22, e22)) | (~ spl144_23 | ~ spl144_294 | spl144_323)), inference(forward_demodulation, [], [f6229, f3037])).
fof(f6229, plain, (~ (op2(e22, e22) = h4(e12)) | (~ spl144_23 | ~ spl144_294 | spl144_323)), inference(forward_demodulation, [], [f6228, f1266])).
fof(f6228, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | (~ spl144_294 | spl144_323)), inference(forward_demodulation, [], [f3221, f3037])).
fof(f3221, plain, (~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | spl144_323), inference(avatar_component_clause, [], [f3219])).
fof(f6227, plain, (~ spl144_96 | ~ spl144_32 | ~ spl144_294 | spl144_330), inference(avatar_split_clause, [], [f6226, f3247, f3036, f1302, f1606])).
fof(f1302, plain, (spl144_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_32])])).
fof(f3247, plain, (spl144_330 <=> (h4(op1(e12, e10)) = op2(h4(e12), e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_330])])).
fof(f6226, plain, (~ (e23 = op2(e22, e20)) | (~ spl144_32 | ~ spl144_294 | spl144_330)), inference(forward_demodulation, [], [f6225, f1102])).
fof(f1102, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f6225, plain, (~ (op2(e22, e20) = h4(e13)) | (~ spl144_32 | ~ spl144_294 | spl144_330)), inference(forward_demodulation, [], [f6224, f1304])).
fof(f1304, plain, ((e13 = op1(e12, e10)) | ~ spl144_32), inference(avatar_component_clause, [], [f1302])).
fof(f6224, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl144_294 | spl144_330)), inference(forward_demodulation, [], [f3249, f3037])).
fof(f3249, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | spl144_330), inference(avatar_component_clause, [], [f3247])).
fof(f6220, plain, (~ spl144_320 | ~ spl144_62 | spl144_338), inference(avatar_split_clause, [], [f6219, f3279, f1430, f3176])).
fof(f3279, plain, (spl144_338 <=> (h1(e12) = h4(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_338])])).
fof(f6219, plain, (~ (e21 = h1(e12)) | (~ spl144_62 | spl144_338)), inference(forward_demodulation, [], [f6218, f3026])).
fof(f6218, plain, (~ (h1(e12) = h4(e11)) | (~ spl144_62 | spl144_338)), inference(forward_demodulation, [], [f3281, f1432])).
fof(f3281, plain, (~ (h1(e12) = h4(op1(e10, e10))) | spl144_338), inference(avatar_component_clause, [], [f3279])).
fof(f6159, plain, (~ spl144_120 | ~ spl144_386), inference(avatar_split_clause, [], [f6147, f3590, f1708])).
fof(f1708, plain, (spl144_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_120])])).
fof(f3590, plain, (spl144_386 <=> (e23 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_386])])).
fof(f6147, plain, (~ (e23 = op2(e20, e22)) | ~ spl144_386), inference(backward_demodulation, [], [f2994, f3591])).
fof(f3591, plain, ((e23 = h1(e12)) | ~ spl144_386), inference(avatar_component_clause, [], [f3590])).
fof(f2994, plain, ~ (op2(e20, e22) = h1(e12)), inference(backward_demodulation, [], [f481, f1093])).
fof(f481, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f6158, plain, (~ spl144_96 | ~ spl144_386), inference(avatar_split_clause, [], [f6145, f3590, f1606])).
fof(f6145, plain, (~ (e23 = op2(e22, e20)) | ~ spl144_386), inference(backward_demodulation, [], [f2991, f3591])).
fof(f6052, plain, (~ spl144_113 | ~ spl144_120 | ~ spl144_291), inference(avatar_contradiction_clause, [], [f6051])).
fof(f6051, plain, ($false | (~ spl144_113 | ~ spl144_120 | ~ spl144_291)), inference(subsumption_resolution, [], [f6050, f511])).
fof(f6050, plain, ((e20 = e22) | (~ spl144_113 | ~ spl144_120 | ~ spl144_291)), inference(forward_demodulation, [], [f6049, f1681])).
fof(f1681, plain, ((e20 = op2(e20, e23)) | ~ spl144_113), inference(avatar_component_clause, [], [f1679])).
fof(f6049, plain, ((e22 = op2(e20, e23)) | (~ spl144_120 | ~ spl144_291)), inference(forward_demodulation, [], [f2976, f1710])).
fof(f1710, plain, ((e23 = op2(e20, e22)) | ~ spl144_120), inference(avatar_component_clause, [], [f1708])).
fof(f2976, plain, ((e22 = op2(e20, op2(e20, e22))) | ~ spl144_291), inference(avatar_component_clause, [], [f2974])).
fof(f2974, plain, (spl144_291 <=> (e22 = op2(e20, op2(e20, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_291])])).
fof(f6048, plain, (spl144_118 | ~ spl144_123 | ~ spl144_290), inference(avatar_contradiction_clause, [], [f6047])).
fof(f6047, plain, ($false | (spl144_118 | ~ spl144_123 | ~ spl144_290)), inference(subsumption_resolution, [], [f6046, f1701])).
fof(f1701, plain, (~ (e21 = op2(e20, e22)) | spl144_118), inference(avatar_component_clause, [], [f1700])).
fof(f1700, plain, (spl144_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_118])])).
fof(f6046, plain, ((e21 = op2(e20, e22)) | (~ spl144_123 | ~ spl144_290)), inference(forward_demodulation, [], [f2972, f1723])).
fof(f1723, plain, ((e22 = op2(e20, e21)) | ~ spl144_123), inference(avatar_component_clause, [], [f1721])).
fof(f2972, plain, ((e21 = op2(e20, op2(e20, e21))) | ~ spl144_290), inference(avatar_component_clause, [], [f2970])).
fof(f2970, plain, (spl144_290 <=> (e21 = op2(e20, op2(e20, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl144_290])])).
fof(f5954, plain, (~ spl144_113 | ~ spl144_126 | ~ spl144_292), inference(avatar_contradiction_clause, [], [f5953])).
fof(f5953, plain, ($false | (~ spl144_113 | ~ spl144_126 | ~ spl144_292)), inference(subsumption_resolution, [], [f5952, f514])).
fof(f5952, plain, ((e21 = e23) | (~ spl144_113 | ~ spl144_126 | ~ spl144_292)), inference(forward_demodulation, [], [f5948, f1736])).
fof(f1736, plain, ((op2(e20, e20) = e21) | ~ spl144_126), inference(avatar_component_clause, [], [f1734])).
fof(f1734, plain, (spl144_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl144_126])])).
fof(f5948, plain, ((op2(e20, e20) = e23) | (~ spl144_113 | ~ spl144_292)), inference(backward_demodulation, [], [f2980, f1681])).
fof(f2980, plain, ((e23 = op2(e20, op2(e20, e23))) | ~ spl144_292), inference(avatar_component_clause, [], [f2978])).
fof(f2978, plain, (spl144_292 <=> (e23 = op2(e20, op2(e20, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl144_292])])).
fof(f5936, plain, (~ spl144_116 | ~ spl144_52 | spl144_335), inference(avatar_split_clause, [], [f5935, f3267, f1387, f1691])).
fof(f3267, plain, (spl144_335 <=> (op2(e20, e23) = h4(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_335])])).
fof(f5935, plain, (~ (e23 = op2(e20, e23)) | (~ spl144_52 | spl144_335)), inference(forward_demodulation, [], [f5934, f1102])).
fof(f5934, plain, (~ (op2(e20, e23) = h4(e13)) | (~ spl144_52 | spl144_335)), inference(forward_demodulation, [], [f3269, f1389])).
fof(f3269, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | spl144_335), inference(avatar_component_clause, [], [f3267])).
fof(f5912, plain, (~ spl144_116 | ~ spl144_120), inference(avatar_split_clause, [], [f5911, f1708, f1691])).
fof(f5911, plain, (~ (e23 = op2(e20, e23)) | ~ spl144_120), inference(forward_demodulation, [], [f485, f1710])).
fof(f5906, plain, (~ spl144_104 | ~ spl144_40 | ~ spl144_294 | spl144_332), inference(avatar_split_clause, [], [f5864, f3255, f3036, f1336, f1640])).
fof(f3255, plain, (spl144_332 <=> (h4(op1(e11, e12)) = op2(e21, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_332])])).
fof(f5864, plain, (~ (e23 = op2(e21, e22)) | (~ spl144_40 | ~ spl144_294 | spl144_332)), inference(forward_demodulation, [], [f5863, f1102])).
fof(f5863, plain, (~ (op2(e21, e22) = h4(e13)) | (~ spl144_40 | ~ spl144_294 | spl144_332)), inference(forward_demodulation, [], [f5862, f1338])).
fof(f5862, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | (~ spl144_294 | spl144_332)), inference(forward_demodulation, [], [f3257, f3037])).
fof(f3257, plain, (~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | spl144_332), inference(avatar_component_clause, [], [f3255])).
fof(f5899, plain, (~ spl144_69 | ~ spl144_70), inference(avatar_contradiction_clause, [], [f5898])).
fof(f5898, plain, ($false | (~ spl144_69 | ~ spl144_70)), inference(subsumption_resolution, [], [f5897, f510])).
fof(f510, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f5897, plain, ((e20 = e21) | (~ spl144_69 | ~ spl144_70)), inference(backward_demodulation, [], [f1498, f1494])).
fof(f1494, plain, ((e20 = op2(e23, e22)) | ~ spl144_69), inference(avatar_component_clause, [], [f1492])).
fof(f1492, plain, (spl144_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_69])])).
fof(f5858, plain, (~ spl144_82 | ~ spl144_283 | ~ spl144_348), inference(avatar_contradiction_clause, [], [f5857])).
fof(f5857, plain, ($false | (~ spl144_82 | ~ spl144_283 | ~ spl144_348)), inference(subsumption_resolution, [], [f5856, f513])).
fof(f513, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f5856, plain, ((e21 = e22) | (~ spl144_82 | ~ spl144_283 | ~ spl144_348)), inference(forward_demodulation, [], [f5849, f1549])).
fof(f1549, plain, ((e21 = op2(e22, e23)) | ~ spl144_82), inference(avatar_component_clause, [], [f1547])).
fof(f5849, plain, ((e22 = op2(e22, e23)) | (~ spl144_283 | ~ spl144_348)), inference(backward_demodulation, [], [f5797, f3363])).
fof(f3363, plain, ((e23 = h3(e12)) | ~ spl144_348), inference(avatar_component_clause, [], [f3362])).
fof(f3362, plain, (spl144_348 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_348])])).
fof(f5797, plain, ((e22 = op2(e22, h3(e12))) | ~ spl144_283), inference(backward_demodulation, [], [f2942, f1101])).
fof(f2942, plain, ((e22 = op2(e22, op2(e22, e22))) | ~ spl144_283), inference(avatar_component_clause, [], [f2940])).
fof(f2940, plain, (spl144_283 <=> (e22 = op2(e22, op2(e22, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_283])])).
fof(f5743, plain, (~ spl144_117 | ~ spl144_120), inference(avatar_contradiction_clause, [], [f5742])).
fof(f5742, plain, ($false | (~ spl144_117 | ~ spl144_120)), inference(subsumption_resolution, [], [f5741, f512])).
fof(f512, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f5741, plain, ((e20 = e23) | (~ spl144_117 | ~ spl144_120)), inference(forward_demodulation, [], [f1710, f1698])).
fof(f5696, plain, (~ spl144_89 | ~ spl144_94 | spl144_281), inference(avatar_contradiction_clause, [], [f5695])).
fof(f5695, plain, ($false | (~ spl144_89 | ~ spl144_94 | spl144_281)), inference(subsumption_resolution, [], [f5693, f1579])).
fof(f5693, plain, (~ (e20 = op2(e22, e21)) | (~ spl144_94 | spl144_281)), inference(backward_demodulation, [], [f2933, f1600])).
fof(f2933, plain, (~ (e20 = op2(e22, op2(e22, e20))) | spl144_281), inference(avatar_component_clause, [], [f2932])).
fof(f5691, plain, (~ spl144_114 | ~ spl144_123 | ~ spl144_292), inference(avatar_contradiction_clause, [], [f5690])).
fof(f5690, plain, ($false | (~ spl144_114 | ~ spl144_123 | ~ spl144_292)), inference(subsumption_resolution, [], [f5689, f515])).
fof(f5689, plain, ((e22 = e23) | (~ spl144_114 | ~ spl144_123 | ~ spl144_292)), inference(forward_demodulation, [], [f5687, f1723])).
fof(f5687, plain, ((e23 = op2(e20, e21)) | (~ spl144_114 | ~ spl144_292)), inference(backward_demodulation, [], [f2980, f1685])).
fof(f1685, plain, ((e21 = op2(e20, e23)) | ~ spl144_114), inference(avatar_component_clause, [], [f1683])).
fof(f1683, plain, (spl144_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_114])])).
fof(f5662, plain, (~ spl144_23 | ~ spl144_67 | ~ spl144_385 | ~ spl144_386), inference(avatar_contradiction_clause, [], [f5661])).
fof(f5661, plain, ($false | (~ spl144_23 | ~ spl144_67 | ~ spl144_385 | ~ spl144_386)), inference(subsumption_resolution, [], [f5660, f515])).
fof(f5660, plain, ((e22 = e23) | (~ spl144_23 | ~ spl144_67 | ~ spl144_385 | ~ spl144_386)), inference(forward_demodulation, [], [f5648, f1485])).
fof(f1485, plain, ((e22 = op2(e23, e23)) | ~ spl144_67), inference(avatar_component_clause, [], [f1483])).
fof(f1483, plain, (spl144_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_67])])).
fof(f5648, plain, ((e23 = op2(e23, e23)) | (~ spl144_23 | ~ spl144_385 | ~ spl144_386)), inference(backward_demodulation, [], [f5619, f3591])).
fof(f5619, plain, ((h1(e12) = op2(h1(e12), h1(e12))) | (~ spl144_23 | ~ spl144_385)), inference(forward_demodulation, [], [f3587, f1266])).
fof(f3587, plain, ((h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ spl144_385), inference(avatar_component_clause, [], [f3586])).
fof(f5610, plain, (~ spl144_59 | ~ spl144_123 | ~ spl144_294 | spl144_337), inference(avatar_contradiction_clause, [], [f5609])).
fof(f5609, plain, ($false | (~ spl144_59 | ~ spl144_123 | ~ spl144_294 | spl144_337)), inference(subsumption_resolution, [], [f5608, f1723])).
fof(f5608, plain, (~ (e22 = op2(e20, e21)) | (~ spl144_59 | ~ spl144_294 | spl144_337)), inference(forward_demodulation, [], [f5607, f3037])).
fof(f5607, plain, (~ (op2(e20, e21) = h4(e12)) | (~ spl144_59 | spl144_337)), inference(forward_demodulation, [], [f3277, f1419])).
fof(f1419, plain, ((e12 = op1(e10, e11)) | ~ spl144_59), inference(avatar_component_clause, [], [f1417])).
fof(f3277, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | spl144_337), inference(avatar_component_clause, [], [f3275])).
fof(f3275, plain, (spl144_337 <=> (op2(e20, e21) = h4(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_337])])).
fof(f5568, plain, (~ spl144_25 | spl144_329), inference(avatar_contradiction_clause, [], [f5567])).
fof(f5567, plain, ($false | (~ spl144_25 | spl144_329)), inference(subsumption_resolution, [], [f5566, f3029])).
fof(f3029, plain, (e20 = h4(e10)), inference(forward_demodulation, [], [f3028, f3024])).
fof(f3024, plain, (e20 = op2(h4(e12), e21)), inference(backward_demodulation, [], [f2989, f1105])).
fof(f2989, plain, (e20 = op2(op2(e23, e23), e21)), inference(forward_demodulation, [], [f1087, f1088])).
fof(f1087, plain, (e20 = op2(op2(e23, e23), op2(e23, op2(e23, e23)))), inference(cnf_transformation, [], [f13])).
fof(f3028, plain, (h4(e10) = op2(h4(e12), e21)), inference(forward_demodulation, [], [f3027, f3023])).
fof(f3027, plain, (h4(e10) = op2(h4(e12), op2(e23, h4(e12)))), inference(forward_demodulation, [], [f1103, f1105])).
fof(f1103, plain, (op2(op2(e23, e23), op2(e23, op2(e23, e23))) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f5566, plain, (~ (e20 = h4(e10)) | (~ spl144_25 | spl144_329)), inference(forward_demodulation, [], [f3245, f1275])).
fof(f1275, plain, ((e10 = op1(e12, e11)) | ~ spl144_25), inference(avatar_component_clause, [], [f1273])).
fof(f1273, plain, (spl144_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_25])])).
fof(f3245, plain, (~ (e20 = h4(op1(e12, e11))) | spl144_329), inference(avatar_component_clause, [], [f3243])).
fof(f3243, plain, (spl144_329 <=> (e20 = h4(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_329])])).
fof(f5552, plain, (~ spl144_42 | ~ spl144_106 | spl144_333), inference(avatar_contradiction_clause, [], [f5551])).
fof(f5551, plain, ($false | (~ spl144_42 | ~ spl144_106 | spl144_333)), inference(subsumption_resolution, [], [f5550, f4076])).
fof(f4076, plain, ((e21 = h2(e12)) | ~ spl144_106), inference(backward_demodulation, [], [f1097, f1651])).
fof(f1651, plain, ((e21 = op2(e21, e21)) | ~ spl144_106), inference(avatar_component_clause, [], [f1649])).
fof(f1649, plain, (spl144_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_106])])).
fof(f5550, plain, (~ (e21 = h2(e12)) | (~ spl144_42 | spl144_333)), inference(forward_demodulation, [], [f5549, f3026])).
fof(f5549, plain, (~ (h2(e12) = h4(e11)) | (~ spl144_42 | spl144_333)), inference(forward_demodulation, [], [f3261, f1347])).
fof(f1347, plain, ((e11 = op1(e11, e11)) | ~ spl144_42), inference(avatar_component_clause, [], [f1345])).
fof(f1345, plain, (spl144_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_42])])).
fof(f3261, plain, (~ (h2(e12) = h4(op1(e11, e11))) | spl144_333), inference(avatar_component_clause, [], [f3259])).
fof(f3259, plain, (spl144_333 <=> (h2(e12) = h4(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_333])])).
fof(f5495, plain, (~ spl144_53 | ~ spl144_117 | ~ spl144_294 | spl144_336), inference(avatar_contradiction_clause, [], [f5494])).
fof(f5494, plain, ($false | (~ spl144_53 | ~ spl144_117 | ~ spl144_294 | spl144_336)), inference(subsumption_resolution, [], [f5493, f1698])).
fof(f5493, plain, (~ (e20 = op2(e20, e22)) | (~ spl144_53 | ~ spl144_294 | spl144_336)), inference(forward_demodulation, [], [f5492, f3029])).
fof(f5492, plain, (~ (op2(e20, e22) = h4(e10)) | (~ spl144_53 | ~ spl144_294 | spl144_336)), inference(forward_demodulation, [], [f5491, f1394])).
fof(f5491, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl144_294 | spl144_336)), inference(forward_demodulation, [], [f3273, f3037])).
fof(f3273, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | spl144_336), inference(avatar_component_clause, [], [f3271])).
fof(f3271, plain, (spl144_336 <=> (h4(op1(e10, e12)) = op2(e20, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_336])])).
fof(f5473, plain, (~ spl144_13 | ~ spl144_77 | spl144_327), inference(avatar_contradiction_clause, [], [f5472])).
fof(f5472, plain, ($false | (~ spl144_13 | ~ spl144_77 | spl144_327)), inference(subsumption_resolution, [], [f5471, f1528])).
fof(f1528, plain, ((e20 = op2(e23, e20)) | ~ spl144_77), inference(avatar_component_clause, [], [f1526])).
fof(f1526, plain, (spl144_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_77])])).
fof(f5471, plain, (~ (e20 = op2(e23, e20)) | (~ spl144_13 | spl144_327)), inference(forward_demodulation, [], [f5470, f3029])).
fof(f5470, plain, (~ (op2(e23, e20) = h4(e10)) | (~ spl144_13 | spl144_327)), inference(forward_demodulation, [], [f3237, f1224])).
fof(f1224, plain, ((e10 = op1(e13, e10)) | ~ spl144_13), inference(avatar_component_clause, [], [f1222])).
fof(f1222, plain, (spl144_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_13])])).
fof(f3237, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | spl144_327), inference(avatar_component_clause, [], [f3235])).
fof(f3235, plain, (spl144_327 <=> (op2(e23, e20) = h4(op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_327])])).
fof(f5451, plain, (~ spl144_3 | spl144_324), inference(avatar_contradiction_clause, [], [f5450])).
fof(f5450, plain, ($false | (~ spl144_3 | spl144_324)), inference(trivial_inequality_removal, [], [f5449])).
fof(f5449, plain, (~ (h4(e12) = h4(e12)) | (~ spl144_3 | spl144_324)), inference(forward_demodulation, [], [f3225, f1181])).
fof(f1181, plain, ((e12 = op1(e13, e13)) | ~ spl144_3), inference(avatar_component_clause, [], [f1179])).
fof(f1179, plain, (spl144_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_3])])).
fof(f3225, plain, (~ (h4(e12) = h4(op1(e13, e13))) | spl144_324), inference(avatar_component_clause, [], [f3223])).
fof(f3223, plain, (spl144_324 <=> (h4(e12) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_324])])).
fof(f5426, plain, (~ spl144_33 | ~ spl144_97 | spl144_331), inference(avatar_contradiction_clause, [], [f5425])).
fof(f5425, plain, ($false | (~ spl144_33 | ~ spl144_97 | spl144_331)), inference(subsumption_resolution, [], [f5423, f3029])).
fof(f5423, plain, (~ (e20 = h4(e10)) | (~ spl144_33 | ~ spl144_97 | spl144_331)), inference(backward_demodulation, [], [f5390, f1309])).
fof(f1309, plain, ((e10 = op1(e11, e13)) | ~ spl144_33), inference(avatar_component_clause, [], [f1307])).
fof(f1307, plain, (spl144_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_33])])).
fof(f5390, plain, (~ (e20 = h4(op1(e11, e13))) | (~ spl144_97 | spl144_331)), inference(forward_demodulation, [], [f3253, f1613])).
fof(f3253, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | spl144_331), inference(avatar_component_clause, [], [f3251])).
fof(f3251, plain, (spl144_331 <=> (op2(e21, e23) = h4(op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_331])])).
fof(f5404, plain, (~ spl144_49 | ~ spl144_53), inference(avatar_split_clause, [], [f5395, f1392, f1375])).
fof(f1375, plain, (spl144_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_49])])).
fof(f5395, plain, (~ (e10 = op1(e10, e13)) | ~ spl144_53), inference(backward_demodulation, [], [f437, f1394])).
fof(f437, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f5403, plain, (~ spl144_37 | ~ spl144_53), inference(avatar_split_clause, [], [f5394, f1392, f1324])).
fof(f1324, plain, (spl144_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_37])])).
fof(f5394, plain, (~ (e10 = op1(e11, e12)) | ~ spl144_53), inference(backward_demodulation, [], [f420, f1394])).
fof(f420, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f5365, plain, (~ spl144_51 | ~ spl144_59), inference(avatar_split_clause, [], [f5364, f1417, f1383])).
fof(f1383, plain, (spl144_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_51])])).
fof(f5364, plain, (~ (e12 = op1(e10, e13)) | ~ spl144_59), inference(forward_demodulation, [], [f436, f1419])).
fof(f436, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f5351, plain, (~ spl144_49 | ~ spl144_62 | ~ spl144_210), inference(avatar_contradiction_clause, [], [f5350])).
fof(f5350, plain, ($false | (~ spl144_49 | ~ spl144_62 | ~ spl144_210)), inference(subsumption_resolution, [], [f5349, f508])).
fof(f508, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax7)).
fof(f5349, plain, ((e11 = e13) | (~ spl144_49 | ~ spl144_62 | ~ spl144_210)), inference(forward_demodulation, [], [f5348, f1432])).
fof(f5348, plain, ((op1(e10, e10) = e13) | (~ spl144_49 | ~ spl144_210)), inference(forward_demodulation, [], [f2376, f1377])).
fof(f1377, plain, ((e10 = op1(e10, e13)) | ~ spl144_49), inference(avatar_component_clause, [], [f1375])).
fof(f2376, plain, ((e13 = op1(e10, op1(e10, e13))) | ~ spl144_210), inference(avatar_component_clause, [], [f2374])).
fof(f2374, plain, (spl144_210 <=> (e13 = op1(e10, op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_210])])).
fof(f5347, plain, (~ spl144_6 | spl144_325), inference(avatar_contradiction_clause, [], [f5346])).
fof(f5346, plain, ($false | (~ spl144_6 | spl144_325)), inference(subsumption_resolution, [], [f5345, f3026])).
fof(f5345, plain, (~ (e21 = h4(e11)) | (~ spl144_6 | spl144_325)), inference(forward_demodulation, [], [f3229, f1194])).
fof(f1194, plain, ((e11 = op1(e13, e12)) | ~ spl144_6), inference(avatar_component_clause, [], [f1192])).
fof(f1192, plain, (spl144_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_6])])).
fof(f3229, plain, (~ (e21 = h4(op1(e13, e12))) | spl144_325), inference(avatar_component_clause, [], [f3227])).
fof(f3227, plain, (spl144_325 <=> (e21 = h4(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_325])])).
fof(f5304, plain, (~ spl144_36 | spl144_206), inference(avatar_contradiction_clause, [], [f5303])).
fof(f5303, plain, ($false | (~ spl144_36 | spl144_206)), inference(subsumption_resolution, [], [f5299, f1321])).
fof(f1321, plain, ((e13 = op1(e11, e13)) | ~ spl144_36), inference(avatar_component_clause, [], [f1319])).
fof(f1319, plain, (spl144_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_36])])).
fof(f5299, plain, (~ (e13 = op1(e11, e13)) | (~ spl144_36 | spl144_206)), inference(backward_demodulation, [], [f2358, f1321])).
fof(f2358, plain, (~ (e13 = op1(e11, op1(e11, e13))) | spl144_206), inference(avatar_component_clause, [], [f2357])).
fof(f2357, plain, (spl144_206 <=> (e13 = op1(e11, op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_206])])).
fof(f5302, plain, (~ spl144_20 | ~ spl144_36), inference(avatar_split_clause, [], [f5298, f1319, f1251])).
fof(f1251, plain, (spl144_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_20])])).
fof(f5298, plain, (~ (e13 = op1(e12, e13)) | ~ spl144_36), inference(backward_demodulation, [], [f429, f1321])).
fof(f429, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5295, plain, (~ spl144_34 | ~ spl144_42), inference(avatar_split_clause, [], [f5290, f1345, f1311])).
fof(f1311, plain, (spl144_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_34])])).
fof(f5290, plain, (~ (e11 = op1(e11, e13)) | ~ spl144_42), inference(backward_demodulation, [], [f442, f1347])).
fof(f442, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5269, plain, (~ spl144_30 | ~ spl144_62), inference(avatar_split_clause, [], [f5261, f1430, f1294])).
fof(f1294, plain, (spl144_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_30])])).
fof(f5261, plain, (~ (e11 = op1(e12, e10)) | ~ spl144_62), inference(backward_demodulation, [], [f409, f1432])).
fof(f409, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f5257, plain, (~ spl144_49 | ~ spl144_56 | ~ spl144_209), inference(avatar_contradiction_clause, [], [f5256])).
fof(f5256, plain, ($false | (~ spl144_49 | ~ spl144_56 | ~ spl144_209)), inference(subsumption_resolution, [], [f5255, f505])).
fof(f505, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f5255, plain, ((e10 = e12) | (~ spl144_49 | ~ spl144_56 | ~ spl144_209)), inference(forward_demodulation, [], [f5254, f1377])).
fof(f5254, plain, ((e12 = op1(e10, e13)) | (~ spl144_56 | ~ spl144_209)), inference(forward_demodulation, [], [f2372, f1406])).
fof(f1406, plain, ((e13 = op1(e10, e12)) | ~ spl144_56), inference(avatar_component_clause, [], [f1404])).
fof(f1404, plain, (spl144_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_56])])).
fof(f2372, plain, ((e12 = op1(e10, op1(e10, e12))) | ~ spl144_209), inference(avatar_component_clause, [], [f2370])).
fof(f5241, plain, (~ spl144_64 | ~ spl144_49 | spl144_210), inference(avatar_split_clause, [], [f5149, f2374, f1375, f1438])).
fof(f5149, plain, (~ (op1(e10, e10) = e13) | (~ spl144_49 | spl144_210)), inference(backward_demodulation, [], [f2375, f1377])).
fof(f2375, plain, (~ (e13 = op1(e10, op1(e10, e13))) | spl144_210), inference(avatar_component_clause, [], [f2374])).
fof(f5229, plain, (~ spl144_37 | spl144_47 | ~ spl144_205), inference(avatar_contradiction_clause, [], [f5228])).
fof(f5228, plain, ($false | (~ spl144_37 | spl144_47 | ~ spl144_205)), inference(subsumption_resolution, [], [f5227, f1367])).
fof(f1367, plain, (~ (e12 = op1(e11, e10)) | spl144_47), inference(avatar_component_clause, [], [f1366])).
fof(f5227, plain, ((e12 = op1(e11, e10)) | (~ spl144_37 | ~ spl144_205)), inference(forward_demodulation, [], [f2355, f1326])).
fof(f1326, plain, ((e10 = op1(e11, e12)) | ~ spl144_37), inference(avatar_component_clause, [], [f1324])).
fof(f2355, plain, ((e12 = op1(e11, op1(e11, e12))) | ~ spl144_205), inference(avatar_component_clause, [], [f2353])).
fof(f2353, plain, (spl144_205 <=> (e12 = op1(e11, op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_205])])).
fof(f5204, plain, (~ spl144_34 | ~ spl144_43 | ~ spl144_206), inference(avatar_contradiction_clause, [], [f5203])).
fof(f5203, plain, ($false | (~ spl144_34 | ~ spl144_43 | ~ spl144_206)), inference(subsumption_resolution, [], [f5202, f509])).
fof(f509, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f5202, plain, ((e12 = e13) | (~ spl144_34 | ~ spl144_43 | ~ spl144_206)), inference(forward_demodulation, [], [f5201, f1351])).
fof(f1351, plain, ((e12 = op1(e11, e11)) | ~ spl144_43), inference(avatar_component_clause, [], [f1349])).
fof(f5201, plain, ((e13 = op1(e11, e11)) | (~ spl144_34 | ~ spl144_206)), inference(forward_demodulation, [], [f2359, f1313])).
fof(f1313, plain, ((e11 = op1(e11, e13)) | ~ spl144_34), inference(avatar_component_clause, [], [f1311])).
fof(f2359, plain, ((e13 = op1(e11, op1(e11, e13))) | ~ spl144_206), inference(avatar_component_clause, [], [f2357])).
fof(f5168, plain, (spl144_38 | ~ spl144_43 | ~ spl144_204), inference(avatar_contradiction_clause, [], [f5167])).
fof(f5167, plain, ($false | (spl144_38 | ~ spl144_43 | ~ spl144_204)), inference(subsumption_resolution, [], [f5165, f1329])).
fof(f1329, plain, (~ (e11 = op1(e11, e12)) | spl144_38), inference(avatar_component_clause, [], [f1328])).
fof(f1328, plain, (spl144_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_38])])).
fof(f5165, plain, ((e11 = op1(e11, e12)) | (~ spl144_43 | ~ spl144_204)), inference(backward_demodulation, [], [f2351, f1351])).
fof(f2351, plain, ((e11 = op1(e11, op1(e11, e11))) | ~ spl144_204), inference(avatar_component_clause, [], [f2349])).
fof(f2349, plain, (spl144_204 <=> (e11 = op1(e11, op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_204])])).
fof(f5150, plain, (~ spl144_33 | ~ spl144_49), inference(avatar_split_clause, [], [f5145, f1375, f1307])).
fof(f5145, plain, (~ (e10 = op1(e11, e13)) | ~ spl144_49), inference(backward_demodulation, [], [f426, f1377])).
fof(f426, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5079, plain, (~ spl144_53 | spl144_63 | ~ spl144_209), inference(avatar_contradiction_clause, [], [f5078])).
fof(f5078, plain, ($false | (~ spl144_53 | spl144_63 | ~ spl144_209)), inference(subsumption_resolution, [], [f5073, f1435])).
fof(f1435, plain, (~ (op1(e10, e10) = e12) | spl144_63), inference(avatar_component_clause, [], [f1434])).
fof(f5073, plain, ((op1(e10, e10) = e12) | (~ spl144_53 | ~ spl144_209)), inference(backward_demodulation, [], [f2372, f1394])).
fof(f5042, plain, (~ spl144_37 | ~ spl144_40), inference(avatar_contradiction_clause, [], [f5041])).
fof(f5041, plain, ($false | (~ spl144_37 | ~ spl144_40)), inference(subsumption_resolution, [], [f5040, f506])).
fof(f506, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f5040, plain, ((e10 = e13) | (~ spl144_37 | ~ spl144_40)), inference(backward_demodulation, [], [f1338, f1326])).
fof(f5015, plain, (~ spl144_58 | ~ spl144_59), inference(avatar_contradiction_clause, [], [f5014])).
fof(f5014, plain, ($false | (~ spl144_58 | ~ spl144_59)), inference(subsumption_resolution, [], [f5013, f507])).
fof(f507, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f5013, plain, ((e11 = e12) | (~ spl144_58 | ~ spl144_59)), inference(backward_demodulation, [], [f1419, f1415])).
fof(f5003, plain, (spl144_54 | ~ spl144_59 | ~ spl144_208), inference(avatar_split_clause, [], [f4978, f2366, f1417, f1396])).
fof(f1396, plain, (spl144_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_54])])).
fof(f4978, plain, ((e11 = op1(e10, e12)) | (~ spl144_59 | ~ spl144_208)), inference(forward_demodulation, [], [f2368, f1419])).
fof(f2368, plain, ((e11 = op1(e10, op1(e10, e11))) | ~ spl144_208), inference(avatar_component_clause, [], [f2366])).
fof(f5002, plain, (~ spl144_52 | ~ spl144_20), inference(avatar_split_clause, [], [f5001, f1251, f1387])).
fof(f5001, plain, (~ (e13 = op1(e10, e13)) | ~ spl144_20), inference(forward_demodulation, [], [f427, f1253])).
fof(f1253, plain, ((e13 = op1(e12, e13)) | ~ spl144_20), inference(avatar_component_clause, [], [f1251])).
fof(f427, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4995, plain, (~ spl144_44 | ~ spl144_12), inference(avatar_split_clause, [], [f4768, f1217, f1353])).
fof(f1353, plain, (spl144_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_44])])).
fof(f1217, plain, (spl144_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_12])])).
fof(f4768, plain, (~ (e13 = op1(e11, e11)) | ~ spl144_12), inference(forward_demodulation, [], [f418, f1219])).
fof(f1219, plain, ((e13 = op1(e13, e11)) | ~ spl144_12), inference(avatar_component_clause, [], [f1217])).
fof(f418, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4950, plain, (spl144_49 | ~ spl144_64 | ~ spl144_207), inference(avatar_contradiction_clause, [], [f4949])).
fof(f4949, plain, ($false | (spl144_49 | ~ spl144_64 | ~ spl144_207)), inference(subsumption_resolution, [], [f4948, f1376])).
fof(f1376, plain, (~ (e10 = op1(e10, e13)) | spl144_49), inference(avatar_component_clause, [], [f1375])).
fof(f4948, plain, ((e10 = op1(e10, e13)) | (~ spl144_64 | ~ spl144_207)), inference(forward_demodulation, [], [f2364, f1440])).
fof(f1440, plain, ((op1(e10, e10) = e13) | ~ spl144_64), inference(avatar_component_clause, [], [f1438])).
fof(f2364, plain, ((e10 = op1(e10, op1(e10, e10))) | ~ spl144_207), inference(avatar_component_clause, [], [f2362])).
fof(f2362, plain, (spl144_207 <=> (e10 = op1(e10, op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_207])])).
fof(f4910, plain, (~ spl144_33 | ~ spl144_40 | ~ spl144_205), inference(avatar_contradiction_clause, [], [f4909])).
fof(f4909, plain, ($false | (~ spl144_33 | ~ spl144_40 | ~ spl144_205)), inference(subsumption_resolution, [], [f4908, f505])).
fof(f4908, plain, ((e10 = e12) | (~ spl144_33 | ~ spl144_40 | ~ spl144_205)), inference(forward_demodulation, [], [f4906, f1309])).
fof(f4906, plain, ((e12 = op1(e11, e13)) | (~ spl144_40 | ~ spl144_205)), inference(backward_demodulation, [], [f2355, f1338])).
fof(f4905, plain, (~ spl144_42 | ~ spl144_43), inference(avatar_contradiction_clause, [], [f4904])).
fof(f4904, plain, ($false | (~ spl144_42 | ~ spl144_43)), inference(subsumption_resolution, [], [f4903, f507])).
fof(f4903, plain, ((e11 = e12) | (~ spl144_42 | ~ spl144_43)), inference(backward_demodulation, [], [f1351, f1347])).
fof(f4902, plain, (~ spl144_39 | ~ spl144_43), inference(avatar_split_clause, [], [f4901, f1349, f1332])).
fof(f1332, plain, (spl144_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_39])])).
fof(f4901, plain, (~ (e12 = op1(e11, e12)) | ~ spl144_43), inference(backward_demodulation, [], [f441, f1351])).
fof(f441, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4875, plain, (~ spl144_48 | ~ spl144_64), inference(avatar_split_clause, [], [f4868, f1438, f1370])).
fof(f4868, plain, (~ (e13 = op1(e11, e10)) | ~ spl144_64), inference(backward_demodulation, [], [f408, f1440])).
fof(f4849, plain, (~ spl144_60 | ~ spl144_12), inference(avatar_split_clause, [], [f4848, f1217, f1421])).
fof(f1421, plain, (spl144_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_60])])).
fof(f4848, plain, (~ (e13 = op1(e10, e11)) | ~ spl144_12), inference(forward_demodulation, [], [f416, f1219])).
fof(f416, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4845, plain, (spl144_60 | ~ spl144_50 | ~ spl144_210), inference(avatar_split_clause, [], [f4812, f2374, f1379, f1421])).
fof(f1379, plain, (spl144_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_50])])).
fof(f4812, plain, ((e13 = op1(e10, e11)) | (~ spl144_50 | ~ spl144_210)), inference(backward_demodulation, [], [f2376, f1381])).
fof(f1381, plain, ((e11 = op1(e10, e13)) | ~ spl144_50), inference(avatar_component_clause, [], [f1379])).
fof(f4844, plain, (~ spl144_39 | ~ spl144_23), inference(avatar_split_clause, [], [f4843, f1264, f1332])).
fof(f4843, plain, (~ (e12 = op1(e11, e12)) | ~ spl144_23), inference(forward_demodulation, [], [f423, f1266])).
fof(f423, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4809, plain, (~ spl144_53 | ~ spl144_56), inference(avatar_contradiction_clause, [], [f4808])).
fof(f4808, plain, ($false | (~ spl144_53 | ~ spl144_56)), inference(subsumption_resolution, [], [f4807, f506])).
fof(f4807, plain, ((e10 = e13) | (~ spl144_53 | ~ spl144_56)), inference(forward_demodulation, [], [f1406, f1394])).
fof(f4781, plain, (~ spl144_63 | ~ spl144_31), inference(avatar_split_clause, [], [f4780, f1298, f1434])).
fof(f1298, plain, (spl144_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_31])])).
fof(f4780, plain, (~ (op1(e10, e10) = e12) | ~ spl144_31), inference(forward_demodulation, [], [f409, f1300])).
fof(f1300, plain, ((e12 = op1(e12, e10)) | ~ spl144_31), inference(avatar_component_clause, [], [f1298])).
fof(f4770, plain, (~ spl144_41 | ~ spl144_25), inference(avatar_split_clause, [], [f4769, f1273, f1341])).
fof(f4769, plain, (~ (e10 = op1(e11, e11)) | ~ spl144_25), inference(forward_demodulation, [], [f417, f1275])).
fof(f417, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f4760, plain, (~ spl144_38 | ~ spl144_6), inference(avatar_split_clause, [], [f4384, f1192, f1328])).
fof(f4384, plain, (~ (e11 = op1(e11, e12)) | ~ spl144_6), inference(forward_demodulation, [], [f424, f1194])).
fof(f424, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4741, plain, (~ spl144_25 | spl144_30 | ~ spl144_200), inference(avatar_contradiction_clause, [], [f4740])).
fof(f4740, plain, ($false | (~ spl144_25 | spl144_30 | ~ spl144_200)), inference(subsumption_resolution, [], [f4739, f1295])).
fof(f1295, plain, (~ (e11 = op1(e12, e10)) | spl144_30), inference(avatar_component_clause, [], [f1294])).
fof(f4739, plain, ((e11 = op1(e12, e10)) | (~ spl144_25 | ~ spl144_200)), inference(forward_demodulation, [], [f2334, f1275])).
fof(f2334, plain, ((e11 = op1(e12, op1(e12, e11))) | ~ spl144_200), inference(avatar_component_clause, [], [f2332])).
fof(f2332, plain, (spl144_200 <=> (e11 = op1(e12, op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_200])])).
fof(f4736, plain, (~ spl144_82 | ~ spl144_89 | ~ spl144_284), inference(avatar_contradiction_clause, [], [f4735])).
fof(f4735, plain, ($false | (~ spl144_82 | ~ spl144_89 | ~ spl144_284)), inference(subsumption_resolution, [], [f4734, f512])).
fof(f4734, plain, ((e20 = e23) | (~ spl144_82 | ~ spl144_89 | ~ spl144_284)), inference(forward_demodulation, [], [f4733, f1579])).
fof(f4733, plain, ((e23 = op2(e22, e21)) | (~ spl144_82 | ~ spl144_284)), inference(forward_demodulation, [], [f2946, f1549])).
fof(f2946, plain, ((e23 = op2(e22, op2(e22, e23))) | ~ spl144_284), inference(avatar_component_clause, [], [f2944])).
fof(f2944, plain, (spl144_284 <=> (e23 = op2(e22, op2(e22, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl144_284])])).
fof(f4694, plain, (~ spl144_24 | ~ spl144_40), inference(avatar_split_clause, [], [f4691, f1336, f1268])).
fof(f1268, plain, (spl144_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_24])])).
fof(f4691, plain, (~ (e13 = op1(e12, e12)) | ~ spl144_40), inference(backward_demodulation, [], [f423, f1338])).
fof(f4681, plain, (spl144_81 | ~ spl144_96 | ~ spl144_281), inference(avatar_split_clause, [], [f4679, f2932, f1606, f1543])).
fof(f1543, plain, (spl144_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_81])])).
fof(f4679, plain, ((e20 = op2(e22, e23)) | (~ spl144_96 | ~ spl144_281)), inference(backward_demodulation, [], [f2934, f1608])).
fof(f4670, plain, (spl144_121 | ~ spl144_126 | ~ spl144_289), inference(avatar_contradiction_clause, [], [f4669])).
fof(f4669, plain, ($false | (spl144_121 | ~ spl144_126 | ~ spl144_289)), inference(subsumption_resolution, [], [f4667, f1714])).
fof(f1714, plain, (~ (e20 = op2(e20, e21)) | spl144_121), inference(avatar_component_clause, [], [f1713])).
fof(f1713, plain, (spl144_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_121])])).
fof(f4667, plain, ((e20 = op2(e20, e21)) | (~ spl144_126 | ~ spl144_289)), inference(backward_demodulation, [], [f2968, f1736])).
fof(f2968, plain, ((e20 = op2(e20, op2(e20, e20))) | ~ spl144_289), inference(avatar_component_clause, [], [f2966])).
fof(f2966, plain, (spl144_289 <=> (e20 = op2(e20, op2(e20, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_289])])).
fof(f4665, plain, (spl144_48 | ~ spl144_33 | ~ spl144_206), inference(avatar_split_clause, [], [f4664, f2357, f1307, f1370])).
fof(f4664, plain, ((e13 = op1(e11, e10)) | (~ spl144_33 | ~ spl144_206)), inference(forward_demodulation, [], [f2359, f1309])).
fof(f4638, plain, (~ spl144_115 | ~ spl144_294), inference(avatar_split_clause, [], [f4413, f3036, f1687])).
fof(f1687, plain, (spl144_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_115])])).
fof(f4413, plain, (~ (e22 = op2(e20, e23)) | ~ spl144_294), inference(forward_demodulation, [], [f3017, f3037])).
fof(f3017, plain, ~ (op2(e20, e23) = h4(e12)), inference(backward_demodulation, [], [f476, f1105])).
fof(f476, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4633, plain, (~ spl144_81 | ~ spl144_89), inference(avatar_split_clause, [], [f4632, f1577, f1543])).
fof(f4632, plain, (~ (e20 = op2(e22, e23)) | ~ spl144_89), inference(forward_demodulation, [], [f496, f1579])).
fof(f496, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4620, plain, (~ spl144_22 | ~ spl144_6), inference(avatar_split_clause, [], [f4378, f1192, f1260])).
fof(f1260, plain, (spl144_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_22])])).
fof(f4378, plain, (~ (e11 = op1(e12, e12)) | ~ spl144_6), inference(forward_demodulation, [], [f425, f1194])).
fof(f425, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4618, plain, (~ spl144_67 | ~ spl144_68), inference(avatar_contradiction_clause, [], [f4617])).
fof(f4617, plain, ($false | (~ spl144_67 | ~ spl144_68)), inference(subsumption_resolution, [], [f4616, f515])).
fof(f4616, plain, ((e22 = e23) | (~ spl144_67 | ~ spl144_68)), inference(forward_demodulation, [], [f1489, f1485])).
fof(f1489, plain, ((e23 = op2(e23, e23)) | ~ spl144_68), inference(avatar_component_clause, [], [f1487])).
fof(f1487, plain, (spl144_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_68])])).
fof(f4614, plain, (~ spl144_18 | ~ spl144_24 | ~ spl144_201), inference(avatar_contradiction_clause, [], [f4613])).
fof(f4613, plain, ($false | (~ spl144_18 | ~ spl144_24 | ~ spl144_201)), inference(subsumption_resolution, [], [f4612, f507])).
fof(f4612, plain, ((e11 = e12) | (~ spl144_18 | ~ spl144_24 | ~ spl144_201)), inference(forward_demodulation, [], [f4611, f1245])).
fof(f4611, plain, ((e12 = op1(e12, e13)) | (~ spl144_24 | ~ spl144_201)), inference(forward_demodulation, [], [f2338, f1270])).
fof(f1270, plain, ((e13 = op1(e12, e12)) | ~ spl144_24), inference(avatar_component_clause, [], [f1268])).
fof(f2338, plain, ((e12 = op1(e12, op1(e12, e12))) | ~ spl144_201), inference(avatar_component_clause, [], [f2336])).
fof(f2336, plain, (spl144_201 <=> (e12 = op1(e12, op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_201])])).
fof(f4604, plain, (~ spl144_117 | spl144_127 | ~ spl144_291), inference(avatar_contradiction_clause, [], [f4603])).
fof(f4603, plain, ($false | (~ spl144_117 | spl144_127 | ~ spl144_291)), inference(subsumption_resolution, [], [f4602, f1739])).
fof(f1739, plain, (~ (op2(e20, e20) = e22) | spl144_127), inference(avatar_component_clause, [], [f1738])).
fof(f1738, plain, (spl144_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl144_127])])).
fof(f4602, plain, ((op2(e20, e20) = e22) | (~ spl144_117 | ~ spl144_291)), inference(forward_demodulation, [], [f2976, f1698])).
fof(f4562, plain, (~ spl144_18 | ~ spl144_25 | ~ spl144_202), inference(avatar_contradiction_clause, [], [f4561])).
fof(f4561, plain, ($false | (~ spl144_18 | ~ spl144_25 | ~ spl144_202)), inference(subsumption_resolution, [], [f4560, f506])).
fof(f4560, plain, ((e10 = e13) | (~ spl144_18 | ~ spl144_25 | ~ spl144_202)), inference(forward_demodulation, [], [f4559, f1275])).
fof(f4559, plain, ((e13 = op1(e12, e11)) | (~ spl144_18 | ~ spl144_202)), inference(forward_demodulation, [], [f2342, f1245])).
fof(f2342, plain, ((e13 = op1(e12, op1(e12, e13))) | ~ spl144_202), inference(avatar_component_clause, [], [f2340])).
fof(f2340, plain, (spl144_202 <=> (e13 = op1(e12, op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_202])])).
fof(f4535, plain, (spl144_113 | ~ spl144_289 | ~ spl144_386), inference(avatar_split_clause, [], [f4531, f3590, f2966, f1679])).
fof(f4531, plain, ((e20 = op2(e20, e23)) | (~ spl144_289 | ~ spl144_386)), inference(backward_demodulation, [], [f3658, f3591])).
fof(f3658, plain, ((e20 = op2(e20, h1(e12))) | ~ spl144_289), inference(forward_demodulation, [], [f2968, f1093])).
fof(f4525, plain, (~ spl144_304 | ~ spl144_89), inference(avatar_split_clause, [], [f4524, f1577, f3095])).
fof(f3095, plain, (spl144_304 <=> (e20 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_304])])).
fof(f4524, plain, (~ (e20 = h3(e12)) | ~ spl144_89), inference(forward_demodulation, [], [f3012, f1579])).
fof(f3012, plain, ~ (op2(e22, e21) = h3(e12)), inference(backward_demodulation, [], [f495, f1101])).
fof(f495, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f4517, plain, (spl144_300 | ~ spl144_86), inference(avatar_split_clause, [], [f4516, f1564, f3075])).
fof(f1564, plain, (spl144_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_86])])).
fof(f4516, plain, ((e21 = h3(e12)) | ~ spl144_86), inference(forward_demodulation, [], [f1101, f1566])).
fof(f1566, plain, ((e21 = op2(e22, e22)) | ~ spl144_86), inference(avatar_component_clause, [], [f1564])).
fof(f4514, plain, (~ spl144_83 | ~ spl144_294), inference(avatar_split_clause, [], [f4513, f3036, f1551])).
fof(f1551, plain, (spl144_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_83])])).
fof(f4513, plain, (~ (e22 = op2(e22, e23)) | ~ spl144_294), inference(forward_demodulation, [], [f3019, f3037])).
fof(f3019, plain, ~ (op2(e22, e23) = h4(e12)), inference(backward_demodulation, [], [f479, f1105])).
fof(f479, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4512, plain, (~ spl144_348 | ~ spl144_84), inference(avatar_split_clause, [], [f4511, f1555, f3362])).
fof(f4511, plain, (~ (e23 = h3(e12)) | ~ spl144_84), inference(forward_demodulation, [], [f3013, f1557])).
fof(f1557, plain, ((e23 = op2(e22, e23)) | ~ spl144_84), inference(avatar_component_clause, [], [f1555])).
fof(f3013, plain, ~ (op2(e22, e23) = h3(e12)), inference(backward_demodulation, [], [f497, f1101])).
fof(f497, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4496, plain, (~ spl144_77 | spl144_277), inference(avatar_contradiction_clause, [], [f4495])).
fof(f4495, plain, ($false | (~ spl144_77 | spl144_277)), inference(subsumption_resolution, [], [f4494, f1528])).
fof(f4494, plain, (~ (e20 = op2(e23, e20)) | (~ spl144_77 | spl144_277)), inference(forward_demodulation, [], [f2916, f1528])).
fof(f2916, plain, (~ (e20 = op2(e23, op2(e23, e20))) | spl144_277), inference(avatar_component_clause, [], [f2915])).
fof(f2915, plain, (spl144_277 <=> (e20 = op2(e23, op2(e23, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_277])])).
fof(f4491, plain, (~ spl144_12 | ~ spl144_76 | spl144_326), inference(avatar_contradiction_clause, [], [f4490])).
fof(f4490, plain, ($false | (~ spl144_12 | ~ spl144_76 | spl144_326)), inference(subsumption_resolution, [], [f4488, f1102])).
fof(f4488, plain, (~ (e23 = h4(e13)) | (~ spl144_12 | ~ spl144_76 | spl144_326)), inference(backward_demodulation, [], [f4471, f1219])).
fof(f4471, plain, (~ (e23 = h4(op1(e13, e11))) | (~ spl144_76 | spl144_326)), inference(backward_demodulation, [], [f3233, f1523])).
fof(f3233, plain, (~ (op2(e23, e21) = h4(op1(e13, e11))) | spl144_326), inference(avatar_component_clause, [], [f3231])).
fof(f3231, plain, (spl144_326 <=> (op2(e23, e21) = h4(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_326])])).
fof(f4423, plain, (spl144_386 | ~ spl144_128), inference(avatar_split_clause, [], [f4422, f1742, f3590])).
fof(f1742, plain, (spl144_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl144_128])])).
fof(f4422, plain, ((e23 = h1(e12)) | ~ spl144_128), inference(backward_demodulation, [], [f1093, f1744])).
fof(f1744, plain, ((op2(e20, e20) = e23) | ~ spl144_128), inference(avatar_component_clause, [], [f1742])).
fof(f4410, plain, (~ spl144_114 | ~ spl144_82), inference(avatar_split_clause, [], [f4409, f1547, f1683])).
fof(f4409, plain, (~ (e21 = op2(e20, e23)) | ~ spl144_82), inference(forward_demodulation, [], [f475, f1549])).
fof(f4407, plain, (~ spl144_101 | ~ spl144_97), inference(avatar_split_clause, [], [f4238, f1611, f1628])).
fof(f4238, plain, (~ (e20 = op2(e21, e22)) | ~ spl144_97), inference(forward_demodulation, [], [f491, f1613])).
fof(f491, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4398, plain, (~ spl144_78 | ~ spl144_70), inference(avatar_split_clause, [], [f4397, f1496, f1530])).
fof(f1530, plain, (spl144_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_78])])).
fof(f4397, plain, (~ (e21 = op2(e23, e20)) | ~ spl144_70), inference(forward_demodulation, [], [f499, f1498])).
fof(f499, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4395, plain, (~ spl144_75 | ~ spl144_294), inference(avatar_split_clause, [], [f4212, f3036, f1517])).
fof(f1517, plain, (spl144_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_75])])).
fof(f4212, plain, (~ (e22 = op2(e23, e21)) | ~ spl144_294), inference(forward_demodulation, [], [f3021, f3037])).
fof(f3021, plain, ~ (op2(e23, e21) = h4(e12)), inference(backward_demodulation, [], [f502, f1105])).
fof(f502, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4394, plain, (~ spl144_73 | ~ spl144_89), inference(avatar_split_clause, [], [f4393, f1577, f1509])).
fof(f1509, plain, (spl144_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_73])])).
fof(f4393, plain, (~ (e20 = op2(e23, e21)) | ~ spl144_89), inference(forward_demodulation, [], [f467, f1579])).
fof(f467, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4392, plain, (~ spl144_54 | ~ spl144_6), inference(avatar_split_clause, [], [f4391, f1192, f1396])).
fof(f4391, plain, (~ (e11 = op1(e10, e12)) | ~ spl144_6), inference(forward_demodulation, [], [f422, f1194])).
fof(f422, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4388, plain, (~ spl144_55 | ~ spl144_59), inference(avatar_split_clause, [], [f4387, f1417, f1400])).
fof(f1400, plain, (spl144_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_55])])).
fof(f4387, plain, (~ (e12 = op1(e10, e12)) | ~ spl144_59), inference(forward_demodulation, [], [f435, f1419])).
fof(f435, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f4373, plain, (~ spl144_56 | ~ spl144_24), inference(avatar_split_clause, [], [f4372, f1268, f1404])).
fof(f4372, plain, (~ (e13 = op1(e10, e12)) | ~ spl144_24), inference(forward_demodulation, [], [f421, f1270])).
fof(f421, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4369, plain, (spl144_21 | ~ spl144_31 | ~ spl144_199), inference(avatar_split_clause, [], [f4333, f2328, f1298, f1256])).
fof(f1256, plain, (spl144_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_21])])).
fof(f2328, plain, (spl144_199 <=> (e10 = op1(e12, op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_199])])).
fof(f4333, plain, ((e10 = op1(e12, e12)) | (~ spl144_31 | ~ spl144_199)), inference(backward_demodulation, [], [f2330, f1300])).
fof(f2330, plain, ((e10 = op1(e12, op1(e12, e10))) | ~ spl144_199), inference(avatar_component_clause, [], [f2328])).
fof(f4359, plain, (~ spl144_10 | ~ spl144_6), inference(avatar_split_clause, [], [f4358, f1192, f1209])).
fof(f1209, plain, (spl144_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_10])])).
fof(f4358, plain, (~ (e11 = op1(e13, e11)) | ~ spl144_6), inference(forward_demodulation, [], [f453, f1194])).
fof(f453, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4355, plain, (~ spl144_11 | ~ spl144_3), inference(avatar_split_clause, [], [f4354, f1179, f1213])).
fof(f1213, plain, (spl144_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_11])])).
fof(f4354, plain, (~ (e12 = op1(e13, e11)) | ~ spl144_3), inference(forward_demodulation, [], [f454, f1181])).
fof(f454, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4353, plain, (~ spl144_9 | ~ spl144_25), inference(avatar_split_clause, [], [f4352, f1273, f1205])).
fof(f1205, plain, (spl144_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_9])])).
fof(f4352, plain, (~ (e10 = op1(e13, e11)) | ~ spl144_25), inference(forward_demodulation, [], [f419, f1275])).
fof(f419, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4347, plain, (~ spl144_13 | ~ spl144_16), inference(avatar_contradiction_clause, [], [f4346])).
fof(f4346, plain, ($false | (~ spl144_13 | ~ spl144_16)), inference(subsumption_resolution, [], [f4344, f506])).
fof(f4344, plain, ((e10 = e13) | (~ spl144_13 | ~ spl144_16)), inference(backward_demodulation, [], [f1236, f1224])).
fof(f1236, plain, ((e13 = op1(e13, e10)) | ~ spl144_16), inference(avatar_component_clause, [], [f1234])).
fof(f1234, plain, (spl144_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_16])])).
fof(f4324, plain, (spl144_33 | ~ spl144_48 | ~ spl144_203), inference(avatar_split_clause, [], [f4322, f2345, f1370, f1307])).
fof(f4322, plain, ((e10 = op1(e11, e13)) | (~ spl144_48 | ~ spl144_203)), inference(backward_demodulation, [], [f2347, f1372])).
fof(f1372, plain, ((e13 = op1(e11, e10)) | ~ spl144_48), inference(avatar_component_clause, [], [f1370])).
fof(f4323, plain, (~ spl144_32 | ~ spl144_48), inference(avatar_split_clause, [], [f4321, f1370, f1302])).
fof(f4321, plain, (~ (e13 = op1(e12, e10)) | ~ spl144_48), inference(backward_demodulation, [], [f411, f1372])).
fof(f411, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f4320, plain, (~ spl144_49 | ~ spl144_52), inference(avatar_contradiction_clause, [], [f4319])).
fof(f4319, plain, ($false | (~ spl144_49 | ~ spl144_52)), inference(subsumption_resolution, [], [f4317, f506])).
fof(f4317, plain, ((e10 = e13) | (~ spl144_49 | ~ spl144_52)), inference(backward_demodulation, [], [f1389, f1377])).
fof(f4315, plain, (spl144_57 | ~ spl144_62 | ~ spl144_207), inference(avatar_contradiction_clause, [], [f4314])).
fof(f4314, plain, ($false | (spl144_57 | ~ spl144_62 | ~ spl144_207)), inference(subsumption_resolution, [], [f4311, f1410])).
fof(f1410, plain, (~ (e10 = op1(e10, e11)) | spl144_57), inference(avatar_component_clause, [], [f1409])).
fof(f1409, plain, (spl144_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_57])])).
fof(f4311, plain, ((e10 = op1(e10, e11)) | (~ spl144_62 | ~ spl144_207)), inference(backward_demodulation, [], [f2364, f1432])).
fof(f4306, plain, (~ spl144_89 | ~ spl144_91), inference(avatar_contradiction_clause, [], [f4305])).
fof(f4305, plain, ($false | (~ spl144_89 | ~ spl144_91)), inference(subsumption_resolution, [], [f4304, f511])).
fof(f4304, plain, ((e20 = e22) | (~ spl144_89 | ~ spl144_91)), inference(backward_demodulation, [], [f1587, f1579])).
fof(f1587, plain, ((e22 = op2(e22, e21)) | ~ spl144_91), inference(avatar_component_clause, [], [f1585])).
fof(f1585, plain, (spl144_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_91])])).
fof(f4298, plain, (~ spl144_106 | ~ spl144_107), inference(avatar_contradiction_clause, [], [f4297])).
fof(f4297, plain, ($false | (~ spl144_106 | ~ spl144_107)), inference(subsumption_resolution, [], [f4296, f513])).
fof(f4296, plain, ((e21 = e22) | (~ spl144_106 | ~ spl144_107)), inference(forward_demodulation, [], [f1655, f1651])).
fof(f4281, plain, (spl144_316 | ~ spl144_127), inference(avatar_split_clause, [], [f4280, f1738, f3156])).
fof(f4280, plain, ((e22 = h1(e12)) | ~ spl144_127), inference(backward_demodulation, [], [f1093, f1740])).
fof(f1740, plain, ((op2(e20, e20) = e22) | ~ spl144_127), inference(avatar_component_clause, [], [f1738])).
fof(f4256, plain, (~ spl144_118 | ~ spl144_70), inference(avatar_split_clause, [], [f4255, f1496, f1700])).
fof(f4255, plain, (~ (e21 = op2(e20, e22)) | ~ spl144_70), inference(forward_demodulation, [], [f470, f1498])).
fof(f470, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4237, plain, (~ spl144_112 | ~ spl144_104), inference(avatar_split_clause, [], [f4236, f1640, f1674])).
fof(f4236, plain, (~ (e23 = op2(e21, e20)) | ~ spl144_104), inference(forward_demodulation, [], [f487, f1642])).
fof(f487, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f4225, plain, (~ spl144_90 | ~ spl144_82), inference(avatar_split_clause, [], [f4224, f1547, f1581])).
fof(f1581, plain, (spl144_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_90])])).
fof(f4224, plain, (~ (e21 = op2(e22, e21)) | ~ spl144_82), inference(forward_demodulation, [], [f496, f1549])).
fof(f4174, plain, (~ spl144_21 | ~ spl144_25), inference(avatar_contradiction_clause, [], [f4173])).
fof(f4173, plain, ($false | (~ spl144_21 | ~ spl144_25)), inference(subsumption_resolution, [], [f4172, f1275])).
fof(f4172, plain, (~ (e10 = op1(e12, e11)) | ~ spl144_21), inference(forward_demodulation, [], [f447, f1258])).
fof(f1258, plain, ((e10 = op1(e12, e12)) | ~ spl144_21), inference(avatar_component_clause, [], [f1256])).
fof(f447, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4171, plain, (~ spl144_17 | ~ spl144_25), inference(avatar_split_clause, [], [f4170, f1273, f1239])).
fof(f1239, plain, (spl144_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_17])])).
fof(f4170, plain, (~ (e10 = op1(e12, e13)) | ~ spl144_25), inference(forward_demodulation, [], [f448, f1275])).
fof(f448, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4163, plain, (~ spl144_35 | ~ spl144_3), inference(avatar_split_clause, [], [f4161, f1179, f1315])).
fof(f1315, plain, (spl144_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_35])])).
fof(f4161, plain, (~ (e12 = op1(e11, e13)) | ~ spl144_3), inference(backward_demodulation, [], [f430, f1181])).
fof(f430, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4162, plain, (~ spl144_19 | ~ spl144_3), inference(avatar_split_clause, [], [f4160, f1179, f1247])).
fof(f1247, plain, (spl144_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_19])])).
fof(f4160, plain, (~ (e12 = op1(e12, e13)) | ~ spl144_3), inference(backward_demodulation, [], [f431, f1181])).
fof(f431, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4134, plain, (~ spl144_25 | ~ spl144_27), inference(avatar_contradiction_clause, [], [f4133])).
fof(f4133, plain, ($false | (~ spl144_25 | ~ spl144_27)), inference(subsumption_resolution, [], [f4132, f505])).
fof(f4132, plain, ((e10 = e12) | (~ spl144_25 | ~ spl144_27)), inference(backward_demodulation, [], [f1283, f1275])).
fof(f1283, plain, ((e12 = op1(e12, e11)) | ~ spl144_27), inference(avatar_component_clause, [], [f1281])).
fof(f1281, plain, (spl144_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_27])])).
fof(f4126, plain, (~ spl144_18 | ~ spl144_30), inference(avatar_split_clause, [], [f4123, f1294, f1243])).
fof(f4123, plain, (~ (e11 = op1(e12, e13)) | ~ spl144_30), inference(backward_demodulation, [], [f446, f1296])).
fof(f1296, plain, ((e11 = op1(e12, e10)) | ~ spl144_30), inference(avatar_component_clause, [], [f1294])).
fof(f446, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4061, plain, (spl144_320 | ~ spl144_126), inference(avatar_split_clause, [], [f4060, f1734, f3176])).
fof(f4060, plain, ((e21 = h1(e12)) | ~ spl144_126), inference(backward_demodulation, [], [f1093, f1736])).
fof(f4059, plain, (spl144_89 | ~ spl144_294), inference(avatar_split_clause, [], [f4054, f3036, f1577])).
fof(f4054, plain, ((e20 = op2(e22, e21)) | ~ spl144_294), inference(backward_demodulation, [], [f3024, f3037])).
fof(f4058, plain, (spl144_70 | ~ spl144_294), inference(avatar_split_clause, [], [f4053, f3036, f1496])).
fof(f4053, plain, ((e21 = op2(e23, e22)) | ~ spl144_294), inference(backward_demodulation, [], [f3023, f3037])).
fof(f4056, plain, (~ spl144_99 | ~ spl144_294), inference(avatar_split_clause, [], [f4051, f3036, f1619])).
fof(f1619, plain, (spl144_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_99])])).
fof(f4051, plain, (~ (e22 = op2(e21, e23)) | ~ spl144_294), inference(backward_demodulation, [], [f3018, f3037])).
fof(f3018, plain, ~ (op2(e21, e23) = h4(e12)), inference(backward_demodulation, [], [f478, f1105])).
fof(f478, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4027, plain, (spl144_304 | ~ spl144_85), inference(avatar_split_clause, [], [f4026, f1560, f3095])).
fof(f4026, plain, ((e20 = h3(e12)) | ~ spl144_85), inference(forward_demodulation, [], [f1101, f1562])).
fof(f1562, plain, ((e20 = op2(e22, e22)) | ~ spl144_85), inference(avatar_component_clause, [], [f1560])).
fof(f4022, plain, (~ spl144_70 | ~ spl144_74), inference(avatar_split_clause, [], [f4021, f1513, f1496])).
fof(f1513, plain, (spl144_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_74])])).
fof(f4021, plain, (~ (e21 = op2(e23, e22)) | ~ spl144_74), inference(forward_demodulation, [], [f501, f1515])).
fof(f1515, plain, ((e21 = op2(e23, e21)) | ~ spl144_74), inference(avatar_component_clause, [], [f1513])).
fof(f501, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4015, plain, (~ spl144_65 | ~ spl144_67), inference(avatar_contradiction_clause, [], [f4014])).
fof(f4014, plain, ($false | (~ spl144_65 | ~ spl144_67)), inference(subsumption_resolution, [], [f4013, f511])).
fof(f4013, plain, ((e20 = e22) | (~ spl144_65 | ~ spl144_67)), inference(forward_demodulation, [], [f1485, f1477])).
fof(f1477, plain, ((e20 = op2(e23, e23)) | ~ spl144_65), inference(avatar_component_clause, [], [f1475])).
fof(f1475, plain, (spl144_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_65])])).
fof(f3966, plain, (spl144_6 | ~ spl144_3), inference(avatar_split_clause, [], [f3950, f1179, f1192])).
fof(f3950, plain, ((e11 = op1(e13, e12)) | ~ spl144_3), inference(backward_demodulation, [], [f1085, f1181])).
fof(f1085, plain, (e11 = op1(e13, op1(e13, e13))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(e13, e13)) & (e11 = op1(e13, op1(e13, e13))) & (e10 = op1(op1(e13, e13), op1(e13, op1(e13, e13))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax12)).
fof(f3965, plain, (~ spl144_1 | ~ spl144_3), inference(avatar_contradiction_clause, [], [f3964])).
fof(f3964, plain, ($false | (~ spl144_1 | ~ spl144_3)), inference(subsumption_resolution, [], [f3963, f505])).
fof(f3963, plain, ((e10 = e12) | (~ spl144_1 | ~ spl144_3)), inference(forward_demodulation, [], [f1181, f1173])).
fof(f1173, plain, ((e10 = op1(e13, e13)) | ~ spl144_1), inference(avatar_component_clause, [], [f1171])).
fof(f1171, plain, (spl144_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_1])])).
fof(f3959, plain, (~ spl144_2 | ~ spl144_3), inference(avatar_contradiction_clause, [], [f3958])).
fof(f3958, plain, ($false | (~ spl144_2 | ~ spl144_3)), inference(subsumption_resolution, [], [f3957, f507])).
fof(f3957, plain, ((e11 = e12) | (~ spl144_2 | ~ spl144_3)), inference(backward_demodulation, [], [f1181, f1177])).
fof(f1177, plain, ((e11 = op1(e13, e13)) | ~ spl144_2), inference(avatar_component_clause, [], [f1175])).
fof(f1175, plain, (spl144_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_2])])).
fof(f3956, plain, (spl144_25 | ~ spl144_3), inference(avatar_split_clause, [], [f3952, f1179, f1273])).
fof(f3952, plain, ((e10 = op1(e12, e11)) | ~ spl144_3), inference(backward_demodulation, [], [f2987, f1181])).
fof(f2987, plain, (e10 = op1(op1(e13, e13), e11)), inference(forward_demodulation, [], [f1084, f1085])).
fof(f1084, plain, (e10 = op1(op1(e13, e13), op1(e13, op1(e13, e13)))), inference(cnf_transformation, [], [f12])).
fof(f3955, plain, (~ spl144_3 | ~ spl144_5), inference(avatar_contradiction_clause, [], [f3954])).
fof(f3954, plain, ($false | (~ spl144_3 | ~ spl144_5)), inference(subsumption_resolution, [], [f3953, f504])).
fof(f504, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f3953, plain, ((e10 = e11) | (~ spl144_3 | ~ spl144_5)), inference(forward_demodulation, [], [f3950, f1190])).
fof(f1190, plain, ((e10 = op1(e13, e12)) | ~ spl144_5), inference(avatar_component_clause, [], [f1188])).
fof(f1188, plain, (spl144_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_5])])).
fof(f3904, plain, (~ spl144_12 | ~ spl144_28), inference(avatar_split_clause, [], [f3901, f1285, f1217])).
fof(f1285, plain, (spl144_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_28])])).
fof(f3901, plain, (~ (e13 = op1(e13, e11)) | ~ spl144_28), inference(backward_demodulation, [], [f419, f1287])).
fof(f1287, plain, ((e13 = op1(e12, e11)) | ~ spl144_28), inference(avatar_component_clause, [], [f1285])).
fof(f3898, plain, (~ spl144_25 | ~ spl144_29), inference(avatar_split_clause, [], [f3892, f1290, f1273])).
fof(f1290, plain, (spl144_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_29])])).
fof(f3892, plain, (~ (e10 = op1(e12, e11)) | ~ spl144_29), inference(backward_demodulation, [], [f444, f1292])).
fof(f1292, plain, ((e10 = op1(e12, e10)) | ~ spl144_29), inference(avatar_component_clause, [], [f1290])).
fof(f444, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3865, plain, (~ spl144_13 | ~ spl144_45), inference(avatar_split_clause, [], [f3858, f1358, f1222])).
fof(f1358, plain, (spl144_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_45])])).
fof(f3858, plain, (~ (e10 = op1(e13, e10)) | ~ spl144_45), inference(backward_demodulation, [], [f412, f1360])).
fof(f1360, plain, ((e10 = op1(e11, e10)) | ~ spl144_45), inference(avatar_component_clause, [], [f1358])).
fof(f412, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3830, plain, (~ spl144_25 | ~ spl144_57), inference(avatar_split_clause, [], [f3825, f1409, f1273])).
fof(f3825, plain, (~ (e10 = op1(e12, e11)) | ~ spl144_57), inference(backward_demodulation, [], [f415, f1411])).
fof(f1411, plain, ((e10 = op1(e10, e11)) | ~ spl144_57), inference(avatar_component_clause, [], [f1409])).
fof(f415, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3820, plain, (~ spl144_13 | ~ spl144_61), inference(avatar_split_clause, [], [f3813, f1426, f1222])).
fof(f1426, plain, (spl144_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_61])])).
fof(f3813, plain, (~ (e10 = op1(e13, e10)) | ~ spl144_61), inference(backward_demodulation, [], [f410, f1428])).
fof(f1428, plain, ((e10 = op1(e10, e10)) | ~ spl144_61), inference(avatar_component_clause, [], [f1426])).
fof(f410, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3804, plain, (spl144_294 | ~ spl144_67), inference(avatar_split_clause, [], [f3803, f1483, f3036])).
fof(f3803, plain, ((e22 = h4(e12)) | ~ spl144_67), inference(backward_demodulation, [], [f1105, f1485])).
fof(f3778, plain, (spl144_65 | ~ spl144_80 | ~ spl144_277), inference(avatar_split_clause, [], [f3774, f2915, f1538, f1475])).
fof(f1538, plain, (spl144_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_80])])).
fof(f3774, plain, ((e20 = op2(e23, e23)) | (~ spl144_80 | ~ spl144_277)), inference(backward_demodulation, [], [f2917, f1540])).
fof(f1540, plain, ((e23 = op2(e23, e20)) | ~ spl144_80), inference(avatar_component_clause, [], [f1538])).
fof(f2917, plain, ((e20 = op2(e23, op2(e23, e20))) | ~ spl144_277), inference(avatar_component_clause, [], [f2915])).
fof(f3763, plain, (~ spl144_87 | ~ spl144_88), inference(avatar_contradiction_clause, [], [f3762])).
fof(f3762, plain, ($false | (~ spl144_87 | ~ spl144_88)), inference(subsumption_resolution, [], [f3761, f515])).
fof(f3761, plain, ((e22 = e23) | (~ spl144_87 | ~ spl144_88)), inference(backward_demodulation, [], [f1574, f1570])).
fof(f1574, plain, ((e23 = op2(e22, e22)) | ~ spl144_88), inference(avatar_component_clause, [], [f1572])).
fof(f1572, plain, (spl144_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_88])])).
fof(f3760, plain, (spl144_348 | ~ spl144_88), inference(avatar_split_clause, [], [f3759, f1572, f3362])).
fof(f3759, plain, ((e23 = h3(e12)) | ~ spl144_88), inference(backward_demodulation, [], [f1101, f1574])).
fof(f3748, plain, (~ spl144_89 | ~ spl144_93), inference(avatar_split_clause, [], [f3742, f1594, f1577])).
fof(f1594, plain, (spl144_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_93])])).
fof(f3742, plain, (~ (e20 = op2(e22, e21)) | ~ spl144_93), inference(backward_demodulation, [], [f492, f1596])).
fof(f1596, plain, ((e20 = op2(e22, e20)) | ~ spl144_93), inference(avatar_component_clause, [], [f1594])).
fof(f492, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3740, plain, (~ spl144_84 | ~ spl144_100), inference(avatar_split_clause, [], [f3737, f1623, f1555])).
fof(f1623, plain, (spl144_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_100])])).
fof(f3737, plain, (~ (e23 = op2(e22, e23)) | ~ spl144_100), inference(backward_demodulation, [], [f477, f1625])).
fof(f1625, plain, ((e23 = op2(e21, e23)) | ~ spl144_100), inference(avatar_component_clause, [], [f1623])).
fof(f3724, plain, (spl144_312 | ~ spl144_105), inference(avatar_split_clause, [], [f3723, f1645, f3136])).
fof(f3723, plain, ((e20 = h2(e12)) | ~ spl144_105), inference(backward_demodulation, [], [f1097, f1647])).
fof(f1647, plain, ((e20 = op2(e21, e21)) | ~ spl144_105), inference(avatar_component_clause, [], [f1645])).
fof(f3702, plain, (~ spl144_100 | ~ spl144_116), inference(avatar_split_clause, [], [f3699, f1691, f1623])).
fof(f3699, plain, (~ (e23 = op2(e21, e23)) | ~ spl144_116), inference(backward_demodulation, [], [f474, f1693])).
fof(f474, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3686, plain, (~ spl144_89 | ~ spl144_121), inference(avatar_split_clause, [], [f3681, f1713, f1577])).
fof(f3681, plain, (~ (e20 = op2(e22, e21)) | ~ spl144_121), inference(backward_demodulation, [], [f463, f1715])).
fof(f1715, plain, ((e20 = op2(e20, e21)) | ~ spl144_121), inference(avatar_component_clause, [], [f1713])).
fof(f463, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3673, plain, (~ spl144_77 | ~ spl144_125), inference(avatar_split_clause, [], [f3664, f1730, f1526])).
fof(f1730, plain, (spl144_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_125])])).
fof(f3664, plain, (~ (e20 = op2(e23, e20)) | ~ spl144_125), inference(backward_demodulation, [], [f2992, f3661])).
fof(f3661, plain, ((e20 = h1(e12)) | ~ spl144_125), inference(backward_demodulation, [], [f1093, f1732])).
fof(f1732, plain, ((e20 = op2(e20, e20)) | ~ spl144_125), inference(avatar_component_clause, [], [f1730])).
fof(f2992, plain, ~ (op2(e23, e20) = h1(e12)), inference(backward_demodulation, [], [f458, f1093])).
fof(f458, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3282, plain, (~ spl144_323 | spl144_297 | spl144_295 | spl144_293 | ~ spl144_324 | ~ spl144_325 | ~ spl144_326 | ~ spl144_327 | ~ spl144_328 | ~ spl144_329 | ~ spl144_330 | ~ spl144_331 | ~ spl144_332 | ~ spl144_333 | ~ spl144_334 | ~ spl144_335 | ~ spl144_336 | ~ spl144_337 | ~ spl144_338), inference(avatar_split_clause, [], [f3217, f3279, f3275, f3271, f3267, f3263, f3259, f3255, f3251, f3247, f3243, f3239, f3235, f3231, f3227, f3223, f3032, f3044, f3057, f3219])).
fof(f3057, plain, (spl144_297 <=> sP141), introduced(avatar_definition, [new_symbols(naming, [spl144_297])])).
fof(f3044, plain, (spl144_295 <=> sP142), introduced(avatar_definition, [new_symbols(naming, [spl144_295])])).
fof(f3032, plain, (spl144_293 <=> sP143), introduced(avatar_definition, [new_symbols(naming, [spl144_293])])).
fof(f3217, plain, (~ (h1(e12) = h4(op1(e10, e10))) | ~ (op2(e20, e21) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), inference(forward_demodulation, [], [f3216, f1093])).
fof(f3216, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | ~ (op2(e20, e21) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), inference(forward_demodulation, [], [f3215, f3029])).
fof(f3215, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3214, f3029])).
fof(f3214, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e21)) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3213, f3026])).
fof(f3213, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3212, f3029])).
fof(f3212, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3211, f3029])).
fof(f3211, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3210, f1102])).
fof(f3210, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3209, f3026])).
fof(f3209, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3208, f3029])).
fof(f3208, plain, (~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3207, f1097])).
fof(f3207, plain, (~ (op2(e21, e21) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3206, f3026])).
fof(f3206, plain, (~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3205, f3026])).
fof(f3205, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3204, f3026])).
fof(f3204, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3203, f1102])).
fof(f3203, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3202, f3029])).
fof(f3202, plain, (~ (e20 = h4(op1(e12, e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3201, f3024])).
fof(f3201, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3200, f3026])).
fof(f3200, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3199, f1102])).
fof(f3199, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3198, f1102])).
fof(f3198, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e20)) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3197, f3029])).
fof(f3197, plain, (~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3196, f1102])).
fof(f3196, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), e21)) | ~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3195, f3026])).
fof(f3195, plain, (~ (e21 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3194, f3023])).
fof(f3194, plain, (~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3193, f1102])).
fof(f3193, plain, (~ (h4(e12) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3192, f1105])).
fof(f3192, plain, (~ (op2(e23, e23) = h4(op1(e13, e13))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3191, f1102])).
fof(f3191, plain, (sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f1169, f1102])).
fof(f1169, plain, (~ (e23 = h4(e13)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f167])).
fof(f167, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP140 | sP139 | sP138 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e166, e165, e164, e163, e162, e161, e160, e159, e158, e157, e156, e155])).
fof(f155, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP132), inference(usedef, [], [e155])).
fof(e155, plain, (sP132 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP132])])).
fof(f156, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP133), inference(usedef, [], [e156])).
fof(e156, plain, (sP133 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP133])])).
fof(f157, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP134), inference(usedef, [], [e157])).
fof(e157, plain, (sP134 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP134])])).
fof(f158, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP135), inference(usedef, [], [e158])).
fof(e158, plain, (sP135 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP135])])).
fof(f159, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP136), inference(usedef, [], [e159])).
fof(e159, plain, (sP136 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP136])])).
fof(f160, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP137), inference(usedef, [], [e160])).
fof(e160, plain, (sP137 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP137])])).
fof(f161, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP138), inference(usedef, [], [e161])).
fof(e161, plain, (sP138 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP138])])).
fof(f162, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP139), inference(usedef, [], [e162])).
fof(e162, plain, (sP139 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP139])])).
fof(f163, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP140), inference(usedef, [], [e163])).
fof(e163, plain, (sP140 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP140])])).
fof(f164, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP141), inference(usedef, [], [e164])).
fof(e164, plain, (sP141 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP141])])).
fof(f165, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP142), inference(usedef, [], [e165])).
fof(e165, plain, (sP142 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP142])])).
fof(f166, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP143), inference(usedef, [], [e166])).
fof(e166, plain, (sP143 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP143])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', co1)).
fof(f3067, plain, ~ spl144_297, inference(avatar_split_clause, [], [f3066, f3057])).
fof(f3066, plain, ~ sP141, inference(subsumption_resolution, [], [f1114, f3029])).
fof(f1114, plain, (~ (e20 = h4(e10)) | ~ sP141), inference(cnf_transformation, [], [f302])).
fof(f302, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP141), inference(nnf_transformation, [], [f164])).
fof(f3053, plain, ~ spl144_295, inference(avatar_split_clause, [], [f3052, f3044])).
fof(f3052, plain, ~ sP142, inference(subsumption_resolution, [], [f1111, f3026])).
fof(f1111, plain, (~ (e21 = h4(e11)) | ~ sP142), inference(cnf_transformation, [], [f301])).
fof(f301, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP142), inference(nnf_transformation, [], [f165])).
fof(f3039, plain, (~ spl144_293 | ~ spl144_294), inference(avatar_split_clause, [], [f1108, f3036, f3032])).
fof(f1108, plain, (~ (e22 = h4(e12)) | ~ sP143), inference(cnf_transformation, [], [f300])).
fof(f300, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP143), inference(nnf_transformation, [], [f166])).
fof(f2988, plain, spl144_67, inference(avatar_split_clause, [], [f1089, f1483])).
fof(f1089, plain, (e22 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f2986, plain, spl144_3, inference(avatar_split_clause, [], [f1086, f1179])).
fof(f1086, plain, (e12 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f2985, plain, (spl144_276 | spl144_275 | spl144_274 | ~ spl144_113), inference(avatar_split_clause, [], [f1072, f1679, f2887, f2895, f2903])).
fof(f2903, plain, (spl144_276 <=> sP66), introduced(avatar_definition, [new_symbols(naming, [spl144_276])])).
fof(f2895, plain, (spl144_275 <=> sP67), introduced(avatar_definition, [new_symbols(naming, [spl144_275])])).
fof(f2887, plain, (spl144_274 <=> sP68), introduced(avatar_definition, [new_symbols(naming, [spl144_274])])).
fof(f1072, plain, (~ (e20 = op2(e20, e23)) | sP68 | sP67 | sP66), inference(cnf_transformation, [], [f154])).
fof(f154, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP131 | sP130 | sP129 | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69) & ((e23 = op2(e23, op2(e23, e23))) | (e22 = op2(e23, op2(e23, e22))) | (e21 = op2(e23, op2(e23, e21))) | (e20 = op2(e23, op2(e23, e20)))) & ((e23 = op2(e22, op2(e22, e23))) | (e22 = op2(e22, op2(e22, e22))) | (e21 = op2(e22, op2(e22, e21))) | (e20 = op2(e22, op2(e22, e20)))) & ((e23 = op2(e21, op2(e21, e23))) | (e22 = op2(e21, op2(e21, e22))) | (e21 = op2(e21, op2(e21, e21))) | (e20 = op2(e21, op2(e21, e20)))) & ((e23 = op2(e20, op2(e20, e23))) | (e22 = op2(e20, op2(e20, e22))) | (e21 = op2(e20, op2(e20, e21))) | (e20 = op2(e20, op2(e20, e20)))) & ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e22, e23)) & ~ (e21 = op2(e21, e23)) & ~ (e20 = op2(e20, e23))) | sP68 | sP67 | sP66)), inference(definition_folding, [], [f11, e153, e152, e151, e150, e149, e148, e147, e146, e145, e144, e143, e142, e141, e140, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124, e123, e122, e121, e120, e119, e118, e117, e116, e115, e114, e113, e112, e111, e110, e109, e108, e107, e106, e105, e104, e103, e102, e101, e100, e99, e98, e97, e96, e95, e94, e93, e92, e91, e90, e89, e88])).
fof(f88, plain, ((~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20))) | ~ sP66), inference(usedef, [], [e88])).
fof(e88, plain, (sP66 <=> (~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP66])])).
fof(f89, plain, ((~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21))) | ~ sP67), inference(usedef, [], [e89])).
fof(e89, plain, (sP67 <=> (~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP67])])).
fof(f90, plain, ((~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22))) | ~ sP68), inference(usedef, [], [e90])).
fof(e90, plain, (sP68 <=> (~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP68])])).
fof(f91, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP69), inference(usedef, [], [e91])).
fof(e91, plain, (sP69 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP69])])).
fof(f92, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP70), inference(usedef, [], [e92])).
fof(e92, plain, (sP70 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP70])])).
fof(f93, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP71), inference(usedef, [], [e93])).
fof(e93, plain, (sP71 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP71])])).
fof(f94, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP72), inference(usedef, [], [e94])).
fof(e94, plain, (sP72 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP72])])).
fof(f95, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP73), inference(usedef, [], [e95])).
fof(e95, plain, (sP73 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP73])])).
fof(f96, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP74), inference(usedef, [], [e96])).
fof(e96, plain, (sP74 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP74])])).
fof(f97, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP75), inference(usedef, [], [e97])).
fof(e97, plain, (sP75 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP75])])).
fof(f98, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP76), inference(usedef, [], [e98])).
fof(e98, plain, (sP76 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP76])])).
fof(f99, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP77), inference(usedef, [], [e99])).
fof(e99, plain, (sP77 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP77])])).
fof(f100, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP78), inference(usedef, [], [e100])).
fof(e100, plain, (sP78 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP78])])).
fof(f101, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP79), inference(usedef, [], [e101])).
fof(e101, plain, (sP79 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP79])])).
fof(f102, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP80), inference(usedef, [], [e102])).
fof(e102, plain, (sP80 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP80])])).
fof(f103, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP81), inference(usedef, [], [e103])).
fof(e103, plain, (sP81 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP81])])).
fof(f104, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP82), inference(usedef, [], [e104])).
fof(e104, plain, (sP82 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP82])])).
fof(f105, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP83), inference(usedef, [], [e105])).
fof(e105, plain, (sP83 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP83])])).
fof(f106, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP84), inference(usedef, [], [e106])).
fof(e106, plain, (sP84 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP84])])).
fof(f107, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP85), inference(usedef, [], [e107])).
fof(e107, plain, (sP85 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP85])])).
fof(f108, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP86), inference(usedef, [], [e108])).
fof(e108, plain, (sP86 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP86])])).
fof(f109, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP87), inference(usedef, [], [e109])).
fof(e109, plain, (sP87 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP87])])).
fof(f110, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | ~ sP88), inference(usedef, [], [e110])).
fof(e110, plain, (sP88 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP88])])).
fof(f111, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP89), inference(usedef, [], [e111])).
fof(e111, plain, (sP89 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP89])])).
fof(f112, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP90), inference(usedef, [], [e112])).
fof(e112, plain, (sP90 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP90])])).
fof(f113, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP91), inference(usedef, [], [e113])).
fof(e113, plain, (sP91 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP91])])).
fof(f114, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP92), inference(usedef, [], [e114])).
fof(e114, plain, (sP92 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP92])])).
fof(f115, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP93), inference(usedef, [], [e115])).
fof(e115, plain, (sP93 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP93])])).
fof(f116, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP94), inference(usedef, [], [e116])).
fof(e116, plain, (sP94 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP94])])).
fof(f117, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP95), inference(usedef, [], [e117])).
fof(e117, plain, (sP95 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP95])])).
fof(f118, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | ~ sP96), inference(usedef, [], [e118])).
fof(e118, plain, (sP96 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP96])])).
fof(f119, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP97), inference(usedef, [], [e119])).
fof(e119, plain, (sP97 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP97])])).
fof(f120, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP98), inference(usedef, [], [e120])).
fof(e120, plain, (sP98 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP98])])).
fof(f121, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP99), inference(usedef, [], [e121])).
fof(e121, plain, (sP99 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP99])])).
fof(f122, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | ~ sP100), inference(usedef, [], [e122])).
fof(e122, plain, (sP100 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP100])])).
fof(f123, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP101), inference(usedef, [], [e123])).
fof(e123, plain, (sP101 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP101])])).
fof(f124, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP102), inference(usedef, [], [e124])).
fof(e124, plain, (sP102 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP102])])).
fof(f125, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP103), inference(usedef, [], [e125])).
fof(e125, plain, (sP103 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP103])])).
fof(f126, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | ~ sP104), inference(usedef, [], [e126])).
fof(e126, plain, (sP104 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP104])])).
fof(f127, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP105), inference(usedef, [], [e127])).
fof(e127, plain, (sP105 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP105])])).
fof(f128, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP106), inference(usedef, [], [e128])).
fof(e128, plain, (sP106 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP106])])).
fof(f129, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP107), inference(usedef, [], [e129])).
fof(e129, plain, (sP107 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP107])])).
fof(f130, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | ~ sP108), inference(usedef, [], [e130])).
fof(e130, plain, (sP108 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP108])])).
fof(f131, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP109), inference(usedef, [], [e131])).
fof(e131, plain, (sP109 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP109])])).
fof(f132, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP110), inference(usedef, [], [e132])).
fof(e132, plain, (sP110 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP110])])).
fof(f133, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP111), inference(usedef, [], [e133])).
fof(e133, plain, (sP111 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP111])])).
fof(f134, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP112), inference(usedef, [], [e134])).
fof(e134, plain, (sP112 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP112])])).
fof(f135, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP113), inference(usedef, [], [e135])).
fof(e135, plain, (sP113 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP113])])).
fof(f136, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP114), inference(usedef, [], [e136])).
fof(e136, plain, (sP114 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP114])])).
fof(f137, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP115), inference(usedef, [], [e137])).
fof(e137, plain, (sP115 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP115])])).
fof(f138, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | ~ sP116), inference(usedef, [], [e138])).
fof(e138, plain, (sP116 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP116])])).
fof(f139, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP117), inference(usedef, [], [e139])).
fof(e139, plain, (sP117 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP117])])).
fof(f140, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP118), inference(usedef, [], [e140])).
fof(e140, plain, (sP118 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP118])])).
fof(f141, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP119), inference(usedef, [], [e141])).
fof(e141, plain, (sP119 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP119])])).
fof(f142, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | ~ sP120), inference(usedef, [], [e142])).
fof(e142, plain, (sP120 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP120])])).
fof(f143, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP121), inference(usedef, [], [e143])).
fof(e143, plain, (sP121 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP121])])).
fof(f144, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP122), inference(usedef, [], [e144])).
fof(e144, plain, (sP122 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP122])])).
fof(f145, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP123), inference(usedef, [], [e145])).
fof(e145, plain, (sP123 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP123])])).
fof(f146, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | ~ sP124), inference(usedef, [], [e146])).
fof(e146, plain, (sP124 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP124])])).
fof(f147, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP125), inference(usedef, [], [e147])).
fof(e147, plain, (sP125 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP125])])).
fof(f148, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP126), inference(usedef, [], [e148])).
fof(e148, plain, (sP126 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP126])])).
fof(f149, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP127), inference(usedef, [], [e149])).
fof(e149, plain, (sP127 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP127])])).
fof(f150, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | ~ sP128), inference(usedef, [], [e150])).
fof(e150, plain, (sP128 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP128])])).
fof(f151, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP129), inference(usedef, [], [e151])).
fof(e151, plain, (sP129 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP129])])).
fof(f152, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP130), inference(usedef, [], [e152])).
fof(e152, plain, (sP130 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP130])])).
fof(f153, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP131), inference(usedef, [], [e153])).
fof(e153, plain, (sP131 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP131])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e23 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e23 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e23 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e22 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e22 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e22 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e21 = op2(e23, e21)) & (e21 = op2(e21, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e21 = op2(e22, e21)) & (e21 = op2(e21, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e21 = op2(e20, e21)) & (e21 = op2(e21, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & (e20 = op2(e23, e20)) & (e20 = op2(e20, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & (e20 = op2(e22, e20)) & (e20 = op2(e20, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & (e20 = op2(e21, e20)) & (e20 = op2(e20, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))) & ((e23 = op2(e23, op2(e23, e23))) | (e22 = op2(e23, op2(e23, e22))) | (e21 = op2(e23, op2(e23, e21))) | (e20 = op2(e23, op2(e23, e20)))) & ((e23 = op2(e22, op2(e22, e23))) | (e22 = op2(e22, op2(e22, e22))) | (e21 = op2(e22, op2(e22, e21))) | (e20 = op2(e22, op2(e22, e20)))) & ((e23 = op2(e21, op2(e21, e23))) | (e22 = op2(e21, op2(e21, e22))) | (e21 = op2(e21, op2(e21, e21))) | (e20 = op2(e21, op2(e21, e20)))) & ((e23 = op2(e20, op2(e20, e23))) | (e22 = op2(e20, op2(e20, e22))) | (e21 = op2(e20, op2(e20, e21))) | (e20 = op2(e20, op2(e20, e20)))) & ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e22, e23)) & ~ (e21 = op2(e21, e23)) & ~ (e20 = op2(e20, e23))) | (~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22))) | (~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21))) | (~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax11)).
fof(f2981, plain, (spl144_289 | spl144_290 | spl144_291 | spl144_292), inference(avatar_split_clause, [], [f1076, f2978, f2974, f2970, f2966])).
fof(f1076, plain, ((e23 = op2(e20, op2(e20, e23))) | (e22 = op2(e20, op2(e20, e22))) | (e21 = op2(e20, op2(e20, e21))) | (e20 = op2(e20, op2(e20, e20)))), inference(cnf_transformation, [], [f154])).
fof(f2964, plain, (spl144_285 | spl144_286 | spl144_287 | spl144_288), inference(avatar_split_clause, [], [f1077, f2961, f2957, f2953, f2949])).
fof(f1077, plain, ((e23 = op2(e21, op2(e21, e23))) | (e22 = op2(e21, op2(e21, e22))) | (e21 = op2(e21, op2(e21, e21))) | (e20 = op2(e21, op2(e21, e20)))), inference(cnf_transformation, [], [f154])).
fof(f2947, plain, (spl144_281 | spl144_282 | spl144_283 | spl144_284), inference(avatar_split_clause, [], [f1078, f2944, f2940, f2936, f2932])).
fof(f1078, plain, ((e23 = op2(e22, op2(e22, e23))) | (e22 = op2(e22, op2(e22, e22))) | (e21 = op2(e22, op2(e22, e21))) | (e20 = op2(e22, op2(e22, e20)))), inference(cnf_transformation, [], [f154])).
fof(f2908, plain, (~ spl144_276 | ~ spl144_110), inference(avatar_split_clause, [], [f1069, f1666, f2903])).
fof(f1069, plain, (~ (e21 = op2(e21, e20)) | ~ sP66), inference(cnf_transformation, [], [f299])).
fof(f299, plain, ((~ (e23 = op2(e23, e20)) & ~ (e22 = op2(e22, e20)) & ~ (e21 = op2(e21, e20)) & ~ (e20 = op2(e20, e20))) | ~ sP66), inference(nnf_transformation, [], [f88])).
fof(f2898, plain, (~ spl144_275 | ~ spl144_76), inference(avatar_split_clause, [], [f1067, f1521, f2895])).
fof(f1067, plain, (~ (e23 = op2(e23, e21)) | ~ sP67), inference(cnf_transformation, [], [f298])).
fof(f298, plain, ((~ (e23 = op2(e23, e21)) & ~ (e22 = op2(e22, e21)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e20, e21))) | ~ sP67), inference(nnf_transformation, [], [f89])).
fof(f2891, plain, (~ spl144_274 | ~ spl144_87), inference(avatar_split_clause, [], [f1062, f1568, f2887])).
fof(f1062, plain, (~ (e22 = op2(e22, e22)) | ~ sP68), inference(cnf_transformation, [], [f297])).
fof(f297, plain, ((~ (e23 = op2(e23, e22)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e21, e22)) & ~ (e20 = op2(e20, e22))) | ~ sP68), inference(nnf_transformation, [], [f90])).
fof(f2381, plain, (spl144_194 | spl144_193 | spl144_192 | ~ spl144_49), inference(avatar_split_clause, [], [f796, f1375, f2283, f2291, f2299])).
fof(f2299, plain, (spl144_194 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl144_194])])).
fof(f2291, plain, (spl144_193 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl144_193])])).
fof(f2283, plain, (spl144_192 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl144_192])])).
fof(f796, plain, (~ (e10 = op1(e10, e13)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP65 | sP64 | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3) & ((e13 = op1(e13, op1(e13, e13))) | (e12 = op1(e13, op1(e13, e12))) | (e11 = op1(e13, op1(e13, e11))) | (e10 = op1(e13, op1(e13, e10)))) & ((e13 = op1(e12, op1(e12, e13))) | (e12 = op1(e12, op1(e12, e12))) | (e11 = op1(e12, op1(e12, e11))) | (e10 = op1(e12, op1(e12, e10)))) & ((e13 = op1(e11, op1(e11, e13))) | (e12 = op1(e11, op1(e11, e12))) | (e11 = op1(e11, op1(e11, e11))) | (e10 = op1(e11, op1(e11, e10)))) & ((e13 = op1(e10, op1(e10, e13))) | (e12 = op1(e10, op1(e10, e12))) | (e11 = op1(e10, op1(e10, e11))) | (e10 = op1(e10, op1(e10, e10)))) & ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e12, e13)) & ~ (e11 = op1(e11, e13)) & ~ (e10 = op1(e10, e13))) | sP2 | sP1 | sP0)), inference(definition_folding, [], [f10, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f36, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP15), inference(usedef, [], [e36])).
fof(e36, plain, (sP15 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f37, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP16), inference(usedef, [], [e37])).
fof(e37, plain, (sP16 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f38, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP17), inference(usedef, [], [e38])).
fof(e38, plain, (sP17 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f39, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP18), inference(usedef, [], [e39])).
fof(e39, plain, (sP18 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f40, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP19), inference(usedef, [], [e40])).
fof(e40, plain, (sP19 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f41, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP20), inference(usedef, [], [e41])).
fof(e41, plain, (sP20 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f42, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP21), inference(usedef, [], [e42])).
fof(e42, plain, (sP21 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f43, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | ~ sP22), inference(usedef, [], [e43])).
fof(e43, plain, (sP22 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f44, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP23), inference(usedef, [], [e44])).
fof(e44, plain, (sP23 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f45, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP24), inference(usedef, [], [e45])).
fof(e45, plain, (sP24 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f46, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP25), inference(usedef, [], [e46])).
fof(e46, plain, (sP25 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f47, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP26), inference(usedef, [], [e47])).
fof(e47, plain, (sP26 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f48, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP27), inference(usedef, [], [e48])).
fof(e48, plain, (sP27 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f49, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP28), inference(usedef, [], [e49])).
fof(e49, plain, (sP28 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f50, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP29), inference(usedef, [], [e50])).
fof(e50, plain, (sP29 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f51, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | ~ sP30), inference(usedef, [], [e51])).
fof(e51, plain, (sP30 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f52, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP31), inference(usedef, [], [e52])).
fof(e52, plain, (sP31 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f53, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP32), inference(usedef, [], [e53])).
fof(e53, plain, (sP32 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f54, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP33), inference(usedef, [], [e54])).
fof(e54, plain, (sP33 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f55, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | ~ sP34), inference(usedef, [], [e55])).
fof(e55, plain, (sP34 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f56, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP35), inference(usedef, [], [e56])).
fof(e56, plain, (sP35 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f57, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP36), inference(usedef, [], [e57])).
fof(e57, plain, (sP36 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f58, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP37), inference(usedef, [], [e58])).
fof(e58, plain, (sP37 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f59, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | ~ sP38), inference(usedef, [], [e59])).
fof(e59, plain, (sP38 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f60, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP39), inference(usedef, [], [e60])).
fof(e60, plain, (sP39 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f61, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP40), inference(usedef, [], [e61])).
fof(e61, plain, (sP40 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f62, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP41), inference(usedef, [], [e62])).
fof(e62, plain, (sP41 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f63, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | ~ sP42), inference(usedef, [], [e63])).
fof(e63, plain, (sP42 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f64, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP43), inference(usedef, [], [e64])).
fof(e64, plain, (sP43 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f65, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP44), inference(usedef, [], [e65])).
fof(e65, plain, (sP44 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f66, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP45), inference(usedef, [], [e66])).
fof(e66, plain, (sP45 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f67, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP46), inference(usedef, [], [e67])).
fof(e67, plain, (sP46 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f68, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP47), inference(usedef, [], [e68])).
fof(e68, plain, (sP47 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f69, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP48), inference(usedef, [], [e69])).
fof(e69, plain, (sP48 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP48])])).
fof(f70, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP49), inference(usedef, [], [e70])).
fof(e70, plain, (sP49 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP49])])).
fof(f71, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | ~ sP50), inference(usedef, [], [e71])).
fof(e71, plain, (sP50 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP50])])).
fof(f72, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP51), inference(usedef, [], [e72])).
fof(e72, plain, (sP51 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP51])])).
fof(f73, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP52), inference(usedef, [], [e73])).
fof(e73, plain, (sP52 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP52])])).
fof(f74, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP53), inference(usedef, [], [e74])).
fof(e74, plain, (sP53 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP53])])).
fof(f75, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | ~ sP54), inference(usedef, [], [e75])).
fof(e75, plain, (sP54 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP54])])).
fof(f76, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP55), inference(usedef, [], [e76])).
fof(e76, plain, (sP55 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP55])])).
fof(f77, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP56), inference(usedef, [], [e77])).
fof(e77, plain, (sP56 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP56])])).
fof(f78, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP57), inference(usedef, [], [e78])).
fof(e78, plain, (sP57 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP57])])).
fof(f79, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | ~ sP58), inference(usedef, [], [e79])).
fof(e79, plain, (sP58 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP58])])).
fof(f80, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP59), inference(usedef, [], [e80])).
fof(e80, plain, (sP59 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP59])])).
fof(f81, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP60), inference(usedef, [], [e81])).
fof(e81, plain, (sP60 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP60])])).
fof(f82, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP61), inference(usedef, [], [e82])).
fof(e82, plain, (sP61 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP61])])).
fof(f83, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | ~ sP62), inference(usedef, [], [e83])).
fof(e83, plain, (sP62 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP62])])).
fof(f84, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP63), inference(usedef, [], [e84])).
fof(e84, plain, (sP63 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP63])])).
fof(f85, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP64), inference(usedef, [], [e85])).
fof(e85, plain, (sP64 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP64])])).
fof(f86, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP65), inference(usedef, [], [e86])).
fof(e86, plain, (sP65 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP65])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e13 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e13 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e13 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e12 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e12 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e12 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e11 = op1(e13, e11)) & (e11 = op1(e11, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e11 = op1(e12, e11)) & (e11 = op1(e11, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e11 = op1(e10, e11)) & (e11 = op1(e11, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & (e10 = op1(e13, e10)) & (e10 = op1(e10, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & (e10 = op1(e12, e10)) & (e10 = op1(e10, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & (e10 = op1(e11, e10)) & (e10 = op1(e10, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))) & ((e13 = op1(e13, op1(e13, e13))) | (e12 = op1(e13, op1(e13, e12))) | (e11 = op1(e13, op1(e13, e11))) | (e10 = op1(e13, op1(e13, e10)))) & ((e13 = op1(e12, op1(e12, e13))) | (e12 = op1(e12, op1(e12, e12))) | (e11 = op1(e12, op1(e12, e11))) | (e10 = op1(e12, op1(e12, e10)))) & ((e13 = op1(e11, op1(e11, e13))) | (e12 = op1(e11, op1(e11, e12))) | (e11 = op1(e11, op1(e11, e11))) | (e10 = op1(e11, op1(e11, e10)))) & ((e13 = op1(e10, op1(e10, e13))) | (e12 = op1(e10, op1(e10, e12))) | (e11 = op1(e10, op1(e10, e11))) | (e10 = op1(e10, op1(e10, e10)))) & ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e12, e13)) & ~ (e11 = op1(e11, e13)) & ~ (e10 = op1(e10, e13))) | (~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12))) | (~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11))) | (~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax10)).
fof(f2377, plain, (spl144_207 | spl144_208 | spl144_209 | spl144_210), inference(avatar_split_clause, [], [f800, f2374, f2370, f2366, f2362])).
fof(f800, plain, ((e13 = op1(e10, op1(e10, e13))) | (e12 = op1(e10, op1(e10, e12))) | (e11 = op1(e10, op1(e10, e11))) | (e10 = op1(e10, op1(e10, e10)))), inference(cnf_transformation, [], [f87])).
fof(f2360, plain, (spl144_203 | spl144_204 | spl144_205 | spl144_206), inference(avatar_split_clause, [], [f801, f2357, f2353, f2349, f2345])).
fof(f801, plain, ((e13 = op1(e11, op1(e11, e13))) | (e12 = op1(e11, op1(e11, e12))) | (e11 = op1(e11, op1(e11, e11))) | (e10 = op1(e11, op1(e11, e10)))), inference(cnf_transformation, [], [f87])).
fof(f2343, plain, (spl144_199 | spl144_200 | spl144_201 | spl144_202), inference(avatar_split_clause, [], [f802, f2340, f2336, f2332, f2328])).
fof(f802, plain, ((e13 = op1(e12, op1(e12, e13))) | (e12 = op1(e12, op1(e12, e12))) | (e11 = op1(e12, op1(e12, e11))) | (e10 = op1(e12, op1(e12, e10)))), inference(cnf_transformation, [], [f87])).
fof(f2304, plain, (~ spl144_194 | ~ spl144_46), inference(avatar_split_clause, [], [f793, f1362, f2299])).
fof(f793, plain, (~ (e11 = op1(e11, e10)) | ~ sP0), inference(cnf_transformation, [], [f233])).
fof(f233, plain, ((~ (e13 = op1(e13, e10)) & ~ (e12 = op1(e12, e10)) & ~ (e11 = op1(e11, e10)) & ~ (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f2294, plain, (~ spl144_193 | ~ spl144_12), inference(avatar_split_clause, [], [f791, f1217, f2291])).
fof(f791, plain, (~ (e13 = op1(e13, e11)) | ~ sP1), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ((~ (e13 = op1(e13, e11)) & ~ (e12 = op1(e12, e11)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f2287, plain, (~ spl144_192 | ~ spl144_23), inference(avatar_split_clause, [], [f786, f1264, f2283])).
fof(f786, plain, (~ (e12 = op1(e12, e12)) | ~ sP2), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ((~ (e13 = op1(e13, e12)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e11, e12)) & ~ (e10 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1777, plain, (spl144_125 | spl144_121 | spl144_117 | spl144_113), inference(avatar_split_clause, [], [f376, f1679, f1696, f1713, f1730])).
fof(f376, plain, ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax4)).
fof(f1774, plain, (spl144_126 | spl144_110 | spl144_94 | spl144_78), inference(avatar_split_clause, [], [f379, f1530, f1598, f1666, f1734])).
fof(f379, plain, ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f1773, plain, (spl144_127 | spl144_123 | spl144_119 | spl144_115), inference(avatar_split_clause, [], [f380, f1687, f1704, f1721, f1738])).
fof(f380, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1771, plain, (spl144_128 | spl144_124 | spl144_120 | spl144_116), inference(avatar_split_clause, [], [f382, f1691, f1708, f1725, f1742])).
fof(f382, plain, ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1770, plain, (spl144_128 | spl144_112 | spl144_96 | spl144_80), inference(avatar_split_clause, [], [f383, f1538, f1606, f1674, f1742])).
fof(f383, plain, ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1766, plain, (spl144_122 | spl144_106 | spl144_90 | spl144_74), inference(avatar_split_clause, [], [f387, f1513, f1581, f1649, f1717])).
fof(f387, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1765, plain, (spl144_111 | spl144_107 | spl144_103 | spl144_99), inference(avatar_split_clause, [], [f388, f1619, f1636, f1653, f1670])).
fof(f388, plain, ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1764, plain, (spl144_123 | spl144_107 | spl144_91 | spl144_75), inference(avatar_split_clause, [], [f389, f1517, f1585, f1653, f1721])).
fof(f389, plain, ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1757, plain, (spl144_95 | spl144_91 | spl144_87 | spl144_83), inference(avatar_split_clause, [], [f396, f1551, f1568, f1585, f1602])).
fof(f396, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1754, plain, (spl144_120 | spl144_104 | spl144_88 | spl144_72), inference(avatar_split_clause, [], [f399, f1504, f1572, f1640, f1708])).
fof(f399, plain, ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1753, plain, (spl144_77 | spl144_73 | spl144_69 | spl144_65), inference(avatar_split_clause, [], [f400, f1475, f1492, f1509, f1526])).
fof(f400, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1752, plain, (spl144_113 | spl144_97 | spl144_81 | spl144_65), inference(avatar_split_clause, [], [f401, f1475, f1543, f1611, f1679])).
fof(f401, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1747, plain, (spl144_80 | spl144_76 | spl144_72 | spl144_68), inference(avatar_split_clause, [], [f406, f1487, f1504, f1521, f1538])).
fof(f406, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1745, plain, (spl144_125 | spl144_126 | spl144_127 | spl144_128), inference(avatar_split_clause, [], [f360, f1742, f1738, f1734, f1730])).
fof(f360, plain, ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax3)).
fof(f1728, plain, (spl144_121 | spl144_122 | spl144_123 | spl144_124), inference(avatar_split_clause, [], [f361, f1725, f1721, f1717, f1713])).
fof(f361, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f1711, plain, (spl144_117 | spl144_118 | spl144_119 | spl144_120), inference(avatar_split_clause, [], [f362, f1708, f1704, f1700, f1696])).
fof(f362, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f1694, plain, (spl144_113 | spl144_114 | spl144_115 | spl144_116), inference(avatar_split_clause, [], [f363, f1691, f1687, f1683, f1679])).
fof(f363, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f1643, plain, (spl144_101 | spl144_102 | spl144_103 | spl144_104), inference(avatar_split_clause, [], [f366, f1640, f1636, f1632, f1628])).
fof(f366, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f1626, plain, (spl144_97 | spl144_98 | spl144_99 | spl144_100), inference(avatar_split_clause, [], [f367, f1623, f1619, f1615, f1611])).
fof(f367, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1609, plain, (spl144_93 | spl144_94 | spl144_95 | spl144_96), inference(avatar_split_clause, [], [f368, f1606, f1602, f1598, f1594])).
fof(f368, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f1575, plain, (spl144_85 | spl144_86 | spl144_87 | spl144_88), inference(avatar_split_clause, [], [f370, f1572, f1568, f1564, f1560])).
fof(f370, plain, ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f3])).
fof(f1558, plain, (spl144_81 | spl144_82 | spl144_83 | spl144_84), inference(avatar_split_clause, [], [f371, f1555, f1551, f1547, f1543])).
fof(f371, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f1473, plain, (spl144_61 | spl144_57 | spl144_53 | spl144_49), inference(avatar_split_clause, [], [f328, f1375, f1392, f1409, f1426])).
fof(f328, plain, ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax2)).
fof(f1471, plain, (spl144_62 | spl144_58 | spl144_54 | spl144_50), inference(avatar_split_clause, [], [f330, f1379, f1396, f1413, f1430])).
fof(f330, plain, ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f1467, plain, (spl144_64 | spl144_60 | spl144_56 | spl144_52), inference(avatar_split_clause, [], [f334, f1387, f1404, f1421, f1438])).
fof(f334, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f1466, plain, (spl144_64 | spl144_48 | spl144_32 | spl144_16), inference(avatar_split_clause, [], [f335, f1234, f1302, f1370, f1438])).
fof(f335, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f1463, plain, (spl144_46 | spl144_42 | spl144_38 | spl144_34), inference(avatar_split_clause, [], [f338, f1311, f1328, f1345, f1362])).
fof(f338, plain, ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f1461, plain, (spl144_47 | spl144_43 | spl144_39 | spl144_35), inference(avatar_split_clause, [], [f340, f1315, f1332, f1349, f1366])).
fof(f340, plain, ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f1460, plain, (spl144_59 | spl144_43 | spl144_27 | spl144_11), inference(avatar_split_clause, [], [f341, f1213, f1281, f1349, f1417])).
fof(f341, plain, ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f1459, plain, (spl144_48 | spl144_44 | spl144_40 | spl144_36), inference(avatar_split_clause, [], [f342, f1319, f1336, f1353, f1370])).
fof(f342, plain, ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f1456, plain, (spl144_53 | spl144_37 | spl144_21 | spl144_5), inference(avatar_split_clause, [], [f345, f1188, f1256, f1324, f1392])).
fof(f345, plain, ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f1453, plain, (spl144_31 | spl144_27 | spl144_23 | spl144_19), inference(avatar_split_clause, [], [f348, f1247, f1264, f1281, f1298])).
fof(f348, plain, ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1451, plain, (spl144_32 | spl144_28 | spl144_24 | spl144_20), inference(avatar_split_clause, [], [f350, f1251, f1268, f1285, f1302])).
fof(f350, plain, ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1449, plain, (spl144_13 | spl144_9 | spl144_5 | spl144_1), inference(avatar_split_clause, [], [f352, f1171, f1188, f1205, f1222])).
fof(f352, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1448, plain, (spl144_49 | spl144_33 | spl144_17 | spl144_1), inference(avatar_split_clause, [], [f353, f1171, f1239, f1307, f1375])).
fof(f353, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1446, plain, (spl144_50 | spl144_34 | spl144_18 | spl144_2), inference(avatar_split_clause, [], [f355, f1175, f1243, f1311, f1379])).
fof(f355, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1441, plain, (spl144_61 | spl144_62 | spl144_63 | spl144_64), inference(avatar_split_clause, [], [f312, f1438, f1434, f1430, f1426])).
fof(f312, plain, ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG119+1.p', ax1)).
fof(f1424, plain, (spl144_57 | spl144_58 | spl144_59 | spl144_60), inference(avatar_split_clause, [], [f313, f1421, f1417, f1413, f1409])).
fof(f313, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1407, plain, (spl144_53 | spl144_54 | spl144_55 | spl144_56), inference(avatar_split_clause, [], [f314, f1404, f1400, f1396, f1392])).
fof(f314, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f1390, plain, (spl144_49 | spl144_50 | spl144_51 | spl144_52), inference(avatar_split_clause, [], [f315, f1387, f1383, f1379, f1375])).
fof(f315, plain, ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f1])).
fof(f1373, plain, (spl144_45 | spl144_46 | spl144_47 | spl144_48), inference(avatar_split_clause, [], [f316, f1370, f1366, f1362, f1358])).
fof(f316, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f1356, plain, (spl144_41 | spl144_42 | spl144_43 | spl144_44), inference(avatar_split_clause, [], [f317, f1353, f1349, f1345, f1341])).
fof(f317, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f1339, plain, (spl144_37 | spl144_38 | spl144_39 | spl144_40), inference(avatar_split_clause, [], [f318, f1336, f1332, f1328, f1324])).
fof(f318, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f1322, plain, (spl144_33 | spl144_34 | spl144_35 | spl144_36), inference(avatar_split_clause, [], [f319, f1319, f1315, f1311, f1307])).
fof(f319, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f1305, plain, (spl144_29 | spl144_30 | spl144_31 | spl144_32), inference(avatar_split_clause, [], [f320, f1302, f1298, f1294, f1290])).
fof(f320, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f1271, plain, (spl144_21 | spl144_22 | spl144_23 | spl144_24), inference(avatar_split_clause, [], [f322, f1268, f1264, f1260, f1256])).
fof(f322, plain, ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))), inference(cnf_transformation, [], [f1])).
fof(f1254, plain, (spl144_17 | spl144_18 | spl144_19 | spl144_20), inference(avatar_split_clause, [], [f323, f1251, f1247, f1243, f1239])).
fof(f323, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f1220, plain, (spl144_9 | spl144_10 | spl144_11 | spl144_12), inference(avatar_split_clause, [], [f325, f1217, f1213, f1209, f1205])).
fof(f325, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))), inference(cnf_transformation, [], [f1])).