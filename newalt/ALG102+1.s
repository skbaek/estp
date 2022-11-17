fof(f5463, plain, $false, inference(avatar_sat_refutation, [], [f1244, f1261, f1278, f1295, f1329, f1346, f1363, f1397, f1431, f1448, f1466, f1467, f1468, f1469, f1478, f1480, f1481, f1490, f1491, f1494, f1531, f1548, f1582, f1633, f1650, f1701, f1718, f1774, f1778, f1781, f1782, f1783, f1784, f1785, f1792, f1793, f1794, f1796, f1797, f1798, f1806, f1813, f1818, f1823, f1835, f1877, f1890, f1898, f1906, f1912, f1920, f1922, f1930, f1936, f1938, f1946, f1954, f1962, f1970, f1977, f1985, f1993, f2001, f2008, f2016, f2024, f2032, f2034, f2040, f2049, f2050, f2057, f2058, f2065, f2066, f2074, f2082, f2090, f2098, f2103, f2106, f2112, f2113, f2114, f2120, f2128, f2136, f2144, f2152, f2160, f2162, f2169, f2177, f2185, f2193, f2200, f2209, f2210, f2217, f2218, f2223, f2231, f2234, f2240, f2242, f2248, f2256, f2264, f2272, f2280, f2290, f2296, f2306, f2314, f2322, f2328, f2336, f2337, f2343, f2344, f2351, f2362, f2370, f2376, f2386, f2387, f2388, f2399, f2409, f2413, f2418, f2430, f2435, f2447, f2452, f2457, f2462, f2467, f2473, f2474, f2479, f2489, f2494, f2500, f2508, f2516, f2524, f2532, f2534, f2539, f2542, f2548, f2550, f2558, f2566, f2574, f2582, f2589, f2597, f2605, f2613, f2622, f2628, f2636, f2644, f2646, f2652, f2662, f2667, f2676, f2686, f2694, f2702, f2710, f2715, f2717, f2723, f2725, f2732, f2741, f2750, f2756, f2764, f2772, f2774, f2781, f2789, f2797, f2805, f2812, f2820, f2829, f2830, f2835, f2843, f2846, f2854, f2860, f2870, f2878, f2886, f2894, f2902, f2908, f2918, f2926, f2934, f2940, f2948, f2949, f2957, f2963, f2971, f2979, f2988, f2997, f2998, f2999, f3006, f3011, f3021, f3024, f3025, f3026, f3028, f3029, f3031, f3203, f3217, f3230, f3635, f3792, f3805, f3827, f3851, f3888, f3894, f3901, f3931, f3945, f3955, f3957, f3958, f3983, f4002, f4003, f4004, f4005, f4006, f4007, f4032, f4037, f4038, f4039, f4047, f4060, f4069, f4074, f4078, f4085, f4089, f4091, f4111, f4113, f4115, f4124, f4129, f4137, f4146, f4155, f4159, f4162, f4172, f4176, f4185, f4197, f4202, f4218, f4248, f4250, f4253, f4263, f4289, f4295, f4298, f4341, f4353, f4366, f4370, f4408, f4411, f4416, f4434, f4436, f4438, f4440, f4448, f4451, f4455, f4490, f4495, f4498, f4529, f4548, f4559, f4560, f4562, f4566, f4580, f4583, f4593, f4616, f4631, f4660, f4671, f4678, f4693, f4709, f4712, f4719, f4732, f4738, f4744, f4752, f4781, f4789, f4795, f4812, f4813, f4822, f4859, f4876, f4894, f4900, f4912, f4915, f4925, f4930, f4939, f4944, f4949, f4968, f4973, f4986, f5000, f5021, f5031, f5065, f5066, f5089, f5133, f5135, f5162, f5169, f5175, f5185, f5203, f5214, f5215, f5232, f5237, f5282, f5287, f5292, f5302, f5348, f5377, f5402, f5409, f5416, f5424, f5435, f5448, f5457])).
fof(f5457, plain, (~ spl144_18 | ~ spl144_22), inference(avatar_split_clause, [], [f5453, f1284, f1267])).
fof(f1267, plain, (spl144_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_18])])).
fof(f1284, plain, (spl144_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_22])])).
fof(f5453, plain, (~ (e11 = op1(e12, e13)) | ~ spl144_22), inference(backward_demodulation, [], [f449, f1286])).
fof(f1286, plain, ((e11 = op1(e12, e12)) | ~ spl144_22), inference(avatar_component_clause, [], [f1284])).
fof(f449, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax5)).
fof(f5448, plain, (~ spl144_18 | ~ spl144_34), inference(avatar_split_clause, [], [f5443, f1335, f1267])).
fof(f1335, plain, (spl144_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_34])])).
fof(f5443, plain, (~ (e11 = op1(e12, e13)) | ~ spl144_34), inference(backward_demodulation, [], [f429, f1337])).
fof(f1337, plain, ((e11 = op1(e11, e13)) | ~ spl144_34), inference(avatar_component_clause, [], [f1335])).
fof(f429, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5435, plain, (~ spl144_36 | ~ spl144_52), inference(avatar_split_clause, [], [f5431, f1411, f1343])).
fof(f1343, plain, (spl144_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_36])])).
fof(f1411, plain, (spl144_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_52])])).
fof(f5431, plain, (~ (e13 = op1(e11, e13)) | ~ spl144_52), inference(backward_demodulation, [], [f426, f1413])).
fof(f1413, plain, ((e13 = op1(e10, e13)) | ~ spl144_52), inference(avatar_component_clause, [], [f1411])).
fof(f426, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5424, plain, (~ spl144_23 | ~ spl144_55), inference(avatar_split_clause, [], [f5419, f1424, f1288])).
fof(f1288, plain, (spl144_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_23])])).
fof(f1424, plain, (spl144_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_55])])).
fof(f5419, plain, (~ (e12 = op1(e12, e12)) | ~ spl144_55), inference(backward_demodulation, [], [f421, f1426])).
fof(f1426, plain, ((e12 = op1(e10, e12)) | ~ spl144_55), inference(avatar_component_clause, [], [f1424])).
fof(f421, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f5416, plain, (~ spl144_54 | ~ spl144_62), inference(avatar_split_clause, [], [f5410, f1454, f1420])).
fof(f1420, plain, (spl144_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_54])])).
fof(f1454, plain, (spl144_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl144_62])])).
fof(f5410, plain, (~ (e11 = op1(e10, e12)) | ~ spl144_62), inference(backward_demodulation, [], [f433, f1456])).
fof(f1456, plain, ((op1(e10, e10) = e11) | ~ spl144_62), inference(avatar_component_clause, [], [f1454])).
fof(f433, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f5409, plain, (~ spl144_65 | ~ spl144_66), inference(avatar_contradiction_clause, [], [f5408])).
fof(f5408, plain, ($false | (~ spl144_65 | ~ spl144_66)), inference(subsumption_resolution, [], [f5407, f510])).
fof(f510, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax8)).
fof(f5407, plain, ((e20 = e21) | (~ spl144_65 | ~ spl144_66)), inference(forward_demodulation, [], [f1505, f1501])).
fof(f1501, plain, ((e20 = op2(e23, e23)) | ~ spl144_65), inference(avatar_component_clause, [], [f1499])).
fof(f1499, plain, (spl144_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_65])])).
fof(f1505, plain, ((e21 = op2(e23, e23)) | ~ spl144_66), inference(avatar_component_clause, [], [f1503])).
fof(f1503, plain, (spl144_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_66])])).
fof(f5402, plain, (~ spl144_79 | ~ spl144_80), inference(avatar_contradiction_clause, [], [f5401])).
fof(f5401, plain, ($false | (~ spl144_79 | ~ spl144_80)), inference(subsumption_resolution, [], [f5400, f515])).
fof(f515, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f5400, plain, ((e22 = e23) | (~ spl144_79 | ~ spl144_80)), inference(forward_demodulation, [], [f1564, f1560])).
fof(f1560, plain, ((e22 = op2(e23, e20)) | ~ spl144_79), inference(avatar_component_clause, [], [f1558])).
fof(f1558, plain, (spl144_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_79])])).
fof(f1564, plain, ((e23 = op2(e23, e20)) | ~ spl144_80), inference(avatar_component_clause, [], [f1562])).
fof(f1562, plain, (spl144_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_80])])).
fof(f5377, plain, (~ spl144_118 | ~ spl144_120), inference(avatar_contradiction_clause, [], [f5376])).
fof(f5376, plain, ($false | (~ spl144_118 | ~ spl144_120)), inference(subsumption_resolution, [], [f5375, f514])).
fof(f514, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f5375, plain, ((e21 = e23) | (~ spl144_118 | ~ spl144_120)), inference(backward_demodulation, [], [f1734, f1726])).
fof(f1726, plain, ((e21 = op2(e20, e22)) | ~ spl144_118), inference(avatar_component_clause, [], [f1724])).
fof(f1724, plain, (spl144_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_118])])).
fof(f1734, plain, ((e23 = op2(e20, e22)) | ~ spl144_120), inference(avatar_component_clause, [], [f1732])).
fof(f1732, plain, (spl144_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_120])])).
fof(f5348, plain, (spl144_82 | ~ spl144_92 | ~ spl144_214), inference(avatar_split_clause, [], [f5347, f2432, f1613, f1571])).
fof(f1571, plain, (spl144_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_82])])).
fof(f1613, plain, (spl144_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_92])])).
fof(f2432, plain, (spl144_214 <=> (e21 = op2(e22, op2(e22, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl144_214])])).
fof(f5347, plain, ((e21 = op2(e22, e23)) | (~ spl144_92 | ~ spl144_214)), inference(forward_demodulation, [], [f2434, f1615])).
fof(f1615, plain, ((e23 = op2(e22, e21)) | ~ spl144_92), inference(avatar_component_clause, [], [f1613])).
fof(f2434, plain, ((e21 = op2(e22, op2(e22, e21))) | ~ spl144_214), inference(avatar_component_clause, [], [f2432])).
fof(f5302, plain, (~ spl144_118 | ~ spl144_329), inference(avatar_split_clause, [], [f5301, f3261, f1724])).
fof(f3261, plain, (spl144_329 <=> (e21 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_329])])).
fof(f5301, plain, (~ (e21 = op2(e20, e22)) | ~ spl144_329), inference(forward_demodulation, [], [f3036, f3262])).
fof(f3262, plain, ((e21 = h1(e12)) | ~ spl144_329), inference(avatar_component_clause, [], [f3261])).
fof(f3036, plain, ~ (op2(e20, e22) = h1(e12)), inference(backward_demodulation, [], [f481, f1116])).
fof(f1116, plain, (op2(e20, e20) = h1(e12)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(op2(e20, e20), e20) = h1(e13)) & (op2(e20, e20) = h1(e12)) & (h1(e10) = op2(op2(op2(e20, e20), e20), op2(op2(e20, e20), e20))) & (e20 = h1(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax14)).
fof(f481, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax6)).
fof(f5292, plain, (~ spl144_91 | ~ spl144_318), inference(avatar_split_clause, [], [f4296, f3200, f1609])).
fof(f1609, plain, (spl144_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_91])])).
fof(f3200, plain, (spl144_318 <=> (e22 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_318])])).
fof(f4296, plain, (~ (e22 = op2(e22, e21)) | ~ spl144_318), inference(forward_demodulation, [], [f3043, f3201])).
fof(f3201, plain, ((e22 = h2(e12)) | ~ spl144_318), inference(avatar_component_clause, [], [f3200])).
fof(f3043, plain, ~ (op2(e22, e21) = h2(e12)), inference(backward_demodulation, [], [f465, f1120])).
fof(f1120, plain, (op2(e21, e21) = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(op2(e21, e21), e21) = h2(e13)) & (op2(e21, e21) = h2(e12)) & (op2(op2(op2(e21, e21), e21), op2(op2(e21, e21), e21)) = h2(e10)) & (e21 = h2(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax15)).
fof(f465, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f5287, plain, (~ spl144_70 | ~ spl144_74), inference(avatar_split_clause, [], [f5286, f1537, f1520])).
fof(f1520, plain, (spl144_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_70])])).
fof(f1537, plain, (spl144_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_74])])).
fof(f5286, plain, (~ (e21 = op2(e23, e22)) | ~ spl144_74), inference(forward_demodulation, [], [f501, f1539])).
fof(f1539, plain, ((e21 = op2(e23, e21)) | ~ spl144_74), inference(avatar_component_clause, [], [f1537])).
fof(f501, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f5282, plain, (~ spl144_62 | ~ spl144_46), inference(avatar_split_clause, [], [f5281, f1386, f1454])).
fof(f1386, plain, (spl144_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_46])])).
fof(f5281, plain, (~ (op1(e10, e10) = e11) | ~ spl144_46), inference(forward_demodulation, [], [f408, f1388])).
fof(f1388, plain, ((e11 = op1(e11, e10)) | ~ spl144_46), inference(avatar_component_clause, [], [f1386])).
fof(f408, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f5237, plain, (~ spl144_1 | ~ spl144_8 | ~ spl144_208), inference(avatar_contradiction_clause, [], [f5236])).
fof(f5236, plain, ($false | (~ spl144_1 | ~ spl144_8 | ~ spl144_208)), inference(subsumption_resolution, [], [f5235, f505])).
fof(f505, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax7)).
fof(f5235, plain, ((e10 = e12) | (~ spl144_1 | ~ spl144_8 | ~ spl144_208)), inference(forward_demodulation, [], [f5234, f1197])).
fof(f1197, plain, ((e10 = op1(e13, e13)) | ~ spl144_1), inference(avatar_component_clause, [], [f1195])).
fof(f1195, plain, (spl144_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_1])])).
fof(f5234, plain, ((e12 = op1(e13, e13)) | (~ spl144_8 | ~ spl144_208)), inference(forward_demodulation, [], [f2398, f1226])).
fof(f1226, plain, ((e13 = op1(e13, e12)) | ~ spl144_8), inference(avatar_component_clause, [], [f1224])).
fof(f1224, plain, (spl144_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_8])])).
fof(f2398, plain, ((e12 = op1(e13, op1(e13, e12))) | ~ spl144_208), inference(avatar_component_clause, [], [f2396])).
fof(f2396, plain, (spl144_208 <=> (e12 = op1(e13, op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_208])])).
fof(f5232, plain, (spl144_5 | ~ spl144_15 | ~ spl144_210), inference(avatar_contradiction_clause, [], [f5231])).
fof(f5231, plain, ($false | (spl144_5 | ~ spl144_15 | ~ spl144_210)), inference(subsumption_resolution, [], [f5230, f1213])).
fof(f1213, plain, (~ (e10 = op1(e13, e12)) | spl144_5), inference(avatar_component_clause, [], [f1212])).
fof(f1212, plain, (spl144_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_5])])).
fof(f5230, plain, ((e10 = op1(e13, e12)) | (~ spl144_15 | ~ spl144_210)), inference(forward_demodulation, [], [f2408, f1256])).
fof(f1256, plain, ((e12 = op1(e13, e10)) | ~ spl144_15), inference(avatar_component_clause, [], [f1254])).
fof(f1254, plain, (spl144_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_15])])).
fof(f2408, plain, ((e10 = op1(e13, op1(e13, e10))) | ~ spl144_210), inference(avatar_component_clause, [], [f2406])).
fof(f2406, plain, (spl144_210 <=> (e10 = op1(e13, op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_210])])).
fof(f5215, plain, (spl144_18 | ~ spl144_28 | ~ spl144_132), inference(avatar_split_clause, [], [f5157, f1820, f1309, f1267])).
fof(f1309, plain, (spl144_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_28])])).
fof(f1820, plain, (spl144_132 <=> (e11 = op1(e12, op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_132])])).
fof(f5157, plain, ((e11 = op1(e12, e13)) | (~ spl144_28 | ~ spl144_132)), inference(forward_demodulation, [], [f1822, f1311])).
fof(f1311, plain, ((e13 = op1(e12, e11)) | ~ spl144_28), inference(avatar_component_clause, [], [f1309])).
fof(f1822, plain, ((e11 = op1(e12, op1(e12, e11))) | ~ spl144_132), inference(avatar_component_clause, [], [f1820])).
fof(f5214, plain, (~ spl144_1 | ~ spl144_3), inference(avatar_contradiction_clause, [], [f5213])).
fof(f5213, plain, ($false | (~ spl144_1 | ~ spl144_3)), inference(subsumption_resolution, [], [f5212, f505])).
fof(f5212, plain, ((e10 = e12) | (~ spl144_1 | ~ spl144_3)), inference(forward_demodulation, [], [f1205, f1197])).
fof(f1205, plain, ((e12 = op1(e13, e13)) | ~ spl144_3), inference(avatar_component_clause, [], [f1203])).
fof(f1203, plain, (spl144_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_3])])).
fof(f5203, plain, (~ spl144_22 | spl144_27 | ~ spl144_131), inference(avatar_contradiction_clause, [], [f5202])).
fof(f5202, plain, ($false | (~ spl144_22 | spl144_27 | ~ spl144_131)), inference(subsumption_resolution, [], [f5200, f1306])).
fof(f1306, plain, (~ (e12 = op1(e12, e11)) | spl144_27), inference(avatar_component_clause, [], [f1305])).
fof(f1305, plain, (spl144_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_27])])).
fof(f5200, plain, ((e12 = op1(e12, e11)) | (~ spl144_22 | ~ spl144_131)), inference(backward_demodulation, [], [f1817, f1286])).
fof(f1817, plain, ((e12 = op1(e12, op1(e12, e12))) | ~ spl144_131), inference(avatar_component_clause, [], [f1815])).
fof(f1815, plain, (spl144_131 <=> (e12 = op1(e12, op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_131])])).
fof(f5185, plain, (~ spl144_21 | ~ spl144_37), inference(avatar_split_clause, [], [f5177, f1348, f1280])).
fof(f1280, plain, (spl144_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_21])])).
fof(f1348, plain, (spl144_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_37])])).
fof(f5177, plain, (~ (e10 = op1(e12, e12)) | ~ spl144_37), inference(backward_demodulation, [], [f423, f1350])).
fof(f1350, plain, ((e10 = op1(e11, e12)) | ~ spl144_37), inference(avatar_component_clause, [], [f1348])).
fof(f423, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f5175, plain, (~ spl144_38 | ~ spl144_46), inference(avatar_split_clause, [], [f5172, f1386, f1352])).
fof(f1352, plain, (spl144_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_38])])).
fof(f5172, plain, (~ (e11 = op1(e11, e12)) | ~ spl144_46), inference(backward_demodulation, [], [f439, f1388])).
fof(f439, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f5169, plain, (~ spl144_7 | ~ spl144_55), inference(avatar_split_clause, [], [f5165, f1424, f1220])).
fof(f1220, plain, (spl144_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_7])])).
fof(f5165, plain, (~ (e12 = op1(e13, e12)) | ~ spl144_55), inference(backward_demodulation, [], [f422, f1426])).
fof(f422, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f5162, plain, (~ spl144_19 | spl144_24 | ~ spl144_130), inference(avatar_contradiction_clause, [], [f5161])).
fof(f5161, plain, ($false | (~ spl144_19 | spl144_24 | ~ spl144_130)), inference(subsumption_resolution, [], [f5160, f1293])).
fof(f1293, plain, (~ (e13 = op1(e12, e12)) | spl144_24), inference(avatar_component_clause, [], [f1292])).
fof(f1292, plain, (spl144_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_24])])).
fof(f5160, plain, ((e13 = op1(e12, e12)) | (~ spl144_19 | ~ spl144_130)), inference(forward_demodulation, [], [f1812, f1273])).
fof(f1273, plain, ((e12 = op1(e12, e13)) | ~ spl144_19), inference(avatar_component_clause, [], [f1271])).
fof(f1271, plain, (spl144_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_19])])).
fof(f1812, plain, ((e13 = op1(e12, op1(e12, e13))) | ~ spl144_130), inference(avatar_component_clause, [], [f1810])).
fof(f1810, plain, (spl144_130 <=> (e13 = op1(e12, op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_130])])).
fof(f5135, plain, (~ spl144_48 | ~ spl144_36), inference(avatar_split_clause, [], [f5134, f1343, f1394])).
fof(f1394, plain, (spl144_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_48])])).
fof(f5134, plain, (~ (e13 = op1(e11, e10)) | ~ spl144_36), inference(forward_demodulation, [], [f440, f1345])).
fof(f1345, plain, ((e13 = op1(e11, e13)) | ~ spl144_36), inference(avatar_component_clause, [], [f1343])).
fof(f440, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5133, plain, (~ spl144_40 | ~ spl144_36), inference(avatar_split_clause, [], [f5132, f1343, f1360])).
fof(f1360, plain, (spl144_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_40])])).
fof(f5132, plain, (~ (e13 = op1(e11, e12)) | ~ spl144_36), inference(forward_demodulation, [], [f443, f1345])).
fof(f443, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5089, plain, (~ spl144_29 | ~ spl144_45), inference(avatar_split_clause, [], [f5085, f1382, f1314])).
fof(f1314, plain, (spl144_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_29])])).
fof(f1382, plain, (spl144_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_45])])).
fof(f5085, plain, (~ (e10 = op1(e12, e10)) | ~ spl144_45), inference(backward_demodulation, [], [f411, f1384])).
fof(f1384, plain, ((e10 = op1(e11, e10)) | ~ spl144_45), inference(avatar_component_clause, [], [f1382])).
fof(f411, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f5066, plain, (~ spl144_52 | ~ spl144_56), inference(avatar_split_clause, [], [f5062, f1428, f1411])).
fof(f1428, plain, (spl144_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_56])])).
fof(f5062, plain, (~ (e13 = op1(e10, e13)) | ~ spl144_56), inference(backward_demodulation, [], [f437, f1430])).
fof(f1430, plain, ((e13 = op1(e10, e12)) | ~ spl144_56), inference(avatar_component_clause, [], [f1428])).
fof(f437, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f5065, plain, (~ spl144_8 | ~ spl144_56), inference(avatar_split_clause, [], [f5061, f1428, f1224])).
fof(f5061, plain, (~ (e13 = op1(e13, e12)) | ~ spl144_56), inference(backward_demodulation, [], [f422, f1430])).
fof(f5031, plain, (spl144_62 | ~ spl144_57 | ~ spl144_142), inference(avatar_split_clause, [], [f4534, f1874, f1433, f1454])).
fof(f1433, plain, (spl144_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_57])])).
fof(f1874, plain, (spl144_142 <=> (e11 = op1(e10, op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_142])])).
fof(f4534, plain, ((op1(e10, e10) = e11) | (~ spl144_57 | ~ spl144_142)), inference(backward_demodulation, [], [f1876, f1435])).
fof(f1435, plain, ((e10 = op1(e10, e11)) | ~ spl144_57), inference(avatar_component_clause, [], [f1433])).
fof(f1876, plain, ((e11 = op1(e10, op1(e10, e11))) | ~ spl144_142), inference(avatar_component_clause, [], [f1874])).
fof(f5021, plain, (~ spl144_38 | ~ spl144_34), inference(avatar_split_clause, [], [f5020, f1335, f1352])).
fof(f5020, plain, (~ (e11 = op1(e11, e12)) | ~ spl144_34), inference(forward_demodulation, [], [f443, f1337])).
fof(f5000, plain, (~ spl144_10 | ~ spl144_74 | spl144_379), inference(avatar_contradiction_clause, [], [f4999])).
fof(f4999, plain, ($false | (~ spl144_10 | ~ spl144_74 | spl144_379)), inference(subsumption_resolution, [], [f4998, f1539])).
fof(f4998, plain, (~ (e21 = op2(e23, e21)) | (~ spl144_10 | spl144_379)), inference(forward_demodulation, [], [f4997, f1118])).
fof(f1118, plain, (e21 = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f4997, plain, (~ (op2(e23, e21) = h2(e11)) | (~ spl144_10 | spl144_379)), inference(forward_demodulation, [], [f3598, f1235])).
fof(f1235, plain, ((e11 = op1(e13, e11)) | ~ spl144_10), inference(avatar_component_clause, [], [f1233])).
fof(f1233, plain, (spl144_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_10])])).
fof(f3598, plain, (~ (op2(e23, e21) = h2(op1(e13, e11))) | spl144_379), inference(avatar_component_clause, [], [f3596])).
fof(f3596, plain, (spl144_379 <=> (op2(e23, e21) = h2(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_379])])).
fof(f4986, plain, (~ spl144_43 | ~ spl144_44), inference(avatar_contradiction_clause, [], [f4985])).
fof(f4985, plain, ($false | (~ spl144_43 | ~ spl144_44)), inference(subsumption_resolution, [], [f4984, f509])).
fof(f509, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f4984, plain, ((e12 = e13) | (~ spl144_43 | ~ spl144_44)), inference(forward_demodulation, [], [f1379, f1375])).
fof(f1375, plain, ((e12 = op1(e11, e11)) | ~ spl144_43), inference(avatar_component_clause, [], [f1373])).
fof(f1373, plain, (spl144_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_43])])).
fof(f1379, plain, ((e13 = op1(e11, e11)) | ~ spl144_44), inference(avatar_component_clause, [], [f1377])).
fof(f1377, plain, (spl144_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_44])])).
fof(f4973, plain, (~ spl144_62 | ~ spl144_64), inference(avatar_contradiction_clause, [], [f4972])).
fof(f4972, plain, ($false | (~ spl144_62 | ~ spl144_64)), inference(subsumption_resolution, [], [f4971, f508])).
fof(f508, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f4971, plain, ((e11 = e13) | (~ spl144_62 | ~ spl144_64)), inference(forward_demodulation, [], [f1464, f1456])).
fof(f1464, plain, ((op1(e10, e10) = e13) | ~ spl144_64), inference(avatar_component_clause, [], [f1462])).
fof(f1462, plain, (spl144_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl144_64])])).
fof(f4968, plain, (~ spl144_47 | ~ spl144_43), inference(avatar_split_clause, [], [f4967, f1373, f1390])).
fof(f1390, plain, (spl144_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_47])])).
fof(f4967, plain, (~ (e12 = op1(e11, e10)) | ~ spl144_43), inference(forward_demodulation, [], [f438, f1375])).
fof(f438, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4949, plain, (~ spl144_15 | ~ spl144_79 | ~ spl144_303 | ~ spl144_318 | spl144_380), inference(avatar_contradiction_clause, [], [f4948])).
fof(f4948, plain, ($false | (~ spl144_15 | ~ spl144_79 | ~ spl144_303 | ~ spl144_318 | spl144_380)), inference(subsumption_resolution, [], [f4947, f1560])).
fof(f4947, plain, (~ (e22 = op2(e23, e20)) | (~ spl144_15 | ~ spl144_303 | ~ spl144_318 | spl144_380)), inference(forward_demodulation, [], [f4946, f3201])).
fof(f4946, plain, (~ (op2(e23, e20) = h2(e12)) | (~ spl144_15 | ~ spl144_303 | spl144_380)), inference(forward_demodulation, [], [f4945, f1256])).
fof(f4945, plain, (~ (op2(e23, e20) = h2(op1(e13, e10))) | (~ spl144_303 | spl144_380)), inference(forward_demodulation, [], [f3602, f3124])).
fof(f3124, plain, ((e20 = h2(e10)) | ~ spl144_303), inference(avatar_component_clause, [], [f3123])).
fof(f3123, plain, (spl144_303 <=> (e20 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_303])])).
fof(f3602, plain, (~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | spl144_380), inference(avatar_component_clause, [], [f3600])).
fof(f3600, plain, (spl144_380 <=> (h2(op1(e13, e10)) = op2(e23, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_380])])).
fof(f4944, plain, (~ spl144_8 | ~ spl144_72 | ~ spl144_318 | spl144_378), inference(avatar_contradiction_clause, [], [f4943])).
fof(f4943, plain, ($false | (~ spl144_8 | ~ spl144_72 | ~ spl144_318 | spl144_378)), inference(subsumption_resolution, [], [f4942, f1530])).
fof(f1530, plain, ((e23 = op2(e23, e22)) | ~ spl144_72), inference(avatar_component_clause, [], [f1528])).
fof(f1528, plain, (spl144_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_72])])).
fof(f4942, plain, (~ (e23 = op2(e23, e22)) | (~ spl144_8 | ~ spl144_318 | spl144_378)), inference(forward_demodulation, [], [f4941, f3041])).
fof(f3041, plain, (e23 = h2(e13)), inference(forward_demodulation, [], [f1121, f1113])).
fof(f1113, plain, (e23 = op2(op2(e21, e21), e21)), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(op2(e21, e21), e21)) & (e22 = op2(e21, e21)) & (e20 = op2(op2(op2(e21, e21), e21), op2(op2(e21, e21), e21)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax13)).
fof(f1121, plain, (op2(op2(e21, e21), e21) = h2(e13)), inference(cnf_transformation, [], [f15])).
fof(f4941, plain, (~ (op2(e23, e22) = h2(e13)) | (~ spl144_8 | ~ spl144_318 | spl144_378)), inference(forward_demodulation, [], [f4940, f1226])).
fof(f4940, plain, (~ (op2(e23, e22) = h2(op1(e13, e12))) | (~ spl144_318 | spl144_378)), inference(forward_demodulation, [], [f3594, f3201])).
fof(f3594, plain, (~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | spl144_378), inference(avatar_component_clause, [], [f3592])).
fof(f3592, plain, (spl144_378 <=> (h2(op1(e13, e12)) = op2(e23, h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_378])])).
fof(f4939, plain, (~ spl144_1 | spl144_377), inference(avatar_contradiction_clause, [], [f4938])).
fof(f4938, plain, ($false | (~ spl144_1 | spl144_377)), inference(trivial_inequality_removal, [], [f4937])).
fof(f4937, plain, (~ (h2(e10) = h2(e10)) | (~ spl144_1 | spl144_377)), inference(forward_demodulation, [], [f3590, f1197])).
fof(f3590, plain, (~ (h2(e10) = h2(op1(e13, e13))) | spl144_377), inference(avatar_component_clause, [], [f3588])).
fof(f3588, plain, (spl144_377 <=> (h2(e10) = h2(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_377])])).
fof(f4930, plain, (~ spl144_19 | ~ spl144_83 | ~ spl144_318 | spl144_381), inference(avatar_contradiction_clause, [], [f4929])).
fof(f4929, plain, ($false | (~ spl144_19 | ~ spl144_83 | ~ spl144_318 | spl144_381)), inference(subsumption_resolution, [], [f4928, f1577])).
fof(f1577, plain, ((e22 = op2(e22, e23)) | ~ spl144_83), inference(avatar_component_clause, [], [f1575])).
fof(f1575, plain, (spl144_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_83])])).
fof(f4928, plain, (~ (e22 = op2(e22, e23)) | (~ spl144_19 | ~ spl144_318 | spl144_381)), inference(forward_demodulation, [], [f4927, f3201])).
fof(f4927, plain, (~ (op2(e22, e23) = h2(e12)) | (~ spl144_19 | ~ spl144_318 | spl144_381)), inference(forward_demodulation, [], [f4926, f1273])).
fof(f4926, plain, (~ (op2(e22, e23) = h2(op1(e12, e13))) | (~ spl144_318 | spl144_381)), inference(forward_demodulation, [], [f3606, f3201])).
fof(f3606, plain, (~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | spl144_381), inference(avatar_component_clause, [], [f3604])).
fof(f3604, plain, (spl144_381 <=> (h2(op1(e12, e13)) = op2(h2(e12), e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_381])])).
fof(f4925, plain, (~ spl144_22 | ~ spl144_86 | ~ spl144_318 | spl144_376), inference(avatar_contradiction_clause, [], [f4924])).
fof(f4924, plain, ($false | (~ spl144_22 | ~ spl144_86 | ~ spl144_318 | spl144_376)), inference(subsumption_resolution, [], [f4923, f1590])).
fof(f1590, plain, ((e21 = op2(e22, e22)) | ~ spl144_86), inference(avatar_component_clause, [], [f1588])).
fof(f1588, plain, (spl144_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_86])])).
fof(f4923, plain, (~ (e21 = op2(e22, e22)) | (~ spl144_22 | ~ spl144_318 | spl144_376)), inference(forward_demodulation, [], [f4922, f1118])).
fof(f4922, plain, (~ (op2(e22, e22) = h2(e11)) | (~ spl144_22 | ~ spl144_318 | spl144_376)), inference(forward_demodulation, [], [f4921, f1286])).
fof(f4921, plain, (~ (op2(e22, e22) = h2(op1(e12, e12))) | (~ spl144_318 | spl144_376)), inference(forward_demodulation, [], [f3586, f3201])).
fof(f3586, plain, (~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | spl144_376), inference(avatar_component_clause, [], [f3584])).
fof(f3584, plain, (spl144_376 <=> (h2(op1(e12, e12)) = op2(h2(e12), h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_376])])).
fof(f4915, plain, (~ spl144_28 | spl144_382), inference(avatar_contradiction_clause, [], [f4914])).
fof(f4914, plain, ($false | (~ spl144_28 | spl144_382)), inference(subsumption_resolution, [], [f4913, f3041])).
fof(f4913, plain, (~ (e23 = h2(e13)) | (~ spl144_28 | spl144_382)), inference(forward_demodulation, [], [f3610, f1311])).
fof(f3610, plain, (~ (e23 = h2(op1(e12, e11))) | spl144_382), inference(avatar_component_clause, [], [f3608])).
fof(f3608, plain, (spl144_382 <=> (e23 = h2(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_382])])).
fof(f4912, plain, (~ spl144_29 | ~ spl144_93 | ~ spl144_303 | ~ spl144_318 | spl144_375), inference(avatar_contradiction_clause, [], [f4911])).
fof(f4911, plain, ($false | (~ spl144_29 | ~ spl144_93 | ~ spl144_303 | ~ spl144_318 | spl144_375)), inference(subsumption_resolution, [], [f4910, f1620])).
fof(f1620, plain, ((e20 = op2(e22, e20)) | ~ spl144_93), inference(avatar_component_clause, [], [f1618])).
fof(f1618, plain, (spl144_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_93])])).
fof(f4910, plain, (~ (e20 = op2(e22, e20)) | (~ spl144_29 | ~ spl144_303 | ~ spl144_318 | spl144_375)), inference(forward_demodulation, [], [f4909, f3124])).
fof(f4909, plain, (~ (op2(e22, e20) = h2(e10)) | (~ spl144_29 | ~ spl144_303 | ~ spl144_318 | spl144_375)), inference(forward_demodulation, [], [f4908, f1316])).
fof(f1316, plain, ((e10 = op1(e12, e10)) | ~ spl144_29), inference(avatar_component_clause, [], [f1314])).
fof(f4908, plain, (~ (op2(e22, e20) = h2(op1(e12, e10))) | (~ spl144_303 | ~ spl144_318 | spl144_375)), inference(forward_demodulation, [], [f4907, f3201])).
fof(f4907, plain, (~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | (~ spl144_303 | spl144_375)), inference(forward_demodulation, [], [f3582, f3124])).
fof(f3582, plain, (~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | spl144_375), inference(avatar_component_clause, [], [f3580])).
fof(f3580, plain, (spl144_375 <=> (h2(op1(e12, e10)) = op2(h2(e12), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_375])])).
fof(f4900, plain, (~ spl144_34 | ~ spl144_98 | spl144_383), inference(avatar_contradiction_clause, [], [f4899])).
fof(f4899, plain, ($false | (~ spl144_34 | ~ spl144_98 | spl144_383)), inference(subsumption_resolution, [], [f4898, f1641])).
fof(f1641, plain, ((e21 = op2(e21, e23)) | ~ spl144_98), inference(avatar_component_clause, [], [f1639])).
fof(f1639, plain, (spl144_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_98])])).
fof(f4898, plain, (~ (e21 = op2(e21, e23)) | (~ spl144_34 | spl144_383)), inference(forward_demodulation, [], [f4897, f1118])).
fof(f4897, plain, (~ (op2(e21, e23) = h2(e11)) | (~ spl144_34 | spl144_383)), inference(forward_demodulation, [], [f3614, f1337])).
fof(f3614, plain, (~ (op2(e21, e23) = h2(op1(e11, e13))) | spl144_383), inference(avatar_component_clause, [], [f3612])).
fof(f3612, plain, (spl144_383 <=> (op2(e21, e23) = h2(op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_383])])).
fof(f4894, plain, (~ spl144_55 | ~ spl144_119 | ~ spl144_303 | ~ spl144_318 | spl144_374), inference(avatar_contradiction_clause, [], [f4893])).
fof(f4893, plain, ($false | (~ spl144_55 | ~ spl144_119 | ~ spl144_303 | ~ spl144_318 | spl144_374)), inference(subsumption_resolution, [], [f4892, f1730])).
fof(f1730, plain, ((e22 = op2(e20, e22)) | ~ spl144_119), inference(avatar_component_clause, [], [f1728])).
fof(f1728, plain, (spl144_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_119])])).
fof(f4892, plain, (~ (e22 = op2(e20, e22)) | (~ spl144_55 | ~ spl144_303 | ~ spl144_318 | spl144_374)), inference(forward_demodulation, [], [f4891, f3201])).
fof(f4891, plain, (~ (op2(e20, e22) = h2(e12)) | (~ spl144_55 | ~ spl144_303 | ~ spl144_318 | spl144_374)), inference(forward_demodulation, [], [f4890, f1426])).
fof(f4890, plain, (~ (op2(e20, e22) = h2(op1(e10, e12))) | (~ spl144_303 | ~ spl144_318 | spl144_374)), inference(forward_demodulation, [], [f4889, f3124])).
fof(f4889, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | (~ spl144_318 | spl144_374)), inference(forward_demodulation, [], [f3578, f3201])).
fof(f3578, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | spl144_374), inference(avatar_component_clause, [], [f3576])).
fof(f3576, plain, (spl144_374 <=> (h2(op1(e10, e12)) = op2(h2(e10), h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_374])])).
fof(f4876, plain, (~ spl144_37 | ~ spl144_101 | ~ spl144_303 | ~ spl144_318 | spl144_384), inference(avatar_contradiction_clause, [], [f4875])).
fof(f4875, plain, ($false | (~ spl144_37 | ~ spl144_101 | ~ spl144_303 | ~ spl144_318 | spl144_384)), inference(subsumption_resolution, [], [f4874, f1654])).
fof(f1654, plain, ((e20 = op2(e21, e22)) | ~ spl144_101), inference(avatar_component_clause, [], [f1652])).
fof(f1652, plain, (spl144_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_101])])).
fof(f4874, plain, (~ (e20 = op2(e21, e22)) | (~ spl144_37 | ~ spl144_303 | ~ spl144_318 | spl144_384)), inference(forward_demodulation, [], [f4873, f3124])).
fof(f4873, plain, (~ (op2(e21, e22) = h2(e10)) | (~ spl144_37 | ~ spl144_318 | spl144_384)), inference(forward_demodulation, [], [f4872, f1350])).
fof(f4872, plain, (~ (op2(e21, e22) = h2(op1(e11, e12))) | (~ spl144_318 | spl144_384)), inference(forward_demodulation, [], [f3618, f3201])).
fof(f3618, plain, (~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | spl144_384), inference(avatar_component_clause, [], [f3616])).
fof(f3616, plain, (spl144_384 <=> (h2(op1(e11, e12)) = op2(e21, h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_384])])).
fof(f4859, plain, (~ spl144_48 | ~ spl144_112 | ~ spl144_303 | spl144_386), inference(avatar_contradiction_clause, [], [f4858])).
fof(f4858, plain, ($false | (~ spl144_48 | ~ spl144_112 | ~ spl144_303 | spl144_386)), inference(subsumption_resolution, [], [f4857, f1700])).
fof(f1700, plain, ((e23 = op2(e21, e20)) | ~ spl144_112), inference(avatar_component_clause, [], [f1698])).
fof(f1698, plain, (spl144_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_112])])).
fof(f4857, plain, (~ (e23 = op2(e21, e20)) | (~ spl144_48 | ~ spl144_303 | spl144_386)), inference(forward_demodulation, [], [f4856, f3041])).
fof(f4856, plain, (~ (op2(e21, e20) = h2(e13)) | (~ spl144_48 | ~ spl144_303 | spl144_386)), inference(forward_demodulation, [], [f4855, f1396])).
fof(f1396, plain, ((e13 = op1(e11, e10)) | ~ spl144_48), inference(avatar_component_clause, [], [f1394])).
fof(f4855, plain, (~ (op2(e21, e20) = h2(op1(e11, e10))) | (~ spl144_303 | spl144_386)), inference(forward_demodulation, [], [f3626, f3124])).
fof(f3626, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | spl144_386), inference(avatar_component_clause, [], [f3624])).
fof(f3624, plain, (spl144_386 <=> (h2(op1(e11, e10)) = op2(e21, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_386])])).
fof(f4822, plain, (~ spl144_57 | ~ spl144_121 | ~ spl144_303 | spl144_388), inference(avatar_contradiction_clause, [], [f4821])).
fof(f4821, plain, ($false | (~ spl144_57 | ~ spl144_121 | ~ spl144_303 | spl144_388)), inference(subsumption_resolution, [], [f4820, f1739])).
fof(f1739, plain, ((e20 = op2(e20, e21)) | ~ spl144_121), inference(avatar_component_clause, [], [f1737])).
fof(f1737, plain, (spl144_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_121])])).
fof(f4820, plain, (~ (e20 = op2(e20, e21)) | (~ spl144_57 | ~ spl144_303 | spl144_388)), inference(forward_demodulation, [], [f4819, f3124])).
fof(f4819, plain, (~ (op2(e20, e21) = h2(e10)) | (~ spl144_57 | ~ spl144_303 | spl144_388)), inference(forward_demodulation, [], [f4818, f1435])).
fof(f4818, plain, (~ (op2(e20, e21) = h2(op1(e10, e11))) | (~ spl144_303 | spl144_388)), inference(forward_demodulation, [], [f3634, f3124])).
fof(f3634, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | spl144_388), inference(avatar_component_clause, [], [f3632])).
fof(f3632, plain, (spl144_388 <=> (h2(op1(e10, e11)) = op2(h2(e10), e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_388])])).
fof(f4813, plain, (spl144_311 | ~ spl144_86), inference(avatar_split_clause, [], [f4799, f1588, f3164])).
fof(f3164, plain, (spl144_311 <=> (e21 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_311])])).
fof(f4799, plain, ((e21 = h3(e12)) | ~ spl144_86), inference(backward_demodulation, [], [f1124, f1590])).
fof(f1124, plain, (op2(e22, e22) = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(op2(e22, e22), e22) = h3(e13)) & (op2(e22, e22) = h3(e12)) & (h3(e10) = op2(op2(op2(e22, e22), e22), op2(op2(e22, e22), e22))) & (e22 = h3(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax16)).
fof(f4812, plain, (~ spl144_94 | ~ spl144_329), inference(avatar_split_clause, [], [f4811, f3261, f1622])).
fof(f1622, plain, (spl144_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_94])])).
fof(f4811, plain, (~ (e21 = op2(e22, e20)) | ~ spl144_329), inference(forward_demodulation, [], [f3033, f3262])).
fof(f3033, plain, ~ (op2(e22, e20) = h1(e12)), inference(backward_demodulation, [], [f457, f1116])).
fof(f457, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f4795, plain, (~ spl144_98 | ~ spl144_107 | ~ spl144_217), inference(avatar_contradiction_clause, [], [f4794])).
fof(f4794, plain, ($false | (~ spl144_98 | ~ spl144_107 | ~ spl144_217)), inference(subsumption_resolution, [], [f4793, f515])).
fof(f4793, plain, ((e22 = e23) | (~ spl144_98 | ~ spl144_107 | ~ spl144_217)), inference(forward_demodulation, [], [f4791, f1679])).
fof(f1679, plain, ((e22 = op2(e21, e21)) | ~ spl144_107), inference(avatar_component_clause, [], [f1677])).
fof(f1677, plain, (spl144_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_107])])).
fof(f4791, plain, ((e23 = op2(e21, e21)) | (~ spl144_98 | ~ spl144_217)), inference(backward_demodulation, [], [f2451, f1641])).
fof(f2451, plain, ((e23 = op2(e21, op2(e21, e23))) | ~ spl144_217), inference(avatar_component_clause, [], [f2449])).
fof(f2449, plain, (spl144_217 <=> (e23 = op2(e21, op2(e21, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl144_217])])).
fof(f4789, plain, (~ spl144_101 | spl144_111 | ~ spl144_218), inference(avatar_contradiction_clause, [], [f4788])).
fof(f4788, plain, ($false | (~ spl144_101 | spl144_111 | ~ spl144_218)), inference(subsumption_resolution, [], [f4786, f1695])).
fof(f1695, plain, (~ (e22 = op2(e21, e20)) | spl144_111), inference(avatar_component_clause, [], [f1694])).
fof(f1694, plain, (spl144_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_111])])).
fof(f4786, plain, ((e22 = op2(e21, e20)) | (~ spl144_101 | ~ spl144_218)), inference(backward_demodulation, [], [f2456, f1654])).
fof(f2456, plain, ((e22 = op2(e21, op2(e21, e22))) | ~ spl144_218), inference(avatar_component_clause, [], [f2454])).
fof(f2454, plain, (spl144_218 <=> (e22 = op2(e21, op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_218])])).
fof(f4781, plain, (~ spl144_100 | ~ spl144_112), inference(avatar_split_clause, [], [f4780, f1698, f1647])).
fof(f1647, plain, (spl144_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_100])])).
fof(f4780, plain, (~ (e23 = op2(e21, e23)) | ~ spl144_112), inference(backward_demodulation, [], [f488, f1700])).
fof(f488, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4752, plain, (~ spl144_62 | ~ spl144_126 | ~ spl144_303 | spl144_373), inference(avatar_contradiction_clause, [], [f4751])).
fof(f4751, plain, ($false | (~ spl144_62 | ~ spl144_126 | ~ spl144_303 | spl144_373)), inference(subsumption_resolution, [], [f4750, f1760])).
fof(f1760, plain, ((op2(e20, e20) = e21) | ~ spl144_126), inference(avatar_component_clause, [], [f1758])).
fof(f1758, plain, (spl144_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl144_126])])).
fof(f4750, plain, (~ (op2(e20, e20) = e21) | (~ spl144_62 | ~ spl144_303 | spl144_373)), inference(forward_demodulation, [], [f4749, f1118])).
fof(f4749, plain, (~ (op2(e20, e20) = h2(e11)) | (~ spl144_62 | ~ spl144_303 | spl144_373)), inference(forward_demodulation, [], [f4748, f1456])).
fof(f4748, plain, (~ (op2(e20, e20) = h2(op1(e10, e10))) | (~ spl144_303 | spl144_373)), inference(forward_demodulation, [], [f3574, f3124])).
fof(f3574, plain, (~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10))) | spl144_373), inference(avatar_component_clause, [], [f3572])).
fof(f3572, plain, (spl144_373 <=> (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_373])])).
fof(f4744, plain, (spl144_329 | ~ spl144_126), inference(avatar_split_clause, [], [f4668, f1758, f3261])).
fof(f4668, plain, ((e21 = h1(e12)) | ~ spl144_126), inference(forward_demodulation, [], [f1116, f1760])).
fof(f4738, plain, (spl144_102 | ~ spl144_107 | ~ spl144_219), inference(avatar_split_clause, [], [f4471, f2459, f1677, f1656])).
fof(f1656, plain, (spl144_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_102])])).
fof(f2459, plain, (spl144_219 <=> (e21 = op2(e21, op2(e21, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl144_219])])).
fof(f4471, plain, ((e21 = op2(e21, e22)) | (~ spl144_107 | ~ spl144_219)), inference(forward_demodulation, [], [f2461, f1679])).
fof(f2461, plain, ((e21 = op2(e21, op2(e21, e21))) | ~ spl144_219), inference(avatar_component_clause, [], [f2459])).
fof(f4732, plain, (~ spl144_81 | ~ spl144_303), inference(avatar_split_clause, [], [f4567, f3123, f1567])).
fof(f1567, plain, (spl144_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_81])])).
fof(f4567, plain, (~ (e20 = op2(e22, e23)) | ~ spl144_303), inference(forward_demodulation, [], [f3053, f3124])).
fof(f3053, plain, ~ (op2(e22, e23) = h2(e10)), inference(backward_demodulation, [], [f479, f3050])).
fof(f3050, plain, (op2(e23, e23) = h2(e10)), inference(forward_demodulation, [], [f3049, f3048])).
fof(f3048, plain, (e23 = op2(h2(e12), e21)), inference(backward_demodulation, [], [f1113, f1120])).
fof(f3049, plain, (h2(e10) = op2(op2(h2(e12), e21), op2(h2(e12), e21))), inference(forward_demodulation, [], [f1119, f1120])).
fof(f1119, plain, (op2(op2(op2(e21, e21), e21), op2(op2(e21, e21), e21)) = h2(e10)), inference(cnf_transformation, [], [f15])).
fof(f479, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4719, plain, (~ spl144_93 | ~ spl144_109), inference(avatar_split_clause, [], [f4718, f1686, f1618])).
fof(f1686, plain, (spl144_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_109])])).
fof(f4718, plain, (~ (e20 = op2(e22, e20)) | ~ spl144_109), inference(backward_demodulation, [], [f459, f1688])).
fof(f1688, plain, ((e20 = op2(e21, e20)) | ~ spl144_109), inference(avatar_component_clause, [], [f1686])).
fof(f459, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f4712, plain, (~ spl144_121 | ~ spl144_124), inference(avatar_contradiction_clause, [], [f4711])).
fof(f4711, plain, ($false | (~ spl144_121 | ~ spl144_124)), inference(subsumption_resolution, [], [f4710, f512])).
fof(f512, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f4710, plain, ((e20 = e23) | (~ spl144_121 | ~ spl144_124)), inference(forward_demodulation, [], [f1751, f1739])).
fof(f1751, plain, ((e23 = op2(e20, e21)) | ~ spl144_124), inference(avatar_component_clause, [], [f1749])).
fof(f1749, plain, (spl144_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_124])])).
fof(f4709, plain, (~ spl144_126 | ~ spl144_127), inference(avatar_contradiction_clause, [], [f4708])).
fof(f4708, plain, ($false | (~ spl144_126 | ~ spl144_127)), inference(subsumption_resolution, [], [f4707, f513])).
fof(f513, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f4707, plain, ((e21 = e22) | (~ spl144_126 | ~ spl144_127)), inference(forward_demodulation, [], [f1764, f1760])).
fof(f1764, plain, ((op2(e20, e20) = e22) | ~ spl144_127), inference(avatar_component_clause, [], [f1762])).
fof(f1762, plain, (spl144_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl144_127])])).
fof(f4693, plain, (spl144_95 | ~ spl144_213 | ~ spl144_315), inference(avatar_contradiction_clause, [], [f4692])).
fof(f4692, plain, ($false | (spl144_95 | ~ spl144_213 | ~ spl144_315)), inference(subsumption_resolution, [], [f4688, f1627])).
fof(f1627, plain, (~ (e22 = op2(e22, e20)) | spl144_95), inference(avatar_component_clause, [], [f1626])).
fof(f1626, plain, (spl144_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_95])])).
fof(f4688, plain, ((e22 = op2(e22, e20)) | (~ spl144_213 | ~ spl144_315)), inference(backward_demodulation, [], [f4433, f3185])).
fof(f3185, plain, ((e20 = h3(e12)) | ~ spl144_315), inference(avatar_component_clause, [], [f3184])).
fof(f3184, plain, (spl144_315 <=> (e20 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_315])])).
fof(f4433, plain, ((e22 = op2(e22, h3(e12))) | ~ spl144_213), inference(backward_demodulation, [], [f2429, f1124])).
fof(f2429, plain, ((e22 = op2(e22, op2(e22, e22))) | ~ spl144_213), inference(avatar_component_clause, [], [f2427])).
fof(f2427, plain, (spl144_213 <=> (e22 = op2(e22, op2(e22, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_213])])).
fof(f4678, plain, (spl144_69 | ~ spl144_79 | ~ spl144_292), inference(avatar_contradiction_clause, [], [f4677])).
fof(f4677, plain, ($false | (spl144_69 | ~ spl144_79 | ~ spl144_292)), inference(subsumption_resolution, [], [f4676, f1517])).
fof(f1517, plain, (~ (e20 = op2(e23, e22)) | spl144_69), inference(avatar_component_clause, [], [f1516])).
fof(f1516, plain, (spl144_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_69])])).
fof(f4676, plain, ((e20 = op2(e23, e22)) | (~ spl144_79 | ~ spl144_292)), inference(forward_demodulation, [], [f3020, f1560])).
fof(f3020, plain, ((e20 = op2(e23, op2(e23, e20))) | ~ spl144_292), inference(avatar_component_clause, [], [f3018])).
fof(f3018, plain, (spl144_292 <=> (e20 = op2(e23, op2(e23, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_292])])).
fof(f4671, plain, (spl144_315 | ~ spl144_85), inference(avatar_split_clause, [], [f4634, f1584, f3184])).
fof(f1584, plain, (spl144_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_85])])).
fof(f4634, plain, ((e20 = h3(e12)) | ~ spl144_85), inference(backward_demodulation, [], [f1124, f1586])).
fof(f1586, plain, ((e20 = op2(e22, e22)) | ~ spl144_85), inference(avatar_component_clause, [], [f1584])).
fof(f4660, plain, (~ spl144_96 | ~ spl144_92), inference(avatar_split_clause, [], [f4573, f1613, f1630])).
fof(f1630, plain, (spl144_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_96])])).
fof(f4573, plain, (~ (e23 = op2(e22, e20)) | ~ spl144_92), inference(forward_demodulation, [], [f492, f1615])).
fof(f492, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f4631, plain, (~ spl144_90 | ~ spl144_92), inference(avatar_contradiction_clause, [], [f4630])).
fof(f4630, plain, ($false | (~ spl144_90 | ~ spl144_92)), inference(subsumption_resolution, [], [f4628, f514])).
fof(f4628, plain, ((e21 = e23) | (~ spl144_90 | ~ spl144_92)), inference(backward_demodulation, [], [f1615, f1607])).
fof(f1607, plain, ((e21 = op2(e22, e21)) | ~ spl144_90), inference(avatar_component_clause, [], [f1605])).
fof(f1605, plain, (spl144_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_90])])).
fof(f4616, plain, (spl144_105 | ~ spl144_110 | ~ spl144_220), inference(avatar_split_clause, [], [f4614, f2464, f1690, f1669])).
fof(f1669, plain, (spl144_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_105])])).
fof(f1690, plain, (spl144_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_110])])).
fof(f2464, plain, (spl144_220 <=> (e20 = op2(e21, op2(e21, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_220])])).
fof(f4614, plain, ((e20 = op2(e21, e21)) | (~ spl144_110 | ~ spl144_220)), inference(backward_demodulation, [], [f2466, f1692])).
fof(f1692, plain, ((e21 = op2(e21, e20)) | ~ spl144_110), inference(avatar_component_clause, [], [f1690])).
fof(f2466, plain, ((e20 = op2(e21, op2(e21, e20))) | ~ spl144_220), inference(avatar_component_clause, [], [f2464])).
fof(f4593, plain, (~ spl144_126 | ~ spl144_128), inference(avatar_contradiction_clause, [], [f4592])).
fof(f4592, plain, ($false | (~ spl144_126 | ~ spl144_128)), inference(subsumption_resolution, [], [f4591, f514])).
fof(f4591, plain, ((e21 = e23) | (~ spl144_126 | ~ spl144_128)), inference(backward_demodulation, [], [f1768, f1760])).
fof(f1768, plain, ((op2(e20, e20) = e23) | ~ spl144_128), inference(avatar_component_clause, [], [f1766])).
fof(f1766, plain, (spl144_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl144_128])])).
fof(f4583, plain, (~ spl144_110 | ~ spl144_102), inference(avatar_split_clause, [], [f4443, f1656, f1690])).
fof(f4443, plain, (~ (e21 = op2(e21, e20)) | ~ spl144_102), inference(forward_demodulation, [], [f487, f1658])).
fof(f1658, plain, ((e21 = op2(e21, e22)) | ~ spl144_102), inference(avatar_component_clause, [], [f1656])).
fof(f487, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f4580, plain, (~ spl144_97 | ~ spl144_303), inference(avatar_split_clause, [], [f4435, f3123, f1635])).
fof(f1635, plain, (spl144_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_97])])).
fof(f4435, plain, (~ (e20 = op2(e21, e23)) | ~ spl144_303), inference(forward_demodulation, [], [f3052, f3124])).
fof(f3052, plain, ~ (op2(e21, e23) = h2(e10)), inference(backward_demodulation, [], [f478, f3050])).
fof(f478, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4566, plain, (~ spl144_84 | ~ spl144_92), inference(avatar_split_clause, [], [f4565, f1613, f1579])).
fof(f1579, plain, (spl144_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_84])])).
fof(f4565, plain, (~ (e23 = op2(e22, e23)) | ~ spl144_92), inference(forward_demodulation, [], [f496, f1615])).
fof(f496, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4562, plain, (~ spl144_74 | ~ spl144_78), inference(avatar_split_clause, [], [f4561, f1554, f1537])).
fof(f1554, plain, (spl144_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_78])])).
fof(f4561, plain, (~ (e21 = op2(e23, e21)) | ~ spl144_78), inference(forward_demodulation, [], [f498, f1556])).
fof(f1556, plain, ((e21 = op2(e23, e20)) | ~ spl144_78), inference(avatar_component_clause, [], [f1554])).
fof(f498, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4560, plain, (~ spl144_75 | ~ spl144_318), inference(avatar_split_clause, [], [f4428, f3200, f1541])).
fof(f1541, plain, (spl144_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_75])])).
fof(f4428, plain, (~ (e22 = op2(e23, e21)) | ~ spl144_318), inference(forward_demodulation, [], [f3044, f3201])).
fof(f3044, plain, ~ (op2(e23, e21) = h2(e12)), inference(backward_demodulation, [], [f466, f1120])).
fof(f466, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4559, plain, (~ spl144_73 | ~ spl144_303), inference(avatar_split_clause, [], [f4430, f3123, f1533])).
fof(f1533, plain, (spl144_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_73])])).
fof(f4430, plain, (~ (e20 = op2(e23, e21)) | ~ spl144_303), inference(forward_demodulation, [], [f3055, f3124])).
fof(f3055, plain, ~ (op2(e23, e21) = h2(e10)), inference(backward_demodulation, [], [f502, f3050])).
fof(f502, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4548, plain, (~ spl144_65 | ~ spl144_72 | ~ spl144_290), inference(avatar_contradiction_clause, [], [f4547])).
fof(f4547, plain, ($false | (~ spl144_65 | ~ spl144_72 | ~ spl144_290)), inference(subsumption_resolution, [], [f4546, f511])).
fof(f511, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f4546, plain, ((e20 = e22) | (~ spl144_65 | ~ spl144_72 | ~ spl144_290)), inference(forward_demodulation, [], [f4545, f1501])).
fof(f4545, plain, ((e22 = op2(e23, e23)) | (~ spl144_72 | ~ spl144_290)), inference(forward_demodulation, [], [f3010, f1530])).
fof(f3010, plain, ((e22 = op2(e23, op2(e23, e22))) | ~ spl144_290), inference(avatar_component_clause, [], [f3008])).
fof(f3008, plain, (spl144_290 <=> (e22 = op2(e23, op2(e23, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl144_290])])).
fof(f4529, plain, (~ spl144_30 | ~ spl144_62), inference(avatar_split_clause, [], [f4524, f1454, f1318])).
fof(f1318, plain, (spl144_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_30])])).
fof(f4524, plain, (~ (e11 = op1(e12, e10)) | ~ spl144_62), inference(backward_demodulation, [], [f409, f1456])).
fof(f409, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f4498, plain, (~ spl144_101 | ~ spl144_102), inference(avatar_contradiction_clause, [], [f4497])).
fof(f4497, plain, ($false | (~ spl144_101 | ~ spl144_102)), inference(subsumption_resolution, [], [f4496, f510])).
fof(f4496, plain, ((e20 = e21) | (~ spl144_101 | ~ spl144_102)), inference(backward_demodulation, [], [f1658, f1654])).
fof(f4495, plain, (~ spl144_105 | ~ spl144_107), inference(avatar_contradiction_clause, [], [f4494])).
fof(f4494, plain, ($false | (~ spl144_105 | ~ spl144_107)), inference(subsumption_resolution, [], [f4493, f511])).
fof(f4493, plain, ((e20 = e22) | (~ spl144_105 | ~ spl144_107)), inference(backward_demodulation, [], [f1679, f1671])).
fof(f1671, plain, ((e20 = op2(e21, e21)) | ~ spl144_105), inference(avatar_component_clause, [], [f1669])).
fof(f4490, plain, (spl144_97 | ~ spl144_112 | ~ spl144_220), inference(avatar_contradiction_clause, [], [f4489])).
fof(f4489, plain, ($false | (spl144_97 | ~ spl144_112 | ~ spl144_220)), inference(subsumption_resolution, [], [f4487, f1636])).
fof(f1636, plain, (~ (e20 = op2(e21, e23)) | spl144_97), inference(avatar_component_clause, [], [f1635])).
fof(f4487, plain, ((e20 = op2(e21, e23)) | (~ spl144_112 | ~ spl144_220)), inference(backward_demodulation, [], [f2466, f1700])).
fof(f4455, plain, (~ spl144_119 | ~ spl144_71), inference(avatar_split_clause, [], [f4454, f1524, f1728])).
fof(f1524, plain, (spl144_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_71])])).
fof(f4454, plain, (~ (e22 = op2(e20, e22)) | ~ spl144_71), inference(forward_demodulation, [], [f470, f1526])).
fof(f1526, plain, ((e22 = op2(e23, e22)) | ~ spl144_71), inference(avatar_component_clause, [], [f1524])).
fof(f470, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4451, plain, (~ spl144_120 | ~ spl144_407), inference(avatar_split_clause, [], [f4321, f3769, f1732])).
fof(f3769, plain, (spl144_407 <=> (e23 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_407])])).
fof(f4321, plain, (~ (e23 = op2(e20, e22)) | ~ spl144_407), inference(forward_demodulation, [], [f3036, f3770])).
fof(f3770, plain, ((e23 = h1(e12)) | ~ spl144_407), inference(avatar_component_clause, [], [f3769])).
fof(f4448, plain, (~ spl144_113 | ~ spl144_303), inference(avatar_split_clause, [], [f4334, f3123, f1703])).
fof(f1703, plain, (spl144_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_113])])).
fof(f4334, plain, (~ (e20 = op2(e20, e23)) | ~ spl144_303), inference(backward_demodulation, [], [f3051, f3124])).
fof(f3051, plain, ~ (op2(e20, e23) = h2(e10)), inference(backward_demodulation, [], [f476, f3050])).
fof(f476, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4440, plain, (~ spl144_111 | ~ spl144_318), inference(avatar_split_clause, [], [f4439, f3200, f1694])).
fof(f4439, plain, (~ (e22 = op2(e21, e20)) | ~ spl144_318), inference(forward_demodulation, [], [f3045, f3201])).
fof(f3045, plain, ~ (op2(e21, e20) = h2(e12)), inference(backward_demodulation, [], [f486, f1120])).
fof(f486, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4438, plain, (~ spl144_98 | ~ spl144_102), inference(avatar_split_clause, [], [f4437, f1656, f1639])).
fof(f4437, plain, (~ (e21 = op2(e21, e23)) | ~ spl144_102), inference(forward_demodulation, [], [f491, f1658])).
fof(f491, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4436, plain, (~ spl144_99 | ~ spl144_318), inference(avatar_split_clause, [], [f4304, f3200, f1643])).
fof(f1643, plain, (spl144_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_99])])).
fof(f4304, plain, (~ (e22 = op2(e21, e23)) | ~ spl144_318), inference(forward_demodulation, [], [f3047, f3201])).
fof(f3047, plain, ~ (op2(e21, e23) = h2(e12)), inference(backward_demodulation, [], [f490, f1120])).
fof(f490, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4434, plain, (~ spl144_98 | ~ spl144_82), inference(avatar_split_clause, [], [f4303, f1571, f1639])).
fof(f4303, plain, (~ (e21 = op2(e21, e23)) | ~ spl144_82), inference(forward_demodulation, [], [f477, f1573])).
fof(f1573, plain, ((e21 = op2(e22, e23)) | ~ spl144_82), inference(avatar_component_clause, [], [f1571])).
fof(f477, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4416, plain, (~ spl144_63 | ~ spl144_55), inference(avatar_split_clause, [], [f4415, f1424, f1458])).
fof(f1458, plain, (spl144_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl144_63])])).
fof(f4415, plain, (~ (op1(e10, e10) = e12) | ~ spl144_55), inference(forward_demodulation, [], [f433, f1426])).
fof(f4411, plain, (~ spl144_31 | ~ spl144_19), inference(avatar_split_clause, [], [f4258, f1271, f1322])).
fof(f1322, plain, (spl144_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_31])])).
fof(f4258, plain, (~ (e12 = op1(e12, e10)) | ~ spl144_19), inference(forward_demodulation, [], [f446, f1273])).
fof(f446, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4408, plain, (~ spl144_1 | ~ spl144_2), inference(avatar_contradiction_clause, [], [f4407])).
fof(f4407, plain, ($false | (~ spl144_1 | ~ spl144_2)), inference(subsumption_resolution, [], [f4406, f504])).
fof(f504, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f4406, plain, ((e10 = e11) | (~ spl144_1 | ~ spl144_2)), inference(forward_demodulation, [], [f1201, f1197])).
fof(f1201, plain, ((e11 = op1(e13, e13)) | ~ spl144_2), inference(avatar_component_clause, [], [f1199])).
fof(f1199, plain, (spl144_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_2])])).
fof(f4370, plain, (~ spl144_107 | ~ spl144_108), inference(avatar_contradiction_clause, [], [f4369])).
fof(f4369, plain, ($false | (~ spl144_107 | ~ spl144_108)), inference(subsumption_resolution, [], [f4368, f515])).
fof(f4368, plain, ((e22 = e23) | (~ spl144_107 | ~ spl144_108)), inference(forward_demodulation, [], [f1683, f1679])).
fof(f1683, plain, ((e23 = op2(e21, e21)) | ~ spl144_108), inference(avatar_component_clause, [], [f1681])).
fof(f1681, plain, (spl144_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_108])])).
fof(f4366, plain, (~ spl144_116 | ~ spl144_303 | spl144_338), inference(avatar_contradiction_clause, [], [f4365])).
fof(f4365, plain, ($false | (~ spl144_116 | ~ spl144_303 | spl144_338)), inference(subsumption_resolution, [], [f4363, f3324])).
fof(f3324, plain, (~ (e23 = h4(e13)) | spl144_338), inference(avatar_component_clause, [], [f3322])).
fof(f3322, plain, (spl144_338 <=> (e23 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_338])])).
fof(f4363, plain, ((e23 = h4(e13)) | (~ spl144_116 | ~ spl144_303)), inference(backward_demodulation, [], [f4336, f1717])).
fof(f1717, plain, ((e23 = op2(e20, e23)) | ~ spl144_116), inference(avatar_component_clause, [], [f1715])).
fof(f1715, plain, (spl144_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_116])])).
fof(f4336, plain, ((op2(e20, e23) = h4(e13)) | ~ spl144_303), inference(backward_demodulation, [], [f3066, f3124])).
fof(f3066, plain, (h4(e13) = op2(h2(e10), e23)), inference(forward_demodulation, [], [f1129, f3050])).
fof(f1129, plain, (op2(op2(e23, e23), e23) = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(op2(e23, e23), e23) = h4(e13)) & (op2(e23, e23) = h4(e12)) & (h4(e10) = op2(op2(op2(e23, e23), e23), op2(op2(e23, e23), e23))) & (e23 = h4(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax17)).
fof(f4353, plain, (~ spl144_86 | spl144_91 | ~ spl144_213), inference(avatar_contradiction_clause, [], [f4352])).
fof(f4352, plain, ($false | (~ spl144_86 | spl144_91 | ~ spl144_213)), inference(subsumption_resolution, [], [f4351, f1610])).
fof(f1610, plain, (~ (e22 = op2(e22, e21)) | spl144_91), inference(avatar_component_clause, [], [f1609])).
fof(f4351, plain, ((e22 = op2(e22, e21)) | (~ spl144_86 | ~ spl144_213)), inference(forward_demodulation, [], [f2429, f1590])).
fof(f4341, plain, (~ spl144_69 | ~ spl144_303), inference(avatar_split_clause, [], [f4335, f3123, f1516])).
fof(f4335, plain, (~ (e20 = op2(e23, e22)) | ~ spl144_303), inference(backward_demodulation, [], [f3056, f3124])).
fof(f3056, plain, ~ (op2(e23, e22) = h2(e10)), inference(backward_demodulation, [], [f503, f3050])).
fof(f503, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4298, plain, (~ spl144_92 | ~ spl144_371), inference(avatar_split_clause, [], [f4297, f3532, f1613])).
fof(f3532, plain, (spl144_371 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_371])])).
fof(f4297, plain, (~ (e23 = op2(e22, e21)) | ~ spl144_371), inference(forward_demodulation, [], [f3061, f3533])).
fof(f3533, plain, ((e23 = h3(e12)) | ~ spl144_371), inference(avatar_component_clause, [], [f3532])).
fof(f3061, plain, ~ (op2(e22, e21) = h3(e12)), inference(backward_demodulation, [], [f495, f1124])).
fof(f495, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f4295, plain, (~ spl144_92 | ~ spl144_76), inference(avatar_split_clause, [], [f4294, f1545, f1613])).
fof(f1545, plain, (spl144_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_76])])).
fof(f4294, plain, (~ (e23 = op2(e22, e21)) | ~ spl144_76), inference(forward_demodulation, [], [f467, f1547])).
fof(f1547, plain, ((e23 = op2(e23, e21)) | ~ spl144_76), inference(avatar_component_clause, [], [f1545])).
fof(f467, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4289, plain, (spl144_92 | ~ spl144_318), inference(avatar_split_clause, [], [f4170, f3200, f1613])).
fof(f4170, plain, ((e23 = op2(e22, e21)) | ~ spl144_318), inference(backward_demodulation, [], [f3048, f3201])).
fof(f4263, plain, (~ spl144_17 | ~ spl144_1), inference(avatar_split_clause, [], [f4262, f1195, f1263])).
fof(f1263, plain, (spl144_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_17])])).
fof(f4262, plain, (~ (e10 = op1(e12, e13)) | ~ spl144_1), inference(forward_demodulation, [], [f431, f1197])).
fof(f431, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4253, plain, (~ spl144_9 | ~ spl144_1), inference(avatar_split_clause, [], [f4252, f1195, f1229])).
fof(f1229, plain, (spl144_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_9])])).
fof(f4252, plain, (~ (e10 = op1(e13, e11)) | ~ spl144_1), inference(forward_demodulation, [], [f454, f1197])).
fof(f454, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4250, plain, (~ spl144_10 | ~ spl144_58), inference(avatar_split_clause, [], [f4249, f1437, f1233])).
fof(f1437, plain, (spl144_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_58])])).
fof(f4249, plain, (~ (e11 = op1(e13, e11)) | ~ spl144_58), inference(forward_demodulation, [], [f416, f1439])).
fof(f1439, plain, ((e11 = op1(e10, e11)) | ~ spl144_58), inference(avatar_component_clause, [], [f1437])).
fof(f416, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4248, plain, (~ spl144_5 | ~ spl144_1), inference(avatar_split_clause, [], [f4247, f1195, f1212])).
fof(f4247, plain, (~ (e10 = op1(e13, e12)) | ~ spl144_1), inference(forward_demodulation, [], [f455, f1197])).
fof(f455, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4218, plain, (~ spl144_41 | ~ spl144_43), inference(avatar_contradiction_clause, [], [f4217])).
fof(f4217, plain, ($false | (~ spl144_41 | ~ spl144_43)), inference(subsumption_resolution, [], [f4216, f505])).
fof(f4216, plain, ((e10 = e12) | (~ spl144_41 | ~ spl144_43)), inference(backward_demodulation, [], [f1375, f1367])).
fof(f1367, plain, ((e10 = op1(e11, e11)) | ~ spl144_41), inference(avatar_component_clause, [], [f1365])).
fof(f1365, plain, (spl144_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_41])])).
fof(f4202, plain, (~ spl144_85 | ~ spl144_86), inference(avatar_contradiction_clause, [], [f4201])).
fof(f4201, plain, ($false | (~ spl144_85 | ~ spl144_86)), inference(subsumption_resolution, [], [f4200, f510])).
fof(f4200, plain, ((e20 = e21) | (~ spl144_85 | ~ spl144_86)), inference(forward_demodulation, [], [f1590, f1586])).
fof(f4197, plain, (~ spl144_94 | ~ spl144_95), inference(avatar_contradiction_clause, [], [f4196])).
fof(f4196, plain, ($false | (~ spl144_94 | ~ spl144_95)), inference(subsumption_resolution, [], [f4195, f513])).
fof(f4195, plain, ((e21 = e22) | (~ spl144_94 | ~ spl144_95)), inference(backward_demodulation, [], [f1628, f1624])).
fof(f1624, plain, ((e21 = op2(e22, e20)) | ~ spl144_94), inference(avatar_component_clause, [], [f1622])).
fof(f1628, plain, ((e22 = op2(e22, e20)) | ~ spl144_95), inference(avatar_component_clause, [], [f1626])).
fof(f4185, plain, (spl144_80 | ~ spl144_65 | ~ spl144_289), inference(avatar_split_clause, [], [f4184, f3003, f1499, f1562])).
fof(f3003, plain, (spl144_289 <=> (e23 = op2(e23, op2(e23, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl144_289])])).
fof(f4184, plain, ((e23 = op2(e23, e20)) | (~ spl144_65 | ~ spl144_289)), inference(forward_demodulation, [], [f3005, f1501])).
fof(f3005, plain, ((e23 = op2(e23, op2(e23, e23))) | ~ spl144_289), inference(avatar_component_clause, [], [f3003])).
fof(f4176, plain, (~ spl144_89 | ~ spl144_318), inference(avatar_contradiction_clause, [], [f4175])).
fof(f4175, plain, ($false | (~ spl144_89 | ~ spl144_318)), inference(subsumption_resolution, [], [f4174, f512])).
fof(f4174, plain, ((e20 = e23) | (~ spl144_89 | ~ spl144_318)), inference(forward_demodulation, [], [f4170, f1603])).
fof(f1603, plain, ((e20 = op2(e22, e21)) | ~ spl144_89), inference(avatar_component_clause, [], [f1601])).
fof(f1601, plain, (spl144_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_89])])).
fof(f4172, plain, (~ spl144_123 | ~ spl144_318), inference(avatar_split_clause, [], [f4168, f3200, f1745])).
fof(f1745, plain, (spl144_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_123])])).
fof(f4168, plain, (~ (e22 = op2(e20, e21)) | ~ spl144_318), inference(backward_demodulation, [], [f3042, f3201])).
fof(f3042, plain, ~ (op2(e20, e21) = h2(e12)), inference(backward_demodulation, [], [f462, f1120])).
fof(f462, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4162, plain, (~ spl144_338 | ~ spl144_52 | spl144_387), inference(avatar_split_clause, [], [f4161, f3628, f1411, f3322])).
fof(f3628, plain, (spl144_387 <=> (h4(e13) = h2(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_387])])).
fof(f4161, plain, (~ (e23 = h4(e13)) | (~ spl144_52 | spl144_387)), inference(forward_demodulation, [], [f4160, f3041])).
fof(f4160, plain, (~ (h2(e13) = h4(e13)) | (~ spl144_52 | spl144_387)), inference(forward_demodulation, [], [f3630, f1413])).
fof(f3630, plain, (~ (h4(e13) = h2(op1(e10, e13))) | spl144_387), inference(avatar_component_clause, [], [f3628])).
fof(f4159, plain, (spl144_113 | ~ spl144_225 | ~ spl144_407), inference(avatar_contradiction_clause, [], [f4158])).
fof(f4158, plain, ($false | (spl144_113 | ~ spl144_225 | ~ spl144_407)), inference(subsumption_resolution, [], [f4154, f1704])).
fof(f1704, plain, (~ (e20 = op2(e20, e23)) | spl144_113), inference(avatar_component_clause, [], [f1703])).
fof(f4154, plain, ((e20 = op2(e20, e23)) | (~ spl144_225 | ~ spl144_407)), inference(backward_demodulation, [], [f3789, f3770])).
fof(f3789, plain, ((e20 = op2(e20, h1(e12))) | ~ spl144_225), inference(forward_demodulation, [], [f2493, f1116])).
fof(f2493, plain, ((e20 = op2(e20, op2(e20, e20))) | ~ spl144_225), inference(avatar_component_clause, [], [f2491])).
fof(f2491, plain, (spl144_225 <=> (e20 = op2(e20, op2(e20, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl144_225])])).
fof(f4155, plain, (~ spl144_80 | ~ spl144_407), inference(avatar_split_clause, [], [f4151, f3769, f1562])).
fof(f4151, plain, (~ (e23 = op2(e23, e20)) | ~ spl144_407), inference(backward_demodulation, [], [f3034, f3770])).
fof(f3034, plain, ~ (op2(e23, e20) = h1(e12)), inference(backward_demodulation, [], [f458, f1116])).
fof(f458, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f4146, plain, (~ spl144_121 | ~ spl144_117), inference(avatar_split_clause, [], [f4145, f1720, f1737])).
fof(f1720, plain, (spl144_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_117])])).
fof(f4145, plain, (~ (e20 = op2(e20, e21)) | ~ spl144_117), inference(forward_demodulation, [], [f483, f1722])).
fof(f1722, plain, ((e20 = op2(e20, e22)) | ~ spl144_117), inference(avatar_component_clause, [], [f1720])).
fof(f483, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f4137, plain, (spl144_124 | ~ spl144_114 | ~ spl144_222), inference(avatar_split_clause, [], [f4136, f2476, f1707, f1749])).
fof(f1707, plain, (spl144_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_114])])).
fof(f2476, plain, (spl144_222 <=> (e23 = op2(e20, op2(e20, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl144_222])])).
fof(f4136, plain, ((e23 = op2(e20, e21)) | (~ spl144_114 | ~ spl144_222)), inference(forward_demodulation, [], [f2478, f1709])).
fof(f1709, plain, ((e21 = op2(e20, e23)) | ~ spl144_114), inference(avatar_component_clause, [], [f1707])).
fof(f2478, plain, ((e23 = op2(e20, op2(e20, e23))) | ~ spl144_222), inference(avatar_component_clause, [], [f2476])).
fof(f4129, plain, (~ spl144_104 | ~ spl144_100), inference(avatar_split_clause, [], [f4128, f1647, f1664])).
fof(f1664, plain, (spl144_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_104])])).
fof(f4128, plain, (~ (e23 = op2(e21, e22)) | ~ spl144_100), inference(forward_demodulation, [], [f491, f1649])).
fof(f1649, plain, ((e23 = op2(e21, e23)) | ~ spl144_100), inference(avatar_component_clause, [], [f1647])).
fof(f4124, plain, (~ spl144_311 | ~ spl144_82), inference(avatar_split_clause, [], [f4123, f1571, f3164])).
fof(f4123, plain, (~ (e21 = h3(e12)) | ~ spl144_82), inference(forward_demodulation, [], [f3062, f1573])).
fof(f3062, plain, ~ (op2(e22, e23) = h3(e12)), inference(backward_demodulation, [], [f497, f1124])).
fof(f497, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4115, plain, (spl144_303 | ~ spl144_65), inference(avatar_split_clause, [], [f4114, f1499, f3123])).
fof(f4114, plain, ((e20 = h2(e10)) | ~ spl144_65), inference(forward_demodulation, [], [f3050, f1501])).
fof(f4113, plain, (~ spl144_59 | ~ spl144_43), inference(avatar_split_clause, [], [f4112, f1373, f1441])).
fof(f1441, plain, (spl144_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_59])])).
fof(f4112, plain, (~ (e12 = op1(e10, e11)) | ~ spl144_43), inference(forward_demodulation, [], [f414, f1375])).
fof(f414, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4111, plain, (~ spl144_60 | ~ spl144_28), inference(avatar_split_clause, [], [f4110, f1309, f1445])).
fof(f1445, plain, (spl144_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_60])])).
fof(f4110, plain, (~ (e13 = op1(e10, e11)) | ~ spl144_28), inference(forward_demodulation, [], [f415, f1311])).
fof(f415, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f4091, plain, (~ spl144_33 | ~ spl144_1), inference(avatar_split_clause, [], [f4090, f1195, f1331])).
fof(f1331, plain, (spl144_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_33])])).
fof(f4090, plain, (~ (e10 = op1(e11, e13)) | ~ spl144_1), inference(forward_demodulation, [], [f430, f1197])).
fof(f430, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4089, plain, (~ spl144_32 | ~ spl144_28), inference(avatar_split_clause, [], [f4088, f1309, f1326])).
fof(f1326, plain, (spl144_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_32])])).
fof(f4088, plain, (~ (e13 = op1(e12, e10)) | ~ spl144_28), inference(forward_demodulation, [], [f444, f1311])).
fof(f444, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f4085, plain, (~ spl144_30 | ~ spl144_18), inference(avatar_split_clause, [], [f4084, f1267, f1318])).
fof(f4084, plain, (~ (e11 = op1(e12, e10)) | ~ spl144_18), inference(forward_demodulation, [], [f446, f1269])).
fof(f1269, plain, ((e11 = op1(e12, e13)) | ~ spl144_18), inference(avatar_component_clause, [], [f1267])).
fof(f4078, plain, (~ spl144_14 | ~ spl144_10), inference(avatar_split_clause, [], [f4077, f1233, f1250])).
fof(f1250, plain, (spl144_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_14])])).
fof(f4077, plain, (~ (e11 = op1(e13, e10)) | ~ spl144_10), inference(forward_demodulation, [], [f450, f1235])).
fof(f450, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4074, plain, (~ spl144_13 | ~ spl144_1), inference(avatar_split_clause, [], [f4073, f1195, f1246])).
fof(f1246, plain, (spl144_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_13])])).
fof(f4073, plain, (~ (e10 = op1(e13, e10)) | ~ spl144_1), inference(forward_demodulation, [], [f452, f1197])).
fof(f452, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4069, plain, (~ spl144_1 | ~ spl144_4), inference(avatar_contradiction_clause, [], [f4068])).
fof(f4068, plain, ($false | (~ spl144_1 | ~ spl144_4)), inference(subsumption_resolution, [], [f4067, f506])).
fof(f506, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f4067, plain, ((e10 = e13) | (~ spl144_1 | ~ spl144_4)), inference(backward_demodulation, [], [f1209, f1197])).
fof(f1209, plain, ((e13 = op1(e13, e13)) | ~ spl144_4), inference(avatar_component_clause, [], [f1207])).
fof(f1207, plain, (spl144_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_4])])).
fof(f4060, plain, (~ spl144_6 | ~ spl144_10), inference(avatar_split_clause, [], [f4058, f1233, f1216])).
fof(f1216, plain, (spl144_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_6])])).
fof(f4058, plain, (~ (e11 = op1(e13, e12)) | ~ spl144_10), inference(backward_demodulation, [], [f453, f1235])).
fof(f453, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4047, plain, (~ spl144_21 | ~ spl144_22), inference(avatar_contradiction_clause, [], [f4046])).
fof(f4046, plain, ($false | (~ spl144_21 | ~ spl144_22)), inference(subsumption_resolution, [], [f4045, f504])).
fof(f4045, plain, ((e10 = e11) | (~ spl144_21 | ~ spl144_22)), inference(backward_demodulation, [], [f1286, f1282])).
fof(f1282, plain, ((e10 = op1(e12, e12)) | ~ spl144_21), inference(avatar_component_clause, [], [f1280])).
fof(f4039, plain, (~ spl144_20 | ~ spl144_28), inference(avatar_split_clause, [], [f4036, f1309, f1275])).
fof(f1275, plain, (spl144_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_20])])).
fof(f4036, plain, (~ (e13 = op1(e12, e13)) | ~ spl144_28), inference(backward_demodulation, [], [f448, f1311])).
fof(f448, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4038, plain, (~ spl144_24 | ~ spl144_28), inference(avatar_split_clause, [], [f4035, f1309, f1292])).
fof(f4035, plain, (~ (e13 = op1(e12, e12)) | ~ spl144_28), inference(backward_demodulation, [], [f447, f1311])).
fof(f447, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4037, plain, (~ spl144_12 | ~ spl144_28), inference(avatar_split_clause, [], [f4034, f1309, f1241])).
fof(f1241, plain, (spl144_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_12])])).
fof(f4034, plain, (~ (e13 = op1(e13, e11)) | ~ spl144_28), inference(backward_demodulation, [], [f419, f1311])).
fof(f419, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4032, plain, (~ spl144_23 | ~ spl144_31), inference(avatar_split_clause, [], [f4028, f1322, f1288])).
fof(f4028, plain, (~ (e12 = op1(e12, e12)) | ~ spl144_31), inference(backward_demodulation, [], [f445, f1324])).
fof(f1324, plain, ((e12 = op1(e12, e10)) | ~ spl144_31), inference(avatar_component_clause, [], [f1322])).
fof(f445, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4007, plain, (spl144_28 | ~ spl144_43), inference(avatar_split_clause, [], [f3998, f1373, f1309])).
fof(f3998, plain, ((e13 = op1(e12, e11)) | ~ spl144_43), inference(backward_demodulation, [], [f1110, f1375])).
fof(f1110, plain, (e13 = op1(op1(e11, e11), e11)), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(op1(e11, e11), e11)) & (e12 = op1(e11, e11)) & (e10 = op1(op1(op1(e11, e11), e11), op1(op1(e11, e11), e11)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax12)).
fof(f4006, plain, (~ spl144_35 | ~ spl144_43), inference(avatar_split_clause, [], [f3997, f1373, f1339])).
fof(f1339, plain, (spl144_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_35])])).
fof(f3997, plain, (~ (e12 = op1(e11, e13)) | ~ spl144_43), inference(backward_demodulation, [], [f442, f1375])).
fof(f442, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4005, plain, (~ spl144_39 | ~ spl144_43), inference(avatar_split_clause, [], [f3996, f1373, f1356])).
fof(f1356, plain, (spl144_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_39])])).
fof(f3996, plain, (~ (e12 = op1(e11, e12)) | ~ spl144_43), inference(backward_demodulation, [], [f441, f1375])).
fof(f441, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4004, plain, (~ spl144_11 | ~ spl144_43), inference(avatar_split_clause, [], [f3995, f1373, f1237])).
fof(f1237, plain, (spl144_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_11])])).
fof(f3995, plain, (~ (e12 = op1(e13, e11)) | ~ spl144_43), inference(backward_demodulation, [], [f418, f1375])).
fof(f418, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4003, plain, (~ spl144_27 | ~ spl144_43), inference(avatar_split_clause, [], [f3994, f1373, f1305])).
fof(f3994, plain, (~ (e12 = op1(e12, e11)) | ~ spl144_43), inference(backward_demodulation, [], [f417, f1375])).
fof(f417, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f4002, plain, (~ spl144_43 | spl144_385), inference(avatar_contradiction_clause, [], [f4001])).
fof(f4001, plain, ($false | (~ spl144_43 | spl144_385)), inference(trivial_inequality_removal, [], [f4000])).
fof(f4000, plain, (~ (h2(e12) = h2(e12)) | (~ spl144_43 | spl144_385)), inference(backward_demodulation, [], [f3622, f1375])).
fof(f3622, plain, (~ (h2(e12) = h2(op1(e11, e11))) | spl144_385), inference(avatar_component_clause, [], [f3620])).
fof(f3620, plain, (spl144_385 <=> (h2(e12) = h2(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_385])])).
fof(f3983, plain, (~ spl144_51 | ~ spl144_52), inference(avatar_contradiction_clause, [], [f3982])).
fof(f3982, plain, ($false | (~ spl144_51 | ~ spl144_52)), inference(subsumption_resolution, [], [f3981, f509])).
fof(f3981, plain, ((e12 = e13) | (~ spl144_51 | ~ spl144_52)), inference(backward_demodulation, [], [f1413, f1409])).
fof(f1409, plain, ((e12 = op1(e10, e13)) | ~ spl144_51), inference(avatar_component_clause, [], [f1407])).
fof(f1407, plain, (spl144_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_51])])).
fof(f3958, plain, (~ spl144_49 | ~ spl144_57), inference(avatar_split_clause, [], [f3952, f1433, f1399])).
fof(f1399, plain, (spl144_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_49])])).
fof(f3952, plain, (~ (e10 = op1(e10, e13)) | ~ spl144_57), inference(backward_demodulation, [], [f436, f1435])).
fof(f436, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3957, plain, (~ spl144_53 | ~ spl144_57), inference(avatar_split_clause, [], [f3951, f1433, f1416])).
fof(f1416, plain, (spl144_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_53])])).
fof(f3951, plain, (~ (e10 = op1(e10, e12)) | ~ spl144_57), inference(backward_demodulation, [], [f435, f1435])).
fof(f435, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3955, plain, (~ spl144_25 | ~ spl144_57), inference(avatar_split_clause, [], [f3949, f1433, f1297])).
fof(f1297, plain, (spl144_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_25])])).
fof(f3949, plain, (~ (e10 = op1(e12, e11)) | ~ spl144_57), inference(backward_demodulation, [], [f415, f1435])).
fof(f3945, plain, (~ spl144_57 | ~ spl144_61), inference(avatar_split_clause, [], [f3935, f1450, f1433])).
fof(f1450, plain, (spl144_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_61])])).
fof(f3935, plain, (~ (e10 = op1(e10, e11)) | ~ spl144_61), inference(backward_demodulation, [], [f432, f1452])).
fof(f1452, plain, ((e10 = op1(e10, e10)) | ~ spl144_61), inference(avatar_component_clause, [], [f1450])).
fof(f432, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f3931, plain, (~ spl144_65 | ~ spl144_67), inference(avatar_contradiction_clause, [], [f3930])).
fof(f3930, plain, ($false | (~ spl144_65 | ~ spl144_67)), inference(subsumption_resolution, [], [f3929, f511])).
fof(f3929, plain, ((e20 = e22) | (~ spl144_65 | ~ spl144_67)), inference(backward_demodulation, [], [f1509, f1501])).
fof(f1509, plain, ((e22 = op2(e23, e23)) | ~ spl144_67), inference(avatar_component_clause, [], [f1507])).
fof(f1507, plain, (spl144_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_67])])).
fof(f3901, plain, (~ spl144_82 | ~ spl144_83), inference(avatar_contradiction_clause, [], [f3900])).
fof(f3900, plain, ($false | (~ spl144_82 | ~ spl144_83)), inference(subsumption_resolution, [], [f3899, f513])).
fof(f3899, plain, ((e21 = e22) | (~ spl144_82 | ~ spl144_83)), inference(backward_demodulation, [], [f1577, f1573])).
fof(f3894, plain, (~ spl144_85 | ~ spl144_87), inference(avatar_contradiction_clause, [], [f3893])).
fof(f3893, plain, ($false | (~ spl144_85 | ~ spl144_87)), inference(subsumption_resolution, [], [f3892, f511])).
fof(f3892, plain, ((e20 = e22) | (~ spl144_85 | ~ spl144_87)), inference(backward_demodulation, [], [f1594, f1586])).
fof(f1594, plain, ((e22 = op2(e22, e22)) | ~ spl144_87), inference(avatar_component_clause, [], [f1592])).
fof(f1592, plain, (spl144_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_87])])).
fof(f3888, plain, (spl144_371 | ~ spl144_88), inference(avatar_split_clause, [], [f3887, f1596, f3532])).
fof(f1596, plain, (spl144_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_88])])).
fof(f3887, plain, ((e23 = h3(e12)) | ~ spl144_88), inference(backward_demodulation, [], [f1124, f1598])).
fof(f1598, plain, ((e23 = op2(e22, e22)) | ~ spl144_88), inference(avatar_component_clause, [], [f1596])).
fof(f3851, plain, (spl144_318 | ~ spl144_107), inference(avatar_split_clause, [], [f3850, f1677, f3200])).
fof(f3850, plain, ((e22 = h2(e12)) | ~ spl144_107), inference(backward_demodulation, [], [f1120, f1679])).
fof(f3827, plain, (~ spl144_83 | ~ spl144_115), inference(avatar_split_clause, [], [f3822, f1711, f1575])).
fof(f1711, plain, (spl144_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_115])])).
fof(f3822, plain, (~ (e22 = op2(e22, e23)) | ~ spl144_115), inference(backward_demodulation, [], [f475, f1713])).
fof(f1713, plain, ((e22 = op2(e20, e23)) | ~ spl144_115), inference(avatar_component_clause, [], [f1711])).
fof(f475, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3805, plain, (~ spl144_121 | spl144_126 | ~ spl144_224), inference(avatar_contradiction_clause, [], [f3804])).
fof(f3804, plain, ($false | (~ spl144_121 | spl144_126 | ~ spl144_224)), inference(subsumption_resolution, [], [f3797, f1759])).
fof(f1759, plain, (~ (op2(e20, e20) = e21) | spl144_126), inference(avatar_component_clause, [], [f1758])).
fof(f3797, plain, ((op2(e20, e20) = e21) | (~ spl144_121 | ~ spl144_224)), inference(backward_demodulation, [], [f2488, f1739])).
fof(f2488, plain, ((e21 = op2(e20, op2(e20, e21))) | ~ spl144_224), inference(avatar_component_clause, [], [f2486])).
fof(f2486, plain, (spl144_224 <=> (e21 = op2(e20, op2(e20, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl144_224])])).
fof(f3792, plain, (spl144_407 | ~ spl144_128), inference(avatar_split_clause, [], [f3791, f1766, f3769])).
fof(f3791, plain, ((e23 = h1(e12)) | ~ spl144_128), inference(backward_demodulation, [], [f1116, f1768])).
fof(f3635, plain, (~ spl144_373 | ~ spl144_374 | ~ spl144_375 | ~ spl144_376 | spl144_321 | spl144_319 | spl144_317 | ~ spl144_377 | ~ spl144_378 | ~ spl144_379 | ~ spl144_380 | ~ spl144_381 | ~ spl144_382 | ~ spl144_383 | ~ spl144_384 | ~ spl144_385 | ~ spl144_386 | ~ spl144_387 | ~ spl144_388), inference(avatar_split_clause, [], [f3570, f3632, f3628, f3624, f3620, f3616, f3612, f3608, f3604, f3600, f3596, f3592, f3588, f3196, f3208, f3221, f3584, f3580, f3576, f3572])).
fof(f3221, plain, (spl144_321 <=> sP135), introduced(avatar_definition, [new_symbols(naming, [spl144_321])])).
fof(f3208, plain, (spl144_319 <=> sP136), introduced(avatar_definition, [new_symbols(naming, [spl144_319])])).
fof(f3196, plain, (spl144_317 <=> sP137), introduced(avatar_definition, [new_symbols(naming, [spl144_317])])).
fof(f3570, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | ~ (h4(e13) = h2(op1(e10, e13))) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3569, f1118])).
fof(f3569, plain, (~ (h4(e13) = h2(op1(e10, e13))) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3568, f3066])).
fof(f3568, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3567, f3041])).
fof(f3567, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3566, f1118])).
fof(f3566, plain, (~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3565, f1120])).
fof(f3565, plain, (~ (op2(e21, e21) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3564, f1118])).
fof(f3564, plain, (~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3563, f1118])).
fof(f3563, plain, (~ (op2(e21, e23) = h2(op1(e11, e13))) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3562, f1118])).
fof(f3562, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), e23)) | ~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3561, f3041])).
fof(f3561, plain, (~ (e23 = h2(op1(e12, e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3560, f3048])).
fof(f3560, plain, (~ (h2(op1(e12, e11)) = op2(h2(e12), e21)) | ~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3559, f1118])).
fof(f3559, plain, (~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | ~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3558, f3041])).
fof(f3558, plain, (~ (h2(op1(e13, e10)) = op2(e23, h2(e10))) | ~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3557, f3041])).
fof(f3557, plain, (~ (op2(e23, e21) = h2(op1(e13, e11))) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3556, f3041])).
fof(f3556, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3555, f1118])).
fof(f3555, plain, (~ (h2(op1(e13, e12)) = op2(e23, h2(e12))) | ~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3554, f3041])).
fof(f3554, plain, (~ (h2(e10) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3553, f3050])).
fof(f3553, plain, (~ (op2(e23, e23) = h2(op1(e13, e13))) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3552, f3041])).
fof(f3552, plain, (sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(subsumption_resolution, [], [f1185, f3041])).
fof(f1185, plain, (~ (e23 = h2(e13)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(cnf_transformation, [], [f167])).
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
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', co1)).
fof(f3230, plain, (~ spl144_321 | ~ spl144_303), inference(avatar_split_clause, [], [f1162, f3123, f3221])).
fof(f1162, plain, (~ (e20 = h2(e10)) | ~ sP135), inference(cnf_transformation, [], [f308])).
fof(f308, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP135), inference(nnf_transformation, [], [f158])).
fof(f3217, plain, ~ spl144_319, inference(avatar_split_clause, [], [f3216, f3208])).
fof(f3216, plain, ~ sP136, inference(subsumption_resolution, [], [f1159, f1118])).
fof(f1159, plain, (~ (e21 = h2(e11)) | ~ sP136), inference(cnf_transformation, [], [f307])).
fof(f307, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP136), inference(nnf_transformation, [], [f159])).
fof(f3203, plain, (~ spl144_317 | ~ spl144_318), inference(avatar_split_clause, [], [f1156, f3200, f3196])).
fof(f1156, plain, (~ (e22 = h2(e12)) | ~ sP137), inference(cnf_transformation, [], [f306])).
fof(f306, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP137), inference(nnf_transformation, [], [f160])).
fof(f3031, plain, spl144_65, inference(avatar_split_clause, [], [f3030, f1499])).
fof(f3030, plain, (e20 = op2(e23, e23)), inference(forward_demodulation, [], [f1111, f1113])).
fof(f1111, plain, (e20 = op2(op2(op2(e21, e21), e21), op2(op2(e21, e21), e21))), inference(cnf_transformation, [], [f13])).
fof(f3029, plain, spl144_107, inference(avatar_split_clause, [], [f1112, f1677])).
fof(f1112, plain, (e22 = op2(e21, e21)), inference(cnf_transformation, [], [f13])).
fof(f3028, plain, spl144_1, inference(avatar_split_clause, [], [f3027, f1195])).
fof(f3027, plain, (e10 = op1(e13, e13)), inference(forward_demodulation, [], [f1108, f1110])).
fof(f1108, plain, (e10 = op1(op1(op1(e11, e11), e11), op1(op1(e11, e11), e11))), inference(cnf_transformation, [], [f12])).
fof(f3026, plain, spl144_43, inference(avatar_split_clause, [], [f1109, f1373])).
fof(f1109, plain, (e12 = op1(e11, e11)), inference(cnf_transformation, [], [f12])).
fof(f3025, plain, (spl144_288 | spl144_287 | spl144_286 | spl144_285 | spl144_284 | spl144_283 | spl144_282 | spl144_281 | spl144_280 | spl144_279 | spl144_278 | spl144_277 | spl144_276 | spl144_275 | spl144_274 | spl144_273 | spl144_272 | spl144_271 | spl144_270 | spl144_269 | spl144_268 | spl144_267 | spl144_266 | spl144_265 | spl144_264 | spl144_263 | spl144_262 | spl144_261 | spl144_260 | spl144_259 | spl144_258 | spl144_257 | spl144_256 | spl144_255 | spl144_254 | spl144_253 | spl144_252 | spl144_251 | spl144_250 | spl144_249 | spl144_248 | spl144_247 | spl144_246 | spl144_245 | spl144_244 | spl144_243 | spl144_242 | spl144_241 | spl144_240 | spl144_239 | spl144_238 | spl144_237 | spl144_236 | spl144_235 | spl144_234 | spl144_233 | spl144_232 | spl144_231 | spl144_230 | spl144_229 | spl144_228 | spl144_227 | spl144_226 | spl144_68), inference(avatar_split_clause, [], [f1096, f1511, f2496, f2504, f2512, f2520, f2528, f2536, f2544, f2552, f2560, f2568, f2576, f2584, f2592, f2600, f2608, f2616, f2624, f2632, f2640, f2648, f2656, f2664, f2672, f2680, f2688, f2696, f2704, f2712, f2720, f2728, f2736, f2744, f2752, f2760, f2768, f2776, f2784, f2792, f2800, f2808, f2816, f2824, f2832, f2840, f2848, f2856, f2864, f2872, f2880, f2888, f2896, f2904, f2912, f2920, f2928, f2936, f2944, f2952, f2960, f2968, f2976, f2984, f2992])).
fof(f2992, plain, (spl144_288 <=> sP66), introduced(avatar_definition, [new_symbols(naming, [spl144_288])])).
fof(f2984, plain, (spl144_287 <=> sP67), introduced(avatar_definition, [new_symbols(naming, [spl144_287])])).
fof(f2976, plain, (spl144_286 <=> sP68), introduced(avatar_definition, [new_symbols(naming, [spl144_286])])).
fof(f2968, plain, (spl144_285 <=> sP69), introduced(avatar_definition, [new_symbols(naming, [spl144_285])])).
fof(f2960, plain, (spl144_284 <=> sP70), introduced(avatar_definition, [new_symbols(naming, [spl144_284])])).
fof(f2952, plain, (spl144_283 <=> sP71), introduced(avatar_definition, [new_symbols(naming, [spl144_283])])).
fof(f2944, plain, (spl144_282 <=> sP72), introduced(avatar_definition, [new_symbols(naming, [spl144_282])])).
fof(f2936, plain, (spl144_281 <=> sP73), introduced(avatar_definition, [new_symbols(naming, [spl144_281])])).
fof(f2928, plain, (spl144_280 <=> sP74), introduced(avatar_definition, [new_symbols(naming, [spl144_280])])).
fof(f2920, plain, (spl144_279 <=> sP75), introduced(avatar_definition, [new_symbols(naming, [spl144_279])])).
fof(f2912, plain, (spl144_278 <=> sP76), introduced(avatar_definition, [new_symbols(naming, [spl144_278])])).
fof(f2904, plain, (spl144_277 <=> sP77), introduced(avatar_definition, [new_symbols(naming, [spl144_277])])).
fof(f2896, plain, (spl144_276 <=> sP78), introduced(avatar_definition, [new_symbols(naming, [spl144_276])])).
fof(f2888, plain, (spl144_275 <=> sP79), introduced(avatar_definition, [new_symbols(naming, [spl144_275])])).
fof(f2880, plain, (spl144_274 <=> sP80), introduced(avatar_definition, [new_symbols(naming, [spl144_274])])).
fof(f2872, plain, (spl144_273 <=> sP81), introduced(avatar_definition, [new_symbols(naming, [spl144_273])])).
fof(f2864, plain, (spl144_272 <=> sP82), introduced(avatar_definition, [new_symbols(naming, [spl144_272])])).
fof(f2856, plain, (spl144_271 <=> sP83), introduced(avatar_definition, [new_symbols(naming, [spl144_271])])).
fof(f2848, plain, (spl144_270 <=> sP84), introduced(avatar_definition, [new_symbols(naming, [spl144_270])])).
fof(f2840, plain, (spl144_269 <=> sP85), introduced(avatar_definition, [new_symbols(naming, [spl144_269])])).
fof(f2832, plain, (spl144_268 <=> sP86), introduced(avatar_definition, [new_symbols(naming, [spl144_268])])).
fof(f2824, plain, (spl144_267 <=> sP87), introduced(avatar_definition, [new_symbols(naming, [spl144_267])])).
fof(f2816, plain, (spl144_266 <=> sP88), introduced(avatar_definition, [new_symbols(naming, [spl144_266])])).
fof(f2808, plain, (spl144_265 <=> sP89), introduced(avatar_definition, [new_symbols(naming, [spl144_265])])).
fof(f2800, plain, (spl144_264 <=> sP90), introduced(avatar_definition, [new_symbols(naming, [spl144_264])])).
fof(f2792, plain, (spl144_263 <=> sP91), introduced(avatar_definition, [new_symbols(naming, [spl144_263])])).
fof(f2784, plain, (spl144_262 <=> sP92), introduced(avatar_definition, [new_symbols(naming, [spl144_262])])).
fof(f2776, plain, (spl144_261 <=> sP93), introduced(avatar_definition, [new_symbols(naming, [spl144_261])])).
fof(f2768, plain, (spl144_260 <=> sP94), introduced(avatar_definition, [new_symbols(naming, [spl144_260])])).
fof(f2760, plain, (spl144_259 <=> sP95), introduced(avatar_definition, [new_symbols(naming, [spl144_259])])).
fof(f2752, plain, (spl144_258 <=> sP96), introduced(avatar_definition, [new_symbols(naming, [spl144_258])])).
fof(f2744, plain, (spl144_257 <=> sP97), introduced(avatar_definition, [new_symbols(naming, [spl144_257])])).
fof(f2736, plain, (spl144_256 <=> sP98), introduced(avatar_definition, [new_symbols(naming, [spl144_256])])).
fof(f2728, plain, (spl144_255 <=> sP99), introduced(avatar_definition, [new_symbols(naming, [spl144_255])])).
fof(f2720, plain, (spl144_254 <=> sP100), introduced(avatar_definition, [new_symbols(naming, [spl144_254])])).
fof(f2712, plain, (spl144_253 <=> sP101), introduced(avatar_definition, [new_symbols(naming, [spl144_253])])).
fof(f2704, plain, (spl144_252 <=> sP102), introduced(avatar_definition, [new_symbols(naming, [spl144_252])])).
fof(f2696, plain, (spl144_251 <=> sP103), introduced(avatar_definition, [new_symbols(naming, [spl144_251])])).
fof(f2688, plain, (spl144_250 <=> sP104), introduced(avatar_definition, [new_symbols(naming, [spl144_250])])).
fof(f2680, plain, (spl144_249 <=> sP105), introduced(avatar_definition, [new_symbols(naming, [spl144_249])])).
fof(f2672, plain, (spl144_248 <=> sP106), introduced(avatar_definition, [new_symbols(naming, [spl144_248])])).
fof(f2664, plain, (spl144_247 <=> sP107), introduced(avatar_definition, [new_symbols(naming, [spl144_247])])).
fof(f2656, plain, (spl144_246 <=> sP108), introduced(avatar_definition, [new_symbols(naming, [spl144_246])])).
fof(f2648, plain, (spl144_245 <=> sP109), introduced(avatar_definition, [new_symbols(naming, [spl144_245])])).
fof(f2640, plain, (spl144_244 <=> sP110), introduced(avatar_definition, [new_symbols(naming, [spl144_244])])).
fof(f2632, plain, (spl144_243 <=> sP111), introduced(avatar_definition, [new_symbols(naming, [spl144_243])])).
fof(f2624, plain, (spl144_242 <=> sP112), introduced(avatar_definition, [new_symbols(naming, [spl144_242])])).
fof(f2616, plain, (spl144_241 <=> sP113), introduced(avatar_definition, [new_symbols(naming, [spl144_241])])).
fof(f2608, plain, (spl144_240 <=> sP114), introduced(avatar_definition, [new_symbols(naming, [spl144_240])])).
fof(f2600, plain, (spl144_239 <=> sP115), introduced(avatar_definition, [new_symbols(naming, [spl144_239])])).
fof(f2592, plain, (spl144_238 <=> sP116), introduced(avatar_definition, [new_symbols(naming, [spl144_238])])).
fof(f2584, plain, (spl144_237 <=> sP117), introduced(avatar_definition, [new_symbols(naming, [spl144_237])])).
fof(f2576, plain, (spl144_236 <=> sP118), introduced(avatar_definition, [new_symbols(naming, [spl144_236])])).
fof(f2568, plain, (spl144_235 <=> sP119), introduced(avatar_definition, [new_symbols(naming, [spl144_235])])).
fof(f2560, plain, (spl144_234 <=> sP120), introduced(avatar_definition, [new_symbols(naming, [spl144_234])])).
fof(f2552, plain, (spl144_233 <=> sP121), introduced(avatar_definition, [new_symbols(naming, [spl144_233])])).
fof(f2544, plain, (spl144_232 <=> sP122), introduced(avatar_definition, [new_symbols(naming, [spl144_232])])).
fof(f2536, plain, (spl144_231 <=> sP123), introduced(avatar_definition, [new_symbols(naming, [spl144_231])])).
fof(f2528, plain, (spl144_230 <=> sP124), introduced(avatar_definition, [new_symbols(naming, [spl144_230])])).
fof(f2520, plain, (spl144_229 <=> sP125), introduced(avatar_definition, [new_symbols(naming, [spl144_229])])).
fof(f2512, plain, (spl144_228 <=> sP126), introduced(avatar_definition, [new_symbols(naming, [spl144_228])])).
fof(f2504, plain, (spl144_227 <=> sP127), introduced(avatar_definition, [new_symbols(naming, [spl144_227])])).
fof(f2496, plain, (spl144_226 <=> sP128), introduced(avatar_definition, [new_symbols(naming, [spl144_226])])).
fof(f1511, plain, (spl144_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_68])])).
fof(f1096, plain, ((e23 = op2(e23, e23)) | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69 | sP68 | sP67 | sP66), inference(cnf_transformation, [], [f154])).
fof(f154, plain, (((((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e23 = op2(e22, e22)) | ~ (e22 = op2(e22, e23))) & ((e23 = op2(e21, e21)) | ~ (e21 = op2(e21, e23))) & ((op2(e20, e20) = e23) | ~ (e20 = op2(e20, e23))) & (e23 = op2(e23, op2(e23, e23))) & (e22 = op2(e23, op2(e23, e22))) & (e21 = op2(e23, op2(e23, e21))) & (e20 = op2(e23, op2(e23, e20)))) | sP131 | sP130 | sP129) & ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69 | sP68 | sP67 | sP66)), inference(definition_folding, [], [f11, e153, e152, e151, e150, e149, e148, e147, e146, e145, e144, e143, e142, e141, e140, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124, e123, e122, e121, e120, e119, e118, e117, e116, e115, e114, e113, e112, e111, e110, e109, e108, e107, e106, e105, e104, e103, e102, e101, e100, e99, e98, e97, e96, e95, e94, e93, e92, e91, e90, e89, e88])).
fof(f88, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP66), inference(usedef, [], [e88])).
fof(e88, plain, (sP66 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP66])])).
fof(f89, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP67), inference(usedef, [], [e89])).
fof(e89, plain, (sP67 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP67])])).
fof(f90, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP68), inference(usedef, [], [e90])).
fof(e90, plain, (sP68 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP68])])).
fof(f91, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP69), inference(usedef, [], [e91])).
fof(e91, plain, (sP69 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP69])])).
fof(f92, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP70), inference(usedef, [], [e92])).
fof(e92, plain, (sP70 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP70])])).
fof(f93, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP71), inference(usedef, [], [e93])).
fof(e93, plain, (sP71 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP71])])).
fof(f94, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP72), inference(usedef, [], [e94])).
fof(e94, plain, (sP72 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP72])])).
fof(f95, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP73), inference(usedef, [], [e95])).
fof(e95, plain, (sP73 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP73])])).
fof(f96, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP74), inference(usedef, [], [e96])).
fof(e96, plain, (sP74 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP74])])).
fof(f97, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP75), inference(usedef, [], [e97])).
fof(e97, plain, (sP75 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP75])])).
fof(f98, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP76), inference(usedef, [], [e98])).
fof(e98, plain, (sP76 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP76])])).
fof(f99, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP77), inference(usedef, [], [e99])).
fof(e99, plain, (sP77 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP77])])).
fof(f100, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP78), inference(usedef, [], [e100])).
fof(e100, plain, (sP78 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP78])])).
fof(f101, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP79), inference(usedef, [], [e101])).
fof(e101, plain, (sP79 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP79])])).
fof(f102, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP80), inference(usedef, [], [e102])).
fof(e102, plain, (sP80 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP80])])).
fof(f103, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP81), inference(usedef, [], [e103])).
fof(e103, plain, (sP81 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP81])])).
fof(f104, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP82), inference(usedef, [], [e104])).
fof(e104, plain, (sP82 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP82])])).
fof(f105, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP83), inference(usedef, [], [e105])).
fof(e105, plain, (sP83 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP83])])).
fof(f106, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP84), inference(usedef, [], [e106])).
fof(e106, plain, (sP84 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP84])])).
fof(f107, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP85), inference(usedef, [], [e107])).
fof(e107, plain, (sP85 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP85])])).
fof(f108, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP86), inference(usedef, [], [e108])).
fof(e108, plain, (sP86 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP86])])).
fof(f109, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP87), inference(usedef, [], [e109])).
fof(e109, plain, (sP87 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP87])])).
fof(f110, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP88), inference(usedef, [], [e110])).
fof(e110, plain, (sP88 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP88])])).
fof(f111, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP89), inference(usedef, [], [e111])).
fof(e111, plain, (sP89 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP89])])).
fof(f112, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP90), inference(usedef, [], [e112])).
fof(e112, plain, (sP90 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP90])])).
fof(f113, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP91), inference(usedef, [], [e113])).
fof(e113, plain, (sP91 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP91])])).
fof(f114, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP92), inference(usedef, [], [e114])).
fof(e114, plain, (sP92 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP92])])).
fof(f115, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP93), inference(usedef, [], [e115])).
fof(e115, plain, (sP93 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP93])])).
fof(f116, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP94), inference(usedef, [], [e116])).
fof(e116, plain, (sP94 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP94])])).
fof(f117, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP95), inference(usedef, [], [e117])).
fof(e117, plain, (sP95 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP95])])).
fof(f118, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP96), inference(usedef, [], [e118])).
fof(e118, plain, (sP96 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP96])])).
fof(f119, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP97), inference(usedef, [], [e119])).
fof(e119, plain, (sP97 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP97])])).
fof(f120, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP98), inference(usedef, [], [e120])).
fof(e120, plain, (sP98 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP98])])).
fof(f121, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP99), inference(usedef, [], [e121])).
fof(e121, plain, (sP99 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP99])])).
fof(f122, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP100), inference(usedef, [], [e122])).
fof(e122, plain, (sP100 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP100])])).
fof(f123, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP101), inference(usedef, [], [e123])).
fof(e123, plain, (sP101 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP101])])).
fof(f124, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP102), inference(usedef, [], [e124])).
fof(e124, plain, (sP102 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP102])])).
fof(f125, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP103), inference(usedef, [], [e125])).
fof(e125, plain, (sP103 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP103])])).
fof(f126, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP104), inference(usedef, [], [e126])).
fof(e126, plain, (sP104 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP104])])).
fof(f127, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP105), inference(usedef, [], [e127])).
fof(e127, plain, (sP105 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP105])])).
fof(f128, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP106), inference(usedef, [], [e128])).
fof(e128, plain, (sP106 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP106])])).
fof(f129, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP107), inference(usedef, [], [e129])).
fof(e129, plain, (sP107 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP107])])).
fof(f130, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP108), inference(usedef, [], [e130])).
fof(e130, plain, (sP108 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP108])])).
fof(f131, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP109), inference(usedef, [], [e131])).
fof(e131, plain, (sP109 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP109])])).
fof(f132, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP110), inference(usedef, [], [e132])).
fof(e132, plain, (sP110 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP110])])).
fof(f133, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP111), inference(usedef, [], [e133])).
fof(e133, plain, (sP111 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP111])])).
fof(f134, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP112), inference(usedef, [], [e134])).
fof(e134, plain, (sP112 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP112])])).
fof(f135, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP113), inference(usedef, [], [e135])).
fof(e135, plain, (sP113 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP113])])).
fof(f136, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP114), inference(usedef, [], [e136])).
fof(e136, plain, (sP114 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP114])])).
fof(f137, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP115), inference(usedef, [], [e137])).
fof(e137, plain, (sP115 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP115])])).
fof(f138, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP116), inference(usedef, [], [e138])).
fof(e138, plain, (sP116 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP116])])).
fof(f139, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP117), inference(usedef, [], [e139])).
fof(e139, plain, (sP117 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP117])])).
fof(f140, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP118), inference(usedef, [], [e140])).
fof(e140, plain, (sP118 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP118])])).
fof(f141, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP119), inference(usedef, [], [e141])).
fof(e141, plain, (sP119 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP119])])).
fof(f142, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP120), inference(usedef, [], [e142])).
fof(e142, plain, (sP120 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP120])])).
fof(f143, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP121), inference(usedef, [], [e143])).
fof(e143, plain, (sP121 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP121])])).
fof(f144, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP122), inference(usedef, [], [e144])).
fof(e144, plain, (sP122 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP122])])).
fof(f145, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP123), inference(usedef, [], [e145])).
fof(e145, plain, (sP123 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP123])])).
fof(f146, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP124), inference(usedef, [], [e146])).
fof(e146, plain, (sP124 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP124])])).
fof(f147, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP125), inference(usedef, [], [e147])).
fof(e147, plain, (sP125 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP125])])).
fof(f148, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP126), inference(usedef, [], [e148])).
fof(e148, plain, (sP126 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP126])])).
fof(f149, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP127), inference(usedef, [], [e149])).
fof(e149, plain, (sP127 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP127])])).
fof(f150, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP128), inference(usedef, [], [e150])).
fof(e150, plain, (sP128 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP128])])).
fof(f151, plain, ((((e20 = op2(e23, e23)) | ~ (e23 = op2(e23, e20))) & ((e20 = op2(e22, e22)) | ~ (e22 = op2(e22, e20))) & ((e20 = op2(e21, e21)) | ~ (e21 = op2(e21, e20))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, op2(e20, e23))) & (e22 = op2(e20, op2(e20, e22))) & (e21 = op2(e20, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20)))) | ~ sP129), inference(usedef, [], [e151])).
fof(e151, plain, (sP129 <=> (((e20 = op2(e23, e23)) | ~ (e23 = op2(e23, e20))) & ((e20 = op2(e22, e22)) | ~ (e22 = op2(e22, e20))) & ((e20 = op2(e21, e21)) | ~ (e21 = op2(e21, e20))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, op2(e20, e23))) & (e22 = op2(e20, op2(e20, e22))) & (e21 = op2(e20, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP129])])).
fof(f152, plain, ((((e21 = op2(e23, e23)) | ~ (e23 = op2(e23, e21))) & ((e21 = op2(e22, e22)) | ~ (e22 = op2(e22, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((op2(e20, e20) = e21) | ~ (e20 = op2(e20, e21))) & (e23 = op2(e21, op2(e21, e23))) & (e22 = op2(e21, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e21, op2(e21, e20)))) | ~ sP130), inference(usedef, [], [e152])).
fof(e152, plain, (sP130 <=> (((e21 = op2(e23, e23)) | ~ (e23 = op2(e23, e21))) & ((e21 = op2(e22, e22)) | ~ (e22 = op2(e22, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((op2(e20, e20) = e21) | ~ (e20 = op2(e20, e21))) & (e23 = op2(e21, op2(e21, e23))) & (e22 = op2(e21, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e21, op2(e21, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP130])])).
fof(f153, plain, ((((e22 = op2(e23, e23)) | ~ (e23 = op2(e23, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e21, e21)) | ~ (e21 = op2(e21, e22))) & ((op2(e20, e20) = e22) | ~ (e20 = op2(e20, e22))) & (e23 = op2(e22, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e22, op2(e22, e21))) & (e20 = op2(e22, op2(e22, e20)))) | ~ sP131), inference(usedef, [], [e153])).
fof(e153, plain, (sP131 <=> (((e22 = op2(e23, e23)) | ~ (e23 = op2(e23, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e21, e21)) | ~ (e21 = op2(e21, e22))) & ((op2(e20, e20) = e22) | ~ (e20 = op2(e20, e22))) & (e23 = op2(e22, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e22, op2(e22, e21))) & (e20 = op2(e22, op2(e22, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP131])])).
fof(f11, plain, (((((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e23 = op2(e22, e22)) | ~ (e22 = op2(e22, e23))) & ((e23 = op2(e21, e21)) | ~ (e21 = op2(e21, e23))) & ((op2(e20, e20) = e23) | ~ (e20 = op2(e20, e23))) & (e23 = op2(e23, op2(e23, e23))) & (e22 = op2(e23, op2(e23, e22))) & (e21 = op2(e23, op2(e23, e21))) & (e20 = op2(e23, op2(e23, e20)))) | (((e22 = op2(e23, e23)) | ~ (e23 = op2(e23, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e21, e21)) | ~ (e21 = op2(e21, e22))) & ((op2(e20, e20) = e22) | ~ (e20 = op2(e20, e22))) & (e23 = op2(e22, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e22, op2(e22, e21))) & (e20 = op2(e22, op2(e22, e20)))) | (((e21 = op2(e23, e23)) | ~ (e23 = op2(e23, e21))) & ((e21 = op2(e22, e22)) | ~ (e22 = op2(e22, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((op2(e20, e20) = e21) | ~ (e20 = op2(e20, e21))) & (e23 = op2(e21, op2(e21, e23))) & (e22 = op2(e21, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e21, op2(e21, e20)))) | (((e20 = op2(e23, e23)) | ~ (e23 = op2(e23, e20))) & ((e20 = op2(e22, e22)) | ~ (e22 = op2(e22, e20))) & ((e20 = op2(e21, e21)) | ~ (e21 = op2(e21, e20))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, op2(e20, e23))) & (e22 = op2(e20, op2(e20, e22))) & (e21 = op2(e20, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20))))) & ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax11)).
fof(f3024, plain, (spl144_288 | spl144_287 | spl144_286 | spl144_285 | spl144_284 | spl144_283 | spl144_282 | spl144_281 | spl144_280 | spl144_279 | spl144_278 | spl144_277 | spl144_276 | spl144_275 | spl144_274 | spl144_273 | spl144_272 | spl144_271 | spl144_270 | spl144_269 | spl144_268 | spl144_267 | spl144_266 | spl144_265 | spl144_264 | spl144_263 | spl144_262 | spl144_261 | spl144_260 | spl144_259 | spl144_258 | spl144_257 | spl144_256 | spl144_255 | spl144_254 | spl144_253 | spl144_252 | spl144_251 | spl144_250 | spl144_249 | spl144_248 | spl144_247 | spl144_246 | spl144_245 | spl144_244 | spl144_243 | spl144_242 | spl144_241 | spl144_240 | spl144_239 | spl144_238 | spl144_237 | spl144_236 | spl144_235 | spl144_234 | spl144_233 | spl144_232 | spl144_231 | spl144_230 | spl144_229 | spl144_228 | spl144_227 | spl144_226 | ~ spl144_68), inference(avatar_split_clause, [], [f1097, f1511, f2496, f2504, f2512, f2520, f2528, f2536, f2544, f2552, f2560, f2568, f2576, f2584, f2592, f2600, f2608, f2616, f2624, f2632, f2640, f2648, f2656, f2664, f2672, f2680, f2688, f2696, f2704, f2712, f2720, f2728, f2736, f2744, f2752, f2760, f2768, f2776, f2784, f2792, f2800, f2808, f2816, f2824, f2832, f2840, f2848, f2856, f2864, f2872, f2880, f2888, f2896, f2904, f2912, f2920, f2928, f2936, f2944, f2952, f2960, f2968, f2976, f2984, f2992])).
fof(f1097, plain, (~ (e23 = op2(e23, e23)) | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69 | sP68 | sP67 | sP66), inference(cnf_transformation, [], [f154])).
fof(f3021, plain, (spl144_221 | spl144_216 | spl144_211 | spl144_292), inference(avatar_split_clause, [], [f1100, f3018, f2415, f2442, f2469])).
fof(f2469, plain, (spl144_221 <=> sP129), introduced(avatar_definition, [new_symbols(naming, [spl144_221])])).
fof(f2442, plain, (spl144_216 <=> sP130), introduced(avatar_definition, [new_symbols(naming, [spl144_216])])).
fof(f2415, plain, (spl144_211 <=> sP131), introduced(avatar_definition, [new_symbols(naming, [spl144_211])])).
fof(f1100, plain, ((e20 = op2(e23, op2(e23, e20))) | sP131 | sP130 | sP129), inference(cnf_transformation, [], [f154])).
fof(f3011, plain, (spl144_221 | spl144_216 | spl144_211 | spl144_290), inference(avatar_split_clause, [], [f1102, f3008, f2415, f2442, f2469])).
fof(f1102, plain, ((e22 = op2(e23, op2(e23, e22))) | sP131 | sP130 | sP129), inference(cnf_transformation, [], [f154])).
fof(f3006, plain, (spl144_221 | spl144_216 | spl144_211 | spl144_289), inference(avatar_split_clause, [], [f1103, f3003, f2415, f2442, f2469])).
fof(f1103, plain, ((e23 = op2(e23, op2(e23, e23))) | sP131 | sP130 | sP129), inference(cnf_transformation, [], [f154])).
fof(f2999, plain, (spl144_221 | spl144_216 | spl144_211 | ~ spl144_83 | spl144_88), inference(avatar_split_clause, [], [f1106, f1596, f1575, f2415, f2442, f2469])).
fof(f1106, plain, ((e23 = op2(e22, e22)) | ~ (e22 = op2(e22, e23)) | sP131 | sP130 | sP129), inference(cnf_transformation, [], [f154])).
fof(f2998, plain, (~ spl144_288 | spl144_125), inference(avatar_split_clause, [], [f1092, f1754, f2992])).
fof(f1754, plain, (spl144_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_125])])).
fof(f1092, plain, ((e20 = op2(e20, e20)) | ~ sP66), inference(cnf_transformation, [], [f299])).
fof(f299, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP66), inference(nnf_transformation, [], [f88])).
fof(f2997, plain, (~ spl144_288 | ~ spl144_125), inference(avatar_split_clause, [], [f1093, f1754, f2992])).
fof(f1093, plain, (~ (e20 = op2(e20, e20)) | ~ sP66), inference(cnf_transformation, [], [f299])).
fof(f2988, plain, (~ spl144_287 | spl144_105), inference(avatar_split_clause, [], [f1090, f1669, f2984])).
fof(f1090, plain, ((e20 = op2(e21, e21)) | ~ sP67), inference(cnf_transformation, [], [f298])).
fof(f298, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP67), inference(nnf_transformation, [], [f89])).
fof(f2979, plain, (~ spl144_286 | ~ spl144_95), inference(avatar_split_clause, [], [f1087, f1626, f2976])).
fof(f1087, plain, (~ (e22 = op2(e22, e20)) | ~ sP68), inference(cnf_transformation, [], [f297])).
fof(f297, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP68), inference(nnf_transformation, [], [f90])).
fof(f2971, plain, (~ spl144_285 | ~ spl144_80), inference(avatar_split_clause, [], [f1083, f1562, f2968])).
fof(f1083, plain, (~ (e23 = op2(e23, e20)) | ~ sP69), inference(cnf_transformation, [], [f296])).
fof(f296, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP69), inference(nnf_transformation, [], [f91])).
fof(f2963, plain, (~ spl144_284 | ~ spl144_121), inference(avatar_split_clause, [], [f1079, f1737, f2960])).
fof(f1079, plain, (~ (e20 = op2(e20, e21)) | ~ sP70), inference(cnf_transformation, [], [f295])).
fof(f295, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP70), inference(nnf_transformation, [], [f92])).
fof(f2957, plain, (~ spl144_283 | ~ spl144_126), inference(avatar_split_clause, [], [f1073, f1758, f2952])).
fof(f1073, plain, (~ (op2(e20, e20) = e21) | ~ sP71), inference(cnf_transformation, [], [f294])).
fof(f294, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP71), inference(nnf_transformation, [], [f93])).
fof(f2949, plain, (~ spl144_282 | ~ spl144_126), inference(avatar_split_clause, [], [f1069, f1758, f2944])).
fof(f1069, plain, (~ (op2(e20, e20) = e21) | ~ sP72), inference(cnf_transformation, [], [f293])).
fof(f293, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP72), inference(nnf_transformation, [], [f94])).
fof(f2948, plain, (~ spl144_282 | spl144_86), inference(avatar_split_clause, [], [f1070, f1588, f2944])).
fof(f1070, plain, ((e21 = op2(e22, e22)) | ~ sP72), inference(cnf_transformation, [], [f293])).
fof(f2940, plain, (~ spl144_281 | spl144_66), inference(avatar_split_clause, [], [f1066, f1503, f2936])).
fof(f1066, plain, ((e21 = op2(e23, e23)) | ~ sP73), inference(cnf_transformation, [], [f292])).
fof(f292, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP73), inference(nnf_transformation, [], [f95])).
fof(f2934, plain, (~ spl144_280 | spl144_117), inference(avatar_split_clause, [], [f1060, f1720, f2928])).
fof(f1060, plain, ((e20 = op2(e20, e22)) | ~ sP74), inference(cnf_transformation, [], [f291])).
fof(f291, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP74), inference(nnf_transformation, [], [f96])).
fof(f2926, plain, (~ spl144_279 | spl144_117), inference(avatar_split_clause, [], [f1056, f1720, f2920])).
fof(f1056, plain, ((e20 = op2(e20, e22)) | ~ sP75), inference(cnf_transformation, [], [f290])).
fof(f290, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP75), inference(nnf_transformation, [], [f97])).
fof(f2918, plain, (~ spl144_278 | spl144_117), inference(avatar_split_clause, [], [f1052, f1720, f2912])).
fof(f1052, plain, ((e20 = op2(e20, e22)) | ~ sP76), inference(cnf_transformation, [], [f289])).
fof(f289, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP76), inference(nnf_transformation, [], [f98])).
fof(f2908, plain, (~ spl144_277 | spl144_67), inference(avatar_split_clause, [], [f1050, f1507, f2904])).
fof(f1050, plain, ((e22 = op2(e23, e23)) | ~ sP77), inference(cnf_transformation, [], [f288])).
fof(f288, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP77), inference(nnf_transformation, [], [f99])).
fof(f2902, plain, (~ spl144_276 | spl144_113), inference(avatar_split_clause, [], [f1044, f1703, f2896])).
fof(f1044, plain, ((e20 = op2(e20, e23)) | ~ sP78), inference(cnf_transformation, [], [f287])).
fof(f287, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP78), inference(nnf_transformation, [], [f100])).
fof(f2894, plain, (~ spl144_275 | spl144_113), inference(avatar_split_clause, [], [f1040, f1703, f2888])).
fof(f1040, plain, ((e20 = op2(e20, e23)) | ~ sP79), inference(cnf_transformation, [], [f286])).
fof(f286, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP79), inference(nnf_transformation, [], [f101])).
fof(f2886, plain, (~ spl144_274 | spl144_113), inference(avatar_split_clause, [], [f1036, f1703, f2880])).
fof(f1036, plain, ((e20 = op2(e20, e23)) | ~ sP80), inference(cnf_transformation, [], [f285])).
fof(f285, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP80), inference(nnf_transformation, [], [f102])).
fof(f2878, plain, (~ spl144_273 | spl144_113), inference(avatar_split_clause, [], [f1032, f1703, f2872])).
fof(f1032, plain, ((e20 = op2(e20, e23)) | ~ sP81), inference(cnf_transformation, [], [f284])).
fof(f284, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP81), inference(nnf_transformation, [], [f103])).
fof(f2870, plain, (~ spl144_272 | spl144_110), inference(avatar_split_clause, [], [f1028, f1690, f2864])).
fof(f1028, plain, ((e21 = op2(e21, e20)) | ~ sP82), inference(cnf_transformation, [], [f283])).
fof(f283, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP82), inference(nnf_transformation, [], [f104])).
fof(f2860, plain, (~ spl144_271 | spl144_105), inference(avatar_split_clause, [], [f1026, f1669, f2856])).
fof(f1026, plain, ((e20 = op2(e21, e21)) | ~ sP83), inference(cnf_transformation, [], [f282])).
fof(f282, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP83), inference(nnf_transformation, [], [f105])).
fof(f2854, plain, (~ spl144_270 | spl144_110), inference(avatar_split_clause, [], [f1020, f1690, f2848])).
fof(f1020, plain, ((e21 = op2(e21, e20)) | ~ sP84), inference(cnf_transformation, [], [f281])).
fof(f281, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP84), inference(nnf_transformation, [], [f106])).
fof(f2846, plain, (~ spl144_269 | spl144_110), inference(avatar_split_clause, [], [f1016, f1690, f2840])).
fof(f1016, plain, ((e21 = op2(e21, e20)) | ~ sP85), inference(cnf_transformation, [], [f280])).
fof(f280, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP85), inference(nnf_transformation, [], [f107])).
fof(f2843, plain, (~ spl144_269 | ~ spl144_80), inference(avatar_split_clause, [], [f1019, f1562, f2840])).
fof(f1019, plain, (~ (e23 = op2(e23, e20)) | ~ sP85), inference(cnf_transformation, [], [f280])).
fof(f2835, plain, (~ spl144_268 | ~ spl144_121), inference(avatar_split_clause, [], [f1015, f1737, f2832])).
fof(f1015, plain, (~ (e20 = op2(e20, e21)) | ~ sP86), inference(cnf_transformation, [], [f279])).
fof(f279, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP86), inference(nnf_transformation, [], [f108])).
fof(f2830, plain, (~ spl144_267 | spl144_106), inference(avatar_split_clause, [], [f1008, f1673, f2824])).
fof(f1673, plain, (spl144_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_106])])).
fof(f1008, plain, ((e21 = op2(e21, e21)) | ~ sP87), inference(cnf_transformation, [], [f278])).
fof(f278, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP87), inference(nnf_transformation, [], [f109])).
fof(f2829, plain, (~ spl144_267 | ~ spl144_106), inference(avatar_split_clause, [], [f1009, f1673, f2824])).
fof(f1009, plain, (~ (e21 = op2(e21, e21)) | ~ sP87), inference(cnf_transformation, [], [f278])).
fof(f2820, plain, (~ spl144_266 | spl144_86), inference(avatar_split_clause, [], [f1006, f1588, f2816])).
fof(f1006, plain, ((e21 = op2(e22, e22)) | ~ sP88), inference(cnf_transformation, [], [f277])).
fof(f277, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP88), inference(nnf_transformation, [], [f110])).
fof(f2812, plain, (~ spl144_265 | spl144_66), inference(avatar_split_clause, [], [f1002, f1503, f2808])).
fof(f1002, plain, ((e21 = op2(e23, e23)) | ~ sP89), inference(cnf_transformation, [], [f276])).
fof(f276, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP89), inference(nnf_transformation, [], [f111])).
fof(f2805, plain, (~ spl144_264 | ~ spl144_107), inference(avatar_split_clause, [], [f997, f1677, f2800])).
fof(f997, plain, (~ (e22 = op2(e21, e21)) | ~ sP90), inference(cnf_transformation, [], [f275])).
fof(f275, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP90), inference(nnf_transformation, [], [f112])).
fof(f2797, plain, (~ spl144_263 | ~ spl144_107), inference(avatar_split_clause, [], [f993, f1677, f2792])).
fof(f993, plain, (~ (e22 = op2(e21, e21)) | ~ sP91), inference(cnf_transformation, [], [f274])).
fof(f274, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP91), inference(nnf_transformation, [], [f113])).
fof(f2789, plain, (~ spl144_262 | ~ spl144_107), inference(avatar_split_clause, [], [f989, f1677, f2784])).
fof(f989, plain, (~ (e22 = op2(e21, e21)) | ~ sP92), inference(cnf_transformation, [], [f273])).
fof(f273, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP92), inference(nnf_transformation, [], [f114])).
fof(f2781, plain, (~ spl144_261 | ~ spl144_107), inference(avatar_split_clause, [], [f985, f1677, f2776])).
fof(f985, plain, (~ (e22 = op2(e21, e21)) | ~ sP93), inference(cnf_transformation, [], [f272])).
fof(f272, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP93), inference(nnf_transformation, [], [f115])).
fof(f2774, plain, (~ spl144_260 | spl144_98), inference(avatar_split_clause, [], [f980, f1639, f2768])).
fof(f980, plain, ((e21 = op2(e21, e23)) | ~ sP94), inference(cnf_transformation, [], [f271])).
fof(f271, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP94), inference(nnf_transformation, [], [f116])).
fof(f2772, plain, (~ spl144_260 | spl144_128), inference(avatar_split_clause, [], [f982, f1766, f2768])).
fof(f982, plain, ((op2(e20, e20) = e23) | ~ sP94), inference(cnf_transformation, [], [f271])).
fof(f2764, plain, (~ spl144_259 | spl144_108), inference(avatar_split_clause, [], [f978, f1681, f2760])).
fof(f978, plain, ((e23 = op2(e21, e21)) | ~ sP95), inference(cnf_transformation, [], [f270])).
fof(f270, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP95), inference(nnf_transformation, [], [f117])).
fof(f2756, plain, (~ spl144_258 | spl144_88), inference(avatar_split_clause, [], [f974, f1596, f2752])).
fof(f974, plain, ((e23 = op2(e22, e22)) | ~ sP96), inference(cnf_transformation, [], [f269])).
fof(f269, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP96), inference(nnf_transformation, [], [f118])).
fof(f2750, plain, (~ spl144_257 | spl144_98), inference(avatar_split_clause, [], [f968, f1639, f2744])).
fof(f968, plain, ((e21 = op2(e21, e23)) | ~ sP97), inference(cnf_transformation, [], [f268])).
fof(f268, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP97), inference(nnf_transformation, [], [f119])).
fof(f2741, plain, (~ spl144_256 | ~ spl144_85), inference(avatar_split_clause, [], [f965, f1584, f2736])).
fof(f965, plain, (~ (e20 = op2(e22, e22)) | ~ sP98), inference(cnf_transformation, [], [f267])).
fof(f267, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP98), inference(nnf_transformation, [], [f120])).
fof(f2732, plain, (~ spl144_255 | spl144_105), inference(avatar_split_clause, [], [f962, f1669, f2728])).
fof(f962, plain, ((e20 = op2(e21, e21)) | ~ sP99), inference(cnf_transformation, [], [f266])).
fof(f266, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP99), inference(nnf_transformation, [], [f121])).
fof(f2725, plain, (~ spl144_254 | ~ spl144_85), inference(avatar_split_clause, [], [f957, f1584, f2720])).
fof(f957, plain, (~ (e20 = op2(e22, e22)) | ~ sP100), inference(cnf_transformation, [], [f265])).
fof(f265, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP100), inference(nnf_transformation, [], [f122])).
fof(f2723, plain, (~ spl144_254 | ~ spl144_95), inference(avatar_split_clause, [], [f959, f1626, f2720])).
fof(f959, plain, (~ (e22 = op2(e22, e20)) | ~ sP100), inference(cnf_transformation, [], [f265])).
fof(f2717, plain, (~ spl144_253 | ~ spl144_85), inference(avatar_split_clause, [], [f953, f1584, f2712])).
fof(f953, plain, (~ (e20 = op2(e22, e22)) | ~ sP101), inference(cnf_transformation, [], [f264])).
fof(f264, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP101), inference(nnf_transformation, [], [f123])).
fof(f2715, plain, (~ spl144_253 | ~ spl144_80), inference(avatar_split_clause, [], [f955, f1562, f2712])).
fof(f955, plain, (~ (e23 = op2(e23, e20)) | ~ sP101), inference(cnf_transformation, [], [f264])).
fof(f2710, plain, (~ spl144_252 | spl144_91), inference(avatar_split_clause, [], [f948, f1609, f2704])).
fof(f948, plain, ((e22 = op2(e22, e21)) | ~ sP102), inference(cnf_transformation, [], [f263])).
fof(f263, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP102), inference(nnf_transformation, [], [f124])).
fof(f2702, plain, (~ spl144_251 | spl144_91), inference(avatar_split_clause, [], [f944, f1609, f2696])).
fof(f944, plain, ((e22 = op2(e22, e21)) | ~ sP103), inference(cnf_transformation, [], [f262])).
fof(f262, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP103), inference(nnf_transformation, [], [f125])).
fof(f2694, plain, (~ spl144_250 | spl144_91), inference(avatar_split_clause, [], [f940, f1609, f2688])).
fof(f940, plain, ((e22 = op2(e22, e21)) | ~ sP104), inference(cnf_transformation, [], [f261])).
fof(f261, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP104), inference(nnf_transformation, [], [f126])).
fof(f2686, plain, (~ spl144_249 | spl144_91), inference(avatar_split_clause, [], [f936, f1609, f2680])).
fof(f936, plain, ((e22 = op2(e22, e21)) | ~ sP105), inference(cnf_transformation, [], [f260])).
fof(f260, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP105), inference(nnf_transformation, [], [f127])).
fof(f2676, plain, (~ spl144_248 | spl144_127), inference(avatar_split_clause, [], [f934, f1762, f2672])).
fof(f934, plain, ((op2(e20, e20) = e22) | ~ sP106), inference(cnf_transformation, [], [f259])).
fof(f259, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP106), inference(nnf_transformation, [], [f128])).
fof(f2667, plain, (~ spl144_247 | ~ spl144_102), inference(avatar_split_clause, [], [f931, f1656, f2664])).
fof(f931, plain, (~ (e21 = op2(e21, e22)) | ~ sP107), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP107), inference(nnf_transformation, [], [f129])).
fof(f2662, plain, (~ spl144_246 | spl144_87), inference(avatar_split_clause, [], [f924, f1592, f2656])).
fof(f924, plain, ((e22 = op2(e22, e22)) | ~ sP108), inference(cnf_transformation, [], [f257])).
fof(f257, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP108), inference(nnf_transformation, [], [f130])).
fof(f2652, plain, (~ spl144_245 | spl144_67), inference(avatar_split_clause, [], [f922, f1507, f2648])).
fof(f922, plain, ((e22 = op2(e23, e23)) | ~ sP109), inference(cnf_transformation, [], [f256])).
fof(f256, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP109), inference(nnf_transformation, [], [f131])).
fof(f2646, plain, (~ spl144_244 | spl144_83), inference(avatar_split_clause, [], [f916, f1575, f2640])).
fof(f916, plain, ((e22 = op2(e22, e23)) | ~ sP110), inference(cnf_transformation, [], [f255])).
fof(f255, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP110), inference(nnf_transformation, [], [f132])).
fof(f2644, plain, (~ spl144_244 | spl144_128), inference(avatar_split_clause, [], [f918, f1766, f2640])).
fof(f918, plain, ((op2(e20, e20) = e23) | ~ sP110), inference(cnf_transformation, [], [f255])).
fof(f2636, plain, (~ spl144_243 | spl144_108), inference(avatar_split_clause, [], [f914, f1681, f2632])).
fof(f914, plain, ((e23 = op2(e21, e21)) | ~ sP111), inference(cnf_transformation, [], [f254])).
fof(f254, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP111), inference(nnf_transformation, [], [f133])).
fof(f2628, plain, (~ spl144_242 | spl144_88), inference(avatar_split_clause, [], [f910, f1596, f2624])).
fof(f910, plain, ((e23 = op2(e22, e22)) | ~ sP112), inference(cnf_transformation, [], [f253])).
fof(f253, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP112), inference(nnf_transformation, [], [f134])).
fof(f2622, plain, (~ spl144_241 | spl144_83), inference(avatar_split_clause, [], [f904, f1575, f2616])).
fof(f904, plain, ((e22 = op2(e22, e23)) | ~ sP113), inference(cnf_transformation, [], [f252])).
fof(f252, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP113), inference(nnf_transformation, [], [f135])).
fof(f2613, plain, (~ spl144_240 | ~ spl144_65), inference(avatar_split_clause, [], [f901, f1499, f2608])).
fof(f901, plain, (~ (e20 = op2(e23, e23)) | ~ sP114), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP114), inference(nnf_transformation, [], [f136])).
fof(f2605, plain, (~ spl144_239 | ~ spl144_65), inference(avatar_split_clause, [], [f897, f1499, f2600])).
fof(f897, plain, (~ (e20 = op2(e23, e23)) | ~ sP115), inference(cnf_transformation, [], [f250])).
fof(f250, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP115), inference(nnf_transformation, [], [f137])).
fof(f2597, plain, (~ spl144_238 | ~ spl144_65), inference(avatar_split_clause, [], [f893, f1499, f2592])).
fof(f893, plain, (~ (e20 = op2(e23, e23)) | ~ sP116), inference(cnf_transformation, [], [f249])).
fof(f249, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP116), inference(nnf_transformation, [], [f138])).
fof(f2589, plain, (~ spl144_237 | ~ spl144_65), inference(avatar_split_clause, [], [f889, f1499, f2584])).
fof(f889, plain, (~ (e20 = op2(e23, e23)) | ~ sP117), inference(cnf_transformation, [], [f248])).
fof(f248, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP117), inference(nnf_transformation, [], [f139])).
fof(f2582, plain, (~ spl144_236 | spl144_76), inference(avatar_split_clause, [], [f884, f1545, f2576])).
fof(f884, plain, ((e23 = op2(e23, e21)) | ~ sP118), inference(cnf_transformation, [], [f247])).
fof(f247, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP118), inference(nnf_transformation, [], [f140])).
fof(f2574, plain, (~ spl144_235 | spl144_76), inference(avatar_split_clause, [], [f880, f1545, f2568])).
fof(f880, plain, ((e23 = op2(e23, e21)) | ~ sP119), inference(cnf_transformation, [], [f246])).
fof(f246, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP119), inference(nnf_transformation, [], [f141])).
fof(f2566, plain, (~ spl144_234 | spl144_76), inference(avatar_split_clause, [], [f876, f1545, f2560])).
fof(f876, plain, ((e23 = op2(e23, e21)) | ~ sP120), inference(cnf_transformation, [], [f245])).
fof(f245, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP120), inference(nnf_transformation, [], [f142])).
fof(f2558, plain, (~ spl144_233 | spl144_76), inference(avatar_split_clause, [], [f872, f1545, f2552])).
fof(f872, plain, ((e23 = op2(e23, e21)) | ~ sP121), inference(cnf_transformation, [], [f244])).
fof(f244, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP121), inference(nnf_transformation, [], [f143])).
fof(f2550, plain, (~ spl144_232 | spl144_72), inference(avatar_split_clause, [], [f868, f1528, f2544])).
fof(f868, plain, ((e23 = op2(e23, e22)) | ~ sP122), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP122), inference(nnf_transformation, [], [f144])).
fof(f2548, plain, (~ spl144_232 | spl144_127), inference(avatar_split_clause, [], [f870, f1762, f2544])).
fof(f870, plain, ((op2(e20, e20) = e22) | ~ sP122), inference(cnf_transformation, [], [f243])).
fof(f2542, plain, (~ spl144_231 | spl144_72), inference(avatar_split_clause, [], [f864, f1528, f2536])).
fof(f864, plain, ((e23 = op2(e23, e22)) | ~ sP123), inference(cnf_transformation, [], [f242])).
fof(f242, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP123), inference(nnf_transformation, [], [f145])).
fof(f2539, plain, (~ spl144_231 | ~ spl144_102), inference(avatar_split_clause, [], [f867, f1656, f2536])).
fof(f867, plain, (~ (e21 = op2(e21, e22)) | ~ sP123), inference(cnf_transformation, [], [f242])).
fof(f2534, plain, (~ spl144_230 | spl144_72), inference(avatar_split_clause, [], [f860, f1528, f2528])).
fof(f860, plain, ((e23 = op2(e23, e22)) | ~ sP124), inference(cnf_transformation, [], [f241])).
fof(f241, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP124), inference(nnf_transformation, [], [f146])).
fof(f2532, plain, (~ spl144_230 | spl144_87), inference(avatar_split_clause, [], [f862, f1592, f2528])).
fof(f862, plain, ((e22 = op2(e22, e22)) | ~ sP124), inference(cnf_transformation, [], [f241])).
fof(f2524, plain, (~ spl144_229 | spl144_67), inference(avatar_split_clause, [], [f858, f1507, f2520])).
fof(f858, plain, ((e22 = op2(e23, e23)) | ~ sP125), inference(cnf_transformation, [], [f240])).
fof(f240, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP125), inference(nnf_transformation, [], [f147])).
fof(f2516, plain, (~ spl144_228 | spl144_128), inference(avatar_split_clause, [], [f854, f1766, f2512])).
fof(f854, plain, ((op2(e20, e20) = e23) | ~ sP126), inference(cnf_transformation, [], [f239])).
fof(f239, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP126), inference(nnf_transformation, [], [f148])).
fof(f2508, plain, (~ spl144_227 | spl144_108), inference(avatar_split_clause, [], [f850, f1681, f2504])).
fof(f850, plain, ((e23 = op2(e21, e21)) | ~ sP127), inference(cnf_transformation, [], [f238])).
fof(f238, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP127), inference(nnf_transformation, [], [f149])).
fof(f2500, plain, (~ spl144_226 | spl144_88), inference(avatar_split_clause, [], [f846, f1596, f2496])).
fof(f846, plain, ((e23 = op2(e22, e22)) | ~ sP128), inference(cnf_transformation, [], [f237])).
fof(f237, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP128), inference(nnf_transformation, [], [f150])).
fof(f2494, plain, (~ spl144_221 | spl144_225), inference(avatar_split_clause, [], [f836, f2491, f2469])).
fof(f836, plain, ((e20 = op2(e20, op2(e20, e20))) | ~ sP129), inference(cnf_transformation, [], [f236])).
fof(f236, plain, ((((e20 = op2(e23, e23)) | ~ (e23 = op2(e23, e20))) & ((e20 = op2(e22, e22)) | ~ (e22 = op2(e22, e20))) & ((e20 = op2(e21, e21)) | ~ (e21 = op2(e21, e20))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e23 = op2(e20, op2(e20, e23))) & (e22 = op2(e20, op2(e20, e22))) & (e21 = op2(e20, op2(e20, e21))) & (e20 = op2(e20, op2(e20, e20)))) | ~ sP129), inference(nnf_transformation, [], [f151])).
fof(f2489, plain, (~ spl144_221 | spl144_224), inference(avatar_split_clause, [], [f837, f2486, f2469])).
fof(f837, plain, ((e21 = op2(e20, op2(e20, e21))) | ~ sP129), inference(cnf_transformation, [], [f236])).
fof(f2479, plain, (~ spl144_221 | spl144_222), inference(avatar_split_clause, [], [f839, f2476, f2469])).
fof(f839, plain, ((e23 = op2(e20, op2(e20, e23))) | ~ sP129), inference(cnf_transformation, [], [f236])).
fof(f2474, plain, (~ spl144_221 | ~ spl144_110 | spl144_105), inference(avatar_split_clause, [], [f841, f1669, f1690, f2469])).
fof(f841, plain, ((e20 = op2(e21, e21)) | ~ (e21 = op2(e21, e20)) | ~ sP129), inference(cnf_transformation, [], [f236])).
fof(f2473, plain, (~ spl144_221 | ~ spl144_95 | spl144_85), inference(avatar_split_clause, [], [f842, f1584, f1626, f2469])).
fof(f842, plain, ((e20 = op2(e22, e22)) | ~ (e22 = op2(e22, e20)) | ~ sP129), inference(cnf_transformation, [], [f236])).
fof(f2467, plain, (~ spl144_216 | spl144_220), inference(avatar_split_clause, [], [f828, f2464, f2442])).
fof(f828, plain, ((e20 = op2(e21, op2(e21, e20))) | ~ sP130), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ((((e21 = op2(e23, e23)) | ~ (e23 = op2(e23, e21))) & ((e21 = op2(e22, e22)) | ~ (e22 = op2(e22, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((op2(e20, e20) = e21) | ~ (e20 = op2(e20, e21))) & (e23 = op2(e21, op2(e21, e23))) & (e22 = op2(e21, op2(e21, e22))) & (e21 = op2(e21, op2(e21, e21))) & (e20 = op2(e21, op2(e21, e20)))) | ~ sP130), inference(nnf_transformation, [], [f152])).
fof(f2462, plain, (~ spl144_216 | spl144_219), inference(avatar_split_clause, [], [f829, f2459, f2442])).
fof(f829, plain, ((e21 = op2(e21, op2(e21, e21))) | ~ sP130), inference(cnf_transformation, [], [f235])).
fof(f2457, plain, (~ spl144_216 | spl144_218), inference(avatar_split_clause, [], [f830, f2454, f2442])).
fof(f830, plain, ((e22 = op2(e21, op2(e21, e22))) | ~ sP130), inference(cnf_transformation, [], [f235])).
fof(f2452, plain, (~ spl144_216 | spl144_217), inference(avatar_split_clause, [], [f831, f2449, f2442])).
fof(f831, plain, ((e23 = op2(e21, op2(e21, e23))) | ~ sP130), inference(cnf_transformation, [], [f235])).
fof(f2447, plain, (~ spl144_216 | ~ spl144_121 | spl144_126), inference(avatar_split_clause, [], [f832, f1758, f1737, f2442])).
fof(f832, plain, ((op2(e20, e20) = e21) | ~ (e20 = op2(e20, e21)) | ~ sP130), inference(cnf_transformation, [], [f235])).
fof(f2435, plain, (~ spl144_211 | spl144_214), inference(avatar_split_clause, [], [f821, f2432, f2415])).
fof(f821, plain, ((e21 = op2(e22, op2(e22, e21))) | ~ sP131), inference(cnf_transformation, [], [f234])).
fof(f234, plain, ((((e22 = op2(e23, e23)) | ~ (e23 = op2(e23, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e21, e21)) | ~ (e21 = op2(e21, e22))) & ((op2(e20, e20) = e22) | ~ (e20 = op2(e20, e22))) & (e23 = op2(e22, op2(e22, e23))) & (e22 = op2(e22, op2(e22, e22))) & (e21 = op2(e22, op2(e22, e21))) & (e20 = op2(e22, op2(e22, e20)))) | ~ sP131), inference(nnf_transformation, [], [f153])).
fof(f2430, plain, (~ spl144_211 | spl144_213), inference(avatar_split_clause, [], [f822, f2427, f2415])).
fof(f822, plain, ((e22 = op2(e22, op2(e22, e22))) | ~ sP131), inference(cnf_transformation, [], [f234])).
fof(f2418, plain, (~ spl144_211 | ~ spl144_72 | spl144_67), inference(avatar_split_clause, [], [f827, f1507, f1528, f2415])).
fof(f827, plain, ((e22 = op2(e23, e23)) | ~ (e23 = op2(e23, e22)) | ~ sP131), inference(cnf_transformation, [], [f234])).
fof(f2413, plain, (spl144_206 | spl144_205 | spl144_204 | spl144_203 | spl144_202 | spl144_201 | spl144_200 | spl144_199 | spl144_198 | spl144_197 | spl144_196 | spl144_195 | spl144_194 | spl144_193 | spl144_192 | spl144_191 | spl144_190 | spl144_189 | spl144_188 | spl144_187 | spl144_186 | spl144_185 | spl144_184 | spl144_183 | spl144_182 | spl144_181 | spl144_180 | spl144_179 | spl144_178 | spl144_177 | spl144_176 | spl144_175 | spl144_174 | spl144_173 | spl144_172 | spl144_171 | spl144_170 | spl144_169 | spl144_168 | spl144_167 | spl144_166 | spl144_165 | spl144_164 | spl144_163 | spl144_162 | spl144_161 | spl144_160 | spl144_159 | spl144_158 | spl144_157 | spl144_156 | spl144_155 | spl144_154 | spl144_153 | spl144_152 | spl144_151 | spl144_150 | spl144_149 | spl144_148 | spl144_147 | spl144_146 | spl144_145 | spl144_144 | spl144_4), inference(avatar_split_clause, [], [f808, f1207, f1884, f1892, f1900, f1908, f1916, f1924, f1932, f1940, f1948, f1956, f1964, f1972, f1980, f1988, f1996, f2004, f2012, f2020, f2028, f2036, f2044, f2052, f2060, f2068, f2076, f2084, f2092, f2100, f2108, f2116, f2124, f2132, f2140, f2148, f2156, f2164, f2172, f2180, f2188, f2196, f2204, f2212, f2220, f2228, f2236, f2244, f2252, f2260, f2268, f2276, f2284, f2292, f2300, f2308, f2316, f2324, f2332, f2340, f2348, f2356, f2364, f2372, f2380])).
fof(f2380, plain, (spl144_206 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl144_206])])).
fof(f2372, plain, (spl144_205 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl144_205])])).
fof(f2364, plain, (spl144_204 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl144_204])])).
fof(f2356, plain, (spl144_203 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl144_203])])).
fof(f2348, plain, (spl144_202 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl144_202])])).
fof(f2340, plain, (spl144_201 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl144_201])])).
fof(f2332, plain, (spl144_200 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl144_200])])).
fof(f2324, plain, (spl144_199 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl144_199])])).
fof(f2316, plain, (spl144_198 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl144_198])])).
fof(f2308, plain, (spl144_197 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl144_197])])).
fof(f2300, plain, (spl144_196 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl144_196])])).
fof(f2292, plain, (spl144_195 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl144_195])])).
fof(f2284, plain, (spl144_194 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl144_194])])).
fof(f2276, plain, (spl144_193 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl144_193])])).
fof(f2268, plain, (spl144_192 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl144_192])])).
fof(f2260, plain, (spl144_191 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl144_191])])).
fof(f2252, plain, (spl144_190 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl144_190])])).
fof(f2244, plain, (spl144_189 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl144_189])])).
fof(f2236, plain, (spl144_188 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl144_188])])).
fof(f2228, plain, (spl144_187 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl144_187])])).
fof(f2220, plain, (spl144_186 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl144_186])])).
fof(f2212, plain, (spl144_185 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl144_185])])).
fof(f2204, plain, (spl144_184 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl144_184])])).
fof(f2196, plain, (spl144_183 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl144_183])])).
fof(f2188, plain, (spl144_182 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl144_182])])).
fof(f2180, plain, (spl144_181 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl144_181])])).
fof(f2172, plain, (spl144_180 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl144_180])])).
fof(f2164, plain, (spl144_179 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl144_179])])).
fof(f2156, plain, (spl144_178 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl144_178])])).
fof(f2148, plain, (spl144_177 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl144_177])])).
fof(f2140, plain, (spl144_176 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl144_176])])).
fof(f2132, plain, (spl144_175 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl144_175])])).
fof(f2124, plain, (spl144_174 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl144_174])])).
fof(f2116, plain, (spl144_173 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl144_173])])).
fof(f2108, plain, (spl144_172 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl144_172])])).
fof(f2100, plain, (spl144_171 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl144_171])])).
fof(f2092, plain, (spl144_170 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl144_170])])).
fof(f2084, plain, (spl144_169 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl144_169])])).
fof(f2076, plain, (spl144_168 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl144_168])])).
fof(f2068, plain, (spl144_167 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl144_167])])).
fof(f2060, plain, (spl144_166 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl144_166])])).
fof(f2052, plain, (spl144_165 <=> sP41), introduced(avatar_definition, [new_symbols(naming, [spl144_165])])).
fof(f2044, plain, (spl144_164 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl144_164])])).
fof(f2036, plain, (spl144_163 <=> sP43), introduced(avatar_definition, [new_symbols(naming, [spl144_163])])).
fof(f2028, plain, (spl144_162 <=> sP44), introduced(avatar_definition, [new_symbols(naming, [spl144_162])])).
fof(f2020, plain, (spl144_161 <=> sP45), introduced(avatar_definition, [new_symbols(naming, [spl144_161])])).
fof(f2012, plain, (spl144_160 <=> sP46), introduced(avatar_definition, [new_symbols(naming, [spl144_160])])).
fof(f2004, plain, (spl144_159 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl144_159])])).
fof(f1996, plain, (spl144_158 <=> sP48), introduced(avatar_definition, [new_symbols(naming, [spl144_158])])).
fof(f1988, plain, (spl144_157 <=> sP49), introduced(avatar_definition, [new_symbols(naming, [spl144_157])])).
fof(f1980, plain, (spl144_156 <=> sP50), introduced(avatar_definition, [new_symbols(naming, [spl144_156])])).
fof(f1972, plain, (spl144_155 <=> sP51), introduced(avatar_definition, [new_symbols(naming, [spl144_155])])).
fof(f1964, plain, (spl144_154 <=> sP52), introduced(avatar_definition, [new_symbols(naming, [spl144_154])])).
fof(f1956, plain, (spl144_153 <=> sP53), introduced(avatar_definition, [new_symbols(naming, [spl144_153])])).
fof(f1948, plain, (spl144_152 <=> sP54), introduced(avatar_definition, [new_symbols(naming, [spl144_152])])).
fof(f1940, plain, (spl144_151 <=> sP55), introduced(avatar_definition, [new_symbols(naming, [spl144_151])])).
fof(f1932, plain, (spl144_150 <=> sP56), introduced(avatar_definition, [new_symbols(naming, [spl144_150])])).
fof(f1924, plain, (spl144_149 <=> sP57), introduced(avatar_definition, [new_symbols(naming, [spl144_149])])).
fof(f1916, plain, (spl144_148 <=> sP58), introduced(avatar_definition, [new_symbols(naming, [spl144_148])])).
fof(f1908, plain, (spl144_147 <=> sP59), introduced(avatar_definition, [new_symbols(naming, [spl144_147])])).
fof(f1900, plain, (spl144_146 <=> sP60), introduced(avatar_definition, [new_symbols(naming, [spl144_146])])).
fof(f1892, plain, (spl144_145 <=> sP61), introduced(avatar_definition, [new_symbols(naming, [spl144_145])])).
fof(f1884, plain, (spl144_144 <=> sP62), introduced(avatar_definition, [new_symbols(naming, [spl144_144])])).
fof(f808, plain, ((e13 = op1(e13, e13)) | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e13 = op1(e12, e12)) | ~ (e12 = op1(e12, e13))) & ((e13 = op1(e11, e11)) | ~ (e11 = op1(e11, e13))) & ((op1(e10, e10) = e13) | ~ (e10 = op1(e10, e13))) & (e13 = op1(e13, op1(e13, e13))) & (e12 = op1(e13, op1(e13, e12))) & (e11 = op1(e13, op1(e13, e11))) & (e10 = op1(e13, op1(e13, e10)))) | sP65 | sP64 | sP63) & ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f10, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f36, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP15), inference(usedef, [], [e36])).
fof(e36, plain, (sP15 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f37, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP16), inference(usedef, [], [e37])).
fof(e37, plain, (sP16 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f38, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP17), inference(usedef, [], [e38])).
fof(e38, plain, (sP17 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f39, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP18), inference(usedef, [], [e39])).
fof(e39, plain, (sP18 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f40, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP19), inference(usedef, [], [e40])).
fof(e40, plain, (sP19 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f41, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP20), inference(usedef, [], [e41])).
fof(e41, plain, (sP20 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f42, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP21), inference(usedef, [], [e42])).
fof(e42, plain, (sP21 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f43, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP22), inference(usedef, [], [e43])).
fof(e43, plain, (sP22 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f44, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP23), inference(usedef, [], [e44])).
fof(e44, plain, (sP23 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f45, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP24), inference(usedef, [], [e45])).
fof(e45, plain, (sP24 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f46, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP25), inference(usedef, [], [e46])).
fof(e46, plain, (sP25 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f47, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP26), inference(usedef, [], [e47])).
fof(e47, plain, (sP26 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f48, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP27), inference(usedef, [], [e48])).
fof(e48, plain, (sP27 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f49, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP28), inference(usedef, [], [e49])).
fof(e49, plain, (sP28 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f50, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP29), inference(usedef, [], [e50])).
fof(e50, plain, (sP29 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f51, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP30), inference(usedef, [], [e51])).
fof(e51, plain, (sP30 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f52, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP31), inference(usedef, [], [e52])).
fof(e52, plain, (sP31 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f53, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP32), inference(usedef, [], [e53])).
fof(e53, plain, (sP32 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f54, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP33), inference(usedef, [], [e54])).
fof(e54, plain, (sP33 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f55, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP34), inference(usedef, [], [e55])).
fof(e55, plain, (sP34 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f56, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP35), inference(usedef, [], [e56])).
fof(e56, plain, (sP35 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f57, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP36), inference(usedef, [], [e57])).
fof(e57, plain, (sP36 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f58, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP37), inference(usedef, [], [e58])).
fof(e58, plain, (sP37 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f59, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP38), inference(usedef, [], [e59])).
fof(e59, plain, (sP38 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f60, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP39), inference(usedef, [], [e60])).
fof(e60, plain, (sP39 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f61, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP40), inference(usedef, [], [e61])).
fof(e61, plain, (sP40 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f62, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP41), inference(usedef, [], [e62])).
fof(e62, plain, (sP41 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f63, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP42), inference(usedef, [], [e63])).
fof(e63, plain, (sP42 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f64, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP43), inference(usedef, [], [e64])).
fof(e64, plain, (sP43 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f65, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP44), inference(usedef, [], [e65])).
fof(e65, plain, (sP44 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f66, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP45), inference(usedef, [], [e66])).
fof(e66, plain, (sP45 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f67, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP46), inference(usedef, [], [e67])).
fof(e67, plain, (sP46 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f68, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP47), inference(usedef, [], [e68])).
fof(e68, plain, (sP47 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f69, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP48), inference(usedef, [], [e69])).
fof(e69, plain, (sP48 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP48])])).
fof(f70, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP49), inference(usedef, [], [e70])).
fof(e70, plain, (sP49 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP49])])).
fof(f71, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP50), inference(usedef, [], [e71])).
fof(e71, plain, (sP50 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP50])])).
fof(f72, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP51), inference(usedef, [], [e72])).
fof(e72, plain, (sP51 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP51])])).
fof(f73, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP52), inference(usedef, [], [e73])).
fof(e73, plain, (sP52 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP52])])).
fof(f74, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP53), inference(usedef, [], [e74])).
fof(e74, plain, (sP53 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP53])])).
fof(f75, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP54), inference(usedef, [], [e75])).
fof(e75, plain, (sP54 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP54])])).
fof(f76, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP55), inference(usedef, [], [e76])).
fof(e76, plain, (sP55 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP55])])).
fof(f77, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP56), inference(usedef, [], [e77])).
fof(e77, plain, (sP56 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP56])])).
fof(f78, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP57), inference(usedef, [], [e78])).
fof(e78, plain, (sP57 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP57])])).
fof(f79, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP58), inference(usedef, [], [e79])).
fof(e79, plain, (sP58 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP58])])).
fof(f80, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP59), inference(usedef, [], [e80])).
fof(e80, plain, (sP59 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP59])])).
fof(f81, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP60), inference(usedef, [], [e81])).
fof(e81, plain, (sP60 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP60])])).
fof(f82, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP61), inference(usedef, [], [e82])).
fof(e82, plain, (sP61 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP61])])).
fof(f83, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP62), inference(usedef, [], [e83])).
fof(e83, plain, (sP62 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP62])])).
fof(f84, plain, ((((e10 = op1(e13, e13)) | ~ (e13 = op1(e13, e10))) & ((e10 = op1(e12, e12)) | ~ (e12 = op1(e12, e10))) & ((e10 = op1(e11, e11)) | ~ (e11 = op1(e11, e10))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, op1(e10, e13))) & (e12 = op1(e10, op1(e10, e12))) & (e11 = op1(e10, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10)))) | ~ sP63), inference(usedef, [], [e84])).
fof(e84, plain, (sP63 <=> (((e10 = op1(e13, e13)) | ~ (e13 = op1(e13, e10))) & ((e10 = op1(e12, e12)) | ~ (e12 = op1(e12, e10))) & ((e10 = op1(e11, e11)) | ~ (e11 = op1(e11, e10))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, op1(e10, e13))) & (e12 = op1(e10, op1(e10, e12))) & (e11 = op1(e10, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP63])])).
fof(f85, plain, ((((e11 = op1(e13, e13)) | ~ (e13 = op1(e13, e11))) & ((e11 = op1(e12, e12)) | ~ (e12 = op1(e12, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((op1(e10, e10) = e11) | ~ (e10 = op1(e10, e11))) & (e13 = op1(e11, op1(e11, e13))) & (e12 = op1(e11, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e11, op1(e11, e10)))) | ~ sP64), inference(usedef, [], [e85])).
fof(e85, plain, (sP64 <=> (((e11 = op1(e13, e13)) | ~ (e13 = op1(e13, e11))) & ((e11 = op1(e12, e12)) | ~ (e12 = op1(e12, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((op1(e10, e10) = e11) | ~ (e10 = op1(e10, e11))) & (e13 = op1(e11, op1(e11, e13))) & (e12 = op1(e11, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e11, op1(e11, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP64])])).
fof(f86, plain, ((((e12 = op1(e13, e13)) | ~ (e13 = op1(e13, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e11, e11)) | ~ (e11 = op1(e11, e12))) & ((op1(e10, e10) = e12) | ~ (e10 = op1(e10, e12))) & (e13 = op1(e12, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e12, op1(e12, e11))) & (e10 = op1(e12, op1(e12, e10)))) | ~ sP65), inference(usedef, [], [e86])).
fof(e86, plain, (sP65 <=> (((e12 = op1(e13, e13)) | ~ (e13 = op1(e13, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e11, e11)) | ~ (e11 = op1(e11, e12))) & ((op1(e10, e10) = e12) | ~ (e10 = op1(e10, e12))) & (e13 = op1(e12, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e12, op1(e12, e11))) & (e10 = op1(e12, op1(e12, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP65])])).
fof(f10, plain, (((((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e13 = op1(e12, e12)) | ~ (e12 = op1(e12, e13))) & ((e13 = op1(e11, e11)) | ~ (e11 = op1(e11, e13))) & ((op1(e10, e10) = e13) | ~ (e10 = op1(e10, e13))) & (e13 = op1(e13, op1(e13, e13))) & (e12 = op1(e13, op1(e13, e12))) & (e11 = op1(e13, op1(e13, e11))) & (e10 = op1(e13, op1(e13, e10)))) | (((e12 = op1(e13, e13)) | ~ (e13 = op1(e13, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e11, e11)) | ~ (e11 = op1(e11, e12))) & ((op1(e10, e10) = e12) | ~ (e10 = op1(e10, e12))) & (e13 = op1(e12, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e12, op1(e12, e11))) & (e10 = op1(e12, op1(e12, e10)))) | (((e11 = op1(e13, e13)) | ~ (e13 = op1(e13, e11))) & ((e11 = op1(e12, e12)) | ~ (e12 = op1(e12, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((op1(e10, e10) = e11) | ~ (e10 = op1(e10, e11))) & (e13 = op1(e11, op1(e11, e13))) & (e12 = op1(e11, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e11, op1(e11, e10)))) | (((e10 = op1(e13, e13)) | ~ (e13 = op1(e13, e10))) & ((e10 = op1(e12, e12)) | ~ (e12 = op1(e12, e10))) & ((e10 = op1(e11, e11)) | ~ (e11 = op1(e11, e10))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, op1(e10, e13))) & (e12 = op1(e10, op1(e10, e12))) & (e11 = op1(e10, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10))))) & ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax10)).
fof(f2409, plain, (spl144_139 | spl144_134 | spl144_129 | spl144_210), inference(avatar_split_clause, [], [f812, f2406, f1803, f1830, f1857])).
fof(f1857, plain, (spl144_139 <=> sP63), introduced(avatar_definition, [new_symbols(naming, [spl144_139])])).
fof(f1830, plain, (spl144_134 <=> sP64), introduced(avatar_definition, [new_symbols(naming, [spl144_134])])).
fof(f1803, plain, (spl144_129 <=> sP65), introduced(avatar_definition, [new_symbols(naming, [spl144_129])])).
fof(f812, plain, ((e10 = op1(e13, op1(e13, e10))) | sP65 | sP64 | sP63), inference(cnf_transformation, [], [f87])).
fof(f2399, plain, (spl144_139 | spl144_134 | spl144_129 | spl144_208), inference(avatar_split_clause, [], [f814, f2396, f1803, f1830, f1857])).
fof(f814, plain, ((e12 = op1(e13, op1(e13, e12))) | sP65 | sP64 | sP63), inference(cnf_transformation, [], [f87])).
fof(f2388, plain, (spl144_139 | spl144_134 | spl144_129 | ~ spl144_34 | spl144_44), inference(avatar_split_clause, [], [f817, f1377, f1335, f1803, f1830, f1857])).
fof(f817, plain, ((e13 = op1(e11, e11)) | ~ (e11 = op1(e11, e13)) | sP65 | sP64 | sP63), inference(cnf_transformation, [], [f87])).
fof(f2387, plain, (spl144_139 | spl144_134 | spl144_129 | ~ spl144_19 | spl144_24), inference(avatar_split_clause, [], [f818, f1292, f1271, f1803, f1830, f1857])).
fof(f818, plain, ((e13 = op1(e12, e12)) | ~ (e12 = op1(e12, e13)) | sP65 | sP64 | sP63), inference(cnf_transformation, [], [f87])).
fof(f2386, plain, (~ spl144_206 | spl144_61), inference(avatar_split_clause, [], [f804, f1450, f2380])).
fof(f804, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f233])).
fof(f233, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f2376, plain, (~ spl144_205 | spl144_41), inference(avatar_split_clause, [], [f802, f1365, f2372])).
fof(f802, plain, ((e10 = op1(e11, e11)) | ~ sP1), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f2370, plain, (~ spl144_204 | spl144_61), inference(avatar_split_clause, [], [f796, f1450, f2364])).
fof(f796, plain, ((e10 = op1(e10, e10)) | ~ sP2), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f2362, plain, (~ spl144_203 | spl144_61), inference(avatar_split_clause, [], [f792, f1450, f2356])).
fof(f792, plain, ((e10 = op1(e10, e10)) | ~ sP3), inference(cnf_transformation, [], [f230])).
fof(f230, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f2351, plain, (~ spl144_202 | ~ spl144_57), inference(avatar_split_clause, [], [f791, f1433, f2348])).
fof(f791, plain, (~ (e10 = op1(e10, e11)) | ~ sP4), inference(cnf_transformation, [], [f229])).
fof(f229, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f2344, plain, (~ spl144_201 | spl144_42), inference(avatar_split_clause, [], [f786, f1369, f2340])).
fof(f1369, plain, (spl144_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_42])])).
fof(f786, plain, ((e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f2343, plain, (~ spl144_201 | ~ spl144_42), inference(avatar_split_clause, [], [f787, f1369, f2340])).
fof(f787, plain, (~ (e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f2337, plain, (~ spl144_200 | ~ spl144_62), inference(avatar_split_clause, [], [f781, f1454, f2332])).
fof(f781, plain, (~ (op1(e10, e10) = e11) | ~ sP6), inference(cnf_transformation, [], [f227])).
fof(f227, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f2336, plain, (~ spl144_200 | spl144_22), inference(avatar_split_clause, [], [f782, f1284, f2332])).
fof(f782, plain, ((e11 = op1(e12, e12)) | ~ sP6), inference(cnf_transformation, [], [f227])).
fof(f2328, plain, (~ spl144_199 | spl144_2), inference(avatar_split_clause, [], [f778, f1199, f2324])).
fof(f778, plain, ((e11 = op1(e13, e13)) | ~ sP7), inference(cnf_transformation, [], [f226])).
fof(f226, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f2322, plain, (~ spl144_198 | spl144_53), inference(avatar_split_clause, [], [f772, f1416, f2316])).
fof(f772, plain, ((e10 = op1(e10, e12)) | ~ sP8), inference(cnf_transformation, [], [f225])).
fof(f225, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f2314, plain, (~ spl144_197 | spl144_53), inference(avatar_split_clause, [], [f768, f1416, f2308])).
fof(f768, plain, ((e10 = op1(e10, e12)) | ~ sP9), inference(cnf_transformation, [], [f224])).
fof(f224, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f2306, plain, (~ spl144_196 | spl144_53), inference(avatar_split_clause, [], [f764, f1416, f2300])).
fof(f764, plain, ((e10 = op1(e10, e12)) | ~ sP10), inference(cnf_transformation, [], [f223])).
fof(f223, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f2296, plain, (~ spl144_195 | spl144_3), inference(avatar_split_clause, [], [f762, f1203, f2292])).
fof(f762, plain, ((e12 = op1(e13, e13)) | ~ sP11), inference(cnf_transformation, [], [f222])).
fof(f222, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f2290, plain, (~ spl144_194 | spl144_49), inference(avatar_split_clause, [], [f756, f1399, f2284])).
fof(f756, plain, ((e10 = op1(e10, e13)) | ~ sP12), inference(cnf_transformation, [], [f221])).
fof(f221, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f2280, plain, (~ spl144_193 | spl144_44), inference(avatar_split_clause, [], [f754, f1377, f2276])).
fof(f754, plain, ((e13 = op1(e11, e11)) | ~ sP13), inference(cnf_transformation, [], [f220])).
fof(f220, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f2272, plain, (~ spl144_192 | spl144_24), inference(avatar_split_clause, [], [f750, f1292, f2268])).
fof(f750, plain, ((e13 = op1(e12, e12)) | ~ sP14), inference(cnf_transformation, [], [f219])).
fof(f219, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f2264, plain, (~ spl144_191 | spl144_4), inference(avatar_split_clause, [], [f746, f1207, f2260])).
fof(f746, plain, ((e13 = op1(e13, e13)) | ~ sP15), inference(cnf_transformation, [], [f218])).
fof(f218, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP15), inference(nnf_transformation, [], [f36])).
fof(f2256, plain, (~ spl144_190 | spl144_61), inference(avatar_split_clause, [], [f742, f1450, f2252])).
fof(f742, plain, ((e10 = op1(e10, e10)) | ~ sP16), inference(cnf_transformation, [], [f217])).
fof(f217, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP16), inference(nnf_transformation, [], [f37])).
fof(f2248, plain, (~ spl144_189 | spl144_41), inference(avatar_split_clause, [], [f738, f1365, f2244])).
fof(f738, plain, ((e10 = op1(e11, e11)) | ~ sP17), inference(cnf_transformation, [], [f216])).
fof(f216, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP17), inference(nnf_transformation, [], [f38])).
fof(f2242, plain, (~ spl144_188 | spl144_46), inference(avatar_split_clause, [], [f732, f1386, f2236])).
fof(f732, plain, ((e11 = op1(e11, e10)) | ~ sP18), inference(cnf_transformation, [], [f215])).
fof(f215, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP18), inference(nnf_transformation, [], [f39])).
fof(f2240, plain, (~ spl144_188 | spl144_21), inference(avatar_split_clause, [], [f734, f1280, f2236])).
fof(f734, plain, ((e10 = op1(e12, e12)) | ~ sP18), inference(cnf_transformation, [], [f215])).
fof(f2234, plain, (~ spl144_187 | spl144_46), inference(avatar_split_clause, [], [f728, f1386, f2228])).
fof(f728, plain, ((e11 = op1(e11, e10)) | ~ sP19), inference(cnf_transformation, [], [f214])).
fof(f214, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP19), inference(nnf_transformation, [], [f40])).
fof(f2231, plain, (~ spl144_187 | ~ spl144_16), inference(avatar_split_clause, [], [f731, f1258, f2228])).
fof(f1258, plain, (spl144_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_16])])).
fof(f731, plain, (~ (e13 = op1(e13, e10)) | ~ sP19), inference(cnf_transformation, [], [f214])).
fof(f2223, plain, (~ spl144_186 | ~ spl144_57), inference(avatar_split_clause, [], [f727, f1433, f2220])).
fof(f727, plain, (~ (e10 = op1(e10, e11)) | ~ sP20), inference(cnf_transformation, [], [f213])).
fof(f213, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP20), inference(nnf_transformation, [], [f41])).
fof(f2218, plain, (~ spl144_185 | spl144_42), inference(avatar_split_clause, [], [f720, f1369, f2212])).
fof(f720, plain, ((e11 = op1(e11, e11)) | ~ sP21), inference(cnf_transformation, [], [f212])).
fof(f212, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP21), inference(nnf_transformation, [], [f42])).
fof(f2217, plain, (~ spl144_185 | ~ spl144_42), inference(avatar_split_clause, [], [f721, f1369, f2212])).
fof(f721, plain, (~ (e11 = op1(e11, e11)) | ~ sP21), inference(cnf_transformation, [], [f212])).
fof(f2210, plain, (~ spl144_184 | spl144_42), inference(avatar_split_clause, [], [f716, f1369, f2204])).
fof(f716, plain, ((e11 = op1(e11, e11)) | ~ sP22), inference(cnf_transformation, [], [f211])).
fof(f211, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP22), inference(nnf_transformation, [], [f43])).
fof(f2209, plain, (~ spl144_184 | ~ spl144_42), inference(avatar_split_clause, [], [f717, f1369, f2204])).
fof(f717, plain, (~ (e11 = op1(e11, e11)) | ~ sP22), inference(cnf_transformation, [], [f211])).
fof(f2200, plain, (~ spl144_183 | spl144_2), inference(avatar_split_clause, [], [f714, f1199, f2196])).
fof(f714, plain, ((e11 = op1(e13, e13)) | ~ sP23), inference(cnf_transformation, [], [f210])).
fof(f210, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP23), inference(nnf_transformation, [], [f44])).
fof(f2193, plain, (~ spl144_182 | ~ spl144_43), inference(avatar_split_clause, [], [f709, f1373, f2188])).
fof(f709, plain, (~ (e12 = op1(e11, e11)) | ~ sP24), inference(cnf_transformation, [], [f209])).
fof(f209, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP24), inference(nnf_transformation, [], [f45])).
fof(f2185, plain, (~ spl144_181 | ~ spl144_43), inference(avatar_split_clause, [], [f705, f1373, f2180])).
fof(f705, plain, (~ (e12 = op1(e11, e11)) | ~ sP25), inference(cnf_transformation, [], [f208])).
fof(f208, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP25), inference(nnf_transformation, [], [f46])).
fof(f2177, plain, (~ spl144_180 | ~ spl144_43), inference(avatar_split_clause, [], [f701, f1373, f2172])).
fof(f701, plain, (~ (e12 = op1(e11, e11)) | ~ sP26), inference(cnf_transformation, [], [f207])).
fof(f207, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP26), inference(nnf_transformation, [], [f47])).
fof(f2169, plain, (~ spl144_179 | ~ spl144_43), inference(avatar_split_clause, [], [f697, f1373, f2164])).
fof(f697, plain, (~ (e12 = op1(e11, e11)) | ~ sP27), inference(cnf_transformation, [], [f206])).
fof(f206, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP27), inference(nnf_transformation, [], [f48])).
fof(f2162, plain, (~ spl144_178 | spl144_34), inference(avatar_split_clause, [], [f692, f1335, f2156])).
fof(f692, plain, ((e11 = op1(e11, e13)) | ~ sP28), inference(cnf_transformation, [], [f205])).
fof(f205, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP28), inference(nnf_transformation, [], [f49])).
fof(f2160, plain, (~ spl144_178 | spl144_64), inference(avatar_split_clause, [], [f694, f1462, f2156])).
fof(f694, plain, ((op1(e10, e10) = e13) | ~ sP28), inference(cnf_transformation, [], [f205])).
fof(f2152, plain, (~ spl144_177 | spl144_44), inference(avatar_split_clause, [], [f690, f1377, f2148])).
fof(f690, plain, ((e13 = op1(e11, e11)) | ~ sP29), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP29), inference(nnf_transformation, [], [f50])).
fof(f2144, plain, (~ spl144_176 | spl144_24), inference(avatar_split_clause, [], [f686, f1292, f2140])).
fof(f686, plain, ((e13 = op1(e12, e12)) | ~ sP30), inference(cnf_transformation, [], [f203])).
fof(f203, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP30), inference(nnf_transformation, [], [f51])).
fof(f2136, plain, (~ spl144_175 | spl144_4), inference(avatar_split_clause, [], [f682, f1207, f2132])).
fof(f682, plain, ((e13 = op1(e13, e13)) | ~ sP31), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP31), inference(nnf_transformation, [], [f52])).
fof(f2128, plain, (~ spl144_174 | spl144_61), inference(avatar_split_clause, [], [f678, f1450, f2124])).
fof(f678, plain, ((e10 = op1(e10, e10)) | ~ sP32), inference(cnf_transformation, [], [f201])).
fof(f201, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP32), inference(nnf_transformation, [], [f53])).
fof(f2120, plain, (~ spl144_173 | spl144_41), inference(avatar_split_clause, [], [f674, f1365, f2116])).
fof(f674, plain, ((e10 = op1(e11, e11)) | ~ sP33), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP33), inference(nnf_transformation, [], [f54])).
fof(f2114, plain, (~ spl144_172 | spl144_31), inference(avatar_split_clause, [], [f668, f1322, f2108])).
fof(f668, plain, ((e12 = op1(e12, e10)) | ~ sP34), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP34), inference(nnf_transformation, [], [f55])).
fof(f2113, plain, (~ spl144_172 | ~ spl144_21), inference(avatar_split_clause, [], [f669, f1280, f2108])).
fof(f669, plain, (~ (e10 = op1(e12, e12)) | ~ sP34), inference(cnf_transformation, [], [f199])).
fof(f2112, plain, (~ spl144_172 | spl144_21), inference(avatar_split_clause, [], [f670, f1280, f2108])).
fof(f670, plain, ((e10 = op1(e12, e12)) | ~ sP34), inference(cnf_transformation, [], [f199])).
fof(f2106, plain, (~ spl144_171 | spl144_31), inference(avatar_split_clause, [], [f664, f1322, f2100])).
fof(f664, plain, ((e12 = op1(e12, e10)) | ~ sP35), inference(cnf_transformation, [], [f198])).
fof(f198, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP35), inference(nnf_transformation, [], [f56])).
fof(f2103, plain, (~ spl144_171 | ~ spl144_16), inference(avatar_split_clause, [], [f667, f1258, f2100])).
fof(f667, plain, (~ (e13 = op1(e13, e10)) | ~ sP35), inference(cnf_transformation, [], [f198])).
fof(f2098, plain, (~ spl144_170 | spl144_27), inference(avatar_split_clause, [], [f660, f1305, f2092])).
fof(f660, plain, ((e12 = op1(e12, e11)) | ~ sP36), inference(cnf_transformation, [], [f197])).
fof(f197, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP36), inference(nnf_transformation, [], [f57])).
fof(f2090, plain, (~ spl144_169 | spl144_27), inference(avatar_split_clause, [], [f656, f1305, f2084])).
fof(f656, plain, ((e12 = op1(e12, e11)) | ~ sP37), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP37), inference(nnf_transformation, [], [f58])).
fof(f2082, plain, (~ spl144_168 | spl144_27), inference(avatar_split_clause, [], [f652, f1305, f2076])).
fof(f652, plain, ((e12 = op1(e12, e11)) | ~ sP38), inference(cnf_transformation, [], [f195])).
fof(f195, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP38), inference(nnf_transformation, [], [f59])).
fof(f2074, plain, (~ spl144_167 | spl144_27), inference(avatar_split_clause, [], [f648, f1305, f2068])).
fof(f648, plain, ((e12 = op1(e12, e11)) | ~ sP39), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP39), inference(nnf_transformation, [], [f60])).
fof(f2066, plain, (~ spl144_166 | spl144_23), inference(avatar_split_clause, [], [f644, f1288, f2060])).
fof(f644, plain, ((e12 = op1(e12, e12)) | ~ sP40), inference(cnf_transformation, [], [f193])).
fof(f193, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP40), inference(nnf_transformation, [], [f61])).
fof(f2065, plain, (~ spl144_166 | ~ spl144_23), inference(avatar_split_clause, [], [f645, f1288, f2060])).
fof(f645, plain, (~ (e12 = op1(e12, e12)) | ~ sP40), inference(cnf_transformation, [], [f193])).
fof(f2058, plain, (~ spl144_165 | spl144_23), inference(avatar_split_clause, [], [f640, f1288, f2052])).
fof(f640, plain, ((e12 = op1(e12, e12)) | ~ sP41), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP41), inference(nnf_transformation, [], [f62])).
fof(f2057, plain, (~ spl144_165 | ~ spl144_23), inference(avatar_split_clause, [], [f641, f1288, f2052])).
fof(f641, plain, (~ (e12 = op1(e12, e12)) | ~ sP41), inference(cnf_transformation, [], [f192])).
fof(f2050, plain, (~ spl144_164 | spl144_23), inference(avatar_split_clause, [], [f636, f1288, f2044])).
fof(f636, plain, ((e12 = op1(e12, e12)) | ~ sP42), inference(cnf_transformation, [], [f191])).
fof(f191, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP42), inference(nnf_transformation, [], [f63])).
fof(f2049, plain, (~ spl144_164 | ~ spl144_23), inference(avatar_split_clause, [], [f637, f1288, f2044])).
fof(f637, plain, (~ (e12 = op1(e12, e12)) | ~ sP42), inference(cnf_transformation, [], [f191])).
fof(f2040, plain, (~ spl144_163 | spl144_3), inference(avatar_split_clause, [], [f634, f1203, f2036])).
fof(f634, plain, ((e12 = op1(e13, e13)) | ~ sP43), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP43), inference(nnf_transformation, [], [f64])).
fof(f2034, plain, (~ spl144_162 | spl144_19), inference(avatar_split_clause, [], [f628, f1271, f2028])).
fof(f628, plain, ((e12 = op1(e12, e13)) | ~ sP44), inference(cnf_transformation, [], [f189])).
fof(f189, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP44), inference(nnf_transformation, [], [f65])).
fof(f2032, plain, (~ spl144_162 | spl144_64), inference(avatar_split_clause, [], [f630, f1462, f2028])).
fof(f630, plain, ((op1(e10, e10) = e13) | ~ sP44), inference(cnf_transformation, [], [f189])).
fof(f2024, plain, (~ spl144_161 | spl144_44), inference(avatar_split_clause, [], [f626, f1377, f2020])).
fof(f626, plain, ((e13 = op1(e11, e11)) | ~ sP45), inference(cnf_transformation, [], [f188])).
fof(f188, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP45), inference(nnf_transformation, [], [f66])).
fof(f2016, plain, (~ spl144_160 | spl144_24), inference(avatar_split_clause, [], [f622, f1292, f2012])).
fof(f622, plain, ((e13 = op1(e12, e12)) | ~ sP46), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP46), inference(nnf_transformation, [], [f67])).
fof(f2008, plain, (~ spl144_159 | spl144_4), inference(avatar_split_clause, [], [f618, f1207, f2004])).
fof(f618, plain, ((e13 = op1(e13, e13)) | ~ sP47), inference(cnf_transformation, [], [f186])).
fof(f186, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP47), inference(nnf_transformation, [], [f68])).
fof(f2001, plain, (~ spl144_158 | ~ spl144_1), inference(avatar_split_clause, [], [f613, f1195, f1996])).
fof(f613, plain, (~ (e10 = op1(e13, e13)) | ~ sP48), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP48), inference(nnf_transformation, [], [f69])).
fof(f1993, plain, (~ spl144_157 | ~ spl144_1), inference(avatar_split_clause, [], [f609, f1195, f1988])).
fof(f609, plain, (~ (e10 = op1(e13, e13)) | ~ sP49), inference(cnf_transformation, [], [f184])).
fof(f184, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP49), inference(nnf_transformation, [], [f70])).
fof(f1985, plain, (~ spl144_156 | ~ spl144_1), inference(avatar_split_clause, [], [f605, f1195, f1980])).
fof(f605, plain, (~ (e10 = op1(e13, e13)) | ~ sP50), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP50), inference(nnf_transformation, [], [f71])).
fof(f1977, plain, (~ spl144_155 | ~ spl144_1), inference(avatar_split_clause, [], [f601, f1195, f1972])).
fof(f601, plain, (~ (e10 = op1(e13, e13)) | ~ sP51), inference(cnf_transformation, [], [f182])).
fof(f182, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP51), inference(nnf_transformation, [], [f72])).
fof(f1970, plain, (~ spl144_154 | spl144_12), inference(avatar_split_clause, [], [f596, f1241, f1964])).
fof(f596, plain, ((e13 = op1(e13, e11)) | ~ sP52), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP52), inference(nnf_transformation, [], [f73])).
fof(f1962, plain, (~ spl144_153 | spl144_12), inference(avatar_split_clause, [], [f592, f1241, f1956])).
fof(f592, plain, ((e13 = op1(e13, e11)) | ~ sP53), inference(cnf_transformation, [], [f180])).
fof(f180, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP53), inference(nnf_transformation, [], [f74])).
fof(f1954, plain, (~ spl144_152 | spl144_12), inference(avatar_split_clause, [], [f588, f1241, f1948])).
fof(f588, plain, ((e13 = op1(e13, e11)) | ~ sP54), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP54), inference(nnf_transformation, [], [f75])).
fof(f1946, plain, (~ spl144_151 | spl144_12), inference(avatar_split_clause, [], [f584, f1241, f1940])).
fof(f584, plain, ((e13 = op1(e13, e11)) | ~ sP55), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP55), inference(nnf_transformation, [], [f76])).
fof(f1938, plain, (~ spl144_150 | spl144_8), inference(avatar_split_clause, [], [f580, f1224, f1932])).
fof(f580, plain, ((e13 = op1(e13, e12)) | ~ sP56), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP56), inference(nnf_transformation, [], [f77])).
fof(f1936, plain, (~ spl144_150 | spl144_63), inference(avatar_split_clause, [], [f582, f1458, f1932])).
fof(f582, plain, ((op1(e10, e10) = e12) | ~ sP56), inference(cnf_transformation, [], [f177])).
fof(f1930, plain, (~ spl144_149 | spl144_8), inference(avatar_split_clause, [], [f576, f1224, f1924])).
fof(f576, plain, ((e13 = op1(e13, e12)) | ~ sP57), inference(cnf_transformation, [], [f176])).
fof(f176, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP57), inference(nnf_transformation, [], [f78])).
fof(f1922, plain, (~ spl144_148 | spl144_8), inference(avatar_split_clause, [], [f572, f1224, f1916])).
fof(f572, plain, ((e13 = op1(e13, e12)) | ~ sP58), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP58), inference(nnf_transformation, [], [f79])).
fof(f1920, plain, (~ spl144_148 | spl144_23), inference(avatar_split_clause, [], [f574, f1288, f1916])).
fof(f574, plain, ((e12 = op1(e12, e12)) | ~ sP58), inference(cnf_transformation, [], [f175])).
fof(f1912, plain, (~ spl144_147 | spl144_3), inference(avatar_split_clause, [], [f570, f1203, f1908])).
fof(f570, plain, ((e12 = op1(e13, e13)) | ~ sP59), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP59), inference(nnf_transformation, [], [f80])).
fof(f1906, plain, (~ spl144_146 | spl144_4), inference(avatar_split_clause, [], [f564, f1207, f1900])).
fof(f564, plain, ((e13 = op1(e13, e13)) | ~ sP60), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP60), inference(nnf_transformation, [], [f81])).
fof(f1898, plain, (~ spl144_145 | spl144_4), inference(avatar_split_clause, [], [f560, f1207, f1892])).
fof(f560, plain, ((e13 = op1(e13, e13)) | ~ sP61), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP61), inference(nnf_transformation, [], [f82])).
fof(f1890, plain, (~ spl144_144 | spl144_4), inference(avatar_split_clause, [], [f556, f1207, f1884])).
fof(f556, plain, ((e13 = op1(e13, e13)) | ~ sP62), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP62), inference(nnf_transformation, [], [f83])).
fof(f1877, plain, (~ spl144_139 | spl144_142), inference(avatar_split_clause, [], [f549, f1874, f1857])).
fof(f549, plain, ((e11 = op1(e10, op1(e10, e11))) | ~ sP63), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ((((e10 = op1(e13, e13)) | ~ (e13 = op1(e13, e10))) & ((e10 = op1(e12, e12)) | ~ (e12 = op1(e12, e10))) & ((e10 = op1(e11, e11)) | ~ (e11 = op1(e11, e10))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e13 = op1(e10, op1(e10, e13))) & (e12 = op1(e10, op1(e10, e12))) & (e11 = op1(e10, op1(e10, e11))) & (e10 = op1(e10, op1(e10, e10)))) | ~ sP63), inference(nnf_transformation, [], [f84])).
fof(f1835, plain, (~ spl144_134 | ~ spl144_57 | spl144_62), inference(avatar_split_clause, [], [f544, f1454, f1433, f1830])).
fof(f544, plain, ((op1(e10, e10) = e11) | ~ (e10 = op1(e10, e11)) | ~ sP64), inference(cnf_transformation, [], [f169])).
fof(f169, plain, ((((e11 = op1(e13, e13)) | ~ (e13 = op1(e13, e11))) & ((e11 = op1(e12, e12)) | ~ (e12 = op1(e12, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((op1(e10, e10) = e11) | ~ (e10 = op1(e10, e11))) & (e13 = op1(e11, op1(e11, e13))) & (e12 = op1(e11, op1(e11, e12))) & (e11 = op1(e11, op1(e11, e11))) & (e10 = op1(e11, op1(e11, e10)))) | ~ sP64), inference(nnf_transformation, [], [f85])).
fof(f1823, plain, (~ spl144_129 | spl144_132), inference(avatar_split_clause, [], [f533, f1820, f1803])).
fof(f533, plain, ((e11 = op1(e12, op1(e12, e11))) | ~ sP65), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ((((e12 = op1(e13, e13)) | ~ (e13 = op1(e13, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e11, e11)) | ~ (e11 = op1(e11, e12))) & ((op1(e10, e10) = e12) | ~ (e10 = op1(e10, e12))) & (e13 = op1(e12, op1(e12, e13))) & (e12 = op1(e12, op1(e12, e12))) & (e11 = op1(e12, op1(e12, e11))) & (e10 = op1(e12, op1(e12, e10)))) | ~ sP65), inference(nnf_transformation, [], [f86])).
fof(f1818, plain, (~ spl144_129 | spl144_131), inference(avatar_split_clause, [], [f534, f1815, f1803])).
fof(f534, plain, ((e12 = op1(e12, op1(e12, e12))) | ~ sP65), inference(cnf_transformation, [], [f168])).
fof(f1813, plain, (~ spl144_129 | spl144_130), inference(avatar_split_clause, [], [f535, f1810, f1803])).
fof(f535, plain, ((e13 = op1(e12, op1(e12, e13))) | ~ sP65), inference(cnf_transformation, [], [f168])).
fof(f1806, plain, (~ spl144_129 | ~ spl144_8 | spl144_3), inference(avatar_split_clause, [], [f539, f1203, f1224, f1803])).
fof(f539, plain, ((e12 = op1(e13, e13)) | ~ (e13 = op1(e13, e12)) | ~ sP65), inference(cnf_transformation, [], [f168])).
fof(f1798, plain, (spl144_126 | spl144_110 | spl144_94 | spl144_78), inference(avatar_split_clause, [], [f379, f1554, f1622, f1690, f1758])).
fof(f379, plain, ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax4)).
fof(f1797, plain, (spl144_127 | spl144_123 | spl144_119 | spl144_115), inference(avatar_split_clause, [], [f380, f1711, f1728, f1745, f1762])).
fof(f380, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1796, plain, (spl144_127 | spl144_111 | spl144_95 | spl144_79), inference(avatar_split_clause, [], [f381, f1558, f1626, f1694, f1762])).
fof(f381, plain, ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1794, plain, (spl144_128 | spl144_112 | spl144_96 | spl144_80), inference(avatar_split_clause, [], [f383, f1562, f1630, f1698, f1766])).
fof(f383, plain, ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1793, plain, (spl144_109 | spl144_105 | spl144_101 | spl144_97), inference(avatar_split_clause, [], [f384, f1635, f1652, f1669, f1686])).
fof(f384, plain, ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1792, plain, (spl144_121 | spl144_105 | spl144_89 | spl144_73), inference(avatar_split_clause, [], [f385, f1533, f1601, f1669, f1737])).
fof(f385, plain, ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1785, plain, (spl144_93 | spl144_89 | spl144_85 | spl144_81), inference(avatar_split_clause, [], [f392, f1567, f1584, f1601, f1618])).
fof(f392, plain, ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1784, plain, (spl144_117 | spl144_101 | spl144_85 | spl144_69), inference(avatar_split_clause, [], [f393, f1516, f1584, f1652, f1720])).
fof(f393, plain, ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1783, plain, (spl144_94 | spl144_90 | spl144_86 | spl144_82), inference(avatar_split_clause, [], [f394, f1571, f1588, f1605, f1622])).
fof(f394, plain, ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1782, plain, (spl144_118 | spl144_102 | spl144_86 | spl144_70), inference(avatar_split_clause, [], [f395, f1520, f1588, f1656, f1724])).
fof(f395, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1781, plain, (spl144_95 | spl144_91 | spl144_87 | spl144_83), inference(avatar_split_clause, [], [f396, f1575, f1592, f1609, f1626])).
fof(f396, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1778, plain, (spl144_120 | spl144_104 | spl144_88 | spl144_72), inference(avatar_split_clause, [], [f399, f1528, f1596, f1664, f1732])).
fof(f399, plain, ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1774, plain, (spl144_114 | spl144_98 | spl144_82 | spl144_66), inference(avatar_split_clause, [], [f403, f1503, f1571, f1639, f1707])).
fof(f403, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1718, plain, (spl144_113 | spl144_114 | spl144_115 | spl144_116), inference(avatar_split_clause, [], [f363, f1715, f1711, f1707, f1703])).
fof(f363, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax3)).
fof(f1701, plain, (spl144_109 | spl144_110 | spl144_111 | spl144_112), inference(avatar_split_clause, [], [f364, f1698, f1694, f1690, f1686])).
fof(f364, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1650, plain, (spl144_97 | spl144_98 | spl144_99 | spl144_100), inference(avatar_split_clause, [], [f367, f1647, f1643, f1639, f1635])).
fof(f367, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1633, plain, (spl144_93 | spl144_94 | spl144_95 | spl144_96), inference(avatar_split_clause, [], [f368, f1630, f1626, f1622, f1618])).
fof(f368, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f1582, plain, (spl144_81 | spl144_82 | spl144_83 | spl144_84), inference(avatar_split_clause, [], [f371, f1579, f1575, f1571, f1567])).
fof(f371, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f1548, plain, (spl144_73 | spl144_74 | spl144_75 | spl144_76), inference(avatar_split_clause, [], [f373, f1545, f1541, f1537, f1533])).
fof(f373, plain, ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))), inference(cnf_transformation, [], [f3])).
fof(f1531, plain, (spl144_69 | spl144_70 | spl144_71 | spl144_72), inference(avatar_split_clause, [], [f374, f1528, f1524, f1520, f1516])).
fof(f374, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f1494, plain, (spl144_62 | spl144_46 | spl144_30 | spl144_14), inference(avatar_split_clause, [], [f331, f1250, f1318, f1386, f1454])).
fof(f331, plain, ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax2)).
fof(f1491, plain, (spl144_64 | spl144_60 | spl144_56 | spl144_52), inference(avatar_split_clause, [], [f334, f1411, f1428, f1445, f1462])).
fof(f334, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f1490, plain, (spl144_64 | spl144_48 | spl144_32 | spl144_16), inference(avatar_split_clause, [], [f335, f1258, f1326, f1394, f1462])).
fof(f335, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f1481, plain, (spl144_29 | spl144_25 | spl144_21 | spl144_17), inference(avatar_split_clause, [], [f344, f1263, f1280, f1297, f1314])).
fof(f344, plain, ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1480, plain, (spl144_53 | spl144_37 | spl144_21 | spl144_5), inference(avatar_split_clause, [], [f345, f1212, f1280, f1348, f1416])).
fof(f345, plain, ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f1478, plain, (spl144_54 | spl144_38 | spl144_22 | spl144_6), inference(avatar_split_clause, [], [f347, f1216, f1284, f1352, f1420])).
fof(f347, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f1469, plain, (spl144_15 | spl144_11 | spl144_7 | spl144_3), inference(avatar_split_clause, [], [f356, f1203, f1220, f1237, f1254])).
fof(f356, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1468, plain, (spl144_51 | spl144_35 | spl144_19 | spl144_3), inference(avatar_split_clause, [], [f357, f1203, f1271, f1339, f1407])).
fof(f357, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1467, plain, (spl144_16 | spl144_12 | spl144_8 | spl144_4), inference(avatar_split_clause, [], [f358, f1207, f1224, f1241, f1258])).
fof(f358, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1466, plain, (spl144_52 | spl144_36 | spl144_20 | spl144_4), inference(avatar_split_clause, [], [f359, f1207, f1275, f1343, f1411])).
fof(f359, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1448, plain, (spl144_57 | spl144_58 | spl144_59 | spl144_60), inference(avatar_split_clause, [], [f313, f1445, f1441, f1437, f1433])).
fof(f313, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG102+1.p', ax1)).
fof(f1431, plain, (spl144_53 | spl144_54 | spl144_55 | spl144_56), inference(avatar_split_clause, [], [f314, f1428, f1424, f1420, f1416])).
fof(f314, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f1397, plain, (spl144_45 | spl144_46 | spl144_47 | spl144_48), inference(avatar_split_clause, [], [f316, f1394, f1390, f1386, f1382])).
fof(f316, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f1363, plain, (spl144_37 | spl144_38 | spl144_39 | spl144_40), inference(avatar_split_clause, [], [f318, f1360, f1356, f1352, f1348])).
fof(f318, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f1346, plain, (spl144_33 | spl144_34 | spl144_35 | spl144_36), inference(avatar_split_clause, [], [f319, f1343, f1339, f1335, f1331])).
fof(f319, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f1329, plain, (spl144_29 | spl144_30 | spl144_31 | spl144_32), inference(avatar_split_clause, [], [f320, f1326, f1322, f1318, f1314])).
fof(f320, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f1295, plain, (spl144_21 | spl144_22 | spl144_23 | spl144_24), inference(avatar_split_clause, [], [f322, f1292, f1288, f1284, f1280])).
fof(f322, plain, ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))), inference(cnf_transformation, [], [f1])).
fof(f1278, plain, (spl144_17 | spl144_18 | spl144_19 | spl144_20), inference(avatar_split_clause, [], [f323, f1275, f1271, f1267, f1263])).
fof(f323, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f1261, plain, (spl144_13 | spl144_14 | spl144_15 | spl144_16), inference(avatar_split_clause, [], [f324, f1258, f1254, f1250, f1246])).
fof(f324, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).
fof(f1244, plain, (spl144_9 | spl144_10 | spl144_11 | spl144_12), inference(avatar_split_clause, [], [f325, f1241, f1237, f1233, f1229])).
fof(f325, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))), inference(cnf_transformation, [], [f1])).