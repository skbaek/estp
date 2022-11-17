fof(f6121, plain, $false, inference(avatar_sat_refutation, [], [f1186, f1203, f1220, f1254, f1288, f1356, f1373, f1390, f1407, f1442, f1443, f1444, f1445, f1446, f1449, f1457, f1458, f1459, f1462, f1463, f1467, f1490, f1507, f1558, f1626, f1677, f1694, f1711, f1746, f1747, f1748, f1749, f1750, f1753, f1754, f1757, f1761, f1762, f1763, f1766, f1767, f1771, f1772, f1783, f1792, f1793, f1799, f1806, f1807, f1808, f1809, f1814, f1815, f1817, f1823, f1831, f1841, f1849, f1857, f1865, f1873, f1881, f1889, f1897, f1902, f1903, f1905, f1911, f1918, f1921, f1927, f1935, f1936, f1937, f1944, f1945, f1951, f1959, f1969, f1977, f1983, f1985, f1993, f2001, f2009, f2017, f2025, f2030, f2031, f2039, f2048, f2049, f2055, f2062, f2063, f2070, f2071, f2079, f2087, f2097, f2105, f2113, f2121, f2129, f2137, f2145, f2153, f2158, f2159, f2161, f2167, f2174, f2177, f2183, f2190, f2191, f2193, f2198, f2199, f2201, f2207, f2215, f2224, f2232, f2240, f2248, f2257, f2265, f2273, f2281, f2286, f2289, f2296, f2305, f2308, f2309, f2311, f2313, f2319, f2328, f2329, f2335, f2342, f2343, f2350, f2353, f2359, f2367, f2375, f2377, f2383, f2385, f2391, f2393, f2401, f2409, f2417, f2425, f2433, f2438, f2439, f2447, f2454, f2457, f2463, f2470, f2471, f2472, f2480, f2481, f2487, f2495, f2505, f2513, f2521, f2529, f2537, f2545, f2553, f2561, f2566, f2567, f2568, f2575, f2584, f2591, f2598, f2599, f2606, f2607, f2615, f2623, f2631, f2633, f2639, f2641, f2649, f2657, f2665, f2673, f2681, f2689, f2694, f2695, f2697, f2703, f2710, f2713, f2719, f2726, f2729, f2734, f2737, f2743, f2751, f2761, f2769, f2777, f2785, f2793, f2801, f2809, f2817, f2822, f2825, f2832, f2841, f2844, f2845, f2847, f2849, f2851, f2852, f2854, f2855, f2907, f2922, f2952, f3028, f3055, f3064, f3086, f3107, f3125, f3246, f3465, f3553, f3582, f3603, f3611, f3626, f3645, f3646, f3650, f3653, f3655, f3663, f3681, f3684, f3713, f3737, f3747, f3788, f3806, f3807, f3808, f3809, f3814, f3815, f3830, f3847, f3851, f3862, f3866, f3881, f3900, f3907, f3914, f3925, f3948, f3951, f3955, f3958, f3974, f3979, f3985, f4000, f4038, f4048, f4064, f4069, f4072, f4075, f4090, f4097, f4101, f4103, f4105, f4110, f4140, f4155, f4159, f4166, f4170, f4173, f4181, f4190, f4235, f4237, f4239, f4241, f4256, f4266, f4271, f4279, f4283, f4300, f4310, f4320, f4333, f4351, f4361, f4367, f4385, f4406, f4408, f4417, f4426, f4429, f4430, f4454, f4467, f4501, f4504, f4517, f4525, f4541, f4556, f4558, f4565, f4584, f4588, f4628, f4645, f4651, f4683, f4686, f4689, f4724, f4731, f4734, f4740, f4741, f4767, f4769, f4799, f4810, f4814, f4816, f4833, f4839, f4855, f4864, f4871, f4875, f4879, f4886, f4905, f4934, f4944, f4972, f4999, f5027, f5051, f5071, f5095, f5106, f5114, f5140, f5163, f5187, f5198, f5206, f5220, f5276, f5281, f5295, f5297, f5344, f5383, f5403, f5424, f5441, f5447, f5457, f5466, f5473, f5481, f5486, f5494, f5506, f5509, f5517, f5522, f5525, f5526, f5529, f5630, f5656, f5708, f5772, f5842, f5846, f5872, f5874, f5879, f5900, f5905, f5918, f5923, f5936, f5941, f5953, f5962, f5967, f5980, f6035, f6041, f6046, f6058, f6074, f6105])).
fof(f6105, plain, (~ spl144_52 | ~ spl144_116 | ~ spl144_367 | spl144_376), inference(avatar_contradiction_clause, [], [f6104])).
fof(f6104, plain, ($false | (~ spl144_52 | ~ spl144_116 | ~ spl144_367 | spl144_376)), inference(subsumption_resolution, [], [f6103, f1693])).
fof(f1693, plain, ((e23 = op2(e20, e23)) | ~ spl144_116), inference(avatar_component_clause, [], [f1691])).
fof(f1691, plain, (spl144_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_116])])).
fof(f6103, plain, (~ (e23 = op2(e20, e23)) | (~ spl144_52 | ~ spl144_367 | spl144_376)), inference(forward_demodulation, [], [f6102, f3503])).
fof(f3503, plain, ((e23 = h1(e13)) | ~ spl144_367), inference(avatar_component_clause, [], [f3502])).
fof(f3502, plain, (spl144_367 <=> (e23 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_367])])).
fof(f6102, plain, (~ (op2(e20, e23) = h1(e13)) | (~ spl144_52 | ~ spl144_367 | spl144_376)), inference(forward_demodulation, [], [f6101, f1389])).
fof(f1389, plain, ((e13 = op1(e10, e13)) | ~ spl144_52), inference(avatar_component_clause, [], [f1387])).
fof(f1387, plain, (spl144_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_52])])).
fof(f6101, plain, (~ (op2(e20, e23) = h1(op1(e10, e13))) | (~ spl144_367 | spl144_376)), inference(forward_demodulation, [], [f3540, f3503])).
fof(f3540, plain, (~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | spl144_376), inference(avatar_component_clause, [], [f3538])).
fof(f3538, plain, (spl144_376 <=> (h1(op1(e10, e13)) = op2(e20, h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_376])])).
fof(f6074, plain, (~ spl144_4 | ~ spl144_12), inference(avatar_split_clause, [], [f6069, f1217, f1183])).
fof(f1183, plain, (spl144_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_4])])).
fof(f1217, plain, (spl144_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_12])])).
fof(f6069, plain, (~ (e13 = op1(e13, e13)) | ~ spl144_12), inference(backward_demodulation, [], [f454, f1219])).
fof(f1219, plain, ((e13 = op1(e13, e11)) | ~ spl144_12), inference(avatar_component_clause, [], [f1217])).
fof(f454, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax5)).
fof(f6058, plain, (~ spl144_10 | ~ spl144_42), inference(avatar_split_clause, [], [f6050, f1345, f1209])).
fof(f1209, plain, (spl144_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_10])])).
fof(f1345, plain, (spl144_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_42])])).
fof(f6050, plain, (~ (e11 = op1(e13, e11)) | ~ spl144_42), inference(backward_demodulation, [], [f418, f1347])).
fof(f1347, plain, ((e11 = op1(e11, e11)) | ~ spl144_42), inference(avatar_component_clause, [], [f1345])).
fof(f418, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f6046, plain, (~ spl144_4 | ~ spl144_52), inference(avatar_split_clause, [], [f6042, f1387, f1183])).
fof(f6042, plain, (~ (e13 = op1(e13, e13)) | ~ spl144_52), inference(backward_demodulation, [], [f428, f1389])).
fof(f428, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f6041, plain, (~ spl144_53 | ~ spl144_117 | spl144_377), inference(avatar_contradiction_clause, [], [f6040])).
fof(f6040, plain, ($false | (~ spl144_53 | ~ spl144_117 | spl144_377)), inference(subsumption_resolution, [], [f6034, f1090])).
fof(f1090, plain, (e20 = h1(e10)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(op2(e20, op2(e20, e20)), e20) = h1(e13)) & (op2(e20, op2(e20, e20)) = h1(e12)) & (op2(e20, e20) = h1(e11)) & (e20 = h1(e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax14)).
fof(f6034, plain, (~ (e20 = h1(e10)) | (~ spl144_53 | ~ spl144_117 | spl144_377)), inference(backward_demodulation, [], [f6007, f1394])).
fof(f1394, plain, ((e10 = op1(e10, e12)) | ~ spl144_53), inference(avatar_component_clause, [], [f1392])).
fof(f1392, plain, (spl144_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_53])])).
fof(f6007, plain, (~ (e20 = h1(op1(e10, e12))) | (~ spl144_117 | spl144_377)), inference(forward_demodulation, [], [f3544, f1698])).
fof(f1698, plain, ((e20 = op2(e20, e22)) | ~ spl144_117), inference(avatar_component_clause, [], [f1696])).
fof(f1696, plain, (spl144_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_117])])).
fof(f3544, plain, (~ (op2(e20, e22) = h1(op1(e10, e12))) | spl144_377), inference(avatar_component_clause, [], [f3542])).
fof(f3542, plain, (spl144_377 <=> (op2(e20, e22) = h1(op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_377])])).
fof(f6035, plain, (~ spl144_5 | ~ spl144_53), inference(avatar_split_clause, [], [f6026, f1392, f1188])).
fof(f1188, plain, (spl144_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_5])])).
fof(f6026, plain, (~ (e10 = op1(e13, e12)) | ~ spl144_53), inference(backward_demodulation, [], [f422, f1394])).
fof(f422, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f5980, plain, (~ spl144_35 | ~ spl144_123 | ~ spl144_263 | ~ spl144_266 | ~ spl144_272 | spl144_308), inference(avatar_contradiction_clause, [], [f5979])).
fof(f5979, plain, ($false | (~ spl144_35 | ~ spl144_123 | ~ spl144_263 | ~ spl144_266 | ~ spl144_272 | spl144_308)), inference(subsumption_resolution, [], [f5978, f1723])).
fof(f1723, plain, ((e22 = op2(e20, e21)) | ~ spl144_123), inference(avatar_component_clause, [], [f1721])).
fof(f1721, plain, (spl144_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_123])])).
fof(f5978, plain, (~ (e22 = op2(e20, e21)) | (~ spl144_35 | ~ spl144_263 | ~ spl144_266 | ~ spl144_272 | spl144_308)), inference(forward_demodulation, [], [f5977, f2905])).
fof(f2905, plain, ((e22 = h4(e12)) | ~ spl144_263), inference(avatar_component_clause, [], [f2904])).
fof(f2904, plain, (spl144_263 <=> (e22 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_263])])).
fof(f5977, plain, (~ (op2(e20, e21) = h4(e12)) | (~ spl144_35 | ~ spl144_266 | ~ spl144_272 | spl144_308)), inference(forward_demodulation, [], [f5976, f1317])).
fof(f1317, plain, ((e12 = op1(e11, e13)) | ~ spl144_35), inference(avatar_component_clause, [], [f1315])).
fof(f1315, plain, (spl144_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_35])])).
fof(f5976, plain, (~ (op2(e20, e21) = h4(op1(e11, e13))) | (~ spl144_266 | ~ spl144_272 | spl144_308)), inference(forward_demodulation, [], [f5975, f2950])).
fof(f2950, plain, ((e20 = h4(e11)) | ~ spl144_272), inference(avatar_component_clause, [], [f2949])).
fof(f2949, plain, (spl144_272 <=> (e20 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_272])])).
fof(f5975, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e21)) | (~ spl144_266 | spl144_308)), inference(forward_demodulation, [], [f3147, f2920])).
fof(f2920, plain, ((e21 = h4(e13)) | ~ spl144_266), inference(avatar_component_clause, [], [f2919])).
fof(f2919, plain, (spl144_266 <=> (e21 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_266])])).
fof(f3147, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | spl144_308), inference(avatar_component_clause, [], [f3145])).
fof(f3145, plain, (spl144_308 <=> (h4(op1(e11, e13)) = op2(h4(e11), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_308])])).
fof(f5967, plain, (~ spl144_56 | ~ spl144_70 | ~ spl144_263 | ~ spl144_266 | spl144_320), inference(avatar_contradiction_clause, [], [f5966])).
fof(f5966, plain, ($false | (~ spl144_56 | ~ spl144_70 | ~ spl144_263 | ~ spl144_266 | spl144_320)), inference(subsumption_resolution, [], [f5965, f1498])).
fof(f1498, plain, ((e21 = op2(e23, e22)) | ~ spl144_70), inference(avatar_component_clause, [], [f1496])).
fof(f1496, plain, (spl144_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_70])])).
fof(f5965, plain, (~ (e21 = op2(e23, e22)) | (~ spl144_56 | ~ spl144_263 | ~ spl144_266 | spl144_320)), inference(forward_demodulation, [], [f5964, f2920])).
fof(f5964, plain, (~ (op2(e23, e22) = h4(e13)) | (~ spl144_56 | ~ spl144_263 | spl144_320)), inference(forward_demodulation, [], [f5963, f1406])).
fof(f1406, plain, ((e13 = op1(e10, e12)) | ~ spl144_56), inference(avatar_component_clause, [], [f1404])).
fof(f1404, plain, (spl144_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_56])])).
fof(f5963, plain, (~ (op2(e23, e22) = h4(op1(e10, e12))) | (~ spl144_263 | spl144_320)), inference(forward_demodulation, [], [f3195, f2905])).
fof(f3195, plain, (~ (h4(op1(e10, e12)) = op2(e23, h4(e12))) | spl144_320), inference(avatar_component_clause, [], [f3193])).
fof(f3193, plain, (spl144_320 <=> (h4(op1(e10, e12)) = op2(e23, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_320])])).
fof(f5962, plain, (~ spl144_5 | ~ spl144_104 | ~ spl144_263 | ~ spl144_266 | spl144_313), inference(avatar_contradiction_clause, [], [f5961])).
fof(f5961, plain, ($false | (~ spl144_5 | ~ spl144_104 | ~ spl144_263 | ~ spl144_266 | spl144_313)), inference(subsumption_resolution, [], [f5960, f1642])).
fof(f1642, plain, ((e23 = op2(e21, e22)) | ~ spl144_104), inference(avatar_component_clause, [], [f1640])).
fof(f1640, plain, (spl144_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_104])])).
fof(f5960, plain, (~ (e23 = op2(e21, e22)) | (~ spl144_5 | ~ spl144_263 | ~ spl144_266 | spl144_313)), inference(forward_demodulation, [], [f5959, f1102])).
fof(f1102, plain, (e23 = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((h4(e13) = op2(op2(e23, op2(e23, e23)), e23)) & (h4(e12) = op2(e23, op2(e23, e23))) & (op2(e23, e23) = h4(e11)) & (e23 = h4(e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax17)).
fof(f5959, plain, (~ (op2(e21, e22) = h4(e10)) | (~ spl144_5 | ~ spl144_263 | ~ spl144_266 | spl144_313)), inference(forward_demodulation, [], [f5958, f1190])).
fof(f1190, plain, ((e10 = op1(e13, e12)) | ~ spl144_5), inference(avatar_component_clause, [], [f1188])).
fof(f5958, plain, (~ (op2(e21, e22) = h4(op1(e13, e12))) | (~ spl144_263 | ~ spl144_266 | spl144_313)), inference(forward_demodulation, [], [f5957, f2920])).
fof(f5957, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), e22)) | (~ spl144_263 | spl144_313)), inference(forward_demodulation, [], [f3167, f2905])).
fof(f3167, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | spl144_313), inference(avatar_component_clause, [], [f3165])).
fof(f3165, plain, (spl144_313 <=> (h4(op1(e13, e12)) = op2(h4(e13), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_313])])).
fof(f5953, plain, (~ spl144_4 | ~ spl144_106 | ~ spl144_266 | spl144_314), inference(avatar_contradiction_clause, [], [f5952])).
fof(f5952, plain, ($false | (~ spl144_4 | ~ spl144_106 | ~ spl144_266 | spl144_314)), inference(subsumption_resolution, [], [f5951, f1651])).
fof(f1651, plain, ((e21 = op2(e21, e21)) | ~ spl144_106), inference(avatar_component_clause, [], [f1649])).
fof(f1649, plain, (spl144_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_106])])).
fof(f5951, plain, (~ (e21 = op2(e21, e21)) | (~ spl144_4 | ~ spl144_266 | spl144_314)), inference(forward_demodulation, [], [f5950, f2920])).
fof(f5950, plain, (~ (op2(e21, e21) = h4(e13)) | (~ spl144_4 | ~ spl144_266 | spl144_314)), inference(forward_demodulation, [], [f5949, f1185])).
fof(f1185, plain, ((e13 = op1(e13, e13)) | ~ spl144_4), inference(avatar_component_clause, [], [f1183])).
fof(f5949, plain, (~ (op2(e21, e21) = h4(op1(e13, e13))) | (~ spl144_266 | spl144_314)), inference(forward_demodulation, [], [f3171, f2920])).
fof(f3171, plain, (~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | spl144_314), inference(avatar_component_clause, [], [f3169])).
fof(f3169, plain, (spl144_314 <=> (h4(op1(e13, e13)) = op2(h4(e13), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_314])])).
fof(f5941, plain, (~ spl144_15 | ~ spl144_99 | ~ spl144_263 | ~ spl144_266 | spl144_316), inference(avatar_contradiction_clause, [], [f5940])).
fof(f5940, plain, ($false | (~ spl144_15 | ~ spl144_99 | ~ spl144_263 | ~ spl144_266 | spl144_316)), inference(subsumption_resolution, [], [f5939, f1621])).
fof(f1621, plain, ((e22 = op2(e21, e23)) | ~ spl144_99), inference(avatar_component_clause, [], [f1619])).
fof(f1619, plain, (spl144_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_99])])).
fof(f5939, plain, (~ (e22 = op2(e21, e23)) | (~ spl144_15 | ~ spl144_263 | ~ spl144_266 | spl144_316)), inference(forward_demodulation, [], [f5938, f2905])).
fof(f5938, plain, (~ (op2(e21, e23) = h4(e12)) | (~ spl144_15 | ~ spl144_266 | spl144_316)), inference(forward_demodulation, [], [f5937, f1232])).
fof(f1232, plain, ((e12 = op1(e13, e10)) | ~ spl144_15), inference(avatar_component_clause, [], [f1230])).
fof(f1230, plain, (spl144_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_15])])).
fof(f5937, plain, (~ (op2(e21, e23) = h4(op1(e13, e10))) | (~ spl144_266 | spl144_316)), inference(forward_demodulation, [], [f3179, f2920])).
fof(f3179, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | spl144_316), inference(avatar_component_clause, [], [f3177])).
fof(f3177, plain, (spl144_316 <=> (h4(op1(e13, e10)) = op2(h4(e13), e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_316])])).
fof(f5936, plain, (~ spl144_18 | ~ spl144_89 | ~ spl144_263 | ~ spl144_266 | ~ spl144_272 | spl144_311), inference(avatar_contradiction_clause, [], [f5935])).
fof(f5935, plain, ($false | (~ spl144_18 | ~ spl144_89 | ~ spl144_263 | ~ spl144_266 | ~ spl144_272 | spl144_311)), inference(subsumption_resolution, [], [f5934, f1579])).
fof(f1579, plain, ((e20 = op2(e22, e21)) | ~ spl144_89), inference(avatar_component_clause, [], [f1577])).
fof(f1577, plain, (spl144_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_89])])).
fof(f5934, plain, (~ (e20 = op2(e22, e21)) | (~ spl144_18 | ~ spl144_263 | ~ spl144_266 | ~ spl144_272 | spl144_311)), inference(forward_demodulation, [], [f5933, f2950])).
fof(f5933, plain, (~ (op2(e22, e21) = h4(e11)) | (~ spl144_18 | ~ spl144_263 | ~ spl144_266 | spl144_311)), inference(forward_demodulation, [], [f5932, f1245])).
fof(f1245, plain, ((e11 = op1(e12, e13)) | ~ spl144_18), inference(avatar_component_clause, [], [f1243])).
fof(f1243, plain, (spl144_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_18])])).
fof(f5932, plain, (~ (op2(e22, e21) = h4(op1(e12, e13))) | (~ spl144_263 | ~ spl144_266 | spl144_311)), inference(forward_demodulation, [], [f5931, f2905])).
fof(f5931, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e21)) | (~ spl144_266 | spl144_311)), inference(forward_demodulation, [], [f3159, f2920])).
fof(f3159, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | spl144_311), inference(avatar_component_clause, [], [f3157])).
fof(f3157, plain, (spl144_311 <=> (h4(op1(e12, e13)) = op2(h4(e12), h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_311])])).
fof(f5923, plain, (~ spl144_45 | ~ spl144_116 | ~ spl144_272 | spl144_318), inference(avatar_contradiction_clause, [], [f5922])).
fof(f5922, plain, ($false | (~ spl144_45 | ~ spl144_116 | ~ spl144_272 | spl144_318)), inference(subsumption_resolution, [], [f5921, f1693])).
fof(f5921, plain, (~ (e23 = op2(e20, e23)) | (~ spl144_45 | ~ spl144_272 | spl144_318)), inference(forward_demodulation, [], [f5920, f1102])).
fof(f5920, plain, (~ (op2(e20, e23) = h4(e10)) | (~ spl144_45 | ~ spl144_272 | spl144_318)), inference(forward_demodulation, [], [f5919, f1360])).
fof(f1360, plain, ((e10 = op1(e11, e10)) | ~ spl144_45), inference(avatar_component_clause, [], [f1358])).
fof(f1358, plain, (spl144_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_45])])).
fof(f5919, plain, (~ (op2(e20, e23) = h4(op1(e11, e10))) | (~ spl144_272 | spl144_318)), inference(forward_demodulation, [], [f3187, f2950])).
fof(f3187, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | spl144_318), inference(avatar_component_clause, [], [f3185])).
fof(f3185, plain, (spl144_318 <=> (h4(op1(e11, e10)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_318])])).
fof(f5918, plain, (~ spl144_38 | ~ spl144_117 | ~ spl144_263 | ~ spl144_272 | spl144_307), inference(avatar_contradiction_clause, [], [f5917])).
fof(f5917, plain, ($false | (~ spl144_38 | ~ spl144_117 | ~ spl144_263 | ~ spl144_272 | spl144_307)), inference(subsumption_resolution, [], [f5916, f1698])).
fof(f5916, plain, (~ (e20 = op2(e20, e22)) | (~ spl144_38 | ~ spl144_263 | ~ spl144_272 | spl144_307)), inference(forward_demodulation, [], [f5915, f2950])).
fof(f5915, plain, (~ (op2(e20, e22) = h4(e11)) | (~ spl144_38 | ~ spl144_263 | ~ spl144_272 | spl144_307)), inference(forward_demodulation, [], [f5914, f1330])).
fof(f1330, plain, ((e11 = op1(e11, e12)) | ~ spl144_38), inference(avatar_component_clause, [], [f1328])).
fof(f1328, plain, (spl144_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_38])])).
fof(f5914, plain, (~ (op2(e20, e22) = h4(op1(e11, e12))) | (~ spl144_263 | ~ spl144_272 | spl144_307)), inference(forward_demodulation, [], [f5913, f2950])).
fof(f5913, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | (~ spl144_263 | spl144_307)), inference(forward_demodulation, [], [f3143, f2905])).
fof(f3143, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | spl144_307), inference(avatar_component_clause, [], [f3141])).
fof(f3141, plain, (spl144_307 <=> (h4(op1(e11, e12)) = op2(h4(e11), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_307])])).
fof(f5905, plain, (~ spl144_49 | ~ spl144_76 | ~ spl144_266 | spl144_319), inference(avatar_contradiction_clause, [], [f5904])).
fof(f5904, plain, ($false | (~ spl144_49 | ~ spl144_76 | ~ spl144_266 | spl144_319)), inference(subsumption_resolution, [], [f5903, f1523])).
fof(f1523, plain, ((e23 = op2(e23, e21)) | ~ spl144_76), inference(avatar_component_clause, [], [f1521])).
fof(f1521, plain, (spl144_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_76])])).
fof(f5903, plain, (~ (e23 = op2(e23, e21)) | (~ spl144_49 | ~ spl144_266 | spl144_319)), inference(forward_demodulation, [], [f5902, f1102])).
fof(f5902, plain, (~ (op2(e23, e21) = h4(e10)) | (~ spl144_49 | ~ spl144_266 | spl144_319)), inference(forward_demodulation, [], [f5901, f1377])).
fof(f1377, plain, ((e10 = op1(e10, e13)) | ~ spl144_49), inference(avatar_component_clause, [], [f1375])).
fof(f1375, plain, (spl144_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_49])])).
fof(f5901, plain, (~ (op2(e23, e21) = h4(op1(e10, e13))) | (~ spl144_266 | spl144_319)), inference(forward_demodulation, [], [f3191, f2920])).
fof(f3191, plain, (~ (h4(op1(e10, e13)) = op2(e23, h4(e13))) | spl144_319), inference(avatar_component_clause, [], [f3189])).
fof(f3189, plain, (spl144_319 <=> (h4(op1(e10, e13)) = op2(e23, h4(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_319])])).
fof(f5900, plain, (~ spl144_10 | ~ spl144_109 | ~ spl144_266 | ~ spl144_272 | spl144_312), inference(avatar_contradiction_clause, [], [f5899])).
fof(f5899, plain, ($false | (~ spl144_10 | ~ spl144_109 | ~ spl144_266 | ~ spl144_272 | spl144_312)), inference(subsumption_resolution, [], [f5893, f1664])).
fof(f1664, plain, ((e20 = op2(e21, e20)) | ~ spl144_109), inference(avatar_component_clause, [], [f1662])).
fof(f1662, plain, (spl144_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_109])])).
fof(f5893, plain, (~ (e20 = op2(e21, e20)) | (~ spl144_10 | ~ spl144_266 | ~ spl144_272 | spl144_312)), inference(backward_demodulation, [], [f5799, f2920])).
fof(f5799, plain, (~ (e20 = op2(h4(e13), e20)) | (~ spl144_10 | ~ spl144_272 | spl144_312)), inference(forward_demodulation, [], [f5795, f2950])).
fof(f5795, plain, (~ (h4(e11) = op2(h4(e13), e20)) | (~ spl144_10 | ~ spl144_272 | spl144_312)), inference(backward_demodulation, [], [f5737, f1211])).
fof(f1211, plain, ((e11 = op1(e13, e11)) | ~ spl144_10), inference(avatar_component_clause, [], [f1209])).
fof(f5737, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), e20)) | (~ spl144_272 | spl144_312)), inference(forward_demodulation, [], [f3163, f2950])).
fof(f3163, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | spl144_312), inference(avatar_component_clause, [], [f3161])).
fof(f3161, plain, (spl144_312 <=> (h4(op1(e13, e11)) = op2(h4(e13), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_312])])).
fof(f5879, plain, (spl144_266 | ~ spl144_82 | ~ spl144_263), inference(avatar_split_clause, [], [f5868, f2904, f1547, f2919])).
fof(f1547, plain, (spl144_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_82])])).
fof(f5868, plain, ((e21 = h4(e13)) | (~ spl144_82 | ~ spl144_263)), inference(forward_demodulation, [], [f5858, f1549])).
fof(f1549, plain, ((e21 = op2(e22, e23)) | ~ spl144_82), inference(avatar_component_clause, [], [f1547])).
fof(f5858, plain, ((op2(e22, e23) = h4(e13)) | ~ spl144_263), inference(backward_demodulation, [], [f2886, f2905])).
fof(f2886, plain, (h4(e13) = op2(h4(e12), e23)), inference(backward_demodulation, [], [f1105, f1104])).
fof(f1104, plain, (h4(e12) = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f17])).
fof(f1105, plain, (h4(e13) = op2(op2(e23, op2(e23, e23)), e23)), inference(cnf_transformation, [], [f17])).
fof(f5874, plain, (~ spl144_23 | ~ spl144_87 | ~ spl144_263 | spl144_310), inference(avatar_contradiction_clause, [], [f5873])).
fof(f5873, plain, ($false | (~ spl144_23 | ~ spl144_87 | ~ spl144_263 | spl144_310)), inference(subsumption_resolution, [], [f5863, f1570])).
fof(f1570, plain, ((e22 = op2(e22, e22)) | ~ spl144_87), inference(avatar_component_clause, [], [f1568])).
fof(f1568, plain, (spl144_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_87])])).
fof(f5863, plain, (~ (e22 = op2(e22, e22)) | (~ spl144_23 | ~ spl144_263 | spl144_310)), inference(backward_demodulation, [], [f5831, f2905])).
fof(f5831, plain, (~ (h4(e12) = op2(h4(e12), h4(e12))) | (~ spl144_23 | spl144_310)), inference(forward_demodulation, [], [f3155, f1266])).
fof(f1266, plain, ((e12 = op1(e12, e12)) | ~ spl144_23), inference(avatar_component_clause, [], [f1264])).
fof(f1264, plain, (spl144_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_23])])).
fof(f3155, plain, (~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | spl144_310), inference(avatar_component_clause, [], [f3153])).
fof(f3153, plain, (spl144_310 <=> (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_310])])).
fof(f5872, plain, (~ spl144_25 | ~ spl144_96 | ~ spl144_263 | ~ spl144_272 | spl144_309), inference(avatar_contradiction_clause, [], [f5871])).
fof(f5871, plain, ($false | (~ spl144_25 | ~ spl144_96 | ~ spl144_263 | ~ spl144_272 | spl144_309)), inference(subsumption_resolution, [], [f5862, f1608])).
fof(f1608, plain, ((e23 = op2(e22, e20)) | ~ spl144_96), inference(avatar_component_clause, [], [f1606])).
fof(f1606, plain, (spl144_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_96])])).
fof(f5862, plain, (~ (e23 = op2(e22, e20)) | (~ spl144_25 | ~ spl144_263 | ~ spl144_272 | spl144_309)), inference(backward_demodulation, [], [f5830, f2905])).
fof(f5830, plain, (~ (e23 = op2(h4(e12), e20)) | (~ spl144_25 | ~ spl144_272 | spl144_309)), inference(forward_demodulation, [], [f5829, f1102])).
fof(f5829, plain, (~ (h4(e10) = op2(h4(e12), e20)) | (~ spl144_25 | ~ spl144_272 | spl144_309)), inference(forward_demodulation, [], [f5760, f1275])).
fof(f1275, plain, ((e10 = op1(e12, e11)) | ~ spl144_25), inference(avatar_component_clause, [], [f1273])).
fof(f1273, plain, (spl144_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_25])])).
fof(f5760, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e20)) | (~ spl144_272 | spl144_309)), inference(forward_demodulation, [], [f3151, f2950])).
fof(f3151, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | spl144_309), inference(avatar_component_clause, [], [f3149])).
fof(f3149, plain, (spl144_309 <=> (h4(op1(e12, e11)) = op2(h4(e12), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_309])])).
fof(f5846, plain, (~ spl144_266 | ~ spl144_44 | ~ spl144_126 | ~ spl144_272 | spl144_306), inference(avatar_split_clause, [], [f5845, f3137, f2949, f1734, f1353, f2919])).
fof(f1353, plain, (spl144_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_44])])).
fof(f1734, plain, (spl144_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl144_126])])).
fof(f3137, plain, (spl144_306 <=> (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_306])])).
fof(f5845, plain, (~ (e21 = h4(e13)) | (~ spl144_44 | ~ spl144_126 | ~ spl144_272 | spl144_306)), inference(forward_demodulation, [], [f5844, f1736])).
fof(f1736, plain, ((op2(e20, e20) = e21) | ~ spl144_126), inference(avatar_component_clause, [], [f1734])).
fof(f5844, plain, (~ (op2(e20, e20) = h4(e13)) | (~ spl144_44 | ~ spl144_272 | spl144_306)), inference(forward_demodulation, [], [f5843, f1355])).
fof(f1355, plain, ((e13 = op1(e11, e11)) | ~ spl144_44), inference(avatar_component_clause, [], [f1353])).
fof(f5843, plain, (~ (op2(e20, e20) = h4(op1(e11, e11))) | (~ spl144_272 | spl144_306)), inference(forward_demodulation, [], [f3139, f2950])).
fof(f3139, plain, (~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | spl144_306), inference(avatar_component_clause, [], [f3137])).
fof(f5842, plain, (spl144_263 | ~ spl144_79 | ~ spl144_272), inference(avatar_split_clause, [], [f5841, f2949, f1534, f2904])).
fof(f1534, plain, (spl144_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_79])])).
fof(f5841, plain, ((e22 = h4(e12)) | (~ spl144_79 | ~ spl144_272)), inference(forward_demodulation, [], [f5840, f1536])).
fof(f1536, plain, ((e22 = op2(e23, e20)) | ~ spl144_79), inference(avatar_component_clause, [], [f1534])).
fof(f5840, plain, ((op2(e23, e20) = h4(e12)) | ~ spl144_272), inference(forward_demodulation, [], [f2893, f2950])).
fof(f2893, plain, (h4(e12) = op2(e23, h4(e11))), inference(backward_demodulation, [], [f1104, f1103])).
fof(f1103, plain, (op2(e23, e23) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f5772, plain, (~ spl144_12 | ~ spl144_44), inference(avatar_split_clause, [], [f5766, f1353, f1217])).
fof(f5766, plain, (~ (e13 = op1(e13, e11)) | ~ spl144_44), inference(backward_demodulation, [], [f418, f1355])).
fof(f5708, plain, (~ spl144_1 | ~ spl144_49), inference(avatar_split_clause, [], [f5707, f1375, f1171])).
fof(f1171, plain, (spl144_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_1])])).
fof(f5707, plain, (~ (e10 = op1(e13, e13)) | ~ spl144_49), inference(forward_demodulation, [], [f428, f1377])).
fof(f5656, plain, (~ spl144_49 | ~ spl144_52), inference(avatar_contradiction_clause, [], [f5655])).
fof(f5655, plain, ($false | (~ spl144_49 | ~ spl144_52)), inference(subsumption_resolution, [], [f5652, f506])).
fof(f506, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax7)).
fof(f5652, plain, ((e10 = e13) | (~ spl144_49 | ~ spl144_52)), inference(backward_demodulation, [], [f1389, f1377])).
fof(f5630, plain, (~ spl144_42 | spl144_125 | ~ spl144_272 | ~ spl144_306), inference(avatar_contradiction_clause, [], [f5629])).
fof(f5629, plain, ($false | (~ spl144_42 | spl144_125 | ~ spl144_272 | ~ spl144_306)), inference(subsumption_resolution, [], [f5621, f1731])).
fof(f1731, plain, (~ (e20 = op2(e20, e20)) | spl144_125), inference(avatar_component_clause, [], [f1730])).
fof(f1730, plain, (spl144_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_125])])).
fof(f5621, plain, ((e20 = op2(e20, e20)) | (~ spl144_42 | ~ spl144_272 | ~ spl144_306)), inference(backward_demodulation, [], [f5543, f2950])).
fof(f5543, plain, ((h4(e11) = op2(h4(e11), h4(e11))) | (~ spl144_42 | ~ spl144_306)), inference(forward_demodulation, [], [f3138, f1347])).
fof(f3138, plain, ((h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ spl144_306), inference(avatar_component_clause, [], [f3137])).
fof(f5529, plain, (~ spl144_106 | ~ spl144_42 | ~ spl144_302 | spl144_363), inference(avatar_split_clause, [], [f5339, f3486, f3104, f1345, f1649])).
fof(f3104, plain, (spl144_302 <=> (e21 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_302])])).
fof(f3486, plain, (spl144_363 <=> (h1(op1(e11, e11)) = op2(h1(e11), h1(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_363])])).
fof(f5339, plain, (~ (e21 = op2(e21, e21)) | (~ spl144_42 | ~ spl144_302 | spl144_363)), inference(forward_demodulation, [], [f5338, f3105])).
fof(f3105, plain, ((e21 = h1(e11)) | ~ spl144_302), inference(avatar_component_clause, [], [f3104])).
fof(f5338, plain, (~ (op2(e21, e21) = h1(e11)) | (~ spl144_42 | ~ spl144_302 | spl144_363)), inference(forward_demodulation, [], [f5337, f1347])).
fof(f5337, plain, (~ (op2(e21, e21) = h1(op1(e11, e11))) | (~ spl144_302 | spl144_363)), inference(forward_demodulation, [], [f3488, f3105])).
fof(f3488, plain, (~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | spl144_363), inference(avatar_component_clause, [], [f3486])).
fof(f5526, plain, (~ spl144_104 | ~ spl144_40 | ~ spl144_302 | ~ spl144_367 | spl144_374), inference(avatar_split_clause, [], [f5368, f3530, f3502, f3104, f1336, f1640])).
fof(f1336, plain, (spl144_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_40])])).
fof(f3530, plain, (spl144_374 <=> (h1(op1(e11, e12)) = op2(h1(e11), e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_374])])).
fof(f5368, plain, (~ (e23 = op2(e21, e22)) | (~ spl144_40 | ~ spl144_302 | ~ spl144_367 | spl144_374)), inference(forward_demodulation, [], [f5367, f3503])).
fof(f5367, plain, (~ (op2(e21, e22) = h1(e13)) | (~ spl144_40 | ~ spl144_302 | spl144_374)), inference(forward_demodulation, [], [f5366, f1338])).
fof(f1338, plain, ((e13 = op1(e11, e12)) | ~ spl144_40), inference(avatar_component_clause, [], [f1336])).
fof(f5366, plain, (~ (op2(e21, e22) = h1(op1(e11, e12))) | (~ spl144_302 | spl144_374)), inference(forward_demodulation, [], [f3532, f3105])).
fof(f3532, plain, (~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | spl144_374), inference(avatar_component_clause, [], [f3530])).
fof(f5525, plain, (~ spl144_76 | ~ spl144_12 | ~ spl144_302 | spl144_365 | ~ spl144_367), inference(avatar_split_clause, [], [f5373, f3502, f3494, f3104, f1217, f1521])).
fof(f3494, plain, (spl144_365 <=> (h1(op1(e13, e11)) = op2(h1(e13), h1(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_365])])).
fof(f5373, plain, (~ (e23 = op2(e23, e21)) | (~ spl144_12 | ~ spl144_302 | spl144_365 | ~ spl144_367)), inference(forward_demodulation, [], [f5372, f3503])).
fof(f5372, plain, (~ (op2(e23, e21) = h1(e13)) | (~ spl144_12 | ~ spl144_302 | spl144_365 | ~ spl144_367)), inference(forward_demodulation, [], [f5371, f1219])).
fof(f5371, plain, (~ (op2(e23, e21) = h1(op1(e13, e11))) | (~ spl144_302 | spl144_365 | ~ spl144_367)), inference(forward_demodulation, [], [f5370, f3503])).
fof(f5370, plain, (~ (h1(op1(e13, e11)) = op2(h1(e13), e21)) | (~ spl144_302 | spl144_365)), inference(forward_demodulation, [], [f3496, f3105])).
fof(f3496, plain, (~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | spl144_365), inference(avatar_component_clause, [], [f3494])).
fof(f5522, plain, (~ spl144_70 | ~ spl144_6 | ~ spl144_302 | ~ spl144_367 | spl144_368), inference(avatar_split_clause, [], [f5335, f3506, f3502, f3104, f1192, f1496])).
fof(f1192, plain, (spl144_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_6])])).
fof(f3506, plain, (spl144_368 <=> (h1(op1(e13, e12)) = op2(h1(e13), e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_368])])).
fof(f5335, plain, (~ (e21 = op2(e23, e22)) | (~ spl144_6 | ~ spl144_302 | ~ spl144_367 | spl144_368)), inference(forward_demodulation, [], [f5334, f3105])).
fof(f5334, plain, (~ (op2(e23, e22) = h1(e11)) | (~ spl144_6 | ~ spl144_367 | spl144_368)), inference(forward_demodulation, [], [f5333, f1194])).
fof(f1194, plain, ((e11 = op1(e13, e12)) | ~ spl144_6), inference(avatar_component_clause, [], [f1192])).
fof(f5333, plain, (~ (op2(e23, e22) = h1(op1(e13, e12))) | (~ spl144_367 | spl144_368)), inference(forward_demodulation, [], [f3508, f3503])).
fof(f3508, plain, (~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | spl144_368), inference(avatar_component_clause, [], [f3506])).
fof(f5517, plain, (~ spl144_65 | ~ spl144_1 | spl144_366 | ~ spl144_367), inference(avatar_split_clause, [], [f5516, f3502, f3498, f1171, f1475])).
fof(f1475, plain, (spl144_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_65])])).
fof(f3498, plain, (spl144_366 <=> (h1(op1(e13, e13)) = op2(h1(e13), h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_366])])).
fof(f5516, plain, (~ (e20 = op2(e23, e23)) | (~ spl144_1 | spl144_366 | ~ spl144_367)), inference(forward_demodulation, [], [f5515, f1090])).
fof(f5515, plain, (~ (op2(e23, e23) = h1(e10)) | (~ spl144_1 | spl144_366 | ~ spl144_367)), inference(forward_demodulation, [], [f5257, f1173])).
fof(f1173, plain, ((e10 = op1(e13, e13)) | ~ spl144_1), inference(avatar_component_clause, [], [f1171])).
fof(f5257, plain, (~ (op2(e23, e23) = h1(op1(e13, e13))) | (spl144_366 | ~ spl144_367)), inference(forward_demodulation, [], [f3500, f3503])).
fof(f3500, plain, (~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | spl144_366), inference(avatar_component_clause, [], [f3498])).
fof(f5509, plain, (~ spl144_56 | ~ spl144_52), inference(avatar_split_clause, [], [f5508, f1387, f1404])).
fof(f5508, plain, (~ (e13 = op1(e10, e12)) | ~ spl144_52), inference(forward_demodulation, [], [f437, f1389])).
fof(f437, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f5506, plain, (~ spl144_18 | ~ spl144_96 | ~ spl144_287 | ~ spl144_294 | spl144_349 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5505])).
fof(f5505, plain, ($false | (~ spl144_18 | ~ spl144_96 | ~ spl144_287 | ~ spl144_294 | spl144_349 | ~ spl144_362)), inference(subsumption_resolution, [], [f5504, f1608])).
fof(f5504, plain, (~ (e23 = op2(e22, e20)) | (~ spl144_18 | ~ spl144_287 | ~ spl144_294 | spl144_349 | ~ spl144_362)), inference(forward_demodulation, [], [f5503, f3463])).
fof(f3463, plain, ((e23 = h2(e11)) | ~ spl144_362), inference(avatar_component_clause, [], [f3462])).
fof(f3462, plain, (spl144_362 <=> (e23 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_362])])).
fof(f5503, plain, (~ (op2(e22, e20) = h2(e11)) | (~ spl144_18 | ~ spl144_287 | ~ spl144_294 | spl144_349)), inference(forward_demodulation, [], [f5502, f1245])).
fof(f5502, plain, (~ (op2(e22, e20) = h2(op1(e12, e13))) | (~ spl144_287 | ~ spl144_294 | spl144_349)), inference(forward_demodulation, [], [f5501, f3026])).
fof(f3026, plain, ((e22 = h2(e12)) | ~ spl144_287), inference(avatar_component_clause, [], [f3025])).
fof(f3025, plain, (spl144_287 <=> (e22 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_287])])).
fof(f5501, plain, (~ (h2(op1(e12, e13)) = op2(h2(e12), e20)) | (~ spl144_294 | spl144_349)), inference(forward_demodulation, [], [f3390, f3062])).
fof(f3062, plain, ((e20 = h2(e13)) | ~ spl144_294), inference(avatar_component_clause, [], [f3061])).
fof(f3061, plain, (spl144_294 <=> (e20 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_294])])).
fof(f3390, plain, (~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | spl144_349), inference(avatar_component_clause, [], [f3388])).
fof(f3388, plain, (spl144_349 <=> (h2(op1(e12, e13)) = op2(h2(e12), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_349])])).
fof(f5494, plain, (~ spl144_53 | ~ spl144_102 | ~ spl144_287 | spl144_358), inference(avatar_contradiction_clause, [], [f5493])).
fof(f5493, plain, ($false | (~ spl144_53 | ~ spl144_102 | ~ spl144_287 | spl144_358)), inference(subsumption_resolution, [], [f5492, f1634])).
fof(f1634, plain, ((e21 = op2(e21, e22)) | ~ spl144_102), inference(avatar_component_clause, [], [f1632])).
fof(f1632, plain, (spl144_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_102])])).
fof(f5492, plain, (~ (e21 = op2(e21, e22)) | (~ spl144_53 | ~ spl144_287 | spl144_358)), inference(forward_demodulation, [], [f5491, f1094])).
fof(f1094, plain, (e21 = h2(e10)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((h2(e13) = op2(op2(e21, op2(e21, e21)), e21)) & (h2(e12) = op2(e21, op2(e21, e21))) & (op2(e21, e21) = h2(e11)) & (e21 = h2(e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax15)).
fof(f5491, plain, (~ (op2(e21, e22) = h2(e10)) | (~ spl144_53 | ~ spl144_287 | spl144_358)), inference(forward_demodulation, [], [f5490, f1394])).
fof(f5490, plain, (~ (op2(e21, e22) = h2(op1(e10, e12))) | (~ spl144_287 | spl144_358)), inference(forward_demodulation, [], [f3426, f3026])).
fof(f3426, plain, (~ (h2(op1(e10, e12)) = op2(e21, h2(e12))) | spl144_358), inference(avatar_component_clause, [], [f3424])).
fof(f3424, plain, (spl144_358 <=> (h2(op1(e10, e12)) = op2(e21, h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_358])])).
fof(f5486, plain, (~ spl144_52 | ~ spl144_109 | ~ spl144_294 | spl144_357), inference(avatar_contradiction_clause, [], [f5485])).
fof(f5485, plain, ($false | (~ spl144_52 | ~ spl144_109 | ~ spl144_294 | spl144_357)), inference(subsumption_resolution, [], [f5484, f1664])).
fof(f5484, plain, (~ (e20 = op2(e21, e20)) | (~ spl144_52 | ~ spl144_294 | spl144_357)), inference(forward_demodulation, [], [f5483, f3062])).
fof(f5483, plain, (~ (op2(e21, e20) = h2(e13)) | (~ spl144_52 | ~ spl144_294 | spl144_357)), inference(forward_demodulation, [], [f5482, f1389])).
fof(f5482, plain, (~ (op2(e21, e20) = h2(op1(e10, e13))) | (~ spl144_294 | spl144_357)), inference(forward_demodulation, [], [f3422, f3062])).
fof(f3422, plain, (~ (h2(op1(e10, e13)) = op2(e21, h2(e13))) | spl144_357), inference(avatar_component_clause, [], [f3420])).
fof(f3420, plain, (spl144_357 <=> (h2(op1(e10, e13)) = op2(e21, h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_357])])).
fof(f5481, plain, (~ spl144_45 | ~ spl144_74 | spl144_356 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5480])).
fof(f5480, plain, ($false | (~ spl144_45 | ~ spl144_74 | spl144_356 | ~ spl144_362)), inference(subsumption_resolution, [], [f5479, f1515])).
fof(f1515, plain, ((e21 = op2(e23, e21)) | ~ spl144_74), inference(avatar_component_clause, [], [f1513])).
fof(f1513, plain, (spl144_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_74])])).
fof(f5479, plain, (~ (e21 = op2(e23, e21)) | (~ spl144_45 | spl144_356 | ~ spl144_362)), inference(forward_demodulation, [], [f5478, f1094])).
fof(f5478, plain, (~ (op2(e23, e21) = h2(e10)) | (~ spl144_45 | spl144_356 | ~ spl144_362)), inference(forward_demodulation, [], [f5477, f1360])).
fof(f5477, plain, (~ (op2(e23, e21) = h2(op1(e11, e10))) | (spl144_356 | ~ spl144_362)), inference(forward_demodulation, [], [f3418, f3463])).
fof(f3418, plain, (~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | spl144_356), inference(avatar_component_clause, [], [f3416])).
fof(f3416, plain, (spl144_356 <=> (h2(op1(e11, e10)) = op2(h2(e11), e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_356])])).
fof(f5473, plain, (~ spl144_32 | spl144_355), inference(avatar_contradiction_clause, [], [f5472])).
fof(f5472, plain, ($false | (~ spl144_32 | spl144_355)), inference(trivial_inequality_removal, [], [f5471])).
fof(f5471, plain, (~ (h2(e13) = h2(e13)) | (~ spl144_32 | spl144_355)), inference(forward_demodulation, [], [f3414, f1304])).
fof(f1304, plain, ((e13 = op1(e12, e10)) | ~ spl144_32), inference(avatar_component_clause, [], [f1302])).
fof(f1302, plain, (spl144_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_32])])).
fof(f3414, plain, (~ (h2(e13) = h2(op1(e12, e10))) | spl144_355), inference(avatar_component_clause, [], [f3412])).
fof(f3412, plain, (spl144_355 <=> (h2(e13) = h2(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_355])])).
fof(f5466, plain, (~ spl144_1 | ~ spl144_126 | ~ spl144_294 | spl144_352), inference(avatar_contradiction_clause, [], [f5465])).
fof(f5465, plain, ($false | (~ spl144_1 | ~ spl144_126 | ~ spl144_294 | spl144_352)), inference(subsumption_resolution, [], [f5464, f1736])).
fof(f5464, plain, (~ (op2(e20, e20) = e21) | (~ spl144_1 | ~ spl144_294 | spl144_352)), inference(forward_demodulation, [], [f5463, f1094])).
fof(f5463, plain, (~ (op2(e20, e20) = h2(e10)) | (~ spl144_1 | ~ spl144_294 | spl144_352)), inference(forward_demodulation, [], [f5462, f1173])).
fof(f5462, plain, (~ (op2(e20, e20) = h2(op1(e13, e13))) | (~ spl144_294 | spl144_352)), inference(forward_demodulation, [], [f3402, f3062])).
fof(f3402, plain, (~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | spl144_352), inference(avatar_component_clause, [], [f3400])).
fof(f3400, plain, (spl144_352 <=> (h2(op1(e13, e13)) = op2(h2(e13), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_352])])).
fof(f5457, plain, (~ spl144_6 | ~ spl144_120 | ~ spl144_287 | ~ spl144_294 | spl144_351 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5456])).
fof(f5456, plain, ($false | (~ spl144_6 | ~ spl144_120 | ~ spl144_287 | ~ spl144_294 | spl144_351 | ~ spl144_362)), inference(subsumption_resolution, [], [f5455, f1710])).
fof(f1710, plain, ((e23 = op2(e20, e22)) | ~ spl144_120), inference(avatar_component_clause, [], [f1708])).
fof(f1708, plain, (spl144_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_120])])).
fof(f5455, plain, (~ (e23 = op2(e20, e22)) | (~ spl144_6 | ~ spl144_287 | ~ spl144_294 | spl144_351 | ~ spl144_362)), inference(forward_demodulation, [], [f5454, f3463])).
fof(f5454, plain, (~ (op2(e20, e22) = h2(e11)) | (~ spl144_6 | ~ spl144_287 | ~ spl144_294 | spl144_351)), inference(forward_demodulation, [], [f5453, f1194])).
fof(f5453, plain, (~ (op2(e20, e22) = h2(op1(e13, e12))) | (~ spl144_287 | ~ spl144_294 | spl144_351)), inference(forward_demodulation, [], [f5452, f3062])).
fof(f5452, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | (~ spl144_287 | spl144_351)), inference(forward_demodulation, [], [f3398, f3026])).
fof(f3398, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | spl144_351), inference(avatar_component_clause, [], [f3396])).
fof(f3396, plain, (spl144_351 <=> (h2(op1(e13, e12)) = op2(h2(e13), h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_351])])).
fof(f5447, plain, (~ spl144_25 | ~ spl144_82 | ~ spl144_287 | spl144_347 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5446])).
fof(f5446, plain, ($false | (~ spl144_25 | ~ spl144_82 | ~ spl144_287 | spl144_347 | ~ spl144_362)), inference(subsumption_resolution, [], [f5445, f1549])).
fof(f5445, plain, (~ (e21 = op2(e22, e23)) | (~ spl144_25 | ~ spl144_287 | spl144_347 | ~ spl144_362)), inference(forward_demodulation, [], [f5444, f1094])).
fof(f5444, plain, (~ (op2(e22, e23) = h2(e10)) | (~ spl144_25 | ~ spl144_287 | spl144_347 | ~ spl144_362)), inference(forward_demodulation, [], [f5443, f1275])).
fof(f5443, plain, (~ (op2(e22, e23) = h2(op1(e12, e11))) | (~ spl144_287 | spl144_347 | ~ spl144_362)), inference(forward_demodulation, [], [f5442, f3026])).
fof(f5442, plain, (~ (h2(op1(e12, e11)) = op2(h2(e12), e23)) | (spl144_347 | ~ spl144_362)), inference(forward_demodulation, [], [f3382, f3463])).
fof(f3382, plain, (~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | spl144_347), inference(avatar_component_clause, [], [f3380])).
fof(f3380, plain, (spl144_347 <=> (h2(op1(e12, e11)) = op2(h2(e12), h2(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_347])])).
fof(f5441, plain, (~ spl144_40 | ~ spl144_69 | ~ spl144_287 | ~ spl144_294 | spl144_345 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5440])).
fof(f5440, plain, ($false | (~ spl144_40 | ~ spl144_69 | ~ spl144_287 | ~ spl144_294 | spl144_345 | ~ spl144_362)), inference(subsumption_resolution, [], [f5439, f1494])).
fof(f1494, plain, ((e20 = op2(e23, e22)) | ~ spl144_69), inference(avatar_component_clause, [], [f1492])).
fof(f1492, plain, (spl144_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_69])])).
fof(f5439, plain, (~ (e20 = op2(e23, e22)) | (~ spl144_40 | ~ spl144_287 | ~ spl144_294 | spl144_345 | ~ spl144_362)), inference(forward_demodulation, [], [f5438, f3062])).
fof(f5438, plain, (~ (op2(e23, e22) = h2(e13)) | (~ spl144_40 | ~ spl144_287 | spl144_345 | ~ spl144_362)), inference(forward_demodulation, [], [f5437, f1338])).
fof(f5437, plain, (~ (op2(e23, e22) = h2(op1(e11, e12))) | (~ spl144_287 | spl144_345 | ~ spl144_362)), inference(forward_demodulation, [], [f5436, f3463])).
fof(f5436, plain, (~ (h2(op1(e11, e12)) = op2(h2(e11), e22)) | (~ spl144_287 | spl144_345)), inference(forward_demodulation, [], [f3374, f3026])).
fof(f3374, plain, (~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | spl144_345), inference(avatar_component_clause, [], [f3372])).
fof(f3372, plain, (spl144_345 <=> (h2(op1(e11, e12)) = op2(h2(e11), h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_345])])).
fof(f5424, plain, (~ spl144_12 | ~ spl144_113 | ~ spl144_294 | spl144_350 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5423])).
fof(f5423, plain, ($false | (~ spl144_12 | ~ spl144_113 | ~ spl144_294 | spl144_350 | ~ spl144_362)), inference(subsumption_resolution, [], [f5422, f1681])).
fof(f1681, plain, ((e20 = op2(e20, e23)) | ~ spl144_113), inference(avatar_component_clause, [], [f1679])).
fof(f1679, plain, (spl144_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_113])])).
fof(f5422, plain, (~ (e20 = op2(e20, e23)) | (~ spl144_12 | ~ spl144_294 | spl144_350 | ~ spl144_362)), inference(forward_demodulation, [], [f5421, f3062])).
fof(f5421, plain, (~ (op2(e20, e23) = h2(e13)) | (~ spl144_12 | ~ spl144_294 | spl144_350 | ~ spl144_362)), inference(forward_demodulation, [], [f5420, f1219])).
fof(f5420, plain, (~ (op2(e20, e23) = h2(op1(e13, e11))) | (~ spl144_294 | spl144_350 | ~ spl144_362)), inference(forward_demodulation, [], [f5419, f3062])).
fof(f5419, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e23)) | (spl144_350 | ~ spl144_362)), inference(forward_demodulation, [], [f3394, f3463])).
fof(f3394, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | spl144_350), inference(avatar_component_clause, [], [f3392])).
fof(f3392, plain, (spl144_350 <=> (h2(op1(e13, e11)) = op2(h2(e13), h2(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_350])])).
fof(f5403, plain, (~ spl144_23 | ~ spl144_87 | ~ spl144_287 | spl144_348), inference(avatar_contradiction_clause, [], [f5402])).
fof(f5402, plain, ($false | (~ spl144_23 | ~ spl144_87 | ~ spl144_287 | spl144_348)), inference(subsumption_resolution, [], [f5401, f1570])).
fof(f5401, plain, (~ (e22 = op2(e22, e22)) | (~ spl144_23 | ~ spl144_287 | spl144_348)), inference(forward_demodulation, [], [f5400, f3026])).
fof(f5400, plain, (~ (op2(e22, e22) = h2(e12)) | (~ spl144_23 | ~ spl144_287 | spl144_348)), inference(forward_demodulation, [], [f5399, f1266])).
fof(f5399, plain, (~ (op2(e22, e22) = h2(op1(e12, e12))) | (~ spl144_287 | spl144_348)), inference(forward_demodulation, [], [f3386, f3026])).
fof(f3386, plain, (~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | spl144_348), inference(avatar_component_clause, [], [f3384])).
fof(f3384, plain, (spl144_348 <=> (h2(op1(e12, e12)) = op2(h2(e12), h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_348])])).
fof(f5383, plain, (~ spl144_35 | ~ spl144_79 | ~ spl144_287 | ~ spl144_294 | spl144_346 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5382])).
fof(f5382, plain, ($false | (~ spl144_35 | ~ spl144_79 | ~ spl144_287 | ~ spl144_294 | spl144_346 | ~ spl144_362)), inference(subsumption_resolution, [], [f5381, f1536])).
fof(f5381, plain, (~ (e22 = op2(e23, e20)) | (~ spl144_35 | ~ spl144_287 | ~ spl144_294 | spl144_346 | ~ spl144_362)), inference(forward_demodulation, [], [f5380, f3026])).
fof(f5380, plain, (~ (op2(e23, e20) = h2(e12)) | (~ spl144_35 | ~ spl144_294 | spl144_346 | ~ spl144_362)), inference(forward_demodulation, [], [f5379, f1317])).
fof(f5379, plain, (~ (op2(e23, e20) = h2(op1(e11, e13))) | (~ spl144_294 | spl144_346 | ~ spl144_362)), inference(forward_demodulation, [], [f5378, f3463])).
fof(f5378, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), e20)) | (~ spl144_294 | spl144_346)), inference(forward_demodulation, [], [f3378, f3062])).
fof(f3378, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | spl144_346), inference(avatar_component_clause, [], [f3376])).
fof(f3376, plain, (spl144_346 <=> (h2(op1(e11, e13)) = op2(h2(e11), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_346])])).
fof(f5344, plain, (~ spl144_15 | ~ spl144_123 | ~ spl144_287 | ~ spl144_294 | spl144_354), inference(avatar_contradiction_clause, [], [f5343])).
fof(f5343, plain, ($false | (~ spl144_15 | ~ spl144_123 | ~ spl144_287 | ~ spl144_294 | spl144_354)), inference(subsumption_resolution, [], [f5342, f1723])).
fof(f5342, plain, (~ (e22 = op2(e20, e21)) | (~ spl144_15 | ~ spl144_287 | ~ spl144_294 | spl144_354)), inference(forward_demodulation, [], [f5341, f3026])).
fof(f5341, plain, (~ (op2(e20, e21) = h2(e12)) | (~ spl144_15 | ~ spl144_294 | spl144_354)), inference(forward_demodulation, [], [f5340, f1232])).
fof(f5340, plain, (~ (op2(e20, e21) = h2(op1(e13, e10))) | (~ spl144_294 | spl144_354)), inference(forward_demodulation, [], [f3410, f3062])).
fof(f3410, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | spl144_354), inference(avatar_component_clause, [], [f3408])).
fof(f3408, plain, (spl144_354 <=> (h2(op1(e13, e10)) = op2(h2(e13), e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_354])])).
fof(f5297, plain, (~ spl144_42 | ~ spl144_68 | spl144_344 | ~ spl144_362), inference(avatar_contradiction_clause, [], [f5296])).
fof(f5296, plain, ($false | (~ spl144_42 | ~ spl144_68 | spl144_344 | ~ spl144_362)), inference(subsumption_resolution, [], [f5289, f3463])).
fof(f5289, plain, (~ (e23 = h2(e11)) | (~ spl144_42 | ~ spl144_68 | spl144_344 | ~ spl144_362)), inference(backward_demodulation, [], [f5241, f1347])).
fof(f5241, plain, (~ (e23 = h2(op1(e11, e11))) | (~ spl144_68 | spl144_344 | ~ spl144_362)), inference(forward_demodulation, [], [f4906, f1489])).
fof(f1489, plain, ((e23 = op2(e23, e23)) | ~ spl144_68), inference(avatar_component_clause, [], [f1487])).
fof(f1487, plain, (spl144_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_68])])).
fof(f4906, plain, (~ (op2(e23, e23) = h2(op1(e11, e11))) | (spl144_344 | ~ spl144_362)), inference(forward_demodulation, [], [f3370, f3463])).
fof(f3370, plain, (~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | spl144_344), inference(avatar_component_clause, [], [f3368])).
fof(f3368, plain, (spl144_344 <=> (h2(op1(e11, e11)) = op2(h2(e11), h2(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_344])])).
fof(f5295, plain, (~ spl144_42 | ~ spl144_108 | ~ spl144_302 | ~ spl144_363), inference(avatar_contradiction_clause, [], [f5294])).
fof(f5294, plain, ($false | (~ spl144_42 | ~ spl144_108 | ~ spl144_302 | ~ spl144_363)), inference(subsumption_resolution, [], [f5293, f514])).
fof(f514, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax8)).
fof(f5293, plain, ((e21 = e23) | (~ spl144_42 | ~ spl144_108 | ~ spl144_302 | ~ spl144_363)), inference(forward_demodulation, [], [f5288, f3105])).
fof(f5288, plain, ((e23 = h1(e11)) | (~ spl144_42 | ~ spl144_108 | ~ spl144_302 | ~ spl144_363)), inference(backward_demodulation, [], [f5240, f1347])).
fof(f5240, plain, ((e23 = h1(op1(e11, e11))) | (~ spl144_108 | ~ spl144_302 | ~ spl144_363)), inference(forward_demodulation, [], [f5000, f1659])).
fof(f1659, plain, ((e23 = op2(e21, e21)) | ~ spl144_108), inference(avatar_component_clause, [], [f1657])).
fof(f1657, plain, (spl144_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_108])])).
fof(f5000, plain, ((op2(e21, e21) = h1(op1(e11, e11))) | (~ spl144_302 | ~ spl144_363)), inference(forward_demodulation, [], [f3487, f3105])).
fof(f3487, plain, ((h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ spl144_363), inference(avatar_component_clause, [], [f3486])).
fof(f5281, plain, (~ spl144_52 | ~ spl144_113 | spl144_304 | ~ spl144_367 | ~ spl144_376), inference(avatar_contradiction_clause, [], [f5280])).
fof(f5280, plain, ($false | (~ spl144_52 | ~ spl144_113 | spl144_304 | ~ spl144_367 | ~ spl144_376)), inference(subsumption_resolution, [], [f5277, f3116])).
fof(f3116, plain, (~ (e20 = h1(e13)) | spl144_304), inference(avatar_component_clause, [], [f3114])).
fof(f3114, plain, (spl144_304 <=> (e20 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_304])])).
fof(f5277, plain, ((e20 = h1(e13)) | (~ spl144_52 | ~ spl144_113 | ~ spl144_367 | ~ spl144_376)), inference(backward_demodulation, [], [f5247, f1389])).
fof(f5247, plain, ((e20 = h1(op1(e10, e13))) | (~ spl144_113 | ~ spl144_367 | ~ spl144_376)), inference(forward_demodulation, [], [f4965, f1681])).
fof(f4965, plain, ((op2(e20, e23) = h1(op1(e10, e13))) | (~ spl144_367 | ~ spl144_376)), inference(forward_demodulation, [], [f3539, f3503])).
fof(f3539, plain, ((h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ spl144_376), inference(avatar_component_clause, [], [f3538])).
fof(f5276, plain, (~ spl144_53 | ~ spl144_120 | ~ spl144_377), inference(avatar_contradiction_clause, [], [f5275])).
fof(f5275, plain, ($false | (~ spl144_53 | ~ spl144_120 | ~ spl144_377)), inference(subsumption_resolution, [], [f5274, f512])).
fof(f512, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f5274, plain, ((e20 = e23) | (~ spl144_53 | ~ spl144_120 | ~ spl144_377)), inference(forward_demodulation, [], [f5271, f1090])).
fof(f5271, plain, ((e23 = h1(e10)) | (~ spl144_53 | ~ spl144_120 | ~ spl144_377)), inference(backward_demodulation, [], [f5252, f1394])).
fof(f5252, plain, ((e23 = h1(op1(e10, e12))) | (~ spl144_120 | ~ spl144_377)), inference(forward_demodulation, [], [f3543, f1710])).
fof(f3543, plain, ((op2(e20, e22) = h1(op1(e10, e12))) | ~ spl144_377), inference(avatar_component_clause, [], [f3542])).
fof(f5220, plain, (~ spl144_62 | spl144_322), inference(avatar_contradiction_clause, [], [f5219])).
fof(f5219, plain, ($false | (~ spl144_62 | spl144_322)), inference(trivial_inequality_removal, [], [f5218])).
fof(f5218, plain, (~ (h4(e11) = h4(e11)) | (~ spl144_62 | spl144_322)), inference(forward_demodulation, [], [f3203, f1432])).
fof(f1432, plain, ((op1(e10, e10) = e11) | ~ spl144_62), inference(avatar_component_clause, [], [f1430])).
fof(f1430, plain, (spl144_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl144_62])])).
fof(f3203, plain, (~ (h4(e11) = h4(op1(e10, e10))) | spl144_322), inference(avatar_component_clause, [], [f3201])).
fof(f3201, plain, (spl144_322 <=> (h4(e11) = h4(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_322])])).
fof(f5206, plain, (~ spl144_18 | ~ spl144_82 | ~ spl144_302 | ~ spl144_367 | spl144_370), inference(avatar_contradiction_clause, [], [f5205])).
fof(f5205, plain, ($false | (~ spl144_18 | ~ spl144_82 | ~ spl144_302 | ~ spl144_367 | spl144_370)), inference(subsumption_resolution, [], [f5204, f1549])).
fof(f5204, plain, (~ (e21 = op2(e22, e23)) | (~ spl144_18 | ~ spl144_302 | ~ spl144_367 | spl144_370)), inference(forward_demodulation, [], [f5203, f3105])).
fof(f5203, plain, (~ (op2(e22, e23) = h1(e11)) | (~ spl144_18 | ~ spl144_367 | spl144_370)), inference(forward_demodulation, [], [f5202, f1245])).
fof(f5202, plain, (~ (op2(e22, e23) = h1(op1(e12, e13))) | (~ spl144_367 | spl144_370)), inference(forward_demodulation, [], [f3516, f3503])).
fof(f3516, plain, (~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | spl144_370), inference(avatar_component_clause, [], [f3514])).
fof(f3514, plain, (spl144_370 <=> (h1(op1(e12, e13)) = op2(e22, h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_370])])).
fof(f5198, plain, (~ spl144_59 | spl144_321), inference(avatar_contradiction_clause, [], [f5197])).
fof(f5197, plain, ($false | (~ spl144_59 | spl144_321)), inference(trivial_inequality_removal, [], [f5196])).
fof(f5196, plain, (~ (h4(e12) = h4(e12)) | (~ spl144_59 | spl144_321)), inference(forward_demodulation, [], [f3199, f1419])).
fof(f1419, plain, ((e12 = op1(e10, e11)) | ~ spl144_59), inference(avatar_component_clause, [], [f1417])).
fof(f1417, plain, (spl144_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_59])])).
fof(f3199, plain, (~ (h4(e12) = h4(op1(e10, e11))) | spl144_321), inference(avatar_component_clause, [], [f3197])).
fof(f3197, plain, (spl144_321 <=> (h4(e12) = h4(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_321])])).
fof(f5187, plain, (~ spl144_5 | ~ spl144_69 | ~ spl144_367 | spl144_368), inference(avatar_contradiction_clause, [], [f5186])).
fof(f5186, plain, ($false | (~ spl144_5 | ~ spl144_69 | ~ spl144_367 | spl144_368)), inference(subsumption_resolution, [], [f5185, f1494])).
fof(f5185, plain, (~ (e20 = op2(e23, e22)) | (~ spl144_5 | ~ spl144_367 | spl144_368)), inference(forward_demodulation, [], [f5184, f1090])).
fof(f5184, plain, (~ (op2(e23, e22) = h1(e10)) | (~ spl144_5 | ~ spl144_367 | spl144_368)), inference(forward_demodulation, [], [f5183, f1190])).
fof(f5183, plain, (~ (op2(e23, e22) = h1(op1(e13, e12))) | (~ spl144_367 | spl144_368)), inference(forward_demodulation, [], [f3508, f3503])).
fof(f5163, plain, (~ spl144_4 | ~ spl144_68 | spl144_366 | ~ spl144_367), inference(avatar_contradiction_clause, [], [f5162])).
fof(f5162, plain, ($false | (~ spl144_4 | ~ spl144_68 | spl144_366 | ~ spl144_367)), inference(subsumption_resolution, [], [f5161, f1489])).
fof(f5161, plain, (~ (e23 = op2(e23, e23)) | (~ spl144_4 | spl144_366 | ~ spl144_367)), inference(forward_demodulation, [], [f5160, f3503])).
fof(f5160, plain, (~ (op2(e23, e23) = h1(e13)) | (~ spl144_4 | spl144_366 | ~ spl144_367)), inference(forward_demodulation, [], [f5159, f1185])).
fof(f5159, plain, (~ (op2(e23, e23) = h1(op1(e13, e13))) | (spl144_366 | ~ spl144_367)), inference(forward_demodulation, [], [f3500, f3503])).
fof(f5140, plain, (~ spl144_10 | ~ spl144_74 | ~ spl144_302 | spl144_365 | ~ spl144_367), inference(avatar_contradiction_clause, [], [f5139])).
fof(f5139, plain, ($false | (~ spl144_10 | ~ spl144_74 | ~ spl144_302 | spl144_365 | ~ spl144_367)), inference(subsumption_resolution, [], [f5138, f1515])).
fof(f5138, plain, (~ (e21 = op2(e23, e21)) | (~ spl144_10 | ~ spl144_302 | spl144_365 | ~ spl144_367)), inference(forward_demodulation, [], [f5137, f3105])).
fof(f5137, plain, (~ (op2(e23, e21) = h1(e11)) | (~ spl144_10 | ~ spl144_302 | spl144_365 | ~ spl144_367)), inference(forward_demodulation, [], [f5136, f1211])).
fof(f5136, plain, (~ (op2(e23, e21) = h1(op1(e13, e11))) | (~ spl144_302 | spl144_365 | ~ spl144_367)), inference(forward_demodulation, [], [f5135, f3503])).
fof(f5135, plain, (~ (h1(op1(e13, e11)) = op2(h1(e13), e21)) | (~ spl144_302 | spl144_365)), inference(forward_demodulation, [], [f3496, f3105])).
fof(f5114, plain, (~ spl144_38 | ~ spl144_102 | ~ spl144_302 | spl144_374), inference(avatar_contradiction_clause, [], [f5113])).
fof(f5113, plain, ($false | (~ spl144_38 | ~ spl144_102 | ~ spl144_302 | spl144_374)), inference(subsumption_resolution, [], [f5112, f1634])).
fof(f5112, plain, (~ (e21 = op2(e21, e22)) | (~ spl144_38 | ~ spl144_302 | spl144_374)), inference(forward_demodulation, [], [f5111, f3105])).
fof(f5111, plain, (~ (op2(e21, e22) = h1(e11)) | (~ spl144_38 | ~ spl144_302 | spl144_374)), inference(forward_demodulation, [], [f5110, f1330])).
fof(f5110, plain, (~ (op2(e21, e22) = h1(op1(e11, e12))) | (~ spl144_302 | spl144_374)), inference(forward_demodulation, [], [f3532, f3105])).
fof(f5106, plain, (~ spl144_32 | spl144_317), inference(avatar_contradiction_clause, [], [f5105])).
fof(f5105, plain, ($false | (~ spl144_32 | spl144_317)), inference(trivial_inequality_removal, [], [f5104])).
fof(f5104, plain, (~ (h4(e13) = h4(e13)) | (~ spl144_32 | spl144_317)), inference(forward_demodulation, [], [f3183, f1304])).
fof(f3183, plain, (~ (h4(e13) = h4(op1(e12, e10))) | spl144_317), inference(avatar_component_clause, [], [f3181])).
fof(f3181, plain, (spl144_317 <=> (h4(e13) = h4(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_317])])).
fof(f5095, plain, (~ spl144_15 | ~ spl144_79 | ~ spl144_367 | spl144_369), inference(avatar_contradiction_clause, [], [f5094])).
fof(f5094, plain, ($false | (~ spl144_15 | ~ spl144_79 | ~ spl144_367 | spl144_369)), inference(subsumption_resolution, [], [f5093, f1536])).
fof(f5093, plain, (~ (e22 = op2(e23, e20)) | (~ spl144_15 | ~ spl144_367 | spl144_369)), inference(forward_demodulation, [], [f5092, f2863])).
fof(f2863, plain, (e22 = h1(e12)), inference(forward_demodulation, [], [f1092, f1088])).
fof(f1088, plain, (e22 = op2(e20, op2(e20, e20))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(op2(e20, op2(e20, e20)), e20)) & (e22 = op2(e20, op2(e20, e20))) & (op2(e20, e20) = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax13)).
fof(f1092, plain, (op2(e20, op2(e20, e20)) = h1(e12)), inference(cnf_transformation, [], [f14])).
fof(f5092, plain, (~ (op2(e23, e20) = h1(e12)) | (~ spl144_15 | ~ spl144_367 | spl144_369)), inference(forward_demodulation, [], [f5091, f1232])).
fof(f5091, plain, (~ (op2(e23, e20) = h1(op1(e13, e10))) | (~ spl144_367 | spl144_369)), inference(forward_demodulation, [], [f3512, f3503])).
fof(f3512, plain, (~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | spl144_369), inference(avatar_component_clause, [], [f3510])).
fof(f3510, plain, (spl144_369 <=> (h1(op1(e13, e10)) = op2(h1(e13), e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_369])])).
fof(f5071, plain, (~ spl144_62 | spl144_379), inference(avatar_contradiction_clause, [], [f5070])).
fof(f5070, plain, ($false | (~ spl144_62 | spl144_379)), inference(trivial_inequality_removal, [], [f5069])).
fof(f5069, plain, (~ (h1(e11) = h1(e11)) | (~ spl144_62 | spl144_379)), inference(forward_demodulation, [], [f3552, f1432])).
fof(f3552, plain, (~ (h1(e11) = h1(op1(e10, e10))) | spl144_379), inference(avatar_component_clause, [], [f3550])).
fof(f3550, plain, (spl144_379 <=> (h1(e11) = h1(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_379])])).
fof(f5051, plain, (~ spl144_25 | ~ spl144_89 | ~ spl144_302 | spl144_372), inference(avatar_contradiction_clause, [], [f5050])).
fof(f5050, plain, ($false | (~ spl144_25 | ~ spl144_89 | ~ spl144_302 | spl144_372)), inference(subsumption_resolution, [], [f5049, f1579])).
fof(f5049, plain, (~ (e20 = op2(e22, e21)) | (~ spl144_25 | ~ spl144_302 | spl144_372)), inference(forward_demodulation, [], [f5048, f1090])).
fof(f5048, plain, (~ (op2(e22, e21) = h1(e10)) | (~ spl144_25 | ~ spl144_302 | spl144_372)), inference(forward_demodulation, [], [f5047, f1275])).
fof(f5047, plain, (~ (op2(e22, e21) = h1(op1(e12, e11))) | (~ spl144_302 | spl144_372)), inference(forward_demodulation, [], [f3524, f3105])).
fof(f3524, plain, (~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | spl144_372), inference(avatar_component_clause, [], [f3522])).
fof(f3522, plain, (spl144_372 <=> (h1(op1(e12, e11)) = op2(e22, h1(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_372])])).
fof(f5027, plain, (~ spl144_32 | spl144_373), inference(avatar_contradiction_clause, [], [f5026])).
fof(f5026, plain, ($false | (~ spl144_32 | spl144_373)), inference(trivial_inequality_removal, [], [f5025])).
fof(f5025, plain, (~ (h1(e13) = h1(e13)) | (~ spl144_32 | spl144_373)), inference(forward_demodulation, [], [f3528, f1304])).
fof(f3528, plain, (~ (h1(e13) = h1(op1(e12, e10))) | spl144_373), inference(avatar_component_clause, [], [f3526])).
fof(f3526, plain, (spl144_373 <=> (h1(e13) = h1(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_373])])).
fof(f4999, plain, (~ spl144_45 | ~ spl144_109 | ~ spl144_302 | spl144_375), inference(avatar_contradiction_clause, [], [f4998])).
fof(f4998, plain, ($false | (~ spl144_45 | ~ spl144_109 | ~ spl144_302 | spl144_375)), inference(subsumption_resolution, [], [f4997, f1664])).
fof(f4997, plain, (~ (e20 = op2(e21, e20)) | (~ spl144_45 | ~ spl144_302 | spl144_375)), inference(forward_demodulation, [], [f4996, f1090])).
fof(f4996, plain, (~ (op2(e21, e20) = h1(e10)) | (~ spl144_45 | ~ spl144_302 | spl144_375)), inference(forward_demodulation, [], [f4995, f1360])).
fof(f4995, plain, (~ (op2(e21, e20) = h1(op1(e11, e10))) | (~ spl144_302 | spl144_375)), inference(forward_demodulation, [], [f3536, f3105])).
fof(f3536, plain, (~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | spl144_375), inference(avatar_component_clause, [], [f3534])).
fof(f3534, plain, (spl144_375 <=> (h1(op1(e11, e10)) = op2(h1(e11), e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_375])])).
fof(f4972, plain, (~ spl144_44 | ~ spl144_108 | ~ spl144_302 | spl144_363 | ~ spl144_367), inference(avatar_contradiction_clause, [], [f4971])).
fof(f4971, plain, ($false | (~ spl144_44 | ~ spl144_108 | ~ spl144_302 | spl144_363 | ~ spl144_367)), inference(subsumption_resolution, [], [f4970, f1659])).
fof(f4970, plain, (~ (e23 = op2(e21, e21)) | (~ spl144_44 | ~ spl144_302 | spl144_363 | ~ spl144_367)), inference(forward_demodulation, [], [f4969, f3503])).
fof(f4969, plain, (~ (op2(e21, e21) = h1(e13)) | (~ spl144_44 | ~ spl144_302 | spl144_363)), inference(forward_demodulation, [], [f4968, f1355])).
fof(f4968, plain, (~ (op2(e21, e21) = h1(op1(e11, e11))) | (~ spl144_302 | spl144_363)), inference(forward_demodulation, [], [f3488, f3105])).
fof(f4944, plain, (~ spl144_49 | ~ spl144_113 | ~ spl144_367 | spl144_376), inference(avatar_contradiction_clause, [], [f4943])).
fof(f4943, plain, ($false | (~ spl144_49 | ~ spl144_113 | ~ spl144_367 | spl144_376)), inference(subsumption_resolution, [], [f4942, f1681])).
fof(f4942, plain, (~ (e20 = op2(e20, e23)) | (~ spl144_49 | ~ spl144_367 | spl144_376)), inference(forward_demodulation, [], [f4941, f1090])).
fof(f4941, plain, (~ (op2(e20, e23) = h1(e10)) | (~ spl144_49 | ~ spl144_367 | spl144_376)), inference(forward_demodulation, [], [f4940, f1377])).
fof(f4940, plain, (~ (op2(e20, e23) = h1(op1(e10, e13))) | (~ spl144_367 | spl144_376)), inference(forward_demodulation, [], [f3540, f3503])).
fof(f4934, plain, (spl144_294 | ~ spl144_89 | ~ spl144_287), inference(avatar_split_clause, [], [f4933, f3025, f1577, f3061])).
fof(f4933, plain, ((e20 = h2(e13)) | (~ spl144_89 | ~ spl144_287)), inference(forward_demodulation, [], [f4932, f1579])).
fof(f4932, plain, ((op2(e22, e21) = h2(e13)) | ~ spl144_287), inference(forward_demodulation, [], [f2871, f3026])).
fof(f2871, plain, (h2(e13) = op2(h2(e12), e21)), inference(backward_demodulation, [], [f1097, f1096])).
fof(f1096, plain, (h2(e12) = op2(e21, op2(e21, e21))), inference(cnf_transformation, [], [f15])).
fof(f1097, plain, (h2(e13) = op2(op2(e21, op2(e21, e21)), e21)), inference(cnf_transformation, [], [f15])).
fof(f4905, plain, (~ spl144_56 | ~ spl144_120 | ~ spl144_367 | spl144_377), inference(avatar_contradiction_clause, [], [f4904])).
fof(f4904, plain, ($false | (~ spl144_56 | ~ spl144_120 | ~ spl144_367 | spl144_377)), inference(subsumption_resolution, [], [f4903, f1710])).
fof(f4903, plain, (~ (e23 = op2(e20, e22)) | (~ spl144_56 | ~ spl144_367 | spl144_377)), inference(forward_demodulation, [], [f4902, f3503])).
fof(f4902, plain, (~ (op2(e20, e22) = h1(e13)) | (~ spl144_56 | spl144_377)), inference(forward_demodulation, [], [f3544, f1406])).
fof(f4886, plain, (~ spl144_59 | spl144_359), inference(avatar_contradiction_clause, [], [f4885])).
fof(f4885, plain, ($false | (~ spl144_59 | spl144_359)), inference(trivial_inequality_removal, [], [f4884])).
fof(f4884, plain, (~ (h2(e12) = h2(e12)) | (~ spl144_59 | spl144_359)), inference(forward_demodulation, [], [f3430, f1419])).
fof(f3430, plain, (~ (h2(e12) = h2(op1(e10, e11))) | spl144_359), inference(avatar_component_clause, [], [f3428])).
fof(f3428, plain, (spl144_359 <=> (h2(e12) = h2(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_359])])).
fof(f4879, plain, (~ spl144_59 | spl144_378), inference(avatar_contradiction_clause, [], [f4878])).
fof(f4878, plain, ($false | (~ spl144_59 | spl144_378)), inference(subsumption_resolution, [], [f4877, f2863])).
fof(f4877, plain, (~ (e22 = h1(e12)) | (~ spl144_59 | spl144_378)), inference(forward_demodulation, [], [f3548, f1419])).
fof(f3548, plain, (~ (e22 = h1(op1(e10, e11))) | spl144_378), inference(avatar_component_clause, [], [f3546])).
fof(f3546, plain, (spl144_378 <=> (e22 = h1(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl144_378])])).
fof(f4875, plain, (spl144_287 | ~ spl144_99 | ~ spl144_362), inference(avatar_split_clause, [], [f4874, f3462, f1619, f3025])).
fof(f4874, plain, ((e22 = h2(e12)) | (~ spl144_99 | ~ spl144_362)), inference(forward_demodulation, [], [f4441, f1621])).
fof(f4441, plain, ((op2(e21, e23) = h2(e12)) | ~ spl144_362), inference(backward_demodulation, [], [f2878, f3463])).
fof(f2878, plain, (h2(e12) = op2(e21, h2(e11))), inference(backward_demodulation, [], [f1096, f1095])).
fof(f1095, plain, (op2(e21, e21) = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f4871, plain, (spl144_275 | ~ spl144_87 | ~ spl144_276), inference(avatar_split_clause, [], [f4849, f2969, f1568, f2964])).
fof(f2964, plain, (spl144_275 <=> (e22 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_275])])).
fof(f2969, plain, (spl144_276 <=> (e22 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_276])])).
fof(f4849, plain, ((e22 = h3(e12)) | (~ spl144_87 | ~ spl144_276)), inference(forward_demodulation, [], [f4848, f1570])).
fof(f4848, plain, ((op2(e22, e22) = h3(e12)) | ~ spl144_276), inference(forward_demodulation, [], [f2885, f2970])).
fof(f2970, plain, ((e22 = h3(e11)) | ~ spl144_276), inference(avatar_component_clause, [], [f2969])).
fof(f2885, plain, (h3(e12) = op2(e22, h3(e11))), inference(backward_demodulation, [], [f1100, f1099])).
fof(f1099, plain, (op2(e22, e22) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((h3(e13) = op2(op2(e22, op2(e22, e22)), e22)) & (h3(e12) = op2(e22, op2(e22, e22))) & (op2(e22, e22) = h3(e11)) & (e22 = h3(e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax16)).
fof(f1100, plain, (h3(e12) = op2(e22, op2(e22, e22))), inference(cnf_transformation, [], [f16])).
fof(f4864, plain, (~ spl144_77 | ~ spl144_109), inference(avatar_split_clause, [], [f4862, f1662, f1526])).
fof(f1526, plain, (spl144_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_77])])).
fof(f4862, plain, (~ (e20 = op2(e23, e20)) | ~ spl144_109), inference(backward_demodulation, [], [f460, f1664])).
fof(f460, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax6)).
fof(f4855, plain, (~ spl144_62 | spl144_360), inference(avatar_contradiction_clause, [], [f4854])).
fof(f4854, plain, ($false | (~ spl144_62 | spl144_360)), inference(trivial_inequality_removal, [], [f4853])).
fof(f4853, plain, (~ (h2(e11) = h2(e11)) | (~ spl144_62 | spl144_360)), inference(forward_demodulation, [], [f3434, f1432])).
fof(f3434, plain, (~ (h2(e11) = h2(op1(e10, e10))) | spl144_360), inference(avatar_component_clause, [], [f3432])).
fof(f3432, plain, (spl144_360 <=> (h2(e11) = h2(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl144_360])])).
fof(f4839, plain, (~ spl144_35 | ~ spl144_99 | ~ spl144_302 | spl144_364 | ~ spl144_367), inference(avatar_contradiction_clause, [], [f4838])).
fof(f4838, plain, ($false | (~ spl144_35 | ~ spl144_99 | ~ spl144_302 | spl144_364 | ~ spl144_367)), inference(subsumption_resolution, [], [f4837, f1621])).
fof(f4837, plain, (~ (e22 = op2(e21, e23)) | (~ spl144_35 | ~ spl144_302 | spl144_364 | ~ spl144_367)), inference(forward_demodulation, [], [f4836, f2863])).
fof(f4836, plain, (~ (op2(e21, e23) = h1(e12)) | (~ spl144_35 | ~ spl144_302 | spl144_364 | ~ spl144_367)), inference(forward_demodulation, [], [f4434, f1317])).
fof(f4434, plain, (~ (op2(e21, e23) = h1(op1(e11, e13))) | (~ spl144_302 | spl144_364 | ~ spl144_367)), inference(forward_demodulation, [], [f4433, f3105])).
fof(f4433, plain, (~ (h1(op1(e11, e13)) = op2(h1(e11), e23)) | (spl144_364 | ~ spl144_367)), inference(forward_demodulation, [], [f3492, f3503])).
fof(f3492, plain, (~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | spl144_364), inference(avatar_component_clause, [], [f3490])).
fof(f3490, plain, (spl144_364 <=> (h1(op1(e11, e13)) = op2(h1(e11), h1(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl144_364])])).
fof(f4833, plain, (~ spl144_111 | ~ spl144_99), inference(avatar_split_clause, [], [f4832, f1619, f1670])).
fof(f1670, plain, (spl144_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_111])])).
fof(f4832, plain, (~ (e22 = op2(e21, e20)) | ~ spl144_99), inference(forward_demodulation, [], [f488, f1621])).
fof(f488, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4816, plain, (~ spl144_75 | ~ spl144_123), inference(avatar_split_clause, [], [f4815, f1721, f1517])).
fof(f1517, plain, (spl144_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_75])])).
fof(f4815, plain, (~ (e22 = op2(e23, e21)) | ~ spl144_123), inference(forward_demodulation, [], [f464, f1723])).
fof(f464, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4814, plain, (spl144_324 | ~ spl144_68), inference(avatar_split_clause, [], [f4813, f1487, f3231])).
fof(f3231, plain, (spl144_324 <=> (e23 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_324])])).
fof(f4813, plain, ((e23 = h4(e11)) | ~ spl144_68), inference(forward_demodulation, [], [f1103, f1489])).
fof(f4810, plain, (~ spl144_66 | ~ spl144_68), inference(avatar_contradiction_clause, [], [f4809])).
fof(f4809, plain, ($false | (~ spl144_66 | ~ spl144_68)), inference(subsumption_resolution, [], [f4808, f514])).
fof(f4808, plain, ((e21 = e23) | (~ spl144_66 | ~ spl144_68)), inference(forward_demodulation, [], [f1489, f1481])).
fof(f1481, plain, ((e21 = op2(e23, e23)) | ~ spl144_66), inference(avatar_component_clause, [], [f1479])).
fof(f1479, plain, (spl144_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_66])])).
fof(f4799, plain, (~ spl144_89 | ~ spl144_90), inference(avatar_contradiction_clause, [], [f4798])).
fof(f4798, plain, ($false | (~ spl144_89 | ~ spl144_90)), inference(subsumption_resolution, [], [f4797, f510])).
fof(f510, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f4797, plain, ((e20 = e21) | (~ spl144_89 | ~ spl144_90)), inference(forward_demodulation, [], [f1583, f1579])).
fof(f1583, plain, ((e21 = op2(e22, e21)) | ~ spl144_90), inference(avatar_component_clause, [], [f1581])).
fof(f1581, plain, (spl144_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_90])])).
fof(f4769, plain, (~ spl144_83 | ~ spl144_276), inference(avatar_split_clause, [], [f4763, f2969, f1551])).
fof(f1551, plain, (spl144_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_83])])).
fof(f4763, plain, (~ (e22 = op2(e22, e23)) | ~ spl144_276), inference(backward_demodulation, [], [f2884, f2970])).
fof(f2884, plain, ~ (op2(e22, e23) = h3(e11)), inference(backward_demodulation, [], [f497, f1099])).
fof(f497, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4767, plain, (spl144_87 | ~ spl144_276), inference(avatar_split_clause, [], [f4759, f2969, f1568])).
fof(f4759, plain, ((e22 = op2(e22, e22)) | ~ spl144_276), inference(backward_demodulation, [], [f1099, f2970])).
fof(f4741, plain, (~ spl144_72 | ~ spl144_324), inference(avatar_split_clause, [], [f4737, f3231, f1504])).
fof(f1504, plain, (spl144_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_72])])).
fof(f4737, plain, (~ (e23 = op2(e23, e22)) | ~ spl144_324), inference(backward_demodulation, [], [f2892, f3232])).
fof(f3232, plain, ((e23 = h4(e11)) | ~ spl144_324), inference(avatar_component_clause, [], [f3231])).
fof(f2892, plain, ~ (op2(e23, e22) = h4(e11)), inference(backward_demodulation, [], [f503, f1103])).
fof(f503, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4740, plain, (~ spl144_116 | ~ spl144_324), inference(avatar_split_clause, [], [f4735, f3231, f1691])).
fof(f4735, plain, (~ (e23 = op2(e20, e23)) | ~ spl144_324), inference(backward_demodulation, [], [f2887, f3232])).
fof(f2887, plain, ~ (op2(e20, e23) = h4(e11)), inference(backward_demodulation, [], [f476, f1103])).
fof(f476, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4734, plain, (spl144_276 | ~ spl144_23 | ~ spl144_371), inference(avatar_split_clause, [], [f4733, f3518, f1264, f2969])).
fof(f3518, plain, (spl144_371 <=> (h3(e11) = h1(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl144_371])])).
fof(f4733, plain, ((e22 = h3(e11)) | (~ spl144_23 | ~ spl144_371)), inference(forward_demodulation, [], [f4732, f2863])).
fof(f4732, plain, ((h1(e12) = h3(e11)) | (~ spl144_23 | ~ spl144_371)), inference(forward_demodulation, [], [f3519, f1266])).
fof(f3519, plain, ((h3(e11) = h1(op1(e12, e12))) | ~ spl144_371), inference(avatar_component_clause, [], [f3518])).
fof(f4731, plain, (spl144_268 | ~ spl144_66), inference(avatar_split_clause, [], [f4705, f1479, f2929])).
fof(f2929, plain, (spl144_268 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_268])])).
fof(f4705, plain, ((e21 = h4(e11)) | ~ spl144_66), inference(backward_demodulation, [], [f1103, f1481])).
fof(f4724, plain, (~ spl144_113 | ~ spl144_97), inference(avatar_split_clause, [], [f4559, f1611, f1679])).
fof(f1611, plain, (spl144_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_97])])).
fof(f4559, plain, (~ (e20 = op2(e20, e23)) | ~ spl144_97), inference(forward_demodulation, [], [f474, f1613])).
fof(f1613, plain, ((e20 = op2(e21, e23)) | ~ spl144_97), inference(avatar_component_clause, [], [f1611])).
fof(f474, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4689, plain, (~ spl144_125 | ~ spl144_126), inference(avatar_contradiction_clause, [], [f4688])).
fof(f4688, plain, ($false | (~ spl144_125 | ~ spl144_126)), inference(subsumption_resolution, [], [f4687, f510])).
fof(f4687, plain, ((e20 = e21) | (~ spl144_125 | ~ spl144_126)), inference(backward_demodulation, [], [f1736, f1732])).
fof(f1732, plain, ((e20 = op2(e20, e20)) | ~ spl144_125), inference(avatar_component_clause, [], [f1730])).
fof(f4686, plain, (~ spl144_126 | ~ spl144_127), inference(avatar_contradiction_clause, [], [f4685])).
fof(f4685, plain, ($false | (~ spl144_126 | ~ spl144_127)), inference(subsumption_resolution, [], [f4684, f513])).
fof(f513, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f4684, plain, ((e21 = e22) | (~ spl144_126 | ~ spl144_127)), inference(forward_demodulation, [], [f1740, f1736])).
fof(f1740, plain, ((op2(e20, e20) = e22) | ~ spl144_127), inference(avatar_component_clause, [], [f1738])).
fof(f1738, plain, (spl144_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl144_127])])).
fof(f4683, plain, (~ spl144_126 | ~ spl144_128), inference(avatar_contradiction_clause, [], [f4682])).
fof(f4682, plain, ($false | (~ spl144_126 | ~ spl144_128)), inference(subsumption_resolution, [], [f4681, f514])).
fof(f4681, plain, ((e21 = e23) | (~ spl144_126 | ~ spl144_128)), inference(forward_demodulation, [], [f1744, f1736])).
fof(f1744, plain, ((op2(e20, e20) = e23) | ~ spl144_128), inference(avatar_component_clause, [], [f1742])).
fof(f1742, plain, (spl144_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl144_128])])).
fof(f4651, plain, (~ spl144_110 | ~ spl144_302), inference(avatar_split_clause, [], [f4415, f3104, f1666])).
fof(f1666, plain, (spl144_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_110])])).
fof(f4415, plain, (~ (e21 = op2(e21, e20)) | ~ spl144_302), inference(forward_demodulation, [], [f2864, f3105])).
fof(f2864, plain, ~ (op2(e21, e20) = h1(e11)), inference(backward_demodulation, [], [f456, f1091])).
fof(f1091, plain, (op2(e20, e20) = h1(e11)), inference(cnf_transformation, [], [f14])).
fof(f456, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f4645, plain, (~ spl144_104 | ~ spl144_72), inference(avatar_split_clause, [], [f4644, f1504, f1640])).
fof(f4644, plain, (~ (e23 = op2(e21, e22)) | ~ spl144_72), inference(forward_demodulation, [], [f472, f1506])).
fof(f1506, plain, ((e23 = op2(e23, e22)) | ~ spl144_72), inference(avatar_component_clause, [], [f1504])).
fof(f472, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4628, plain, (~ spl144_7 | ~ spl144_23), inference(avatar_split_clause, [], [f4624, f1264, f1196])).
fof(f1196, plain, (spl144_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_7])])).
fof(f4624, plain, (~ (e12 = op1(e13, e12)) | ~ spl144_23), inference(backward_demodulation, [], [f425, f1266])).
fof(f425, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4588, plain, (~ spl144_85 | ~ spl144_87), inference(avatar_contradiction_clause, [], [f4587])).
fof(f4587, plain, ($false | (~ spl144_85 | ~ spl144_87)), inference(subsumption_resolution, [], [f4586, f511])).
fof(f511, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f4586, plain, ((e20 = e22) | (~ spl144_85 | ~ spl144_87)), inference(backward_demodulation, [], [f1570, f1562])).
fof(f1562, plain, ((e20 = op2(e22, e22)) | ~ spl144_85), inference(avatar_component_clause, [], [f1560])).
fof(f1560, plain, (spl144_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_85])])).
fof(f4584, plain, (~ spl144_87 | spl144_276), inference(avatar_contradiction_clause, [], [f4583])).
fof(f4583, plain, ($false | (~ spl144_87 | spl144_276)), inference(subsumption_resolution, [], [f4582, f2971])).
fof(f2971, plain, (~ (e22 = h3(e11)) | spl144_276), inference(avatar_component_clause, [], [f2969])).
fof(f4582, plain, ((e22 = h3(e11)) | ~ spl144_87), inference(backward_demodulation, [], [f1099, f1570])).
fof(f4565, plain, (~ spl144_276 | ~ spl144_71), inference(avatar_split_clause, [], [f4564, f1500, f2969])).
fof(f1500, plain, (spl144_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_71])])).
fof(f4564, plain, (~ (e22 = h3(e11)) | ~ spl144_71), inference(forward_demodulation, [], [f2882, f1502])).
fof(f1502, plain, ((e22 = op2(e23, e22)) | ~ spl144_71), inference(avatar_component_clause, [], [f1500])).
fof(f2882, plain, ~ (op2(e23, e22) = h3(e11)), inference(backward_demodulation, [], [f473, f1099])).
fof(f473, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4558, plain, (~ spl144_115 | ~ spl144_123), inference(avatar_split_clause, [], [f4557, f1721, f1687])).
fof(f1687, plain, (spl144_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_115])])).
fof(f4557, plain, (~ (e22 = op2(e20, e23)) | ~ spl144_123), inference(forward_demodulation, [], [f484, f1723])).
fof(f484, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f4556, plain, (spl144_362 | ~ spl144_108), inference(avatar_split_clause, [], [f4555, f1657, f3462])).
fof(f4555, plain, ((e23 = h2(e11)) | ~ spl144_108), inference(forward_demodulation, [], [f1095, f1659])).
fof(f4541, plain, (~ spl144_80 | ~ spl144_367), inference(avatar_split_clause, [], [f4396, f3502, f1538])).
fof(f1538, plain, (spl144_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_80])])).
fof(f4396, plain, (~ (e23 = op2(e23, e20)) | ~ spl144_367), inference(forward_demodulation, [], [f2859, f3503])).
fof(f2859, plain, ~ (op2(e23, e20) = h1(e13)), inference(backward_demodulation, [], [f461, f2856])).
fof(f2856, plain, (op2(e22, e20) = h1(e13)), inference(forward_demodulation, [], [f1093, f1088])).
fof(f1093, plain, (op2(op2(e20, op2(e20, e20)), e20) = h1(e13)), inference(cnf_transformation, [], [f14])).
fof(f461, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f4525, plain, (~ spl144_47 | ~ spl144_15), inference(avatar_split_clause, [], [f4524, f1230, f1366])).
fof(f1366, plain, (spl144_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_47])])).
fof(f4524, plain, (~ (e12 = op1(e11, e10)) | ~ spl144_15), inference(forward_demodulation, [], [f412, f1232])).
fof(f412, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f4517, plain, (~ spl144_23 | ~ spl144_19), inference(avatar_split_clause, [], [f4516, f1247, f1264])).
fof(f1247, plain, (spl144_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_19])])).
fof(f4516, plain, (~ (e12 = op1(e12, e12)) | ~ spl144_19), inference(forward_demodulation, [], [f449, f1249])).
fof(f1249, plain, ((e12 = op1(e12, e13)) | ~ spl144_19), inference(avatar_component_clause, [], [f1247])).
fof(f449, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4504, plain, (~ spl144_8 | ~ spl144_4), inference(avatar_split_clause, [], [f4195, f1183, f1200])).
fof(f1200, plain, (spl144_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_8])])).
fof(f4195, plain, (~ (e13 = op1(e13, e12)) | ~ spl144_4), inference(forward_demodulation, [], [f455, f1185])).
fof(f455, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f4501, plain, (~ spl144_8 | ~ spl144_56), inference(avatar_split_clause, [], [f4500, f1404, f1200])).
fof(f4500, plain, (~ (e13 = op1(e13, e12)) | ~ spl144_56), inference(forward_demodulation, [], [f422, f1406])).
fof(f4467, plain, (~ spl144_93 | ~ spl144_96), inference(avatar_contradiction_clause, [], [f4466])).
fof(f4466, plain, ($false | (~ spl144_93 | ~ spl144_96)), inference(subsumption_resolution, [], [f4465, f512])).
fof(f4465, plain, ((e20 = e23) | (~ spl144_93 | ~ spl144_96)), inference(backward_demodulation, [], [f1608, f1596])).
fof(f1596, plain, ((e20 = op2(e22, e20)) | ~ spl144_93), inference(avatar_component_clause, [], [f1594])).
fof(f1594, plain, (spl144_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_93])])).
fof(f4454, plain, (~ spl144_117 | ~ spl144_120), inference(avatar_contradiction_clause, [], [f4453])).
fof(f4453, plain, ($false | (~ spl144_117 | ~ spl144_120)), inference(subsumption_resolution, [], [f4452, f512])).
fof(f4452, plain, ((e20 = e23) | (~ spl144_117 | ~ spl144_120)), inference(forward_demodulation, [], [f1710, f1698])).
fof(f4430, plain, (spl144_288 | ~ spl144_107), inference(avatar_split_clause, [], [f4288, f1653, f3030])).
fof(f3030, plain, (spl144_288 <=> (e22 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_288])])).
fof(f1653, plain, (spl144_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_107])])).
fof(f4288, plain, ((e22 = h2(e11)) | ~ spl144_107), inference(backward_demodulation, [], [f1095, f1655])).
fof(f1655, plain, ((e22 = op2(e21, e21)) | ~ spl144_107), inference(avatar_component_clause, [], [f1653])).
fof(f4429, plain, (~ spl144_288 | ~ spl144_123), inference(avatar_split_clause, [], [f4428, f1721, f3030])).
fof(f4428, plain, (~ (e22 = h2(e11)) | ~ spl144_123), inference(forward_demodulation, [], [f2872, f1723])).
fof(f2872, plain, ~ (op2(e20, e21) = h2(e11)), inference(backward_demodulation, [], [f462, f1095])).
fof(f462, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4426, plain, (~ spl144_275 | ~ spl144_123 | ~ spl144_280), inference(avatar_split_clause, [], [f4425, f2990, f1721, f2964])).
fof(f2990, plain, (spl144_280 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_280])])).
fof(f4425, plain, (~ (e22 = h3(e12)) | (~ spl144_123 | ~ spl144_280)), inference(forward_demodulation, [], [f4257, f1723])).
fof(f4257, plain, (~ (op2(e20, e21) = h3(e12)) | ~ spl144_280), inference(forward_demodulation, [], [f463, f4115])).
fof(f4115, plain, ((op2(e22, e21) = h3(e12)) | ~ spl144_280), inference(backward_demodulation, [], [f2885, f2991])).
fof(f2991, plain, ((e21 = h3(e11)) | ~ spl144_280), inference(avatar_component_clause, [], [f2990])).
fof(f463, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f4417, plain, (~ spl144_112 | ~ spl144_367), inference(avatar_split_clause, [], [f4416, f3502, f1674])).
fof(f1674, plain, (spl144_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_112])])).
fof(f4416, plain, (~ (e23 = op2(e21, e20)) | ~ spl144_367), inference(forward_demodulation, [], [f2858, f3503])).
fof(f2858, plain, ~ (op2(e21, e20) = h1(e13)), inference(backward_demodulation, [], [f459, f2856])).
fof(f459, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f4408, plain, (~ spl144_89 | ~ spl144_73), inference(avatar_split_clause, [], [f4260, f1509, f1577])).
fof(f1509, plain, (spl144_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_73])])).
fof(f4260, plain, (~ (e20 = op2(e22, e21)) | ~ spl144_73), inference(forward_demodulation, [], [f467, f1511])).
fof(f1511, plain, ((e20 = op2(e23, e21)) | ~ spl144_73), inference(avatar_component_clause, [], [f1509])).
fof(f467, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f4406, plain, (~ spl144_84 | ~ spl144_367), inference(avatar_split_clause, [], [f4076, f3502, f1555])).
fof(f1555, plain, (spl144_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_84])])).
fof(f4076, plain, (~ (e23 = op2(e22, e23)) | ~ spl144_367), inference(forward_demodulation, [], [f2862, f3503])).
fof(f2862, plain, ~ (op2(e22, e23) = h1(e13)), inference(backward_demodulation, [], [f494, f2856])).
fof(f494, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4385, plain, (~ spl144_43 | ~ spl144_59), inference(avatar_split_clause, [], [f4001, f1417, f1349])).
fof(f1349, plain, (spl144_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_43])])).
fof(f4001, plain, (~ (e12 = op1(e11, e11)) | ~ spl144_59), inference(backward_demodulation, [], [f414, f1419])).
fof(f414, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4367, plain, (~ spl144_29 | ~ spl144_32), inference(avatar_contradiction_clause, [], [f4366])).
fof(f4366, plain, ($false | (~ spl144_29 | ~ spl144_32)), inference(subsumption_resolution, [], [f4365, f506])).
fof(f4365, plain, ((e10 = e13) | (~ spl144_29 | ~ spl144_32)), inference(forward_demodulation, [], [f1304, f1292])).
fof(f1292, plain, ((e10 = op1(e12, e10)) | ~ spl144_29), inference(avatar_component_clause, [], [f1290])).
fof(f1290, plain, (spl144_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_29])])).
fof(f4361, plain, (~ spl144_25 | ~ spl144_9), inference(avatar_split_clause, [], [f4360, f1205, f1273])).
fof(f1205, plain, (spl144_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_9])])).
fof(f4360, plain, (~ (e10 = op1(e12, e11)) | ~ spl144_9), inference(forward_demodulation, [], [f419, f1207])).
fof(f1207, plain, ((e10 = op1(e13, e11)) | ~ spl144_9), inference(avatar_component_clause, [], [f1205])).
fof(f419, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4351, plain, (~ spl144_1 | ~ spl144_4), inference(avatar_contradiction_clause, [], [f4350])).
fof(f4350, plain, ($false | (~ spl144_1 | ~ spl144_4)), inference(subsumption_resolution, [], [f4348, f506])).
fof(f4348, plain, ((e10 = e13) | (~ spl144_1 | ~ spl144_4)), inference(backward_demodulation, [], [f1185, f1173])).
fof(f4333, plain, (~ spl144_31 | ~ spl144_32), inference(avatar_contradiction_clause, [], [f4332])).
fof(f4332, plain, ($false | (~ spl144_31 | ~ spl144_32)), inference(subsumption_resolution, [], [f4331, f509])).
fof(f509, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f4331, plain, ((e12 = e13) | (~ spl144_31 | ~ spl144_32)), inference(backward_demodulation, [], [f1304, f1300])).
fof(f1300, plain, ((e12 = op1(e12, e10)) | ~ spl144_31), inference(avatar_component_clause, [], [f1298])).
fof(f1298, plain, (spl144_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_31])])).
fof(f4320, plain, (~ spl144_17 | ~ spl144_49), inference(avatar_split_clause, [], [f4317, f1375, f1239])).
fof(f1239, plain, (spl144_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_17])])).
fof(f4317, plain, (~ (e10 = op1(e12, e13)) | ~ spl144_49), inference(backward_demodulation, [], [f427, f1377])).
fof(f427, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4310, plain, (~ spl144_62 | ~ spl144_64), inference(avatar_contradiction_clause, [], [f4309])).
fof(f4309, plain, ($false | (~ spl144_62 | ~ spl144_64)), inference(subsumption_resolution, [], [f4308, f508])).
fof(f508, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f4308, plain, ((e11 = e13) | (~ spl144_62 | ~ spl144_64)), inference(forward_demodulation, [], [f1440, f1432])).
fof(f1440, plain, ((op1(e10, e10) = e13) | ~ spl144_64), inference(avatar_component_clause, [], [f1438])).
fof(f1438, plain, (spl144_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl144_64])])).
fof(f4300, plain, (~ spl144_95 | ~ spl144_96), inference(avatar_contradiction_clause, [], [f4299])).
fof(f4299, plain, ($false | (~ spl144_95 | ~ spl144_96)), inference(subsumption_resolution, [], [f4298, f515])).
fof(f515, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f4298, plain, ((e22 = e23) | (~ spl144_95 | ~ spl144_96)), inference(backward_demodulation, [], [f1608, f1604])).
fof(f1604, plain, ((e22 = op2(e22, e20)) | ~ spl144_95), inference(avatar_component_clause, [], [f1602])).
fof(f1602, plain, (spl144_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl144_95])])).
fof(f4283, plain, (~ spl144_113 | ~ spl144_117), inference(avatar_split_clause, [], [f4281, f1696, f1679])).
fof(f4281, plain, (~ (e20 = op2(e20, e23)) | ~ spl144_117), inference(backward_demodulation, [], [f485, f1698])).
fof(f485, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f4279, plain, (~ spl144_123 | ~ spl144_124), inference(avatar_contradiction_clause, [], [f4278])).
fof(f4278, plain, ($false | (~ spl144_123 | ~ spl144_124)), inference(subsumption_resolution, [], [f4277, f515])).
fof(f4277, plain, ((e22 = e23) | (~ spl144_123 | ~ spl144_124)), inference(backward_demodulation, [], [f1727, f1723])).
fof(f1727, plain, ((e23 = op2(e20, e21)) | ~ spl144_124), inference(avatar_component_clause, [], [f1725])).
fof(f1725, plain, (spl144_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_124])])).
fof(f4271, plain, (~ spl144_120 | ~ spl144_104), inference(avatar_split_clause, [], [f4270, f1640, f1708])).
fof(f4270, plain, (~ (e23 = op2(e20, e22)) | ~ spl144_104), inference(forward_demodulation, [], [f468, f1642])).
fof(f468, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f4266, plain, (~ spl144_114 | ~ spl144_302), inference(avatar_split_clause, [], [f4093, f3104, f1683])).
fof(f1683, plain, (spl144_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_114])])).
fof(f4093, plain, (~ (e21 = op2(e20, e23)) | ~ spl144_302), inference(forward_demodulation, [], [f2868, f3105])).
fof(f2868, plain, ~ (op2(e20, e23) = h1(e11)), inference(backward_demodulation, [], [f482, f1091])).
fof(f482, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f4256, plain, (~ spl144_268 | ~ spl144_70), inference(avatar_split_clause, [], [f4255, f1496, f2929])).
fof(f4255, plain, (~ (e21 = h4(e11)) | ~ spl144_70), inference(forward_demodulation, [], [f2892, f1498])).
fof(f4241, plain, (~ spl144_55 | ~ spl144_59), inference(avatar_split_clause, [], [f4240, f1417, f1400])).
fof(f1400, plain, (spl144_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_55])])).
fof(f4240, plain, (~ (e12 = op1(e10, e12)) | ~ spl144_59), inference(forward_demodulation, [], [f435, f1419])).
fof(f435, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f4239, plain, (~ spl144_54 | ~ spl144_62), inference(avatar_split_clause, [], [f4238, f1430, f1396])).
fof(f1396, plain, (spl144_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_54])])).
fof(f4238, plain, (~ (e11 = op1(e10, e12)) | ~ spl144_62), inference(forward_demodulation, [], [f433, f1432])).
fof(f433, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f4237, plain, (~ spl144_46 | ~ spl144_62), inference(avatar_split_clause, [], [f4236, f1430, f1362])).
fof(f1362, plain, (spl144_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_46])])).
fof(f4236, plain, (~ (e11 = op1(e11, e10)) | ~ spl144_62), inference(forward_demodulation, [], [f408, f1432])).
fof(f408, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f4235, plain, (~ spl144_48 | ~ spl144_32), inference(avatar_split_clause, [], [f3889, f1302, f1370])).
fof(f1370, plain, (spl144_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_48])])).
fof(f3889, plain, (~ (e13 = op1(e11, e10)) | ~ spl144_32), inference(forward_demodulation, [], [f411, f1304])).
fof(f411, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f4190, plain, (~ spl144_3 | ~ spl144_4), inference(avatar_contradiction_clause, [], [f4189])).
fof(f4189, plain, ($false | (~ spl144_3 | ~ spl144_4)), inference(subsumption_resolution, [], [f4188, f509])).
fof(f4188, plain, ((e12 = e13) | (~ spl144_3 | ~ spl144_4)), inference(forward_demodulation, [], [f1185, f1181])).
fof(f1181, plain, ((e12 = op1(e13, e13)) | ~ spl144_3), inference(avatar_component_clause, [], [f1179])).
fof(f1179, plain, (spl144_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_3])])).
fof(f4181, plain, (~ spl144_13 | ~ spl144_15), inference(avatar_contradiction_clause, [], [f4180])).
fof(f4180, plain, ($false | (~ spl144_13 | ~ spl144_15)), inference(subsumption_resolution, [], [f4179, f505])).
fof(f505, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f4179, plain, ((e10 = e12) | (~ spl144_13 | ~ spl144_15)), inference(forward_demodulation, [], [f1232, f1224])).
fof(f1224, plain, ((e10 = op1(e13, e10)) | ~ spl144_13), inference(avatar_component_clause, [], [f1222])).
fof(f1222, plain, (spl144_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_13])])).
fof(f4173, plain, (~ spl144_34 | ~ spl144_35), inference(avatar_contradiction_clause, [], [f4172])).
fof(f4172, plain, ($false | (~ spl144_34 | ~ spl144_35)), inference(subsumption_resolution, [], [f4171, f507])).
fof(f507, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f4171, plain, ((e11 = e12) | (~ spl144_34 | ~ spl144_35)), inference(backward_demodulation, [], [f1317, f1313])).
fof(f1313, plain, ((e11 = op1(e11, e13)) | ~ spl144_34), inference(avatar_component_clause, [], [f1311])).
fof(f1311, plain, (spl144_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_34])])).
fof(f4170, plain, (~ spl144_35 | ~ spl144_36), inference(avatar_contradiction_clause, [], [f4169])).
fof(f4169, plain, ($false | (~ spl144_35 | ~ spl144_36)), inference(subsumption_resolution, [], [f4168, f509])).
fof(f4168, plain, ((e12 = e13) | (~ spl144_35 | ~ spl144_36)), inference(backward_demodulation, [], [f1321, f1317])).
fof(f1321, plain, ((e13 = op1(e11, e13)) | ~ spl144_36), inference(avatar_component_clause, [], [f1319])).
fof(f1319, plain, (spl144_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_36])])).
fof(f4166, plain, (~ spl144_38 | ~ spl144_40), inference(avatar_contradiction_clause, [], [f4165])).
fof(f4165, plain, ($false | (~ spl144_38 | ~ spl144_40)), inference(subsumption_resolution, [], [f4164, f508])).
fof(f4164, plain, ((e11 = e13) | (~ spl144_38 | ~ spl144_40)), inference(forward_demodulation, [], [f1338, f1330])).
fof(f4159, plain, (~ spl144_59 | ~ spl144_60), inference(avatar_contradiction_clause, [], [f4158])).
fof(f4158, plain, ($false | (~ spl144_59 | ~ spl144_60)), inference(subsumption_resolution, [], [f4157, f509])).
fof(f4157, plain, ((e12 = e13) | (~ spl144_59 | ~ spl144_60)), inference(forward_demodulation, [], [f1423, f1419])).
fof(f1423, plain, ((e13 = op1(e10, e11)) | ~ spl144_60), inference(avatar_component_clause, [], [f1421])).
fof(f1421, plain, (spl144_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_60])])).
fof(f4155, plain, (~ spl144_65 | ~ spl144_68), inference(avatar_contradiction_clause, [], [f4154])).
fof(f4154, plain, ($false | (~ spl144_65 | ~ spl144_68)), inference(subsumption_resolution, [], [f4153, f512])).
fof(f4153, plain, ((e20 = e23) | (~ spl144_65 | ~ spl144_68)), inference(forward_demodulation, [], [f1489, f1477])).
fof(f1477, plain, ((e20 = op2(e23, e23)) | ~ spl144_65), inference(avatar_component_clause, [], [f1475])).
fof(f4140, plain, (~ spl144_362 | ~ spl144_104), inference(avatar_split_clause, [], [f4139, f1640, f3462])).
fof(f4139, plain, (~ (e23 = h2(e11)) | ~ spl144_104), inference(backward_demodulation, [], [f2876, f1642])).
fof(f2876, plain, ~ (op2(e21, e22) = h2(e11)), inference(backward_demodulation, [], [f489, f1095])).
fof(f489, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f4110, plain, (~ spl144_76 | ~ spl144_324), inference(avatar_split_clause, [], [f4107, f3231, f1521])).
fof(f4107, plain, (~ (e23 = op2(e23, e21)) | ~ spl144_324), inference(backward_demodulation, [], [f2891, f3232])).
fof(f2891, plain, ~ (op2(e23, e21) = h4(e11)), inference(backward_demodulation, [], [f502, f1103])).
fof(f502, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4105, plain, (~ spl144_122 | ~ spl144_302), inference(avatar_split_clause, [], [f4104, f3104, f1717])).
fof(f1717, plain, (spl144_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_122])])).
fof(f4104, plain, (~ (e21 = op2(e20, e21)) | ~ spl144_302), inference(forward_demodulation, [], [f2866, f3105])).
fof(f2866, plain, ~ (op2(e20, e21) = h1(e11)), inference(backward_demodulation, [], [f480, f1091])).
fof(f480, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f4103, plain, (~ spl144_123 | ~ spl144_91), inference(avatar_split_clause, [], [f4102, f1585, f1721])).
fof(f1585, plain, (spl144_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_91])])).
fof(f4102, plain, (~ (e22 = op2(e20, e21)) | ~ spl144_91), inference(forward_demodulation, [], [f463, f1587])).
fof(f1587, plain, ((e22 = op2(e22, e21)) | ~ spl144_91), inference(avatar_component_clause, [], [f1585])).
fof(f4101, plain, (~ spl144_123 | ~ spl144_119), inference(avatar_split_clause, [], [f4100, f1704, f1721])).
fof(f1704, plain, (spl144_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_119])])).
fof(f4100, plain, (~ (e22 = op2(e20, e21)) | ~ spl144_119), inference(forward_demodulation, [], [f483, f1706])).
fof(f1706, plain, ((e22 = op2(e20, e22)) | ~ spl144_119), inference(avatar_component_clause, [], [f1704])).
fof(f483, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f4097, plain, (spl144_123 | ~ spl144_302), inference(avatar_split_clause, [], [f3953, f3104, f1721])).
fof(f3953, plain, ((e22 = op2(e20, e21)) | ~ spl144_302), inference(backward_demodulation, [], [f2869, f3105])).
fof(f2869, plain, (e22 = op2(e20, h1(e11))), inference(backward_demodulation, [], [f1088, f1091])).
fof(f4090, plain, (~ spl144_113 | ~ spl144_81), inference(avatar_split_clause, [], [f4089, f1543, f1679])).
fof(f1543, plain, (spl144_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_81])])).
fof(f4089, plain, (~ (e20 = op2(e20, e23)) | ~ spl144_81), inference(forward_demodulation, [], [f475, f1545])).
fof(f1545, plain, ((e20 = op2(e22, e23)) | ~ spl144_81), inference(avatar_component_clause, [], [f1543])).
fof(f475, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f4075, plain, (~ spl144_76 | ~ spl144_72), inference(avatar_split_clause, [], [f4074, f1504, f1521])).
fof(f4074, plain, (~ (e23 = op2(e23, e21)) | ~ spl144_72), inference(forward_demodulation, [], [f501, f1506])).
fof(f501, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4072, plain, (~ spl144_50 | ~ spl144_62), inference(avatar_split_clause, [], [f4071, f1430, f1379])).
fof(f1379, plain, (spl144_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_50])])).
fof(f4071, plain, (~ (e11 = op1(e10, e13)) | ~ spl144_62), inference(backward_demodulation, [], [f434, f1432])).
fof(f434, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f4069, plain, (~ spl144_51 | ~ spl144_59), inference(avatar_split_clause, [], [f4068, f1417, f1383])).
fof(f1383, plain, (spl144_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_51])])).
fof(f4068, plain, (~ (e12 = op1(e10, e13)) | ~ spl144_59), inference(forward_demodulation, [], [f436, f1419])).
fof(f436, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f4064, plain, (~ spl144_45 | ~ spl144_41), inference(avatar_split_clause, [], [f4063, f1341, f1358])).
fof(f1341, plain, (spl144_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_41])])).
fof(f4063, plain, (~ (e10 = op1(e11, e10)) | ~ spl144_41), inference(forward_demodulation, [], [f438, f1343])).
fof(f1343, plain, ((e10 = op1(e11, e11)) | ~ spl144_41), inference(avatar_component_clause, [], [f1341])).
fof(f438, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4048, plain, (~ spl144_52 | ~ spl144_36), inference(avatar_split_clause, [], [f4047, f1319, f1387])).
fof(f4047, plain, (~ (e13 = op1(e10, e13)) | ~ spl144_36), inference(forward_demodulation, [], [f426, f1321])).
fof(f426, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4038, plain, (~ spl144_11 | ~ spl144_59), inference(avatar_split_clause, [], [f4037, f1417, f1213])).
fof(f1213, plain, (spl144_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_11])])).
fof(f4037, plain, (~ (e12 = op1(e13, e11)) | ~ spl144_59), inference(forward_demodulation, [], [f416, f1419])).
fof(f416, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f4000, plain, (~ spl144_61 | ~ spl144_62), inference(avatar_contradiction_clause, [], [f3999])).
fof(f3999, plain, ($false | (~ spl144_61 | ~ spl144_62)), inference(subsumption_resolution, [], [f3998, f504])).
fof(f504, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f3998, plain, ((e10 = e11) | (~ spl144_61 | ~ spl144_62)), inference(backward_demodulation, [], [f1432, f1428])).
fof(f1428, plain, ((e10 = op1(e10, e10)) | ~ spl144_61), inference(avatar_component_clause, [], [f1426])).
fof(f1426, plain, (spl144_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_61])])).
fof(f3985, plain, (spl144_280 | ~ spl144_86), inference(avatar_split_clause, [], [f3983, f1564, f2990])).
fof(f1564, plain, (spl144_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_86])])).
fof(f3983, plain, ((e21 = h3(e11)) | ~ spl144_86), inference(backward_demodulation, [], [f1099, f1566])).
fof(f1566, plain, ((e21 = op2(e22, e22)) | ~ spl144_86), inference(avatar_component_clause, [], [f1564])).
fof(f3979, plain, (~ spl144_70 | ~ spl144_102), inference(avatar_split_clause, [], [f3977, f1632, f1496])).
fof(f3977, plain, (~ (e21 = op2(e23, e22)) | ~ spl144_102), inference(backward_demodulation, [], [f472, f1634])).
fof(f3974, plain, (~ spl144_106 | ~ spl144_108), inference(avatar_contradiction_clause, [], [f3973])).
fof(f3973, plain, ($false | (~ spl144_106 | ~ spl144_108)), inference(subsumption_resolution, [], [f3971, f514])).
fof(f3971, plain, ((e21 = e23) | (~ spl144_106 | ~ spl144_108)), inference(backward_demodulation, [], [f1659, f1651])).
fof(f3958, plain, (~ spl144_121 | ~ spl144_302), inference(avatar_contradiction_clause, [], [f3957])).
fof(f3957, plain, ($false | (~ spl144_121 | ~ spl144_302)), inference(subsumption_resolution, [], [f3956, f511])).
fof(f3956, plain, ((e20 = e22) | (~ spl144_121 | ~ spl144_302)), inference(forward_demodulation, [], [f3953, f1715])).
fof(f1715, plain, ((e20 = op2(e20, e21)) | ~ spl144_121), inference(avatar_component_clause, [], [f1713])).
fof(f1713, plain, (spl144_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_121])])).
fof(f3955, plain, (~ spl144_118 | ~ spl144_302), inference(avatar_split_clause, [], [f3952, f3104, f1700])).
fof(f1700, plain, (spl144_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_118])])).
fof(f3952, plain, (~ (e21 = op2(e20, e22)) | ~ spl144_302), inference(backward_demodulation, [], [f2867, f3105])).
fof(f2867, plain, ~ (op2(e20, e22) = h1(e11)), inference(backward_demodulation, [], [f481, f1091])).
fof(f481, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3951, plain, (~ spl144_88 | ~ spl144_367), inference(avatar_split_clause, [], [f3949, f3502, f1572])).
fof(f1572, plain, (spl144_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl144_88])])).
fof(f3949, plain, (~ (e23 = op2(e22, e22)) | ~ spl144_367), inference(backward_demodulation, [], [f2861, f3503])).
fof(f2861, plain, ~ (op2(e22, e22) = h1(e13)), inference(backward_demodulation, [], [f493, f2856])).
fof(f493, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f3948, plain, (~ spl144_120 | ~ spl144_116), inference(avatar_split_clause, [], [f3947, f1691, f1708])).
fof(f3947, plain, (~ (e23 = op2(e20, e22)) | ~ spl144_116), inference(forward_demodulation, [], [f485, f1693])).
fof(f3925, plain, (spl144_272 | ~ spl144_65), inference(avatar_split_clause, [], [f3924, f1475, f2949])).
fof(f3924, plain, ((e20 = h4(e11)) | ~ spl144_65), inference(forward_demodulation, [], [f1103, f1477])).
fof(f3914, plain, (~ spl144_58 | ~ spl144_62), inference(avatar_split_clause, [], [f3913, f1430, f1413])).
fof(f1413, plain, (spl144_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_58])])).
fof(f3913, plain, (~ (e11 = op1(e10, e11)) | ~ spl144_62), inference(forward_demodulation, [], [f432, f1432])).
fof(f432, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f3907, plain, (spl144_59 | ~ spl144_62), inference(avatar_split_clause, [], [f3906, f1430, f1417])).
fof(f3906, plain, ((e12 = op1(e10, e11)) | ~ spl144_62), inference(forward_demodulation, [], [f1085, f1432])).
fof(f1085, plain, (e12 = op1(e10, op1(e10, e10))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(op1(e10, op1(e10, e10)), e10)) & (e12 = op1(e10, op1(e10, e10))) & (op1(e10, e10) = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax12)).
fof(f3900, plain, (~ spl144_57 | ~ spl144_53), inference(avatar_split_clause, [], [f3899, f1392, f1409])).
fof(f1409, plain, (spl144_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_57])])).
fof(f3899, plain, (~ (e10 = op1(e10, e11)) | ~ spl144_53), inference(forward_demodulation, [], [f435, f1394])).
fof(f3881, plain, (~ spl144_42 | ~ spl144_26), inference(avatar_split_clause, [], [f3880, f1277, f1345])).
fof(f1277, plain, (spl144_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_26])])).
fof(f3880, plain, (~ (e11 = op1(e11, e11)) | ~ spl144_26), inference(forward_demodulation, [], [f417, f1279])).
fof(f1279, plain, ((e11 = op1(e12, e11)) | ~ spl144_26), inference(avatar_component_clause, [], [f1277])).
fof(f417, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3866, plain, (~ spl144_18 | ~ spl144_26), inference(avatar_split_clause, [], [f3865, f1277, f1243])).
fof(f3865, plain, (~ (e11 = op1(e12, e13)) | ~ spl144_26), inference(forward_demodulation, [], [f448, f1279])).
fof(f448, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f3862, plain, (~ spl144_276 | ~ spl144_23 | spl144_371), inference(avatar_split_clause, [], [f3861, f3518, f1264, f2969])).
fof(f3861, plain, (~ (e22 = h3(e11)) | (~ spl144_23 | spl144_371)), inference(forward_demodulation, [], [f3860, f2863])).
fof(f3860, plain, (~ (h1(e12) = h3(e11)) | (~ spl144_23 | spl144_371)), inference(forward_demodulation, [], [f3520, f1266])).
fof(f3520, plain, (~ (h3(e11) = h1(op1(e12, e12))) | spl144_371), inference(avatar_component_clause, [], [f3518])).
fof(f3851, plain, (~ spl144_2 | ~ spl144_6), inference(avatar_split_clause, [], [f3850, f1192, f1175])).
fof(f1175, plain, (spl144_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_2])])).
fof(f3850, plain, (~ (e11 = op1(e13, e13)) | ~ spl144_6), inference(backward_demodulation, [], [f455, f1194])).
fof(f3847, plain, (~ spl144_8 | ~ spl144_12), inference(avatar_split_clause, [], [f3845, f1217, f1200])).
fof(f3845, plain, (~ (e13 = op1(e13, e12)) | ~ spl144_12), inference(backward_demodulation, [], [f453, f1219])).
fof(f453, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3830, plain, (~ spl144_21 | ~ spl144_23), inference(avatar_contradiction_clause, [], [f3829])).
fof(f3829, plain, ($false | (~ spl144_21 | ~ spl144_23)), inference(subsumption_resolution, [], [f3828, f505])).
fof(f3828, plain, ((e10 = e12) | (~ spl144_21 | ~ spl144_23)), inference(backward_demodulation, [], [f1266, f1258])).
fof(f1258, plain, ((e10 = op1(e12, e12)) | ~ spl144_21), inference(avatar_component_clause, [], [f1256])).
fof(f1256, plain, (spl144_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_21])])).
fof(f3815, plain, (~ spl144_19 | ~ spl144_27), inference(avatar_split_clause, [], [f3812, f1281, f1247])).
fof(f1281, plain, (spl144_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_27])])).
fof(f3812, plain, (~ (e12 = op1(e12, e13)) | ~ spl144_27), inference(backward_demodulation, [], [f448, f1283])).
fof(f1283, plain, ((e12 = op1(e12, e11)) | ~ spl144_27), inference(avatar_component_clause, [], [f1281])).
fof(f3814, plain, (~ spl144_23 | ~ spl144_27), inference(avatar_split_clause, [], [f3811, f1281, f1264])).
fof(f3811, plain, (~ (e12 = op1(e12, e12)) | ~ spl144_27), inference(backward_demodulation, [], [f447, f1283])).
fof(f447, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f3809, plain, (~ spl144_20 | ~ spl144_32), inference(avatar_split_clause, [], [f3805, f1302, f1251])).
fof(f1251, plain, (spl144_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl144_20])])).
fof(f3805, plain, (~ (e13 = op1(e12, e13)) | ~ spl144_32), inference(backward_demodulation, [], [f446, f1304])).
fof(f446, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f3808, plain, (~ spl144_24 | ~ spl144_32), inference(avatar_split_clause, [], [f3804, f1302, f1268])).
fof(f1268, plain, (spl144_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl144_24])])).
fof(f3804, plain, (~ (e13 = op1(e12, e12)) | ~ spl144_32), inference(backward_demodulation, [], [f445, f1304])).
fof(f445, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f3807, plain, (~ spl144_28 | ~ spl144_32), inference(avatar_split_clause, [], [f3803, f1302, f1285])).
fof(f1285, plain, (spl144_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl144_28])])).
fof(f3803, plain, (~ (e13 = op1(e12, e11)) | ~ spl144_32), inference(backward_demodulation, [], [f444, f1304])).
fof(f444, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3806, plain, (~ spl144_16 | ~ spl144_32), inference(avatar_split_clause, [], [f3802, f1302, f1234])).
fof(f1234, plain, (spl144_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl144_16])])).
fof(f3802, plain, (~ (e13 = op1(e13, e10)) | ~ spl144_32), inference(backward_demodulation, [], [f413, f1304])).
fof(f413, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3788, plain, (~ spl144_34 | ~ spl144_42), inference(avatar_split_clause, [], [f3781, f1345, f1311])).
fof(f3781, plain, (~ (e11 = op1(e11, e13)) | ~ spl144_42), inference(backward_demodulation, [], [f442, f1347])).
fof(f442, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3747, plain, (~ spl144_49 | ~ spl144_57), inference(avatar_split_clause, [], [f3742, f1409, f1375])).
fof(f3742, plain, (~ (e10 = op1(e10, e13)) | ~ spl144_57), inference(backward_demodulation, [], [f436, f1411])).
fof(f1411, plain, ((e10 = op1(e10, e11)) | ~ spl144_57), inference(avatar_component_clause, [], [f1409])).
fof(f3737, plain, (~ spl144_62 | ~ spl144_63), inference(avatar_contradiction_clause, [], [f3736])).
fof(f3736, plain, ($false | (~ spl144_62 | ~ spl144_63)), inference(subsumption_resolution, [], [f3735, f507])).
fof(f3735, plain, ((e11 = e12) | (~ spl144_62 | ~ spl144_63)), inference(backward_demodulation, [], [f1436, f1432])).
fof(f1436, plain, ((op1(e10, e10) = e12) | ~ spl144_63), inference(avatar_component_clause, [], [f1434])).
fof(f1434, plain, (spl144_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl144_63])])).
fof(f3713, plain, (~ spl144_67 | ~ spl144_68), inference(avatar_contradiction_clause, [], [f3712])).
fof(f3712, plain, ($false | (~ spl144_67 | ~ spl144_68)), inference(subsumption_resolution, [], [f3711, f515])).
fof(f3711, plain, ((e22 = e23) | (~ spl144_67 | ~ spl144_68)), inference(backward_demodulation, [], [f1489, f1485])).
fof(f1485, plain, ((e22 = op2(e23, e23)) | ~ spl144_67), inference(avatar_component_clause, [], [f1483])).
fof(f1483, plain, (spl144_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_67])])).
fof(f3684, plain, (~ spl144_81 | ~ spl144_82), inference(avatar_contradiction_clause, [], [f3683])).
fof(f3683, plain, ($false | (~ spl144_81 | ~ spl144_82)), inference(subsumption_resolution, [], [f3682, f510])).
fof(f3682, plain, ((e20 = e21) | (~ spl144_81 | ~ spl144_82)), inference(backward_demodulation, [], [f1549, f1545])).
fof(f3681, plain, (~ spl144_82 | ~ spl144_83), inference(avatar_contradiction_clause, [], [f3680])).
fof(f3680, plain, ($false | (~ spl144_82 | ~ spl144_83)), inference(subsumption_resolution, [], [f3679, f513])).
fof(f3679, plain, ((e21 = e22) | (~ spl144_82 | ~ spl144_83)), inference(backward_demodulation, [], [f1553, f1549])).
fof(f1553, plain, ((e22 = op2(e22, e23)) | ~ spl144_83), inference(avatar_component_clause, [], [f1551])).
fof(f3663, plain, (~ spl144_367 | ~ spl144_92), inference(avatar_split_clause, [], [f3658, f1589, f3502])).
fof(f1589, plain, (spl144_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl144_92])])).
fof(f3658, plain, (~ (e23 = h1(e13)) | ~ spl144_92), inference(backward_demodulation, [], [f2860, f1591])).
fof(f1591, plain, ((e23 = op2(e22, e21)) | ~ spl144_92), inference(avatar_component_clause, [], [f1589])).
fof(f2860, plain, ~ (op2(e22, e21) = h1(e13)), inference(backward_demodulation, [], [f492, f2856])).
fof(f492, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3655, plain, (spl144_367 | ~ spl144_96), inference(avatar_split_clause, [], [f3654, f1606, f3502])).
fof(f3654, plain, ((e23 = h1(e13)) | ~ spl144_96), inference(backward_demodulation, [], [f2856, f1608])).
fof(f3653, plain, (~ spl144_98 | ~ spl144_99), inference(avatar_contradiction_clause, [], [f3652])).
fof(f3652, plain, ($false | (~ spl144_98 | ~ spl144_99)), inference(subsumption_resolution, [], [f3651, f513])).
fof(f3651, plain, ((e21 = e22) | (~ spl144_98 | ~ spl144_99)), inference(backward_demodulation, [], [f1621, f1617])).
fof(f1617, plain, ((e21 = op2(e21, e23)) | ~ spl144_98), inference(avatar_component_clause, [], [f1615])).
fof(f1615, plain, (spl144_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_98])])).
fof(f3650, plain, (~ spl144_99 | ~ spl144_100), inference(avatar_contradiction_clause, [], [f3649])).
fof(f3649, plain, ($false | (~ spl144_99 | ~ spl144_100)), inference(subsumption_resolution, [], [f3648, f515])).
fof(f3648, plain, ((e22 = e23) | (~ spl144_99 | ~ spl144_100)), inference(backward_demodulation, [], [f1625, f1621])).
fof(f1625, plain, ((e23 = op2(e21, e23)) | ~ spl144_100), inference(avatar_component_clause, [], [f1623])).
fof(f1623, plain, (spl144_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl144_100])])).
fof(f3646, plain, (~ spl144_362 | ~ spl144_100), inference(avatar_split_clause, [], [f3643, f1623, f3462])).
fof(f3643, plain, (~ (e23 = h2(e11)) | ~ spl144_100), inference(backward_demodulation, [], [f2877, f1625])).
fof(f2877, plain, ~ (op2(e21, e23) = h2(e11)), inference(backward_demodulation, [], [f490, f1095])).
fof(f490, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3645, plain, (~ spl144_324 | ~ spl144_100), inference(avatar_split_clause, [], [f3642, f1623, f3231])).
fof(f3642, plain, (~ (e23 = h4(e11)) | ~ spl144_100), inference(backward_demodulation, [], [f2888, f1625])).
fof(f2888, plain, ~ (op2(e21, e23) = h4(e11)), inference(backward_demodulation, [], [f478, f1103])).
fof(f478, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3626, plain, (~ spl144_304 | ~ spl144_109), inference(avatar_split_clause, [], [f3620, f1662, f3114])).
fof(f3620, plain, (~ (e20 = h1(e13)) | ~ spl144_109), inference(backward_demodulation, [], [f2858, f1664])).
fof(f3611, plain, (~ spl144_100 | ~ spl144_116), inference(avatar_split_clause, [], [f3607, f1691, f1623])).
fof(f3607, plain, (~ (e23 = op2(e21, e23)) | ~ spl144_116), inference(backward_demodulation, [], [f474, f1693])).
fof(f3603, plain, (~ spl144_69 | ~ spl144_117), inference(avatar_split_clause, [], [f3598, f1696, f1492])).
fof(f3598, plain, (~ (e20 = op2(e23, e22)) | ~ spl144_117), inference(backward_demodulation, [], [f470, f1698])).
fof(f470, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3582, plain, (spl144_302 | ~ spl144_126), inference(avatar_split_clause, [], [f3581, f1734, f3104])).
fof(f3581, plain, ((e21 = h1(e11)) | ~ spl144_126), inference(backward_demodulation, [], [f1091, f1736])).
fof(f3553, plain, (~ spl144_363 | ~ spl144_364 | ~ spl144_365 | ~ spl144_366 | spl144_303 | spl144_300 | spl144_297 | ~ spl144_367 | ~ spl144_368 | ~ spl144_369 | ~ spl144_370 | ~ spl144_371 | ~ spl144_372 | ~ spl144_373 | ~ spl144_374 | ~ spl144_375 | ~ spl144_376 | ~ spl144_377 | ~ spl144_378 | ~ spl144_379), inference(avatar_split_clause, [], [f3484, f3550, f3546, f3542, f3538, f3534, f3530, f3526, f3522, f3518, f3514, f3510, f3506, f3502, f3077, f3094, f3110, f3498, f3494, f3490, f3486])).
fof(f3110, plain, (spl144_303 <=> sP132), introduced(avatar_definition, [new_symbols(naming, [spl144_303])])).
fof(f3094, plain, (spl144_300 <=> sP133), introduced(avatar_definition, [new_symbols(naming, [spl144_300])])).
fof(f3077, plain, (spl144_297 <=> sP134), introduced(avatar_definition, [new_symbols(naming, [spl144_297])])).
fof(f3484, plain, (~ (h1(e11) = h1(op1(e10, e10))) | ~ (e22 = h1(op1(e10, e11))) | ~ (op2(e20, e22) = h1(op1(e10, e12))) | ~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11)))), inference(forward_demodulation, [], [f3483, f1091])).
fof(f3483, plain, (~ (op2(e20, e20) = h1(op1(e10, e10))) | ~ (e22 = h1(op1(e10, e11))) | ~ (op2(e20, e22) = h1(op1(e10, e12))) | ~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11)))), inference(forward_demodulation, [], [f3482, f1090])).
fof(f3482, plain, (~ (e22 = h1(op1(e10, e11))) | ~ (op2(e20, e22) = h1(op1(e10, e12))) | ~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3481, f2869])).
fof(f3481, plain, (~ (h1(op1(e10, e11)) = op2(e20, h1(e11))) | ~ (op2(e20, e22) = h1(op1(e10, e12))) | ~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3480, f1090])).
fof(f3480, plain, (~ (op2(e20, e22) = h1(op1(e10, e12))) | ~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3479, f1090])).
fof(f3479, plain, (~ (h1(op1(e10, e12)) = op2(h1(e10), e22)) | ~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3478, f2863])).
fof(f3478, plain, (~ (h1(op1(e10, e13)) = op2(e20, h1(e13))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3477, f1090])).
fof(f3477, plain, (~ (h1(op1(e11, e10)) = op2(h1(e11), e20)) | ~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3476, f1090])).
fof(f3476, plain, (~ (h1(op1(e11, e12)) = op2(h1(e11), e22)) | ~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3475, f2863])).
fof(f3475, plain, (~ (h1(e13) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3474, f2856])).
fof(f3474, plain, (~ (op2(e22, e20) = h1(op1(e12, e10))) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3473, f2863])).
fof(f3473, plain, (~ (h1(op1(e12, e10)) = op2(h1(e12), e20)) | ~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3472, f1090])).
fof(f3472, plain, (~ (h1(op1(e12, e11)) = op2(e22, h1(e11))) | ~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3471, f2863])).
fof(f3471, plain, (~ (h3(e11) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3470, f1099])).
fof(f3470, plain, (~ (op2(e22, e22) = h1(op1(e12, e12))) | ~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3469, f2863])).
fof(f3469, plain, (~ (h1(op1(e12, e13)) = op2(e22, h1(e13))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3468, f2863])).
fof(f3468, plain, (~ (h1(op1(e13, e10)) = op2(h1(e13), e20)) | ~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f3467, f1090])).
fof(f3467, plain, (~ (h1(op1(e13, e12)) = op2(h1(e13), e22)) | ~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(forward_demodulation, [], [f1157, f2863])).
fof(f1157, plain, (~ (e23 = h1(e13)) | sP134 | sP133 | sP132 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10)))), inference(cnf_transformation, [], [f167])).
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
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', co1)).
fof(f3465, plain, (~ spl144_344 | ~ spl144_345 | ~ spl144_346 | ~ spl144_347 | ~ spl144_348 | ~ spl144_349 | ~ spl144_350 | ~ spl144_351 | ~ spl144_352 | spl144_293 | spl144_289 | spl144_285 | ~ spl144_362 | ~ spl144_354 | ~ spl144_355 | ~ spl144_356 | ~ spl144_357 | ~ spl144_358 | ~ spl144_359 | ~ spl144_360), inference(avatar_split_clause, [], [f3460, f3432, f3428, f3424, f3420, f3416, f3412, f3408, f3462, f3016, f3036, f3057, f3400, f3396, f3392, f3388, f3384, f3380, f3376, f3372, f3368])).
fof(f3057, plain, (spl144_293 <=> sP135), introduced(avatar_definition, [new_symbols(naming, [spl144_293])])).
fof(f3036, plain, (spl144_289 <=> sP136), introduced(avatar_definition, [new_symbols(naming, [spl144_289])])).
fof(f3016, plain, (spl144_285 <=> sP137), introduced(avatar_definition, [new_symbols(naming, [spl144_285])])).
fof(f3460, plain, (~ (h2(e11) = h2(op1(e10, e10))) | ~ (h2(e12) = h2(op1(e10, e11))) | ~ (h2(op1(e10, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | ~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11)))), inference(forward_demodulation, [], [f3459, f1095])).
fof(f3459, plain, (~ (op2(e21, e21) = h2(op1(e10, e10))) | ~ (h2(e12) = h2(op1(e10, e11))) | ~ (h2(op1(e10, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | ~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11)))), inference(forward_demodulation, [], [f3458, f1094])).
fof(f3458, plain, (~ (h2(e12) = h2(op1(e10, e11))) | ~ (h2(op1(e10, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | ~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3457, f2878])).
fof(f3457, plain, (~ (h2(op1(e10, e11)) = op2(e21, h2(e11))) | ~ (h2(op1(e10, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | ~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3456, f1094])).
fof(f3456, plain, (~ (h2(op1(e10, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | ~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3455, f1094])).
fof(f3455, plain, (~ (h2(op1(e10, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | ~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3454, f1094])).
fof(f3454, plain, (~ (h2(op1(e11, e10)) = op2(h2(e11), e21)) | ~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3453, f1094])).
fof(f3453, plain, (~ (h2(e13) = h2(op1(e12, e10))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3452, f2871])).
fof(f3452, plain, (~ (h2(op1(e12, e10)) = op2(h2(e12), e21)) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f3451, f1094])).
fof(f3451, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), e21)) | ~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1159, f1094])).
fof(f1159, plain, (~ (e23 = h2(e11)) | sP137 | sP136 | sP135 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(cnf_transformation, [], [f167])).
fof(f3246, plain, (~ spl144_306 | ~ spl144_307 | ~ spl144_308 | ~ spl144_309 | ~ spl144_310 | ~ spl144_311 | ~ spl144_312 | ~ spl144_313 | ~ spl144_314 | spl144_269 | spl144_265 | spl144_261 | ~ spl144_316 | ~ spl144_317 | ~ spl144_318 | ~ spl144_319 | ~ spl144_320 | ~ spl144_321 | ~ spl144_322), inference(avatar_split_clause, [], [f3245, f3201, f3197, f3193, f3189, f3185, f3181, f3177, f2895, f2915, f2935, f3169, f3165, f3161, f3157, f3153, f3149, f3145, f3141, f3137])).
fof(f2935, plain, (spl144_269 <=> sP141), introduced(avatar_definition, [new_symbols(naming, [spl144_269])])).
fof(f2915, plain, (spl144_265 <=> sP142), introduced(avatar_definition, [new_symbols(naming, [spl144_265])])).
fof(f2895, plain, (spl144_261 <=> sP143), introduced(avatar_definition, [new_symbols(naming, [spl144_261])])).
fof(f3245, plain, (~ (h4(e11) = h4(op1(e10, e10))) | ~ (h4(e12) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e23, h4(e12))) | ~ (h4(op1(e10, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | ~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), inference(forward_demodulation, [], [f3244, f1103])).
fof(f3244, plain, (~ (op2(e23, e23) = h4(op1(e10, e10))) | ~ (h4(e12) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e23, h4(e12))) | ~ (h4(op1(e10, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | ~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), inference(forward_demodulation, [], [f3243, f1102])).
fof(f3243, plain, (~ (h4(e12) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e23, h4(e12))) | ~ (h4(op1(e10, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | ~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3242, f2893])).
fof(f3242, plain, (~ (h4(op1(e10, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e23, h4(e12))) | ~ (h4(op1(e10, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | ~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3241, f1102])).
fof(f3241, plain, (~ (h4(op1(e10, e12)) = op2(e23, h4(e12))) | ~ (h4(op1(e10, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | ~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3240, f1102])).
fof(f3240, plain, (~ (h4(op1(e10, e13)) = op2(e23, h4(e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | ~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3239, f1102])).
fof(f3239, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e23)) | ~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3238, f1102])).
fof(f3238, plain, (~ (h4(e13) = h4(op1(e12, e10))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3237, f2886])).
fof(f3237, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e23)) | ~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3236, f1102])).
fof(f3236, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e23)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f3235, f1102])).
fof(f3235, plain, (sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f1166, f1102])).
fof(f1166, plain, (~ (e23 = h4(e10)) | sP143 | sP142 | sP141 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f167])).
fof(f3125, plain, ~ spl144_303, inference(avatar_split_clause, [], [f3124, f3110])).
fof(f3124, plain, ~ sP132, inference(subsumption_resolution, [], [f1150, f1090])).
fof(f1150, plain, (~ (e20 = h1(e10)) | ~ sP132), inference(cnf_transformation, [], [f311])).
fof(f311, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP132), inference(nnf_transformation, [], [f155])).
fof(f3107, plain, (~ spl144_300 | ~ spl144_302), inference(avatar_split_clause, [], [f1147, f3104, f3094])).
fof(f1147, plain, (~ (e21 = h1(e11)) | ~ sP133), inference(cnf_transformation, [], [f310])).
fof(f310, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP133), inference(nnf_transformation, [], [f156])).
fof(f3086, plain, ~ spl144_297, inference(avatar_split_clause, [], [f3085, f3077])).
fof(f3085, plain, ~ sP134, inference(subsumption_resolution, [], [f1144, f2863])).
fof(f1144, plain, (~ (e22 = h1(e12)) | ~ sP134), inference(cnf_transformation, [], [f309])).
fof(f309, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP134), inference(nnf_transformation, [], [f157])).
fof(f3064, plain, (~ spl144_293 | ~ spl144_294), inference(avatar_split_clause, [], [f1141, f3061, f3057])).
fof(f1141, plain, (~ (e20 = h2(e13)) | ~ sP135), inference(cnf_transformation, [], [f308])).
fof(f308, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP135), inference(nnf_transformation, [], [f158])).
fof(f3055, plain, ~ spl144_289, inference(avatar_split_clause, [], [f3054, f3036])).
fof(f3054, plain, ~ sP136, inference(subsumption_resolution, [], [f1134, f1094])).
fof(f1134, plain, (~ (e21 = h2(e10)) | ~ sP136), inference(cnf_transformation, [], [f307])).
fof(f307, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP136), inference(nnf_transformation, [], [f159])).
fof(f3028, plain, (~ spl144_285 | ~ spl144_287), inference(avatar_split_clause, [], [f1132, f3025, f3016])).
fof(f1132, plain, (~ (e22 = h2(e12)) | ~ sP137), inference(cnf_transformation, [], [f306])).
fof(f306, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP137), inference(nnf_transformation, [], [f160])).
fof(f2952, plain, (~ spl144_269 | ~ spl144_272), inference(avatar_split_clause, [], [f1115, f2949, f2935])).
fof(f1115, plain, (~ (e20 = h4(e11)) | ~ sP141), inference(cnf_transformation, [], [f302])).
fof(f302, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP141), inference(nnf_transformation, [], [f164])).
fof(f2922, plain, (~ spl144_265 | ~ spl144_266), inference(avatar_split_clause, [], [f1113, f2919, f2915])).
fof(f1113, plain, (~ (e21 = h4(e13)) | ~ sP142), inference(cnf_transformation, [], [f301])).
fof(f301, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP142), inference(nnf_transformation, [], [f165])).
fof(f2907, plain, (~ spl144_261 | ~ spl144_263), inference(avatar_split_clause, [], [f1108, f2904, f2895])).
fof(f1108, plain, (~ (e22 = h4(e12)) | ~ sP143), inference(cnf_transformation, [], [f300])).
fof(f300, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP143), inference(nnf_transformation, [], [f166])).
fof(f2855, plain, spl144_126, inference(avatar_split_clause, [], [f1087, f1734])).
fof(f1087, plain, (op2(e20, e20) = e21), inference(cnf_transformation, [], [f13])).
fof(f2854, plain, spl144_96, inference(avatar_split_clause, [], [f2853, f1606])).
fof(f2853, plain, (e23 = op2(e22, e20)), inference(backward_demodulation, [], [f1089, f1088])).
fof(f1089, plain, (e23 = op2(op2(e20, op2(e20, e20)), e20)), inference(cnf_transformation, [], [f13])).
fof(f2852, plain, spl144_62, inference(avatar_split_clause, [], [f1084, f1430])).
fof(f1084, plain, (op1(e10, e10) = e11), inference(cnf_transformation, [], [f12])).
fof(f2851, plain, spl144_32, inference(avatar_split_clause, [], [f2850, f1302])).
fof(f2850, plain, (e13 = op1(e12, e10)), inference(backward_demodulation, [], [f1086, f1085])).
fof(f1086, plain, (e13 = op1(op1(e10, op1(e10, e10)), e10)), inference(cnf_transformation, [], [f12])).
fof(f2849, plain, (spl144_260 | spl144_259 | spl144_258 | spl144_68), inference(avatar_split_clause, [], [f1075, f1487, f2819, f2827, f2835])).
fof(f2835, plain, (spl144_260 <=> sP66), introduced(avatar_definition, [new_symbols(naming, [spl144_260])])).
fof(f2827, plain, (spl144_259 <=> sP67), introduced(avatar_definition, [new_symbols(naming, [spl144_259])])).
fof(f2819, plain, (spl144_258 <=> sP68), introduced(avatar_definition, [new_symbols(naming, [spl144_258])])).
fof(f1075, plain, ((e23 = op2(e23, e23)) | sP68 | sP67 | sP66), inference(cnf_transformation, [], [f154])).
fof(f154, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP131 | sP130 | sP129 | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69) & ((((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | sP68 | sP67 | sP66)), inference(definition_folding, [], [f11, e153, e152, e151, e150, e149, e148, e147, e146, e145, e144, e143, e142, e141, e140, e139, e138, e137, e136, e135, e134, e133, e132, e131, e130, e129, e128, e127, e126, e125, e124, e123, e122, e121, e120, e119, e118, e117, e116, e115, e114, e113, e112, e111, e110, e109, e108, e107, e106, e105, e104, e103, e102, e101, e100, e99, e98, e97, e96, e95, e94, e93, e92, e91, e90, e89, e88])).
fof(f88, plain, ((((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP66), inference(usedef, [], [e88])).
fof(e88, plain, (sP66 <=> (((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP66])])).
fof(f89, plain, ((((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP67), inference(usedef, [], [e89])).
fof(e89, plain, (sP67 <=> (((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP67])])).
fof(f90, plain, ((((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP68), inference(usedef, [], [e90])).
fof(e90, plain, (sP68 <=> (((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP68])])).
fof(f91, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP69), inference(usedef, [], [e91])).
fof(e91, plain, (sP69 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP69])])).
fof(f92, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP70), inference(usedef, [], [e92])).
fof(e92, plain, (sP70 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP70])])).
fof(f93, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP71), inference(usedef, [], [e93])).
fof(e93, plain, (sP71 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP71])])).
fof(f94, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP72), inference(usedef, [], [e94])).
fof(e94, plain, (sP72 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP72])])).
fof(f95, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP73), inference(usedef, [], [e95])).
fof(e95, plain, (sP73 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP73])])).
fof(f96, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP74), inference(usedef, [], [e96])).
fof(e96, plain, (sP74 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP74])])).
fof(f97, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP75), inference(usedef, [], [e97])).
fof(e97, plain, (sP75 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP75])])).
fof(f98, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP76), inference(usedef, [], [e98])).
fof(e98, plain, (sP76 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP76])])).
fof(f99, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP77), inference(usedef, [], [e99])).
fof(e99, plain, (sP77 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP77])])).
fof(f100, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP78), inference(usedef, [], [e100])).
fof(e100, plain, (sP78 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP78])])).
fof(f101, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP79), inference(usedef, [], [e101])).
fof(e101, plain, (sP79 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP79])])).
fof(f102, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP80), inference(usedef, [], [e102])).
fof(e102, plain, (sP80 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP80])])).
fof(f103, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP81), inference(usedef, [], [e103])).
fof(e103, plain, (sP81 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP81])])).
fof(f104, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP82), inference(usedef, [], [e104])).
fof(e104, plain, (sP82 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP82])])).
fof(f105, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP83), inference(usedef, [], [e105])).
fof(e105, plain, (sP83 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP83])])).
fof(f106, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP84), inference(usedef, [], [e106])).
fof(e106, plain, (sP84 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP84])])).
fof(f107, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP85), inference(usedef, [], [e107])).
fof(e107, plain, (sP85 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP85])])).
fof(f108, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP86), inference(usedef, [], [e108])).
fof(e108, plain, (sP86 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP86])])).
fof(f109, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP87), inference(usedef, [], [e109])).
fof(e109, plain, (sP87 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP87])])).
fof(f110, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP88), inference(usedef, [], [e110])).
fof(e110, plain, (sP88 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP88])])).
fof(f111, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP89), inference(usedef, [], [e111])).
fof(e111, plain, (sP89 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP89])])).
fof(f112, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP90), inference(usedef, [], [e112])).
fof(e112, plain, (sP90 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP90])])).
fof(f113, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP91), inference(usedef, [], [e113])).
fof(e113, plain, (sP91 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP91])])).
fof(f114, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP92), inference(usedef, [], [e114])).
fof(e114, plain, (sP92 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP92])])).
fof(f115, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP93), inference(usedef, [], [e115])).
fof(e115, plain, (sP93 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP93])])).
fof(f116, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP94), inference(usedef, [], [e116])).
fof(e116, plain, (sP94 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP94])])).
fof(f117, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP95), inference(usedef, [], [e117])).
fof(e117, plain, (sP95 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP95])])).
fof(f118, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP96), inference(usedef, [], [e118])).
fof(e118, plain, (sP96 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP96])])).
fof(f119, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP97), inference(usedef, [], [e119])).
fof(e119, plain, (sP97 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP97])])).
fof(f120, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP98), inference(usedef, [], [e120])).
fof(e120, plain, (sP98 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP98])])).
fof(f121, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP99), inference(usedef, [], [e121])).
fof(e121, plain, (sP99 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP99])])).
fof(f122, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP100), inference(usedef, [], [e122])).
fof(e122, plain, (sP100 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP100])])).
fof(f123, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP101), inference(usedef, [], [e123])).
fof(e123, plain, (sP101 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP101])])).
fof(f124, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP102), inference(usedef, [], [e124])).
fof(e124, plain, (sP102 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP102])])).
fof(f125, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP103), inference(usedef, [], [e125])).
fof(e125, plain, (sP103 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP103])])).
fof(f126, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP104), inference(usedef, [], [e126])).
fof(e126, plain, (sP104 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP104])])).
fof(f127, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP105), inference(usedef, [], [e127])).
fof(e127, plain, (sP105 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP105])])).
fof(f128, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP106), inference(usedef, [], [e128])).
fof(e128, plain, (sP106 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP106])])).
fof(f129, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP107), inference(usedef, [], [e129])).
fof(e129, plain, (sP107 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP107])])).
fof(f130, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP108), inference(usedef, [], [e130])).
fof(e130, plain, (sP108 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP108])])).
fof(f131, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP109), inference(usedef, [], [e131])).
fof(e131, plain, (sP109 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP109])])).
fof(f132, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP110), inference(usedef, [], [e132])).
fof(e132, plain, (sP110 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP110])])).
fof(f133, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP111), inference(usedef, [], [e133])).
fof(e133, plain, (sP111 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP111])])).
fof(f134, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP112), inference(usedef, [], [e134])).
fof(e134, plain, (sP112 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP112])])).
fof(f135, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP113), inference(usedef, [], [e135])).
fof(e135, plain, (sP113 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP113])])).
fof(f136, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP114), inference(usedef, [], [e136])).
fof(e136, plain, (sP114 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP114])])).
fof(f137, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP115), inference(usedef, [], [e137])).
fof(e137, plain, (sP115 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP115])])).
fof(f138, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP116), inference(usedef, [], [e138])).
fof(e138, plain, (sP116 <=> (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP116])])).
fof(f139, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP117), inference(usedef, [], [e139])).
fof(e139, plain, (sP117 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP117])])).
fof(f140, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP118), inference(usedef, [], [e140])).
fof(e140, plain, (sP118 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP118])])).
fof(f141, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP119), inference(usedef, [], [e141])).
fof(e141, plain, (sP119 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP119])])).
fof(f142, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP120), inference(usedef, [], [e142])).
fof(e142, plain, (sP120 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP120])])).
fof(f143, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP121), inference(usedef, [], [e143])).
fof(e143, plain, (sP121 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP121])])).
fof(f144, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP122), inference(usedef, [], [e144])).
fof(e144, plain, (sP122 <=> (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP122])])).
fof(f145, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP123), inference(usedef, [], [e145])).
fof(e145, plain, (sP123 <=> (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP123])])).
fof(f146, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP124), inference(usedef, [], [e146])).
fof(e146, plain, (sP124 <=> (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP124])])).
fof(f147, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP125), inference(usedef, [], [e147])).
fof(e147, plain, (sP125 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP125])])).
fof(f148, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP126), inference(usedef, [], [e148])).
fof(e148, plain, (sP126 <=> (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP126])])).
fof(f149, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP127), inference(usedef, [], [e149])).
fof(e149, plain, (sP127 <=> (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP127])])).
fof(f150, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP128), inference(usedef, [], [e150])).
fof(e150, plain, (sP128 <=> (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP128])])).
fof(f151, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP129), inference(usedef, [], [e151])).
fof(e151, plain, (sP129 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP129])])).
fof(f152, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP130), inference(usedef, [], [e152])).
fof(e152, plain, (sP130 <=> (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP130])])).
fof(f153, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP131), inference(usedef, [], [e153])).
fof(e153, plain, (sP131 <=> (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP131])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))) & ((((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & (e23 = op2(e23, e23))) | (((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | (((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | (((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax11)).
fof(f2847, plain, (spl144_260 | spl144_259 | spl144_258 | ~ spl144_108 | spl144_98), inference(avatar_split_clause, [], [f1077, f1615, f1657, f2819, f2827, f2835])).
fof(f1077, plain, ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21)) | sP68 | sP67 | sP66), inference(cnf_transformation, [], [f154])).
fof(f2845, plain, (spl144_257 | spl144_256 | spl144_255 | spl144_254 | spl144_253 | spl144_252 | spl144_251 | spl144_250 | spl144_249 | spl144_248 | spl144_247 | spl144_246 | spl144_245 | spl144_244 | spl144_243 | spl144_242 | spl144_241 | spl144_240 | spl144_239 | spl144_238 | spl144_237 | spl144_236 | spl144_235 | spl144_234 | spl144_233 | spl144_232 | spl144_231 | spl144_230 | spl144_229 | spl144_228 | spl144_227 | spl144_226 | spl144_225 | spl144_224 | spl144_223 | spl144_222 | spl144_221 | spl144_220 | spl144_219 | spl144_218 | spl144_217 | spl144_216 | spl144_215 | spl144_214 | spl144_213 | spl144_212 | spl144_211 | spl144_210 | spl144_209 | spl144_208 | spl144_207 | spl144_206 | spl144_205 | spl144_204 | spl144_203 | spl144_202 | spl144_201 | spl144_200 | spl144_199 | spl144_198 | spl144_197 | spl144_196 | spl144_195 | spl144_68), inference(avatar_split_clause, [], [f1080, f1487, f2315, f2323, f2331, f2339, f2347, f2355, f2363, f2371, f2379, f2387, f2395, f2403, f2411, f2419, f2427, f2435, f2443, f2451, f2459, f2467, f2475, f2483, f2491, f2499, f2507, f2515, f2523, f2531, f2539, f2547, f2555, f2563, f2571, f2579, f2587, f2595, f2603, f2611, f2619, f2627, f2635, f2643, f2651, f2659, f2667, f2675, f2683, f2691, f2699, f2707, f2715, f2723, f2731, f2739, f2747, f2755, f2763, f2771, f2779, f2787, f2795, f2803, f2811])).
fof(f2811, plain, (spl144_257 <=> sP69), introduced(avatar_definition, [new_symbols(naming, [spl144_257])])).
fof(f2803, plain, (spl144_256 <=> sP70), introduced(avatar_definition, [new_symbols(naming, [spl144_256])])).
fof(f2795, plain, (spl144_255 <=> sP71), introduced(avatar_definition, [new_symbols(naming, [spl144_255])])).
fof(f2787, plain, (spl144_254 <=> sP72), introduced(avatar_definition, [new_symbols(naming, [spl144_254])])).
fof(f2779, plain, (spl144_253 <=> sP73), introduced(avatar_definition, [new_symbols(naming, [spl144_253])])).
fof(f2771, plain, (spl144_252 <=> sP74), introduced(avatar_definition, [new_symbols(naming, [spl144_252])])).
fof(f2763, plain, (spl144_251 <=> sP75), introduced(avatar_definition, [new_symbols(naming, [spl144_251])])).
fof(f2755, plain, (spl144_250 <=> sP76), introduced(avatar_definition, [new_symbols(naming, [spl144_250])])).
fof(f2747, plain, (spl144_249 <=> sP77), introduced(avatar_definition, [new_symbols(naming, [spl144_249])])).
fof(f2739, plain, (spl144_248 <=> sP78), introduced(avatar_definition, [new_symbols(naming, [spl144_248])])).
fof(f2731, plain, (spl144_247 <=> sP79), introduced(avatar_definition, [new_symbols(naming, [spl144_247])])).
fof(f2723, plain, (spl144_246 <=> sP80), introduced(avatar_definition, [new_symbols(naming, [spl144_246])])).
fof(f2715, plain, (spl144_245 <=> sP81), introduced(avatar_definition, [new_symbols(naming, [spl144_245])])).
fof(f2707, plain, (spl144_244 <=> sP82), introduced(avatar_definition, [new_symbols(naming, [spl144_244])])).
fof(f2699, plain, (spl144_243 <=> sP83), introduced(avatar_definition, [new_symbols(naming, [spl144_243])])).
fof(f2691, plain, (spl144_242 <=> sP84), introduced(avatar_definition, [new_symbols(naming, [spl144_242])])).
fof(f2683, plain, (spl144_241 <=> sP85), introduced(avatar_definition, [new_symbols(naming, [spl144_241])])).
fof(f2675, plain, (spl144_240 <=> sP86), introduced(avatar_definition, [new_symbols(naming, [spl144_240])])).
fof(f2667, plain, (spl144_239 <=> sP87), introduced(avatar_definition, [new_symbols(naming, [spl144_239])])).
fof(f2659, plain, (spl144_238 <=> sP88), introduced(avatar_definition, [new_symbols(naming, [spl144_238])])).
fof(f2651, plain, (spl144_237 <=> sP89), introduced(avatar_definition, [new_symbols(naming, [spl144_237])])).
fof(f2643, plain, (spl144_236 <=> sP90), introduced(avatar_definition, [new_symbols(naming, [spl144_236])])).
fof(f2635, plain, (spl144_235 <=> sP91), introduced(avatar_definition, [new_symbols(naming, [spl144_235])])).
fof(f2627, plain, (spl144_234 <=> sP92), introduced(avatar_definition, [new_symbols(naming, [spl144_234])])).
fof(f2619, plain, (spl144_233 <=> sP93), introduced(avatar_definition, [new_symbols(naming, [spl144_233])])).
fof(f2611, plain, (spl144_232 <=> sP94), introduced(avatar_definition, [new_symbols(naming, [spl144_232])])).
fof(f2603, plain, (spl144_231 <=> sP95), introduced(avatar_definition, [new_symbols(naming, [spl144_231])])).
fof(f2595, plain, (spl144_230 <=> sP96), introduced(avatar_definition, [new_symbols(naming, [spl144_230])])).
fof(f2587, plain, (spl144_229 <=> sP97), introduced(avatar_definition, [new_symbols(naming, [spl144_229])])).
fof(f2579, plain, (spl144_228 <=> sP98), introduced(avatar_definition, [new_symbols(naming, [spl144_228])])).
fof(f2571, plain, (spl144_227 <=> sP99), introduced(avatar_definition, [new_symbols(naming, [spl144_227])])).
fof(f2563, plain, (spl144_226 <=> sP100), introduced(avatar_definition, [new_symbols(naming, [spl144_226])])).
fof(f2555, plain, (spl144_225 <=> sP101), introduced(avatar_definition, [new_symbols(naming, [spl144_225])])).
fof(f2547, plain, (spl144_224 <=> sP102), introduced(avatar_definition, [new_symbols(naming, [spl144_224])])).
fof(f2539, plain, (spl144_223 <=> sP103), introduced(avatar_definition, [new_symbols(naming, [spl144_223])])).
fof(f2531, plain, (spl144_222 <=> sP104), introduced(avatar_definition, [new_symbols(naming, [spl144_222])])).
fof(f2523, plain, (spl144_221 <=> sP105), introduced(avatar_definition, [new_symbols(naming, [spl144_221])])).
fof(f2515, plain, (spl144_220 <=> sP106), introduced(avatar_definition, [new_symbols(naming, [spl144_220])])).
fof(f2507, plain, (spl144_219 <=> sP107), introduced(avatar_definition, [new_symbols(naming, [spl144_219])])).
fof(f2499, plain, (spl144_218 <=> sP108), introduced(avatar_definition, [new_symbols(naming, [spl144_218])])).
fof(f2491, plain, (spl144_217 <=> sP109), introduced(avatar_definition, [new_symbols(naming, [spl144_217])])).
fof(f2483, plain, (spl144_216 <=> sP110), introduced(avatar_definition, [new_symbols(naming, [spl144_216])])).
fof(f2475, plain, (spl144_215 <=> sP111), introduced(avatar_definition, [new_symbols(naming, [spl144_215])])).
fof(f2467, plain, (spl144_214 <=> sP112), introduced(avatar_definition, [new_symbols(naming, [spl144_214])])).
fof(f2459, plain, (spl144_213 <=> sP113), introduced(avatar_definition, [new_symbols(naming, [spl144_213])])).
fof(f2451, plain, (spl144_212 <=> sP114), introduced(avatar_definition, [new_symbols(naming, [spl144_212])])).
fof(f2443, plain, (spl144_211 <=> sP115), introduced(avatar_definition, [new_symbols(naming, [spl144_211])])).
fof(f2435, plain, (spl144_210 <=> sP116), introduced(avatar_definition, [new_symbols(naming, [spl144_210])])).
fof(f2427, plain, (spl144_209 <=> sP117), introduced(avatar_definition, [new_symbols(naming, [spl144_209])])).
fof(f2419, plain, (spl144_208 <=> sP118), introduced(avatar_definition, [new_symbols(naming, [spl144_208])])).
fof(f2411, plain, (spl144_207 <=> sP119), introduced(avatar_definition, [new_symbols(naming, [spl144_207])])).
fof(f2403, plain, (spl144_206 <=> sP120), introduced(avatar_definition, [new_symbols(naming, [spl144_206])])).
fof(f2395, plain, (spl144_205 <=> sP121), introduced(avatar_definition, [new_symbols(naming, [spl144_205])])).
fof(f2387, plain, (spl144_204 <=> sP122), introduced(avatar_definition, [new_symbols(naming, [spl144_204])])).
fof(f2379, plain, (spl144_203 <=> sP123), introduced(avatar_definition, [new_symbols(naming, [spl144_203])])).
fof(f2371, plain, (spl144_202 <=> sP124), introduced(avatar_definition, [new_symbols(naming, [spl144_202])])).
fof(f2363, plain, (spl144_201 <=> sP125), introduced(avatar_definition, [new_symbols(naming, [spl144_201])])).
fof(f2355, plain, (spl144_200 <=> sP126), introduced(avatar_definition, [new_symbols(naming, [spl144_200])])).
fof(f2347, plain, (spl144_199 <=> sP127), introduced(avatar_definition, [new_symbols(naming, [spl144_199])])).
fof(f2339, plain, (spl144_198 <=> sP128), introduced(avatar_definition, [new_symbols(naming, [spl144_198])])).
fof(f2331, plain, (spl144_197 <=> sP129), introduced(avatar_definition, [new_symbols(naming, [spl144_197])])).
fof(f2323, plain, (spl144_196 <=> sP130), introduced(avatar_definition, [new_symbols(naming, [spl144_196])])).
fof(f2315, plain, (spl144_195 <=> sP131), introduced(avatar_definition, [new_symbols(naming, [spl144_195])])).
fof(f1080, plain, ((e23 = op2(e23, e23)) | sP131 | sP130 | sP129 | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69), inference(cnf_transformation, [], [f154])).
fof(f2844, plain, (spl144_257 | spl144_256 | spl144_255 | spl144_254 | spl144_253 | spl144_252 | spl144_251 | spl144_250 | spl144_249 | spl144_248 | spl144_247 | spl144_246 | spl144_245 | spl144_244 | spl144_243 | spl144_242 | spl144_241 | spl144_240 | spl144_239 | spl144_238 | spl144_237 | spl144_236 | spl144_235 | spl144_234 | spl144_233 | spl144_232 | spl144_231 | spl144_230 | spl144_229 | spl144_228 | spl144_227 | spl144_226 | spl144_225 | spl144_224 | spl144_223 | spl144_222 | spl144_221 | spl144_220 | spl144_219 | spl144_218 | spl144_217 | spl144_216 | spl144_215 | spl144_214 | spl144_213 | spl144_212 | spl144_211 | spl144_210 | spl144_209 | spl144_208 | spl144_207 | spl144_206 | spl144_205 | spl144_204 | spl144_203 | spl144_202 | spl144_201 | spl144_200 | spl144_199 | spl144_198 | spl144_197 | spl144_196 | spl144_195 | ~ spl144_68), inference(avatar_split_clause, [], [f1081, f1487, f2315, f2323, f2331, f2339, f2347, f2355, f2363, f2371, f2379, f2387, f2395, f2403, f2411, f2419, f2427, f2435, f2443, f2451, f2459, f2467, f2475, f2483, f2491, f2499, f2507, f2515, f2523, f2531, f2539, f2547, f2555, f2563, f2571, f2579, f2587, f2595, f2603, f2611, f2619, f2627, f2635, f2643, f2651, f2659, f2667, f2675, f2683, f2691, f2699, f2707, f2715, f2723, f2731, f2739, f2747, f2755, f2763, f2771, f2779, f2787, f2795, f2803, f2811])).
fof(f1081, plain, (~ (e23 = op2(e23, e23)) | sP131 | sP130 | sP129 | sP128 | sP127 | sP126 | sP125 | sP124 | sP123 | sP122 | sP121 | sP120 | sP119 | sP118 | sP117 | sP116 | sP115 | sP114 | sP113 | sP112 | sP111 | sP110 | sP109 | sP108 | sP107 | sP106 | sP105 | sP104 | sP103 | sP102 | sP101 | sP100 | sP99 | sP98 | sP97 | sP96 | sP95 | sP94 | sP93 | sP92 | sP91 | sP90 | sP89 | sP88 | sP87 | sP86 | sP85 | sP84 | sP83 | sP82 | sP81 | sP80 | sP79 | sP78 | sP77 | sP76 | sP75 | sP74 | sP73 | sP72 | sP71 | sP70 | sP69), inference(cnf_transformation, [], [f154])).
fof(f2841, plain, (~ spl144_260 | spl144_125), inference(avatar_split_clause, [], [f1070, f1730, f2835])).
fof(f1070, plain, ((e20 = op2(e20, e20)) | ~ sP66), inference(cnf_transformation, [], [f299])).
fof(f299, plain, ((((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & (e20 = op2(e20, e20))) | ~ sP66), inference(nnf_transformation, [], [f88])).
fof(f2832, plain, (~ spl144_259 | ~ spl144_126 | spl144_121), inference(avatar_split_clause, [], [f1066, f1713, f1734, f2827])).
fof(f1066, plain, ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21) | ~ sP67), inference(cnf_transformation, [], [f298])).
fof(f298, plain, ((((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & (e21 = op2(e21, e21))) | ~ sP67), inference(nnf_transformation, [], [f89])).
fof(f2825, plain, (~ spl144_258 | spl144_87), inference(avatar_split_clause, [], [f1060, f1568, f2819])).
fof(f1060, plain, ((e22 = op2(e22, e22)) | ~ sP68), inference(cnf_transformation, [], [f297])).
fof(f297, plain, ((((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & (e22 = op2(e22, e22))) | ~ sP68), inference(nnf_transformation, [], [f90])).
fof(f2822, plain, (~ spl144_258 | ~ spl144_67 | spl144_72), inference(avatar_split_clause, [], [f1064, f1504, f1483, f2819])).
fof(f1064, plain, ((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23)) | ~ sP68), inference(cnf_transformation, [], [f297])).
fof(f2817, plain, (~ spl144_257 | spl144_125), inference(avatar_split_clause, [], [f1056, f1730, f2811])).
fof(f1056, plain, ((e20 = op2(e20, e20)) | ~ sP69), inference(cnf_transformation, [], [f296])).
fof(f296, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP69), inference(nnf_transformation, [], [f91])).
fof(f2809, plain, (~ spl144_256 | spl144_125), inference(avatar_split_clause, [], [f1052, f1730, f2803])).
fof(f1052, plain, ((e20 = op2(e20, e20)) | ~ sP70), inference(cnf_transformation, [], [f295])).
fof(f295, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP70), inference(nnf_transformation, [], [f92])).
fof(f2801, plain, (~ spl144_255 | spl144_125), inference(avatar_split_clause, [], [f1048, f1730, f2795])).
fof(f1048, plain, ((e20 = op2(e20, e20)) | ~ sP71), inference(cnf_transformation, [], [f294])).
fof(f294, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP71), inference(nnf_transformation, [], [f93])).
fof(f2793, plain, (~ spl144_254 | spl144_125), inference(avatar_split_clause, [], [f1044, f1730, f2787])).
fof(f1044, plain, ((e20 = op2(e20, e20)) | ~ sP72), inference(cnf_transformation, [], [f293])).
fof(f293, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP72), inference(nnf_transformation, [], [f94])).
fof(f2785, plain, (~ spl144_253 | spl144_121), inference(avatar_split_clause, [], [f1040, f1713, f2779])).
fof(f1040, plain, ((e20 = op2(e20, e21)) | ~ sP73), inference(cnf_transformation, [], [f292])).
fof(f292, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP73), inference(nnf_transformation, [], [f95])).
fof(f2777, plain, (~ spl144_252 | spl144_121), inference(avatar_split_clause, [], [f1036, f1713, f2771])).
fof(f1036, plain, ((e20 = op2(e20, e21)) | ~ sP74), inference(cnf_transformation, [], [f291])).
fof(f291, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP74), inference(nnf_transformation, [], [f96])).
fof(f2769, plain, (~ spl144_251 | spl144_121), inference(avatar_split_clause, [], [f1032, f1713, f2763])).
fof(f1032, plain, ((e20 = op2(e20, e21)) | ~ sP75), inference(cnf_transformation, [], [f290])).
fof(f290, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP75), inference(nnf_transformation, [], [f97])).
fof(f2761, plain, (~ spl144_250 | spl144_121), inference(avatar_split_clause, [], [f1028, f1713, f2755])).
fof(f1028, plain, ((e20 = op2(e20, e21)) | ~ sP76), inference(cnf_transformation, [], [f289])).
fof(f289, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP76), inference(nnf_transformation, [], [f98])).
fof(f2751, plain, (~ spl144_249 | spl144_127), inference(avatar_split_clause, [], [f1026, f1738, f2747])).
fof(f1026, plain, ((op2(e20, e20) = e22) | ~ sP77), inference(cnf_transformation, [], [f288])).
fof(f288, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP77), inference(nnf_transformation, [], [f99])).
fof(f2743, plain, (~ spl144_248 | spl144_107), inference(avatar_split_clause, [], [f1022, f1653, f2739])).
fof(f1022, plain, ((e22 = op2(e21, e21)) | ~ sP78), inference(cnf_transformation, [], [f287])).
fof(f287, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP78), inference(nnf_transformation, [], [f100])).
fof(f2737, plain, (~ spl144_247 | spl144_117), inference(avatar_split_clause, [], [f1016, f1696, f2731])).
fof(f1016, plain, ((e20 = op2(e20, e22)) | ~ sP79), inference(cnf_transformation, [], [f286])).
fof(f286, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP79), inference(nnf_transformation, [], [f101])).
fof(f2734, plain, (~ spl144_247 | ~ spl144_87), inference(avatar_split_clause, [], [f1019, f1568, f2731])).
fof(f1019, plain, (~ (e22 = op2(e22, e22)) | ~ sP79), inference(cnf_transformation, [], [f286])).
fof(f2729, plain, (~ spl144_246 | spl144_117), inference(avatar_split_clause, [], [f1012, f1696, f2723])).
fof(f1012, plain, ((e20 = op2(e20, e22)) | ~ sP80), inference(cnf_transformation, [], [f285])).
fof(f285, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP80), inference(nnf_transformation, [], [f102])).
fof(f2726, plain, (~ spl144_246 | ~ spl144_72), inference(avatar_split_clause, [], [f1015, f1504, f2723])).
fof(f1015, plain, (~ (e23 = op2(e23, e22)) | ~ sP80), inference(cnf_transformation, [], [f285])).
fof(f2719, plain, (~ spl144_245 | spl144_128), inference(avatar_split_clause, [], [f1010, f1742, f2715])).
fof(f1010, plain, ((op2(e20, e20) = e23) | ~ sP81), inference(cnf_transformation, [], [f284])).
fof(f284, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP81), inference(nnf_transformation, [], [f103])).
fof(f2713, plain, (~ spl144_244 | spl144_113), inference(avatar_split_clause, [], [f1004, f1679, f2707])).
fof(f1004, plain, ((e20 = op2(e20, e23)) | ~ sP82), inference(cnf_transformation, [], [f283])).
fof(f283, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP82), inference(nnf_transformation, [], [f104])).
fof(f2710, plain, (~ spl144_244 | ~ spl144_98), inference(avatar_split_clause, [], [f1007, f1615, f2707])).
fof(f1007, plain, (~ (e21 = op2(e21, e23)) | ~ sP82), inference(cnf_transformation, [], [f283])).
fof(f2703, plain, (~ spl144_243 | spl144_88), inference(avatar_split_clause, [], [f1002, f1572, f2699])).
fof(f1002, plain, ((e23 = op2(e22, e22)) | ~ sP83), inference(cnf_transformation, [], [f282])).
fof(f282, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP83), inference(nnf_transformation, [], [f105])).
fof(f2697, plain, (~ spl144_242 | spl144_113), inference(avatar_split_clause, [], [f996, f1679, f2691])).
fof(f996, plain, ((e20 = op2(e20, e23)) | ~ sP84), inference(cnf_transformation, [], [f281])).
fof(f281, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP84), inference(nnf_transformation, [], [f106])).
fof(f2695, plain, (~ spl144_242 | spl144_68), inference(avatar_split_clause, [], [f998, f1487, f2691])).
fof(f998, plain, ((e23 = op2(e23, e23)) | ~ sP84), inference(cnf_transformation, [], [f281])).
fof(f2694, plain, (~ spl144_242 | ~ spl144_68), inference(avatar_split_clause, [], [f999, f1487, f2691])).
fof(f999, plain, (~ (e23 = op2(e23, e23)) | ~ sP84), inference(cnf_transformation, [], [f281])).
fof(f2689, plain, (~ spl144_241 | spl144_110), inference(avatar_split_clause, [], [f992, f1666, f2683])).
fof(f992, plain, ((e21 = op2(e21, e20)) | ~ sP85), inference(cnf_transformation, [], [f280])).
fof(f280, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP85), inference(nnf_transformation, [], [f107])).
fof(f2681, plain, (~ spl144_240 | spl144_110), inference(avatar_split_clause, [], [f988, f1666, f2675])).
fof(f988, plain, ((e21 = op2(e21, e20)) | ~ sP86), inference(cnf_transformation, [], [f279])).
fof(f279, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP86), inference(nnf_transformation, [], [f108])).
fof(f2673, plain, (~ spl144_239 | spl144_110), inference(avatar_split_clause, [], [f984, f1666, f2667])).
fof(f984, plain, ((e21 = op2(e21, e20)) | ~ sP87), inference(cnf_transformation, [], [f278])).
fof(f278, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP87), inference(nnf_transformation, [], [f109])).
fof(f2665, plain, (~ spl144_238 | spl144_110), inference(avatar_split_clause, [], [f980, f1666, f2659])).
fof(f980, plain, ((e21 = op2(e21, e20)) | ~ sP88), inference(cnf_transformation, [], [f277])).
fof(f277, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP88), inference(nnf_transformation, [], [f110])).
fof(f2657, plain, (~ spl144_237 | spl144_106), inference(avatar_split_clause, [], [f976, f1649, f2651])).
fof(f976, plain, ((e21 = op2(e21, e21)) | ~ sP89), inference(cnf_transformation, [], [f276])).
fof(f276, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP89), inference(nnf_transformation, [], [f111])).
fof(f2649, plain, (~ spl144_236 | spl144_106), inference(avatar_split_clause, [], [f972, f1649, f2643])).
fof(f972, plain, ((e21 = op2(e21, e21)) | ~ sP90), inference(cnf_transformation, [], [f275])).
fof(f275, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP90), inference(nnf_transformation, [], [f112])).
fof(f2641, plain, (~ spl144_235 | spl144_106), inference(avatar_split_clause, [], [f968, f1649, f2635])).
fof(f968, plain, ((e21 = op2(e21, e21)) | ~ sP91), inference(cnf_transformation, [], [f274])).
fof(f274, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP91), inference(nnf_transformation, [], [f113])).
fof(f2639, plain, (~ spl144_235 | spl144_86), inference(avatar_split_clause, [], [f970, f1564, f2635])).
fof(f970, plain, ((e21 = op2(e22, e22)) | ~ sP91), inference(cnf_transformation, [], [f274])).
fof(f2633, plain, (~ spl144_234 | spl144_106), inference(avatar_split_clause, [], [f964, f1649, f2627])).
fof(f964, plain, ((e21 = op2(e21, e21)) | ~ sP92), inference(cnf_transformation, [], [f273])).
fof(f273, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP92), inference(nnf_transformation, [], [f114])).
fof(f2631, plain, (~ spl144_234 | spl144_66), inference(avatar_split_clause, [], [f966, f1479, f2627])).
fof(f966, plain, ((e21 = op2(e23, e23)) | ~ sP92), inference(cnf_transformation, [], [f273])).
fof(f2623, plain, (~ spl144_233 | spl144_127), inference(avatar_split_clause, [], [f962, f1738, f2619])).
fof(f962, plain, ((op2(e20, e20) = e22) | ~ sP93), inference(cnf_transformation, [], [f272])).
fof(f272, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP93), inference(nnf_transformation, [], [f115])).
fof(f2615, plain, (~ spl144_232 | spl144_107), inference(avatar_split_clause, [], [f958, f1653, f2611])).
fof(f958, plain, ((e22 = op2(e21, e21)) | ~ sP94), inference(cnf_transformation, [], [f271])).
fof(f271, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP94), inference(nnf_transformation, [], [f116])).
fof(f2607, plain, (~ spl144_231 | spl144_87), inference(avatar_split_clause, [], [f954, f1568, f2603])).
fof(f954, plain, ((e22 = op2(e22, e22)) | ~ sP95), inference(cnf_transformation, [], [f270])).
fof(f270, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP95), inference(nnf_transformation, [], [f117])).
fof(f2606, plain, (~ spl144_231 | ~ spl144_87), inference(avatar_split_clause, [], [f955, f1568, f2603])).
fof(f955, plain, (~ (e22 = op2(e22, e22)) | ~ sP95), inference(cnf_transformation, [], [f270])).
fof(f2599, plain, (~ spl144_230 | spl144_67), inference(avatar_split_clause, [], [f950, f1483, f2595])).
fof(f950, plain, ((e22 = op2(e23, e23)) | ~ sP96), inference(cnf_transformation, [], [f269])).
fof(f269, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP96), inference(nnf_transformation, [], [f118])).
fof(f2598, plain, (~ spl144_230 | ~ spl144_72), inference(avatar_split_clause, [], [f951, f1504, f2595])).
fof(f951, plain, (~ (e23 = op2(e23, e22)) | ~ sP96), inference(cnf_transformation, [], [f269])).
fof(f2591, plain, (~ spl144_229 | spl144_128), inference(avatar_split_clause, [], [f946, f1742, f2587])).
fof(f946, plain, ((op2(e20, e20) = e23) | ~ sP97), inference(cnf_transformation, [], [f268])).
fof(f268, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP97), inference(nnf_transformation, [], [f119])).
fof(f2584, plain, (~ spl144_228 | ~ spl144_108), inference(avatar_split_clause, [], [f941, f1657, f2579])).
fof(f941, plain, (~ (e23 = op2(e21, e21)) | ~ sP98), inference(cnf_transformation, [], [f267])).
fof(f267, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP98), inference(nnf_transformation, [], [f120])).
fof(f2575, plain, (~ spl144_227 | spl144_88), inference(avatar_split_clause, [], [f938, f1572, f2571])).
fof(f938, plain, ((e23 = op2(e22, e22)) | ~ sP99), inference(cnf_transformation, [], [f266])).
fof(f266, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP99), inference(nnf_transformation, [], [f121])).
fof(f2568, plain, (~ spl144_226 | ~ spl144_108), inference(avatar_split_clause, [], [f933, f1657, f2563])).
fof(f933, plain, (~ (e23 = op2(e21, e21)) | ~ sP100), inference(cnf_transformation, [], [f265])).
fof(f265, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP100), inference(nnf_transformation, [], [f122])).
fof(f2567, plain, (~ spl144_226 | spl144_68), inference(avatar_split_clause, [], [f934, f1487, f2563])).
fof(f934, plain, ((e23 = op2(e23, e23)) | ~ sP100), inference(cnf_transformation, [], [f265])).
fof(f2566, plain, (~ spl144_226 | ~ spl144_68), inference(avatar_split_clause, [], [f935, f1487, f2563])).
fof(f935, plain, (~ (e23 = op2(e23, e23)) | ~ sP100), inference(cnf_transformation, [], [f265])).
fof(f2561, plain, (~ spl144_225 | spl144_95), inference(avatar_split_clause, [], [f928, f1602, f2555])).
fof(f928, plain, ((e22 = op2(e22, e20)) | ~ sP101), inference(cnf_transformation, [], [f264])).
fof(f264, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP101), inference(nnf_transformation, [], [f123])).
fof(f2553, plain, (~ spl144_224 | spl144_95), inference(avatar_split_clause, [], [f924, f1602, f2547])).
fof(f924, plain, ((e22 = op2(e22, e20)) | ~ sP102), inference(cnf_transformation, [], [f263])).
fof(f263, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP102), inference(nnf_transformation, [], [f124])).
fof(f2545, plain, (~ spl144_223 | spl144_95), inference(avatar_split_clause, [], [f920, f1602, f2539])).
fof(f920, plain, ((e22 = op2(e22, e20)) | ~ sP103), inference(cnf_transformation, [], [f262])).
fof(f262, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP103), inference(nnf_transformation, [], [f125])).
fof(f2537, plain, (~ spl144_222 | spl144_95), inference(avatar_split_clause, [], [f916, f1602, f2531])).
fof(f916, plain, ((e22 = op2(e22, e20)) | ~ sP104), inference(cnf_transformation, [], [f261])).
fof(f261, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP104), inference(nnf_transformation, [], [f126])).
fof(f2529, plain, (~ spl144_221 | spl144_91), inference(avatar_split_clause, [], [f912, f1585, f2523])).
fof(f912, plain, ((e22 = op2(e22, e21)) | ~ sP105), inference(cnf_transformation, [], [f260])).
fof(f260, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP105), inference(nnf_transformation, [], [f127])).
fof(f2521, plain, (~ spl144_220 | spl144_91), inference(avatar_split_clause, [], [f908, f1585, f2515])).
fof(f908, plain, ((e22 = op2(e22, e21)) | ~ sP106), inference(cnf_transformation, [], [f259])).
fof(f259, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP106), inference(nnf_transformation, [], [f128])).
fof(f2513, plain, (~ spl144_219 | spl144_91), inference(avatar_split_clause, [], [f904, f1585, f2507])).
fof(f904, plain, ((e22 = op2(e22, e21)) | ~ sP107), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP107), inference(nnf_transformation, [], [f129])).
fof(f2505, plain, (~ spl144_218 | spl144_91), inference(avatar_split_clause, [], [f900, f1585, f2499])).
fof(f900, plain, ((e22 = op2(e22, e21)) | ~ sP108), inference(cnf_transformation, [], [f257])).
fof(f257, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP108), inference(nnf_transformation, [], [f130])).
fof(f2495, plain, (~ spl144_217 | spl144_127), inference(avatar_split_clause, [], [f898, f1738, f2491])).
fof(f898, plain, ((op2(e20, e20) = e22) | ~ sP109), inference(cnf_transformation, [], [f256])).
fof(f256, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP109), inference(nnf_transformation, [], [f131])).
fof(f2487, plain, (~ spl144_216 | spl144_107), inference(avatar_split_clause, [], [f894, f1653, f2483])).
fof(f894, plain, ((e22 = op2(e21, e21)) | ~ sP110), inference(cnf_transformation, [], [f255])).
fof(f255, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP110), inference(nnf_transformation, [], [f132])).
fof(f2481, plain, (~ spl144_215 | spl144_87), inference(avatar_split_clause, [], [f888, f1568, f2475])).
fof(f888, plain, ((e22 = op2(e22, e22)) | ~ sP111), inference(cnf_transformation, [], [f254])).
fof(f254, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP111), inference(nnf_transformation, [], [f133])).
fof(f2480, plain, (~ spl144_215 | ~ spl144_87), inference(avatar_split_clause, [], [f889, f1568, f2475])).
fof(f889, plain, (~ (e22 = op2(e22, e22)) | ~ sP111), inference(cnf_transformation, [], [f254])).
fof(f2472, plain, (~ spl144_214 | ~ spl144_87), inference(avatar_split_clause, [], [f885, f1568, f2467])).
fof(f885, plain, (~ (e22 = op2(e22, e22)) | ~ sP112), inference(cnf_transformation, [], [f253])).
fof(f253, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP112), inference(nnf_transformation, [], [f134])).
fof(f2471, plain, (~ spl144_214 | spl144_67), inference(avatar_split_clause, [], [f886, f1483, f2467])).
fof(f886, plain, ((e22 = op2(e23, e23)) | ~ sP112), inference(cnf_transformation, [], [f253])).
fof(f2470, plain, (~ spl144_214 | ~ spl144_72), inference(avatar_split_clause, [], [f887, f1504, f2467])).
fof(f887, plain, (~ (e23 = op2(e23, e22)) | ~ sP112), inference(cnf_transformation, [], [f253])).
fof(f2463, plain, (~ spl144_213 | spl144_128), inference(avatar_split_clause, [], [f882, f1742, f2459])).
fof(f882, plain, ((op2(e20, e20) = e23) | ~ sP113), inference(cnf_transformation, [], [f252])).
fof(f252, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP113), inference(nnf_transformation, [], [f135])).
fof(f2457, plain, (~ spl144_212 | spl144_83), inference(avatar_split_clause, [], [f876, f1551, f2451])).
fof(f876, plain, ((e22 = op2(e22, e23)) | ~ sP114), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP114), inference(nnf_transformation, [], [f136])).
fof(f2454, plain, (~ spl144_212 | ~ spl144_98), inference(avatar_split_clause, [], [f879, f1615, f2451])).
fof(f879, plain, (~ (e21 = op2(e21, e23)) | ~ sP114), inference(cnf_transformation, [], [f251])).
fof(f2447, plain, (~ spl144_211 | spl144_88), inference(avatar_split_clause, [], [f874, f1572, f2443])).
fof(f874, plain, ((e23 = op2(e22, e22)) | ~ sP115), inference(cnf_transformation, [], [f250])).
fof(f250, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP115), inference(nnf_transformation, [], [f137])).
fof(f2439, plain, (~ spl144_210 | spl144_68), inference(avatar_split_clause, [], [f870, f1487, f2435])).
fof(f870, plain, ((e23 = op2(e23, e23)) | ~ sP116), inference(cnf_transformation, [], [f249])).
fof(f249, plain, ((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP116), inference(nnf_transformation, [], [f138])).
fof(f2438, plain, (~ spl144_210 | ~ spl144_68), inference(avatar_split_clause, [], [f871, f1487, f2435])).
fof(f871, plain, (~ (e23 = op2(e23, e23)) | ~ sP116), inference(cnf_transformation, [], [f249])).
fof(f2433, plain, (~ spl144_209 | spl144_80), inference(avatar_split_clause, [], [f864, f1538, f2427])).
fof(f864, plain, ((e23 = op2(e23, e20)) | ~ sP117), inference(cnf_transformation, [], [f248])).
fof(f248, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP117), inference(nnf_transformation, [], [f139])).
fof(f2425, plain, (~ spl144_208 | spl144_80), inference(avatar_split_clause, [], [f860, f1538, f2419])).
fof(f860, plain, ((e23 = op2(e23, e20)) | ~ sP118), inference(cnf_transformation, [], [f247])).
fof(f247, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP118), inference(nnf_transformation, [], [f140])).
fof(f2417, plain, (~ spl144_207 | spl144_80), inference(avatar_split_clause, [], [f856, f1538, f2411])).
fof(f856, plain, ((e23 = op2(e23, e20)) | ~ sP119), inference(cnf_transformation, [], [f246])).
fof(f246, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP119), inference(nnf_transformation, [], [f141])).
fof(f2409, plain, (~ spl144_206 | spl144_80), inference(avatar_split_clause, [], [f852, f1538, f2403])).
fof(f852, plain, ((e23 = op2(e23, e20)) | ~ sP120), inference(cnf_transformation, [], [f245])).
fof(f245, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP120), inference(nnf_transformation, [], [f142])).
fof(f2401, plain, (~ spl144_205 | spl144_76), inference(avatar_split_clause, [], [f848, f1521, f2395])).
fof(f848, plain, ((e23 = op2(e23, e21)) | ~ sP121), inference(cnf_transformation, [], [f244])).
fof(f244, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP121), inference(nnf_transformation, [], [f143])).
fof(f2393, plain, (~ spl144_204 | spl144_76), inference(avatar_split_clause, [], [f844, f1521, f2387])).
fof(f844, plain, ((e23 = op2(e23, e21)) | ~ sP122), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ((~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP122), inference(nnf_transformation, [], [f144])).
fof(f2391, plain, (~ spl144_204 | spl144_106), inference(avatar_split_clause, [], [f846, f1649, f2387])).
fof(f846, plain, ((e21 = op2(e21, e21)) | ~ sP122), inference(cnf_transformation, [], [f243])).
fof(f2385, plain, (~ spl144_203 | spl144_76), inference(avatar_split_clause, [], [f840, f1521, f2379])).
fof(f840, plain, ((e23 = op2(e23, e21)) | ~ sP123), inference(cnf_transformation, [], [f242])).
fof(f242, plain, ((~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP123), inference(nnf_transformation, [], [f145])).
fof(f2383, plain, (~ spl144_203 | spl144_86), inference(avatar_split_clause, [], [f842, f1564, f2379])).
fof(f842, plain, ((e21 = op2(e22, e22)) | ~ sP123), inference(cnf_transformation, [], [f242])).
fof(f2377, plain, (~ spl144_202 | spl144_76), inference(avatar_split_clause, [], [f836, f1521, f2371])).
fof(f836, plain, ((e23 = op2(e23, e21)) | ~ sP124), inference(cnf_transformation, [], [f241])).
fof(f241, plain, ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP124), inference(nnf_transformation, [], [f146])).
fof(f2375, plain, (~ spl144_202 | spl144_66), inference(avatar_split_clause, [], [f838, f1479, f2371])).
fof(f838, plain, ((e21 = op2(e23, e23)) | ~ sP124), inference(cnf_transformation, [], [f241])).
fof(f2367, plain, (~ spl144_201 | spl144_127), inference(avatar_split_clause, [], [f834, f1738, f2363])).
fof(f834, plain, ((op2(e20, e20) = e22) | ~ sP125), inference(cnf_transformation, [], [f240])).
fof(f240, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP125), inference(nnf_transformation, [], [f147])).
fof(f2359, plain, (~ spl144_200 | spl144_107), inference(avatar_split_clause, [], [f830, f1653, f2355])).
fof(f830, plain, ((e22 = op2(e21, e21)) | ~ sP126), inference(cnf_transformation, [], [f239])).
fof(f239, plain, ((~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP126), inference(nnf_transformation, [], [f148])).
fof(f2353, plain, (~ spl144_199 | spl144_72), inference(avatar_split_clause, [], [f824, f1504, f2347])).
fof(f824, plain, ((e23 = op2(e23, e22)) | ~ sP127), inference(cnf_transformation, [], [f238])).
fof(f238, plain, ((~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP127), inference(nnf_transformation, [], [f149])).
fof(f2350, plain, (~ spl144_199 | ~ spl144_87), inference(avatar_split_clause, [], [f827, f1568, f2347])).
fof(f827, plain, (~ (e22 = op2(e22, e22)) | ~ sP127), inference(cnf_transformation, [], [f238])).
fof(f2343, plain, (~ spl144_198 | spl144_67), inference(avatar_split_clause, [], [f822, f1483, f2339])).
fof(f822, plain, ((e22 = op2(e23, e23)) | ~ sP128), inference(cnf_transformation, [], [f237])).
fof(f237, plain, ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP128), inference(nnf_transformation, [], [f150])).
fof(f2342, plain, (~ spl144_198 | ~ spl144_72), inference(avatar_split_clause, [], [f823, f1504, f2339])).
fof(f823, plain, (~ (e23 = op2(e23, e22)) | ~ sP128), inference(cnf_transformation, [], [f237])).
fof(f2335, plain, (~ spl144_197 | spl144_128), inference(avatar_split_clause, [], [f818, f1742, f2331])).
fof(f818, plain, ((op2(e20, e20) = e23) | ~ sP129), inference(cnf_transformation, [], [f236])).
fof(f236, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP129), inference(nnf_transformation, [], [f151])).
fof(f2329, plain, (~ spl144_196 | spl144_68), inference(avatar_split_clause, [], [f812, f1487, f2323])).
fof(f812, plain, ((e23 = op2(e23, e23)) | ~ sP130), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ((~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP130), inference(nnf_transformation, [], [f152])).
fof(f2328, plain, (~ spl144_196 | ~ spl144_68), inference(avatar_split_clause, [], [f813, f1487, f2323])).
fof(f813, plain, (~ (e23 = op2(e23, e23)) | ~ sP130), inference(cnf_transformation, [], [f235])).
fof(f2319, plain, (~ spl144_195 | spl144_88), inference(avatar_split_clause, [], [f810, f1572, f2315])).
fof(f810, plain, ((e23 = op2(e22, e22)) | ~ sP131), inference(cnf_transformation, [], [f234])).
fof(f234, plain, ((~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | ~ sP131), inference(nnf_transformation, [], [f153])).
fof(f2313, plain, (spl144_194 | spl144_193 | spl144_192 | spl144_4), inference(avatar_split_clause, [], [f799, f1183, f2283, f2291, f2299])).
fof(f2299, plain, (spl144_194 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl144_194])])).
fof(f2291, plain, (spl144_193 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl144_193])])).
fof(f2283, plain, (spl144_192 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl144_192])])).
fof(f799, plain, ((e13 = op1(e13, e13)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP65 | sP64 | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3) & ((((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | sP2 | sP1 | sP0)), inference(definition_folding, [], [f10, e86, e85, e84, e83, e82, e81, e80, e79, e78, e77, e76, e75, e74, e73, e72, e71, e70, e69, e68, e67, e66, e65, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f36, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP15), inference(usedef, [], [e36])).
fof(e36, plain, (sP15 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f37, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP16), inference(usedef, [], [e37])).
fof(e37, plain, (sP16 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f38, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP17), inference(usedef, [], [e38])).
fof(e38, plain, (sP17 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f39, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP18), inference(usedef, [], [e39])).
fof(e39, plain, (sP18 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f40, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP19), inference(usedef, [], [e40])).
fof(e40, plain, (sP19 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f41, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP20), inference(usedef, [], [e41])).
fof(e41, plain, (sP20 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f42, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP21), inference(usedef, [], [e42])).
fof(e42, plain, (sP21 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f43, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP22), inference(usedef, [], [e43])).
fof(e43, plain, (sP22 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f44, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP23), inference(usedef, [], [e44])).
fof(e44, plain, (sP23 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f45, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP24), inference(usedef, [], [e45])).
fof(e45, plain, (sP24 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f46, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP25), inference(usedef, [], [e46])).
fof(e46, plain, (sP25 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f47, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP26), inference(usedef, [], [e47])).
fof(e47, plain, (sP26 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f48, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP27), inference(usedef, [], [e48])).
fof(e48, plain, (sP27 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f49, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP28), inference(usedef, [], [e49])).
fof(e49, plain, (sP28 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f50, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP29), inference(usedef, [], [e50])).
fof(e50, plain, (sP29 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f51, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP30), inference(usedef, [], [e51])).
fof(e51, plain, (sP30 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f52, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP31), inference(usedef, [], [e52])).
fof(e52, plain, (sP31 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f53, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP32), inference(usedef, [], [e53])).
fof(e53, plain, (sP32 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f54, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP33), inference(usedef, [], [e54])).
fof(e54, plain, (sP33 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f55, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP34), inference(usedef, [], [e55])).
fof(e55, plain, (sP34 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f56, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP35), inference(usedef, [], [e56])).
fof(e56, plain, (sP35 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f57, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP36), inference(usedef, [], [e57])).
fof(e57, plain, (sP36 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f58, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP37), inference(usedef, [], [e58])).
fof(e58, plain, (sP37 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f59, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP38), inference(usedef, [], [e59])).
fof(e59, plain, (sP38 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f60, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP39), inference(usedef, [], [e60])).
fof(e60, plain, (sP39 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f61, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP40), inference(usedef, [], [e61])).
fof(e61, plain, (sP40 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f62, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP41), inference(usedef, [], [e62])).
fof(e62, plain, (sP41 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f63, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP42), inference(usedef, [], [e63])).
fof(e63, plain, (sP42 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f64, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP43), inference(usedef, [], [e64])).
fof(e64, plain, (sP43 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f65, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP44), inference(usedef, [], [e65])).
fof(e65, plain, (sP44 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f66, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP45), inference(usedef, [], [e66])).
fof(e66, plain, (sP45 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f67, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP46), inference(usedef, [], [e67])).
fof(e67, plain, (sP46 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f68, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP47), inference(usedef, [], [e68])).
fof(e68, plain, (sP47 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f69, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP48), inference(usedef, [], [e69])).
fof(e69, plain, (sP48 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP48])])).
fof(f70, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP49), inference(usedef, [], [e70])).
fof(e70, plain, (sP49 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP49])])).
fof(f71, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP50), inference(usedef, [], [e71])).
fof(e71, plain, (sP50 <=> (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP50])])).
fof(f72, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP51), inference(usedef, [], [e72])).
fof(e72, plain, (sP51 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP51])])).
fof(f73, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP52), inference(usedef, [], [e73])).
fof(e73, plain, (sP52 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP52])])).
fof(f74, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP53), inference(usedef, [], [e74])).
fof(e74, plain, (sP53 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP53])])).
fof(f75, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP54), inference(usedef, [], [e75])).
fof(e75, plain, (sP54 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP54])])).
fof(f76, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP55), inference(usedef, [], [e76])).
fof(e76, plain, (sP55 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP55])])).
fof(f77, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP56), inference(usedef, [], [e77])).
fof(e77, plain, (sP56 <=> (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP56])])).
fof(f78, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP57), inference(usedef, [], [e78])).
fof(e78, plain, (sP57 <=> (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP57])])).
fof(f79, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP58), inference(usedef, [], [e79])).
fof(e79, plain, (sP58 <=> (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP58])])).
fof(f80, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP59), inference(usedef, [], [e80])).
fof(e80, plain, (sP59 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP59])])).
fof(f81, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP60), inference(usedef, [], [e81])).
fof(e81, plain, (sP60 <=> (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP60])])).
fof(f82, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP61), inference(usedef, [], [e82])).
fof(e82, plain, (sP61 <=> (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP61])])).
fof(f83, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP62), inference(usedef, [], [e83])).
fof(e83, plain, (sP62 <=> (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP62])])).
fof(f84, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP63), inference(usedef, [], [e84])).
fof(e84, plain, (sP63 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP63])])).
fof(f85, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP64), inference(usedef, [], [e85])).
fof(e85, plain, (sP64 <=> (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP64])])).
fof(f86, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP65), inference(usedef, [], [e86])).
fof(e86, plain, (sP65 <=> (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP65])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))) & ((((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & (e13 = op1(e13, e13))) | (((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | (((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | (((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax10)).
fof(f2311, plain, (spl144_194 | spl144_193 | spl144_192 | ~ spl144_44 | spl144_34), inference(avatar_split_clause, [], [f801, f1311, f1353, f2283, f2291, f2299])).
fof(f801, plain, ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f87])).
fof(f2309, plain, (spl144_191 | spl144_190 | spl144_189 | spl144_188 | spl144_187 | spl144_186 | spl144_185 | spl144_184 | spl144_183 | spl144_182 | spl144_181 | spl144_180 | spl144_179 | spl144_178 | spl144_177 | spl144_176 | spl144_175 | spl144_174 | spl144_173 | spl144_172 | spl144_171 | spl144_170 | spl144_169 | spl144_168 | spl144_167 | spl144_166 | spl144_165 | spl144_164 | spl144_163 | spl144_162 | spl144_161 | spl144_160 | spl144_159 | spl144_158 | spl144_157 | spl144_156 | spl144_155 | spl144_154 | spl144_153 | spl144_152 | spl144_151 | spl144_150 | spl144_149 | spl144_148 | spl144_147 | spl144_146 | spl144_145 | spl144_144 | spl144_143 | spl144_142 | spl144_141 | spl144_140 | spl144_139 | spl144_138 | spl144_137 | spl144_136 | spl144_135 | spl144_134 | spl144_133 | spl144_132 | spl144_131 | spl144_130 | spl144_129 | spl144_4), inference(avatar_split_clause, [], [f804, f1183, f1779, f1787, f1795, f1803, f1811, f1819, f1827, f1835, f1843, f1851, f1859, f1867, f1875, f1883, f1891, f1899, f1907, f1915, f1923, f1931, f1939, f1947, f1955, f1963, f1971, f1979, f1987, f1995, f2003, f2011, f2019, f2027, f2035, f2043, f2051, f2059, f2067, f2075, f2083, f2091, f2099, f2107, f2115, f2123, f2131, f2139, f2147, f2155, f2163, f2171, f2179, f2187, f2195, f2203, f2211, f2219, f2227, f2235, f2243, f2251, f2259, f2267, f2275])).
fof(f2275, plain, (spl144_191 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl144_191])])).
fof(f2267, plain, (spl144_190 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl144_190])])).
fof(f2259, plain, (spl144_189 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl144_189])])).
fof(f2251, plain, (spl144_188 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl144_188])])).
fof(f2243, plain, (spl144_187 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl144_187])])).
fof(f2235, plain, (spl144_186 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl144_186])])).
fof(f2227, plain, (spl144_185 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl144_185])])).
fof(f2219, plain, (spl144_184 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl144_184])])).
fof(f2211, plain, (spl144_183 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl144_183])])).
fof(f2203, plain, (spl144_182 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl144_182])])).
fof(f2195, plain, (spl144_181 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl144_181])])).
fof(f2187, plain, (spl144_180 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl144_180])])).
fof(f2179, plain, (spl144_179 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl144_179])])).
fof(f2171, plain, (spl144_178 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl144_178])])).
fof(f2163, plain, (spl144_177 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl144_177])])).
fof(f2155, plain, (spl144_176 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl144_176])])).
fof(f2147, plain, (spl144_175 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl144_175])])).
fof(f2139, plain, (spl144_174 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl144_174])])).
fof(f2131, plain, (spl144_173 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl144_173])])).
fof(f2123, plain, (spl144_172 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl144_172])])).
fof(f2115, plain, (spl144_171 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl144_171])])).
fof(f2107, plain, (spl144_170 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl144_170])])).
fof(f2099, plain, (spl144_169 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl144_169])])).
fof(f2091, plain, (spl144_168 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl144_168])])).
fof(f2083, plain, (spl144_167 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl144_167])])).
fof(f2075, plain, (spl144_166 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl144_166])])).
fof(f2067, plain, (spl144_165 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl144_165])])).
fof(f2059, plain, (spl144_164 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl144_164])])).
fof(f2051, plain, (spl144_163 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl144_163])])).
fof(f2043, plain, (spl144_162 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl144_162])])).
fof(f2035, plain, (spl144_161 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl144_161])])).
fof(f2027, plain, (spl144_160 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl144_160])])).
fof(f2019, plain, (spl144_159 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl144_159])])).
fof(f2011, plain, (spl144_158 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl144_158])])).
fof(f2003, plain, (spl144_157 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl144_157])])).
fof(f1995, plain, (spl144_156 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl144_156])])).
fof(f1987, plain, (spl144_155 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl144_155])])).
fof(f1979, plain, (spl144_154 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl144_154])])).
fof(f1971, plain, (spl144_153 <=> sP41), introduced(avatar_definition, [new_symbols(naming, [spl144_153])])).
fof(f1963, plain, (spl144_152 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl144_152])])).
fof(f1955, plain, (spl144_151 <=> sP43), introduced(avatar_definition, [new_symbols(naming, [spl144_151])])).
fof(f1947, plain, (spl144_150 <=> sP44), introduced(avatar_definition, [new_symbols(naming, [spl144_150])])).
fof(f1939, plain, (spl144_149 <=> sP45), introduced(avatar_definition, [new_symbols(naming, [spl144_149])])).
fof(f1931, plain, (spl144_148 <=> sP46), introduced(avatar_definition, [new_symbols(naming, [spl144_148])])).
fof(f1923, plain, (spl144_147 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl144_147])])).
fof(f1915, plain, (spl144_146 <=> sP48), introduced(avatar_definition, [new_symbols(naming, [spl144_146])])).
fof(f1907, plain, (spl144_145 <=> sP49), introduced(avatar_definition, [new_symbols(naming, [spl144_145])])).
fof(f1899, plain, (spl144_144 <=> sP50), introduced(avatar_definition, [new_symbols(naming, [spl144_144])])).
fof(f1891, plain, (spl144_143 <=> sP51), introduced(avatar_definition, [new_symbols(naming, [spl144_143])])).
fof(f1883, plain, (spl144_142 <=> sP52), introduced(avatar_definition, [new_symbols(naming, [spl144_142])])).
fof(f1875, plain, (spl144_141 <=> sP53), introduced(avatar_definition, [new_symbols(naming, [spl144_141])])).
fof(f1867, plain, (spl144_140 <=> sP54), introduced(avatar_definition, [new_symbols(naming, [spl144_140])])).
fof(f1859, plain, (spl144_139 <=> sP55), introduced(avatar_definition, [new_symbols(naming, [spl144_139])])).
fof(f1851, plain, (spl144_138 <=> sP56), introduced(avatar_definition, [new_symbols(naming, [spl144_138])])).
fof(f1843, plain, (spl144_137 <=> sP57), introduced(avatar_definition, [new_symbols(naming, [spl144_137])])).
fof(f1835, plain, (spl144_136 <=> sP58), introduced(avatar_definition, [new_symbols(naming, [spl144_136])])).
fof(f1827, plain, (spl144_135 <=> sP59), introduced(avatar_definition, [new_symbols(naming, [spl144_135])])).
fof(f1819, plain, (spl144_134 <=> sP60), introduced(avatar_definition, [new_symbols(naming, [spl144_134])])).
fof(f1811, plain, (spl144_133 <=> sP61), introduced(avatar_definition, [new_symbols(naming, [spl144_133])])).
fof(f1803, plain, (spl144_132 <=> sP62), introduced(avatar_definition, [new_symbols(naming, [spl144_132])])).
fof(f1795, plain, (spl144_131 <=> sP63), introduced(avatar_definition, [new_symbols(naming, [spl144_131])])).
fof(f1787, plain, (spl144_130 <=> sP64), introduced(avatar_definition, [new_symbols(naming, [spl144_130])])).
fof(f1779, plain, (spl144_129 <=> sP65), introduced(avatar_definition, [new_symbols(naming, [spl144_129])])).
fof(f804, plain, ((e13 = op1(e13, e13)) | sP65 | sP64 | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f87])).
fof(f2308, plain, (spl144_191 | spl144_190 | spl144_189 | spl144_188 | spl144_187 | spl144_186 | spl144_185 | spl144_184 | spl144_183 | spl144_182 | spl144_181 | spl144_180 | spl144_179 | spl144_178 | spl144_177 | spl144_176 | spl144_175 | spl144_174 | spl144_173 | spl144_172 | spl144_171 | spl144_170 | spl144_169 | spl144_168 | spl144_167 | spl144_166 | spl144_165 | spl144_164 | spl144_163 | spl144_162 | spl144_161 | spl144_160 | spl144_159 | spl144_158 | spl144_157 | spl144_156 | spl144_155 | spl144_154 | spl144_153 | spl144_152 | spl144_151 | spl144_150 | spl144_149 | spl144_148 | spl144_147 | spl144_146 | spl144_145 | spl144_144 | spl144_143 | spl144_142 | spl144_141 | spl144_140 | spl144_139 | spl144_138 | spl144_137 | spl144_136 | spl144_135 | spl144_134 | spl144_133 | spl144_132 | spl144_131 | spl144_130 | spl144_129 | ~ spl144_4), inference(avatar_split_clause, [], [f805, f1183, f1779, f1787, f1795, f1803, f1811, f1819, f1827, f1835, f1843, f1851, f1859, f1867, f1875, f1883, f1891, f1899, f1907, f1915, f1923, f1931, f1939, f1947, f1955, f1963, f1971, f1979, f1987, f1995, f2003, f2011, f2019, f2027, f2035, f2043, f2051, f2059, f2067, f2075, f2083, f2091, f2099, f2107, f2115, f2123, f2131, f2139, f2147, f2155, f2163, f2171, f2179, f2187, f2195, f2203, f2211, f2219, f2227, f2235, f2243, f2251, f2259, f2267, f2275])).
fof(f805, plain, (~ (e13 = op1(e13, e13)) | sP65 | sP64 | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f87])).
fof(f2305, plain, (~ spl144_194 | spl144_61), inference(avatar_split_clause, [], [f794, f1426, f2299])).
fof(f794, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f233])).
fof(f233, plain, ((((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f2296, plain, (~ spl144_193 | ~ spl144_62 | spl144_57), inference(avatar_split_clause, [], [f790, f1409, f1430, f2291])).
fof(f790, plain, ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11) | ~ sP1), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ((((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & (e11 = op1(e11, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f2289, plain, (~ spl144_192 | spl144_23), inference(avatar_split_clause, [], [f784, f1264, f2283])).
fof(f784, plain, ((e12 = op1(e12, e12)) | ~ sP2), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ((((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & (e12 = op1(e12, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f2286, plain, (~ spl144_192 | ~ spl144_3 | spl144_8), inference(avatar_split_clause, [], [f788, f1200, f1179, f2283])).
fof(f788, plain, ((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13)) | ~ sP2), inference(cnf_transformation, [], [f231])).
fof(f2281, plain, (~ spl144_191 | spl144_61), inference(avatar_split_clause, [], [f780, f1426, f2275])).
fof(f780, plain, ((e10 = op1(e10, e10)) | ~ sP3), inference(cnf_transformation, [], [f230])).
fof(f230, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f2273, plain, (~ spl144_190 | spl144_61), inference(avatar_split_clause, [], [f776, f1426, f2267])).
fof(f776, plain, ((e10 = op1(e10, e10)) | ~ sP4), inference(cnf_transformation, [], [f229])).
fof(f229, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f2265, plain, (~ spl144_189 | spl144_61), inference(avatar_split_clause, [], [f772, f1426, f2259])).
fof(f772, plain, ((e10 = op1(e10, e10)) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f2257, plain, (~ spl144_188 | spl144_61), inference(avatar_split_clause, [], [f768, f1426, f2251])).
fof(f768, plain, ((e10 = op1(e10, e10)) | ~ sP6), inference(cnf_transformation, [], [f227])).
fof(f227, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f2248, plain, (~ spl144_187 | ~ spl144_62), inference(avatar_split_clause, [], [f765, f1430, f2243])).
fof(f765, plain, (~ (op1(e10, e10) = e11) | ~ sP7), inference(cnf_transformation, [], [f226])).
fof(f226, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f2240, plain, (~ spl144_186 | ~ spl144_62), inference(avatar_split_clause, [], [f761, f1430, f2235])).
fof(f761, plain, (~ (op1(e10, e10) = e11) | ~ sP8), inference(cnf_transformation, [], [f225])).
fof(f225, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f2232, plain, (~ spl144_185 | ~ spl144_62), inference(avatar_split_clause, [], [f757, f1430, f2227])).
fof(f757, plain, (~ (op1(e10, e10) = e11) | ~ sP9), inference(cnf_transformation, [], [f224])).
fof(f224, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f2224, plain, (~ spl144_184 | ~ spl144_62), inference(avatar_split_clause, [], [f753, f1430, f2219])).
fof(f753, plain, (~ (op1(e10, e10) = e11) | ~ sP10), inference(cnf_transformation, [], [f223])).
fof(f223, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f2215, plain, (~ spl144_183 | spl144_63), inference(avatar_split_clause, [], [f750, f1434, f2211])).
fof(f750, plain, ((op1(e10, e10) = e12) | ~ sP11), inference(cnf_transformation, [], [f222])).
fof(f222, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f2207, plain, (~ spl144_182 | spl144_43), inference(avatar_split_clause, [], [f746, f1349, f2203])).
fof(f746, plain, ((e12 = op1(e11, e11)) | ~ sP12), inference(cnf_transformation, [], [f221])).
fof(f221, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f2201, plain, (~ spl144_181 | spl144_53), inference(avatar_split_clause, [], [f740, f1392, f2195])).
fof(f740, plain, ((e10 = op1(e10, e12)) | ~ sP13), inference(cnf_transformation, [], [f220])).
fof(f220, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f2199, plain, (~ spl144_181 | spl144_23), inference(avatar_split_clause, [], [f742, f1264, f2195])).
fof(f742, plain, ((e12 = op1(e12, e12)) | ~ sP13), inference(cnf_transformation, [], [f220])).
fof(f2198, plain, (~ spl144_181 | ~ spl144_23), inference(avatar_split_clause, [], [f743, f1264, f2195])).
fof(f743, plain, (~ (e12 = op1(e12, e12)) | ~ sP13), inference(cnf_transformation, [], [f220])).
fof(f2193, plain, (~ spl144_180 | spl144_53), inference(avatar_split_clause, [], [f736, f1392, f2187])).
fof(f736, plain, ((e10 = op1(e10, e12)) | ~ sP14), inference(cnf_transformation, [], [f219])).
fof(f219, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f2191, plain, (~ spl144_180 | spl144_3), inference(avatar_split_clause, [], [f738, f1179, f2187])).
fof(f738, plain, ((e12 = op1(e13, e13)) | ~ sP14), inference(cnf_transformation, [], [f219])).
fof(f2190, plain, (~ spl144_180 | ~ spl144_8), inference(avatar_split_clause, [], [f739, f1200, f2187])).
fof(f739, plain, (~ (e13 = op1(e13, e12)) | ~ sP14), inference(cnf_transformation, [], [f219])).
fof(f2183, plain, (~ spl144_179 | spl144_64), inference(avatar_split_clause, [], [f734, f1438, f2179])).
fof(f734, plain, ((op1(e10, e10) = e13) | ~ sP15), inference(cnf_transformation, [], [f218])).
fof(f218, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP15), inference(nnf_transformation, [], [f36])).
fof(f2177, plain, (~ spl144_178 | spl144_49), inference(avatar_split_clause, [], [f728, f1375, f2171])).
fof(f728, plain, ((e10 = op1(e10, e13)) | ~ sP16), inference(cnf_transformation, [], [f217])).
fof(f217, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP16), inference(nnf_transformation, [], [f37])).
fof(f2174, plain, (~ spl144_178 | ~ spl144_34), inference(avatar_split_clause, [], [f731, f1311, f2171])).
fof(f731, plain, (~ (e11 = op1(e11, e13)) | ~ sP16), inference(cnf_transformation, [], [f217])).
fof(f2167, plain, (~ spl144_177 | spl144_24), inference(avatar_split_clause, [], [f726, f1268, f2163])).
fof(f726, plain, ((e13 = op1(e12, e12)) | ~ sP17), inference(cnf_transformation, [], [f216])).
fof(f216, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP17), inference(nnf_transformation, [], [f38])).
fof(f2161, plain, (~ spl144_176 | spl144_49), inference(avatar_split_clause, [], [f720, f1375, f2155])).
fof(f720, plain, ((e10 = op1(e10, e13)) | ~ sP18), inference(cnf_transformation, [], [f215])).
fof(f215, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP18), inference(nnf_transformation, [], [f39])).
fof(f2159, plain, (~ spl144_176 | spl144_4), inference(avatar_split_clause, [], [f722, f1183, f2155])).
fof(f722, plain, ((e13 = op1(e13, e13)) | ~ sP18), inference(cnf_transformation, [], [f215])).
fof(f2158, plain, (~ spl144_176 | ~ spl144_4), inference(avatar_split_clause, [], [f723, f1183, f2155])).
fof(f723, plain, (~ (e13 = op1(e13, e13)) | ~ sP18), inference(cnf_transformation, [], [f215])).
fof(f2153, plain, (~ spl144_175 | spl144_46), inference(avatar_split_clause, [], [f716, f1362, f2147])).
fof(f716, plain, ((e11 = op1(e11, e10)) | ~ sP19), inference(cnf_transformation, [], [f214])).
fof(f214, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP19), inference(nnf_transformation, [], [f40])).
fof(f2145, plain, (~ spl144_174 | spl144_46), inference(avatar_split_clause, [], [f712, f1362, f2139])).
fof(f712, plain, ((e11 = op1(e11, e10)) | ~ sP20), inference(cnf_transformation, [], [f213])).
fof(f213, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP20), inference(nnf_transformation, [], [f41])).
fof(f2137, plain, (~ spl144_173 | spl144_46), inference(avatar_split_clause, [], [f708, f1362, f2131])).
fof(f708, plain, ((e11 = op1(e11, e10)) | ~ sP21), inference(cnf_transformation, [], [f212])).
fof(f212, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP21), inference(nnf_transformation, [], [f42])).
fof(f2129, plain, (~ spl144_172 | spl144_46), inference(avatar_split_clause, [], [f704, f1362, f2123])).
fof(f704, plain, ((e11 = op1(e11, e10)) | ~ sP22), inference(cnf_transformation, [], [f211])).
fof(f211, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP22), inference(nnf_transformation, [], [f43])).
fof(f2121, plain, (~ spl144_171 | spl144_42), inference(avatar_split_clause, [], [f700, f1345, f2115])).
fof(f700, plain, ((e11 = op1(e11, e11)) | ~ sP23), inference(cnf_transformation, [], [f210])).
fof(f210, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP23), inference(nnf_transformation, [], [f44])).
fof(f2113, plain, (~ spl144_170 | spl144_42), inference(avatar_split_clause, [], [f696, f1345, f2107])).
fof(f696, plain, ((e11 = op1(e11, e11)) | ~ sP24), inference(cnf_transformation, [], [f209])).
fof(f209, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP24), inference(nnf_transformation, [], [f45])).
fof(f2105, plain, (~ spl144_169 | spl144_42), inference(avatar_split_clause, [], [f692, f1345, f2099])).
fof(f692, plain, ((e11 = op1(e11, e11)) | ~ sP25), inference(cnf_transformation, [], [f208])).
fof(f208, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP25), inference(nnf_transformation, [], [f46])).
fof(f2097, plain, (~ spl144_168 | spl144_42), inference(avatar_split_clause, [], [f688, f1345, f2091])).
fof(f688, plain, ((e11 = op1(e11, e11)) | ~ sP26), inference(cnf_transformation, [], [f207])).
fof(f207, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP26), inference(nnf_transformation, [], [f47])).
fof(f2087, plain, (~ spl144_167 | spl144_63), inference(avatar_split_clause, [], [f686, f1434, f2083])).
fof(f686, plain, ((op1(e10, e10) = e12) | ~ sP27), inference(cnf_transformation, [], [f206])).
fof(f206, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP27), inference(nnf_transformation, [], [f48])).
fof(f2079, plain, (~ spl144_166 | spl144_43), inference(avatar_split_clause, [], [f682, f1349, f2075])).
fof(f682, plain, ((e12 = op1(e11, e11)) | ~ sP28), inference(cnf_transformation, [], [f205])).
fof(f205, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP28), inference(nnf_transformation, [], [f49])).
fof(f2071, plain, (~ spl144_165 | spl144_23), inference(avatar_split_clause, [], [f678, f1264, f2067])).
fof(f678, plain, ((e12 = op1(e12, e12)) | ~ sP29), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP29), inference(nnf_transformation, [], [f50])).
fof(f2070, plain, (~ spl144_165 | ~ spl144_23), inference(avatar_split_clause, [], [f679, f1264, f2067])).
fof(f679, plain, (~ (e12 = op1(e12, e12)) | ~ sP29), inference(cnf_transformation, [], [f204])).
fof(f2063, plain, (~ spl144_164 | spl144_3), inference(avatar_split_clause, [], [f674, f1179, f2059])).
fof(f674, plain, ((e12 = op1(e13, e13)) | ~ sP30), inference(cnf_transformation, [], [f203])).
fof(f203, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP30), inference(nnf_transformation, [], [f51])).
fof(f2062, plain, (~ spl144_164 | ~ spl144_8), inference(avatar_split_clause, [], [f675, f1200, f2059])).
fof(f675, plain, (~ (e13 = op1(e13, e12)) | ~ sP30), inference(cnf_transformation, [], [f203])).
fof(f2055, plain, (~ spl144_163 | spl144_64), inference(avatar_split_clause, [], [f670, f1438, f2051])).
fof(f670, plain, ((op1(e10, e10) = e13) | ~ sP31), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP31), inference(nnf_transformation, [], [f52])).
fof(f2049, plain, (~ spl144_162 | spl144_34), inference(avatar_split_clause, [], [f664, f1311, f2043])).
fof(f664, plain, ((e11 = op1(e11, e13)) | ~ sP32), inference(cnf_transformation, [], [f201])).
fof(f201, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP32), inference(nnf_transformation, [], [f53])).
fof(f2048, plain, (~ spl144_162 | ~ spl144_44), inference(avatar_split_clause, [], [f665, f1353, f2043])).
fof(f665, plain, (~ (e13 = op1(e11, e11)) | ~ sP32), inference(cnf_transformation, [], [f201])).
fof(f2039, plain, (~ spl144_161 | spl144_24), inference(avatar_split_clause, [], [f662, f1268, f2035])).
fof(f662, plain, ((e13 = op1(e12, e12)) | ~ sP33), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP33), inference(nnf_transformation, [], [f54])).
fof(f2031, plain, (~ spl144_160 | spl144_4), inference(avatar_split_clause, [], [f658, f1183, f2027])).
fof(f658, plain, ((e13 = op1(e13, e13)) | ~ sP34), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP34), inference(nnf_transformation, [], [f55])).
fof(f2030, plain, (~ spl144_160 | ~ spl144_4), inference(avatar_split_clause, [], [f659, f1183, f2027])).
fof(f659, plain, (~ (e13 = op1(e13, e13)) | ~ sP34), inference(cnf_transformation, [], [f199])).
fof(f2025, plain, (~ spl144_159 | spl144_31), inference(avatar_split_clause, [], [f652, f1298, f2019])).
fof(f652, plain, ((e12 = op1(e12, e10)) | ~ sP35), inference(cnf_transformation, [], [f198])).
fof(f198, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP35), inference(nnf_transformation, [], [f56])).
fof(f2017, plain, (~ spl144_158 | spl144_31), inference(avatar_split_clause, [], [f648, f1298, f2011])).
fof(f648, plain, ((e12 = op1(e12, e10)) | ~ sP36), inference(cnf_transformation, [], [f197])).
fof(f197, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP36), inference(nnf_transformation, [], [f57])).
fof(f2009, plain, (~ spl144_157 | spl144_31), inference(avatar_split_clause, [], [f644, f1298, f2003])).
fof(f644, plain, ((e12 = op1(e12, e10)) | ~ sP37), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP37), inference(nnf_transformation, [], [f58])).
fof(f2001, plain, (~ spl144_156 | spl144_31), inference(avatar_split_clause, [], [f640, f1298, f1995])).
fof(f640, plain, ((e12 = op1(e12, e10)) | ~ sP38), inference(cnf_transformation, [], [f195])).
fof(f195, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP38), inference(nnf_transformation, [], [f59])).
fof(f1993, plain, (~ spl144_155 | spl144_27), inference(avatar_split_clause, [], [f636, f1281, f1987])).
fof(f636, plain, ((e12 = op1(e12, e11)) | ~ sP39), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP39), inference(nnf_transformation, [], [f60])).
fof(f1985, plain, (~ spl144_154 | spl144_27), inference(avatar_split_clause, [], [f632, f1281, f1979])).
fof(f632, plain, ((e12 = op1(e12, e11)) | ~ sP40), inference(cnf_transformation, [], [f193])).
fof(f193, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP40), inference(nnf_transformation, [], [f61])).
fof(f1983, plain, (~ spl144_154 | spl144_42), inference(avatar_split_clause, [], [f634, f1345, f1979])).
fof(f634, plain, ((e11 = op1(e11, e11)) | ~ sP40), inference(cnf_transformation, [], [f193])).
fof(f1977, plain, (~ spl144_153 | spl144_27), inference(avatar_split_clause, [], [f628, f1281, f1971])).
fof(f628, plain, ((e12 = op1(e12, e11)) | ~ sP41), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP41), inference(nnf_transformation, [], [f62])).
fof(f1969, plain, (~ spl144_152 | spl144_27), inference(avatar_split_clause, [], [f624, f1281, f1963])).
fof(f624, plain, ((e12 = op1(e12, e11)) | ~ sP42), inference(cnf_transformation, [], [f191])).
fof(f191, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP42), inference(nnf_transformation, [], [f63])).
fof(f1959, plain, (~ spl144_151 | spl144_63), inference(avatar_split_clause, [], [f622, f1434, f1955])).
fof(f622, plain, ((op1(e10, e10) = e12) | ~ sP43), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP43), inference(nnf_transformation, [], [f64])).
fof(f1951, plain, (~ spl144_150 | spl144_43), inference(avatar_split_clause, [], [f618, f1349, f1947])).
fof(f618, plain, ((e12 = op1(e11, e11)) | ~ sP44), inference(cnf_transformation, [], [f189])).
fof(f189, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP44), inference(nnf_transformation, [], [f65])).
fof(f1945, plain, (~ spl144_149 | spl144_23), inference(avatar_split_clause, [], [f612, f1264, f1939])).
fof(f612, plain, ((e12 = op1(e12, e12)) | ~ sP45), inference(cnf_transformation, [], [f188])).
fof(f188, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP45), inference(nnf_transformation, [], [f66])).
fof(f1944, plain, (~ spl144_149 | ~ spl144_23), inference(avatar_split_clause, [], [f613, f1264, f1939])).
fof(f613, plain, (~ (e12 = op1(e12, e12)) | ~ sP45), inference(cnf_transformation, [], [f188])).
fof(f1937, plain, (~ spl144_148 | spl144_23), inference(avatar_split_clause, [], [f608, f1264, f1931])).
fof(f608, plain, ((e12 = op1(e12, e12)) | ~ sP46), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP46), inference(nnf_transformation, [], [f67])).
fof(f1936, plain, (~ spl144_148 | ~ spl144_23), inference(avatar_split_clause, [], [f609, f1264, f1931])).
fof(f609, plain, (~ (e12 = op1(e12, e12)) | ~ sP46), inference(cnf_transformation, [], [f187])).
fof(f1935, plain, (~ spl144_148 | spl144_3), inference(avatar_split_clause, [], [f610, f1179, f1931])).
fof(f610, plain, ((e12 = op1(e13, e13)) | ~ sP46), inference(cnf_transformation, [], [f187])).
fof(f1927, plain, (~ spl144_147 | spl144_64), inference(avatar_split_clause, [], [f606, f1438, f1923])).
fof(f606, plain, ((op1(e10, e10) = e13) | ~ sP47), inference(cnf_transformation, [], [f186])).
fof(f186, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP47), inference(nnf_transformation, [], [f68])).
fof(f1921, plain, (~ spl144_146 | spl144_19), inference(avatar_split_clause, [], [f600, f1247, f1915])).
fof(f600, plain, ((e12 = op1(e12, e13)) | ~ sP48), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP48), inference(nnf_transformation, [], [f69])).
fof(f1918, plain, (~ spl144_146 | ~ spl144_34), inference(avatar_split_clause, [], [f603, f1311, f1915])).
fof(f603, plain, (~ (e11 = op1(e11, e13)) | ~ sP48), inference(cnf_transformation, [], [f185])).
fof(f1911, plain, (~ spl144_145 | spl144_24), inference(avatar_split_clause, [], [f598, f1268, f1907])).
fof(f598, plain, ((e13 = op1(e12, e12)) | ~ sP49), inference(cnf_transformation, [], [f184])).
fof(f184, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP49), inference(nnf_transformation, [], [f70])).
fof(f1905, plain, (~ spl144_144 | spl144_19), inference(avatar_split_clause, [], [f592, f1247, f1899])).
fof(f592, plain, ((e12 = op1(e12, e13)) | ~ sP50), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP50), inference(nnf_transformation, [], [f71])).
fof(f1903, plain, (~ spl144_144 | spl144_4), inference(avatar_split_clause, [], [f594, f1183, f1899])).
fof(f594, plain, ((e13 = op1(e13, e13)) | ~ sP50), inference(cnf_transformation, [], [f183])).
fof(f1902, plain, (~ spl144_144 | ~ spl144_4), inference(avatar_split_clause, [], [f595, f1183, f1899])).
fof(f595, plain, (~ (e13 = op1(e13, e13)) | ~ sP50), inference(cnf_transformation, [], [f183])).
fof(f1897, plain, (~ spl144_143 | spl144_16), inference(avatar_split_clause, [], [f588, f1234, f1891])).
fof(f588, plain, ((e13 = op1(e13, e10)) | ~ sP51), inference(cnf_transformation, [], [f182])).
fof(f182, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP51), inference(nnf_transformation, [], [f72])).
fof(f1889, plain, (~ spl144_142 | spl144_16), inference(avatar_split_clause, [], [f584, f1234, f1883])).
fof(f584, plain, ((e13 = op1(e13, e10)) | ~ sP52), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP52), inference(nnf_transformation, [], [f73])).
fof(f1881, plain, (~ spl144_141 | spl144_16), inference(avatar_split_clause, [], [f580, f1234, f1875])).
fof(f580, plain, ((e13 = op1(e13, e10)) | ~ sP53), inference(cnf_transformation, [], [f180])).
fof(f180, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP53), inference(nnf_transformation, [], [f74])).
fof(f1873, plain, (~ spl144_140 | spl144_16), inference(avatar_split_clause, [], [f576, f1234, f1867])).
fof(f576, plain, ((e13 = op1(e13, e10)) | ~ sP54), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP54), inference(nnf_transformation, [], [f75])).
fof(f1865, plain, (~ spl144_139 | spl144_12), inference(avatar_split_clause, [], [f572, f1217, f1859])).
fof(f572, plain, ((e13 = op1(e13, e11)) | ~ sP55), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP55), inference(nnf_transformation, [], [f76])).
fof(f1857, plain, (~ spl144_138 | spl144_12), inference(avatar_split_clause, [], [f568, f1217, f1851])).
fof(f568, plain, ((e13 = op1(e13, e11)) | ~ sP56), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ((~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP56), inference(nnf_transformation, [], [f77])).
fof(f1849, plain, (~ spl144_137 | spl144_12), inference(avatar_split_clause, [], [f564, f1217, f1843])).
fof(f564, plain, ((e13 = op1(e13, e11)) | ~ sP57), inference(cnf_transformation, [], [f176])).
fof(f176, plain, ((~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP57), inference(nnf_transformation, [], [f78])).
fof(f1841, plain, (~ spl144_136 | spl144_12), inference(avatar_split_clause, [], [f560, f1217, f1835])).
fof(f560, plain, ((e13 = op1(e13, e11)) | ~ sP58), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP58), inference(nnf_transformation, [], [f79])).
fof(f1831, plain, (~ spl144_135 | spl144_63), inference(avatar_split_clause, [], [f558, f1434, f1827])).
fof(f558, plain, ((op1(e10, e10) = e12) | ~ sP59), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP59), inference(nnf_transformation, [], [f80])).
fof(f1823, plain, (~ spl144_134 | spl144_43), inference(avatar_split_clause, [], [f554, f1349, f1819])).
fof(f554, plain, ((e12 = op1(e11, e11)) | ~ sP60), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ((~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP60), inference(nnf_transformation, [], [f81])).
fof(f1817, plain, (~ spl144_133 | spl144_8), inference(avatar_split_clause, [], [f548, f1200, f1811])).
fof(f548, plain, ((e13 = op1(e13, e12)) | ~ sP61), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ((~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP61), inference(nnf_transformation, [], [f82])).
fof(f1815, plain, (~ spl144_133 | spl144_23), inference(avatar_split_clause, [], [f550, f1264, f1811])).
fof(f550, plain, ((e12 = op1(e12, e12)) | ~ sP61), inference(cnf_transformation, [], [f172])).
fof(f1814, plain, (~ spl144_133 | ~ spl144_23), inference(avatar_split_clause, [], [f551, f1264, f1811])).
fof(f551, plain, (~ (e12 = op1(e12, e12)) | ~ sP61), inference(cnf_transformation, [], [f172])).
fof(f1809, plain, (~ spl144_132 | spl144_8), inference(avatar_split_clause, [], [f544, f1200, f1803])).
fof(f544, plain, ((e13 = op1(e13, e12)) | ~ sP62), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP62), inference(nnf_transformation, [], [f83])).
fof(f1808, plain, (~ spl144_132 | ~ spl144_3), inference(avatar_split_clause, [], [f545, f1179, f1803])).
fof(f545, plain, (~ (e12 = op1(e13, e13)) | ~ sP62), inference(cnf_transformation, [], [f171])).
fof(f1807, plain, (~ spl144_132 | spl144_3), inference(avatar_split_clause, [], [f546, f1179, f1803])).
fof(f546, plain, ((e12 = op1(e13, e13)) | ~ sP62), inference(cnf_transformation, [], [f171])).
fof(f1806, plain, (~ spl144_132 | ~ spl144_8), inference(avatar_split_clause, [], [f547, f1200, f1803])).
fof(f547, plain, (~ (e13 = op1(e13, e12)) | ~ sP62), inference(cnf_transformation, [], [f171])).
fof(f1799, plain, (~ spl144_131 | spl144_64), inference(avatar_split_clause, [], [f542, f1438, f1795])).
fof(f542, plain, ((op1(e10, e10) = e13) | ~ sP63), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP63), inference(nnf_transformation, [], [f84])).
fof(f1793, plain, (~ spl144_130 | spl144_4), inference(avatar_split_clause, [], [f536, f1183, f1787])).
fof(f536, plain, ((e13 = op1(e13, e13)) | ~ sP64), inference(cnf_transformation, [], [f169])).
fof(f169, plain, ((~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP64), inference(nnf_transformation, [], [f85])).
fof(f1792, plain, (~ spl144_130 | ~ spl144_4), inference(avatar_split_clause, [], [f537, f1183, f1787])).
fof(f537, plain, (~ (e13 = op1(e13, e13)) | ~ sP64), inference(cnf_transformation, [], [f169])).
fof(f1783, plain, (~ spl144_129 | spl144_24), inference(avatar_split_clause, [], [f534, f1268, f1779])).
fof(f534, plain, ((e13 = op1(e12, e12)) | ~ sP65), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ((~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | ~ sP65), inference(nnf_transformation, [], [f86])).
fof(f1772, plain, (spl144_127 | spl144_111 | spl144_95 | spl144_79), inference(avatar_split_clause, [], [f381, f1534, f1602, f1670, f1738])).
fof(f381, plain, ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax4)).
fof(f1771, plain, (spl144_128 | spl144_124 | spl144_120 | spl144_116), inference(avatar_split_clause, [], [f382, f1691, f1708, f1725, f1742])).
fof(f382, plain, ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1767, plain, (spl144_110 | spl144_106 | spl144_102 | spl144_98), inference(avatar_split_clause, [], [f386, f1615, f1632, f1649, f1666])).
fof(f386, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1766, plain, (spl144_122 | spl144_106 | spl144_90 | spl144_74), inference(avatar_split_clause, [], [f387, f1513, f1581, f1649, f1717])).
fof(f387, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1763, plain, (spl144_112 | spl144_108 | spl144_104 | spl144_100), inference(avatar_split_clause, [], [f390, f1623, f1640, f1657, f1674])).
fof(f390, plain, ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1762, plain, (spl144_124 | spl144_108 | spl144_92 | spl144_76), inference(avatar_split_clause, [], [f391, f1521, f1589, f1657, f1725])).
fof(f391, plain, ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1761, plain, (spl144_93 | spl144_89 | spl144_85 | spl144_81), inference(avatar_split_clause, [], [f392, f1543, f1560, f1577, f1594])).
fof(f392, plain, ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1757, plain, (spl144_95 | spl144_91 | spl144_87 | spl144_83), inference(avatar_split_clause, [], [f396, f1551, f1568, f1585, f1602])).
fof(f396, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1754, plain, (spl144_120 | spl144_104 | spl144_88 | spl144_72), inference(avatar_split_clause, [], [f399, f1504, f1572, f1640, f1708])).
fof(f399, plain, ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1753, plain, (spl144_77 | spl144_73 | spl144_69 | spl144_65), inference(avatar_split_clause, [], [f400, f1475, f1492, f1509, f1526])).
fof(f400, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1750, plain, (spl144_114 | spl144_98 | spl144_82 | spl144_66), inference(avatar_split_clause, [], [f403, f1479, f1547, f1615, f1683])).
fof(f403, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1749, plain, (spl144_79 | spl144_75 | spl144_71 | spl144_67), inference(avatar_split_clause, [], [f404, f1483, f1500, f1517, f1534])).
fof(f404, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1748, plain, (spl144_115 | spl144_99 | spl144_83 | spl144_67), inference(avatar_split_clause, [], [f405, f1483, f1551, f1619, f1687])).
fof(f405, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1747, plain, (spl144_80 | spl144_76 | spl144_72 | spl144_68), inference(avatar_split_clause, [], [f406, f1487, f1504, f1521, f1538])).
fof(f406, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1746, plain, (spl144_116 | spl144_100 | spl144_84 | spl144_68), inference(avatar_split_clause, [], [f407, f1487, f1555, f1623, f1691])).
fof(f407, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1711, plain, (spl144_117 | spl144_118 | spl144_119 | spl144_120), inference(avatar_split_clause, [], [f362, f1708, f1704, f1700, f1696])).
fof(f362, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax3)).
fof(f1694, plain, (spl144_113 | spl144_114 | spl144_115 | spl144_116), inference(avatar_split_clause, [], [f363, f1691, f1687, f1683, f1679])).
fof(f363, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f1677, plain, (spl144_109 | spl144_110 | spl144_111 | spl144_112), inference(avatar_split_clause, [], [f364, f1674, f1670, f1666, f1662])).
fof(f364, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1626, plain, (spl144_97 | spl144_98 | spl144_99 | spl144_100), inference(avatar_split_clause, [], [f367, f1623, f1619, f1615, f1611])).
fof(f367, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1558, plain, (spl144_81 | spl144_82 | spl144_83 | spl144_84), inference(avatar_split_clause, [], [f371, f1555, f1551, f1547, f1543])).
fof(f371, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f1507, plain, (spl144_69 | spl144_70 | spl144_71 | spl144_72), inference(avatar_split_clause, [], [f374, f1504, f1500, f1496, f1492])).
fof(f374, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f1490, plain, (spl144_65 | spl144_66 | spl144_67 | spl144_68), inference(avatar_split_clause, [], [f375, f1487, f1483, f1479, f1475])).
fof(f375, plain, ((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))), inference(cnf_transformation, [], [f3])).
fof(f1467, plain, (spl144_64 | spl144_60 | spl144_56 | spl144_52), inference(avatar_split_clause, [], [f334, f1387, f1404, f1421, f1438])).
fof(f334, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax2)).
fof(f1463, plain, (spl144_46 | spl144_42 | spl144_38 | spl144_34), inference(avatar_split_clause, [], [f338, f1311, f1328, f1345, f1362])).
fof(f338, plain, ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f1462, plain, (spl144_58 | spl144_42 | spl144_26 | spl144_10), inference(avatar_split_clause, [], [f339, f1209, f1277, f1345, f1413])).
fof(f339, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f1459, plain, (spl144_48 | spl144_44 | spl144_40 | spl144_36), inference(avatar_split_clause, [], [f342, f1319, f1336, f1353, f1370])).
fof(f342, plain, ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f1458, plain, (spl144_60 | spl144_44 | spl144_28 | spl144_12), inference(avatar_split_clause, [], [f343, f1217, f1285, f1353, f1421])).
fof(f343, plain, ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f1457, plain, (spl144_29 | spl144_25 | spl144_21 | spl144_17), inference(avatar_split_clause, [], [f344, f1239, f1256, f1273, f1290])).
fof(f344, plain, ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f1449, plain, (spl144_13 | spl144_9 | spl144_5 | spl144_1), inference(avatar_split_clause, [], [f352, f1171, f1188, f1205, f1222])).
fof(f352, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1446, plain, (spl144_50 | spl144_34 | spl144_18 | spl144_2), inference(avatar_split_clause, [], [f355, f1175, f1243, f1311, f1379])).
fof(f355, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1445, plain, (spl144_15 | spl144_11 | spl144_7 | spl144_3), inference(avatar_split_clause, [], [f356, f1179, f1196, f1213, f1230])).
fof(f356, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1444, plain, (spl144_51 | spl144_35 | spl144_19 | spl144_3), inference(avatar_split_clause, [], [f357, f1179, f1247, f1315, f1383])).
fof(f357, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1443, plain, (spl144_16 | spl144_12 | spl144_8 | spl144_4), inference(avatar_split_clause, [], [f358, f1183, f1200, f1217, f1234])).
fof(f358, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f1442, plain, (spl144_52 | spl144_36 | spl144_20 | spl144_4), inference(avatar_split_clause, [], [f359, f1183, f1251, f1319, f1387])).
fof(f359, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f1407, plain, (spl144_53 | spl144_54 | spl144_55 | spl144_56), inference(avatar_split_clause, [], [f314, f1404, f1400, f1396, f1392])).
fof(f314, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG103+1.p', ax1)).
fof(f1390, plain, (spl144_49 | spl144_50 | spl144_51 | spl144_52), inference(avatar_split_clause, [], [f315, f1387, f1383, f1379, f1375])).
fof(f315, plain, ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f1])).
fof(f1373, plain, (spl144_45 | spl144_46 | spl144_47 | spl144_48), inference(avatar_split_clause, [], [f316, f1370, f1366, f1362, f1358])).
fof(f316, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f1356, plain, (spl144_41 | spl144_42 | spl144_43 | spl144_44), inference(avatar_split_clause, [], [f317, f1353, f1349, f1345, f1341])).
fof(f317, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f1288, plain, (spl144_25 | spl144_26 | spl144_27 | spl144_28), inference(avatar_split_clause, [], [f321, f1285, f1281, f1277, f1273])).
fof(f321, plain, ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f1254, plain, (spl144_17 | spl144_18 | spl144_19 | spl144_20), inference(avatar_split_clause, [], [f323, f1251, f1247, f1243, f1239])).
fof(f323, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f1220, plain, (spl144_9 | spl144_10 | spl144_11 | spl144_12), inference(avatar_split_clause, [], [f325, f1217, f1213, f1209, f1205])).
fof(f325, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))), inference(cnf_transformation, [], [f1])).
fof(f1203, plain, (spl144_5 | spl144_6 | spl144_7 | spl144_8), inference(avatar_split_clause, [], [f326, f1200, f1196, f1192, f1188])).
fof(f326, plain, ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))), inference(cnf_transformation, [], [f1])).
fof(f1186, plain, (spl144_1 | spl144_2 | spl144_3 | spl144_4), inference(avatar_split_clause, [], [f327, f1183, f1179, f1175, f1171])).
fof(f327, plain, ((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))), inference(cnf_transformation, [], [f1])).