fof(f2491, plain, $false, inference(avatar_sat_refutation, [], [f479, f521, f563, f626, f710, f731, f817, f828, f837, f845, f848, f851, f865, f866, f869, f877, f885, f896, f901, f910, f921, f929, f940, f943, f951, f961, f962, f966, f968, f969, f994, f1009, f1019, f1037, f1120, f1131, f1254, f1279, f1282, f1330, f1366, f1379, f1403, f1505, f1577, f1603, f1636, f1644, f1678, f1687, f1706, f1757, f1763, f1770, f1774, f1794, f1815, f1913, f1915, f1942, f1969, f1978, f1983, f2035, f2107, f2112, f2140, f2141, f2144, f2149, f2252, f2253, f2287, f2294, f2296, f2317, f2364, f2403, f2429, f2480])).
fof(f2480, plain, (~ spl8_16 | ~ spl8_20), inference(avatar_contradiction_clause, [], [f2479])).
fof(f2479, plain, ($false | (~ spl8_16 | ~ spl8_20)), inference(subsumption_resolution, [], [f2478, f230])).
fof(f230, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG170+1.p', ax5)).
fof(f2478, plain, ((e0 = e4) | (~ spl8_16 | ~ spl8_20)), inference(forward_demodulation, [], [f373, f357])).
fof(f357, plain, ((e0 = op(e4, e1)) | ~ spl8_16), inference(avatar_component_clause, [], [f355])).
fof(f355, plain, (spl8_16 <=> (e0 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_16])])).
fof(f373, plain, ((e4 = op(e4, e1)) | ~ spl8_20), inference(avatar_component_clause, [], [f371])).
fof(f371, plain, (spl8_20 <=> (e4 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_20])])).
fof(f2429, plain, (~ spl8_31 | ~ spl8_64), inference(avatar_contradiction_clause, [], [f2428])).
fof(f2428, plain, ($false | (~ spl8_31 | ~ spl8_64)), inference(subsumption_resolution, [], [f2427, f228])).
fof(f228, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f2427, plain, ((e0 = e2) | (~ spl8_31 | ~ spl8_64)), inference(forward_demodulation, [], [f2421, f420])).
fof(f420, plain, ((e0 = op(e3, e3)) | ~ spl8_31), inference(avatar_component_clause, [], [f418])).
fof(f418, plain, (spl8_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_31])])).
fof(f2421, plain, ((e2 = op(e3, e3)) | ~ spl8_64), inference(backward_demodulation, [], [f114, f558])).
fof(f558, plain, ((e3 = op(e2, e2)) | ~ spl8_64), inference(avatar_component_clause, [], [f556])).
fof(f556, plain, (spl8_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_64])])).
fof(f114, plain, (e2 = op(op(e2, e2), op(e2, e2))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ((e4 = op(op(e4, e4), op(e4, e4))) & (e4 = op(op(e3, e4), op(e4, e3))) & (e4 = op(op(e2, e4), op(e4, e2))) & (e4 = op(op(e1, e4), op(e4, e1))) & (e4 = op(op(e0, e4), op(e4, e0))) & (e3 = op(op(e4, e3), op(e3, e4))) & (e3 = op(op(e3, e3), op(e3, e3))) & (e3 = op(op(e2, e3), op(e3, e2))) & (e3 = op(op(e1, e3), op(e3, e1))) & (e3 = op(op(e0, e3), op(e3, e0))) & (e2 = op(op(e4, e2), op(e2, e4))) & (e2 = op(op(e3, e2), op(e2, e3))) & (e2 = op(op(e2, e2), op(e2, e2))) & (e2 = op(op(e1, e2), op(e2, e1))) & (e2 = op(op(e0, e2), op(e2, e0))) & (e1 = op(op(e4, e1), op(e1, e4))) & (e1 = op(op(e3, e1), op(e1, e3))) & (e1 = op(op(e2, e1), op(e1, e2))) & (e1 = op(op(e1, e1), op(e1, e1))) & (e1 = op(op(e0, e1), op(e1, e0))) & (e0 = op(op(e4, e0), op(e0, e4))) & (e0 = op(op(e3, e0), op(e0, e3))) & (e0 = op(op(e2, e0), op(e0, e2))) & (e0 = op(op(e1, e0), op(e0, e1))) & (e0 = op(op(e0, e0), op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG170+1.p', ax3)).
fof(f2403, plain, (~ spl8_93 | ~ spl8_95), inference(avatar_contradiction_clause, [], [f2402])).
fof(f2402, plain, ($false | (~ spl8_93 | ~ spl8_95)), inference(subsumption_resolution, [], [f2401, f235])).
fof(f235, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f2401, plain, ((e2 = e4) | (~ spl8_93 | ~ spl8_95)), inference(backward_demodulation, [], [f688, f680])).
fof(f680, plain, ((e2 = op(e1, e1)) | ~ spl8_93), inference(avatar_component_clause, [], [f678])).
fof(f678, plain, (spl8_93 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_93])])).
fof(f688, plain, ((e4 = op(e1, e1)) | ~ spl8_95), inference(avatar_component_clause, [], [f686])).
fof(f686, plain, (spl8_95 <=> (e4 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_95])])).
fof(f2364, plain, (~ spl8_2 | ~ spl8_5), inference(avatar_contradiction_clause, [], [f2363])).
fof(f2363, plain, ($false | (~ spl8_2 | ~ spl8_5)), inference(subsumption_resolution, [], [f2362, f233])).
fof(f233, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f2362, plain, ((e1 = e4) | (~ spl8_2 | ~ spl8_5)), inference(forward_demodulation, [], [f310, f298])).
fof(f298, plain, ((e1 = op(e4, e4)) | ~ spl8_2), inference(avatar_component_clause, [], [f296])).
fof(f296, plain, (spl8_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_2])])).
fof(f310, plain, ((e4 = op(e4, e4)) | ~ spl8_5), inference(avatar_component_clause, [], [f308])).
fof(f308, plain, (spl8_5 <=> (e4 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_5])])).
fof(f2317, plain, (spl8_93 | ~ spl8_62), inference(avatar_split_clause, [], [f2311, f548, f678])).
fof(f548, plain, (spl8_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_62])])).
fof(f2311, plain, ((e2 = op(e1, e1)) | ~ spl8_62), inference(backward_demodulation, [], [f114, f550])).
fof(f550, plain, ((e1 = op(e2, e2)) | ~ spl8_62), inference(avatar_component_clause, [], [f548])).
fof(f2296, plain, (~ spl8_16 | ~ spl8_78 | spl8_112), inference(avatar_contradiction_clause, [], [f2295])).
fof(f2295, plain, ($false | (~ spl8_16 | ~ spl8_78 | spl8_112)), inference(subsumption_resolution, [], [f2291, f759])).
fof(f759, plain, (~ (e1 = op(e0, e2)) | spl8_112), inference(avatar_component_clause, [], [f758])).
fof(f758, plain, (spl8_112 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_112])])).
fof(f2291, plain, ((e1 = op(e0, e2)) | (~ spl8_16 | ~ spl8_78)), inference(backward_demodulation, [], [f1740, f617])).
fof(f617, plain, ((e2 = op(e1, e4)) | ~ spl8_78), inference(avatar_component_clause, [], [f615])).
fof(f615, plain, (spl8_78 <=> (e2 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_78])])).
fof(f1740, plain, ((e1 = op(e0, op(e1, e4))) | ~ spl8_16), inference(forward_demodulation, [], [f111, f357])).
fof(f111, plain, (e1 = op(op(e4, e1), op(e1, e4))), inference(cnf_transformation, [], [f3])).
fof(f2294, plain, (~ spl8_16 | ~ spl8_71 | ~ spl8_78), inference(avatar_contradiction_clause, [], [f2293])).
fof(f2293, plain, ($false | (~ spl8_16 | ~ spl8_71 | ~ spl8_78)), inference(subsumption_resolution, [], [f2292, f230])).
fof(f2292, plain, ((e0 = e4) | (~ spl8_16 | ~ spl8_71 | ~ spl8_78)), inference(forward_demodulation, [], [f2290, f588])).
fof(f588, plain, ((e0 = op(e2, e0)) | ~ spl8_71), inference(avatar_component_clause, [], [f586])).
fof(f586, plain, (spl8_71 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_71])])).
fof(f2290, plain, ((e4 = op(e2, e0)) | (~ spl8_16 | ~ spl8_78)), inference(backward_demodulation, [], [f1532, f617])).
fof(f1532, plain, ((e4 = op(op(e1, e4), e0)) | ~ spl8_16), inference(forward_demodulation, [], [f123, f357])).
fof(f123, plain, (e4 = op(op(e1, e4), op(e4, e1))), inference(cnf_transformation, [], [f3])).
fof(f2287, plain, (~ spl8_83 | ~ spl8_84), inference(avatar_contradiction_clause, [], [f2286])).
fof(f2286, plain, ($false | (~ spl8_83 | ~ spl8_84)), inference(subsumption_resolution, [], [f2285, f234])).
fof(f234, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f2285, plain, ((e2 = e3) | (~ spl8_83 | ~ spl8_84)), inference(forward_demodulation, [], [f642, f638])).
fof(f638, plain, ((e2 = op(e1, e3)) | ~ spl8_83), inference(avatar_component_clause, [], [f636])).
fof(f636, plain, (spl8_83 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_83])])).
fof(f642, plain, ((e3 = op(e1, e3)) | ~ spl8_84), inference(avatar_component_clause, [], [f640])).
fof(f640, plain, (spl8_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_84])])).
fof(f2253, plain, (spl8_67 | ~ spl8_97 | ~ spl8_118), inference(avatar_split_clause, [], [f2005, f783, f695, f569])).
fof(f569, plain, (spl8_67 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_67])])).
fof(f695, plain, (spl8_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_97])])).
fof(f783, plain, (spl8_118 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_118])])).
fof(f2005, plain, ((e1 = op(e2, e1)) | (~ spl8_97 | ~ spl8_118)), inference(backward_demodulation, [], [f1766, f697])).
fof(f697, plain, ((e1 = op(e1, e0)) | ~ spl8_97), inference(avatar_component_clause, [], [f695])).
fof(f1766, plain, ((e1 = op(e2, op(e1, e0))) | ~ spl8_118), inference(forward_demodulation, [], [f107, f785])).
fof(f785, plain, ((e2 = op(e0, e1)) | ~ spl8_118), inference(avatar_component_clause, [], [f783])).
fof(f107, plain, (e1 = op(op(e0, e1), op(e1, e0))), inference(cnf_transformation, [], [f3])).
fof(f2252, plain, (~ spl8_63 | ~ spl8_53), inference(avatar_split_clause, [], [f2251, f510, f552])).
fof(f552, plain, (spl8_63 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_63])])).
fof(f510, plain, (spl8_53 <=> (e2 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_53])])).
fof(f2251, plain, (~ (e2 = op(e2, e2)) | ~ spl8_53), inference(forward_demodulation, [], [f205, f512])).
fof(f512, plain, ((e2 = op(e2, e4)) | ~ spl8_53), inference(avatar_component_clause, [], [f510])).
fof(f205, plain, ~ (op(e2, e2) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG170+1.p', ax4)).
fof(f2149, plain, (~ spl8_94 | ~ spl8_95), inference(avatar_contradiction_clause, [], [f2148])).
fof(f2148, plain, ($false | (~ spl8_94 | ~ spl8_95)), inference(subsumption_resolution, [], [f2147, f236])).
fof(f236, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f2147, plain, ((e3 = e4) | (~ spl8_94 | ~ spl8_95)), inference(backward_demodulation, [], [f688, f684])).
fof(f684, plain, ((e3 = op(e1, e1)) | ~ spl8_94), inference(avatar_component_clause, [], [f682])).
fof(f682, plain, (spl8_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_94])])).
fof(f2144, plain, (~ spl8_25 | ~ spl8_101), inference(avatar_contradiction_clause, [], [f2143])).
fof(f2143, plain, ($false | (~ spl8_25 | ~ spl8_101)), inference(subsumption_resolution, [], [f2142, f230])).
fof(f2142, plain, ((e0 = e4) | (~ spl8_25 | ~ spl8_101)), inference(forward_demodulation, [], [f2138, f714])).
fof(f714, plain, ((e0 = op(e0, e4)) | ~ spl8_101), inference(avatar_component_clause, [], [f712])).
fof(f712, plain, (spl8_101 <=> (e0 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_101])])).
fof(f2138, plain, ((e4 = op(e0, e4)) | (~ spl8_25 | ~ spl8_101)), inference(backward_demodulation, [], [f1971, f714])).
fof(f1971, plain, ((e4 = op(op(e0, e4), e4)) | ~ spl8_25), inference(forward_demodulation, [], [f122, f394])).
fof(f394, plain, ((e4 = op(e4, e0)) | ~ spl8_25), inference(avatar_component_clause, [], [f392])).
fof(f392, plain, (spl8_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_25])])).
fof(f122, plain, (e4 = op(op(e0, e4), op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f2141, plain, (~ spl8_51 | ~ spl8_101), inference(avatar_split_clause, [], [f2133, f712, f502])).
fof(f502, plain, (spl8_51 <=> (e0 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_51])])).
fof(f2133, plain, (~ (e0 = op(e2, e4)) | ~ spl8_101), inference(backward_demodulation, [], [f168, f714])).
fof(f168, plain, ~ (op(e0, e4) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f2140, plain, (~ spl8_76 | ~ spl8_101), inference(avatar_split_clause, [], [f2132, f712, f607])).
fof(f607, plain, (spl8_76 <=> (e0 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_76])])).
fof(f2132, plain, (~ (e0 = op(e1, e4)) | ~ spl8_101), inference(backward_demodulation, [], [f167, f714])).
fof(f167, plain, ~ (op(e0, e4) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f2112, plain, (spl8_86 | ~ spl8_97 | ~ spl8_118), inference(avatar_split_clause, [], [f2004, f783, f695, f649])).
fof(f649, plain, (spl8_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_86])])).
fof(f2004, plain, ((e0 = op(e1, e2)) | (~ spl8_97 | ~ spl8_118)), inference(backward_demodulation, [], [f1765, f697])).
fof(f1765, plain, ((e0 = op(op(e1, e0), e2)) | ~ spl8_118), inference(forward_demodulation, [], [f103, f785])).
fof(f103, plain, (e0 = op(op(e1, e0), op(e0, e1))), inference(cnf_transformation, [], [f3])).
fof(f2107, plain, (~ spl8_77 | ~ spl8_2), inference(avatar_split_clause, [], [f1361, f296, f611])).
fof(f611, plain, (spl8_77 <=> (e1 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_77])])).
fof(f1361, plain, (~ (e1 = op(e1, e4)) | ~ spl8_2), inference(forward_demodulation, [], [f173, f298])).
fof(f173, plain, ~ (op(e1, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2035, plain, (~ spl8_67 | ~ spl8_69), inference(avatar_contradiction_clause, [], [f2034])).
fof(f2034, plain, ($false | (~ spl8_67 | ~ spl8_69)), inference(subsumption_resolution, [], [f2033, f232])).
fof(f232, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f2033, plain, ((e1 = e3) | (~ spl8_67 | ~ spl8_69)), inference(forward_demodulation, [], [f579, f571])).
fof(f571, plain, ((e1 = op(e2, e1)) | ~ spl8_67), inference(avatar_component_clause, [], [f569])).
fof(f579, plain, ((e3 = op(e2, e1)) | ~ spl8_69), inference(avatar_component_clause, [], [f577])).
fof(f577, plain, (spl8_69 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_69])])).
fof(f1983, plain, (~ spl8_111 | ~ spl8_112), inference(avatar_contradiction_clause, [], [f1982])).
fof(f1982, plain, ($false | (~ spl8_111 | ~ spl8_112)), inference(subsumption_resolution, [], [f1981, f227])).
fof(f227, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f1981, plain, ((e0 = e1) | (~ spl8_111 | ~ spl8_112)), inference(forward_demodulation, [], [f760, f756])).
fof(f756, plain, ((e0 = op(e0, e2)) | ~ spl8_111), inference(avatar_component_clause, [], [f754])).
fof(f754, plain, (spl8_111 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_111])])).
fof(f760, plain, ((e1 = op(e0, e2)) | ~ spl8_112), inference(avatar_component_clause, [], [f758])).
fof(f1978, plain, (~ spl8_121 | ~ spl8_124), inference(avatar_contradiction_clause, [], [f1977])).
fof(f1977, plain, ($false | (~ spl8_121 | ~ spl8_124)), inference(subsumption_resolution, [], [f1976, f229])).
fof(f229, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f1976, plain, ((e0 = e3) | (~ spl8_121 | ~ spl8_124)), inference(forward_demodulation, [], [f810, f798])).
fof(f798, plain, ((e0 = op(e0, e0)) | ~ spl8_121), inference(avatar_component_clause, [], [f796])).
fof(f796, plain, (spl8_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_121])])).
fof(f810, plain, ((op(e0, e0) = e3) | ~ spl8_124), inference(avatar_component_clause, [], [f808])).
fof(f808, plain, (spl8_124 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl8_124])])).
fof(f1969, plain, (~ spl8_102 | ~ spl8_2), inference(avatar_split_clause, [], [f1775, f296, f716])).
fof(f716, plain, (spl8_102 <=> (e1 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_102])])).
fof(f1775, plain, (~ (e1 = op(e0, e4)) | ~ spl8_2), inference(forward_demodulation, [], [f170, f298])).
fof(f170, plain, ~ (op(e0, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1942, plain, (~ spl8_2 | ~ spl8_65), inference(avatar_contradiction_clause, [], [f1941])).
fof(f1941, plain, ($false | (~ spl8_2 | ~ spl8_65)), inference(subsumption_resolution, [], [f1940, f231])).
fof(f231, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f1940, plain, ((e1 = e2) | (~ spl8_2 | ~ spl8_65)), inference(forward_demodulation, [], [f1939, f298])).
fof(f1939, plain, ((e2 = op(e4, e4)) | ~ spl8_65), inference(forward_demodulation, [], [f114, f562])).
fof(f562, plain, ((e4 = op(e2, e2)) | ~ spl8_65), inference(avatar_component_clause, [], [f560])).
fof(f560, plain, (spl8_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_65])])).
fof(f1915, plain, (~ spl8_43 | ~ spl8_118), inference(avatar_split_clause, [], [f1914, f783, f468])).
fof(f468, plain, (spl8_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_43])])).
fof(f1914, plain, (~ (e2 = op(e3, e1)) | ~ spl8_118), inference(forward_demodulation, [], [f139, f785])).
fof(f139, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f1913, plain, (~ spl8_45 | ~ spl8_95), inference(avatar_split_clause, [], [f1912, f686, f476])).
fof(f476, plain, (spl8_45 <=> (e4 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_45])])).
fof(f1912, plain, (~ (e4 = op(e3, e1)) | ~ spl8_95), inference(forward_demodulation, [], [f142, f688])).
fof(f142, plain, ~ (op(e1, e1) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f1815, plain, (spl8_61 | ~ spl8_98 | ~ spl8_118), inference(avatar_contradiction_clause, [], [f1814])).
fof(f1814, plain, ($false | (spl8_61 | ~ spl8_98 | ~ spl8_118)), inference(subsumption_resolution, [], [f1810, f545])).
fof(f545, plain, (~ (e0 = op(e2, e2)) | spl8_61), inference(avatar_component_clause, [], [f544])).
fof(f544, plain, (spl8_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_61])])).
fof(f1810, plain, ((e0 = op(e2, e2)) | (~ spl8_98 | ~ spl8_118)), inference(backward_demodulation, [], [f1765, f701])).
fof(f701, plain, ((e2 = op(e1, e0)) | ~ spl8_98), inference(avatar_component_clause, [], [f699])).
fof(f699, plain, (spl8_98 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_98])])).
fof(f1794, plain, (~ spl8_123 | ~ spl8_124), inference(avatar_contradiction_clause, [], [f1793])).
fof(f1793, plain, ($false | (~ spl8_123 | ~ spl8_124)), inference(subsumption_resolution, [], [f1792, f234])).
fof(f1792, plain, ((e2 = e3) | (~ spl8_123 | ~ spl8_124)), inference(backward_demodulation, [], [f810, f806])).
fof(f806, plain, ((op(e0, e0) = e2) | ~ spl8_123), inference(avatar_component_clause, [], [f804])).
fof(f804, plain, (spl8_123 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl8_123])])).
fof(f1774, plain, (~ spl8_103 | ~ spl8_118), inference(avatar_split_clause, [], [f1773, f783, f720])).
fof(f720, plain, (spl8_103 <=> (e2 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_103])])).
fof(f1773, plain, (~ (e2 = op(e0, e4)) | ~ spl8_118), inference(forward_demodulation, [], [f183, f785])).
fof(f183, plain, ~ (op(e0, e1) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f1770, plain, (~ spl8_100 | ~ spl8_95), inference(avatar_split_clause, [], [f1769, f686, f707])).
fof(f707, plain, (spl8_100 <=> (e4 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_100])])).
fof(f1769, plain, (~ (e4 = op(e1, e0)) | ~ spl8_95), inference(forward_demodulation, [], [f187, f688])).
fof(f187, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f1763, plain, (~ spl8_70 | ~ spl8_95), inference(avatar_split_clause, [], [f1762, f686, f581])).
fof(f581, plain, (spl8_70 <=> (e4 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_70])])).
fof(f1762, plain, (~ (e4 = op(e2, e1)) | ~ spl8_95), inference(forward_demodulation, [], [f141, f688])).
fof(f141, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f1757, plain, (~ spl8_80 | ~ spl8_95), inference(avatar_split_clause, [], [f1756, f686, f623])).
fof(f623, plain, (spl8_80 <=> (e4 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_80])])).
fof(f1756, plain, (~ (e4 = op(e1, e4)) | ~ spl8_95), inference(backward_demodulation, [], [f193, f688])).
fof(f193, plain, ~ (op(e1, e1) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f1706, plain, (~ spl8_16 | ~ spl8_19), inference(avatar_contradiction_clause, [], [f1705])).
fof(f1705, plain, ($false | (~ spl8_16 | ~ spl8_19)), inference(subsumption_resolution, [], [f1704, f229])).
fof(f1704, plain, ((e0 = e3) | (~ spl8_16 | ~ spl8_19)), inference(forward_demodulation, [], [f369, f357])).
fof(f369, plain, ((e3 = op(e4, e1)) | ~ spl8_19), inference(avatar_component_clause, [], [f367])).
fof(f367, plain, (spl8_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_19])])).
fof(f1687, plain, (~ spl8_59 | ~ spl8_60), inference(avatar_contradiction_clause, [], [f1686])).
fof(f1686, plain, ($false | (~ spl8_59 | ~ spl8_60)), inference(subsumption_resolution, [], [f1685, f236])).
fof(f1685, plain, ((e3 = e4) | (~ spl8_59 | ~ spl8_60)), inference(backward_demodulation, [], [f541, f537])).
fof(f537, plain, ((e3 = op(e2, e3)) | ~ spl8_59), inference(avatar_component_clause, [], [f535])).
fof(f535, plain, (spl8_59 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_59])])).
fof(f541, plain, ((e4 = op(e2, e3)) | ~ spl8_60), inference(avatar_component_clause, [], [f539])).
fof(f539, plain, (spl8_60 <=> (e4 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_60])])).
fof(f1678, plain, (~ spl8_75 | spl8_103 | ~ spl8_111), inference(avatar_contradiction_clause, [], [f1677])).
fof(f1677, plain, ($false | (~ spl8_75 | spl8_103 | ~ spl8_111)), inference(subsumption_resolution, [], [f1672, f721])).
fof(f721, plain, (~ (e2 = op(e0, e4)) | spl8_103), inference(avatar_component_clause, [], [f720])).
fof(f1672, plain, ((e2 = op(e0, e4)) | (~ spl8_75 | ~ spl8_111)), inference(backward_demodulation, [], [f987, f604])).
fof(f604, plain, ((e4 = op(e2, e0)) | ~ spl8_75), inference(avatar_component_clause, [], [f602])).
fof(f602, plain, (spl8_75 <=> (e4 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_75])])).
fof(f987, plain, ((e2 = op(e0, op(e2, e0))) | ~ spl8_111), inference(backward_demodulation, [], [f112, f756])).
fof(f112, plain, (e2 = op(op(e0, e2), op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f1644, plain, (~ spl8_88 | ~ spl8_63), inference(avatar_split_clause, [], [f1643, f552, f657])).
fof(f657, plain, (spl8_88 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_88])])).
fof(f1643, plain, (~ (e2 = op(e1, e2)) | ~ spl8_63), inference(forward_demodulation, [], [f151, f554])).
fof(f554, plain, ((e2 = op(e2, e2)) | ~ spl8_63), inference(avatar_component_clause, [], [f552])).
fof(f151, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f1636, plain, (spl8_59 | ~ spl8_44 | ~ spl8_83), inference(avatar_split_clause, [], [f1635, f636, f472, f535])).
fof(f472, plain, (spl8_44 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_44])])).
fof(f1635, plain, ((e3 = op(e2, e3)) | (~ spl8_44 | ~ spl8_83)), inference(forward_demodulation, [], [f1634, f638])).
fof(f1634, plain, ((e3 = op(op(e1, e3), e3)) | ~ spl8_44), inference(forward_demodulation, [], [f118, f474])).
fof(f474, plain, ((e3 = op(e3, e1)) | ~ spl8_44), inference(avatar_component_clause, [], [f472])).
fof(f118, plain, (e3 = op(op(e1, e3), op(e3, e1))), inference(cnf_transformation, [], [f3])).
fof(f1603, plain, (~ spl8_56 | ~ spl8_31), inference(avatar_split_clause, [], [f1602, f418, f523])).
fof(f523, plain, (spl8_56 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_56])])).
fof(f1602, plain, (~ (e0 = op(e2, e3)) | ~ spl8_31), inference(forward_demodulation, [], [f164, f420])).
fof(f164, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1577, plain, (~ spl8_41 | ~ spl8_31), inference(avatar_split_clause, [], [f1576, f418, f460])).
fof(f460, plain, (spl8_41 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_41])])).
fof(f1576, plain, (~ (e0 = op(e3, e1)) | ~ spl8_31), inference(forward_demodulation, [], [f212, f420])).
fof(f212, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1505, plain, (~ spl8_31 | ~ spl8_34), inference(avatar_contradiction_clause, [], [f1504])).
fof(f1504, plain, ($false | (~ spl8_31 | ~ spl8_34)), inference(subsumption_resolution, [], [f1503, f229])).
fof(f1503, plain, ((e0 = e3) | (~ spl8_31 | ~ spl8_34)), inference(backward_demodulation, [], [f432, f420])).
fof(f432, plain, ((e3 = op(e3, e3)) | ~ spl8_34), inference(avatar_component_clause, [], [f430])).
fof(f430, plain, (spl8_34 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_34])])).
fof(f1403, plain, (spl8_118 | ~ spl8_2), inference(avatar_split_clause, [], [f1275, f296, f783])).
fof(f1275, plain, ((e2 = op(e0, e1)) | ~ spl8_2), inference(backward_demodulation, [], [f867, f298])).
fof(f867, plain, (e2 = op(e0, op(e4, e4))), inference(backward_demodulation, [], [f239, f237])).
fof(f237, plain, (e0 = op(e4, op(e4, e4))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e3 = op(op(e4, op(e4, e4)), op(e4, op(e4, e4)))) & (e2 = op(op(e4, op(e4, e4)), op(e4, e4))) & (e1 = op(e4, e4)) & (e0 = op(e4, op(e4, e4)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG170+1.p', ax6)).
fof(f239, plain, (e2 = op(op(e4, op(e4, e4)), op(e4, e4))), inference(cnf_transformation, [], [f6])).
fof(f1379, plain, (spl8_95 | ~ spl8_2), inference(avatar_split_clause, [], [f1272, f296, f686])).
fof(f1272, plain, ((e4 = op(e1, e1)) | ~ spl8_2), inference(backward_demodulation, [], [f126, f298])).
fof(f126, plain, (e4 = op(op(e4, e4), op(e4, e4))), inference(cnf_transformation, [], [f3])).
fof(f1366, plain, (~ spl8_54 | ~ spl8_79), inference(avatar_contradiction_clause, [], [f1365])).
fof(f1365, plain, ($false | (~ spl8_54 | ~ spl8_79)), inference(subsumption_resolution, [], [f1364, f621])).
fof(f621, plain, ((e3 = op(e1, e4)) | ~ spl8_79), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl8_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_79])])).
fof(f1364, plain, (~ (e3 = op(e1, e4)) | ~ spl8_54), inference(forward_demodulation, [], [f171, f516])).
fof(f516, plain, ((e3 = op(e2, e4)) | ~ spl8_54), inference(avatar_component_clause, [], [f514])).
fof(f514, plain, (spl8_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_54])])).
fof(f171, plain, ~ (op(e1, e4) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f1330, plain, (~ spl8_52 | ~ spl8_2), inference(avatar_split_clause, [], [f1329, f296, f506])).
fof(f506, plain, (spl8_52 <=> (e1 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_52])])).
fof(f1329, plain, (~ (e1 = op(e2, e4)) | ~ spl8_2), inference(forward_demodulation, [], [f175, f298])).
fof(f175, plain, ~ (op(e2, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1282, plain, (~ spl8_2 | ~ spl8_116), inference(avatar_contradiction_clause, [], [f1281])).
fof(f1281, plain, ($false | (~ spl8_2 | ~ spl8_116)), inference(subsumption_resolution, [], [f1280, f228])).
fof(f1280, plain, ((e0 = e2) | (~ spl8_2 | ~ spl8_116)), inference(forward_demodulation, [], [f1275, f777])).
fof(f777, plain, ((e0 = op(e0, e1)) | ~ spl8_116), inference(avatar_component_clause, [], [f775])).
fof(f775, plain, (spl8_116 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_116])])).
fof(f1279, plain, (spl8_16 | ~ spl8_2), inference(avatar_split_clause, [], [f1273, f296, f355])).
fof(f1273, plain, ((e0 = op(e4, e1)) | ~ spl8_2), inference(backward_demodulation, [], [f237, f298])).
fof(f1254, plain, (spl8_5 | ~ spl8_15 | ~ spl8_55), inference(avatar_split_clause, [], [f1250, f518, f350, f308])).
fof(f350, plain, (spl8_15 <=> (e4 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl8_15])])).
fof(f518, plain, (spl8_55 <=> (e4 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_55])])).
fof(f1250, plain, ((e4 = op(e4, e4)) | (~ spl8_15 | ~ spl8_55)), inference(backward_demodulation, [], [f1145, f352])).
fof(f352, plain, ((e4 = op(e4, e2)) | ~ spl8_15), inference(avatar_component_clause, [], [f350])).
fof(f1145, plain, ((e4 = op(e4, op(e4, e2))) | ~ spl8_55), inference(backward_demodulation, [], [f124, f520])).
fof(f520, plain, ((e4 = op(e2, e4)) | ~ spl8_55), inference(avatar_component_clause, [], [f518])).
fof(f124, plain, (e4 = op(op(e2, e4), op(e4, e2))), inference(cnf_transformation, [], [f3])).
fof(f1131, plain, (spl8_123 | ~ spl8_61), inference(avatar_split_clause, [], [f1126, f544, f804])).
fof(f1126, plain, ((op(e0, e0) = e2) | ~ spl8_61), inference(backward_demodulation, [], [f114, f546])).
fof(f546, plain, ((e0 = op(e2, e2)) | ~ spl8_61), inference(avatar_component_clause, [], [f544])).
fof(f1120, plain, (~ spl8_16 | ~ spl8_66), inference(avatar_split_clause, [], [f1113, f565, f355])).
fof(f565, plain, (spl8_66 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_66])])).
fof(f1113, plain, (~ (e0 = op(e4, e1)) | ~ spl8_66), inference(backward_demodulation, [], [f145, f567])).
fof(f567, plain, ((e0 = op(e2, e1)) | ~ spl8_66), inference(avatar_component_clause, [], [f565])).
fof(f145, plain, ~ (op(e2, e1) = op(e4, e1)), inference(cnf_transformation, [], [f4])).
fof(f1037, plain, (~ spl8_71 | ~ spl8_96), inference(avatar_split_clause, [], [f1028, f691, f586])).
fof(f691, plain, (spl8_96 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_96])])).
fof(f1028, plain, (~ (e0 = op(e2, e0)) | ~ spl8_96), inference(backward_demodulation, [], [f131, f693])).
fof(f693, plain, ((e0 = op(e1, e0)) | ~ spl8_96), inference(avatar_component_clause, [], [f691])).
fof(f131, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f1019, plain, (~ spl8_55 | ~ spl8_105), inference(avatar_split_clause, [], [f1015, f728, f518])).
fof(f728, plain, (spl8_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_105])])).
fof(f1015, plain, (~ (e4 = op(e2, e4)) | ~ spl8_105), inference(backward_demodulation, [], [f168, f730])).
fof(f730, plain, ((e4 = op(e0, e4)) | ~ spl8_105), inference(avatar_component_clause, [], [f728])).
fof(f1009, plain, (~ spl8_31 | ~ spl8_106), inference(avatar_split_clause, [], [f1004, f733, f418])).
fof(f733, plain, (spl8_106 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_106])])).
fof(f1004, plain, (~ (e0 = op(e3, e3)) | ~ spl8_106), inference(backward_demodulation, [], [f159, f735])).
fof(f735, plain, ((e0 = op(e0, e3)) | ~ spl8_106), inference(avatar_component_clause, [], [f733])).
fof(f159, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f994, plain, (~ spl8_86 | ~ spl8_111), inference(avatar_split_clause, [], [f988, f754, f649])).
fof(f988, plain, (~ (e0 = op(e1, e2)) | ~ spl8_111), inference(backward_demodulation, [], [f147, f756])).
fof(f147, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f969, plain, (~ spl8_104 | ~ spl8_124), inference(avatar_split_clause, [], [f960, f808, f724])).
fof(f724, plain, (spl8_104 <=> (e3 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl8_104])])).
fof(f960, plain, (~ (e3 = op(e0, e4)) | ~ spl8_124), inference(backward_demodulation, [], [f180, f810])).
fof(f180, plain, ~ (op(e0, e0) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f968, plain, (~ spl8_109 | ~ spl8_124), inference(avatar_split_clause, [], [f959, f808, f745])).
fof(f745, plain, (spl8_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_109])])).
fof(f959, plain, (~ (e3 = op(e0, e3)) | ~ spl8_124), inference(backward_demodulation, [], [f179, f810])).
fof(f179, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f966, plain, (~ spl8_119 | ~ spl8_124), inference(avatar_split_clause, [], [f957, f808, f787])).
fof(f787, plain, (spl8_119 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_119])])).
fof(f957, plain, (~ (e3 = op(e0, e1)) | ~ spl8_124), inference(backward_demodulation, [], [f177, f810])).
fof(f177, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f4])).
fof(f962, plain, (~ spl8_99 | ~ spl8_124), inference(avatar_split_clause, [], [f953, f808, f703])).
fof(f703, plain, (spl8_99 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl8_99])])).
fof(f953, plain, (~ (e3 = op(e1, e0)) | ~ spl8_124), inference(backward_demodulation, [], [f127, f810])).
fof(f127, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f4])).
fof(f961, plain, (spl8_31 | ~ spl8_124), inference(avatar_split_clause, [], [f952, f808, f418])).
fof(f952, plain, ((e0 = op(e3, e3)) | ~ spl8_124), inference(backward_demodulation, [], [f102, f810])).
fof(f102, plain, (e0 = op(op(e0, e0), op(e0, e0))), inference(cnf_transformation, [], [f3])).
fof(f951, plain, (spl8_133 | spl8_132 | spl8_131 | spl8_130 | ~ spl8_101), inference(avatar_split_clause, [], [f281, f712, f907, f916, f925, f934])).
fof(f934, plain, (spl8_133 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl8_133])])).
fof(f925, plain, (spl8_132 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl8_132])])).
fof(f916, plain, (spl8_131 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl8_131])])).
fof(f907, plain, (spl8_130 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl8_130])])).
fof(f281, plain, (~ (e0 = op(e0, e4)) | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f18])).
fof(f18, plain, (((~ (e4 = op(e4, e4)) & ~ (e3 = op(e4, e3)) & ~ (e2 = op(e4, e2)) & ~ (e1 = op(e4, e1)) & ~ (e0 = op(e4, e0))) | sP7 | sP6 | sP5 | sP4) & ((~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e4)) & ~ (e2 = op(e2, e4)) & ~ (e1 = op(e1, e4)) & ~ (e0 = op(e0, e4))) | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f9, e17, e16, e15, e14, e13, e12, e11, e10])).
fof(f10, plain, ((~ (e4 = op(e4, e0)) & ~ (e3 = op(e3, e0)) & ~ (e2 = op(e2, e0)) & ~ (e1 = op(e1, e0)) & ~ (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> (~ (e4 = op(e4, e0)) & ~ (e3 = op(e3, e0)) & ~ (e2 = op(e2, e0)) & ~ (e1 = op(e1, e0)) & ~ (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, ((~ (e4 = op(e4, e1)) & ~ (e3 = op(e3, e1)) & ~ (e2 = op(e2, e1)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e1))) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> (~ (e4 = op(e4, e1)) & ~ (e3 = op(e3, e1)) & ~ (e2 = op(e2, e1)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, ((~ (e4 = op(e4, e2)) & ~ (e3 = op(e3, e2)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & ~ (e0 = op(e0, e2))) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> (~ (e4 = op(e4, e2)) & ~ (e3 = op(e3, e2)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & ~ (e0 = op(e0, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f13, plain, ((~ (e4 = op(e4, e3)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & ~ (e1 = op(e1, e3)) & ~ (e0 = op(e0, e3))) | ~ sP3), inference(usedef, [], [e13])).
fof(e13, plain, (sP3 <=> (~ (e4 = op(e4, e3)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & ~ (e1 = op(e1, e3)) & ~ (e0 = op(e0, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f14, plain, ((~ (e4 = op(e0, e4)) & ~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0))) | ~ sP4), inference(usedef, [], [e14])).
fof(e14, plain, (sP4 <=> (~ (e4 = op(e0, e4)) & ~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f15, plain, ((~ (e4 = op(e1, e4)) & ~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0))) | ~ sP5), inference(usedef, [], [e15])).
fof(e15, plain, (sP5 <=> (~ (e4 = op(e1, e4)) & ~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f16, plain, ((~ (e4 = op(e2, e4)) & ~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0))) | ~ sP6), inference(usedef, [], [e16])).
fof(e16, plain, (sP6 <=> (~ (e4 = op(e2, e4)) & ~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f17, plain, ((~ (e4 = op(e3, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0))) | ~ sP7), inference(usedef, [], [e17])).
fof(e17, plain, (sP7 <=> (~ (e4 = op(e3, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f9, plain, (((~ (e4 = op(e4, e4)) & ~ (e3 = op(e4, e3)) & ~ (e2 = op(e4, e2)) & ~ (e1 = op(e4, e1)) & ~ (e0 = op(e4, e0))) | (~ (e4 = op(e3, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0))) | (~ (e4 = op(e2, e4)) & ~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0))) | (~ (e4 = op(e1, e4)) & ~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0))) | (~ (e4 = op(e0, e4)) & ~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)))) & ((~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e4)) & ~ (e2 = op(e2, e4)) & ~ (e1 = op(e1, e4)) & ~ (e0 = op(e0, e4))) | (~ (e4 = op(e4, e3)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & ~ (e1 = op(e1, e3)) & ~ (e0 = op(e0, e3))) | (~ (e4 = op(e4, e2)) & ~ (e3 = op(e3, e2)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & ~ (e0 = op(e0, e2))) | (~ (e4 = op(e4, e1)) & ~ (e3 = op(e3, e1)) & ~ (e2 = op(e2, e1)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e1))) | (~ (e4 = op(e4, e0)) & ~ (e3 = op(e3, e0)) & ~ (e2 = op(e2, e0)) & ~ (e1 = op(e1, e0)) & ~ (e0 = op(e0, e0))))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(e4, e4)) & ~ (e3 = op(e4, e3)) & ~ (e2 = op(e4, e2)) & ~ (e1 = op(e4, e1)) & ~ (e0 = op(e4, e0))) | (~ (e4 = op(e3, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0))) | (~ (e4 = op(e2, e4)) & ~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0))) | (~ (e4 = op(e1, e4)) & ~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0))) | (~ (e4 = op(e0, e4)) & ~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)))) & ((~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e4)) & ~ (e2 = op(e2, e4)) & ~ (e1 = op(e1, e4)) & ~ (e0 = op(e0, e4))) | (~ (e4 = op(e4, e3)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & ~ (e1 = op(e1, e3)) & ~ (e0 = op(e0, e3))) | (~ (e4 = op(e4, e2)) & ~ (e3 = op(e3, e2)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & ~ (e0 = op(e0, e2))) | (~ (e4 = op(e4, e1)) & ~ (e3 = op(e3, e1)) & ~ (e2 = op(e2, e1)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e1))) | (~ (e4 = op(e4, e0)) & ~ (e3 = op(e3, e0)) & ~ (e2 = op(e2, e0)) & ~ (e1 = op(e1, e0)) & ~ (e0 = op(e0, e0))))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(e4, e4)) & ~ (e3 = op(e4, e3)) & ~ (e2 = op(e4, e2)) & ~ (e1 = op(e4, e1)) & ~ (e0 = op(e4, e0))) | (~ (e4 = op(e3, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0))) | (~ (e4 = op(e2, e4)) & ~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0))) | (~ (e4 = op(e1, e4)) & ~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0))) | (~ (e4 = op(e0, e4)) & ~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0)))) & ((~ (e4 = op(e4, e4)) & ~ (e3 = op(e3, e4)) & ~ (e2 = op(e2, e4)) & ~ (e1 = op(e1, e4)) & ~ (e0 = op(e0, e4))) | (~ (e4 = op(e4, e3)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & ~ (e1 = op(e1, e3)) & ~ (e0 = op(e0, e3))) | (~ (e4 = op(e4, e2)) & ~ (e3 = op(e3, e2)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & ~ (e0 = op(e0, e2))) | (~ (e4 = op(e4, e1)) & ~ (e3 = op(e3, e1)) & ~ (e2 = op(e2, e1)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e1))) | (~ (e4 = op(e4, e0)) & ~ (e3 = op(e3, e0)) & ~ (e2 = op(e2, e0)) & ~ (e1 = op(e1, e0)) & ~ (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG170+1.p', co1)).
fof(f943, plain, (spl8_129 | spl8_128 | spl8_127 | spl8_126 | ~ spl8_9), inference(avatar_split_clause, [], [f289, f325, f871, f880, f889, f898])).
fof(f898, plain, (spl8_129 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl8_129])])).
fof(f889, plain, (spl8_128 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl8_128])])).
fof(f880, plain, (spl8_127 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl8_127])])).
fof(f871, plain, (spl8_126 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl8_126])])).
fof(f325, plain, (spl8_9 <=> (e3 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_9])])).
fof(f289, plain, (~ (e3 = op(e4, e3)) | sP7 | sP6 | sP5 | sP4), inference(cnf_transformation, [], [f18])).
fof(f940, plain, (~ spl8_133 | ~ spl8_97), inference(avatar_split_clause, [], [f277, f695, f934])).
fof(f277, plain, (~ (e1 = op(e1, e0)) | ~ sP0), inference(cnf_transformation, [], [f26])).
fof(f26, plain, ((~ (e4 = op(e4, e0)) & ~ (e3 = op(e3, e0)) & ~ (e2 = op(e2, e0)) & ~ (e1 = op(e1, e0)) & ~ (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f929, plain, (~ spl8_132 | ~ spl8_44), inference(avatar_split_clause, [], [f274, f472, f925])).
fof(f274, plain, (~ (e3 = op(e3, e1)) | ~ sP1), inference(cnf_transformation, [], [f25])).
fof(f25, plain, ((~ (e4 = op(e4, e1)) & ~ (e3 = op(e3, e1)) & ~ (e2 = op(e2, e1)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e0, e1))) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f921, plain, (~ spl8_131 | ~ spl8_63), inference(avatar_split_clause, [], [f268, f552, f916])).
fof(f268, plain, (~ (e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f24])).
fof(f24, plain, ((~ (e4 = op(e4, e2)) & ~ (e3 = op(e3, e2)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e1, e2)) & ~ (e0 = op(e0, e2))) | ~ sP2), inference(nnf_transformation, [], [f12])).
fof(f910, plain, (~ spl8_130 | ~ spl8_10), inference(avatar_split_clause, [], [f265, f329, f907])).
fof(f329, plain, (spl8_10 <=> (e4 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl8_10])])).
fof(f265, plain, (~ (e4 = op(e4, e3)) | ~ sP3), inference(cnf_transformation, [], [f23])).
fof(f23, plain, ((~ (e4 = op(e4, e3)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e2, e3)) & ~ (e1 = op(e1, e3)) & ~ (e0 = op(e0, e3))) | ~ sP3), inference(nnf_transformation, [], [f13])).
fof(f901, plain, (~ spl8_129 | ~ spl8_105), inference(avatar_split_clause, [], [f260, f728, f898])).
fof(f260, plain, (~ (e4 = op(e0, e4)) | ~ sP4), inference(cnf_transformation, [], [f22])).
fof(f22, plain, ((~ (e4 = op(e0, e4)) & ~ (e3 = op(e0, e3)) & ~ (e2 = op(e0, e2)) & ~ (e1 = op(e0, e1)) & ~ (e0 = op(e0, e0))) | ~ sP4), inference(nnf_transformation, [], [f14])).
fof(f896, plain, (~ spl8_128 | ~ spl8_96), inference(avatar_split_clause, [], [f251, f691, f889])).
fof(f251, plain, (~ (e0 = op(e1, e0)) | ~ sP5), inference(cnf_transformation, [], [f21])).
fof(f21, plain, ((~ (e4 = op(e1, e4)) & ~ (e3 = op(e1, e3)) & ~ (e2 = op(e1, e2)) & ~ (e1 = op(e1, e1)) & ~ (e0 = op(e1, e0))) | ~ sP5), inference(nnf_transformation, [], [f15])).
fof(f885, plain, (~ spl8_127 | ~ spl8_63), inference(avatar_split_clause, [], [f248, f552, f880])).
fof(f248, plain, (~ (e2 = op(e2, e2)) | ~ sP6), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((~ (e4 = op(e2, e4)) & ~ (e3 = op(e2, e3)) & ~ (e2 = op(e2, e2)) & ~ (e1 = op(e2, e1)) & ~ (e0 = op(e2, e0))) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f877, plain, (~ spl8_126 | ~ spl8_42), inference(avatar_split_clause, [], [f242, f464, f871])).
fof(f464, plain, (spl8_42 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl8_42])])).
fof(f242, plain, (~ (e1 = op(e3, e1)) | ~ sP7), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((~ (e4 = op(e3, e4)) & ~ (e3 = op(e3, e3)) & ~ (e2 = op(e3, e2)) & ~ (e1 = op(e3, e1)) & ~ (e0 = op(e3, e0))) | ~ sP7), inference(nnf_transformation, [], [f17])).
fof(f869, plain, spl8_124, inference(avatar_split_clause, [], [f868, f808])).
fof(f868, plain, (op(e0, e0) = e3), inference(backward_demodulation, [], [f240, f237])).
fof(f240, plain, (e3 = op(op(e4, op(e4, e4)), op(e4, op(e4, e4)))), inference(cnf_transformation, [], [f6])).
fof(f866, plain, spl8_2, inference(avatar_split_clause, [], [f238, f296])).
fof(f238, plain, (e1 = op(e4, e4)), inference(cnf_transformation, [], [f6])).
fof(f865, plain, (spl8_121 | spl8_116 | spl8_111 | spl8_106 | spl8_101), inference(avatar_split_clause, [], [f52, f712, f733, f754, f775, f796])).
fof(f52, plain, ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG170+1.p', ax2)).
fof(f851, plain, (spl8_98 | spl8_93 | spl8_88 | spl8_83 | spl8_78), inference(avatar_split_clause, [], [f66, f615, f636, f657, f678, f699])).
fof(f66, plain, ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))), inference(cnf_transformation, [], [f2])).
fof(f848, plain, (spl8_119 | spl8_94 | spl8_69 | spl8_44 | spl8_19), inference(avatar_split_clause, [], [f69, f367, f472, f577, f682, f787])).
fof(f69, plain, ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f2])).
fof(f845, plain, (spl8_71 | spl8_66 | spl8_61 | spl8_56 | spl8_51), inference(avatar_split_clause, [], [f72, f502, f523, f544, f565, f586])).
fof(f72, plain, ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f837, plain, (spl8_75 | spl8_70 | spl8_65 | spl8_60 | spl8_55), inference(avatar_split_clause, [], [f80, f518, f539, f560, f581, f602])).
fof(f80, plain, ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))), inference(cnf_transformation, [], [f2])).
fof(f828, plain, (spl8_109 | spl8_84 | spl8_59 | spl8_34 | spl8_9), inference(avatar_split_clause, [], [f89, f325, f430, f535, f640, f745])).
fof(f89, plain, ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))), inference(cnf_transformation, [], [f2])).
fof(f817, plain, (spl8_25 | spl8_20 | spl8_15 | spl8_10 | spl8_5), inference(avatar_split_clause, [], [f100, f308, f329, f350, f371, f392])).
fof(f100, plain, ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))), inference(cnf_transformation, [], [f2])).
fof(f731, plain, (spl8_101 | spl8_102 | spl8_103 | spl8_104 | spl8_105), inference(avatar_split_clause, [], [f31, f728, f724, f720, f716, f712])).
fof(f31, plain, ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG170+1.p', ax1)).
fof(f710, plain, (spl8_96 | spl8_97 | spl8_98 | spl8_99 | spl8_100), inference(avatar_split_clause, [], [f32, f707, f703, f699, f695, f691])).
fof(f32, plain, ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))), inference(cnf_transformation, [], [f1])).
fof(f626, plain, (spl8_76 | spl8_77 | spl8_78 | spl8_79 | spl8_80), inference(avatar_split_clause, [], [f36, f623, f619, f615, f611, f607])).
fof(f36, plain, ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))), inference(cnf_transformation, [], [f1])).
fof(f563, plain, (spl8_61 | spl8_62 | spl8_63 | spl8_64 | spl8_65), inference(avatar_split_clause, [], [f39, f560, f556, f552, f548, f544])).
fof(f39, plain, ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))), inference(cnf_transformation, [], [f1])).
fof(f521, plain, (spl8_51 | spl8_52 | spl8_53 | spl8_54 | spl8_55), inference(avatar_split_clause, [], [f41, f518, f514, f510, f506, f502])).
fof(f41, plain, ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))), inference(cnf_transformation, [], [f1])).
fof(f479, plain, (spl8_41 | spl8_42 | spl8_43 | spl8_44 | spl8_45), inference(avatar_split_clause, [], [f43, f476, f472, f468, f464, f460])).
fof(f43, plain, ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))), inference(cnf_transformation, [], [f1])).