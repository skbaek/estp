fof(f3342, plain, $false, inference(avatar_sat_refutation, [], [f670, f674, f678, f679, f683, f684, f686, f787, f821, f991, f995, f996, f997, f999, f1001, f1004, f1032, f1034, f1035, f1037, f1212, f1230, f1251, f1630, f1776, f1780, f1784, f1814, f1834, f1836, f1841, f1844, f1847, f1865, f1890, f1914, f1927, f1928, f1930, f1944, f1998, f2000, f2004, f2005, f2010, f2013, f2042, f2051, f2122, f2125, f2129, f2131, f2144, f2153, f2163, f2180, f2208, f2210, f2224, f2227, f2228, f2235, f2254, f2259, f2290, f2311, f2324, f2336, f2349, f2370, f2387, f2412, f2426, f2525, f2538, f2547, f2578, f2581, f2633, f2644, f2676, f2724, f2748, f2780, f2781, f2782, f2783, f2787, f2817, f2826, f2857, f2858, f2859, f2860, f2891, f2919, f2942, f2965, f3001, f3039, f3074, f3109, f3145, f3177, f3208, f3239, f3271, f3304, f3332])).
fof(f3332, plain, (~ spl12_58 | ~ spl12_126 | ~ spl12_169 | spl12_236), inference(avatar_contradiction_clause, [], [f3331])).
fof(f3331, plain, ($false | (~ spl12_58 | ~ spl12_126 | ~ spl12_169 | spl12_236)), inference(subsumption_resolution, [], [f3330, f948])).
fof(f948, plain, ((e21 = op2(e20, e21)) | ~ spl12_126), inference(avatar_component_clause, [], [f946])).
fof(f946, plain, (spl12_126 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_126])])).
fof(f3330, plain, (~ (e21 = op2(e20, e21)) | (~ spl12_58 | ~ spl12_169 | spl12_236)), inference(forward_demodulation, [], [f3329, f306])).
fof(f306, plain, (e21 = h2(e11)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e21, e21) = h2(e13)) & (op2(e21, op2(e21, e21)) = h2(e12)) & (op2(e21, op2(e21, op2(e21, e21))) = h2(e10)) & (e21 = h2(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax17)).
fof(f3329, plain, (~ (op2(e20, e21) = h2(e11)) | (~ spl12_58 | ~ spl12_169 | spl12_236)), inference(forward_demodulation, [], [f3328, f627])).
fof(f627, plain, ((e11 = op1(e10, e11)) | ~ spl12_58), inference(avatar_component_clause, [], [f625])).
fof(f625, plain, (spl12_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl12_58])])).
fof(f3328, plain, (~ (op2(e20, e21) = h2(op1(e10, e11))) | (~ spl12_169 | spl12_236)), inference(forward_demodulation, [], [f1629, f1249])).
fof(f1249, plain, ((e20 = h2(e10)) | ~ spl12_169), inference(avatar_component_clause, [], [f1248])).
fof(f1248, plain, (spl12_169 <=> (e20 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_169])])).
fof(f1629, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | spl12_236), inference(avatar_component_clause, [], [f1627])).
fof(f1627, plain, (spl12_236 <=> (h2(op1(e10, e11)) = op2(h2(e10), e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_236])])).
fof(f3304, plain, (~ spl12_44 | spl12_233), inference(avatar_contradiction_clause, [], [f3303])).
fof(f3303, plain, ($false | (~ spl12_44 | spl12_233)), inference(trivial_inequality_removal, [], [f3302])).
fof(f3302, plain, (~ (h2(e13) = h2(e13)) | (~ spl12_44 | spl12_233)), inference(forward_demodulation, [], [f1617, f567])).
fof(f567, plain, ((e13 = op1(e11, e11)) | ~ spl12_44), inference(avatar_component_clause, [], [f565])).
fof(f565, plain, (spl12_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl12_44])])).
fof(f1617, plain, (~ (h2(e13) = h2(op1(e11, e11))) | spl12_233), inference(avatar_component_clause, [], [f1615])).
fof(f1615, plain, (spl12_233 <=> (h2(e13) = h2(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl12_233])])).
fof(f3271, plain, (~ spl12_37 | spl12_232), inference(avatar_contradiction_clause, [], [f3270])).
fof(f3270, plain, ($false | (~ spl12_37 | spl12_232)), inference(trivial_inequality_removal, [], [f3269])).
fof(f3269, plain, (~ (h2(e10) = h2(e10)) | (~ spl12_37 | spl12_232)), inference(forward_demodulation, [], [f1613, f538])).
fof(f538, plain, ((e10 = op1(e11, e12)) | ~ spl12_37), inference(avatar_component_clause, [], [f536])).
fof(f536, plain, (spl12_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_37])])).
fof(f1613, plain, (~ (h2(e10) = h2(op1(e11, e12))) | spl12_232), inference(avatar_component_clause, [], [f1611])).
fof(f1611, plain, (spl12_232 <=> (h2(e10) = h2(op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl12_232])])).
fof(f3239, plain, (~ spl12_31 | ~ spl12_99 | ~ spl12_169 | spl12_230), inference(avatar_contradiction_clause, [], [f3238])).
fof(f3238, plain, ($false | (~ spl12_31 | ~ spl12_99 | ~ spl12_169 | spl12_230)), inference(subsumption_resolution, [], [f3237, f833])).
fof(f833, plain, ((e22 = op2(e22, e20)) | ~ spl12_99), inference(avatar_component_clause, [], [f831])).
fof(f831, plain, (spl12_99 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_99])])).
fof(f3237, plain, (~ (e22 = op2(e22, e20)) | (~ spl12_31 | ~ spl12_169 | spl12_230)), inference(forward_demodulation, [], [f3236, f1055])).
fof(f1055, plain, (e22 = h2(e12)), inference(forward_demodulation, [], [f1054, f1053])).
fof(f1053, plain, (e22 = op2(e21, h2(e13))), inference(backward_demodulation, [], [f300, f309])).
fof(f309, plain, (op2(e21, e21) = h2(e13)), inference(cnf_transformation, [], [f17])).
fof(f300, plain, (e22 = op2(e21, op2(e21, e21))), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((e23 = op2(e21, e21)) & (e22 = op2(e21, op2(e21, e21))) & (e20 = op2(e21, op2(e21, op2(e21, e21))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax15)).
fof(f1054, plain, (h2(e12) = op2(e21, h2(e13))), inference(forward_demodulation, [], [f308, f309])).
fof(f308, plain, (op2(e21, op2(e21, e21)) = h2(e12)), inference(cnf_transformation, [], [f17])).
fof(f3236, plain, (~ (op2(e22, e20) = h2(e12)) | (~ spl12_31 | ~ spl12_169 | spl12_230)), inference(forward_demodulation, [], [f3235, f512])).
fof(f512, plain, ((e12 = op1(e12, e10)) | ~ spl12_31), inference(avatar_component_clause, [], [f510])).
fof(f510, plain, (spl12_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_31])])).
fof(f3235, plain, (~ (op2(e22, e20) = h2(op1(e12, e10))) | (~ spl12_169 | spl12_230)), inference(forward_demodulation, [], [f1605, f1249])).
fof(f1605, plain, (~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | spl12_230), inference(avatar_component_clause, [], [f1603])).
fof(f1603, plain, (spl12_230 <=> (h2(op1(e12, e10)) = op2(e22, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl12_230])])).
fof(f3208, plain, (~ spl12_24 | ~ spl12_210 | ~ spl12_224 | spl12_228), inference(avatar_contradiction_clause, [], [f3207])).
fof(f3207, plain, ($false | (~ spl12_24 | ~ spl12_210 | ~ spl12_224 | spl12_228)), inference(subsumption_resolution, [], [f3206, f1580])).
fof(f1580, plain, ((e23 = h2(e13)) | ~ spl12_224), inference(avatar_component_clause, [], [f1579])).
fof(f1579, plain, (spl12_224 <=> (e23 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_224])])).
fof(f3206, plain, (~ (e23 = h2(e13)) | (~ spl12_24 | ~ spl12_210 | spl12_228)), inference(forward_demodulation, [], [f3205, f1482])).
fof(f1482, plain, ((e23 = h3(e13)) | ~ spl12_210), inference(avatar_component_clause, [], [f1481])).
fof(f1481, plain, (spl12_210 <=> (e23 = h3(e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_210])])).
fof(f3205, plain, (~ (h2(e13) = h3(e13)) | (~ spl12_24 | spl12_228)), inference(forward_demodulation, [], [f1597, f482])).
fof(f482, plain, ((e13 = op1(e12, e12)) | ~ spl12_24), inference(avatar_component_clause, [], [f480])).
fof(f480, plain, (spl12_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_24])])).
fof(f1597, plain, (~ (h3(e13) = h2(op1(e12, e12))) | spl12_228), inference(avatar_component_clause, [], [f1595])).
fof(f1595, plain, (spl12_228 <=> (h3(e13) = h2(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl12_228])])).
fof(f3177, plain, (~ spl12_11 | ~ spl12_79 | ~ spl12_224 | spl12_226), inference(avatar_contradiction_clause, [], [f3176])).
fof(f3176, plain, ($false | (~ spl12_11 | ~ spl12_79 | ~ spl12_224 | spl12_226)), inference(subsumption_resolution, [], [f3175, f748])).
fof(f748, plain, ((e22 = op2(e23, e21)) | ~ spl12_79), inference(avatar_component_clause, [], [f746])).
fof(f746, plain, (spl12_79 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_79])])).
fof(f3175, plain, (~ (e22 = op2(e23, e21)) | (~ spl12_11 | ~ spl12_224 | spl12_226)), inference(forward_demodulation, [], [f3174, f1055])).
fof(f3174, plain, (~ (op2(e23, e21) = h2(e12)) | (~ spl12_11 | ~ spl12_224 | spl12_226)), inference(forward_demodulation, [], [f3173, f427])).
fof(f427, plain, ((e12 = op1(e13, e11)) | ~ spl12_11), inference(avatar_component_clause, [], [f425])).
fof(f425, plain, (spl12_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl12_11])])).
fof(f3173, plain, (~ (op2(e23, e21) = h2(op1(e13, e11))) | (~ spl12_224 | spl12_226)), inference(forward_demodulation, [], [f1589, f1580])).
fof(f1589, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | spl12_226), inference(avatar_component_clause, [], [f1587])).
fof(f1587, plain, (spl12_226 <=> (h2(op1(e13, e11)) = op2(h2(e13), e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_226])])).
fof(f3145, plain, (~ spl12_6 | ~ spl12_74 | ~ spl12_224 | spl12_225), inference(avatar_contradiction_clause, [], [f3144])).
fof(f3144, plain, ($false | (~ spl12_6 | ~ spl12_74 | ~ spl12_224 | spl12_225)), inference(subsumption_resolution, [], [f3143, f727])).
fof(f727, plain, ((e21 = op2(e23, e22)) | ~ spl12_74), inference(avatar_component_clause, [], [f725])).
fof(f725, plain, (spl12_74 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_74])])).
fof(f3143, plain, (~ (e21 = op2(e23, e22)) | (~ spl12_6 | ~ spl12_224 | spl12_225)), inference(forward_demodulation, [], [f3142, f306])).
fof(f3142, plain, (~ (op2(e23, e22) = h2(e11)) | (~ spl12_6 | ~ spl12_224 | spl12_225)), inference(forward_demodulation, [], [f3141, f406])).
fof(f406, plain, ((e11 = op1(e13, e12)) | ~ spl12_6), inference(avatar_component_clause, [], [f404])).
fof(f404, plain, (spl12_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_6])])).
fof(f3141, plain, (~ (op2(e23, e22) = h2(op1(e13, e12))) | (~ spl12_224 | spl12_225)), inference(forward_demodulation, [], [f1585, f1580])).
fof(f1585, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | spl12_225), inference(avatar_component_clause, [], [f1583])).
fof(f1583, plain, (spl12_225 <=> (h2(op1(e13, e12)) = op2(h2(e13), e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_225])])).
fof(f3109, plain, (~ spl12_55 | ~ spl12_123 | ~ spl12_169 | spl12_235), inference(avatar_contradiction_clause, [], [f3108])).
fof(f3108, plain, ($false | (~ spl12_55 | ~ spl12_123 | ~ spl12_169 | spl12_235)), inference(subsumption_resolution, [], [f3107, f935])).
fof(f935, plain, ((e22 = op2(e20, e22)) | ~ spl12_123), inference(avatar_component_clause, [], [f933])).
fof(f933, plain, (spl12_123 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_123])])).
fof(f3107, plain, (~ (e22 = op2(e20, e22)) | (~ spl12_55 | ~ spl12_169 | spl12_235)), inference(forward_demodulation, [], [f3106, f1055])).
fof(f3106, plain, (~ (op2(e20, e22) = h2(e12)) | (~ spl12_55 | ~ spl12_169 | spl12_235)), inference(forward_demodulation, [], [f3105, f614])).
fof(f614, plain, ((e12 = op1(e10, e12)) | ~ spl12_55), inference(avatar_component_clause, [], [f612])).
fof(f612, plain, (spl12_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_55])])).
fof(f3105, plain, (~ (op2(e20, e22) = h2(op1(e10, e12))) | (~ spl12_169 | spl12_235)), inference(forward_demodulation, [], [f1625, f1249])).
fof(f1625, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | spl12_235), inference(avatar_component_clause, [], [f1623])).
fof(f1623, plain, (spl12_235 <=> (h2(op1(e10, e12)) = op2(h2(e10), e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_235])])).
fof(f3074, plain, (~ spl12_35 | spl12_231), inference(avatar_contradiction_clause, [], [f3073])).
fof(f3073, plain, ($false | (~ spl12_35 | spl12_231)), inference(subsumption_resolution, [], [f3072, f1055])).
fof(f3072, plain, (~ (e22 = h2(e12)) | (~ spl12_35 | spl12_231)), inference(forward_demodulation, [], [f1609, f529])).
fof(f529, plain, ((e12 = op1(e11, e13)) | ~ spl12_35), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl12_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_35])])).
fof(f1609, plain, (~ (e22 = h2(op1(e11, e13))) | spl12_231), inference(avatar_component_clause, [], [f1607])).
fof(f1607, plain, (spl12_231 <=> (e22 = h2(op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl12_231])])).
fof(f3039, plain, (~ spl12_18 | ~ spl12_86 | ~ spl12_224 | spl12_227), inference(avatar_contradiction_clause, [], [f3038])).
fof(f3038, plain, ($false | (~ spl12_18 | ~ spl12_86 | ~ spl12_224 | spl12_227)), inference(subsumption_resolution, [], [f3037, f778])).
fof(f778, plain, ((e21 = op2(e22, e23)) | ~ spl12_86), inference(avatar_component_clause, [], [f776])).
fof(f776, plain, (spl12_86 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_86])])).
fof(f3037, plain, (~ (e21 = op2(e22, e23)) | (~ spl12_18 | ~ spl12_224 | spl12_227)), inference(forward_demodulation, [], [f3036, f306])).
fof(f3036, plain, (~ (op2(e22, e23) = h2(e11)) | (~ spl12_18 | ~ spl12_224 | spl12_227)), inference(forward_demodulation, [], [f3035, f457])).
fof(f457, plain, ((e11 = op1(e12, e13)) | ~ spl12_18), inference(avatar_component_clause, [], [f455])).
fof(f455, plain, (spl12_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_18])])).
fof(f3035, plain, (~ (op2(e22, e23) = h2(op1(e12, e13))) | (~ spl12_224 | spl12_227)), inference(forward_demodulation, [], [f1593, f1580])).
fof(f1593, plain, (~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | spl12_227), inference(avatar_component_clause, [], [f1591])).
fof(f1591, plain, (spl12_227 <=> (h2(op1(e12, e13)) = op2(e22, h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl12_227])])).
fof(f3001, plain, (~ spl12_1 | ~ spl12_69 | ~ spl12_169 | spl12_223 | ~ spl12_224), inference(avatar_contradiction_clause, [], [f3000])).
fof(f3000, plain, ($false | (~ spl12_1 | ~ spl12_69 | ~ spl12_169 | spl12_223 | ~ spl12_224)), inference(subsumption_resolution, [], [f2999, f706])).
fof(f706, plain, ((e20 = op2(e23, e23)) | ~ spl12_69), inference(avatar_component_clause, [], [f704])).
fof(f704, plain, (spl12_69 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_69])])).
fof(f2999, plain, (~ (e20 = op2(e23, e23)) | (~ spl12_1 | ~ spl12_169 | spl12_223 | ~ spl12_224)), inference(forward_demodulation, [], [f2998, f1249])).
fof(f2998, plain, (~ (op2(e23, e23) = h2(e10)) | (~ spl12_1 | spl12_223 | ~ spl12_224)), inference(forward_demodulation, [], [f2997, f385])).
fof(f385, plain, ((e10 = op1(e13, e13)) | ~ spl12_1), inference(avatar_component_clause, [], [f383])).
fof(f383, plain, (spl12_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_1])])).
fof(f2997, plain, (~ (op2(e23, e23) = h2(op1(e13, e13))) | (spl12_223 | ~ spl12_224)), inference(forward_demodulation, [], [f1577, f1580])).
fof(f1577, plain, (~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | spl12_223), inference(avatar_component_clause, [], [f1575])).
fof(f1575, plain, (spl12_223 <=> (h2(op1(e13, e13)) = op2(h2(e13), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl12_223])])).
fof(f2965, plain, (~ spl12_16 | ~ spl12_84 | ~ spl12_169 | spl12_222 | ~ spl12_224), inference(avatar_contradiction_clause, [], [f2964])).
fof(f2964, plain, ($false | (~ spl12_16 | ~ spl12_84 | ~ spl12_169 | spl12_222 | ~ spl12_224)), inference(subsumption_resolution, [], [f2963, f769])).
fof(f769, plain, ((e23 = op2(e23, e20)) | ~ spl12_84), inference(avatar_component_clause, [], [f767])).
fof(f767, plain, (spl12_84 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_84])])).
fof(f2963, plain, (~ (e23 = op2(e23, e20)) | (~ spl12_16 | ~ spl12_169 | spl12_222 | ~ spl12_224)), inference(forward_demodulation, [], [f2962, f1580])).
fof(f2962, plain, (~ (op2(e23, e20) = h2(e13)) | (~ spl12_16 | ~ spl12_169 | spl12_222 | ~ spl12_224)), inference(forward_demodulation, [], [f2961, f448])).
fof(f448, plain, ((e13 = op1(e13, e10)) | ~ spl12_16), inference(avatar_component_clause, [], [f446])).
fof(f446, plain, (spl12_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_16])])).
fof(f2961, plain, (~ (op2(e23, e20) = h2(op1(e13, e10))) | (~ spl12_169 | spl12_222 | ~ spl12_224)), inference(forward_demodulation, [], [f2960, f1580])).
fof(f2960, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | (~ spl12_169 | spl12_222)), inference(forward_demodulation, [], [f1573, f1249])).
fof(f1573, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | spl12_222), inference(avatar_component_clause, [], [f1571])).
fof(f1571, plain, (spl12_222 <=> (h2(op1(e13, e10)) = op2(h2(e13), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl12_222])])).
fof(f2942, plain, (~ spl12_52 | ~ spl12_120 | ~ spl12_169 | spl12_221 | ~ spl12_224), inference(avatar_contradiction_clause, [], [f2941])).
fof(f2941, plain, ($false | (~ spl12_52 | ~ spl12_120 | ~ spl12_169 | spl12_221 | ~ spl12_224)), inference(subsumption_resolution, [], [f2940, f922])).
fof(f922, plain, ((e23 = op2(e20, e23)) | ~ spl12_120), inference(avatar_component_clause, [], [f920])).
fof(f920, plain, (spl12_120 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_120])])).
fof(f2940, plain, (~ (e23 = op2(e20, e23)) | (~ spl12_52 | ~ spl12_169 | spl12_221 | ~ spl12_224)), inference(forward_demodulation, [], [f2939, f1580])).
fof(f2939, plain, (~ (op2(e20, e23) = h2(e13)) | (~ spl12_52 | ~ spl12_169 | spl12_221 | ~ spl12_224)), inference(forward_demodulation, [], [f2938, f601])).
fof(f601, plain, ((e13 = op1(e10, e13)) | ~ spl12_52), inference(avatar_component_clause, [], [f599])).
fof(f599, plain, (spl12_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_52])])).
fof(f2938, plain, (~ (op2(e20, e23) = h2(op1(e10, e13))) | (~ spl12_169 | spl12_221 | ~ spl12_224)), inference(forward_demodulation, [], [f2937, f1249])).
fof(f2937, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), e23)) | (spl12_221 | ~ spl12_224)), inference(forward_demodulation, [], [f1569, f1580])).
fof(f1569, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | spl12_221), inference(avatar_component_clause, [], [f1567])).
fof(f1567, plain, (spl12_221 <=> (h2(op1(e10, e13)) = op2(h2(e10), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl12_221])])).
fof(f2919, plain, (~ spl12_46 | ~ spl12_114 | ~ spl12_169 | spl12_234), inference(avatar_contradiction_clause, [], [f2918])).
fof(f2918, plain, ($false | (~ spl12_46 | ~ spl12_114 | ~ spl12_169 | spl12_234)), inference(subsumption_resolution, [], [f2917, f897])).
fof(f897, plain, ((e21 = op2(e21, e20)) | ~ spl12_114), inference(avatar_component_clause, [], [f895])).
fof(f895, plain, (spl12_114 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_114])])).
fof(f2917, plain, (~ (e21 = op2(e21, e20)) | (~ spl12_46 | ~ spl12_169 | spl12_234)), inference(forward_demodulation, [], [f2916, f306])).
fof(f2916, plain, (~ (op2(e21, e20) = h2(e11)) | (~ spl12_46 | ~ spl12_169 | spl12_234)), inference(forward_demodulation, [], [f2915, f576])).
fof(f576, plain, ((e11 = op1(e11, e10)) | ~ spl12_46), inference(avatar_component_clause, [], [f574])).
fof(f574, plain, (spl12_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_46])])).
fof(f2915, plain, (~ (op2(e21, e20) = h2(op1(e11, e10))) | (~ spl12_169 | spl12_234)), inference(forward_demodulation, [], [f1621, f1249])).
fof(f1621, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | spl12_234), inference(avatar_component_clause, [], [f1619])).
fof(f1619, plain, (spl12_234 <=> (h2(op1(e11, e10)) = op2(e21, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl12_234])])).
fof(f2891, plain, (~ spl12_8 | ~ spl12_16), inference(avatar_split_clause, [], [f2889, f446, f412])).
fof(f412, plain, (spl12_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_8])])).
fof(f2889, plain, (~ (e13 = op1(e13, e12)) | ~ spl12_16), inference(backward_demodulation, [], [f207, f448])).
fof(f207, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax7)).
fof(f2860, plain, (spl12_16 | ~ spl12_65), inference(avatar_split_clause, [], [f2852, f655, f446])).
fof(f655, plain, (spl12_65 <=> (e10 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl12_65])])).
fof(f2852, plain, ((e13 = op1(e13, e10)) | ~ spl12_65), inference(backward_demodulation, [], [f73, f657])).
fof(f657, plain, ((e10 = unit1) | ~ spl12_65), inference(avatar_component_clause, [], [f655])).
fof(f73, plain, (e13 = op1(e13, unit1)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)) & (e13 = op1(e13, unit1)) & (e13 = op1(unit1, e13)) & (e12 = op1(e12, unit1)) & (e12 = op1(unit1, e12)) & (e11 = op1(e11, unit1)) & (e11 = op1(unit1, e11)) & (e10 = op1(e10, unit1)) & (e10 = op1(unit1, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax2)).
fof(f2859, plain, (spl12_52 | ~ spl12_65), inference(avatar_split_clause, [], [f2851, f655, f599])).
fof(f2851, plain, ((e13 = op1(e10, e13)) | ~ spl12_65), inference(backward_demodulation, [], [f72, f657])).
fof(f72, plain, (e13 = op1(unit1, e13)), inference(cnf_transformation, [], [f2])).
fof(f2858, plain, (spl12_55 | ~ spl12_65), inference(avatar_split_clause, [], [f2849, f655, f612])).
fof(f2849, plain, ((e12 = op1(e10, e12)) | ~ spl12_65), inference(backward_demodulation, [], [f70, f657])).
fof(f70, plain, (e12 = op1(unit1, e12)), inference(cnf_transformation, [], [f2])).
fof(f2857, plain, (spl12_61 | ~ spl12_65), inference(avatar_split_clause, [], [f2846, f655, f638])).
fof(f638, plain, (spl12_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_61])])).
fof(f2846, plain, ((e10 = op1(e10, e10)) | ~ spl12_65), inference(backward_demodulation, [], [f67, f657])).
fof(f67, plain, (e10 = op1(e10, unit1)), inference(cnf_transformation, [], [f2])).
fof(f2826, plain, (~ spl12_77 | ~ spl12_93), inference(avatar_split_clause, [], [f2823, f806, f738])).
fof(f738, plain, (spl12_77 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_77])])).
fof(f806, plain, (spl12_93 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_93])])).
fof(f2823, plain, (~ (e20 = op2(e23, e21)) | ~ spl12_93), inference(backward_demodulation, [], [f223, f808])).
fof(f808, plain, ((e20 = op2(e22, e21)) | ~ spl12_93), inference(avatar_component_clause, [], [f806])).
fof(f223, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax8)).
fof(f2817, plain, (~ spl12_95 | ~ spl12_99), inference(avatar_split_clause, [], [f2813, f831, f814])).
fof(f814, plain, (spl12_95 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_95])])).
fof(f2813, plain, (~ (e22 = op2(e22, e21)) | ~ spl12_99), inference(backward_demodulation, [], [f248, f833])).
fof(f248, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f2787, plain, (spl12_84 | ~ spl12_133), inference(avatar_split_clause, [], [f2775, f976, f767])).
fof(f976, plain, (spl12_133 <=> (e20 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl12_133])])).
fof(f2775, plain, ((e23 = op2(e23, e20)) | ~ spl12_133), inference(backward_demodulation, [], [f130, f978])).
fof(f978, plain, ((e20 = unit2) | ~ spl12_133), inference(avatar_component_clause, [], [f976])).
fof(f130, plain, (e23 = op2(e23, unit2)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (((e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)) & (e23 = op2(e23, unit2)) & (e23 = op2(unit2, e23)) & (e22 = op2(e22, unit2)) & (e22 = op2(unit2, e22)) & (e21 = op2(e21, unit2)) & (e21 = op2(unit2, e21)) & (e20 = op2(e20, unit2)) & (e20 = op2(unit2, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax5)).
fof(f2783, plain, (spl12_99 | ~ spl12_133), inference(avatar_split_clause, [], [f2773, f976, f831])).
fof(f2773, plain, ((e22 = op2(e22, e20)) | ~ spl12_133), inference(backward_demodulation, [], [f128, f978])).
fof(f128, plain, (e22 = op2(e22, unit2)), inference(cnf_transformation, [], [f5])).
fof(f2782, plain, (spl12_123 | ~ spl12_133), inference(avatar_split_clause, [], [f2772, f976, f933])).
fof(f2772, plain, ((e22 = op2(e20, e22)) | ~ spl12_133), inference(backward_demodulation, [], [f127, f978])).
fof(f127, plain, (e22 = op2(unit2, e22)), inference(cnf_transformation, [], [f5])).
fof(f2781, plain, (spl12_114 | ~ spl12_133), inference(avatar_split_clause, [], [f2771, f976, f895])).
fof(f2771, plain, ((e21 = op2(e21, e20)) | ~ spl12_133), inference(backward_demodulation, [], [f126, f978])).
fof(f126, plain, (e21 = op2(e21, unit2)), inference(cnf_transformation, [], [f5])).
fof(f2780, plain, (spl12_129 | ~ spl12_133), inference(avatar_split_clause, [], [f2769, f976, f959])).
fof(f959, plain, (spl12_129 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_129])])).
fof(f2769, plain, ((e20 = op2(e20, e20)) | ~ spl12_133), inference(backward_demodulation, [], [f124, f978])).
fof(f124, plain, (e20 = op2(e20, unit2)), inference(cnf_transformation, [], [f5])).
fof(f2748, plain, (~ spl12_81 | ~ spl12_179), inference(avatar_split_clause, [], [f2742, f1297, f755])).
fof(f755, plain, (spl12_81 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_81])])).
fof(f1297, plain, (spl12_179 <=> (e20 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_179])])).
fof(f2742, plain, (~ (e20 = op2(e23, e20)) | ~ spl12_179), inference(backward_demodulation, [], [f1040, f1298])).
fof(f1298, plain, ((e20 = h1(e13)) | ~ spl12_179), inference(avatar_component_clause, [], [f1297])).
fof(f1040, plain, ~ (op2(e23, e20) = h1(e13)), inference(backward_demodulation, [], [f214, f305])).
fof(f305, plain, (op2(e20, e20) = h1(e13)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e20, e20) = h1(e13)) & (op2(e20, op2(e20, e20)) = h1(e12)) & (h1(e10) = op2(e20, op2(e20, op2(e20, e20)))) & (e20 = h1(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax16)).
fof(f214, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f8])).
fof(f2724, plain, (~ spl12_93 | ~ spl12_25 | ~ spl12_169 | spl12_229), inference(avatar_split_clause, [], [f2723, f1599, f1248, f485, f806])).
fof(f485, plain, (spl12_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl12_25])])).
fof(f1599, plain, (spl12_229 <=> (op2(e22, e21) = h2(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl12_229])])).
fof(f2723, plain, (~ (e20 = op2(e22, e21)) | (~ spl12_25 | ~ spl12_169 | spl12_229)), inference(forward_demodulation, [], [f2722, f1249])).
fof(f2722, plain, (~ (op2(e22, e21) = h2(e10)) | (~ spl12_25 | spl12_229)), inference(forward_demodulation, [], [f1601, f487])).
fof(f487, plain, ((e10 = op1(e12, e11)) | ~ spl12_25), inference(avatar_component_clause, [], [f485])).
fof(f1601, plain, (~ (op2(e22, e21) = h2(op1(e12, e11))) | spl12_229), inference(avatar_component_clause, [], [f1599])).
fof(f2676, plain, (spl12_34 | ~ spl12_68), inference(avatar_contradiction_clause, [], [f2675])).
fof(f2675, plain, ($false | (spl12_34 | ~ spl12_68)), inference(subsumption_resolution, [], [f2668, f524])).
fof(f524, plain, (~ (e11 = op1(e11, e13)) | spl12_34), inference(avatar_component_clause, [], [f523])).
fof(f523, plain, (spl12_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_34])])).
fof(f2668, plain, ((e11 = op1(e11, e13)) | ~ spl12_68), inference(backward_demodulation, [], [f69, f669])).
fof(f669, plain, ((e13 = unit1) | ~ spl12_68), inference(avatar_component_clause, [], [f667])).
fof(f667, plain, (spl12_68 <=> (e13 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl12_68])])).
fof(f69, plain, (e11 = op1(e11, unit1)), inference(cnf_transformation, [], [f2])).
fof(f2644, plain, (~ spl12_105 | ~ spl12_106), inference(avatar_contradiction_clause, [], [f2643])).
fof(f2643, plain, ($false | (~ spl12_105 | ~ spl12_106)), inference(subsumption_resolution, [], [f2642, f266])).
fof(f266, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f10])).
fof(f10, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax10)).
fof(f2642, plain, ((e20 = e21) | (~ spl12_105 | ~ spl12_106)), inference(forward_demodulation, [], [f863, f859])).
fof(f859, plain, ((e20 = op2(e21, e22)) | ~ spl12_105), inference(avatar_component_clause, [], [f857])).
fof(f857, plain, (spl12_105 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_105])])).
fof(f863, plain, ((e21 = op2(e21, e22)) | ~ spl12_106), inference(avatar_component_clause, [], [f861])).
fof(f861, plain, (spl12_106 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_106])])).
fof(f2633, plain, (~ spl12_78 | ~ spl12_126), inference(avatar_split_clause, [], [f2631, f946, f742])).
fof(f742, plain, (spl12_78 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_78])])).
fof(f2631, plain, (~ (e21 = op2(e23, e21)) | ~ spl12_126), inference(backward_demodulation, [], [f220, f948])).
fof(f220, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f8])).
fof(f2581, plain, (spl12_103 | ~ spl12_224), inference(avatar_split_clause, [], [f2151, f1579, f848])).
fof(f848, plain, (spl12_103 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_103])])).
fof(f2151, plain, ((e22 = op2(e21, e23)) | ~ spl12_224), inference(backward_demodulation, [], [f1053, f1580])).
fof(f2578, plain, (spl12_102 | ~ spl12_136), inference(avatar_split_clause, [], [f2457, f988, f844])).
fof(f844, plain, (spl12_102 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_102])])).
fof(f988, plain, (spl12_136 <=> (e23 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl12_136])])).
fof(f2457, plain, ((e21 = op2(e21, e23)) | ~ spl12_136), inference(backward_demodulation, [], [f126, f990])).
fof(f990, plain, ((e23 = unit2) | ~ spl12_136), inference(avatar_component_clause, [], [f988])).
fof(f2547, plain, (~ spl12_22 | ~ spl12_24), inference(avatar_contradiction_clause, [], [f2546])).
fof(f2546, plain, ($false | (~ spl12_22 | ~ spl12_24)), inference(subsumption_resolution, [], [f2545, f264])).
fof(f264, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f9])).
fof(f9, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax9)).
fof(f2545, plain, ((e11 = e13) | (~ spl12_22 | ~ spl12_24)), inference(backward_demodulation, [], [f482, f474])).
fof(f474, plain, ((e11 = op1(e12, e12)) | ~ spl12_22), inference(avatar_component_clause, [], [f472])).
fof(f472, plain, (spl12_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_22])])).
fof(f2538, plain, (~ spl12_55 | ~ spl12_56), inference(avatar_contradiction_clause, [], [f2537])).
fof(f2537, plain, ($false | (~ spl12_55 | ~ spl12_56)), inference(subsumption_resolution, [], [f2536, f265])).
fof(f265, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f9])).
fof(f2536, plain, ((e12 = e13) | (~ spl12_55 | ~ spl12_56)), inference(forward_demodulation, [], [f618, f614])).
fof(f618, plain, ((e13 = op1(e10, e12)) | ~ spl12_56), inference(avatar_component_clause, [], [f616])).
fof(f616, plain, (spl12_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_56])])).
fof(f2525, plain, (~ spl12_37 | ~ spl12_67), inference(avatar_contradiction_clause, [], [f2524])).
fof(f2524, plain, ($false | (~ spl12_37 | ~ spl12_67)), inference(subsumption_resolution, [], [f2523, f260])).
fof(f260, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f9])).
fof(f2523, plain, ((e10 = e11) | (~ spl12_37 | ~ spl12_67)), inference(forward_demodulation, [], [f2511, f538])).
fof(f2511, plain, ((e11 = op1(e11, e12)) | ~ spl12_67), inference(backward_demodulation, [], [f69, f665])).
fof(f665, plain, ((e12 = unit1) | ~ spl12_67), inference(avatar_component_clause, [], [f663])).
fof(f663, plain, (spl12_67 <=> (e12 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl12_67])])).
fof(f2426, plain, (~ spl12_13 | ~ spl12_16), inference(avatar_contradiction_clause, [], [f2425])).
fof(f2425, plain, ($false | (~ spl12_13 | ~ spl12_16)), inference(subsumption_resolution, [], [f2423, f262])).
fof(f262, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f9])).
fof(f2423, plain, ((e10 = e13) | (~ spl12_13 | ~ spl12_16)), inference(backward_demodulation, [], [f448, f436])).
fof(f436, plain, ((e10 = op1(e13, e10)) | ~ spl12_13), inference(avatar_component_clause, [], [f434])).
fof(f434, plain, (spl12_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_13])])).
fof(f2412, plain, (~ spl12_37 | ~ spl12_38), inference(avatar_contradiction_clause, [], [f2411])).
fof(f2411, plain, ($false | (~ spl12_37 | ~ spl12_38)), inference(subsumption_resolution, [], [f2410, f260])).
fof(f2410, plain, ((e10 = e11) | (~ spl12_37 | ~ spl12_38)), inference(forward_demodulation, [], [f542, f538])).
fof(f542, plain, ((e11 = op1(e11, e12)) | ~ spl12_38), inference(avatar_component_clause, [], [f540])).
fof(f540, plain, (spl12_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_38])])).
fof(f2387, plain, (~ spl12_37 | ~ spl12_66), inference(avatar_contradiction_clause, [], [f2386])).
fof(f2386, plain, ($false | (~ spl12_37 | ~ spl12_66)), inference(subsumption_resolution, [], [f2385, f261])).
fof(f261, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f9])).
fof(f2385, plain, ((e10 = e12) | (~ spl12_37 | ~ spl12_66)), inference(forward_demodulation, [], [f2375, f538])).
fof(f2375, plain, ((e12 = op1(e11, e12)) | ~ spl12_66), inference(backward_demodulation, [], [f70, f661])).
fof(f661, plain, ((e11 = unit1) | ~ spl12_66), inference(avatar_component_clause, [], [f659])).
fof(f659, plain, (spl12_66 <=> (e11 = unit1)), introduced(avatar_definition, [new_symbols(naming, [spl12_66])])).
fof(f2370, plain, (~ spl12_71 | spl12_138), inference(avatar_contradiction_clause, [], [f2369])).
fof(f2369, plain, ($false | (~ spl12_71 | spl12_138)), inference(subsumption_resolution, [], [f2368, f1088])).
fof(f1088, plain, (~ (e22 = h4(e13)) | spl12_138), inference(avatar_component_clause, [], [f1086])).
fof(f1086, plain, (spl12_138 <=> (e22 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_138])])).
fof(f2368, plain, ((e22 = h4(e13)) | ~ spl12_71), inference(backward_demodulation, [], [f317, f714])).
fof(f714, plain, ((e22 = op2(e23, e23)) | ~ spl12_71), inference(avatar_component_clause, [], [f712])).
fof(f712, plain, (spl12_71 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_71])])).
fof(f317, plain, (op2(e23, e23) = h4(e13)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((op2(e23, e23) = h4(e13)) & (op2(e23, op2(e23, e23)) = h4(e12)) & (h4(e10) = op2(e23, op2(e23, op2(e23, e23)))) & (e23 = h4(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax19)).
fof(f2349, plain, (~ spl12_90 | spl12_154), inference(avatar_contradiction_clause, [], [f2348])).
fof(f2348, plain, ($false | (~ spl12_90 | spl12_154)), inference(subsumption_resolution, [], [f2347, f1169])).
fof(f1169, plain, (~ (e21 = h3(e13)) | spl12_154), inference(avatar_component_clause, [], [f1167])).
fof(f1167, plain, (spl12_154 <=> (e21 = h3(e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_154])])).
fof(f2347, plain, ((e21 = h3(e13)) | ~ spl12_90), inference(backward_demodulation, [], [f313, f795])).
fof(f795, plain, ((e21 = op2(e22, e22)) | ~ spl12_90), inference(avatar_component_clause, [], [f793])).
fof(f793, plain, (spl12_90 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_90])])).
fof(f313, plain, (op2(e22, e22) = h3(e13)), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ((op2(e22, e22) = h3(e13)) & (op2(e22, op2(e22, e22)) = h3(e12)) & (h3(e10) = op2(e22, op2(e22, op2(e22, e22)))) & (e22 = h3(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax18)).
fof(f2336, plain, (~ spl12_84 | ~ spl12_100), inference(avatar_split_clause, [], [f2334, f835, f767])).
fof(f835, plain, (spl12_100 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_100])])).
fof(f2334, plain, (~ (e23 = op2(e23, e20)) | ~ spl12_100), inference(backward_demodulation, [], [f217, f837])).
fof(f837, plain, ((e23 = op2(e22, e20)) | ~ spl12_100), inference(avatar_component_clause, [], [f835])).
fof(f217, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f8])).
fof(f2324, plain, (~ spl12_122 | ~ spl12_123), inference(avatar_contradiction_clause, [], [f2323])).
fof(f2323, plain, ($false | (~ spl12_122 | ~ spl12_123)), inference(subsumption_resolution, [], [f2322, f269])).
fof(f269, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f10])).
fof(f2322, plain, ((e21 = e22) | (~ spl12_122 | ~ spl12_123)), inference(backward_demodulation, [], [f935, f931])).
fof(f931, plain, ((e21 = op2(e20, e22)) | ~ spl12_122), inference(avatar_component_clause, [], [f929])).
fof(f929, plain, (spl12_122 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_122])])).
fof(f2311, plain, (~ spl12_105 | ~ spl12_135), inference(avatar_contradiction_clause, [], [f2310])).
fof(f2310, plain, ($false | (~ spl12_105 | ~ spl12_135)), inference(subsumption_resolution, [], [f2309, f266])).
fof(f2309, plain, ((e20 = e21) | (~ spl12_105 | ~ spl12_135)), inference(forward_demodulation, [], [f2300, f859])).
fof(f2300, plain, ((e21 = op2(e21, e22)) | ~ spl12_135), inference(backward_demodulation, [], [f126, f986])).
fof(f986, plain, ((e22 = unit2) | ~ spl12_135), inference(avatar_component_clause, [], [f984])).
fof(f984, plain, (spl12_135 <=> (e22 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl12_135])])).
fof(f2290, plain, (~ spl12_73 | ~ spl12_169), inference(avatar_split_clause, [], [f2289, f1248, f721])).
fof(f721, plain, (spl12_73 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_73])])).
fof(f2289, plain, (~ (e20 = op2(e23, e22)) | ~ spl12_169), inference(forward_demodulation, [], [f1061, f1249])).
fof(f1061, plain, ~ (op2(e23, e22) = h2(e10)), inference(backward_demodulation, [], [f228, f1057])).
fof(f1057, plain, (op2(e21, e22) = h2(e10)), inference(forward_demodulation, [], [f1056, f1053])).
fof(f1056, plain, (h2(e10) = op2(e21, op2(e21, h2(e13)))), inference(forward_demodulation, [], [f307, f309])).
fof(f307, plain, (op2(e21, op2(e21, op2(e21, e21))) = h2(e10)), inference(cnf_transformation, [], [f17])).
fof(f228, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f8])).
fof(f2259, plain, (~ spl12_7 | ~ spl12_55), inference(avatar_split_clause, [], [f2258, f612, f408])).
fof(f408, plain, (spl12_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_7])])).
fof(f2258, plain, (~ (e12 = op1(e13, e12)) | ~ spl12_55), inference(forward_demodulation, [], [f178, f614])).
fof(f178, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f2254, plain, (~ spl12_9 | ~ spl12_11), inference(avatar_contradiction_clause, [], [f2253])).
fof(f2253, plain, ($false | (~ spl12_9 | ~ spl12_11)), inference(subsumption_resolution, [], [f2252, f261])).
fof(f2252, plain, ((e10 = e12) | (~ spl12_9 | ~ spl12_11)), inference(backward_demodulation, [], [f427, f419])).
fof(f419, plain, ((e10 = op1(e13, e11)) | ~ spl12_9), inference(avatar_component_clause, [], [f417])).
fof(f417, plain, (spl12_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl12_9])])).
fof(f2235, plain, (~ spl12_30 | ~ spl12_46), inference(avatar_split_clause, [], [f2233, f574, f506])).
fof(f506, plain, (spl12_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_30])])).
fof(f2233, plain, (~ (e11 = op1(e12, e10)) | ~ spl12_46), inference(backward_demodulation, [], [f167, f576])).
fof(f167, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f2228, plain, (~ spl12_54 | ~ spl12_58), inference(avatar_split_clause, [], [f2226, f625, f608])).
fof(f608, plain, (spl12_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_54])])).
fof(f2226, plain, (~ (e11 = op1(e10, e12)) | ~ spl12_58), inference(backward_demodulation, [], [f191, f627])).
fof(f191, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f7])).
fof(f2227, plain, (~ spl12_26 | ~ spl12_58), inference(avatar_split_clause, [], [f2225, f625, f489])).
fof(f489, plain, (spl12_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl12_26])])).
fof(f2225, plain, (~ (e11 = op1(e12, e11)) | ~ spl12_58), inference(backward_demodulation, [], [f171, f627])).
fof(f171, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f7])).
fof(f2224, plain, (spl12_142 | ~ spl12_70), inference(avatar_split_clause, [], [f2223, f708, f1106])).
fof(f1106, plain, (spl12_142 <=> (e21 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_142])])).
fof(f708, plain, (spl12_70 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_70])])).
fof(f2223, plain, ((e21 = h4(e13)) | ~ spl12_70), inference(backward_demodulation, [], [f317, f710])).
fof(f710, plain, ((e21 = op2(e23, e23)) | ~ spl12_70), inference(avatar_component_clause, [], [f708])).
fof(f2210, plain, (spl12_210 | ~ spl12_92), inference(avatar_split_clause, [], [f2209, f801, f1481])).
fof(f801, plain, (spl12_92 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_92])])).
fof(f2209, plain, ((e23 = h3(e13)) | ~ spl12_92), inference(backward_demodulation, [], [f313, f803])).
fof(f803, plain, ((e23 = op2(e22, e22)) | ~ spl12_92), inference(avatar_component_clause, [], [f801])).
fof(f2208, plain, (~ spl12_154 | ~ spl12_94), inference(avatar_split_clause, [], [f2207, f810, f1167])).
fof(f810, plain, (spl12_94 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_94])])).
fof(f2207, plain, (~ (e21 = h3(e13)) | ~ spl12_94), inference(backward_demodulation, [], [f1067, f812])).
fof(f812, plain, ((e21 = op2(e22, e21)) | ~ spl12_94), inference(avatar_component_clause, [], [f810])).
fof(f1067, plain, ~ (op2(e22, e21) = h3(e13)), inference(backward_demodulation, [], [f251, f313])).
fof(f251, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f8])).
fof(f2180, plain, (~ spl12_105 | ~ spl12_134), inference(avatar_contradiction_clause, [], [f2179])).
fof(f2179, plain, ($false | (~ spl12_105 | ~ spl12_134)), inference(subsumption_resolution, [], [f2178, f267])).
fof(f267, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f10])).
fof(f2178, plain, ((e20 = e22) | (~ spl12_105 | ~ spl12_134)), inference(forward_demodulation, [], [f2169, f859])).
fof(f2169, plain, ((e22 = op2(e21, e22)) | ~ spl12_134), inference(backward_demodulation, [], [f127, f982])).
fof(f982, plain, ((e21 = unit2) | ~ spl12_134), inference(avatar_component_clause, [], [f980])).
fof(f980, plain, (spl12_134 <=> (e21 = unit2)), introduced(avatar_definition, [new_symbols(naming, [spl12_134])])).
fof(f2163, plain, (~ spl12_129 | ~ spl12_61 | ~ spl12_169 | spl12_220), inference(avatar_split_clause, [], [f2159, f1563, f1248, f638, f959])).
fof(f1563, plain, (spl12_220 <=> (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl12_220])])).
fof(f2159, plain, (~ (e20 = op2(e20, e20)) | (~ spl12_61 | ~ spl12_169 | spl12_220)), inference(backward_demodulation, [], [f1942, f1249])).
fof(f1942, plain, (~ (h2(e10) = op2(h2(e10), h2(e10))) | (~ spl12_61 | spl12_220)), inference(backward_demodulation, [], [f1565, f640])).
fof(f640, plain, ((e10 = op1(e10, e10)) | ~ spl12_61), inference(avatar_component_clause, [], [f638])).
fof(f1565, plain, (~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10))) | spl12_220), inference(avatar_component_clause, [], [f1563])).
fof(f2153, plain, (~ spl12_96 | ~ spl12_224), inference(avatar_split_clause, [], [f2149, f1579, f818])).
fof(f818, plain, (spl12_96 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_96])])).
fof(f2149, plain, (~ (e23 = op2(e22, e21)) | ~ spl12_224), inference(backward_demodulation, [], [f1048, f1580])).
fof(f1048, plain, ~ (op2(e22, e21) = h2(e13)), inference(backward_demodulation, [], [f221, f309])).
fof(f221, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f8])).
fof(f2144, plain, (~ spl12_118 | ~ spl12_120), inference(avatar_contradiction_clause, [], [f2143])).
fof(f2143, plain, ($false | (~ spl12_118 | ~ spl12_120)), inference(subsumption_resolution, [], [f2142, f270])).
fof(f270, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f10])).
fof(f2142, plain, ((e21 = e23) | (~ spl12_118 | ~ spl12_120)), inference(forward_demodulation, [], [f922, f914])).
fof(f914, plain, ((e21 = op2(e20, e23)) | ~ spl12_118), inference(avatar_component_clause, [], [f912])).
fof(f912, plain, (spl12_118 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_118])])).
fof(f2131, plain, (~ spl12_94 | ~ spl12_86), inference(avatar_split_clause, [], [f2130, f776, f810])).
fof(f2130, plain, (~ (e21 = op2(e22, e21)) | ~ spl12_86), inference(forward_demodulation, [], [f252, f778])).
fof(f252, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f2129, plain, (~ spl12_142 | ~ spl12_86), inference(avatar_split_clause, [], [f2128, f776, f1106])).
fof(f2128, plain, (~ (e21 = h4(e13)) | ~ spl12_86), inference(forward_demodulation, [], [f1074, f778])).
fof(f1074, plain, ~ (op2(e22, e23) = h4(e13)), inference(backward_demodulation, [], [f235, f317])).
fof(f235, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f2125, plain, (~ spl12_82 | ~ spl12_84), inference(avatar_contradiction_clause, [], [f2124])).
fof(f2124, plain, ($false | (~ spl12_82 | ~ spl12_84)), inference(subsumption_resolution, [], [f2123, f270])).
fof(f2123, plain, ((e21 = e23) | (~ spl12_82 | ~ spl12_84)), inference(forward_demodulation, [], [f769, f761])).
fof(f761, plain, ((e21 = op2(e23, e20)) | ~ spl12_82), inference(avatar_component_clause, [], [f759])).
fof(f759, plain, (spl12_82 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_82])])).
fof(f2122, plain, (~ spl12_142 | ~ spl12_74), inference(avatar_split_clause, [], [f2121, f725, f1106])).
fof(f2121, plain, (~ (e21 = h4(e13)) | ~ spl12_74), inference(forward_demodulation, [], [f1077, f727])).
fof(f1077, plain, ~ (op2(e23, e22) = h4(e13)), inference(backward_demodulation, [], [f259, f317])).
fof(f259, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f2051, plain, (~ spl12_15 | ~ spl12_16), inference(avatar_contradiction_clause, [], [f2050])).
fof(f2050, plain, ($false | (~ spl12_15 | ~ spl12_16)), inference(subsumption_resolution, [], [f2049, f265])).
fof(f2049, plain, ((e12 = e13) | (~ spl12_15 | ~ spl12_16)), inference(backward_demodulation, [], [f448, f444])).
fof(f444, plain, ((e12 = op1(e13, e10)) | ~ spl12_15), inference(avatar_component_clause, [], [f442])).
fof(f442, plain, (spl12_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_15])])).
fof(f2042, plain, (~ spl12_17 | ~ spl12_18), inference(avatar_contradiction_clause, [], [f2041])).
fof(f2041, plain, ($false | (~ spl12_17 | ~ spl12_18)), inference(subsumption_resolution, [], [f2040, f260])).
fof(f2040, plain, ((e10 = e11) | (~ spl12_17 | ~ spl12_18)), inference(backward_demodulation, [], [f457, f453])).
fof(f453, plain, ((e10 = op1(e12, e13)) | ~ spl12_17), inference(avatar_component_clause, [], [f451])).
fof(f451, plain, (spl12_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_17])])).
fof(f2013, plain, (~ spl12_34 | ~ spl12_35), inference(avatar_contradiction_clause, [], [f2012])).
fof(f2012, plain, ($false | (~ spl12_34 | ~ spl12_35)), inference(subsumption_resolution, [], [f2011, f263])).
fof(f263, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f9])).
fof(f2011, plain, ((e11 = e12) | (~ spl12_34 | ~ spl12_35)), inference(backward_demodulation, [], [f529, f525])).
fof(f525, plain, ((e11 = op1(e11, e13)) | ~ spl12_34), inference(avatar_component_clause, [], [f523])).
fof(f2010, plain, (~ spl12_3 | ~ spl12_35), inference(avatar_split_clause, [], [f2008, f527, f391])).
fof(f391, plain, (spl12_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl12_3])])).
fof(f2008, plain, (~ (e12 = op1(e13, e13)) | ~ spl12_35), inference(backward_demodulation, [], [f186, f529])).
fof(f186, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f7])).
fof(f2005, plain, (~ spl12_5 | ~ spl12_37), inference(avatar_split_clause, [], [f2002, f536, f400])).
fof(f400, plain, (spl12_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_5])])).
fof(f2002, plain, (~ (e10 = op1(e13, e12)) | ~ spl12_37), inference(backward_demodulation, [], [f180, f538])).
fof(f180, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f7])).
fof(f2004, plain, (~ spl12_21 | ~ spl12_37), inference(avatar_split_clause, [], [f2001, f536, f468])).
fof(f468, plain, (spl12_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_21])])).
fof(f2001, plain, (~ (e10 = op1(e12, e12)) | ~ spl12_37), inference(backward_demodulation, [], [f179, f538])).
fof(f179, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f7])).
fof(f2000, plain, (spl12_35 | ~ spl12_44), inference(avatar_split_clause, [], [f1995, f565, f527])).
fof(f1995, plain, ((e12 = op1(e11, e13)) | ~ spl12_44), inference(backward_demodulation, [], [f297, f567])).
fof(f297, plain, (e12 = op1(e11, op1(e11, e11))), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((e13 = op1(e11, e11)) & (e12 = op1(e11, op1(e11, e11))) & (e10 = op1(e11, op1(e11, op1(e11, e11))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax14)).
fof(f1998, plain, (~ spl12_40 | ~ spl12_44), inference(avatar_split_clause, [], [f1993, f565, f548])).
fof(f548, plain, (spl12_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl12_40])])).
fof(f1993, plain, (~ (e13 = op1(e11, e12)) | ~ spl12_44), inference(backward_demodulation, [], [f197, f567])).
fof(f197, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f7])).
fof(f1944, plain, (~ spl12_29 | ~ spl12_61), inference(avatar_split_clause, [], [f1934, f638, f502])).
fof(f502, plain, (spl12_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl12_29])])).
fof(f1934, plain, (~ (e10 = op1(e12, e10)) | ~ spl12_61), inference(backward_demodulation, [], [f165, f640])).
fof(f165, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f7])).
fof(f1930, plain, (spl12_31 | ~ spl12_65), inference(avatar_split_clause, [], [f1922, f655, f510])).
fof(f1922, plain, ((e12 = op1(e12, e10)) | ~ spl12_65), inference(backward_demodulation, [], [f71, f657])).
fof(f71, plain, (e12 = op1(e12, unit1)), inference(cnf_transformation, [], [f2])).
fof(f1928, plain, (spl12_46 | ~ spl12_65), inference(avatar_split_clause, [], [f1920, f655, f574])).
fof(f1920, plain, ((e11 = op1(e11, e10)) | ~ spl12_65), inference(backward_demodulation, [], [f69, f657])).
fof(f1927, plain, (spl12_58 | ~ spl12_65), inference(avatar_split_clause, [], [f1919, f655, f625])).
fof(f1919, plain, ((e11 = op1(e10, e11)) | ~ spl12_65), inference(backward_demodulation, [], [f68, f657])).
fof(f68, plain, (e11 = op1(unit1, e11)), inference(cnf_transformation, [], [f2])).
fof(f1914, plain, (~ spl12_74 | ~ spl12_75), inference(avatar_contradiction_clause, [], [f1913])).
fof(f1913, plain, ($false | (~ spl12_74 | ~ spl12_75)), inference(subsumption_resolution, [], [f1912, f269])).
fof(f1912, plain, ((e21 = e22) | (~ spl12_74 | ~ spl12_75)), inference(backward_demodulation, [], [f731, f727])).
fof(f731, plain, ((e22 = op2(e23, e22)) | ~ spl12_75), inference(avatar_component_clause, [], [f729])).
fof(f729, plain, (spl12_75 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl12_75])])).
fof(f1890, plain, (~ spl12_83 | ~ spl12_84), inference(avatar_contradiction_clause, [], [f1889])).
fof(f1889, plain, ($false | (~ spl12_83 | ~ spl12_84)), inference(subsumption_resolution, [], [f1888, f271])).
fof(f271, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f10])).
fof(f1888, plain, ((e22 = e23) | (~ spl12_83 | ~ spl12_84)), inference(backward_demodulation, [], [f769, f765])).
fof(f765, plain, ((e22 = op2(e23, e20)) | ~ spl12_83), inference(avatar_component_clause, [], [f763])).
fof(f763, plain, (spl12_83 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl12_83])])).
fof(f1865, plain, (~ spl12_85 | ~ spl12_93), inference(avatar_split_clause, [], [f1861, f806, f772])).
fof(f772, plain, (spl12_85 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_85])])).
fof(f1861, plain, (~ (e20 = op2(e22, e23)) | ~ spl12_93), inference(backward_demodulation, [], [f252, f808])).
fof(f1847, plain, (~ spl12_102 | ~ spl12_103), inference(avatar_contradiction_clause, [], [f1846])).
fof(f1846, plain, ($false | (~ spl12_102 | ~ spl12_103)), inference(subsumption_resolution, [], [f1845, f269])).
fof(f1845, plain, ((e21 = e22) | (~ spl12_102 | ~ spl12_103)), inference(backward_demodulation, [], [f850, f846])).
fof(f846, plain, ((e21 = op2(e21, e23)) | ~ spl12_102), inference(avatar_component_clause, [], [f844])).
fof(f850, plain, ((e22 = op2(e21, e23)) | ~ spl12_103), inference(avatar_component_clause, [], [f848])).
fof(f1844, plain, (~ spl12_87 | ~ spl12_103), inference(avatar_split_clause, [], [f1840, f848, f780])).
fof(f780, plain, (spl12_87 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_87])])).
fof(f1840, plain, (~ (e22 = op2(e22, e23)) | ~ spl12_103), inference(backward_demodulation, [], [f233, f850])).
fof(f233, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f1841, plain, (~ spl12_138 | ~ spl12_103), inference(avatar_split_clause, [], [f1837, f848, f1086])).
fof(f1837, plain, (~ (e22 = h4(e13)) | ~ spl12_103), inference(backward_demodulation, [], [f1073, f850])).
fof(f1073, plain, ~ (op2(e21, e23) = h4(e13)), inference(backward_demodulation, [], [f234, f317])).
fof(f234, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f8])).
fof(f1836, plain, (spl12_169 | ~ spl12_105), inference(avatar_split_clause, [], [f1835, f857, f1248])).
fof(f1835, plain, ((e20 = h2(e10)) | ~ spl12_105), inference(backward_demodulation, [], [f1057, f859])).
fof(f1834, plain, (spl12_224 | ~ spl12_112), inference(avatar_split_clause, [], [f1833, f886, f1579])).
fof(f886, plain, (spl12_112 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl12_112])])).
fof(f1833, plain, ((e23 = h2(e13)) | ~ spl12_112), inference(backward_demodulation, [], [f309, f888])).
fof(f888, plain, ((e23 = op2(e21, e21)) | ~ spl12_112), inference(avatar_component_clause, [], [f886])).
fof(f1814, plain, (~ spl12_88 | ~ spl12_120), inference(avatar_split_clause, [], [f1810, f920, f784])).
fof(f784, plain, (spl12_88 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl12_88])])).
fof(f1810, plain, (~ (e23 = op2(e22, e23)) | ~ spl12_120), inference(backward_demodulation, [], [f231, f922])).
fof(f231, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f8])).
fof(f1784, plain, (spl12_179 | ~ spl12_129), inference(avatar_split_clause, [], [f1783, f959, f1297])).
fof(f1783, plain, ((e20 = h1(e13)) | ~ spl12_129), inference(backward_demodulation, [], [f305, f961])).
fof(f961, plain, ((e20 = op2(e20, e20)) | ~ spl12_129), inference(avatar_component_clause, [], [f959])).
fof(f1780, plain, (spl12_120 | ~ spl12_133), inference(avatar_split_clause, [], [f1772, f976, f920])).
fof(f1772, plain, ((e23 = op2(e20, e23)) | ~ spl12_133), inference(backward_demodulation, [], [f129, f978])).
fof(f129, plain, (e23 = op2(unit2, e23)), inference(cnf_transformation, [], [f5])).
fof(f1776, plain, (spl12_126 | ~ spl12_133), inference(avatar_split_clause, [], [f1768, f976, f946])).
fof(f1768, plain, ((e21 = op2(e20, e21)) | ~ spl12_133), inference(backward_demodulation, [], [f125, f978])).
fof(f125, plain, (e21 = op2(unit2, e21)), inference(cnf_transformation, [], [f5])).
fof(f1630, plain, (~ spl12_220 | ~ spl12_221 | ~ spl12_222 | ~ spl12_223 | spl12_167 | spl12_164 | spl12_161 | ~ spl12_224 | ~ spl12_225 | ~ spl12_226 | ~ spl12_227 | ~ spl12_228 | ~ spl12_229 | ~ spl12_230 | ~ spl12_231 | ~ spl12_232 | ~ spl12_233 | ~ spl12_234 | ~ spl12_235 | ~ spl12_236), inference(avatar_split_clause, [], [f1561, f1627, f1623, f1619, f1615, f1611, f1607, f1603, f1599, f1595, f1591, f1587, f1583, f1579, f1203, f1220, f1237, f1575, f1571, f1567, f1563])).
fof(f1237, plain, (spl12_167 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl12_167])])).
fof(f1220, plain, (spl12_164 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl12_164])])).
fof(f1203, plain, (spl12_161 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl12_161])])).
fof(f1561, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | ~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e13) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1560, f306])).
fof(f1560, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e22)) | ~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e13) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1559, f1055])).
fof(f1559, plain, (~ (h2(op1(e11, e10)) = op2(e21, h2(e10))) | ~ (h2(e13) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1558, f306])).
fof(f1558, plain, (~ (h2(e13) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1557, f309])).
fof(f1557, plain, (~ (op2(e21, e21) = h2(op1(e11, e11))) | ~ (h2(e10) = h2(op1(e11, e12))) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1556, f306])).
fof(f1556, plain, (~ (h2(e10) = h2(op1(e11, e12))) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1555, f1057])).
fof(f1555, plain, (~ (op2(e21, e22) = h2(op1(e11, e12))) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1554, f306])).
fof(f1554, plain, (~ (h2(op1(e11, e12)) = op2(h2(e11), e22)) | ~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1553, f1055])).
fof(f1553, plain, (~ (e22 = h2(op1(e11, e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1552, f1053])).
fof(f1552, plain, (~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1551, f306])).
fof(f1551, plain, (~ (h2(op1(e12, e10)) = op2(e22, h2(e10))) | ~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1550, f1055])).
fof(f1550, plain, (~ (op2(e22, e21) = h2(op1(e12, e11))) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1549, f1055])).
fof(f1549, plain, (~ (h2(op1(e12, e11)) = op2(h2(e12), e21)) | ~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1548, f306])).
fof(f1548, plain, (~ (h3(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1547, f313])).
fof(f1547, plain, (~ (op2(e22, e22) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1546, f1055])).
fof(f1546, plain, (~ (h2(op1(e12, e13)) = op2(e22, h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1545, f1055])).
fof(f1545, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1544, f306])).
fof(f1544, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | ~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f373, f1055])).
fof(f373, plain, (~ (e23 = h2(e13)) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(cnf_transformation, [], [f37])).
fof(f37, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP11 | sP10 | sP9 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP8 | sP7 | sP6 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP5 | sP4 | sP3 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP2 | sP1 | sP0 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f24, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25])).
fof(f25, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP0), inference(usedef, [], [e25])).
fof(e25, plain, (sP0 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f26, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP1), inference(usedef, [], [e26])).
fof(e26, plain, (sP1 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f27, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP2), inference(usedef, [], [e27])).
fof(e27, plain, (sP2 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f28, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP3), inference(usedef, [], [e28])).
fof(e28, plain, (sP3 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f29, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP4), inference(usedef, [], [e29])).
fof(e29, plain, (sP4 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f30, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP5), inference(usedef, [], [e30])).
fof(e30, plain, (sP5 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f31, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP6), inference(usedef, [], [e31])).
fof(e31, plain, (sP6 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f32, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP7), inference(usedef, [], [e32])).
fof(e32, plain, (sP7 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f33, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP8), inference(usedef, [], [e33])).
fof(e33, plain, (sP8 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f34, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP9), inference(usedef, [], [e34])).
fof(e34, plain, (sP9 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f35, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP10), inference(usedef, [], [e35])).
fof(e35, plain, (sP10 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f36, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP11), inference(usedef, [], [e36])).
fof(e36, plain, (sP11 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f24, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f20])).
fof(f20, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', co1)).
fof(f1251, plain, (~ spl12_167 | ~ spl12_169), inference(avatar_split_clause, [], [f350, f1248, f1237])).
fof(f350, plain, (~ (e20 = h2(e10)) | ~ sP3), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP3), inference(nnf_transformation, [], [f28])).
fof(f1230, plain, ~ spl12_164, inference(avatar_split_clause, [], [f1229, f1220])).
fof(f1229, plain, ~ sP4, inference(subsumption_resolution, [], [f347, f306])).
fof(f347, plain, (~ (e21 = h2(e11)) | ~ sP4), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP4), inference(nnf_transformation, [], [f29])).
fof(f1212, plain, ~ spl12_161, inference(avatar_split_clause, [], [f1211, f1203])).
fof(f1211, plain, ~ sP5, inference(subsumption_resolution, [], [f344, f1055])).
fof(f344, plain, (~ (e22 = h2(e12)) | ~ sP5), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP5), inference(nnf_transformation, [], [f30])).
fof(f1037, plain, spl12_105, inference(avatar_split_clause, [], [f1036, f857])).
fof(f1036, plain, (e20 = op2(e21, e22)), inference(forward_demodulation, [], [f299, f300])).
fof(f299, plain, (e20 = op2(e21, op2(e21, op2(e21, e21)))), inference(cnf_transformation, [], [f15])).
fof(f1035, plain, spl12_112, inference(avatar_split_clause, [], [f301, f886])).
fof(f301, plain, (e23 = op2(e21, e21)), inference(cnf_transformation, [], [f15])).
fof(f1034, plain, spl12_37, inference(avatar_split_clause, [], [f1033, f536])).
fof(f1033, plain, (e10 = op1(e11, e12)), inference(forward_demodulation, [], [f296, f297])).
fof(f296, plain, (e10 = op1(e11, op1(e11, op1(e11, e11)))), inference(cnf_transformation, [], [f14])).
fof(f1032, plain, spl12_44, inference(avatar_split_clause, [], [f298, f565])).
fof(f298, plain, (e13 = op1(e11, e11)), inference(cnf_transformation, [], [f14])).
fof(f1004, plain, (spl12_122 | spl12_106 | spl12_90 | spl12_74), inference(avatar_split_clause, [], [f151, f725, f793, f861, f929])).
fof(f151, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax6)).
fof(f1001, plain, (spl12_100 | spl12_96 | spl12_92 | spl12_88), inference(avatar_split_clause, [], [f154, f784, f801, f818, f835])).
fof(f154, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f6])).
fof(f999, plain, (spl12_81 | spl12_77 | spl12_73 | spl12_69), inference(avatar_split_clause, [], [f156, f704, f721, f738, f755])).
fof(f156, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f997, plain, (spl12_82 | spl12_78 | spl12_74 | spl12_70), inference(avatar_split_clause, [], [f158, f708, f725, f742, f759])).
fof(f158, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f996, plain, (spl12_118 | spl12_102 | spl12_86 | spl12_70), inference(avatar_split_clause, [], [f159, f708, f776, f844, f912])).
fof(f159, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))), inference(cnf_transformation, [], [f6])).
fof(f995, plain, (spl12_83 | spl12_79 | spl12_75 | spl12_71), inference(avatar_split_clause, [], [f160, f712, f729, f746, f763])).
fof(f160, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))), inference(cnf_transformation, [], [f6])).
fof(f991, plain, (spl12_133 | spl12_134 | spl12_135 | spl12_136), inference(avatar_split_clause, [], [f131, f988, f984, f980, f976])).
fof(f131, plain, ((e23 = unit2) | (e22 = unit2) | (e21 = unit2) | (e20 = unit2)), inference(cnf_transformation, [], [f5])).
fof(f821, plain, (spl12_93 | spl12_94 | spl12_95 | spl12_96), inference(avatar_split_clause, [], [f116, f818, f814, f810, f806])).
fof(f116, plain, ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax4)).
fof(f787, plain, (spl12_85 | spl12_86 | spl12_87 | spl12_88), inference(avatar_split_clause, [], [f118, f784, f780, f776, f772])).
fof(f118, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f4])).
fof(f686, plain, (spl12_29 | spl12_25 | spl12_21 | spl12_17), inference(avatar_split_clause, [], [f91, f451, f468, f485, f502])).
fof(f91, plain, ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG038+1.p', ax3)).
fof(f684, plain, (spl12_30 | spl12_26 | spl12_22 | spl12_18), inference(avatar_split_clause, [], [f93, f455, f472, f489, f506])).
fof(f93, plain, ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))), inference(cnf_transformation, [], [f3])).
fof(f683, plain, (spl12_54 | spl12_38 | spl12_22 | spl12_6), inference(avatar_split_clause, [], [f94, f404, f472, f540, f608])).
fof(f94, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f3])).
fof(f679, plain, (spl12_56 | spl12_40 | spl12_24 | spl12_8), inference(avatar_split_clause, [], [f98, f412, f480, f548, f616])).
fof(f98, plain, ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))), inference(cnf_transformation, [], [f3])).
fof(f678, plain, (spl12_13 | spl12_9 | spl12_5 | spl12_1), inference(avatar_split_clause, [], [f99, f383, f400, f417, f434])).
fof(f99, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f3])).
fof(f674, plain, (spl12_15 | spl12_11 | spl12_7 | spl12_3), inference(avatar_split_clause, [], [f103, f391, f408, f425, f442])).
fof(f103, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f3])).
fof(f670, plain, (spl12_65 | spl12_66 | spl12_67 | spl12_68), inference(avatar_split_clause, [], [f74, f667, f663, f659, f655])).
fof(f74, plain, ((e13 = unit1) | (e12 = unit1) | (e11 = unit1) | (e10 = unit1)), inference(cnf_transformation, [], [f2])).