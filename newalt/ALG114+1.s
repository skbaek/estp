fof(f3082, plain, $false, inference(avatar_sat_refutation, [], [f517, f534, f568, f602, f619, f725, f728, f738, f750, f751, f752, f821, f838, f889, f906, f991, f1008, f1036, f1044, f1046, f1047, f1057, f1069, f1074, f1080, f1081, f1091, f1093, f1101, f1112, f1113, f1125, f1136, f1137, f1149, f1169, f1170, f1171, f1321, f1337, f1356, f1757, f1870, f1891, f1938, f1945, f1948, f1958, f1995, f2029, f2049, f2066, f2071, f2096, f2097, f2098, f2100, f2110, f2142, f2146, f2148, f2152, f2206, f2208, f2210, f2213, f2232, f2254, f2291, f2295, f2303, f2324, f2333, f2361, f2362, f2368, f2393, f2400, f2421, f2447, f2454, f2469, f2494, f2504, f2513, f2517, f2526, f2530, f2534, f2538, f2560, f2571, f2593, f2605, f2610, f2628, f2629, f2631, f2636, f2644, f2652, f2659, f2667, f2687, f2724, f2728, f2738, f2782, f2785, f2797, f2837, f2857, f2868, f2877, f2895, f2911, f2925, f2936, f2946, f2955, f2968, f2985, f3001, f3016, f3047, f3055, f3063, f3079])).
fof(f3079, plain, (~ spl20_87 | ~ spl20_88), inference(avatar_contradiction_clause, [], [f3078])).
fof(f3078, plain, ($false | (~ spl20_87 | ~ spl20_88)), inference(subsumption_resolution, [], [f3077, f267])).
fof(f267, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax8)).
fof(f3077, plain, ((e22 = e23) | (~ spl20_87 | ~ spl20_88)), inference(backward_demodulation, [], [f854, f850])).
fof(f850, plain, ((e22 = op2(e22, e22)) | ~ spl20_87), inference(avatar_component_clause, [], [f848])).
fof(f848, plain, (spl20_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_87])])).
fof(f854, plain, ((e23 = op2(e22, e22)) | ~ spl20_88), inference(avatar_component_clause, [], [f852])).
fof(f852, plain, (spl20_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_88])])).
fof(f3063, plain, (~ spl20_83 | ~ spl20_46 | ~ spl20_157 | spl20_218 | ~ spl20_233), inference(avatar_split_clause, [], [f3062, f1754, f1671, f1318, f642, f831])).
fof(f831, plain, (spl20_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_83])])).
fof(f642, plain, (spl20_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_46])])).
fof(f1318, plain, (spl20_157 <=> (e22 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_157])])).
fof(f1671, plain, (spl20_218 <=> (h2(op1(e11, e10)) = op2(h2(e11), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_218])])).
fof(f1754, plain, (spl20_233 <=> (e23 = h2(e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_233])])).
fof(f3062, plain, (~ (e22 = op2(e22, e23)) | (~ spl20_46 | ~ spl20_157 | spl20_218 | ~ spl20_233)), inference(forward_demodulation, [], [f3061, f1319])).
fof(f1319, plain, ((e22 = h2(e11)) | ~ spl20_157), inference(avatar_component_clause, [], [f1318])).
fof(f3061, plain, (~ (op2(e22, e23) = h2(e11)) | (~ spl20_46 | ~ spl20_157 | spl20_218 | ~ spl20_233)), inference(forward_demodulation, [], [f3060, f644])).
fof(f644, plain, ((e11 = op1(e11, e10)) | ~ spl20_46), inference(avatar_component_clause, [], [f642])).
fof(f3060, plain, (~ (op2(e22, e23) = h2(op1(e11, e10))) | (~ spl20_157 | spl20_218 | ~ spl20_233)), inference(forward_demodulation, [], [f3059, f1319])).
fof(f3059, plain, (~ (h2(op1(e11, e10)) = op2(h2(e11), e23)) | (spl20_218 | ~ spl20_233)), inference(forward_demodulation, [], [f1673, f1755])).
fof(f1755, plain, ((e23 = h2(e10)) | ~ spl20_233), inference(avatar_component_clause, [], [f1754])).
fof(f1673, plain, (~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | spl20_218), inference(avatar_component_clause, [], [f1671])).
fof(f3055, plain, (~ spl20_82 | ~ spl20_142), inference(avatar_split_clause, [], [f2491, f1233, f827])).
fof(f827, plain, (spl20_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_82])])).
fof(f1233, plain, (spl20_142 <=> (e21 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_142])])).
fof(f2491, plain, (~ (e21 = op2(e22, e23)) | ~ spl20_142), inference(forward_demodulation, [], [f1202, f1234])).
fof(f1234, plain, ((e21 = h4(e13)) | ~ spl20_142), inference(avatar_component_clause, [], [f1233])).
fof(f1202, plain, ~ (op2(e22, e23) = h4(e13)), inference(backward_demodulation, [], [f231, f385])).
fof(f385, plain, (op2(e23, e23) = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, e23) = h4(e13)) & (h4(e11) = op2(op2(e23, e23), op2(e23, e23))) & (h4(e10) = op2(op2(e23, e23), e23)) & (e23 = h4(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax17)).
fof(f231, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax6)).
fof(f3047, plain, (~ spl20_60 | ~ spl20_69 | ~ spl20_157 | ~ spl20_164 | spl20_216 | ~ spl20_233), inference(avatar_contradiction_clause, [], [f3046])).
fof(f3046, plain, ($false | (~ spl20_60 | ~ spl20_69 | ~ spl20_157 | ~ spl20_164 | spl20_216 | ~ spl20_233)), inference(subsumption_resolution, [], [f3045, f774])).
fof(f774, plain, ((e20 = op2(e23, e22)) | ~ spl20_69), inference(avatar_component_clause, [], [f772])).
fof(f772, plain, (spl20_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_69])])).
fof(f3045, plain, (~ (e20 = op2(e23, e22)) | (~ spl20_60 | ~ spl20_157 | ~ spl20_164 | spl20_216 | ~ spl20_233)), inference(forward_demodulation, [], [f3044, f1354])).
fof(f1354, plain, ((e20 = h2(e13)) | ~ spl20_164), inference(avatar_component_clause, [], [f1353])).
fof(f1353, plain, (spl20_164 <=> (e20 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_164])])).
fof(f3044, plain, (~ (op2(e23, e22) = h2(e13)) | (~ spl20_60 | ~ spl20_157 | spl20_216 | ~ spl20_233)), inference(forward_demodulation, [], [f3043, f703])).
fof(f703, plain, ((e13 = op1(e10, e11)) | ~ spl20_60), inference(avatar_component_clause, [], [f701])).
fof(f701, plain, (spl20_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_60])])).
fof(f3043, plain, (~ (op2(e23, e22) = h2(op1(e10, e11))) | (~ spl20_157 | spl20_216 | ~ spl20_233)), inference(forward_demodulation, [], [f3042, f1755])).
fof(f3042, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e22)) | (~ spl20_157 | spl20_216)), inference(forward_demodulation, [], [f1665, f1319])).
fof(f1665, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | spl20_216), inference(avatar_component_clause, [], [f1663])).
fof(f1663, plain, (spl20_216 <=> (h2(op1(e10, e11)) = op2(h2(e10), h2(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_216])])).
fof(f3016, plain, (~ spl20_41 | ~ spl20_88 | ~ spl20_157 | spl20_219 | ~ spl20_233), inference(avatar_contradiction_clause, [], [f3015])).
fof(f3015, plain, ($false | (~ spl20_41 | ~ spl20_88 | ~ spl20_157 | spl20_219 | ~ spl20_233)), inference(subsumption_resolution, [], [f3014, f854])).
fof(f3014, plain, (~ (e23 = op2(e22, e22)) | (~ spl20_41 | ~ spl20_157 | spl20_219 | ~ spl20_233)), inference(forward_demodulation, [], [f3013, f1755])).
fof(f3013, plain, (~ (op2(e22, e22) = h2(e10)) | (~ spl20_41 | ~ spl20_157 | spl20_219)), inference(forward_demodulation, [], [f3012, f623])).
fof(f623, plain, ((e10 = op1(e11, e11)) | ~ spl20_41), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl20_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_41])])).
fof(f3012, plain, (~ (op2(e22, e22) = h2(op1(e11, e11))) | (~ spl20_157 | spl20_219)), inference(forward_demodulation, [], [f1677, f1319])).
fof(f1677, plain, (~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | spl20_219), inference(avatar_component_clause, [], [f1675])).
fof(f1675, plain, (spl20_219 <=> (h2(op1(e11, e11)) = op2(h2(e11), h2(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_219])])).
fof(f3001, plain, (~ spl20_36 | ~ spl20_93 | ~ spl20_157 | ~ spl20_164 | spl20_220), inference(avatar_contradiction_clause, [], [f3000])).
fof(f3000, plain, ($false | (~ spl20_36 | ~ spl20_93 | ~ spl20_157 | ~ spl20_164 | spl20_220)), inference(subsumption_resolution, [], [f2999, f876])).
fof(f876, plain, ((e20 = op2(e22, e20)) | ~ spl20_93), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl20_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_93])])).
fof(f2999, plain, (~ (e20 = op2(e22, e20)) | (~ spl20_36 | ~ spl20_157 | ~ spl20_164 | spl20_220)), inference(forward_demodulation, [], [f2998, f1354])).
fof(f2998, plain, (~ (op2(e22, e20) = h2(e13)) | (~ spl20_36 | ~ spl20_157 | ~ spl20_164 | spl20_220)), inference(forward_demodulation, [], [f2997, f601])).
fof(f601, plain, ((e13 = op1(e11, e13)) | ~ spl20_36), inference(avatar_component_clause, [], [f599])).
fof(f599, plain, (spl20_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_36])])).
fof(f2997, plain, (~ (op2(e22, e20) = h2(op1(e11, e13))) | (~ spl20_157 | ~ spl20_164 | spl20_220)), inference(forward_demodulation, [], [f2996, f1319])).
fof(f2996, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), e20)) | (~ spl20_164 | spl20_220)), inference(forward_demodulation, [], [f1681, f1354])).
fof(f1681, plain, (~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | spl20_220), inference(avatar_component_clause, [], [f1679])).
fof(f1679, plain, (spl20_220 <=> (h2(op1(e11, e13)) = op2(h2(e11), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_220])])).
fof(f2985, plain, (~ spl20_16 | ~ spl20_113 | ~ spl20_164 | spl20_221 | ~ spl20_233), inference(avatar_contradiction_clause, [], [f2984])).
fof(f2984, plain, ($false | (~ spl20_16 | ~ spl20_113 | ~ spl20_164 | spl20_221 | ~ spl20_233)), inference(subsumption_resolution, [], [f2983, f961])).
fof(f961, plain, ((e20 = op2(e20, e23)) | ~ spl20_113), inference(avatar_component_clause, [], [f959])).
fof(f959, plain, (spl20_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_113])])).
fof(f2983, plain, (~ (e20 = op2(e20, e23)) | (~ spl20_16 | ~ spl20_164 | spl20_221 | ~ spl20_233)), inference(forward_demodulation, [], [f2982, f1354])).
fof(f2982, plain, (~ (op2(e20, e23) = h2(e13)) | (~ spl20_16 | ~ spl20_164 | spl20_221 | ~ spl20_233)), inference(forward_demodulation, [], [f2981, f516])).
fof(f516, plain, ((e13 = op1(e13, e10)) | ~ spl20_16), inference(avatar_component_clause, [], [f514])).
fof(f514, plain, (spl20_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_16])])).
fof(f2981, plain, (~ (op2(e20, e23) = h2(op1(e13, e10))) | (~ spl20_164 | spl20_221 | ~ spl20_233)), inference(forward_demodulation, [], [f2980, f1354])).
fof(f2980, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), e23)) | (spl20_221 | ~ spl20_233)), inference(forward_demodulation, [], [f1685, f1755])).
fof(f1685, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | spl20_221), inference(avatar_component_clause, [], [f1683])).
fof(f1683, plain, (spl20_221 <=> (h2(op1(e13, e10)) = op2(h2(e13), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_221])])).
fof(f2968, plain, (~ spl20_11 | ~ spl20_118 | ~ spl20_157 | ~ spl20_164 | spl20_222), inference(avatar_contradiction_clause, [], [f2967])).
fof(f2967, plain, ($false | (~ spl20_11 | ~ spl20_118 | ~ spl20_157 | ~ spl20_164 | spl20_222)), inference(subsumption_resolution, [], [f2966, f982])).
fof(f982, plain, ((e21 = op2(e20, e22)) | ~ spl20_118), inference(avatar_component_clause, [], [f980])).
fof(f980, plain, (spl20_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_118])])).
fof(f2966, plain, (~ (e21 = op2(e20, e22)) | (~ spl20_11 | ~ spl20_157 | ~ spl20_164 | spl20_222)), inference(forward_demodulation, [], [f2965, f374])).
fof(f374, plain, (e21 = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, e21) = h2(e13)) & (h2(e11) = op2(op2(e21, e21), op2(e21, e21))) & (h2(e10) = op2(op2(e21, e21), e21)) & (e21 = h2(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax15)).
fof(f2965, plain, (~ (op2(e20, e22) = h2(e12)) | (~ spl20_11 | ~ spl20_157 | ~ spl20_164 | spl20_222)), inference(forward_demodulation, [], [f2964, f495])).
fof(f495, plain, ((e12 = op1(e13, e11)) | ~ spl20_11), inference(avatar_component_clause, [], [f493])).
fof(f493, plain, (spl20_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_11])])).
fof(f2964, plain, (~ (op2(e20, e22) = h2(op1(e13, e11))) | (~ spl20_157 | ~ spl20_164 | spl20_222)), inference(forward_demodulation, [], [f2963, f1354])).
fof(f2963, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e22)) | (~ spl20_157 | spl20_222)), inference(forward_demodulation, [], [f1689, f1319])).
fof(f1689, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | spl20_222), inference(avatar_component_clause, [], [f1687])).
fof(f1687, plain, (spl20_222 <=> (h2(op1(e13, e11)) = op2(h2(e13), h2(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_222])])).
fof(f2955, plain, (~ spl20_2 | spl20_224), inference(avatar_contradiction_clause, [], [f2954])).
fof(f2954, plain, ($false | (~ spl20_2 | spl20_224)), inference(trivial_inequality_removal, [], [f2953])).
fof(f2953, plain, (~ (h2(e11) = h2(e11)) | (~ spl20_2 | spl20_224)), inference(forward_demodulation, [], [f1697, f457])).
fof(f457, plain, ((e11 = op1(e13, e13)) | ~ spl20_2), inference(avatar_component_clause, [], [f455])).
fof(f455, plain, (spl20_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_2])])).
fof(f1697, plain, (~ (h2(e11) = h2(op1(e13, e13))) | spl20_224), inference(avatar_component_clause, [], [f1695])).
fof(f1695, plain, (spl20_224 <=> (h2(e11) = h2(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_224])])).
fof(f2946, plain, (~ spl20_5 | spl20_225), inference(avatar_contradiction_clause, [], [f2945])).
fof(f2945, plain, ($false | (~ spl20_5 | spl20_225)), inference(trivial_inequality_removal, [], [f2944])).
fof(f2944, plain, (~ (h2(e10) = h2(e10)) | (~ spl20_5 | spl20_225)), inference(forward_demodulation, [], [f1701, f470])).
fof(f470, plain, ((e10 = op1(e13, e12)) | ~ spl20_5), inference(avatar_component_clause, [], [f468])).
fof(f468, plain, (spl20_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_5])])).
fof(f1701, plain, (~ (h2(e10) = h2(op1(e13, e12))) | spl20_225), inference(avatar_component_clause, [], [f1699])).
fof(f1699, plain, (spl20_225 <=> (h2(e10) = h2(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_225])])).
fof(f2936, plain, (~ spl20_19 | ~ spl20_110 | ~ spl20_164 | spl20_226), inference(avatar_contradiction_clause, [], [f2935])).
fof(f2935, plain, ($false | (~ spl20_19 | ~ spl20_110 | ~ spl20_164 | spl20_226)), inference(subsumption_resolution, [], [f2934, f948])).
fof(f948, plain, ((e21 = op2(e21, e20)) | ~ spl20_110), inference(avatar_component_clause, [], [f946])).
fof(f946, plain, (spl20_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_110])])).
fof(f2934, plain, (~ (e21 = op2(e21, e20)) | (~ spl20_19 | ~ spl20_164 | spl20_226)), inference(forward_demodulation, [], [f2933, f374])).
fof(f2933, plain, (~ (op2(e21, e20) = h2(e12)) | (~ spl20_19 | ~ spl20_164 | spl20_226)), inference(forward_demodulation, [], [f2932, f529])).
fof(f529, plain, ((e12 = op1(e12, e13)) | ~ spl20_19), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl20_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_19])])).
fof(f2932, plain, (~ (op2(e21, e20) = h2(op1(e12, e13))) | (~ spl20_164 | spl20_226)), inference(forward_demodulation, [], [f1705, f1354])).
fof(f1705, plain, (~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | spl20_226), inference(avatar_component_clause, [], [f1703])).
fof(f1703, plain, (spl20_226 <=> (h2(op1(e12, e13)) = op2(e21, h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_226])])).
fof(f2925, plain, (~ spl20_24 | spl20_227), inference(avatar_contradiction_clause, [], [f2924])).
fof(f2924, plain, ($false | (~ spl20_24 | spl20_227)), inference(trivial_inequality_removal, [], [f2923])).
fof(f2923, plain, (~ (h2(e13) = h2(e13)) | (~ spl20_24 | spl20_227)), inference(forward_demodulation, [], [f1709, f550])).
fof(f550, plain, ((e13 = op1(e12, e12)) | ~ spl20_24), inference(avatar_component_clause, [], [f548])).
fof(f548, plain, (spl20_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_24])])).
fof(f1709, plain, (~ (h2(e13) = h2(op1(e12, e12))) | spl20_227), inference(avatar_component_clause, [], [f1707])).
fof(f1707, plain, (spl20_227 <=> (h2(e13) = h2(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_227])])).
fof(f2911, plain, (~ spl20_26 | ~ spl20_103 | ~ spl20_157 | spl20_228), inference(avatar_contradiction_clause, [], [f2910])).
fof(f2910, plain, ($false | (~ spl20_26 | ~ spl20_103 | ~ spl20_157 | spl20_228)), inference(subsumption_resolution, [], [f2909, f918])).
fof(f918, plain, ((e22 = op2(e21, e22)) | ~ spl20_103), inference(avatar_component_clause, [], [f916])).
fof(f916, plain, (spl20_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_103])])).
fof(f2909, plain, (~ (e22 = op2(e21, e22)) | (~ spl20_26 | ~ spl20_157 | spl20_228)), inference(forward_demodulation, [], [f2908, f1319])).
fof(f2908, plain, (~ (op2(e21, e22) = h2(e11)) | (~ spl20_26 | ~ spl20_157 | spl20_228)), inference(forward_demodulation, [], [f2907, f559])).
fof(f559, plain, ((e11 = op1(e12, e11)) | ~ spl20_26), inference(avatar_component_clause, [], [f557])).
fof(f557, plain, (spl20_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_26])])).
fof(f2907, plain, (~ (op2(e21, e22) = h2(op1(e12, e11))) | (~ spl20_157 | spl20_228)), inference(forward_demodulation, [], [f1713, f1319])).
fof(f1713, plain, (~ (h2(op1(e12, e11)) = op2(e21, h2(e11))) | spl20_228), inference(avatar_component_clause, [], [f1711])).
fof(f1711, plain, (spl20_228 <=> (h2(op1(e12, e11)) = op2(e21, h2(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_228])])).
fof(f2895, plain, (~ spl20_29 | ~ spl20_100 | spl20_229 | ~ spl20_233), inference(avatar_contradiction_clause, [], [f2894])).
fof(f2894, plain, ($false | (~ spl20_29 | ~ spl20_100 | spl20_229 | ~ spl20_233)), inference(subsumption_resolution, [], [f2893, f905])).
fof(f905, plain, ((e23 = op2(e21, e23)) | ~ spl20_100), inference(avatar_component_clause, [], [f903])).
fof(f903, plain, (spl20_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_100])])).
fof(f2893, plain, (~ (e23 = op2(e21, e23)) | (~ spl20_29 | spl20_229 | ~ spl20_233)), inference(forward_demodulation, [], [f2892, f1755])).
fof(f2892, plain, (~ (op2(e21, e23) = h2(e10)) | (~ spl20_29 | spl20_229 | ~ spl20_233)), inference(forward_demodulation, [], [f2891, f572])).
fof(f572, plain, ((e10 = op1(e12, e10)) | ~ spl20_29), inference(avatar_component_clause, [], [f570])).
fof(f570, plain, (spl20_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_29])])).
fof(f2891, plain, (~ (op2(e21, e23) = h2(op1(e12, e10))) | (spl20_229 | ~ spl20_233)), inference(forward_demodulation, [], [f1717, f1755])).
fof(f1717, plain, (~ (h2(op1(e12, e10)) = op2(e21, h2(e10))) | spl20_229), inference(avatar_component_clause, [], [f1715])).
fof(f1715, plain, (spl20_229 <=> (h2(op1(e12, e10)) = op2(e21, h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_229])])).
fof(f2877, plain, (~ spl20_39 | ~ spl20_90 | ~ spl20_157 | spl20_230), inference(avatar_contradiction_clause, [], [f2876])).
fof(f2876, plain, ($false | (~ spl20_39 | ~ spl20_90 | ~ spl20_157 | spl20_230)), inference(subsumption_resolution, [], [f2875, f863])).
fof(f863, plain, ((e21 = op2(e22, e21)) | ~ spl20_90), inference(avatar_component_clause, [], [f861])).
fof(f861, plain, (spl20_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_90])])).
fof(f2875, plain, (~ (e21 = op2(e22, e21)) | (~ spl20_39 | ~ spl20_157 | spl20_230)), inference(forward_demodulation, [], [f2874, f374])).
fof(f2874, plain, (~ (op2(e22, e21) = h2(e12)) | (~ spl20_39 | ~ spl20_157 | spl20_230)), inference(forward_demodulation, [], [f2873, f614])).
fof(f614, plain, ((e12 = op1(e11, e12)) | ~ spl20_39), inference(avatar_component_clause, [], [f612])).
fof(f612, plain, (spl20_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_39])])).
fof(f2873, plain, (~ (op2(e22, e21) = h2(op1(e11, e12))) | (~ spl20_157 | spl20_230)), inference(forward_demodulation, [], [f1721, f1319])).
fof(f1721, plain, (~ (h2(op1(e11, e12)) = op2(h2(e11), e21)) | spl20_230), inference(avatar_component_clause, [], [f1719])).
fof(f1719, plain, (spl20_230 <=> (h2(op1(e11, e12)) = op2(h2(e11), e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_230])])).
fof(f2868, plain, (~ spl20_49 | ~ spl20_80 | ~ spl20_164 | spl20_217 | ~ spl20_233), inference(avatar_contradiction_clause, [], [f2867])).
fof(f2867, plain, ($false | (~ spl20_49 | ~ spl20_80 | ~ spl20_164 | spl20_217 | ~ spl20_233)), inference(subsumption_resolution, [], [f2866, f820])).
fof(f820, plain, ((e23 = op2(e23, e20)) | ~ spl20_80), inference(avatar_component_clause, [], [f818])).
fof(f818, plain, (spl20_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_80])])).
fof(f2866, plain, (~ (e23 = op2(e23, e20)) | (~ spl20_49 | ~ spl20_164 | spl20_217 | ~ spl20_233)), inference(forward_demodulation, [], [f2865, f1755])).
fof(f2865, plain, (~ (op2(e23, e20) = h2(e10)) | (~ spl20_49 | ~ spl20_164 | spl20_217 | ~ spl20_233)), inference(forward_demodulation, [], [f2864, f657])).
fof(f657, plain, ((e10 = op1(e10, e13)) | ~ spl20_49), inference(avatar_component_clause, [], [f655])).
fof(f655, plain, (spl20_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_49])])).
fof(f2864, plain, (~ (op2(e23, e20) = h2(op1(e10, e13))) | (~ spl20_164 | spl20_217 | ~ spl20_233)), inference(forward_demodulation, [], [f2863, f1755])).
fof(f2863, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), e20)) | (~ spl20_164 | spl20_217)), inference(forward_demodulation, [], [f1669, f1354])).
fof(f1669, plain, (~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | spl20_217), inference(avatar_component_clause, [], [f1667])).
fof(f1667, plain, (spl20_217 <=> (h2(op1(e10, e13)) = op2(h2(e10), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_217])])).
fof(f2857, plain, (~ spl20_17 | ~ spl20_29), inference(avatar_split_clause, [], [f2854, f570, f519])).
fof(f519, plain, (spl20_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_17])])).
fof(f2854, plain, (~ (e10 = op1(e12, e13)) | ~ spl20_29), inference(backward_demodulation, [], [f198, f572])).
fof(f198, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax5)).
fof(f2837, plain, (~ spl20_157 | ~ spl20_54 | ~ spl20_75 | spl20_231 | ~ spl20_233), inference(avatar_split_clause, [], [f2836, f1754, f1723, f797, f676, f1318])).
fof(f676, plain, (spl20_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_54])])).
fof(f797, plain, (spl20_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_75])])).
fof(f1723, plain, (spl20_231 <=> (h2(op1(e10, e12)) = op2(h2(e10), e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_231])])).
fof(f2836, plain, (~ (e22 = h2(e11)) | (~ spl20_54 | ~ spl20_75 | spl20_231 | ~ spl20_233)), inference(forward_demodulation, [], [f2835, f799])).
fof(f799, plain, ((e22 = op2(e23, e21)) | ~ spl20_75), inference(avatar_component_clause, [], [f797])).
fof(f2835, plain, (~ (op2(e23, e21) = h2(e11)) | (~ spl20_54 | spl20_231 | ~ spl20_233)), inference(forward_demodulation, [], [f2834, f678])).
fof(f678, plain, ((e11 = op1(e10, e12)) | ~ spl20_54), inference(avatar_component_clause, [], [f676])).
fof(f2834, plain, (~ (op2(e23, e21) = h2(op1(e10, e12))) | (spl20_231 | ~ spl20_233)), inference(forward_demodulation, [], [f1725, f1755])).
fof(f1725, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e21)) | spl20_231), inference(avatar_component_clause, [], [f1723])).
fof(f2797, plain, (~ spl20_61 | ~ spl20_63), inference(avatar_contradiction_clause, [], [f2796])).
fof(f2796, plain, ($false | (~ spl20_61 | ~ spl20_63)), inference(subsumption_resolution, [], [f2795, f257])).
fof(f257, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax7)).
fof(f2795, plain, ((e10 = e12) | (~ spl20_61 | ~ spl20_63)), inference(backward_demodulation, [], [f716, f708])).
fof(f708, plain, ((e10 = op1(e10, e10)) | ~ spl20_61), inference(avatar_component_clause, [], [f706])).
fof(f706, plain, (spl20_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_61])])).
fof(f716, plain, ((op1(e10, e10) = e12) | ~ spl20_63), inference(avatar_component_clause, [], [f714])).
fof(f714, plain, (spl20_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl20_63])])).
fof(f2785, plain, (~ spl20_63 | ~ spl20_66 | spl20_215 | ~ spl20_233), inference(avatar_contradiction_clause, [], [f2784])).
fof(f2784, plain, ($false | (~ spl20_63 | ~ spl20_66 | spl20_215 | ~ spl20_233)), inference(subsumption_resolution, [], [f2783, f761])).
fof(f761, plain, ((e21 = op2(e23, e23)) | ~ spl20_66), inference(avatar_component_clause, [], [f759])).
fof(f759, plain, (spl20_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_66])])).
fof(f2783, plain, (~ (e21 = op2(e23, e23)) | (~ spl20_63 | spl20_215 | ~ spl20_233)), inference(backward_demodulation, [], [f2704, f1755])).
fof(f2704, plain, (~ (e21 = op2(h2(e10), h2(e10))) | (~ spl20_63 | spl20_215)), inference(forward_demodulation, [], [f2701, f374])).
fof(f2701, plain, (~ (h2(e12) = op2(h2(e10), h2(e10))) | (~ spl20_63 | spl20_215)), inference(backward_demodulation, [], [f1661, f716])).
fof(f1661, plain, (~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10))) | spl20_215), inference(avatar_component_clause, [], [f1659])).
fof(f1659, plain, (spl20_215 <=> (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_215])])).
fof(f2782, plain, (spl20_157 | ~ spl20_127 | ~ spl20_164), inference(avatar_split_clause, [], [f2781, f1353, f1018, f1318])).
fof(f1018, plain, (spl20_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl20_127])])).
fof(f2781, plain, ((e22 = h2(e11)) | (~ spl20_127 | ~ spl20_164)), inference(forward_demodulation, [], [f2780, f1020])).
fof(f1020, plain, ((op2(e20, e20) = e22) | ~ spl20_127), inference(avatar_component_clause, [], [f1018])).
fof(f2780, plain, ((op2(e20, e20) = h2(e11)) | ~ spl20_164), inference(forward_demodulation, [], [f1186, f1354])).
fof(f1186, plain, (h2(e11) = op2(h2(e13), h2(e13))), inference(forward_demodulation, [], [f376, f377])).
fof(f377, plain, (op2(e21, e21) = h2(e13)), inference(cnf_transformation, [], [f15])).
fof(f376, plain, (h2(e11) = op2(op2(e21, e21), op2(e21, e21))), inference(cnf_transformation, [], [f15])).
fof(f2738, plain, (~ spl20_15 | ~ spl20_63), inference(avatar_split_clause, [], [f2737, f714, f510])).
fof(f510, plain, (spl20_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_15])])).
fof(f2737, plain, (~ (e12 = op1(e13, e10)) | ~ spl20_63), inference(forward_demodulation, [], [f162, f716])).
fof(f162, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2728, plain, (~ spl20_19 | ~ spl20_27), inference(avatar_split_clause, [], [f2726, f561, f527])).
fof(f561, plain, (spl20_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_27])])).
fof(f2726, plain, (~ (e12 = op1(e12, e13)) | ~ spl20_27), inference(backward_demodulation, [], [f200, f563])).
fof(f563, plain, ((e12 = op1(e12, e11)) | ~ spl20_27), inference(avatar_component_clause, [], [f561])).
fof(f200, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2724, plain, (~ spl20_29 | ~ spl20_30), inference(avatar_contradiction_clause, [], [f2723])).
fof(f2723, plain, ($false | (~ spl20_29 | ~ spl20_30)), inference(subsumption_resolution, [], [f2722, f256])).
fof(f256, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f2722, plain, ((e10 = e11) | (~ spl20_29 | ~ spl20_30)), inference(backward_demodulation, [], [f576, f572])).
fof(f576, plain, ((e11 = op1(e12, e10)) | ~ spl20_30), inference(avatar_component_clause, [], [f574])).
fof(f574, plain, (spl20_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_30])])).
fof(f2687, plain, (~ spl20_174 | ~ spl20_90 | ~ spl20_168), inference(avatar_split_clause, [], [f2683, f1373, f861, f1404])).
fof(f1404, plain, (spl20_174 <=> (e21 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_174])])).
fof(f1373, plain, (spl20_168 <=> (e22 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_168])])).
fof(f2683, plain, (~ (e21 = h1(e10)) | (~ spl20_90 | ~ spl20_168)), inference(backward_demodulation, [], [f2632, f863])).
fof(f2632, plain, (~ (op2(e22, e21) = h1(e10)) | ~ spl20_168), inference(forward_demodulation, [], [f244, f2511])).
fof(f2511, plain, ((op2(e22, e20) = h1(e10)) | ~ spl20_168), inference(backward_demodulation, [], [f1179, f1374])).
fof(f1374, plain, ((e22 = h1(e13)) | ~ spl20_168), inference(avatar_component_clause, [], [f1373])).
fof(f1179, plain, (h1(e10) = op2(h1(e13), e20)), inference(forward_demodulation, [], [f371, f373])).
fof(f373, plain, (op2(e20, e20) = h1(e13)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, e20) = h1(e13)) & (h1(e11) = op2(op2(e20, e20), op2(e20, e20))) & (h1(e10) = op2(op2(e20, e20), e20)) & (e20 = h1(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax14)).
fof(f371, plain, (h1(e10) = op2(op2(e20, e20), e20)), inference(cnf_transformation, [], [f14])).
fof(f244, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2667, plain, (~ spl20_148 | ~ spl20_113 | ~ spl20_142), inference(avatar_split_clause, [], [f2662, f1233, f959, f1264])).
fof(f1264, plain, (spl20_148 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_148])])).
fof(f2662, plain, (~ (e20 = h4(e10)) | (~ spl20_113 | ~ spl20_142)), inference(backward_demodulation, [], [f2641, f961])).
fof(f2641, plain, (~ (op2(e20, e23) = h4(e10)) | ~ spl20_142), inference(forward_demodulation, [], [f226, f2357])).
fof(f2357, plain, ((op2(e21, e23) = h4(e10)) | ~ spl20_142), inference(backward_demodulation, [], [f1207, f1234])).
fof(f1207, plain, (h4(e10) = op2(h4(e13), e23)), inference(forward_demodulation, [], [f383, f385])).
fof(f383, plain, (h4(e10) = op2(op2(e23, e23), e23)), inference(cnf_transformation, [], [f17])).
fof(f226, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2659, plain, (spl20_233 | ~ spl20_124 | ~ spl20_164), inference(avatar_split_clause, [], [f2658, f1353, f1005, f1754])).
fof(f1005, plain, (spl20_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_124])])).
fof(f2658, plain, ((e23 = h2(e10)) | (~ spl20_124 | ~ spl20_164)), inference(backward_demodulation, [], [f2648, f1007])).
fof(f1007, plain, ((e23 = op2(e20, e21)) | ~ spl20_124), inference(avatar_component_clause, [], [f1005])).
fof(f2648, plain, ((op2(e20, e21) = h2(e10)) | ~ spl20_164), inference(backward_demodulation, [], [f1187, f1354])).
fof(f1187, plain, (h2(e10) = op2(h2(e13), e21)), inference(forward_demodulation, [], [f375, f377])).
fof(f375, plain, (h2(e10) = op2(op2(e21, e21), e21)), inference(cnf_transformation, [], [f15])).
fof(f2652, plain, (~ spl20_121 | ~ spl20_164), inference(avatar_split_clause, [], [f2645, f1353, f993])).
fof(f993, plain, (spl20_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_121])])).
fof(f2645, plain, (~ (e20 = op2(e20, e21)) | ~ spl20_164), inference(backward_demodulation, [], [f1180, f1354])).
fof(f1180, plain, ~ (op2(e20, e21) = h2(e13)), inference(backward_demodulation, [], [f214, f377])).
fof(f214, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f2644, plain, (spl20_164 | ~ spl20_105), inference(avatar_split_clause, [], [f2499, f925, f1353])).
fof(f925, plain, (spl20_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_105])])).
fof(f2499, plain, ((e20 = h2(e13)) | ~ spl20_105), inference(forward_demodulation, [], [f377, f927])).
fof(f927, plain, ((e20 = op2(e21, e21)) | ~ spl20_105), inference(avatar_component_clause, [], [f925])).
fof(f2636, plain, (~ spl20_95 | ~ spl20_168), inference(avatar_split_clause, [], [f2635, f1373, f882])).
fof(f882, plain, (spl20_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_95])])).
fof(f2635, plain, (~ (e22 = op2(e22, e20)) | ~ spl20_168), inference(forward_demodulation, [], [f1173, f1374])).
fof(f1173, plain, ~ (op2(e22, e20) = h1(e13)), inference(backward_demodulation, [], [f209, f373])).
fof(f209, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2631, plain, (~ spl20_79 | ~ spl20_168), inference(avatar_split_clause, [], [f2630, f1373, f814])).
fof(f814, plain, (spl20_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_79])])).
fof(f2630, plain, (~ (e22 = op2(e23, e20)) | ~ spl20_168), inference(forward_demodulation, [], [f1174, f1374])).
fof(f1174, plain, ~ (op2(e23, e20) = h1(e13)), inference(backward_demodulation, [], [f210, f373])).
fof(f210, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2629, plain, (~ spl20_77 | ~ spl20_69), inference(avatar_split_clause, [], [f2482, f772, f806])).
fof(f806, plain, (spl20_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_77])])).
fof(f2482, plain, (~ (e20 = op2(e23, e20)) | ~ spl20_69), inference(forward_demodulation, [], [f251, f774])).
fof(f251, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2628, plain, (~ spl20_74 | ~ spl20_142), inference(avatar_split_clause, [], [f2627, f1233, f793])).
fof(f793, plain, (spl20_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_74])])).
fof(f2627, plain, (~ (e21 = op2(e23, e21)) | ~ spl20_142), inference(forward_demodulation, [], [f1204, f1234])).
fof(f1204, plain, ~ (op2(e23, e21) = h4(e13)), inference(backward_demodulation, [], [f254, f385])).
fof(f254, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2610, plain, (~ spl20_25 | ~ spl20_41), inference(avatar_split_clause, [], [f2609, f621, f553])).
fof(f553, plain, (spl20_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_25])])).
fof(f2609, plain, (~ (e10 = op1(e12, e11)) | ~ spl20_41), inference(forward_demodulation, [], [f169, f623])).
fof(f169, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2605, plain, (~ spl20_33 | ~ spl20_41), inference(avatar_split_clause, [], [f2604, f621, f587])).
fof(f587, plain, (spl20_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_33])])).
fof(f2604, plain, (~ (e10 = op1(e11, e13)) | ~ spl20_41), inference(forward_demodulation, [], [f194, f623])).
fof(f194, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2593, plain, (~ spl20_18 | ~ spl20_2), inference(avatar_split_clause, [], [f2592, f455, f523])).
fof(f523, plain, (spl20_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_18])])).
fof(f2592, plain, (~ (e11 = op1(e12, e13)) | ~ spl20_2), inference(forward_demodulation, [], [f183, f457])).
fof(f183, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2571, plain, (~ spl20_41 | ~ spl20_43), inference(avatar_contradiction_clause, [], [f2570])).
fof(f2570, plain, ($false | (~ spl20_41 | ~ spl20_43)), inference(subsumption_resolution, [], [f2569, f257])).
fof(f2569, plain, ((e10 = e12) | (~ spl20_41 | ~ spl20_43)), inference(backward_demodulation, [], [f631, f623])).
fof(f631, plain, ((e12 = op1(e11, e11)) | ~ spl20_43), inference(avatar_component_clause, [], [f629])).
fof(f629, plain, (spl20_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_43])])).
fof(f2560, plain, (~ spl20_26 | ~ spl20_58), inference(avatar_split_clause, [], [f2558, f693, f557])).
fof(f693, plain, (spl20_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_58])])).
fof(f2558, plain, (~ (e11 = op1(e12, e11)) | ~ spl20_58), inference(backward_demodulation, [], [f167, f695])).
fof(f695, plain, ((e11 = op1(e10, e11)) | ~ spl20_58), inference(avatar_component_clause, [], [f693])).
fof(f167, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2538, plain, (~ spl20_83 | ~ spl20_91), inference(avatar_split_clause, [], [f2537, f865, f831])).
fof(f865, plain, (spl20_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_91])])).
fof(f2537, plain, (~ (e22 = op2(e22, e23)) | ~ spl20_91), inference(backward_demodulation, [], [f248, f867])).
fof(f867, plain, ((e22 = op2(e22, e21)) | ~ spl20_91), inference(avatar_component_clause, [], [f865])).
fof(f248, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2534, plain, (spl20_140 | ~ spl20_99 | ~ spl20_142), inference(avatar_split_clause, [], [f2532, f1233, f899, f1224])).
fof(f1224, plain, (spl20_140 <=> (e22 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_140])])).
fof(f899, plain, (spl20_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_99])])).
fof(f2532, plain, ((e22 = h4(e10)) | (~ spl20_99 | ~ spl20_142)), inference(backward_demodulation, [], [f2357, f901])).
fof(f901, plain, ((e22 = op2(e21, e23)) | ~ spl20_99), inference(avatar_component_clause, [], [f899])).
fof(f2530, plain, (~ spl20_140 | ~ spl20_103 | ~ spl20_142), inference(avatar_split_clause, [], [f2528, f1233, f916, f1224])).
fof(f2528, plain, (~ (e22 = h4(e10)) | (~ spl20_103 | ~ spl20_142)), inference(backward_demodulation, [], [f2496, f918])).
fof(f2496, plain, (~ (op2(e21, e22) = h4(e10)) | ~ spl20_142), inference(forward_demodulation, [], [f243, f2357])).
fof(f243, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2526, plain, (~ spl20_105 | ~ spl20_106), inference(avatar_contradiction_clause, [], [f2525])).
fof(f2525, plain, ($false | (~ spl20_105 | ~ spl20_106)), inference(subsumption_resolution, [], [f2524, f262])).
fof(f262, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f2524, plain, ((e20 = e21) | (~ spl20_105 | ~ spl20_106)), inference(forward_demodulation, [], [f931, f927])).
fof(f931, plain, ((e21 = op2(e21, e21)) | ~ spl20_106), inference(avatar_component_clause, [], [f929])).
fof(f929, plain, (spl20_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_106])])).
fof(f2517, plain, (spl20_174 | ~ spl20_94 | ~ spl20_168), inference(avatar_split_clause, [], [f2516, f1373, f878, f1404])).
fof(f878, plain, (spl20_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_94])])).
fof(f2516, plain, ((e21 = h1(e10)) | (~ spl20_94 | ~ spl20_168)), inference(forward_demodulation, [], [f2511, f880])).
fof(f880, plain, ((e21 = op2(e22, e20)) | ~ spl20_94), inference(avatar_component_clause, [], [f878])).
fof(f2513, plain, (~ spl20_123 | ~ spl20_168), inference(avatar_split_clause, [], [f2509, f1373, f1001])).
fof(f1001, plain, (spl20_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_123])])).
fof(f2509, plain, (~ (e22 = op2(e20, e21)) | ~ spl20_168), inference(backward_demodulation, [], [f1175, f1374])).
fof(f1175, plain, ~ (op2(e20, e21) = h1(e13)), inference(backward_demodulation, [], [f232, f373])).
fof(f232, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f2504, plain, (~ spl20_122 | ~ spl20_118), inference(avatar_split_clause, [], [f2503, f980, f997])).
fof(f997, plain, (spl20_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_122])])).
fof(f2503, plain, (~ (e21 = op2(e20, e21)) | ~ spl20_118), inference(forward_demodulation, [], [f235, f982])).
fof(f235, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f2494, plain, (~ spl20_102 | ~ spl20_118), inference(avatar_split_clause, [], [f2493, f980, f912])).
fof(f912, plain, (spl20_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_102])])).
fof(f2493, plain, (~ (e21 = op2(e21, e22)) | ~ spl20_118), inference(forward_demodulation, [], [f220, f982])).
fof(f220, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2469, plain, (~ spl20_46 | ~ spl20_38), inference(avatar_split_clause, [], [f2468, f608, f642])).
fof(f608, plain, (spl20_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_38])])).
fof(f2468, plain, (~ (e11 = op1(e11, e10)) | ~ spl20_38), inference(forward_demodulation, [], [f191, f610])).
fof(f610, plain, ((e11 = op1(e11, e12)) | ~ spl20_38), inference(avatar_component_clause, [], [f608])).
fof(f191, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2454, plain, (~ spl20_13 | ~ spl20_5), inference(avatar_split_clause, [], [f2298, f468, f502])).
fof(f502, plain, (spl20_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_13])])).
fof(f2298, plain, (~ (e10 = op1(e13, e10)) | ~ spl20_5), inference(forward_demodulation, [], [f203, f470])).
fof(f203, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2447, plain, (~ spl20_21 | ~ spl20_24), inference(avatar_contradiction_clause, [], [f2446])).
fof(f2446, plain, ($false | (~ spl20_21 | ~ spl20_24)), inference(subsumption_resolution, [], [f2445, f258])).
fof(f258, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f2445, plain, ((e10 = e13) | (~ spl20_21 | ~ spl20_24)), inference(backward_demodulation, [], [f550, f538])).
fof(f538, plain, ((e10 = op1(e12, e12)) | ~ spl20_21), inference(avatar_component_clause, [], [f536])).
fof(f536, plain, (spl20_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_21])])).
fof(f2421, plain, (~ spl20_69 | ~ spl20_71), inference(avatar_contradiction_clause, [], [f2420])).
fof(f2420, plain, ($false | (~ spl20_69 | ~ spl20_71)), inference(subsumption_resolution, [], [f2419, f263])).
fof(f263, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f2419, plain, ((e20 = e22) | (~ spl20_69 | ~ spl20_71)), inference(backward_demodulation, [], [f782, f774])).
fof(f782, plain, ((e22 = op2(e23, e22)) | ~ spl20_71), inference(avatar_component_clause, [], [f780])).
fof(f780, plain, (spl20_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_71])])).
fof(f2400, plain, (spl20_148 | ~ spl20_97 | ~ spl20_142), inference(avatar_split_clause, [], [f2398, f1233, f891, f1264])).
fof(f891, plain, (spl20_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_97])])).
fof(f2398, plain, ((e20 = h4(e10)) | (~ spl20_97 | ~ spl20_142)), inference(backward_demodulation, [], [f2357, f893])).
fof(f893, plain, ((e20 = op2(e21, e23)) | ~ spl20_97), inference(avatar_component_clause, [], [f891])).
fof(f2393, plain, (~ spl20_105 | ~ spl20_107), inference(avatar_contradiction_clause, [], [f2392])).
fof(f2392, plain, ($false | (~ spl20_105 | ~ spl20_107)), inference(subsumption_resolution, [], [f2391, f263])).
fof(f2391, plain, ((e20 = e22) | (~ spl20_105 | ~ spl20_107)), inference(backward_demodulation, [], [f935, f927])).
fof(f935, plain, ((e22 = op2(e21, e21)) | ~ spl20_107), inference(avatar_component_clause, [], [f933])).
fof(f933, plain, (spl20_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_107])])).
fof(f2368, plain, (spl20_168 | ~ spl20_127), inference(avatar_split_clause, [], [f2367, f1018, f1373])).
fof(f2367, plain, ((e22 = h1(e13)) | ~ spl20_127), inference(backward_demodulation, [], [f373, f1020])).
fof(f2362, plain, (~ spl20_78 | ~ spl20_142), inference(avatar_split_clause, [], [f2355, f1233, f810])).
fof(f810, plain, (spl20_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_78])])).
fof(f2355, plain, (~ (e21 = op2(e23, e20)) | ~ spl20_142), inference(backward_demodulation, [], [f1203, f1234])).
fof(f1203, plain, ~ (op2(e23, e20) = h4(e13)), inference(backward_demodulation, [], [f252, f385])).
fof(f252, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2361, plain, (~ spl20_98 | ~ spl20_142), inference(avatar_split_clause, [], [f2354, f1233, f895])).
fof(f895, plain, (spl20_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_98])])).
fof(f2354, plain, (~ (e21 = op2(e21, e23)) | ~ spl20_142), inference(backward_demodulation, [], [f1201, f1234])).
fof(f1201, plain, ~ (op2(e21, e23) = h4(e13)), inference(backward_demodulation, [], [f230, f385])).
fof(f230, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2333, plain, (spl20_66 | ~ spl20_198), inference(avatar_split_clause, [], [f2204, f1577, f759])).
fof(f1577, plain, (spl20_198 <=> (e23 = h3(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_198])])).
fof(f2204, plain, ((e21 = op2(e23, e23)) | ~ spl20_198), inference(backward_demodulation, [], [f1195, f1578])).
fof(f1578, plain, ((e23 = h3(e13)) | ~ spl20_198), inference(avatar_component_clause, [], [f1577])).
fof(f1195, plain, (e21 = op2(h3(e13), h3(e13))), inference(backward_demodulation, [], [f368, f381])).
fof(f381, plain, (op2(e22, e22) = h3(e13)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, e22) = h3(e13)) & (op2(op2(e22, e22), op2(e22, e22)) = h3(e11)) & (op2(op2(e22, e22), e22) = h3(e10)) & (e22 = h3(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax16)).
fof(f368, plain, (e21 = op2(op2(e22, e22), op2(e22, e22))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(e22, e22)) & (e21 = op2(op2(e22, e22), op2(e22, e22))) & (e20 = op2(op2(e22, e22), e22))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax13)).
fof(f2324, plain, (~ spl20_50 | ~ spl20_2), inference(avatar_split_clause, [], [f2323, f455, f659])).
fof(f659, plain, (spl20_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_50])])).
fof(f2323, plain, (~ (e11 = op1(e10, e13)) | ~ spl20_2), inference(forward_demodulation, [], [f180, f457])).
fof(f180, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2303, plain, (~ spl20_14 | ~ spl20_2), inference(avatar_split_clause, [], [f2132, f455, f506])).
fof(f506, plain, (spl20_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_14])])).
fof(f2132, plain, (~ (e11 = op1(e13, e10)) | ~ spl20_2), inference(forward_demodulation, [], [f204, f457])).
fof(f204, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2295, plain, (~ spl20_1 | ~ spl20_2), inference(avatar_contradiction_clause, [], [f2294])).
fof(f2294, plain, ($false | (~ spl20_1 | ~ spl20_2)), inference(subsumption_resolution, [], [f2293, f256])).
fof(f2293, plain, ((e10 = e11) | (~ spl20_1 | ~ spl20_2)), inference(backward_demodulation, [], [f457, f453])).
fof(f453, plain, ((e10 = op1(e13, e13)) | ~ spl20_1), inference(avatar_component_clause, [], [f451])).
fof(f451, plain, (spl20_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_1])])).
fof(f2291, plain, (~ spl20_5 | ~ spl20_7), inference(avatar_contradiction_clause, [], [f2290])).
fof(f2290, plain, ($false | (~ spl20_5 | ~ spl20_7)), inference(subsumption_resolution, [], [f2289, f257])).
fof(f2289, plain, ((e10 = e12) | (~ spl20_5 | ~ spl20_7)), inference(backward_demodulation, [], [f478, f470])).
fof(f478, plain, ((e12 = op1(e13, e12)) | ~ spl20_7), inference(avatar_component_clause, [], [f476])).
fof(f476, plain, (spl20_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_7])])).
fof(f2254, plain, (~ spl20_85 | ~ spl20_88), inference(avatar_contradiction_clause, [], [f2253])).
fof(f2253, plain, ($false | (~ spl20_85 | ~ spl20_88)), inference(subsumption_resolution, [], [f2252, f264])).
fof(f264, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f2252, plain, ((e20 = e23) | (~ spl20_85 | ~ spl20_88)), inference(backward_demodulation, [], [f854, f842])).
fof(f842, plain, ((e20 = op2(e22, e22)) | ~ spl20_85), inference(avatar_component_clause, [], [f840])).
fof(f840, plain, (spl20_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_85])])).
fof(f2232, plain, (~ spl20_168 | ~ spl20_119), inference(avatar_split_clause, [], [f2230, f984, f1373])).
fof(f984, plain, (spl20_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_119])])).
fof(f2230, plain, (~ (e22 = h1(e13)) | ~ spl20_119), inference(backward_demodulation, [], [f1176, f986])).
fof(f986, plain, ((e22 = op2(e20, e22)) | ~ spl20_119), inference(avatar_component_clause, [], [f984])).
fof(f1176, plain, ~ (op2(e20, e22) = h1(e13)), inference(backward_demodulation, [], [f233, f373])).
fof(f233, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f2213, plain, (~ spl20_65 | ~ spl20_198), inference(avatar_contradiction_clause, [], [f2212])).
fof(f2212, plain, ($false | (~ spl20_65 | ~ spl20_198)), inference(subsumption_resolution, [], [f2211, f262])).
fof(f2211, plain, ((e20 = e21) | (~ spl20_65 | ~ spl20_198)), inference(forward_demodulation, [], [f2204, f757])).
fof(f757, plain, ((e20 = op2(e23, e23)) | ~ spl20_65), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl20_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_65])])).
fof(f2210, plain, (spl20_69 | ~ spl20_198), inference(avatar_split_clause, [], [f2203, f1577, f772])).
fof(f2203, plain, ((e20 = op2(e23, e22)) | ~ spl20_198), inference(backward_demodulation, [], [f1194, f1578])).
fof(f1194, plain, (e20 = op2(h3(e13), e22)), inference(backward_demodulation, [], [f367, f381])).
fof(f367, plain, (e20 = op2(op2(e22, e22), e22)), inference(cnf_transformation, [], [f13])).
fof(f2208, plain, (~ spl20_96 | ~ spl20_198), inference(avatar_split_clause, [], [f2201, f1577, f886])).
fof(f886, plain, (spl20_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_96])])).
fof(f2201, plain, (~ (e23 = op2(e22, e20)) | ~ spl20_198), inference(backward_demodulation, [], [f1191, f1578])).
fof(f1191, plain, ~ (op2(e22, e20) = h3(e13)), inference(backward_demodulation, [], [f245, f381])).
fof(f245, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2206, plain, (~ spl20_120 | ~ spl20_198), inference(avatar_split_clause, [], [f2199, f1577, f988])).
fof(f988, plain, (spl20_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_120])])).
fof(f2199, plain, (~ (e23 = op2(e20, e22)) | ~ spl20_198), inference(backward_demodulation, [], [f1188, f1578])).
fof(f1188, plain, ~ (op2(e20, e22) = h3(e13)), inference(backward_demodulation, [], [f221, f381])).
fof(f221, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2152, plain, (~ spl20_40 | ~ spl20_24), inference(avatar_split_clause, [], [f2151, f548, f616])).
fof(f616, plain, (spl20_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_40])])).
fof(f2151, plain, (~ (e13 = op1(e11, e12)) | ~ spl20_24), inference(forward_demodulation, [], [f175, f550])).
fof(f175, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2148, plain, (~ spl20_35 | ~ spl20_19), inference(avatar_split_clause, [], [f2147, f527, f595])).
fof(f595, plain, (spl20_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_35])])).
fof(f2147, plain, (~ (e12 = op1(e11, e13)) | ~ spl20_19), inference(forward_demodulation, [], [f181, f529])).
fof(f181, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2146, plain, (~ spl20_34 | ~ spl20_2), inference(avatar_split_clause, [], [f2145, f455, f591])).
fof(f591, plain, (spl20_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_34])])).
fof(f2145, plain, (~ (e11 = op1(e11, e13)) | ~ spl20_2), inference(forward_demodulation, [], [f182, f457])).
fof(f182, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2142, plain, (~ spl20_28 | ~ spl20_24), inference(avatar_split_clause, [], [f2141, f548, f565])).
fof(f565, plain, (spl20_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_28])])).
fof(f2141, plain, (~ (e13 = op1(e12, e11)) | ~ spl20_24), inference(forward_demodulation, [], [f199, f550])).
fof(f199, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2110, plain, (~ spl20_12 | ~ spl20_16), inference(avatar_split_clause, [], [f2107, f514, f497])).
fof(f497, plain, (spl20_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_12])])).
fof(f2107, plain, (~ (e13 = op1(e13, e11)) | ~ spl20_16), inference(backward_demodulation, [], [f202, f516])).
fof(f202, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2100, plain, (~ spl20_3 | ~ spl20_19), inference(avatar_split_clause, [], [f2099, f527, f459])).
fof(f459, plain, (spl20_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_3])])).
fof(f2099, plain, (~ (e12 = op1(e13, e13)) | ~ spl20_19), inference(backward_demodulation, [], [f183, f529])).
fof(f2098, plain, (spl20_2 | ~ spl20_24), inference(avatar_split_clause, [], [f2094, f548, f455])).
fof(f2094, plain, ((e11 = op1(e13, e13)) | ~ spl20_24), inference(backward_demodulation, [], [f365, f550])).
fof(f365, plain, (e11 = op1(op1(e12, e12), op1(e12, e12))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(e12, e12)) & (e11 = op1(op1(e12, e12), op1(e12, e12))) & (e10 = op1(op1(e12, e12), e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax12)).
fof(f2097, plain, (spl20_5 | ~ spl20_24), inference(avatar_split_clause, [], [f2093, f548, f468])).
fof(f2093, plain, ((e10 = op1(e13, e12)) | ~ spl20_24), inference(backward_demodulation, [], [f364, f550])).
fof(f364, plain, (e10 = op1(op1(e12, e12), e12)), inference(cnf_transformation, [], [f12])).
fof(f2096, plain, (~ spl20_20 | ~ spl20_24), inference(avatar_split_clause, [], [f2092, f548, f531])).
fof(f531, plain, (spl20_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_20])])).
fof(f2092, plain, (~ (e13 = op1(e12, e13)) | ~ spl20_24), inference(backward_demodulation, [], [f201, f550])).
fof(f201, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2071, plain, (~ spl20_5 | ~ spl20_37), inference(avatar_split_clause, [], [f2068, f604, f468])).
fof(f604, plain, (spl20_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_37])])).
fof(f2068, plain, (~ (e10 = op1(e13, e12)) | ~ spl20_37), inference(backward_demodulation, [], [f176, f606])).
fof(f606, plain, ((e10 = op1(e11, e12)) | ~ spl20_37), inference(avatar_component_clause, [], [f604])).
fof(f176, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2066, plain, (~ spl20_41 | ~ spl20_42), inference(avatar_contradiction_clause, [], [f2065])).
fof(f2065, plain, ($false | (~ spl20_41 | ~ spl20_42)), inference(subsumption_resolution, [], [f2064, f256])).
fof(f2064, plain, ((e10 = e11) | (~ spl20_41 | ~ spl20_42)), inference(backward_demodulation, [], [f627, f623])).
fof(f627, plain, ((e11 = op1(e11, e11)) | ~ spl20_42), inference(avatar_component_clause, [], [f625])).
fof(f625, plain, (spl20_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_42])])).
fof(f2049, plain, (~ spl20_41 | ~ spl20_45), inference(avatar_split_clause, [], [f2044, f638, f621])).
fof(f638, plain, (spl20_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_45])])).
fof(f2044, plain, (~ (e10 = op1(e11, e11)) | ~ spl20_45), inference(backward_demodulation, [], [f190, f640])).
fof(f640, plain, ((e10 = op1(e11, e10)) | ~ spl20_45), inference(avatar_component_clause, [], [f638])).
fof(f190, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2029, plain, (~ spl20_22 | ~ spl20_54), inference(avatar_split_clause, [], [f2025, f676, f540])).
fof(f540, plain, (spl20_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_22])])).
fof(f2025, plain, (~ (e11 = op1(e12, e12)) | ~ spl20_54), inference(backward_demodulation, [], [f173, f678])).
fof(f173, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f1995, plain, (spl20_142 | ~ spl20_66), inference(avatar_split_clause, [], [f1994, f759, f1233])).
fof(f1994, plain, ((e21 = h4(e13)) | ~ spl20_66), inference(backward_demodulation, [], [f385, f761])).
fof(f1958, plain, (spl20_198 | ~ spl20_88), inference(avatar_split_clause, [], [f1957, f852, f1577])).
fof(f1957, plain, ((e23 = h3(e13)) | ~ spl20_88), inference(backward_demodulation, [], [f381, f854])).
fof(f1948, plain, (~ spl20_81 | ~ spl20_93), inference(avatar_split_clause, [], [f1943, f874, f823])).
fof(f823, plain, (spl20_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_81])])).
fof(f1943, plain, (~ (e20 = op2(e22, e23)) | ~ spl20_93), inference(backward_demodulation, [], [f246, f876])).
fof(f246, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f1945, plain, (~ spl20_176 | ~ spl20_93), inference(avatar_split_clause, [], [f1940, f874, f1413])).
fof(f1413, plain, (spl20_176 <=> (e20 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_176])])).
fof(f1940, plain, (~ (e20 = h1(e13)) | ~ spl20_93), inference(backward_demodulation, [], [f1173, f876])).
fof(f1938, plain, (~ spl20_84 | ~ spl20_100), inference(avatar_split_clause, [], [f1935, f903, f835])).
fof(f835, plain, (spl20_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_84])])).
fof(f1935, plain, (~ (e23 = op2(e22, e23)) | ~ spl20_100), inference(backward_demodulation, [], [f229, f905])).
fof(f229, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f1891, plain, (~ spl20_69 | ~ spl20_117), inference(avatar_split_clause, [], [f1886, f976, f772])).
fof(f976, plain, (spl20_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_117])])).
fof(f1886, plain, (~ (e20 = op2(e23, e22)) | ~ spl20_117), inference(backward_demodulation, [], [f222, f978])).
fof(f978, plain, ((e20 = op2(e20, e22)) | ~ spl20_117), inference(avatar_component_clause, [], [f976])).
fof(f222, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f1870, plain, (spl20_176 | ~ spl20_125), inference(avatar_split_clause, [], [f1869, f1010, f1413])).
fof(f1010, plain, (spl20_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_125])])).
fof(f1869, plain, ((e20 = h1(e13)) | ~ spl20_125), inference(backward_demodulation, [], [f373, f1012])).
fof(f1012, plain, ((e20 = op2(e20, e20)) | ~ spl20_125), inference(avatar_component_clause, [], [f1010])).
fof(f1757, plain, (~ spl20_215 | ~ spl20_216 | ~ spl20_217 | ~ spl20_218 | ~ spl20_219 | ~ spl20_220 | ~ spl20_221 | ~ spl20_222 | spl20_163 | spl20_159 | spl20_155 | ~ spl20_233 | ~ spl20_224 | ~ spl20_225 | ~ spl20_226 | ~ spl20_227 | ~ spl20_228 | ~ spl20_229 | ~ spl20_230 | ~ spl20_231), inference(avatar_split_clause, [], [f1752, f1723, f1719, f1715, f1711, f1707, f1703, f1699, f1695, f1754, f1308, f1328, f1349, f1687, f1683, f1679, f1675, f1671, f1667, f1663, f1659])).
fof(f1349, plain, (spl20_163 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl20_163])])).
fof(f1328, plain, (spl20_159 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl20_159])])).
fof(f1308, plain, (spl20_155 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl20_155])])).
fof(f1752, plain, (~ (h2(op1(e10, e12)) = op2(h2(e10), e21)) | ~ (h2(op1(e11, e12)) = op2(h2(e11), e21)) | ~ (h2(op1(e12, e10)) = op2(e21, h2(e10))) | ~ (h2(op1(e12, e11)) = op2(e21, h2(e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | ~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1751, f374])).
fof(f1751, plain, (~ (h2(op1(e11, e12)) = op2(h2(e11), e21)) | ~ (h2(op1(e12, e10)) = op2(e21, h2(e10))) | ~ (h2(op1(e12, e11)) = op2(e21, h2(e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | ~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1750, f374])).
fof(f1750, plain, (~ (h2(op1(e12, e10)) = op2(e21, h2(e10))) | ~ (h2(op1(e12, e11)) = op2(e21, h2(e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | ~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1749, f374])).
fof(f1749, plain, (~ (h2(op1(e12, e11)) = op2(e21, h2(e11))) | ~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | ~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1748, f374])).
fof(f1748, plain, (~ (h2(e13) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | ~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1747, f377])).
fof(f1747, plain, (~ (op2(e21, e21) = h2(op1(e12, e12))) | ~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | ~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1746, f374])).
fof(f1746, plain, (~ (h2(op1(e12, e13)) = op2(e21, h2(e13))) | ~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1745, f374])).
fof(f1745, plain, (~ (h2(e10) = h2(op1(e13, e12))) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1744, f1187])).
fof(f1744, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e21)) | ~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1743, f374])).
fof(f1743, plain, (~ (h2(e11) = h2(op1(e13, e13))) | ~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f438, f1186])).
fof(f438, plain, (~ (e23 = h2(e10)) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP16 | sP15 | sP14 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP10 | sP9 | sP8 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31])).
fof(f31, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP8), inference(usedef, [], [e31])).
fof(e31, plain, (sP8 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f32, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP9), inference(usedef, [], [e32])).
fof(e32, plain, (sP9 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f33, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP10), inference(usedef, [], [e33])).
fof(e33, plain, (sP10 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f34, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP11), inference(usedef, [], [e34])).
fof(e34, plain, (sP11 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f35, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP12), inference(usedef, [], [e35])).
fof(e35, plain, (sP12 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f36, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP13), inference(usedef, [], [e36])).
fof(e36, plain, (sP13 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f37, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP14), inference(usedef, [], [e37])).
fof(e37, plain, (sP14 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f38, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP15), inference(usedef, [], [e38])).
fof(e38, plain, (sP15 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f39, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP16), inference(usedef, [], [e39])).
fof(e39, plain, (sP16 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f40, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP17), inference(usedef, [], [e40])).
fof(e40, plain, (sP17 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f41, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP18), inference(usedef, [], [e41])).
fof(e41, plain, (sP18 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f42, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP19), inference(usedef, [], [e42])).
fof(e42, plain, (sP19 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', co1)).
fof(f1356, plain, (~ spl20_163 | ~ spl20_164), inference(avatar_split_clause, [], [f421, f1353, f1349])).
fof(f421, plain, (~ (e20 = h2(e13)) | ~ sP11), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP11), inference(nnf_transformation, [], [f34])).
fof(f1337, plain, ~ spl20_159, inference(avatar_split_clause, [], [f1336, f1328])).
fof(f1336, plain, ~ sP12, inference(subsumption_resolution, [], [f416, f374])).
fof(f416, plain, (~ (e21 = h2(e12)) | ~ sP12), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP12), inference(nnf_transformation, [], [f35])).
fof(f1321, plain, (~ spl20_155 | ~ spl20_157), inference(avatar_split_clause, [], [f411, f1318, f1308])).
fof(f411, plain, (~ (e22 = h2(e11)) | ~ sP13), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP13), inference(nnf_transformation, [], [f36])).
fof(f1171, plain, spl20_88, inference(avatar_split_clause, [], [f369, f852])).
fof(f369, plain, (e23 = op2(e22, e22)), inference(cnf_transformation, [], [f13])).
fof(f1170, plain, spl20_24, inference(avatar_split_clause, [], [f366, f548])).
fof(f366, plain, (e13 = op1(e12, e12)), inference(cnf_transformation, [], [f12])).
fof(f1169, plain, (spl20_136 | spl20_105 | spl20_85 | spl20_65), inference(avatar_split_clause, [], [f332, f755, f840, f925, f1133])).
fof(f1133, plain, (spl20_136 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl20_136])])).
fof(f332, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | sP4), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e23, e23)) & (e23 = op2(e22, e22))) | (~ (e21 = op2(e23, e23)) & (e23 = op2(e21, e21))) | sP7) & ((~ (e23 = op2(e22, e22)) & (e22 = op2(e23, e23))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e22, e22)) & (e22 = op2(e21, e21))) | sP6) & ((~ (e23 = op2(e21, e21)) & (e21 = op2(e23, e23))) | (~ (e22 = op2(e21, e21)) & (e21 = op2(e22, e22))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | sP5) & ((~ (op2(e20, e20) = e23) & (e20 = op2(e23, e23))) | (~ (op2(e20, e20) = e22) & (e20 = op2(e22, e22))) | (~ (op2(e20, e20) = e21) & (e20 = op2(e21, e21))) | sP4)), inference(definition_folding, [], [f11, e29, e28, e27, e26])).
fof(f26, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP4), inference(usedef, [], [e26])).
fof(e26, plain, (sP4 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f27, plain, ((~ (e20 = op2(e21, e21)) & (op2(e20, e20) = e21)) | ~ sP5), inference(usedef, [], [e27])).
fof(e27, plain, (sP5 <=> (~ (e20 = op2(e21, e21)) & (op2(e20, e20) = e21))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f28, plain, ((~ (e20 = op2(e22, e22)) & (op2(e20, e20) = e22)) | ~ sP6), inference(usedef, [], [e28])).
fof(e28, plain, (sP6 <=> (~ (e20 = op2(e22, e22)) & (op2(e20, e20) = e22))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f29, plain, ((~ (e20 = op2(e23, e23)) & (op2(e20, e20) = e23)) | ~ sP7), inference(usedef, [], [e29])).
fof(e29, plain, (sP7 <=> (~ (e20 = op2(e23, e23)) & (op2(e20, e20) = e23))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e23, e23)) & (e23 = op2(e22, e22))) | (~ (e21 = op2(e23, e23)) & (e23 = op2(e21, e21))) | (~ (e20 = op2(e23, e23)) & (op2(e20, e20) = e23))) & ((~ (e23 = op2(e22, e22)) & (e22 = op2(e23, e23))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e22, e22)) & (e22 = op2(e21, e21))) | (~ (e20 = op2(e22, e22)) & (op2(e20, e20) = e22))) & ((~ (e23 = op2(e21, e21)) & (e21 = op2(e23, e23))) | (~ (e22 = op2(e21, e21)) & (e21 = op2(e22, e22))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e20 = op2(e21, e21)) & (op2(e20, e20) = e21))) & ((~ (op2(e20, e20) = e23) & (e20 = op2(e23, e23))) | (~ (op2(e20, e20) = e22) & (e20 = op2(e22, e22))) | (~ (op2(e20, e20) = e21) & (e20 = op2(e21, e21))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax11)).
fof(f1149, plain, (spl20_134 | spl20_107 | spl20_87 | ~ spl20_88), inference(avatar_split_clause, [], [f352, f852, f848, f933, f1121])).
fof(f1121, plain, (spl20_134 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl20_134])])).
fof(f352, plain, (~ (e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e21)) | sP6), inference(cnf_transformation, [], [f30])).
fof(f1137, plain, (~ spl20_136 | spl20_125), inference(avatar_split_clause, [], [f330, f1010, f1133])).
fof(f330, plain, ((e20 = op2(e20, e20)) | ~ sP4), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP4), inference(nnf_transformation, [], [f26])).
fof(f1136, plain, (~ spl20_136 | ~ spl20_125), inference(avatar_split_clause, [], [f331, f1010, f1133])).
fof(f331, plain, (~ (e20 = op2(e20, e20)) | ~ sP4), inference(cnf_transformation, [], [f51])).
fof(f1125, plain, (~ spl20_134 | spl20_127), inference(avatar_split_clause, [], [f326, f1018, f1121])).
fof(f326, plain, ((op2(e20, e20) = e22) | ~ sP6), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e20 = op2(e22, e22)) & (op2(e20, e20) = e22)) | ~ sP6), inference(nnf_transformation, [], [f28])).
fof(f1113, plain, (spl20_132 | spl20_41 | spl20_21 | spl20_1), inference(avatar_split_clause, [], [f292, f451, f536, f621, f1077])).
fof(f1077, plain, (spl20_132 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl20_132])])).
fof(f292, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | sP0), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e13, e13)) & (e13 = op1(e12, e12))) | (~ (e11 = op1(e13, e13)) & (e13 = op1(e11, e11))) | sP3) & ((~ (e13 = op1(e12, e12)) & (e12 = op1(e13, e13))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e12, e12)) & (e12 = op1(e11, e11))) | sP2) & ((~ (e13 = op1(e11, e11)) & (e11 = op1(e13, e13))) | (~ (e12 = op1(e11, e11)) & (e11 = op1(e12, e12))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | sP1) & ((~ (op1(e10, e10) = e13) & (e10 = op1(e13, e13))) | (~ (op1(e10, e10) = e12) & (e10 = op1(e12, e12))) | (~ (op1(e10, e10) = e11) & (e10 = op1(e11, e11))) | sP0)), inference(definition_folding, [], [f10, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e10 = op1(e11, e11)) & (op1(e10, e10) = e11)) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e10 = op1(e11, e11)) & (op1(e10, e10) = e11))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e10 = op1(e12, e12)) & (op1(e10, e10) = e12)) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e10 = op1(e12, e12)) & (op1(e10, e10) = e12))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e10 = op1(e13, e13)) & (op1(e10, e10) = e13)) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e10 = op1(e13, e13)) & (op1(e10, e10) = e13))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e13, e13)) & (e13 = op1(e12, e12))) | (~ (e11 = op1(e13, e13)) & (e13 = op1(e11, e11))) | (~ (e10 = op1(e13, e13)) & (op1(e10, e10) = e13))) & ((~ (e13 = op1(e12, e12)) & (e12 = op1(e13, e13))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e12, e12)) & (e12 = op1(e11, e11))) | (~ (e10 = op1(e12, e12)) & (op1(e10, e10) = e12))) & ((~ (e13 = op1(e11, e11)) & (e11 = op1(e13, e13))) | (~ (e12 = op1(e11, e11)) & (e11 = op1(e12, e12))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e10 = op1(e11, e11)) & (op1(e10, e10) = e11))) & ((~ (op1(e10, e10) = e13) & (e10 = op1(e13, e13))) | (~ (op1(e10, e10) = e12) & (e10 = op1(e12, e12))) | (~ (op1(e10, e10) = e11) & (e10 = op1(e11, e11))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax10)).
fof(f1112, plain, (spl20_132 | ~ spl20_62 | spl20_21 | spl20_1), inference(avatar_split_clause, [], [f293, f451, f536, f710, f1077])).
fof(f710, plain, (spl20_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl20_62])])).
fof(f293, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | ~ (op1(e10, e10) = e11) | sP0), inference(cnf_transformation, [], [f25])).
fof(f1101, plain, (spl20_131 | spl20_42 | spl20_22 | ~ spl20_44), inference(avatar_split_clause, [], [f304, f633, f540, f625, f1071])).
fof(f1071, plain, (spl20_131 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl20_131])])).
fof(f633, plain, (spl20_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_44])])).
fof(f304, plain, (~ (e13 = op1(e11, e11)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | sP1), inference(cnf_transformation, [], [f25])).
fof(f1093, plain, (spl20_130 | spl20_43 | spl20_23 | ~ spl20_24), inference(avatar_split_clause, [], [f312, f548, f544, f629, f1065])).
fof(f1065, plain, (spl20_130 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl20_130])])).
fof(f544, plain, (spl20_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_23])])).
fof(f312, plain, (~ (e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | sP2), inference(cnf_transformation, [], [f25])).
fof(f1091, plain, (spl20_130 | spl20_43 | ~ spl20_23 | ~ spl20_24), inference(avatar_split_clause, [], [f314, f548, f544, f629, f1065])).
fof(f314, plain, (~ (e13 = op1(e12, e12)) | ~ (e12 = op1(e12, e12)) | (e12 = op1(e11, e11)) | sP2), inference(cnf_transformation, [], [f25])).
fof(f1081, plain, (~ spl20_132 | spl20_61), inference(avatar_split_clause, [], [f290, f706, f1077])).
fof(f290, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1080, plain, (~ spl20_132 | ~ spl20_61), inference(avatar_split_clause, [], [f291, f706, f1077])).
fof(f291, plain, (~ (e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f47])).
fof(f1074, plain, (~ spl20_131 | ~ spl20_41), inference(avatar_split_clause, [], [f289, f621, f1071])).
fof(f289, plain, (~ (e10 = op1(e11, e11)) | ~ sP1), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e10 = op1(e11, e11)) & (op1(e10, e10) = e11)) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1069, plain, (~ spl20_130 | spl20_63), inference(avatar_split_clause, [], [f286, f714, f1065])).
fof(f286, plain, ((op1(e10, e10) = e12) | ~ sP2), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e10 = op1(e12, e12)) & (op1(e10, e10) = e12)) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1057, plain, (spl20_125 | spl20_121 | spl20_117 | spl20_113), inference(avatar_split_clause, [], [f128, f959, f976, f993, f1010])).
fof(f128, plain, ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax4)).
fof(f1047, plain, (spl20_110 | spl20_106 | spl20_102 | spl20_98), inference(avatar_split_clause, [], [f138, f895, f912, f929, f946])).
fof(f138, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1046, plain, (spl20_122 | spl20_106 | spl20_90 | spl20_74), inference(avatar_split_clause, [], [f139, f793, f861, f929, f997])).
fof(f139, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1044, plain, (spl20_123 | spl20_107 | spl20_91 | spl20_75), inference(avatar_split_clause, [], [f141, f797, f865, f933, f1001])).
fof(f141, plain, ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1036, plain, (spl20_119 | spl20_103 | spl20_87 | spl20_71), inference(avatar_split_clause, [], [f149, f780, f848, f916, f984])).
fof(f149, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1008, plain, (spl20_121 | spl20_122 | spl20_123 | spl20_124), inference(avatar_split_clause, [], [f113, f1005, f1001, f997, f993])).
fof(f113, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax3)).
fof(f991, plain, (spl20_117 | spl20_118 | spl20_119 | spl20_120), inference(avatar_split_clause, [], [f114, f988, f984, f980, f976])).
fof(f114, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f906, plain, (spl20_97 | spl20_98 | spl20_99 | spl20_100), inference(avatar_split_clause, [], [f119, f903, f899, f895, f891])).
fof(f119, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f889, plain, (spl20_93 | spl20_94 | spl20_95 | spl20_96), inference(avatar_split_clause, [], [f120, f886, f882, f878, f874])).
fof(f120, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f838, plain, (spl20_81 | spl20_82 | spl20_83 | spl20_84), inference(avatar_split_clause, [], [f123, f835, f831, f827, f823])).
fof(f123, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f821, plain, (spl20_77 | spl20_78 | spl20_79 | spl20_80), inference(avatar_split_clause, [], [f124, f818, f814, f810, f806])).
fof(f124, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f752, plain, (spl20_61 | spl20_45 | spl20_29 | spl20_13), inference(avatar_split_clause, [], [f81, f502, f570, f638, f706])).
fof(f81, plain, ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax2)).
fof(f751, plain, (spl20_62 | spl20_58 | spl20_54 | spl20_50), inference(avatar_split_clause, [], [f82, f659, f676, f693, f710])).
fof(f82, plain, ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f750, plain, (spl20_62 | spl20_46 | spl20_30 | spl20_14), inference(avatar_split_clause, [], [f83, f506, f574, f642, f710])).
fof(f83, plain, ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f738, plain, (spl20_60 | spl20_44 | spl20_28 | spl20_12), inference(avatar_split_clause, [], [f95, f497, f565, f633, f701])).
fof(f95, plain, ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f728, plain, (spl20_49 | spl20_33 | spl20_17 | spl20_1), inference(avatar_split_clause, [], [f105, f451, f519, f587, f655])).
fof(f105, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f725, plain, (spl20_15 | spl20_11 | spl20_7 | spl20_3), inference(avatar_split_clause, [], [f108, f459, f476, f493, f510])).
fof(f108, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f619, plain, (spl20_37 | spl20_38 | spl20_39 | spl20_40), inference(avatar_split_clause, [], [f70, f616, f612, f608, f604])).
fof(f70, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG114+1.p', ax1)).
fof(f602, plain, (spl20_33 | spl20_34 | spl20_35 | spl20_36), inference(avatar_split_clause, [], [f71, f599, f595, f591, f587])).
fof(f71, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f568, plain, (spl20_25 | spl20_26 | spl20_27 | spl20_28), inference(avatar_split_clause, [], [f73, f565, f561, f557, f553])).
fof(f73, plain, ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f534, plain, (spl20_17 | spl20_18 | spl20_19 | spl20_20), inference(avatar_split_clause, [], [f75, f531, f527, f523, f519])).
fof(f75, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f517, plain, (spl20_13 | spl20_14 | spl20_15 | spl20_16), inference(avatar_split_clause, [], [f76, f514, f510, f506, f502])).
fof(f76, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).