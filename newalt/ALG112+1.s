fof(f4087, plain, $false, inference(avatar_sat_refutation, [], [f533, f550, f567, f618, f635, f669, f671, f673, f680, f681, f683, f684, f690, f695, f697, f701, f752, f769, f837, f854, f871, f922, f939, f956, f973, f984, f985, f988, f994, f999, f1004, f1024, f1029, f1043, f1048, f1062, f1072, f1077, f1092, f1097, f1116, f1121, f1135, f1140, f1154, f1164, f1184, f1189, f1190, f1192, f1193, f1195, f1372, f1390, f1408, f1787, f1925, f1934, f1965, f1966, f1968, f1976, f1979, f2001, f2007, f2009, f2012, f2031, f2032, f2072, f2109, f2110, f2112, f2113, f2119, f2120, f2121, f2122, f2123, f2133, f2153, f2180, f2212, f2220, f2241, f2243, f2263, f2272, f2273, f2277, f2282, f2283, f2286, f2290, f2297, f2303, f2317, f2325, f2333, f2335, f2341, f2346, f2371, f2375, f2387, f2389, f2395, f2397, f2400, f2416, f2426, f2472, f2482, f2483, f2495, f2498, f2525, f2539, f2572, f2585, f2588, f2600, f2638, f2645, f2646, f2653, f2660, f2674, f2697, f2698, f2699, f2732, f2746, f2769, f2780, f2783, f2804, f2843, f2845, f2854, f2861, f2907, f2944, f2957, f2960, f2972, f2995, f2997, f3011, f3022, f3025, f3029, f3070, f3075, f3077, f3085, f3110, f3150, f3170, f3192, f3245, f3257, f3259, f3282, f3341, f3370, f3376, f3396, f3433, f3449, f3469, f3491, f3515, f3548, f3549, f3559, f3605, f3628, f3631, f3639, f3665, f3719, f3730, f3759, f3788, f3824, f3858, f3888, f3918, f3950, f3981, f4018, f4047, f4071])).
fof(f4071, plain, (~ spl18_58 | ~ spl18_122 | spl18_265), inference(avatar_contradiction_clause, [], [f4070])).
fof(f4070, plain, ($false | (~ spl18_58 | ~ spl18_122 | spl18_265)), inference(subsumption_resolution, [], [f4069, f947])).
fof(f947, plain, ((e21 = op2(e20, e21)) | ~ spl18_122), inference(avatar_component_clause, [], [f945])).
fof(f945, plain, (spl18_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_122])])).
fof(f4069, plain, (~ (e21 = op2(e20, e21)) | (~ spl18_58 | spl18_265)), inference(forward_demodulation, [], [f4068, f322])).
fof(f322, plain, (e21 = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, op2(op2(e21, e21), e21)) = h2(e13)) & (op2(e21, e21) = h2(e12)) & (op2(op2(e21, e21), e21) = h2(e10)) & (e21 = h2(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax15)).
fof(f4068, plain, (~ (op2(e20, e21) = h2(e11)) | (~ spl18_58 | spl18_265)), inference(forward_demodulation, [], [f1782, f643])).
fof(f643, plain, ((e11 = op1(e10, e11)) | ~ spl18_58), inference(avatar_component_clause, [], [f641])).
fof(f641, plain, (spl18_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_58])])).
fof(f1782, plain, (~ (op2(e20, e21) = h2(op1(e10, e11))) | spl18_265), inference(avatar_component_clause, [], [f1780])).
fof(f1780, plain, (spl18_265 <=> (op2(e20, e21) = h2(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_265])])).
fof(f4047, plain, (~ spl18_55 | ~ spl18_119 | ~ spl18_193 | spl18_264), inference(avatar_contradiction_clause, [], [f4046])).
fof(f4046, plain, ($false | (~ spl18_55 | ~ spl18_119 | ~ spl18_193 | spl18_264)), inference(subsumption_resolution, [], [f4045, f934])).
fof(f934, plain, ((e22 = op2(e20, e22)) | ~ spl18_119), inference(avatar_component_clause, [], [f932])).
fof(f932, plain, (spl18_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_119])])).
fof(f4045, plain, (~ (e22 = op2(e20, e22)) | (~ spl18_55 | ~ spl18_193 | spl18_264)), inference(forward_demodulation, [], [f4044, f1370])).
fof(f1370, plain, ((e22 = h2(e12)) | ~ spl18_193), inference(avatar_component_clause, [], [f1369])).
fof(f1369, plain, (spl18_193 <=> (e22 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_193])])).
fof(f4044, plain, (~ (op2(e20, e22) = h2(e12)) | (~ spl18_55 | ~ spl18_193 | spl18_264)), inference(forward_demodulation, [], [f4043, f630])).
fof(f630, plain, ((e12 = op1(e10, e12)) | ~ spl18_55), inference(avatar_component_clause, [], [f628])).
fof(f628, plain, (spl18_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_55])])).
fof(f4043, plain, (~ (op2(e20, e22) = h2(op1(e10, e12))) | (~ spl18_193 | spl18_264)), inference(forward_demodulation, [], [f1778, f1370])).
fof(f1778, plain, (~ (h2(op1(e10, e12)) = op2(e20, h2(e12))) | spl18_264), inference(avatar_component_clause, [], [f1776])).
fof(f1776, plain, (spl18_264 <=> (h2(op1(e10, e12)) = op2(e20, h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_264])])).
fof(f4018, plain, (~ spl18_43 | spl18_261), inference(avatar_contradiction_clause, [], [f4017])).
fof(f4017, plain, ($false | (~ spl18_43 | spl18_261)), inference(trivial_inequality_removal, [], [f4016])).
fof(f4016, plain, (~ (h2(e12) = h2(e12)) | (~ spl18_43 | spl18_261)), inference(forward_demodulation, [], [f1766, f579])).
fof(f579, plain, ((e12 = op1(e11, e11)) | ~ spl18_43), inference(avatar_component_clause, [], [f577])).
fof(f577, plain, (spl18_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_43])])).
fof(f1766, plain, (~ (h2(e12) = h2(op1(e11, e11))) | spl18_261), inference(avatar_component_clause, [], [f1764])).
fof(f1764, plain, (spl18_261 <=> (h2(e12) = h2(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_261])])).
fof(f3981, plain, (~ spl18_52 | ~ spl18_116 | ~ spl18_254 | spl18_263), inference(avatar_contradiction_clause, [], [f3980])).
fof(f3980, plain, ($false | (~ spl18_52 | ~ spl18_116 | ~ spl18_254 | spl18_263)), inference(subsumption_resolution, [], [f3979, f921])).
fof(f921, plain, ((e23 = op2(e20, e23)) | ~ spl18_116), inference(avatar_component_clause, [], [f919])).
fof(f919, plain, (spl18_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_116])])).
fof(f3979, plain, (~ (e23 = op2(e20, e23)) | (~ spl18_52 | ~ spl18_254 | spl18_263)), inference(forward_demodulation, [], [f3978, f1737])).
fof(f1737, plain, ((e23 = h2(e13)) | ~ spl18_254), inference(avatar_component_clause, [], [f1736])).
fof(f1736, plain, (spl18_254 <=> (e23 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_254])])).
fof(f3978, plain, (~ (op2(e20, e23) = h2(e13)) | (~ spl18_52 | ~ spl18_254 | spl18_263)), inference(forward_demodulation, [], [f3977, f617])).
fof(f617, plain, ((e13 = op1(e10, e13)) | ~ spl18_52), inference(avatar_component_clause, [], [f615])).
fof(f615, plain, (spl18_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_52])])).
fof(f3977, plain, (~ (op2(e20, e23) = h2(op1(e10, e13))) | (~ spl18_254 | spl18_263)), inference(forward_demodulation, [], [f1774, f1737])).
fof(f1774, plain, (~ (h2(op1(e10, e13)) = op2(e20, h2(e13))) | spl18_263), inference(avatar_component_clause, [], [f1772])).
fof(f1772, plain, (spl18_263 <=> (h2(op1(e10, e13)) = op2(e20, h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_263])])).
fof(f3950, plain, (~ spl18_33 | ~ spl18_97 | ~ spl18_254 | spl18_259), inference(avatar_contradiction_clause, [], [f3949])).
fof(f3949, plain, ($false | (~ spl18_33 | ~ spl18_97 | ~ spl18_254 | spl18_259)), inference(subsumption_resolution, [], [f3948, f841])).
fof(f841, plain, ((e20 = op2(e21, e23)) | ~ spl18_97), inference(avatar_component_clause, [], [f839])).
fof(f839, plain, (spl18_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_97])])).
fof(f3948, plain, (~ (e20 = op2(e21, e23)) | (~ spl18_33 | ~ spl18_254 | spl18_259)), inference(forward_demodulation, [], [f3947, f1219])).
fof(f1219, plain, (e20 = h2(e10)), inference(forward_demodulation, [], [f1218, f1216])).
fof(f1216, plain, (e20 = op2(h2(e12), e21)), inference(backward_demodulation, [], [f315, f324])).
fof(f324, plain, (op2(e21, e21) = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f315, plain, (e20 = op2(op2(e21, e21), e21)), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(e21, op2(op2(e21, e21), e21))) & (e22 = op2(e21, e21)) & (e20 = op2(op2(e21, e21), e21))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax13)).
fof(f1218, plain, (h2(e10) = op2(h2(e12), e21)), inference(forward_demodulation, [], [f323, f324])).
fof(f323, plain, (op2(op2(e21, e21), e21) = h2(e10)), inference(cnf_transformation, [], [f15])).
fof(f3947, plain, (~ (op2(e21, e23) = h2(e10)) | (~ spl18_33 | ~ spl18_254 | spl18_259)), inference(forward_demodulation, [], [f3946, f537])).
fof(f537, plain, ((e10 = op1(e11, e13)) | ~ spl18_33), inference(avatar_component_clause, [], [f535])).
fof(f535, plain, (spl18_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_33])])).
fof(f3946, plain, (~ (op2(e21, e23) = h2(op1(e11, e13))) | (~ spl18_254 | spl18_259)), inference(forward_demodulation, [], [f1758, f1737])).
fof(f1758, plain, (~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | spl18_259), inference(avatar_component_clause, [], [f1756])).
fof(f1756, plain, (spl18_259 <=> (h2(op1(e11, e13)) = op2(e21, h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_259])])).
fof(f3918, plain, (~ spl18_25 | spl18_257), inference(avatar_contradiction_clause, [], [f3917])).
fof(f3917, plain, ($false | (~ spl18_25 | spl18_257)), inference(subsumption_resolution, [], [f3916, f1219])).
fof(f3916, plain, (~ (e20 = h2(e10)) | (~ spl18_25 | spl18_257)), inference(forward_demodulation, [], [f1750, f503])).
fof(f503, plain, ((e10 = op1(e12, e11)) | ~ spl18_25), inference(avatar_component_clause, [], [f501])).
fof(f501, plain, (spl18_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_25])])).
fof(f1750, plain, (~ (e20 = h2(op1(e12, e11))) | spl18_257), inference(avatar_component_clause, [], [f1748])).
fof(f1748, plain, (spl18_257 <=> (e20 = h2(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_257])])).
fof(f3888, plain, (~ spl18_15 | ~ spl18_79 | ~ spl18_193 | ~ spl18_254 | spl18_256), inference(avatar_contradiction_clause, [], [f3887])).
fof(f3887, plain, ($false | (~ spl18_15 | ~ spl18_79 | ~ spl18_193 | ~ spl18_254 | spl18_256)), inference(subsumption_resolution, [], [f3886, f764])).
fof(f764, plain, ((e22 = op2(e23, e20)) | ~ spl18_79), inference(avatar_component_clause, [], [f762])).
fof(f762, plain, (spl18_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_79])])).
fof(f3886, plain, (~ (e22 = op2(e23, e20)) | (~ spl18_15 | ~ spl18_193 | ~ spl18_254 | spl18_256)), inference(forward_demodulation, [], [f3885, f1370])).
fof(f3885, plain, (~ (op2(e23, e20) = h2(e12)) | (~ spl18_15 | ~ spl18_254 | spl18_256)), inference(forward_demodulation, [], [f3884, f460])).
fof(f460, plain, ((e12 = op1(e13, e10)) | ~ spl18_15), inference(avatar_component_clause, [], [f458])).
fof(f458, plain, (spl18_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f3884, plain, (~ (op2(e23, e20) = h2(op1(e13, e10))) | (~ spl18_254 | spl18_256)), inference(forward_demodulation, [], [f1746, f1737])).
fof(f1746, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | spl18_256), inference(avatar_component_clause, [], [f1744])).
fof(f1744, plain, (spl18_256 <=> (h2(op1(e13, e10)) = op2(h2(e13), e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_256])])).
fof(f3858, plain, (~ spl18_12 | ~ spl18_76 | ~ spl18_254 | spl18_255), inference(avatar_contradiction_clause, [], [f3857])).
fof(f3857, plain, ($false | (~ spl18_12 | ~ spl18_76 | ~ spl18_254 | spl18_255)), inference(subsumption_resolution, [], [f3856, f751])).
fof(f751, plain, ((e23 = op2(e23, e21)) | ~ spl18_76), inference(avatar_component_clause, [], [f749])).
fof(f749, plain, (spl18_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_76])])).
fof(f3856, plain, (~ (e23 = op2(e23, e21)) | (~ spl18_12 | ~ spl18_254 | spl18_255)), inference(forward_demodulation, [], [f3855, f1737])).
fof(f3855, plain, (~ (op2(e23, e21) = h2(e13)) | (~ spl18_12 | ~ spl18_254 | spl18_255)), inference(forward_demodulation, [], [f3854, f447])).
fof(f447, plain, ((e13 = op1(e13, e11)) | ~ spl18_12), inference(avatar_component_clause, [], [f445])).
fof(f445, plain, (spl18_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_12])])).
fof(f3854, plain, (~ (op2(e23, e21) = h2(op1(e13, e11))) | (~ spl18_254 | spl18_255)), inference(forward_demodulation, [], [f1742, f1737])).
fof(f1742, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | spl18_255), inference(avatar_component_clause, [], [f1740])).
fof(f1740, plain, (spl18_255 <=> (h2(op1(e13, e11)) = op2(h2(e13), e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_255])])).
fof(f3824, plain, (~ spl18_61 | ~ spl18_210 | spl18_266), inference(avatar_contradiction_clause, [], [f3823])).
fof(f3823, plain, ($false | (~ spl18_61 | ~ spl18_210 | spl18_266)), inference(subsumption_resolution, [], [f3822, f1460])).
fof(f1460, plain, ((e20 = h1(e12)) | ~ spl18_210), inference(avatar_component_clause, [], [f1459])).
fof(f1459, plain, (spl18_210 <=> (e20 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_210])])).
fof(f3822, plain, (~ (e20 = h1(e12)) | (~ spl18_61 | spl18_266)), inference(forward_demodulation, [], [f3821, f1219])).
fof(f3821, plain, (~ (h1(e12) = h2(e10)) | (~ spl18_61 | spl18_266)), inference(forward_demodulation, [], [f1786, f656])).
fof(f656, plain, ((e10 = op1(e10, e10)) | ~ spl18_61), inference(avatar_component_clause, [], [f654])).
fof(f654, plain, (spl18_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_61])])).
fof(f1786, plain, (~ (h1(e12) = h2(op1(e10, e10))) | spl18_266), inference(avatar_component_clause, [], [f1784])).
fof(f1784, plain, (spl18_266 <=> (h1(e12) = h2(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_266])])).
fof(f3788, plain, (~ spl18_48 | spl18_262), inference(avatar_contradiction_clause, [], [f3787])).
fof(f3787, plain, ($false | (~ spl18_48 | spl18_262)), inference(trivial_inequality_removal, [], [f3786])).
fof(f3786, plain, (~ (h2(e13) = h2(e13)) | (~ spl18_48 | spl18_262)), inference(forward_demodulation, [], [f1770, f600])).
fof(f600, plain, ((e13 = op1(e11, e10)) | ~ spl18_48), inference(avatar_component_clause, [], [f598])).
fof(f598, plain, (spl18_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_48])])).
fof(f1770, plain, (~ (h2(e13) = h2(op1(e11, e10))) | spl18_262), inference(avatar_component_clause, [], [f1768])).
fof(f1768, plain, (spl18_262 <=> (h2(e13) = h2(op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_262])])).
fof(f3759, plain, (~ spl18_30 | ~ spl18_94 | ~ spl18_193 | spl18_258), inference(avatar_contradiction_clause, [], [f3758])).
fof(f3758, plain, ($false | (~ spl18_30 | ~ spl18_94 | ~ spl18_193 | spl18_258)), inference(subsumption_resolution, [], [f3757, f828])).
fof(f828, plain, ((e21 = op2(e22, e20)) | ~ spl18_94), inference(avatar_component_clause, [], [f826])).
fof(f826, plain, (spl18_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_94])])).
fof(f3757, plain, (~ (e21 = op2(e22, e20)) | (~ spl18_30 | ~ spl18_193 | spl18_258)), inference(forward_demodulation, [], [f3756, f322])).
fof(f3756, plain, (~ (op2(e22, e20) = h2(e11)) | (~ spl18_30 | ~ spl18_193 | spl18_258)), inference(forward_demodulation, [], [f3755, f524])).
fof(f524, plain, ((e11 = op1(e12, e10)) | ~ spl18_30), inference(avatar_component_clause, [], [f522])).
fof(f522, plain, (spl18_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_30])])).
fof(f3755, plain, (~ (op2(e22, e20) = h2(op1(e12, e10))) | (~ spl18_193 | spl18_258)), inference(forward_demodulation, [], [f1754, f1370])).
fof(f1754, plain, (~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | spl18_258), inference(avatar_component_clause, [], [f1752])).
fof(f1752, plain, (spl18_258 <=> (h2(op1(e12, e10)) = op2(h2(e12), e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_258])])).
fof(f3730, plain, (~ spl18_2 | ~ spl18_66 | spl18_253 | ~ spl18_254), inference(avatar_contradiction_clause, [], [f3729])).
fof(f3729, plain, ($false | (~ spl18_2 | ~ spl18_66 | spl18_253 | ~ spl18_254)), inference(subsumption_resolution, [], [f3728, f709])).
fof(f709, plain, ((e21 = op2(e23, e23)) | ~ spl18_66), inference(avatar_component_clause, [], [f707])).
fof(f707, plain, (spl18_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_66])])).
fof(f3728, plain, (~ (e21 = op2(e23, e23)) | (~ spl18_2 | spl18_253 | ~ spl18_254)), inference(forward_demodulation, [], [f3727, f322])).
fof(f3727, plain, (~ (op2(e23, e23) = h2(e11)) | (~ spl18_2 | spl18_253 | ~ spl18_254)), inference(forward_demodulation, [], [f3726, f405])).
fof(f405, plain, ((e11 = op1(e13, e13)) | ~ spl18_2), inference(avatar_component_clause, [], [f403])).
fof(f403, plain, (spl18_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_2])])).
fof(f3726, plain, (~ (op2(e23, e23) = h2(op1(e13, e13))) | (spl18_253 | ~ spl18_254)), inference(forward_demodulation, [], [f1734, f1737])).
fof(f1734, plain, (~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | spl18_253), inference(avatar_component_clause, [], [f1732])).
fof(f1732, plain, (spl18_253 <=> (h2(op1(e13, e13)) = op2(h2(e13), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_253])])).
fof(f3719, plain, (~ spl18_19 | ~ spl18_83 | ~ spl18_193 | spl18_251 | ~ spl18_254), inference(avatar_contradiction_clause, [], [f3718])).
fof(f3718, plain, ($false | (~ spl18_19 | ~ spl18_83 | ~ spl18_193 | spl18_251 | ~ spl18_254)), inference(subsumption_resolution, [], [f3716, f1370])).
fof(f3716, plain, (~ (e22 = h2(e12)) | (~ spl18_19 | ~ spl18_83 | ~ spl18_193 | spl18_251 | ~ spl18_254)), inference(backward_demodulation, [], [f3697, f477])).
fof(f477, plain, ((e12 = op1(e12, e13)) | ~ spl18_19), inference(avatar_component_clause, [], [f475])).
fof(f475, plain, (spl18_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_19])])).
fof(f3697, plain, (~ (e22 = h2(op1(e12, e13))) | (~ spl18_83 | ~ spl18_193 | spl18_251 | ~ spl18_254)), inference(backward_demodulation, [], [f3655, f781])).
fof(f781, plain, ((e22 = op2(e22, e23)) | ~ spl18_83), inference(avatar_component_clause, [], [f779])).
fof(f779, plain, (spl18_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_83])])).
fof(f3655, plain, (~ (op2(e22, e23) = h2(op1(e12, e13))) | (~ spl18_193 | spl18_251 | ~ spl18_254)), inference(forward_demodulation, [], [f3654, f1370])).
fof(f3654, plain, (~ (h2(op1(e12, e13)) = op2(h2(e12), e23)) | (spl18_251 | ~ spl18_254)), inference(forward_demodulation, [], [f1726, f1737])).
fof(f1726, plain, (~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | spl18_251), inference(avatar_component_clause, [], [f1724])).
fof(f1724, plain, (spl18_251 <=> (h2(op1(e12, e13)) = op2(h2(e12), h2(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_251])])).
fof(f3665, plain, (~ spl18_120 | ~ spl18_248), inference(avatar_split_clause, [], [f3659, f1681, f936])).
fof(f936, plain, (spl18_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_120])])).
fof(f1681, plain, (spl18_248 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_248])])).
fof(f3659, plain, (~ (e23 = op2(e20, e22)) | ~ spl18_248), inference(backward_demodulation, [], [f1220, f1682])).
fof(f1682, plain, ((e23 = h3(e12)) | ~ spl18_248), inference(avatar_component_clause, [], [f1681])).
fof(f1220, plain, ~ (op2(e20, e22) = h3(e12)), inference(backward_demodulation, [], [f217, f328])).
fof(f328, plain, (op2(e22, e22) = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((h3(e13) = op2(e22, op2(op2(e22, e22), e22))) & (op2(e22, e22) = h3(e12)) & (h3(e10) = op2(op2(e22, e22), e22)) & (e22 = h3(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax16)).
fof(f217, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax6)).
fof(f3639, plain, (spl18_88 | ~ spl18_79 | ~ spl18_159), inference(avatar_split_clause, [], [f3602, f1151, f762, f800])).
fof(f800, plain, (spl18_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_88])])).
fof(f1151, plain, (spl18_159 <=> (e23 = op2(op2(e23, e20), op2(e23, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_159])])).
fof(f3602, plain, ((e23 = op2(e22, e22)) | (~ spl18_79 | ~ spl18_159)), inference(backward_demodulation, [], [f1153, f764])).
fof(f1153, plain, ((e23 = op2(op2(e23, e20), op2(e23, e20))) | ~ spl18_159), inference(avatar_component_clause, [], [f1151])).
fof(f3631, plain, (spl18_24 | ~ spl18_15 | ~ spl18_140), inference(avatar_split_clause, [], [f3624, f1059, f458, f496])).
fof(f496, plain, (spl18_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_24])])).
fof(f1059, plain, (spl18_140 <=> (e13 = op1(op1(e13, e10), op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_140])])).
fof(f3624, plain, ((e13 = op1(e12, e12)) | (~ spl18_15 | ~ spl18_140)), inference(backward_demodulation, [], [f1061, f460])).
fof(f1061, plain, ((e13 = op1(op1(e13, e10), op1(e13, e10))) | ~ spl18_140), inference(avatar_component_clause, [], [f1059])).
fof(f3628, plain, (~ spl18_15 | ~ spl18_23 | ~ spl18_140), inference(avatar_contradiction_clause, [], [f3627])).
fof(f3627, plain, ($false | (~ spl18_15 | ~ spl18_23 | ~ spl18_140)), inference(subsumption_resolution, [], [f3626, f257])).
fof(f257, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax7)).
fof(f3626, plain, ((e12 = e13) | (~ spl18_15 | ~ spl18_23 | ~ spl18_140)), inference(forward_demodulation, [], [f3624, f494])).
fof(f494, plain, ((e12 = op1(e12, e12)) | ~ spl18_23), inference(avatar_component_clause, [], [f492])).
fof(f492, plain, (spl18_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_23])])).
fof(f3605, plain, (~ spl18_79 | ~ spl18_87 | ~ spl18_159), inference(avatar_contradiction_clause, [], [f3604])).
fof(f3604, plain, ($false | (~ spl18_79 | ~ spl18_87 | ~ spl18_159)), inference(subsumption_resolution, [], [f3603, f263])).
fof(f263, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax8)).
fof(f3603, plain, ((e22 = e23) | (~ spl18_79 | ~ spl18_87 | ~ spl18_159)), inference(forward_demodulation, [], [f3602, f798])).
fof(f798, plain, ((e22 = op2(e22, e22)) | ~ spl18_87), inference(avatar_component_clause, [], [f796])).
fof(f796, plain, (spl18_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_87])])).
fof(f3559, plain, (spl18_66 | ~ spl18_112 | ~ spl18_161), inference(avatar_split_clause, [], [f3558, f1161, f902, f707])).
fof(f902, plain, (spl18_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_112])])).
fof(f1161, plain, (spl18_161 <=> (e21 = op2(op2(e21, e20), op2(e21, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_161])])).
fof(f3558, plain, ((e21 = op2(e23, e23)) | (~ spl18_112 | ~ spl18_161)), inference(forward_demodulation, [], [f1163, f904])).
fof(f904, plain, ((e23 = op2(e21, e20)) | ~ spl18_112), inference(avatar_component_clause, [], [f902])).
fof(f1163, plain, ((e21 = op2(op2(e21, e20), op2(e21, e20))) | ~ spl18_161), inference(avatar_component_clause, [], [f1161])).
fof(f3549, plain, (~ spl18_74 | ~ spl18_173), inference(avatar_split_clause, [], [f3541, f1268, f741])).
fof(f741, plain, (spl18_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_74])])).
fof(f1268, plain, (spl18_173 <=> (e21 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_173])])).
fof(f3541, plain, (~ (e21 = op2(e23, e21)) | ~ spl18_173), inference(backward_demodulation, [], [f1233, f1269])).
fof(f1269, plain, ((e21 = h4(e12)) | ~ spl18_173), inference(avatar_component_clause, [], [f1268])).
fof(f1233, plain, ~ (op2(e23, e21) = h4(e12)), inference(backward_demodulation, [], [f250, f332])).
fof(f332, plain, (op2(e23, e23) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((h4(e13) = op2(e23, op2(op2(e23, e23), e23))) & (op2(e23, e23) = h4(e12)) & (h4(e10) = op2(op2(e23, e23), e23)) & (e23 = h4(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax17)).
fof(f250, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3548, plain, (~ spl18_98 | ~ spl18_173), inference(avatar_split_clause, [], [f3539, f1268, f843])).
fof(f843, plain, (spl18_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_98])])).
fof(f3539, plain, (~ (e21 = op2(e21, e23)) | ~ spl18_173), inference(backward_demodulation, [], [f1230, f1269])).
fof(f1230, plain, ~ (op2(e21, e23) = h4(e12)), inference(backward_demodulation, [], [f226, f332])).
fof(f226, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3515, plain, (~ spl18_77 | ~ spl18_210), inference(avatar_split_clause, [], [f3507, f1459, f754])).
fof(f754, plain, (spl18_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_77])])).
fof(f3507, plain, (~ (e20 = op2(e23, e20)) | ~ spl18_210), inference(backward_demodulation, [], [f1198, f1460])).
fof(f1198, plain, ~ (op2(e23, e20) = h1(e12)), inference(backward_demodulation, [], [f206, f320])).
fof(f320, plain, (op2(e20, e20) = h1(e12)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((h1(e13) = op2(e20, op2(op2(e20, e20), e20))) & (op2(e20, e20) = h1(e12)) & (h1(e10) = op2(op2(e20, e20), e20)) & (e20 = h1(e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax14)).
fof(f206, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3491, plain, (~ spl18_69 | ~ spl18_5 | ~ spl18_193 | spl18_252 | ~ spl18_254), inference(avatar_split_clause, [], [f3490, f1736, f1728, f1369, f416, f720])).
fof(f720, plain, (spl18_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_69])])).
fof(f416, plain, (spl18_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_5])])).
fof(f1728, plain, (spl18_252 <=> (h2(op1(e13, e12)) = op2(h2(e13), h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_252])])).
fof(f3490, plain, (~ (e20 = op2(e23, e22)) | (~ spl18_5 | ~ spl18_193 | spl18_252 | ~ spl18_254)), inference(forward_demodulation, [], [f3489, f1219])).
fof(f3489, plain, (~ (op2(e23, e22) = h2(e10)) | (~ spl18_5 | ~ spl18_193 | spl18_252 | ~ spl18_254)), inference(forward_demodulation, [], [f3130, f418])).
fof(f418, plain, ((e10 = op1(e13, e12)) | ~ spl18_5), inference(avatar_component_clause, [], [f416])).
fof(f3130, plain, (~ (op2(e23, e22) = h2(op1(e13, e12))) | (~ spl18_193 | spl18_252 | ~ spl18_254)), inference(forward_demodulation, [], [f3129, f1737])).
fof(f3129, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), e22)) | (~ spl18_193 | spl18_252)), inference(forward_demodulation, [], [f1730, f1370])).
fof(f1730, plain, (~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | spl18_252), inference(avatar_component_clause, [], [f1728])).
fof(f3469, plain, (~ spl18_2 | ~ spl18_3), inference(avatar_contradiction_clause, [], [f3468])).
fof(f3468, plain, ($false | (~ spl18_2 | ~ spl18_3)), inference(subsumption_resolution, [], [f3467, f255])).
fof(f255, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f3467, plain, ((e11 = e12) | (~ spl18_2 | ~ spl18_3)), inference(backward_demodulation, [], [f409, f405])).
fof(f409, plain, ((e12 = op1(e13, e13)) | ~ spl18_3), inference(avatar_component_clause, [], [f407])).
fof(f407, plain, (spl18_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_3])])).
fof(f3449, plain, (~ spl18_25 | ~ spl18_26), inference(avatar_contradiction_clause, [], [f3448])).
fof(f3448, plain, ($false | (~ spl18_25 | ~ spl18_26)), inference(subsumption_resolution, [], [f3447, f252])).
fof(f252, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f3447, plain, ((e10 = e11) | (~ spl18_25 | ~ spl18_26)), inference(forward_demodulation, [], [f507, f503])).
fof(f507, plain, ((e11 = op1(e12, e11)) | ~ spl18_26), inference(avatar_component_clause, [], [f505])).
fof(f505, plain, (spl18_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_26])])).
fof(f3433, plain, (~ spl18_53 | ~ spl18_61), inference(avatar_split_clause, [], [f3424, f654, f620])).
fof(f620, plain, (spl18_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_53])])).
fof(f3424, plain, (~ (e10 = op1(e10, e12)) | ~ spl18_61), inference(backward_demodulation, [], [f181, f656])).
fof(f181, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax5)).
fof(f3396, plain, (~ spl18_98 | spl18_106 | ~ spl18_165), inference(avatar_contradiction_clause, [], [f3395])).
fof(f3395, plain, ($false | (~ spl18_98 | spl18_106 | ~ spl18_165)), inference(subsumption_resolution, [], [f3391, f878])).
fof(f878, plain, (~ (e21 = op2(e21, e21)) | spl18_106), inference(avatar_component_clause, [], [f877])).
fof(f877, plain, (spl18_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_106])])).
fof(f3391, plain, ((e21 = op2(e21, e21)) | (~ spl18_98 | ~ spl18_165)), inference(backward_demodulation, [], [f1183, f845])).
fof(f845, plain, ((e21 = op2(e21, e23)) | ~ spl18_98), inference(avatar_component_clause, [], [f843])).
fof(f1183, plain, ((e21 = op2(op2(e21, e23), op2(e21, e23))) | ~ spl18_165), inference(avatar_component_clause, [], [f1181])).
fof(f1181, plain, (spl18_165 <=> (e21 = op2(op2(e21, e23), op2(e21, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_165])])).
fof(f3376, plain, (~ spl18_76 | ~ spl18_124), inference(avatar_split_clause, [], [f3373, f953, f749])).
fof(f953, plain, (spl18_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_124])])).
fof(f3373, plain, (~ (e23 = op2(e23, e21)) | ~ spl18_124), inference(backward_demodulation, [], [f212, f955])).
fof(f955, plain, ((e23 = op2(e20, e21)) | ~ spl18_124), inference(avatar_component_clause, [], [f953])).
fof(f212, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3370, plain, (spl18_2 | ~ spl18_48 | ~ spl18_142), inference(avatar_split_clause, [], [f3369, f1069, f598, f403])).
fof(f1069, plain, (spl18_142 <=> (e11 = op1(op1(e11, e10), op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_142])])).
fof(f3369, plain, ((e11 = op1(e13, e13)) | (~ spl18_48 | ~ spl18_142)), inference(forward_demodulation, [], [f1071, f600])).
fof(f1071, plain, ((e11 = op1(op1(e11, e10), op1(e11, e10))) | ~ spl18_142), inference(avatar_component_clause, [], [f1069])).
fof(f3341, plain, (~ spl18_122 | ~ spl18_206), inference(avatar_split_clause, [], [f3340, f1439, f945])).
fof(f1439, plain, (spl18_206 <=> (e21 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_206])])).
fof(f3340, plain, (~ (e21 = op2(e20, e21)) | ~ spl18_206), inference(forward_demodulation, [], [f1199, f1440])).
fof(f1440, plain, ((e21 = h1(e12)) | ~ spl18_206), inference(avatar_component_clause, [], [f1439])).
fof(f1199, plain, ~ (op2(e20, e21) = h1(e12)), inference(backward_demodulation, [], [f228, f320])).
fof(f228, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f3282, plain, (~ spl18_1 | ~ spl18_33), inference(avatar_split_clause, [], [f3100, f535, f399])).
fof(f399, plain, (spl18_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f3100, plain, (~ (e10 = op1(e13, e13)) | ~ spl18_33), inference(forward_demodulation, [], [f178, f537])).
fof(f178, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3259, plain, (spl18_1 | ~ spl18_52 | ~ spl18_147), inference(avatar_contradiction_clause, [], [f3258])).
fof(f3258, plain, ($false | (spl18_1 | ~ spl18_52 | ~ spl18_147)), inference(subsumption_resolution, [], [f3256, f400])).
fof(f400, plain, (~ (e10 = op1(e13, e13)) | spl18_1), inference(avatar_component_clause, [], [f399])).
fof(f3256, plain, ((e10 = op1(e13, e13)) | (~ spl18_52 | ~ spl18_147)), inference(backward_demodulation, [], [f1096, f617])).
fof(f1096, plain, ((e10 = op1(op1(e10, e13), op1(e10, e13))) | ~ spl18_147), inference(avatar_component_clause, [], [f1094])).
fof(f1094, plain, (spl18_147 <=> (e10 = op1(op1(e10, e13), op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_147])])).
fof(f3257, plain, (~ spl18_4 | ~ spl18_52), inference(avatar_split_clause, [], [f3255, f615, f411])).
fof(f411, plain, (spl18_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_4])])).
fof(f3255, plain, (~ (e13 = op1(e13, e13)) | ~ spl18_52), inference(backward_demodulation, [], [f176, f617])).
fof(f176, plain, ~ (op1(e10, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3245, plain, (~ spl18_31 | ~ spl18_63), inference(avatar_split_clause, [], [f3238, f662, f526])).
fof(f526, plain, (spl18_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_31])])).
fof(f662, plain, (spl18_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl18_63])])).
fof(f3238, plain, (~ (e12 = op1(e12, e10)) | ~ spl18_63), inference(backward_demodulation, [], [f157, f664])).
fof(f664, plain, ((op1(e10, e10) = e12) | ~ spl18_63), inference(avatar_component_clause, [], [f662])).
fof(f157, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f3192, plain, (~ spl18_125 | ~ spl18_126), inference(avatar_contradiction_clause, [], [f3191])).
fof(f3191, plain, ($false | (~ spl18_125 | ~ spl18_126)), inference(subsumption_resolution, [], [f3190, f258])).
fof(f258, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f3190, plain, ((e20 = e21) | (~ spl18_125 | ~ spl18_126)), inference(backward_demodulation, [], [f964, f960])).
fof(f960, plain, ((e20 = op2(e20, e20)) | ~ spl18_125), inference(avatar_component_clause, [], [f958])).
fof(f958, plain, (spl18_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_125])])).
fof(f964, plain, ((op2(e20, e20) = e21) | ~ spl18_126), inference(avatar_component_clause, [], [f962])).
fof(f962, plain, (spl18_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl18_126])])).
fof(f3170, plain, (~ spl18_78 | ~ spl18_173), inference(avatar_split_clause, [], [f3159, f1268, f758])).
fof(f758, plain, (spl18_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_78])])).
fof(f3159, plain, (~ (e21 = op2(e23, e20)) | ~ spl18_173), inference(backward_demodulation, [], [f1232, f1269])).
fof(f1232, plain, ~ (op2(e23, e20) = h4(e12)), inference(backward_demodulation, [], [f248, f332])).
fof(f248, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3150, plain, (~ spl18_117 | ~ spl18_210), inference(avatar_split_clause, [], [f3144, f1459, f924])).
fof(f924, plain, (spl18_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_117])])).
fof(f3144, plain, (~ (e20 = op2(e20, e22)) | ~ spl18_210), inference(backward_demodulation, [], [f1200, f1460])).
fof(f1200, plain, ~ (op2(e20, e22) = h1(e12)), inference(backward_demodulation, [], [f229, f320])).
fof(f229, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3110, plain, (spl18_62 | ~ spl18_33 | ~ spl18_146), inference(avatar_split_clause, [], [f3090, f1089, f535, f658])).
fof(f658, plain, (spl18_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl18_62])])).
fof(f1089, plain, (spl18_146 <=> (e11 = op1(op1(e11, e13), op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_146])])).
fof(f3090, plain, ((op1(e10, e10) = e11) | (~ spl18_33 | ~ spl18_146)), inference(backward_demodulation, [], [f1091, f537])).
fof(f1091, plain, ((e11 = op1(op1(e11, e13), op1(e11, e13))) | ~ spl18_146), inference(avatar_component_clause, [], [f1089])).
fof(f3085, plain, (~ spl18_38 | ~ spl18_102 | ~ spl18_193 | spl18_260), inference(avatar_contradiction_clause, [], [f3084])).
fof(f3084, plain, ($false | (~ spl18_38 | ~ spl18_102 | ~ spl18_193 | spl18_260)), inference(subsumption_resolution, [], [f3082, f322])).
fof(f3082, plain, (~ (e21 = h2(e11)) | (~ spl18_38 | ~ spl18_102 | ~ spl18_193 | spl18_260)), inference(backward_demodulation, [], [f2988, f558])).
fof(f558, plain, ((e11 = op1(e11, e12)) | ~ spl18_38), inference(avatar_component_clause, [], [f556])).
fof(f556, plain, (spl18_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_38])])).
fof(f2988, plain, (~ (e21 = h2(op1(e11, e12))) | (~ spl18_102 | ~ spl18_193 | spl18_260)), inference(forward_demodulation, [], [f2927, f862])).
fof(f862, plain, ((e21 = op2(e21, e22)) | ~ spl18_102), inference(avatar_component_clause, [], [f860])).
fof(f860, plain, (spl18_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_102])])).
fof(f2927, plain, (~ (op2(e21, e22) = h2(op1(e11, e12))) | (~ spl18_193 | spl18_260)), inference(forward_demodulation, [], [f1762, f1370])).
fof(f1762, plain, (~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | spl18_260), inference(avatar_component_clause, [], [f1760])).
fof(f1760, plain, (spl18_260 <=> (h2(op1(e11, e12)) = op2(e21, h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_260])])).
fof(f3077, plain, (spl18_21 | ~ spl18_51 | ~ spl18_147), inference(avatar_contradiction_clause, [], [f3076])).
fof(f3076, plain, ($false | (spl18_21 | ~ spl18_51 | ~ spl18_147)), inference(subsumption_resolution, [], [f3074, f485])).
fof(f485, plain, (~ (e10 = op1(e12, e12)) | spl18_21), inference(avatar_component_clause, [], [f484])).
fof(f484, plain, (spl18_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_21])])).
fof(f3074, plain, ((e10 = op1(e12, e12)) | (~ spl18_51 | ~ spl18_147)), inference(backward_demodulation, [], [f1096, f613])).
fof(f613, plain, ((e12 = op1(e10, e13)) | ~ spl18_51), inference(avatar_component_clause, [], [f611])).
fof(f611, plain, (spl18_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_51])])).
fof(f3075, plain, (~ spl18_19 | ~ spl18_51), inference(avatar_split_clause, [], [f3072, f611, f475])).
fof(f3072, plain, (~ (e12 = op1(e12, e13)) | ~ spl18_51), inference(backward_demodulation, [], [f175, f613])).
fof(f175, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f3070, plain, (~ spl18_37 | ~ spl18_53), inference(avatar_split_clause, [], [f3068, f620, f552])).
fof(f552, plain, (spl18_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_37])])).
fof(f3068, plain, (~ (e10 = op1(e11, e12)) | ~ spl18_53), inference(backward_demodulation, [], [f168, f622])).
fof(f622, plain, ((e10 = op1(e10, e12)) | ~ spl18_53), inference(avatar_component_clause, [], [f620])).
fof(f168, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f3029, plain, (spl18_127 | ~ spl18_89 | ~ spl18_155), inference(avatar_split_clause, [], [f3028, f1132, f805, f966])).
fof(f966, plain, (spl18_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl18_127])])).
fof(f805, plain, (spl18_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_89])])).
fof(f1132, plain, (spl18_155 <=> (e22 = op2(op2(e22, e21), op2(e22, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_155])])).
fof(f3028, plain, ((op2(e20, e20) = e22) | (~ spl18_89 | ~ spl18_155)), inference(forward_demodulation, [], [f1134, f807])).
fof(f807, plain, ((e20 = op2(e22, e21)) | ~ spl18_89), inference(avatar_component_clause, [], [f805])).
fof(f1134, plain, ((e22 = op2(op2(e22, e21), op2(e22, e21))) | ~ spl18_155), inference(avatar_component_clause, [], [f1132])).
fof(f3025, plain, (spl18_96 | ~ spl18_202 | ~ spl18_286), inference(avatar_contradiction_clause, [], [f3024])).
fof(f3024, plain, ($false | (spl18_96 | ~ spl18_202 | ~ spl18_286)), inference(subsumption_resolution, [], [f3019, f835])).
fof(f835, plain, (~ (e23 = op2(e22, e20)) | spl18_96), inference(avatar_component_clause, [], [f834])).
fof(f834, plain, (spl18_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_96])])).
fof(f3019, plain, ((e23 = op2(e22, e20)) | (~ spl18_202 | ~ spl18_286)), inference(backward_demodulation, [], [f3012, f1420])).
fof(f1420, plain, ((e22 = h1(e12)) | ~ spl18_202), inference(avatar_component_clause, [], [f1419])).
fof(f1419, plain, (spl18_202 <=> (e22 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_202])])).
fof(f3012, plain, ((e23 = op2(h1(e12), e20)) | ~ spl18_286), inference(forward_demodulation, [], [f1203, f1920])).
fof(f1920, plain, ((e23 = h1(e10)) | ~ spl18_286), inference(avatar_component_clause, [], [f1919])).
fof(f1919, plain, (spl18_286 <=> (e23 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_286])])).
fof(f1203, plain, (h1(e10) = op2(h1(e12), e20)), inference(forward_demodulation, [], [f319, f320])).
fof(f319, plain, (h1(e10) = op2(op2(e20, e20), e20)), inference(cnf_transformation, [], [f14])).
fof(f3022, plain, (~ spl18_95 | ~ spl18_202), inference(avatar_split_clause, [], [f3016, f1419, f830])).
fof(f830, plain, (spl18_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_95])])).
fof(f3016, plain, (~ (e22 = op2(e22, e20)) | ~ spl18_202), inference(backward_demodulation, [], [f1197, f1420])).
fof(f1197, plain, ~ (op2(e22, e20) = h1(e12)), inference(backward_demodulation, [], [f205, f320])).
fof(f205, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f3011, plain, (spl18_126 | ~ spl18_97 | ~ spl18_165), inference(avatar_split_clause, [], [f2945, f1181, f839, f962])).
fof(f2945, plain, ((op2(e20, e20) = e21) | (~ spl18_97 | ~ spl18_165)), inference(forward_demodulation, [], [f1183, f841])).
fof(f2997, plain, (spl18_63 | ~ spl18_25 | ~ spl18_136), inference(avatar_split_clause, [], [f2747, f1040, f501, f662])).
fof(f1040, plain, (spl18_136 <=> (e12 = op1(op1(e12, e11), op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_136])])).
fof(f2747, plain, ((op1(e10, e10) = e12) | (~ spl18_25 | ~ spl18_136)), inference(forward_demodulation, [], [f1042, f503])).
fof(f1042, plain, ((e12 = op1(op1(e12, e11), op1(e12, e11))) | ~ spl18_136), inference(avatar_component_clause, [], [f1040])).
fof(f2995, plain, (~ spl18_55 | ~ spl18_7), inference(avatar_split_clause, [], [f2994, f424, f628])).
fof(f424, plain, (spl18_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_7])])).
fof(f2994, plain, (~ (e12 = op1(e10, e12)) | ~ spl18_7), inference(forward_demodulation, [], [f170, f426])).
fof(f426, plain, ((e12 = op1(e13, e12)) | ~ spl18_7), inference(avatar_component_clause, [], [f424])).
fof(f170, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2972, plain, (~ spl18_105 | ~ spl18_107), inference(avatar_contradiction_clause, [], [f2971])).
fof(f2971, plain, ($false | (~ spl18_105 | ~ spl18_107)), inference(subsumption_resolution, [], [f2970, f259])).
fof(f259, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f2970, plain, ((e20 = e22) | (~ spl18_105 | ~ spl18_107)), inference(backward_demodulation, [], [f883, f875])).
fof(f875, plain, ((e20 = op2(e21, e21)) | ~ spl18_105), inference(avatar_component_clause, [], [f873])).
fof(f873, plain, (spl18_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_105])])).
fof(f883, plain, ((e22 = op2(e21, e21)) | ~ spl18_107), inference(avatar_component_clause, [], [f881])).
fof(f881, plain, (spl18_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_107])])).
fof(f2960, plain, (~ spl18_34 | spl18_42 | ~ spl18_146), inference(avatar_contradiction_clause, [], [f2959])).
fof(f2959, plain, ($false | (~ spl18_34 | spl18_42 | ~ spl18_146)), inference(subsumption_resolution, [], [f2958, f574])).
fof(f574, plain, (~ (e11 = op1(e11, e11)) | spl18_42), inference(avatar_component_clause, [], [f573])).
fof(f573, plain, (spl18_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_42])])).
fof(f2958, plain, ((e11 = op1(e11, e11)) | (~ spl18_34 | ~ spl18_146)), inference(forward_demodulation, [], [f1091, f541])).
fof(f541, plain, ((e11 = op1(e11, e13)) | ~ spl18_34), inference(avatar_component_clause, [], [f539])).
fof(f539, plain, (spl18_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_34])])).
fof(f2957, plain, (~ spl18_49 | spl18_61 | ~ spl18_147), inference(avatar_contradiction_clause, [], [f2956])).
fof(f2956, plain, ($false | (~ spl18_49 | spl18_61 | ~ spl18_147)), inference(subsumption_resolution, [], [f2955, f655])).
fof(f655, plain, (~ (e10 = op1(e10, e10)) | spl18_61), inference(avatar_component_clause, [], [f654])).
fof(f2955, plain, ((e10 = op1(e10, e10)) | (~ spl18_49 | ~ spl18_147)), inference(forward_demodulation, [], [f1096, f605])).
fof(f605, plain, ((e10 = op1(e10, e13)) | ~ spl18_49), inference(avatar_component_clause, [], [f603])).
fof(f603, plain, (spl18_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_49])])).
fof(f2944, plain, (spl18_85 | ~ spl18_115 | ~ spl18_166), inference(avatar_contradiction_clause, [], [f2943])).
fof(f2943, plain, ($false | (spl18_85 | ~ spl18_115 | ~ spl18_166)), inference(subsumption_resolution, [], [f2942, f789])).
fof(f789, plain, (~ (e20 = op2(e22, e22)) | spl18_85), inference(avatar_component_clause, [], [f788])).
fof(f788, plain, (spl18_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_85])])).
fof(f2942, plain, ((e20 = op2(e22, e22)) | (~ spl18_115 | ~ spl18_166)), inference(forward_demodulation, [], [f1188, f917])).
fof(f917, plain, ((e22 = op2(e20, e23)) | ~ spl18_115), inference(avatar_component_clause, [], [f915])).
fof(f915, plain, (spl18_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_115])])).
fof(f1188, plain, ((e20 = op2(op2(e20, e23), op2(e20, e23))) | ~ spl18_166), inference(avatar_component_clause, [], [f1186])).
fof(f1186, plain, (spl18_166 <=> (e20 = op2(op2(e20, e23), op2(e20, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_166])])).
fof(f2907, plain, (~ spl18_58 | ~ spl18_60), inference(avatar_contradiction_clause, [], [f2906])).
fof(f2906, plain, ($false | (~ spl18_58 | ~ spl18_60)), inference(subsumption_resolution, [], [f2904, f256])).
fof(f256, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f2904, plain, ((e11 = e13) | (~ spl18_58 | ~ spl18_60)), inference(backward_demodulation, [], [f651, f643])).
fof(f651, plain, ((e13 = op1(e10, e11)) | ~ spl18_60), inference(avatar_component_clause, [], [f649])).
fof(f649, plain, (spl18_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_60])])).
fof(f2861, plain, (spl18_21 | ~ spl18_63 | ~ spl18_143), inference(avatar_contradiction_clause, [], [f2860])).
fof(f2860, plain, ($false | (spl18_21 | ~ spl18_63 | ~ spl18_143)), inference(subsumption_resolution, [], [f2859, f485])).
fof(f2859, plain, ((e10 = op1(e12, e12)) | (~ spl18_63 | ~ spl18_143)), inference(forward_demodulation, [], [f1076, f664])).
fof(f1076, plain, ((e10 = op1(op1(e10, e10), op1(e10, e10))) | ~ spl18_143), inference(avatar_component_clause, [], [f1074])).
fof(f1074, plain, (spl18_143 <=> (e10 = op1(op1(e10, e10), op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_143])])).
fof(f2854, plain, (~ spl18_77 | spl18_128 | ~ spl18_159), inference(avatar_contradiction_clause, [], [f2853])).
fof(f2853, plain, ($false | (~ spl18_77 | spl18_128 | ~ spl18_159)), inference(subsumption_resolution, [], [f2852, f971])).
fof(f971, plain, (~ (op2(e20, e20) = e23) | spl18_128), inference(avatar_component_clause, [], [f970])).
fof(f970, plain, (spl18_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl18_128])])).
fof(f2852, plain, ((op2(e20, e20) = e23) | (~ spl18_77 | ~ spl18_159)), inference(forward_demodulation, [], [f1153, f756])).
fof(f756, plain, ((e20 = op2(e23, e20)) | ~ spl18_77), inference(avatar_component_clause, [], [f754])).
fof(f2845, plain, (spl18_286 | ~ spl18_112 | ~ spl18_206), inference(avatar_split_clause, [], [f2844, f1439, f902, f1919])).
fof(f2844, plain, ((e23 = h1(e10)) | (~ spl18_112 | ~ spl18_206)), inference(forward_demodulation, [], [f2839, f904])).
fof(f2839, plain, ((op2(e21, e20) = h1(e10)) | ~ spl18_206), inference(backward_demodulation, [], [f1203, f1440])).
fof(f2843, plain, (~ spl18_114 | ~ spl18_206), inference(avatar_split_clause, [], [f2838, f1439, f911])).
fof(f911, plain, (spl18_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_114])])).
fof(f2838, plain, (~ (e21 = op2(e20, e23)) | ~ spl18_206), inference(backward_demodulation, [], [f1201, f1440])).
fof(f1201, plain, ~ (op2(e20, e23) = h1(e12)), inference(backward_demodulation, [], [f230, f320])).
fof(f230, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f2804, plain, (~ spl18_120 | ~ spl18_124), inference(avatar_split_clause, [], [f2803, f953, f936])).
fof(f2803, plain, (~ (e23 = op2(e20, e22)) | ~ spl18_124), inference(forward_demodulation, [], [f231, f955])).
fof(f231, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f2783, plain, (~ spl18_8 | ~ spl18_24), inference(avatar_split_clause, [], [f2782, f496, f428])).
fof(f428, plain, (spl18_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_8])])).
fof(f2782, plain, (~ (e13 = op1(e13, e12)) | ~ spl18_24), inference(forward_demodulation, [], [f173, f498])).
fof(f498, plain, ((e13 = op1(e12, e12)) | ~ spl18_24), inference(avatar_component_clause, [], [f496])).
fof(f173, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2780, plain, (~ spl18_41 | ~ spl18_43), inference(avatar_contradiction_clause, [], [f2779])).
fof(f2779, plain, ($false | (~ spl18_41 | ~ spl18_43)), inference(subsumption_resolution, [], [f2778, f253])).
fof(f253, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2778, plain, ((e10 = e12) | (~ spl18_41 | ~ spl18_43)), inference(backward_demodulation, [], [f579, f571])).
fof(f571, plain, ((e10 = op1(e11, e11)) | ~ spl18_41), inference(avatar_component_clause, [], [f569])).
fof(f569, plain, (spl18_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_41])])).
fof(f2769, plain, (~ spl18_89 | ~ spl18_90), inference(avatar_contradiction_clause, [], [f2768])).
fof(f2768, plain, ($false | (~ spl18_89 | ~ spl18_90)), inference(subsumption_resolution, [], [f2767, f258])).
fof(f2767, plain, ((e20 = e21) | (~ spl18_89 | ~ spl18_90)), inference(forward_demodulation, [], [f811, f807])).
fof(f811, plain, ((e21 = op2(e22, e21)) | ~ spl18_90), inference(avatar_component_clause, [], [f809])).
fof(f809, plain, (spl18_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_90])])).
fof(f2746, plain, (spl18_22 | ~ spl18_43 | ~ spl18_137), inference(avatar_contradiction_clause, [], [f2745])).
fof(f2745, plain, ($false | (spl18_22 | ~ spl18_43 | ~ spl18_137)), inference(subsumption_resolution, [], [f2744, f489])).
fof(f489, plain, (~ (e11 = op1(e12, e12)) | spl18_22), inference(avatar_component_clause, [], [f488])).
fof(f488, plain, (spl18_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_22])])).
fof(f2744, plain, ((e11 = op1(e12, e12)) | (~ spl18_43 | ~ spl18_137)), inference(forward_demodulation, [], [f1047, f579])).
fof(f1047, plain, ((e11 = op1(op1(e11, e11), op1(e11, e11))) | ~ spl18_137), inference(avatar_component_clause, [], [f1045])).
fof(f1045, plain, (spl18_137 <=> (e11 = op1(op1(e11, e11), op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_137])])).
fof(f2732, plain, (spl18_86 | ~ spl18_107 | ~ spl18_156), inference(avatar_split_clause, [], [f2731, f1137, f881, f792])).
fof(f792, plain, (spl18_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_86])])).
fof(f1137, plain, (spl18_156 <=> (e21 = op2(op2(e21, e21), op2(e21, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_156])])).
fof(f2731, plain, ((e21 = op2(e22, e22)) | (~ spl18_107 | ~ spl18_156)), inference(forward_demodulation, [], [f1139, f883])).
fof(f1139, plain, ((e21 = op2(op2(e21, e21), op2(e21, e21))) | ~ spl18_156), inference(avatar_component_clause, [], [f1137])).
fof(f2699, plain, (~ spl18_100 | ~ spl18_254), inference(avatar_split_clause, [], [f2552, f1736, f851])).
fof(f851, plain, (spl18_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_100])])).
fof(f2552, plain, (~ (e23 = op2(e21, e23)) | ~ spl18_254), inference(forward_demodulation, [], [f1210, f1737])).
fof(f1210, plain, ~ (op2(e21, e23) = h2(e13)), inference(backward_demodulation, [], [f236, f1205])).
fof(f1205, plain, (op2(e21, e20) = h2(e13)), inference(forward_demodulation, [], [f325, f315])).
fof(f325, plain, (op2(e21, op2(op2(e21, e21), e21)) = h2(e13)), inference(cnf_transformation, [], [f15])).
fof(f236, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2698, plain, (~ spl18_99 | ~ spl18_193), inference(avatar_split_clause, [], [f2554, f1369, f847])).
fof(f847, plain, (spl18_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_99])])).
fof(f2554, plain, (~ (e22 = op2(e21, e23)) | ~ spl18_193), inference(forward_demodulation, [], [f1215, f1370])).
fof(f1215, plain, ~ (op2(e21, e23) = h2(e12)), inference(backward_demodulation, [], [f238, f324])).
fof(f238, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2697, plain, (~ spl18_97 | ~ spl18_113), inference(avatar_split_clause, [], [f2696, f907, f839])).
fof(f907, plain, (spl18_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_113])])).
fof(f2696, plain, (~ (e20 = op2(e21, e23)) | ~ spl18_113), inference(forward_demodulation, [], [f222, f909])).
fof(f909, plain, ((e20 = op2(e20, e23)) | ~ spl18_113), inference(avatar_component_clause, [], [f907])).
fof(f222, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2674, plain, (~ spl18_2 | ~ spl18_10), inference(avatar_split_clause, [], [f2672, f437, f403])).
fof(f437, plain, (spl18_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_10])])).
fof(f2672, plain, (~ (e11 = op1(e13, e13)) | ~ spl18_10), inference(backward_demodulation, [], [f202, f439])).
fof(f439, plain, ((e11 = op1(e13, e11)) | ~ spl18_10), inference(avatar_component_clause, [], [f437])).
fof(f202, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2660, plain, (~ spl18_2 | ~ spl18_34), inference(avatar_split_clause, [], [f2659, f539, f403])).
fof(f2659, plain, (~ (e11 = op1(e13, e13)) | ~ spl18_34), inference(backward_demodulation, [], [f178, f541])).
fof(f2653, plain, (~ spl18_33 | ~ spl18_37), inference(avatar_split_clause, [], [f2651, f552, f535])).
fof(f2651, plain, (~ (e10 = op1(e11, e13)) | ~ spl18_37), inference(backward_demodulation, [], [f191, f554])).
fof(f554, plain, ((e10 = op1(e11, e12)) | ~ spl18_37), inference(avatar_component_clause, [], [f552])).
fof(f191, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2646, plain, (~ spl18_1 | ~ spl18_49), inference(avatar_split_clause, [], [f2641, f603, f399])).
fof(f2641, plain, (~ (e10 = op1(e13, e13)) | ~ spl18_49), inference(backward_demodulation, [], [f176, f605])).
fof(f2645, plain, (~ spl18_33 | ~ spl18_49), inference(avatar_split_clause, [], [f2639, f603, f535])).
fof(f2639, plain, (~ (e10 = op1(e11, e13)) | ~ spl18_49), inference(backward_demodulation, [], [f174, f605])).
fof(f174, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2638, plain, (spl18_41 | ~ spl18_54 | ~ spl18_133), inference(avatar_split_clause, [], [f2636, f1026, f624, f569])).
fof(f624, plain, (spl18_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_54])])).
fof(f1026, plain, (spl18_133 <=> (e10 = op1(op1(e10, e12), op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_133])])).
fof(f2636, plain, ((e10 = op1(e11, e11)) | (~ spl18_54 | ~ spl18_133)), inference(backward_demodulation, [], [f1028, f626])).
fof(f626, plain, ((e11 = op1(e10, e12)) | ~ spl18_54), inference(avatar_component_clause, [], [f624])).
fof(f1028, plain, ((e10 = op1(op1(e10, e12), op1(e10, e12))) | ~ spl18_133), inference(avatar_component_clause, [], [f1026])).
fof(f2600, plain, (~ spl18_86 | spl18_185), inference(avatar_contradiction_clause, [], [f2599])).
fof(f2599, plain, ($false | (~ spl18_86 | spl18_185)), inference(subsumption_resolution, [], [f2598, f1331])).
fof(f1331, plain, (~ (e21 = h3(e12)) | spl18_185), inference(avatar_component_clause, [], [f1329])).
fof(f1329, plain, (spl18_185 <=> (e21 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_185])])).
fof(f2598, plain, ((e21 = h3(e12)) | ~ spl18_86), inference(backward_demodulation, [], [f328, f794])).
fof(f794, plain, ((e21 = op2(e22, e22)) | ~ spl18_86), inference(avatar_component_clause, [], [f792])).
fof(f2588, plain, (~ spl18_106 | ~ spl18_107), inference(avatar_contradiction_clause, [], [f2587])).
fof(f2587, plain, ($false | (~ spl18_106 | ~ spl18_107)), inference(subsumption_resolution, [], [f2586, f261])).
fof(f261, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f2586, plain, ((e21 = e22) | (~ spl18_106 | ~ spl18_107)), inference(backward_demodulation, [], [f883, f879])).
fof(f879, plain, ((e21 = op2(e21, e21)) | ~ spl18_106), inference(avatar_component_clause, [], [f877])).
fof(f2585, plain, (~ spl18_109 | ~ spl18_112), inference(avatar_contradiction_clause, [], [f2584])).
fof(f2584, plain, ($false | (~ spl18_109 | ~ spl18_112)), inference(subsumption_resolution, [], [f2583, f260])).
fof(f260, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f2583, plain, ((e20 = e23) | (~ spl18_109 | ~ spl18_112)), inference(backward_demodulation, [], [f904, f892])).
fof(f892, plain, ((e20 = op2(e21, e20)) | ~ spl18_109), inference(avatar_component_clause, [], [f890])).
fof(f890, plain, (spl18_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_109])])).
fof(f2572, plain, (~ spl18_128 | spl18_285), inference(avatar_contradiction_clause, [], [f2571])).
fof(f2571, plain, ($false | (~ spl18_128 | spl18_285)), inference(subsumption_resolution, [], [f2570, f1905])).
fof(f1905, plain, (~ (e23 = h1(e12)) | spl18_285), inference(avatar_component_clause, [], [f1903])).
fof(f1903, plain, (spl18_285 <=> (e23 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_285])])).
fof(f2570, plain, ((e23 = h1(e12)) | ~ spl18_128), inference(backward_demodulation, [], [f320, f972])).
fof(f972, plain, ((op2(e20, e20) = e23) | ~ spl18_128), inference(avatar_component_clause, [], [f970])).
fof(f2539, plain, (~ spl18_80 | ~ spl18_254), inference(avatar_split_clause, [], [f2382, f1736, f766])).
fof(f766, plain, (spl18_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_80])])).
fof(f2382, plain, (~ (e23 = op2(e23, e20)) | ~ spl18_254), inference(forward_demodulation, [], [f1207, f1737])).
fof(f1207, plain, ~ (op2(e23, e20) = h2(e13)), inference(backward_demodulation, [], [f208, f1205])).
fof(f208, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2525, plain, (~ spl18_23 | ~ spl18_7), inference(avatar_split_clause, [], [f2524, f424, f492])).
fof(f2524, plain, (~ (e12 = op1(e12, e12)) | ~ spl18_7), inference(forward_demodulation, [], [f173, f426])).
fof(f2498, plain, (~ spl18_30 | ~ spl18_31), inference(avatar_contradiction_clause, [], [f2497])).
fof(f2497, plain, ($false | (~ spl18_30 | ~ spl18_31)), inference(subsumption_resolution, [], [f2496, f255])).
fof(f2496, plain, ((e11 = e12) | (~ spl18_30 | ~ spl18_31)), inference(forward_demodulation, [], [f528, f524])).
fof(f528, plain, ((e12 = op1(e12, e10)) | ~ spl18_31), inference(avatar_component_clause, [], [f526])).
fof(f2495, plain, (~ spl18_42 | ~ spl18_43), inference(avatar_contradiction_clause, [], [f2494])).
fof(f2494, plain, ($false | (~ spl18_42 | ~ spl18_43)), inference(subsumption_resolution, [], [f2493, f255])).
fof(f2493, plain, ((e11 = e12) | (~ spl18_42 | ~ spl18_43)), inference(backward_demodulation, [], [f579, f575])).
fof(f575, plain, ((e11 = op1(e11, e11)) | ~ spl18_42), inference(avatar_component_clause, [], [f573])).
fof(f2483, plain, (spl18_1 | ~ spl18_56 | ~ spl18_133), inference(avatar_split_clause, [], [f2479, f1026, f632, f399])).
fof(f632, plain, (spl18_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_56])])).
fof(f2479, plain, ((e10 = op1(e13, e13)) | (~ spl18_56 | ~ spl18_133)), inference(backward_demodulation, [], [f1028, f634])).
fof(f634, plain, ((e13 = op1(e10, e12)) | ~ spl18_56), inference(avatar_component_clause, [], [f632])).
fof(f2482, plain, (~ spl18_24 | ~ spl18_56), inference(avatar_split_clause, [], [f2478, f632, f496])).
fof(f2478, plain, (~ (e13 = op1(e12, e12)) | ~ spl18_56), inference(backward_demodulation, [], [f169, f634])).
fof(f169, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2472, plain, (~ spl18_50 | ~ spl18_62), inference(avatar_split_clause, [], [f2467, f658, f607])).
fof(f607, plain, (spl18_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_50])])).
fof(f2467, plain, (~ (e11 = op1(e10, e13)) | ~ spl18_62), inference(backward_demodulation, [], [f182, f660])).
fof(f660, plain, ((op1(e10, e10) = e11) | ~ spl18_62), inference(avatar_component_clause, [], [f658])).
fof(f182, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2426, plain, (spl18_106 | ~ spl18_102 | ~ spl18_151), inference(avatar_split_clause, [], [f2423, f1113, f860, f877])).
fof(f1113, plain, (spl18_151 <=> (e21 = op2(op2(e21, e22), op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_151])])).
fof(f2423, plain, ((e21 = op2(e21, e21)) | (~ spl18_102 | ~ spl18_151)), inference(backward_demodulation, [], [f1115, f862])).
fof(f1115, plain, ((e21 = op2(op2(e21, e22), op2(e21, e22))) | ~ spl18_151), inference(avatar_component_clause, [], [f1113])).
fof(f2416, plain, (spl18_105 | ~ spl18_118 | ~ spl18_152), inference(avatar_split_clause, [], [f2411, f1118, f928, f873])).
fof(f928, plain, (spl18_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_118])])).
fof(f1118, plain, (spl18_152 <=> (e20 = op2(op2(e20, e22), op2(e20, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_152])])).
fof(f2411, plain, ((e20 = op2(e21, e21)) | (~ spl18_118 | ~ spl18_152)), inference(backward_demodulation, [], [f1120, f930])).
fof(f930, plain, ((e21 = op2(e20, e22)) | ~ spl18_118), inference(avatar_component_clause, [], [f928])).
fof(f1120, plain, ((e20 = op2(op2(e20, e22), op2(e20, e22))) | ~ spl18_152), inference(avatar_component_clause, [], [f1118])).
fof(f2400, plain, (~ spl18_127 | spl18_202), inference(avatar_contradiction_clause, [], [f2399])).
fof(f2399, plain, ($false | (~ spl18_127 | spl18_202)), inference(subsumption_resolution, [], [f2398, f1421])).
fof(f1421, plain, (~ (e22 = h1(e12)) | spl18_202), inference(avatar_component_clause, [], [f1419])).
fof(f2398, plain, ((e22 = h1(e12)) | ~ spl18_127), inference(backward_demodulation, [], [f320, f968])).
fof(f968, plain, ((op2(e20, e20) = e22) | ~ spl18_127), inference(avatar_component_clause, [], [f966])).
fof(f2397, plain, (~ spl18_123 | ~ spl18_193), inference(avatar_split_clause, [], [f2396, f1369, f949])).
fof(f949, plain, (spl18_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_123])])).
fof(f2396, plain, (~ (e22 = op2(e20, e21)) | ~ spl18_193), inference(forward_demodulation, [], [f1211, f1370])).
fof(f1211, plain, ~ (op2(e20, e21) = h2(e12)), inference(backward_demodulation, [], [f210, f324])).
fof(f210, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f2395, plain, (~ spl18_124 | ~ spl18_116), inference(avatar_split_clause, [], [f2394, f919, f953])).
fof(f2394, plain, (~ (e23 = op2(e20, e21)) | ~ spl18_116), inference(forward_demodulation, [], [f232, f921])).
fof(f232, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f2389, plain, (~ spl18_104 | ~ spl18_254), inference(avatar_split_clause, [], [f2388, f1736, f868])).
fof(f868, plain, (spl18_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_104])])).
fof(f2388, plain, (~ (e23 = op2(e21, e22)) | ~ spl18_254), inference(forward_demodulation, [], [f1209, f1737])).
fof(f1209, plain, ~ (op2(e21, e22) = h2(e13)), inference(backward_demodulation, [], [f235, f1205])).
fof(f235, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2387, plain, (~ spl18_103 | ~ spl18_193), inference(avatar_split_clause, [], [f2386, f1369, f864])).
fof(f864, plain, (spl18_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_103])])).
fof(f2386, plain, (~ (e22 = op2(e21, e22)) | ~ spl18_193), inference(forward_demodulation, [], [f1214, f1370])).
fof(f1214, plain, ~ (op2(e21, e22) = h2(e12)), inference(backward_demodulation, [], [f237, f324])).
fof(f237, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2375, plain, (~ spl18_64 | ~ spl18_48), inference(avatar_split_clause, [], [f2374, f598, f666])).
fof(f666, plain, (spl18_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl18_64])])).
fof(f2374, plain, (~ (op1(e10, e10) = e13) | ~ spl18_48), inference(forward_demodulation, [], [f156, f600])).
fof(f156, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f2371, plain, (~ spl18_62 | ~ spl18_58), inference(avatar_split_clause, [], [f2370, f641, f658])).
fof(f2370, plain, (~ (op1(e10, e10) = e11) | ~ spl18_58), inference(forward_demodulation, [], [f180, f643])).
fof(f180, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f2346, plain, (~ spl18_248 | ~ spl18_24 | ~ spl18_193 | spl18_250 | ~ spl18_254), inference(avatar_split_clause, [], [f2345, f1736, f1720, f1369, f496, f1681])).
fof(f1720, plain, (spl18_250 <=> (h2(op1(e12, e12)) = op2(h2(e12), h2(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_250])])).
fof(f2345, plain, (~ (e23 = h3(e12)) | (~ spl18_24 | ~ spl18_193 | spl18_250 | ~ spl18_254)), inference(forward_demodulation, [], [f2343, f1737])).
fof(f2343, plain, (~ (h2(e13) = h3(e12)) | (~ spl18_24 | ~ spl18_193 | spl18_250)), inference(backward_demodulation, [], [f2284, f498])).
fof(f2284, plain, (~ (h3(e12) = h2(op1(e12, e12))) | (~ spl18_193 | spl18_250)), inference(forward_demodulation, [], [f2280, f328])).
fof(f2280, plain, (~ (op2(e22, e22) = h2(op1(e12, e12))) | (~ spl18_193 | spl18_250)), inference(backward_demodulation, [], [f1722, f1370])).
fof(f1722, plain, (~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | spl18_250), inference(avatar_component_clause, [], [f1720])).
fof(f2341, plain, (~ spl18_22 | ~ spl18_30), inference(avatar_split_clause, [], [f2340, f522, f488])).
fof(f2340, plain, (~ (e11 = op1(e12, e12)) | ~ spl18_30), inference(backward_demodulation, [], [f193, f524])).
fof(f193, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2335, plain, (spl18_42 | ~ spl18_38 | ~ spl18_132), inference(avatar_split_clause, [], [f2332, f1021, f556, f573])).
fof(f1021, plain, (spl18_132 <=> (e11 = op1(op1(e11, e12), op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_132])])).
fof(f2332, plain, ((e11 = op1(e11, e11)) | (~ spl18_38 | ~ spl18_132)), inference(backward_demodulation, [], [f1023, f558])).
fof(f1023, plain, ((e11 = op1(op1(e11, e12), op1(e11, e12))) | ~ spl18_132), inference(avatar_component_clause, [], [f1021])).
fof(f2333, plain, (~ spl18_22 | ~ spl18_38), inference(avatar_split_clause, [], [f2330, f556, f488])).
fof(f2330, plain, (~ (e11 = op1(e12, e12)) | ~ spl18_38), inference(backward_demodulation, [], [f171, f558])).
fof(f171, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2325, plain, (spl18_21 | ~ spl18_55 | ~ spl18_133), inference(avatar_split_clause, [], [f2322, f1026, f628, f484])).
fof(f2322, plain, ((e10 = op1(e12, e12)) | (~ spl18_55 | ~ spl18_133)), inference(backward_demodulation, [], [f1028, f630])).
fof(f2317, plain, (~ spl18_66 | spl18_173), inference(avatar_contradiction_clause, [], [f2316])).
fof(f2316, plain, ($false | (~ spl18_66 | spl18_173)), inference(subsumption_resolution, [], [f2315, f1270])).
fof(f1270, plain, (~ (e21 = h4(e12)) | spl18_173), inference(avatar_component_clause, [], [f1268])).
fof(f2315, plain, ((e21 = h4(e12)) | ~ spl18_66), inference(backward_demodulation, [], [f332, f709])).
fof(f2303, plain, (spl18_248 | ~ spl18_88), inference(avatar_split_clause, [], [f2302, f800, f1681])).
fof(f2302, plain, ((e23 = h3(e12)) | ~ spl18_88), inference(backward_demodulation, [], [f328, f802])).
fof(f802, plain, ((e23 = op2(e22, e22)) | ~ spl18_88), inference(avatar_component_clause, [], [f800])).
fof(f2297, plain, (~ spl18_185 | ~ spl18_94), inference(avatar_split_clause, [], [f2294, f826, f1329])).
fof(f2294, plain, (~ (e21 = h3(e12)) | ~ spl18_94), inference(backward_demodulation, [], [f1223, f828])).
fof(f1223, plain, ~ (op2(e22, e20) = h3(e12)), inference(backward_demodulation, [], [f241, f328])).
fof(f241, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2290, plain, (spl18_85 | ~ spl18_119 | ~ spl18_152), inference(avatar_split_clause, [], [f2287, f1118, f932, f788])).
fof(f2287, plain, ((e20 = op2(e22, e22)) | (~ spl18_119 | ~ spl18_152)), inference(backward_demodulation, [], [f1120, f934])).
fof(f2286, plain, (spl18_206 | ~ spl18_126), inference(avatar_split_clause, [], [f2285, f962, f1439])).
fof(f2285, plain, ((e21 = h1(e12)) | ~ spl18_126), inference(backward_demodulation, [], [f320, f964])).
fof(f2283, plain, (spl18_89 | ~ spl18_193), inference(avatar_split_clause, [], [f2279, f1369, f805])).
fof(f2279, plain, ((e20 = op2(e22, e21)) | ~ spl18_193), inference(backward_demodulation, [], [f1216, f1370])).
fof(f2282, plain, (~ spl18_91 | ~ spl18_193), inference(avatar_split_clause, [], [f2278, f1369, f813])).
fof(f813, plain, (spl18_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_91])])).
fof(f2278, plain, (~ (e22 = op2(e22, e21)) | ~ spl18_193), inference(backward_demodulation, [], [f1212, f1370])).
fof(f1212, plain, ~ (op2(e22, e21) = h2(e12)), inference(backward_demodulation, [], [f213, f324])).
fof(f213, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2277, plain, (~ spl18_96 | ~ spl18_254), inference(avatar_split_clause, [], [f2275, f1736, f834])).
fof(f2275, plain, (~ (e23 = op2(e22, e20)) | ~ spl18_254), inference(backward_demodulation, [], [f1206, f1737])).
fof(f1206, plain, ~ (op2(e22, e20) = h2(e13)), inference(backward_demodulation, [], [f207, f1205])).
fof(f207, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2273, plain, (spl18_126 | ~ spl18_101 | ~ spl18_151), inference(avatar_split_clause, [], [f1971, f1113, f856, f962])).
fof(f856, plain, (spl18_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_101])])).
fof(f1971, plain, ((op2(e20, e20) = e21) | (~ spl18_101 | ~ spl18_151)), inference(backward_demodulation, [], [f1115, f858])).
fof(f858, plain, ((e20 = op2(e21, e22)) | ~ spl18_101), inference(avatar_component_clause, [], [f856])).
fof(f2272, plain, (~ spl18_117 | ~ spl18_101), inference(avatar_split_clause, [], [f2271, f856, f924])).
fof(f2271, plain, (~ (e20 = op2(e20, e22)) | ~ spl18_101), inference(forward_demodulation, [], [f216, f858])).
fof(f216, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2263, plain, (~ spl18_95 | ~ spl18_79), inference(avatar_split_clause, [], [f2262, f762, f830])).
fof(f2262, plain, (~ (e22 = op2(e22, e20)) | ~ spl18_79), inference(forward_demodulation, [], [f209, f764])).
fof(f209, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2243, plain, (~ spl18_59 | ~ spl18_43), inference(avatar_split_clause, [], [f2242, f577, f645])).
fof(f645, plain, (spl18_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_59])])).
fof(f2242, plain, (~ (e12 = op1(e10, e11)) | ~ spl18_43), inference(forward_demodulation, [], [f162, f579])).
fof(f162, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2241, plain, (~ spl18_57 | ~ spl18_25), inference(avatar_split_clause, [], [f2240, f501, f637])).
fof(f637, plain, (spl18_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_57])])).
fof(f2240, plain, (~ (e10 = op1(e10, e11)) | ~ spl18_25), inference(forward_demodulation, [], [f163, f503])).
fof(f163, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2220, plain, (~ spl18_29 | ~ spl18_25), inference(avatar_split_clause, [], [f2219, f501, f518])).
fof(f518, plain, (spl18_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_29])])).
fof(f2219, plain, (~ (e10 = op1(e12, e10)) | ~ spl18_25), inference(forward_demodulation, [], [f192, f503])).
fof(f192, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2212, plain, (~ spl18_18 | ~ spl18_2), inference(avatar_split_clause, [], [f2211, f403, f471])).
fof(f471, plain, (spl18_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_18])])).
fof(f2211, plain, (~ (e11 = op1(e12, e13)) | ~ spl18_2), inference(forward_demodulation, [], [f179, f405])).
fof(f179, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2180, plain, (~ spl18_7 | ~ spl18_15), inference(avatar_split_clause, [], [f2177, f458, f424])).
fof(f2177, plain, (~ (e12 = op1(e13, e12)) | ~ spl18_15), inference(backward_demodulation, [], [f199, f460])).
fof(f199, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2153, plain, (~ spl18_21 | ~ spl18_25), inference(avatar_split_clause, [], [f2147, f501, f484])).
fof(f2147, plain, (~ (e10 = op1(e12, e12)) | ~ spl18_25), inference(backward_demodulation, [], [f195, f503])).
fof(f195, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2133, plain, (~ spl18_37 | ~ spl18_61 | ~ spl18_132), inference(avatar_contradiction_clause, [], [f2132])).
fof(f2132, plain, ($false | (~ spl18_37 | ~ spl18_61 | ~ spl18_132)), inference(subsumption_resolution, [], [f2131, f252])).
fof(f2131, plain, ((e10 = e11) | (~ spl18_37 | ~ spl18_61 | ~ spl18_132)), inference(forward_demodulation, [], [f2127, f656])).
fof(f2127, plain, ((op1(e10, e10) = e11) | (~ spl18_37 | ~ spl18_132)), inference(backward_demodulation, [], [f1023, f554])).
fof(f2123, plain, (spl18_25 | ~ spl18_43), inference(avatar_split_clause, [], [f2118, f577, f501])).
fof(f2118, plain, ((e10 = op1(e12, e11)) | ~ spl18_43), inference(backward_demodulation, [], [f312, f579])).
fof(f312, plain, (e10 = op1(op1(e11, e11), e11)), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(e11, op1(op1(e11, e11), e11))) & (e12 = op1(e11, e11)) & (e10 = op1(op1(e11, e11), e11))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax12)).
fof(f2122, plain, (~ spl18_35 | ~ spl18_43), inference(avatar_split_clause, [], [f2117, f577, f543])).
fof(f543, plain, (spl18_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_35])])).
fof(f2117, plain, (~ (e12 = op1(e11, e13)) | ~ spl18_43), inference(backward_demodulation, [], [f190, f579])).
fof(f190, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2121, plain, (~ spl18_39 | ~ spl18_43), inference(avatar_split_clause, [], [f2116, f577, f560])).
fof(f560, plain, (spl18_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_39])])).
fof(f2116, plain, (~ (e12 = op1(e11, e12)) | ~ spl18_43), inference(backward_demodulation, [], [f189, f579])).
fof(f189, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2120, plain, (~ spl18_11 | ~ spl18_43), inference(avatar_split_clause, [], [f2115, f577, f441])).
fof(f441, plain, (spl18_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_11])])).
fof(f2115, plain, (~ (e12 = op1(e13, e11)) | ~ spl18_43), inference(backward_demodulation, [], [f166, f579])).
fof(f166, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2119, plain, (~ spl18_27 | ~ spl18_43), inference(avatar_split_clause, [], [f2114, f577, f509])).
fof(f509, plain, (spl18_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_27])])).
fof(f2114, plain, (~ (e12 = op1(e12, e11)) | ~ spl18_43), inference(backward_demodulation, [], [f165, f579])).
fof(f165, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2113, plain, (~ spl18_36 | ~ spl18_48), inference(avatar_split_clause, [], [f2108, f598, f547])).
fof(f547, plain, (spl18_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_36])])).
fof(f2108, plain, (~ (e13 = op1(e11, e13)) | ~ spl18_48), inference(backward_demodulation, [], [f188, f600])).
fof(f188, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2112, plain, (~ spl18_40 | ~ spl18_48), inference(avatar_split_clause, [], [f2107, f598, f564])).
fof(f564, plain, (spl18_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_40])])).
fof(f2107, plain, (~ (e13 = op1(e11, e12)) | ~ spl18_48), inference(backward_demodulation, [], [f187, f600])).
fof(f187, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2110, plain, (~ spl18_16 | ~ spl18_48), inference(avatar_split_clause, [], [f2105, f598, f462])).
fof(f462, plain, (spl18_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_16])])).
fof(f2105, plain, (~ (e13 = op1(e13, e10)) | ~ spl18_48), inference(backward_demodulation, [], [f160, f600])).
fof(f160, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2109, plain, (~ spl18_32 | ~ spl18_48), inference(avatar_split_clause, [], [f2104, f598, f530])).
fof(f530, plain, (spl18_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_32])])).
fof(f2104, plain, (~ (e13 = op1(e12, e10)) | ~ spl18_48), inference(backward_demodulation, [], [f159, f600])).
fof(f159, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2072, plain, (~ spl18_49 | ~ spl18_61), inference(avatar_split_clause, [], [f2064, f654, f603])).
fof(f2064, plain, (~ (e10 = op1(e10, e13)) | ~ spl18_61), inference(backward_demodulation, [], [f182, f656])).
fof(f2032, plain, (~ spl18_71 | ~ spl18_79), inference(avatar_split_clause, [], [f2027, f762, f728])).
fof(f728, plain, (spl18_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_71])])).
fof(f2027, plain, (~ (e22 = op2(e23, e22)) | ~ spl18_79), inference(backward_demodulation, [], [f247, f764])).
fof(f247, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2031, plain, (~ spl18_75 | ~ spl18_79), inference(avatar_split_clause, [], [f2026, f762, f745])).
fof(f745, plain, (spl18_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_75])])).
fof(f2026, plain, (~ (e22 = op2(e23, e21)) | ~ spl18_79), inference(backward_demodulation, [], [f246, f764])).
fof(f246, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2012, plain, (spl18_189 | ~ spl18_85), inference(avatar_split_clause, [], [f2011, f788, f1349])).
fof(f1349, plain, (spl18_189 <=> (e20 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_189])])).
fof(f2011, plain, ((e20 = h3(e12)) | ~ spl18_85), inference(backward_demodulation, [], [f328, f790])).
fof(f790, plain, ((e20 = op2(e22, e22)) | ~ spl18_85), inference(avatar_component_clause, [], [f788])).
fof(f2009, plain, (~ spl18_73 | ~ spl18_89), inference(avatar_split_clause, [], [f2005, f805, f737])).
fof(f737, plain, (spl18_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_73])])).
fof(f2005, plain, (~ (e20 = op2(e23, e21)) | ~ spl18_89), inference(backward_demodulation, [], [f215, f807])).
fof(f215, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2007, plain, (~ spl18_189 | ~ spl18_89), inference(avatar_split_clause, [], [f2003, f805, f1349])).
fof(f2003, plain, (~ (e20 = h3(e12)) | ~ spl18_89), inference(backward_demodulation, [], [f1224, f807])).
fof(f1224, plain, ~ (op2(e22, e21) = h3(e12)), inference(backward_demodulation, [], [f243, f328])).
fof(f243, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2001, plain, (~ spl18_89 | ~ spl18_93), inference(avatar_split_clause, [], [f1995, f822, f805])).
fof(f822, plain, (spl18_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_93])])).
fof(f1995, plain, (~ (e20 = op2(e22, e21)) | ~ spl18_93), inference(backward_demodulation, [], [f240, f824])).
fof(f824, plain, ((e20 = op2(e22, e20)) | ~ spl18_93), inference(avatar_component_clause, [], [f822])).
fof(f240, plain, ~ (op2(e22, e20) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f1979, plain, (~ spl18_101 | ~ spl18_125 | ~ spl18_151), inference(avatar_contradiction_clause, [], [f1978])).
fof(f1978, plain, ($false | (~ spl18_101 | ~ spl18_125 | ~ spl18_151)), inference(subsumption_resolution, [], [f1977, f258])).
fof(f1977, plain, ((e20 = e21) | (~ spl18_101 | ~ spl18_125 | ~ spl18_151)), inference(forward_demodulation, [], [f1971, f960])).
fof(f1976, plain, (~ spl18_97 | ~ spl18_101), inference(avatar_split_clause, [], [f1970, f856, f839])).
fof(f1970, plain, (~ (e20 = op2(e21, e23)) | ~ spl18_101), inference(backward_demodulation, [], [f239, f858])).
fof(f239, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f1968, plain, (spl18_193 | ~ spl18_107), inference(avatar_split_clause, [], [f1967, f881, f1369])).
fof(f1967, plain, ((e22 = h2(e12)) | ~ spl18_107), inference(backward_demodulation, [], [f324, f883])).
fof(f1966, plain, (~ spl18_285 | ~ spl18_112), inference(avatar_split_clause, [], [f1964, f902, f1903])).
fof(f1964, plain, (~ (e23 = h1(e12)) | ~ spl18_112), inference(backward_demodulation, [], [f1196, f904])).
fof(f1196, plain, ~ (op2(e21, e20) = h1(e12)), inference(backward_demodulation, [], [f204, f320])).
fof(f204, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f1965, plain, (spl18_254 | ~ spl18_112), inference(avatar_split_clause, [], [f1963, f902, f1736])).
fof(f1963, plain, ((e23 = h2(e13)) | ~ spl18_112), inference(backward_demodulation, [], [f1205, f904])).
fof(f1934, plain, (~ spl18_89 | ~ spl18_121), inference(avatar_split_clause, [], [f1928, f941, f805])).
fof(f941, plain, (spl18_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_121])])).
fof(f1928, plain, (~ (e20 = op2(e22, e21)) | ~ spl18_121), inference(backward_demodulation, [], [f211, f943])).
fof(f943, plain, ((e20 = op2(e20, e21)) | ~ spl18_121), inference(avatar_component_clause, [], [f941])).
fof(f211, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f1925, plain, (spl18_210 | ~ spl18_125), inference(avatar_split_clause, [], [f1924, f958, f1459])).
fof(f1924, plain, ((e20 = h1(e12)) | ~ spl18_125), inference(backward_demodulation, [], [f320, f960])).
fof(f1787, plain, (~ spl18_250 | ~ spl18_251 | ~ spl18_252 | ~ spl18_253 | spl18_197 | spl18_194 | spl18_191 | ~ spl18_254 | ~ spl18_255 | ~ spl18_256 | ~ spl18_257 | ~ spl18_258 | ~ spl18_259 | ~ spl18_260 | ~ spl18_261 | ~ spl18_262 | ~ spl18_263 | ~ spl18_264 | ~ spl18_265 | ~ spl18_266), inference(avatar_split_clause, [], [f1718, f1784, f1780, f1776, f1772, f1768, f1764, f1760, f1756, f1752, f1748, f1744, f1740, f1736, f1360, f1376, f1393, f1732, f1728, f1724, f1720])).
fof(f1393, plain, (spl18_197 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl18_197])])).
fof(f1376, plain, (spl18_194 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl18_194])])).
fof(f1360, plain, (spl18_191 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl18_191])])).
fof(f1718, plain, (~ (h1(e12) = h2(op1(e10, e10))) | ~ (op2(e20, e21) = h2(op1(e10, e11))) | ~ (h2(op1(e10, e12)) = op2(e20, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e20, h2(e13))) | ~ (h2(e13) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12)))), inference(forward_demodulation, [], [f1717, f320])).
fof(f1717, plain, (~ (op2(e20, e20) = h2(op1(e10, e10))) | ~ (op2(e20, e21) = h2(op1(e10, e11))) | ~ (h2(op1(e10, e12)) = op2(e20, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e20, h2(e13))) | ~ (h2(e13) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12)))), inference(forward_demodulation, [], [f1716, f1219])).
fof(f1716, plain, (~ (op2(e20, e21) = h2(op1(e10, e11))) | ~ (h2(op1(e10, e12)) = op2(e20, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e20, h2(e13))) | ~ (h2(e13) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1715, f1219])).
fof(f1715, plain, (~ (h2(op1(e10, e11)) = op2(h2(e10), e21)) | ~ (h2(op1(e10, e12)) = op2(e20, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e20, h2(e13))) | ~ (h2(e13) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1714, f322])).
fof(f1714, plain, (~ (h2(op1(e10, e12)) = op2(e20, h2(e12))) | ~ (h2(op1(e10, e13)) = op2(e20, h2(e13))) | ~ (h2(e13) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1713, f1219])).
fof(f1713, plain, (~ (h2(op1(e10, e13)) = op2(e20, h2(e13))) | ~ (h2(e13) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1712, f1219])).
fof(f1712, plain, (~ (h2(e13) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1711, f1205])).
fof(f1711, plain, (~ (op2(e21, e20) = h2(op1(e11, e10))) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1710, f322])).
fof(f1710, plain, (~ (h2(op1(e11, e10)) = op2(h2(e11), e20)) | ~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1709, f1219])).
fof(f1709, plain, (~ (h2(e12) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1708, f324])).
fof(f1708, plain, (~ (op2(e21, e21) = h2(op1(e11, e11))) | ~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1707, f322])).
fof(f1707, plain, (~ (h2(op1(e11, e12)) = op2(e21, h2(e12))) | ~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1706, f322])).
fof(f1706, plain, (~ (h2(op1(e11, e13)) = op2(e21, h2(e13))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1705, f322])).
fof(f1705, plain, (~ (h2(op1(e12, e10)) = op2(h2(e12), e20)) | ~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1704, f1219])).
fof(f1704, plain, (~ (e20 = h2(op1(e12, e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1703, f1216])).
fof(f1703, plain, (~ (h2(op1(e12, e11)) = op2(h2(e12), e21)) | ~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1702, f322])).
fof(f1702, plain, (~ (h2(op1(e13, e10)) = op2(h2(e13), e20)) | ~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f1701, f1219])).
fof(f1701, plain, (~ (h2(op1(e13, e11)) = op2(h2(e13), e21)) | ~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(forward_demodulation, [], [f389, f322])).
fof(f389, plain, (~ (e23 = h2(e13)) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP14 | sP13 | sP12 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP8 | sP7 | sP6 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29])).
fof(f29, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP6), inference(usedef, [], [e29])).
fof(e29, plain, (sP6 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f30, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP7), inference(usedef, [], [e30])).
fof(e30, plain, (sP7 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f31, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP8), inference(usedef, [], [e31])).
fof(e31, plain, (sP8 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f32, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP9), inference(usedef, [], [e32])).
fof(e32, plain, (sP9 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f33, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP10), inference(usedef, [], [e33])).
fof(e33, plain, (sP10 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f34, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP11), inference(usedef, [], [e34])).
fof(e34, plain, (sP11 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f35, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP12), inference(usedef, [], [e35])).
fof(e35, plain, (sP12 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f36, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP13), inference(usedef, [], [e36])).
fof(e36, plain, (sP13 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f37, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP14), inference(usedef, [], [e37])).
fof(e37, plain, (sP14 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f38, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP15), inference(usedef, [], [e38])).
fof(e38, plain, (sP15 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f39, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP16), inference(usedef, [], [e39])).
fof(e39, plain, (sP16 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f40, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP17), inference(usedef, [], [e40])).
fof(e40, plain, (sP17 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', co1)).
fof(f1408, plain, ~ spl18_197, inference(avatar_split_clause, [], [f1407, f1393])).
fof(f1407, plain, ~ sP9, inference(subsumption_resolution, [], [f366, f1219])).
fof(f366, plain, (~ (e20 = h2(e10)) | ~ sP9), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP9), inference(nnf_transformation, [], [f32])).
fof(f1390, plain, ~ spl18_194, inference(avatar_split_clause, [], [f1389, f1376])).
fof(f1389, plain, ~ sP10, inference(subsumption_resolution, [], [f363, f322])).
fof(f363, plain, (~ (e21 = h2(e11)) | ~ sP10), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP10), inference(nnf_transformation, [], [f33])).
fof(f1372, plain, (~ spl18_191 | ~ spl18_193), inference(avatar_split_clause, [], [f360, f1369, f1360])).
fof(f360, plain, (~ (e22 = h2(e12)) | ~ sP11), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP11), inference(nnf_transformation, [], [f34])).
fof(f1195, plain, spl18_112, inference(avatar_split_clause, [], [f1194, f902])).
fof(f1194, plain, (e23 = op2(e21, e20)), inference(backward_demodulation, [], [f317, f315])).
fof(f317, plain, (e23 = op2(e21, op2(op2(e21, e21), e21))), inference(cnf_transformation, [], [f13])).
fof(f1193, plain, spl18_107, inference(avatar_split_clause, [], [f316, f881])).
fof(f316, plain, (e22 = op2(e21, e21)), inference(cnf_transformation, [], [f13])).
fof(f1192, plain, spl18_48, inference(avatar_split_clause, [], [f1191, f598])).
fof(f1191, plain, (e13 = op1(e11, e10)), inference(backward_demodulation, [], [f314, f312])).
fof(f314, plain, (e13 = op1(e11, op1(op1(e11, e11), e11))), inference(cnf_transformation, [], [f12])).
fof(f1190, plain, spl18_43, inference(avatar_split_clause, [], [f313, f577])).
fof(f313, plain, (e12 = op1(e11, e11)), inference(cnf_transformation, [], [f12])).
fof(f1189, plain, (spl18_158 | spl18_153 | spl18_148 | spl18_166), inference(avatar_split_clause, [], [f308, f1186, f1099, f1123, f1147])).
fof(f1147, plain, (spl18_158 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl18_158])])).
fof(f1123, plain, (spl18_153 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl18_153])])).
fof(f1099, plain, (spl18_148 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl18_148])])).
fof(f308, plain, ((e20 = op2(op2(e20, e23), op2(e20, e23))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (((e23 = op2(op2(e23, e23), op2(e23, e23))) & (e22 = op2(op2(e22, e23), op2(e22, e23))) & (e21 = op2(op2(e21, e23), op2(e21, e23))) & (e20 = op2(op2(e20, e23), op2(e20, e23)))) | sP5 | sP4 | sP3), inference(definition_folding, [], [f11, e27, e26, e25])).
fof(f25, plain, (((e23 = op2(op2(e23, e20), op2(e23, e20))) & (e22 = op2(op2(e22, e20), op2(e22, e20))) & (e21 = op2(op2(e21, e20), op2(e21, e20))) & (e20 = op2(op2(e20, e20), op2(e20, e20)))) | ~ sP3), inference(usedef, [], [e25])).
fof(e25, plain, (sP3 <=> ((e23 = op2(op2(e23, e20), op2(e23, e20))) & (e22 = op2(op2(e22, e20), op2(e22, e20))) & (e21 = op2(op2(e21, e20), op2(e21, e20))) & (e20 = op2(op2(e20, e20), op2(e20, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f26, plain, (((e23 = op2(op2(e23, e21), op2(e23, e21))) & (e22 = op2(op2(e22, e21), op2(e22, e21))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e20, e21), op2(e20, e21)))) | ~ sP4), inference(usedef, [], [e26])).
fof(e26, plain, (sP4 <=> ((e23 = op2(op2(e23, e21), op2(e23, e21))) & (e22 = op2(op2(e22, e21), op2(e22, e21))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e20, e21), op2(e20, e21))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f27, plain, (((e23 = op2(op2(e23, e22), op2(e23, e22))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e21, e22), op2(e21, e22))) & (e20 = op2(op2(e20, e22), op2(e20, e22)))) | ~ sP5), inference(usedef, [], [e27])).
fof(e27, plain, (sP5 <=> ((e23 = op2(op2(e23, e22), op2(e23, e22))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e21, e22), op2(e21, e22))) & (e20 = op2(op2(e20, e22), op2(e20, e22))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f11, plain, (((e23 = op2(op2(e23, e23), op2(e23, e23))) & (e22 = op2(op2(e22, e23), op2(e22, e23))) & (e21 = op2(op2(e21, e23), op2(e21, e23))) & (e20 = op2(op2(e20, e23), op2(e20, e23)))) | ((e23 = op2(op2(e23, e22), op2(e23, e22))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e21, e22), op2(e21, e22))) & (e20 = op2(op2(e20, e22), op2(e20, e22)))) | ((e23 = op2(op2(e23, e21), op2(e23, e21))) & (e22 = op2(op2(e22, e21), op2(e22, e21))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e20, e21), op2(e20, e21)))) | ((e23 = op2(op2(e23, e20), op2(e23, e20))) & (e22 = op2(op2(e22, e20), op2(e22, e20))) & (e21 = op2(op2(e21, e20), op2(e21, e20))) & (e20 = op2(op2(e20, e20), op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax11)).
fof(f1184, plain, (spl18_158 | spl18_153 | spl18_148 | spl18_165), inference(avatar_split_clause, [], [f309, f1181, f1099, f1123, f1147])).
fof(f309, plain, ((e21 = op2(op2(e21, e23), op2(e21, e23))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f1164, plain, (~ spl18_158 | spl18_161), inference(avatar_split_clause, [], [f305, f1161, f1147])).
fof(f305, plain, ((e21 = op2(op2(e21, e20), op2(e21, e20))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (((e23 = op2(op2(e23, e20), op2(e23, e20))) & (e22 = op2(op2(e22, e20), op2(e22, e20))) & (e21 = op2(op2(e21, e20), op2(e21, e20))) & (e20 = op2(op2(e20, e20), op2(e20, e20)))) | ~ sP3), inference(nnf_transformation, [], [f25])).
fof(f1154, plain, (~ spl18_158 | spl18_159), inference(avatar_split_clause, [], [f307, f1151, f1147])).
fof(f307, plain, ((e23 = op2(op2(e23, e20), op2(e23, e20))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f1140, plain, (~ spl18_153 | spl18_156), inference(avatar_split_clause, [], [f301, f1137, f1123])).
fof(f301, plain, ((e21 = op2(op2(e21, e21), op2(e21, e21))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (((e23 = op2(op2(e23, e21), op2(e23, e21))) & (e22 = op2(op2(e22, e21), op2(e22, e21))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e20, e21), op2(e20, e21)))) | ~ sP4), inference(nnf_transformation, [], [f26])).
fof(f1135, plain, (~ spl18_153 | spl18_155), inference(avatar_split_clause, [], [f302, f1132, f1123])).
fof(f302, plain, ((e22 = op2(op2(e22, e21), op2(e22, e21))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f1121, plain, (~ spl18_148 | spl18_152), inference(avatar_split_clause, [], [f296, f1118, f1099])).
fof(f296, plain, ((e20 = op2(op2(e20, e22), op2(e20, e22))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (((e23 = op2(op2(e23, e22), op2(e23, e22))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e21, e22), op2(e21, e22))) & (e20 = op2(op2(e20, e22), op2(e20, e22)))) | ~ sP5), inference(nnf_transformation, [], [f27])).
fof(f1116, plain, (~ spl18_148 | spl18_151), inference(avatar_split_clause, [], [f297, f1113, f1099])).
fof(f297, plain, ((e21 = op2(op2(e21, e22), op2(e21, e22))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f1097, plain, (spl18_139 | spl18_134 | spl18_129 | spl18_147), inference(avatar_split_clause, [], [f292, f1094, f1007, f1031, f1055])).
fof(f1055, plain, (spl18_139 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl18_139])])).
fof(f1031, plain, (spl18_134 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl18_134])])).
fof(f1007, plain, (spl18_129 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl18_129])])).
fof(f292, plain, ((e10 = op1(op1(e10, e13), op1(e10, e13))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f24, plain, (((e13 = op1(op1(e13, e13), op1(e13, e13))) & (e12 = op1(op1(e12, e13), op1(e12, e13))) & (e11 = op1(op1(e11, e13), op1(e11, e13))) & (e10 = op1(op1(e10, e13), op1(e10, e13)))) | sP2 | sP1 | sP0), inference(definition_folding, [], [f10, e23, e22, e21])).
fof(f21, plain, (((e13 = op1(op1(e13, e10), op1(e13, e10))) & (e12 = op1(op1(e12, e10), op1(e12, e10))) & (e11 = op1(op1(e11, e10), op1(e11, e10))) & (e10 = op1(op1(e10, e10), op1(e10, e10)))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> ((e13 = op1(op1(e13, e10), op1(e13, e10))) & (e12 = op1(op1(e12, e10), op1(e12, e10))) & (e11 = op1(op1(e11, e10), op1(e11, e10))) & (e10 = op1(op1(e10, e10), op1(e10, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, (((e13 = op1(op1(e13, e11), op1(e13, e11))) & (e12 = op1(op1(e12, e11), op1(e12, e11))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e10, e11), op1(e10, e11)))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> ((e13 = op1(op1(e13, e11), op1(e13, e11))) & (e12 = op1(op1(e12, e11), op1(e12, e11))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e10, e11), op1(e10, e11))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, (((e13 = op1(op1(e13, e12), op1(e13, e12))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e11, e12), op1(e11, e12))) & (e10 = op1(op1(e10, e12), op1(e10, e12)))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> ((e13 = op1(op1(e13, e12), op1(e13, e12))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e11, e12), op1(e11, e12))) & (e10 = op1(op1(e10, e12), op1(e10, e12))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f10, plain, (((e13 = op1(op1(e13, e13), op1(e13, e13))) & (e12 = op1(op1(e12, e13), op1(e12, e13))) & (e11 = op1(op1(e11, e13), op1(e11, e13))) & (e10 = op1(op1(e10, e13), op1(e10, e13)))) | ((e13 = op1(op1(e13, e12), op1(e13, e12))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e11, e12), op1(e11, e12))) & (e10 = op1(op1(e10, e12), op1(e10, e12)))) | ((e13 = op1(op1(e13, e11), op1(e13, e11))) & (e12 = op1(op1(e12, e11), op1(e12, e11))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e10, e11), op1(e10, e11)))) | ((e13 = op1(op1(e13, e10), op1(e13, e10))) & (e12 = op1(op1(e12, e10), op1(e12, e10))) & (e11 = op1(op1(e11, e10), op1(e11, e10))) & (e10 = op1(op1(e10, e10), op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax10)).
fof(f1092, plain, (spl18_139 | spl18_134 | spl18_129 | spl18_146), inference(avatar_split_clause, [], [f293, f1089, f1007, f1031, f1055])).
fof(f293, plain, ((e11 = op1(op1(e11, e13), op1(e11, e13))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f1077, plain, (~ spl18_139 | spl18_143), inference(avatar_split_clause, [], [f288, f1074, f1055])).
fof(f288, plain, ((e10 = op1(op1(e10, e10), op1(e10, e10))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (((e13 = op1(op1(e13, e10), op1(e13, e10))) & (e12 = op1(op1(e12, e10), op1(e12, e10))) & (e11 = op1(op1(e11, e10), op1(e11, e10))) & (e10 = op1(op1(e10, e10), op1(e10, e10)))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1072, plain, (~ spl18_139 | spl18_142), inference(avatar_split_clause, [], [f289, f1069, f1055])).
fof(f289, plain, ((e11 = op1(op1(e11, e10), op1(e11, e10))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f1062, plain, (~ spl18_139 | spl18_140), inference(avatar_split_clause, [], [f291, f1059, f1055])).
fof(f291, plain, ((e13 = op1(op1(e13, e10), op1(e13, e10))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f1048, plain, (~ spl18_134 | spl18_137), inference(avatar_split_clause, [], [f285, f1045, f1031])).
fof(f285, plain, ((e11 = op1(op1(e11, e11), op1(e11, e11))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (((e13 = op1(op1(e13, e11), op1(e13, e11))) & (e12 = op1(op1(e12, e11), op1(e12, e11))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e10, e11), op1(e10, e11)))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1043, plain, (~ spl18_134 | spl18_136), inference(avatar_split_clause, [], [f286, f1040, f1031])).
fof(f286, plain, ((e12 = op1(op1(e12, e11), op1(e12, e11))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f1029, plain, (~ spl18_129 | spl18_133), inference(avatar_split_clause, [], [f280, f1026, f1007])).
fof(f280, plain, ((e10 = op1(op1(e10, e12), op1(e10, e12))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (((e13 = op1(op1(e13, e12), op1(e13, e12))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e11, e12), op1(e11, e12))) & (e10 = op1(op1(e10, e12), op1(e10, e12)))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1024, plain, (~ spl18_129 | spl18_132), inference(avatar_split_clause, [], [f281, f1021, f1007])).
fof(f281, plain, ((e11 = op1(op1(e11, e12), op1(e11, e12))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f1004, plain, (spl18_125 | spl18_109 | spl18_93 | spl18_77), inference(avatar_split_clause, [], [f125, f754, f822, f890, f958])).
fof(f125, plain, ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax4)).
fof(f999, plain, (spl18_128 | spl18_124 | spl18_120 | spl18_116), inference(avatar_split_clause, [], [f130, f919, f936, f953, f970])).
fof(f130, plain, ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f994, plain, (spl18_122 | spl18_106 | spl18_90 | spl18_74), inference(avatar_split_clause, [], [f135, f741, f809, f877, f945])).
fof(f135, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f988, plain, (spl18_117 | spl18_101 | spl18_85 | spl18_69), inference(avatar_split_clause, [], [f141, f720, f788, f856, f924])).
fof(f141, plain, ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f985, plain, (spl18_95 | spl18_91 | spl18_87 | spl18_83), inference(avatar_split_clause, [], [f144, f779, f796, f813, f830])).
fof(f144, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f984, plain, (spl18_119 | spl18_103 | spl18_87 | spl18_71), inference(avatar_split_clause, [], [f145, f728, f796, f864, f932])).
fof(f145, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f973, plain, (spl18_125 | spl18_126 | spl18_127 | spl18_128), inference(avatar_split_clause, [], [f108, f970, f966, f962, f958])).
fof(f108, plain, ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax3)).
fof(f956, plain, (spl18_121 | spl18_122 | spl18_123 | spl18_124), inference(avatar_split_clause, [], [f109, f953, f949, f945, f941])).
fof(f109, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f939, plain, (spl18_117 | spl18_118 | spl18_119 | spl18_120), inference(avatar_split_clause, [], [f110, f936, f932, f928, f924])).
fof(f110, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f922, plain, (spl18_113 | spl18_114 | spl18_115 | spl18_116), inference(avatar_split_clause, [], [f111, f919, f915, f911, f907])).
fof(f111, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f871, plain, (spl18_101 | spl18_102 | spl18_103 | spl18_104), inference(avatar_split_clause, [], [f114, f868, f864, f860, f856])).
fof(f114, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f854, plain, (spl18_97 | spl18_98 | spl18_99 | spl18_100), inference(avatar_split_clause, [], [f115, f851, f847, f843, f839])).
fof(f115, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f837, plain, (spl18_93 | spl18_94 | spl18_95 | spl18_96), inference(avatar_split_clause, [], [f116, f834, f830, f826, f822])).
fof(f116, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f769, plain, (spl18_77 | spl18_78 | spl18_79 | spl18_80), inference(avatar_split_clause, [], [f120, f766, f762, f758, f754])).
fof(f120, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f752, plain, (spl18_73 | spl18_74 | spl18_75 | spl18_76), inference(avatar_split_clause, [], [f121, f749, f745, f741, f737])).
fof(f121, plain, ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))), inference(cnf_transformation, [], [f3])).
fof(f701, plain, (spl18_61 | spl18_57 | spl18_53 | spl18_49), inference(avatar_split_clause, [], [f76, f603, f620, f637, f654])).
fof(f76, plain, ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax2)).
fof(f697, plain, (spl18_63 | spl18_59 | spl18_55 | spl18_51), inference(avatar_split_clause, [], [f80, f611, f628, f645, f662])).
fof(f80, plain, ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f695, plain, (spl18_64 | spl18_60 | spl18_56 | spl18_52), inference(avatar_split_clause, [], [f82, f615, f632, f649, f666])).
fof(f82, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f690, plain, (spl18_58 | spl18_42 | spl18_26 | spl18_10), inference(avatar_split_clause, [], [f87, f437, f505, f573, f641])).
fof(f87, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f684, plain, (spl18_53 | spl18_37 | spl18_21 | spl18_5), inference(avatar_split_clause, [], [f93, f416, f484, f552, f620])).
fof(f93, plain, ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f683, plain, (spl18_30 | spl18_26 | spl18_22 | spl18_18), inference(avatar_split_clause, [], [f94, f471, f488, f505, f522])).
fof(f94, plain, ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f681, plain, (spl18_31 | spl18_27 | spl18_23 | spl18_19), inference(avatar_split_clause, [], [f96, f475, f492, f509, f526])).
fof(f96, plain, ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f680, plain, (spl18_55 | spl18_39 | spl18_23 | spl18_7), inference(avatar_split_clause, [], [f97, f424, f492, f560, f628])).
fof(f97, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f673, plain, (spl18_15 | spl18_11 | spl18_7 | spl18_3), inference(avatar_split_clause, [], [f104, f407, f424, f441, f458])).
fof(f104, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f671, plain, (spl18_16 | spl18_12 | spl18_8 | spl18_4), inference(avatar_split_clause, [], [f106, f411, f428, f445, f462])).
fof(f106, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f669, plain, (spl18_61 | spl18_62 | spl18_63 | spl18_64), inference(avatar_split_clause, [], [f60, f666, f662, f658, f654])).
fof(f60, plain, ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG112+1.p', ax1)).
fof(f635, plain, (spl18_53 | spl18_54 | spl18_55 | spl18_56), inference(avatar_split_clause, [], [f62, f632, f628, f624, f620])).
fof(f62, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f618, plain, (spl18_49 | spl18_50 | spl18_51 | spl18_52), inference(avatar_split_clause, [], [f63, f615, f611, f607, f603])).
fof(f63, plain, ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f1])).
fof(f567, plain, (spl18_37 | spl18_38 | spl18_39 | spl18_40), inference(avatar_split_clause, [], [f66, f564, f560, f556, f552])).
fof(f66, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f550, plain, (spl18_33 | spl18_34 | spl18_35 | spl18_36), inference(avatar_split_clause, [], [f67, f547, f543, f539, f535])).
fof(f67, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f533, plain, (spl18_29 | spl18_30 | spl18_31 | spl18_32), inference(avatar_split_clause, [], [f68, f530, f526, f522, f518])).
fof(f68, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).