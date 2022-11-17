fof(f3214, plain, $false, inference(avatar_sat_refutation, [], [f560, f623, f728, f959, f963, f965, f976, f986, f988, f1010, f1018, f1025, f1032, f1037, f1038, f1045, f1053, f1059, f1067, f1072, f1074, f1080, f1088, f1095, f1102, f1109, f1115, f1122, f1129, f1135, f1137, f1142, f1144, f1151, f1157, f1158, f1164, f1170, f1172, f1177, f1179, f1301, f1310, f1311, f1314, f1457, f1480, f1512, f1516, f1550, f1569, f1580, f1672, f1673, f1674, f1677, f1713, f1749, f1752, f1771, f1774, f1777, f1782, f1788, f1802, f1833, f1875, f1897, f1937, f1946, f1971, f1984, f1999, f2026, f2036, f2089, f2116, f2149, f2219, f2223, f2227, f2228, f2230, f2233, f2239, f2300, f2465, f2576, f2578, f2593, f2636, f2637, f2638, f2699, f2725, f2733, f2742, f2761, f2843, f2882, f2905, f2906, f2908, f2909, f2910, f2913, f2914, f2916, f2924, f2926, f3042, f3178, f3186, f3213])).
fof(f3213, plain, (~ spl34_72 | ~ spl34_116 | ~ spl34_203), inference(avatar_contradiction_clause, [], [f3212])).
fof(f3212, plain, ($false | (~ spl34_72 | ~ spl34_116 | ~ spl34_203)), inference(subsumption_resolution, [], [f3211, f266])).
fof(f266, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG056+1.p', ax5)).
fof(f3211, plain, ((e0 = e2) | (~ spl34_72 | ~ spl34_116 | ~ spl34_203)), inference(forward_demodulation, [], [f3210, f900])).
fof(f900, plain, ((e0 = op(e0, e1)) | ~ spl34_116), inference(avatar_component_clause, [], [f898])).
fof(f898, plain, (spl34_116 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_116])])).
fof(f3210, plain, ((e2 = op(e0, e1)) | (~ spl34_72 | ~ spl34_203)), inference(forward_demodulation, [], [f1438, f715])).
fof(f715, plain, ((e1 = op(e2, e0)) | ~ spl34_72), inference(avatar_component_clause, [], [f713])).
fof(f713, plain, (spl34_72 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_72])])).
fof(f1438, plain, ((e2 = op(e0, op(e2, e0))) | ~ spl34_203), inference(avatar_component_clause, [], [f1436])).
fof(f1436, plain, (spl34_203 <=> (e2 = op(e0, op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_203])])).
fof(f3186, plain, (spl34_110 | ~ spl34_24 | ~ spl34_205), inference(avatar_split_clause, [], [f3136, f1444, f511, f872])).
fof(f872, plain, (spl34_110 <=> (e4 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_110])])).
fof(f511, plain, (spl34_24 <=> (e3 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_24])])).
fof(f1444, plain, (spl34_205 <=> (e4 = op(e0, op(e4, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_205])])).
fof(f3136, plain, ((e4 = op(e0, e3)) | (~ spl34_24 | ~ spl34_205)), inference(forward_demodulation, [], [f1446, f513])).
fof(f513, plain, ((e3 = op(e4, e0)) | ~ spl34_24), inference(avatar_component_clause, [], [f511])).
fof(f1446, plain, ((e4 = op(e0, op(e4, e0))) | ~ spl34_205), inference(avatar_component_clause, [], [f1444])).
fof(f3178, plain, (~ spl34_13 | ~ spl34_88), inference(avatar_split_clause, [], [f3177, f780, f465])).
fof(f465, plain, (spl34_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_13])])).
fof(f780, plain, (spl34_88 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_88])])).
fof(f3177, plain, (~ (e2 = op(e4, e2)) | ~ spl34_88), inference(forward_demodulation, [], [f191, f782])).
fof(f782, plain, ((e2 = op(e1, e2)) | ~ spl34_88), inference(avatar_component_clause, [], [f780])).
fof(f191, plain, ~ (op(e1, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG056+1.p', ax4)).
fof(f3042, plain, (~ spl34_36 | ~ spl34_48 | spl34_207), inference(avatar_split_clause, [], [f3037, f1454, f612, f562])).
fof(f562, plain, (spl34_36 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_36])])).
fof(f612, plain, (spl34_48 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_48])])).
fof(f1454, plain, (spl34_207 <=> (e0 = op(e3, op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_207])])).
fof(f3037, plain, (~ (e0 = op(e3, e2)) | (~ spl34_48 | spl34_207)), inference(backward_demodulation, [], [f1456, f614])).
fof(f614, plain, ((e2 = op(e3, e0)) | ~ spl34_48), inference(avatar_component_clause, [], [f612])).
fof(f1456, plain, (~ (e0 = op(e3, op(e3, e0))) | spl34_207), inference(avatar_component_clause, [], [f1454])).
fof(f2926, plain, (~ spl34_105 | ~ spl34_125), inference(avatar_split_clause, [], [f2923, f935, f851])).
fof(f851, plain, (spl34_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_105])])).
fof(f935, plain, (spl34_125 <=> (op(e0, e0) = e4)), introduced(avatar_definition, [new_symbols(naming, [spl34_125])])).
fof(f2923, plain, (~ (e4 = op(e0, e4)) | ~ spl34_125), inference(backward_demodulation, [], [f218, f937])).
fof(f937, plain, ((op(e0, e0) = e4) | ~ spl34_125), inference(avatar_component_clause, [], [f935])).
fof(f218, plain, ~ (op(e0, e0) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f2924, plain, (~ spl34_50 | ~ spl34_125), inference(avatar_split_clause, [], [f2919, f935, f620])).
fof(f620, plain, (spl34_50 <=> (e4 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_50])])).
fof(f2919, plain, (~ (e4 = op(e3, e0)) | ~ spl34_125), inference(backward_demodulation, [], [f167, f937])).
fof(f167, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f2916, plain, (spl34_20 | ~ spl34_127), inference(avatar_split_clause, [], [f2901, f944, f494])).
fof(f494, plain, (spl34_20 <=> (e4 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_20])])).
fof(f944, plain, (spl34_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_127])])).
fof(f2901, plain, ((e4 = op(e4, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f113, f946])).
fof(f946, plain, ((e1 = unit) | ~ spl34_127), inference(avatar_component_clause, [], [f944])).
fof(f113, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG056+1.p', ax2)).
fof(f2914, plain, (spl34_44 | ~ spl34_127), inference(avatar_split_clause, [], [f2899, f944, f595])).
fof(f595, plain, (spl34_44 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_44])])).
fof(f2899, plain, ((e3 = op(e3, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f111, f946])).
fof(f111, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f2913, plain, (~ spl34_82 | ~ spl34_127), inference(avatar_contradiction_clause, [], [f2912])).
fof(f2912, plain, ($false | (~ spl34_82 | ~ spl34_127)), inference(subsumption_resolution, [], [f2911, f270])).
fof(f270, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f2911, plain, ((e1 = e3) | (~ spl34_82 | ~ spl34_127)), inference(forward_demodulation, [], [f2898, f757])).
fof(f757, plain, ((e1 = op(e1, e3)) | ~ spl34_82), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl34_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_82])])).
fof(f2898, plain, ((e3 = op(e1, e3)) | ~ spl34_127), inference(backward_demodulation, [], [f110, f946])).
fof(f110, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f2910, plain, (spl34_68 | ~ spl34_127), inference(avatar_split_clause, [], [f2897, f944, f696])).
fof(f696, plain, (spl34_68 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_68])])).
fof(f2897, plain, ((e2 = op(e2, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f109, f946])).
fof(f109, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f2909, plain, (spl34_88 | ~ spl34_127), inference(avatar_split_clause, [], [f2896, f944, f780])).
fof(f2896, plain, ((e2 = op(e1, e2)) | ~ spl34_127), inference(backward_demodulation, [], [f108, f946])).
fof(f108, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f2908, plain, (spl34_92 | ~ spl34_127), inference(avatar_split_clause, [], [f2895, f944, f797])).
fof(f797, plain, (spl34_92 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_92])])).
fof(f2895, plain, ((e1 = op(e1, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f107, f946])).
fof(f107, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f2906, plain, (spl34_116 | ~ spl34_127), inference(avatar_split_clause, [], [f2893, f944, f898])).
fof(f2893, plain, ((e0 = op(e0, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f105, f946])).
fof(f105, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f2905, plain, (spl34_96 | ~ spl34_127), inference(avatar_split_clause, [], [f2892, f944, f814])).
fof(f814, plain, (spl34_96 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_96])])).
fof(f2892, plain, ((e0 = op(e1, e0)) | ~ spl34_127), inference(backward_demodulation, [], [f104, f946])).
fof(f104, plain, (e0 = op(unit, e0)), inference(cnf_transformation, [], [f2])).
fof(f2882, plain, (~ spl34_110 | ~ spl34_35), inference(avatar_split_clause, [], [f2881, f557, f872])).
fof(f557, plain, (spl34_35 <=> (e4 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_35])])).
fof(f2881, plain, (~ (e4 = op(e0, e3)) | ~ spl34_35), inference(forward_demodulation, [], [f197, f559])).
fof(f559, plain, ((e4 = op(e3, e3)) | ~ spl34_35), inference(avatar_component_clause, [], [f557])).
fof(f197, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2843, plain, (~ spl34_61 | ~ spl34_65), inference(avatar_contradiction_clause, [], [f2842])).
fof(f2842, plain, ($false | (~ spl34_61 | ~ spl34_65)), inference(subsumption_resolution, [], [f2841, f268])).
fof(f268, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f2841, plain, ((e0 = e4) | (~ spl34_61 | ~ spl34_65)), inference(backward_demodulation, [], [f685, f669])).
fof(f669, plain, ((e0 = op(e2, e2)) | ~ spl34_61), inference(avatar_component_clause, [], [f667])).
fof(f667, plain, (spl34_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_61])])).
fof(f685, plain, ((e4 = op(e2, e2)) | ~ spl34_65), inference(avatar_component_clause, [], [f683])).
fof(f683, plain, (spl34_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_65])])).
fof(f2761, plain, (spl34_105 | ~ spl34_126), inference(avatar_split_clause, [], [f2629, f940, f851])).
fof(f940, plain, (spl34_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_126])])).
fof(f2629, plain, ((e4 = op(e0, e4)) | ~ spl34_126), inference(backward_demodulation, [], [f112, f942])).
fof(f942, plain, ((e0 = unit) | ~ spl34_126), inference(avatar_component_clause, [], [f940])).
fof(f112, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2742, plain, (~ spl34_68 | ~ spl34_73), inference(avatar_split_clause, [], [f2741, f717, f696])).
fof(f717, plain, (spl34_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_73])])).
fof(f2741, plain, (~ (e2 = op(e2, e1)) | ~ spl34_73), inference(forward_demodulation, [], [f235, f719])).
fof(f719, plain, ((e2 = op(e2, e0)) | ~ spl34_73), inference(avatar_component_clause, [], [f717])).
fof(f235, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f2733, plain, (spl34_49 | ~ spl34_126), inference(avatar_split_clause, [], [f2628, f940, f616])).
fof(f616, plain, (spl34_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_49])])).
fof(f2628, plain, ((e3 = op(e3, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f111, f942])).
fof(f2725, plain, (spl34_25 | ~ spl34_126), inference(avatar_split_clause, [], [f2630, f940, f515])).
fof(f515, plain, (spl34_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_25])])).
fof(f2630, plain, ((e4 = op(e4, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f113, f942])).
fof(f2699, plain, (~ spl34_62 | ~ spl34_65), inference(avatar_contradiction_clause, [], [f2698])).
fof(f2698, plain, ($false | (~ spl34_62 | ~ spl34_65)), inference(subsumption_resolution, [], [f2697, f271])).
fof(f271, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f2697, plain, ((e1 = e4) | (~ spl34_62 | ~ spl34_65)), inference(backward_demodulation, [], [f685, f673])).
fof(f673, plain, ((e1 = op(e2, e2)) | ~ spl34_62), inference(avatar_component_clause, [], [f671])).
fof(f671, plain, (spl34_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_62])])).
fof(f2638, plain, (spl34_97 | ~ spl34_126), inference(avatar_split_clause, [], [f2624, f940, f818])).
fof(f818, plain, (spl34_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_97])])).
fof(f2624, plain, ((e1 = op(e1, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f107, f942])).
fof(f2637, plain, (spl34_117 | ~ spl34_126), inference(avatar_split_clause, [], [f2623, f940, f902])).
fof(f902, plain, (spl34_117 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_117])])).
fof(f2623, plain, ((e1 = op(e0, e1)) | ~ spl34_126), inference(backward_demodulation, [], [f106, f942])).
fof(f106, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f2636, plain, (spl34_121 | ~ spl34_126), inference(avatar_split_clause, [], [f2622, f940, f919])).
fof(f919, plain, (spl34_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_121])])).
fof(f2622, plain, ((e0 = op(e0, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f105, f942])).
fof(f2593, plain, (~ spl34_74 | ~ spl34_54), inference(avatar_split_clause, [], [f2078, f637, f721])).
fof(f721, plain, (spl34_74 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_74])])).
fof(f637, plain, (spl34_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_54])])).
fof(f2078, plain, (~ (e3 = op(e2, e0)) | ~ spl34_54), inference(forward_demodulation, [], [f238, f639])).
fof(f639, plain, ((e3 = op(e2, e4)) | ~ spl34_54), inference(avatar_component_clause, [], [f637])).
fof(f238, plain, ~ (op(e2, e0) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f2578, plain, (~ spl34_33 | ~ spl34_8), inference(avatar_split_clause, [], [f2577, f444, f549])).
fof(f549, plain, (spl34_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_33])])).
fof(f444, plain, (spl34_8 <=> (e2 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_8])])).
fof(f2577, plain, (~ (e2 = op(e3, e3)) | ~ spl34_8), inference(forward_demodulation, [], [f204, f446])).
fof(f446, plain, ((e2 = op(e4, e3)) | ~ spl34_8), inference(avatar_component_clause, [], [f444])).
fof(f204, plain, ~ (op(e3, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f2576, plain, (~ spl34_1 | ~ spl34_2), inference(avatar_contradiction_clause, [], [f2575])).
fof(f2575, plain, ($false | (~ spl34_1 | ~ spl34_2)), inference(subsumption_resolution, [], [f2574, f265])).
fof(f265, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f2574, plain, ((e0 = e1) | (~ spl34_1 | ~ spl34_2)), inference(forward_demodulation, [], [f421, f417])).
fof(f417, plain, ((e0 = op(e4, e4)) | ~ spl34_1), inference(avatar_component_clause, [], [f415])).
fof(f415, plain, (spl34_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_1])])).
fof(f421, plain, ((e1 = op(e4, e4)) | ~ spl34_2), inference(avatar_component_clause, [], [f419])).
fof(f419, plain, (spl34_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_2])])).
fof(f2465, plain, (~ spl34_86 | ~ spl34_127), inference(avatar_contradiction_clause, [], [f2464])).
fof(f2464, plain, ($false | (~ spl34_86 | ~ spl34_127)), inference(subsumption_resolution, [], [f2463, f266])).
fof(f2463, plain, ((e0 = e2) | (~ spl34_86 | ~ spl34_127)), inference(forward_demodulation, [], [f2449, f774])).
fof(f774, plain, ((e0 = op(e1, e2)) | ~ spl34_86), inference(avatar_component_clause, [], [f772])).
fof(f772, plain, (spl34_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_86])])).
fof(f2449, plain, ((e2 = op(e1, e2)) | ~ spl34_127), inference(backward_demodulation, [], [f108, f946])).
fof(f2300, plain, (spl34_63 | ~ spl34_128), inference(avatar_contradiction_clause, [], [f2299])).
fof(f2299, plain, ($false | (spl34_63 | ~ spl34_128)), inference(subsumption_resolution, [], [f2284, f676])).
fof(f676, plain, (~ (e2 = op(e2, e2)) | spl34_63), inference(avatar_component_clause, [], [f675])).
fof(f675, plain, (spl34_63 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_63])])).
fof(f2284, plain, ((e2 = op(e2, e2)) | ~ spl34_128), inference(backward_demodulation, [], [f109, f950])).
fof(f950, plain, ((e2 = unit) | ~ spl34_128), inference(avatar_component_clause, [], [f948])).
fof(f948, plain, (spl34_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_128])])).
fof(f2239, plain, (~ spl34_124 | ~ spl34_49), inference(avatar_split_clause, [], [f2238, f616, f931])).
fof(f931, plain, (spl34_124 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl34_124])])).
fof(f2238, plain, (~ (op(e0, e0) = e3) | ~ spl34_49), inference(forward_demodulation, [], [f167, f618])).
fof(f618, plain, ((e3 = op(e3, e0)) | ~ spl34_49), inference(avatar_component_clause, [], [f616])).
fof(f2233, plain, (~ spl34_47 | ~ spl34_27), inference(avatar_split_clause, [], [f2064, f524, f608])).
fof(f608, plain, (spl34_47 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_47])])).
fof(f524, plain, (spl34_27 <=> (e1 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_27])])).
fof(f2064, plain, (~ (e1 = op(e3, e0)) | ~ spl34_27), inference(forward_demodulation, [], [f248, f526])).
fof(f526, plain, ((e1 = op(e3, e4)) | ~ spl34_27), inference(avatar_component_clause, [], [f524])).
fof(f248, plain, ~ (op(e3, e0) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2230, plain, (~ spl34_44 | ~ spl34_49), inference(avatar_contradiction_clause, [], [f2229])).
fof(f2229, plain, ($false | (~ spl34_44 | ~ spl34_49)), inference(subsumption_resolution, [], [f2071, f618])).
fof(f2071, plain, (~ (e3 = op(e3, e0)) | ~ spl34_44), inference(forward_demodulation, [], [f245, f597])).
fof(f597, plain, ((e3 = op(e3, e1)) | ~ spl34_44), inference(avatar_component_clause, [], [f595])).
fof(f245, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f2228, plain, (~ spl34_48 | ~ spl34_23), inference(avatar_split_clause, [], [f2073, f507, f612])).
fof(f507, plain, (spl34_23 <=> (e2 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_23])])).
fof(f2073, plain, (~ (e2 = op(e3, e0)) | ~ spl34_23), inference(forward_demodulation, [], [f174, f509])).
fof(f509, plain, ((e2 = op(e4, e0)) | ~ spl34_23), inference(avatar_component_clause, [], [f507])).
fof(f174, plain, ~ (op(e3, e0) = op(e4, e0)), inference(cnf_transformation, [], [f4])).
fof(f2227, plain, (~ spl34_34 | ~ spl34_49), inference(avatar_split_clause, [], [f2226, f616, f553])).
fof(f553, plain, (spl34_34 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_34])])).
fof(f2226, plain, (~ (e3 = op(e3, e3)) | ~ spl34_49), inference(backward_demodulation, [], [f247, f618])).
fof(f247, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2223, plain, (~ spl34_32 | ~ spl34_27), inference(avatar_split_clause, [], [f2222, f524, f545])).
fof(f545, plain, (spl34_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_32])])).
fof(f2222, plain, (~ (e1 = op(e3, e3)) | ~ spl34_27), inference(forward_demodulation, [], [f254, f526])).
fof(f254, plain, ~ (op(e3, e3) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2219, plain, (~ spl34_34 | ~ spl34_44), inference(avatar_split_clause, [], [f2218, f595, f553])).
fof(f2218, plain, (~ (e3 = op(e3, e3)) | ~ spl34_44), inference(forward_demodulation, [], [f250, f597])).
fof(f250, plain, ~ (op(e3, e1) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2149, plain, (~ spl34_27 | ~ spl34_129), inference(avatar_contradiction_clause, [], [f2148])).
fof(f2148, plain, ($false | (~ spl34_27 | ~ spl34_129)), inference(subsumption_resolution, [], [f2147, f271])).
fof(f2147, plain, ((e1 = e4) | (~ spl34_27 | ~ spl34_129)), inference(forward_demodulation, [], [f2133, f526])).
fof(f2133, plain, ((e4 = op(e3, e4)) | ~ spl34_129), inference(backward_demodulation, [], [f112, f954])).
fof(f954, plain, ((e3 = unit) | ~ spl34_129), inference(avatar_component_clause, [], [f952])).
fof(f952, plain, (spl34_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_129])])).
fof(f2116, plain, (~ spl34_117 | ~ spl34_92), inference(avatar_split_clause, [], [f2115, f797, f902])).
fof(f2115, plain, (~ (e1 = op(e0, e1)) | ~ spl34_92), inference(forward_demodulation, [], [f175, f799])).
fof(f799, plain, ((e1 = op(e1, e1)) | ~ spl34_92), inference(avatar_component_clause, [], [f797])).
fof(f175, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f2089, plain, (~ spl34_77 | ~ spl34_27), inference(avatar_split_clause, [], [f1847, f524, f734])).
fof(f734, plain, (spl34_77 <=> (e1 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_77])])).
fof(f1847, plain, (~ (e1 = op(e1, e4)) | ~ spl34_27), inference(forward_demodulation, [], [f210, f526])).
fof(f210, plain, ~ (op(e1, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2036, plain, (~ spl34_1 | ~ spl34_3), inference(avatar_contradiction_clause, [], [f2035])).
fof(f2035, plain, ($false | (~ spl34_1 | ~ spl34_3)), inference(subsumption_resolution, [], [f2034, f266])).
fof(f2034, plain, ((e0 = e2) | (~ spl34_1 | ~ spl34_3)), inference(forward_demodulation, [], [f425, f417])).
fof(f425, plain, ((e2 = op(e4, e4)) | ~ spl34_3), inference(avatar_component_clause, [], [f423])).
fof(f423, plain, (spl34_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_3])])).
fof(f2026, plain, (~ spl34_12 | ~ spl34_14), inference(avatar_contradiction_clause, [], [f2025])).
fof(f2025, plain, ($false | (~ spl34_12 | ~ spl34_14)), inference(subsumption_resolution, [], [f2024, f270])).
fof(f2024, plain, ((e1 = e3) | (~ spl34_12 | ~ spl34_14)), inference(forward_demodulation, [], [f471, f463])).
fof(f463, plain, ((e1 = op(e4, e2)) | ~ spl34_12), inference(avatar_component_clause, [], [f461])).
fof(f461, plain, (spl34_12 <=> (e1 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_12])])).
fof(f471, plain, ((e3 = op(e4, e2)) | ~ spl34_14), inference(avatar_component_clause, [], [f469])).
fof(f469, plain, (spl34_14 <=> (e3 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_14])])).
fof(f1999, plain, (~ spl34_63 | ~ spl34_65), inference(avatar_contradiction_clause, [], [f1998])).
fof(f1998, plain, ($false | (~ spl34_63 | ~ spl34_65)), inference(subsumption_resolution, [], [f1997, f273])).
fof(f273, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f1997, plain, ((e2 = e4) | (~ spl34_63 | ~ spl34_65)), inference(backward_demodulation, [], [f685, f677])).
fof(f677, plain, ((e2 = op(e2, e2)) | ~ spl34_63), inference(avatar_component_clause, [], [f675])).
fof(f1984, plain, (~ spl34_57 | ~ spl34_72), inference(avatar_split_clause, [], [f1982, f713, f650])).
fof(f650, plain, (spl34_57 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_57])])).
fof(f1982, plain, (~ (e1 = op(e2, e3)) | ~ spl34_72), inference(backward_demodulation, [], [f237, f715])).
fof(f237, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f1971, plain, (~ spl34_87 | ~ spl34_92), inference(avatar_split_clause, [], [f1968, f797, f776])).
fof(f776, plain, (spl34_87 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_87])])).
fof(f1968, plain, (~ (e1 = op(e1, e2)) | ~ spl34_92), inference(backward_demodulation, [], [f229, f799])).
fof(f229, plain, ~ (op(e1, e1) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f1946, plain, (~ spl34_107 | ~ spl34_112), inference(avatar_split_clause, [], [f1943, f881, f860])).
fof(f860, plain, (spl34_107 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_107])])).
fof(f881, plain, (spl34_112 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_112])])).
fof(f1943, plain, (~ (e1 = op(e0, e3)) | ~ spl34_112), inference(backward_demodulation, [], [f222, f883])).
fof(f883, plain, ((e1 = op(e0, e2)) | ~ spl34_112), inference(avatar_component_clause, [], [f881])).
fof(f222, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f1937, plain, (~ spl34_97 | ~ spl34_122), inference(avatar_split_clause, [], [f1933, f923, f818])).
fof(f923, plain, (spl34_122 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl34_122])])).
fof(f1933, plain, (~ (e1 = op(e1, e0)) | ~ spl34_122), inference(backward_demodulation, [], [f165, f925])).
fof(f925, plain, ((op(e0, e0) = e1) | ~ spl34_122), inference(avatar_component_clause, [], [f923])).
fof(f165, plain, ~ (op(e0, e0) = op(e1, e0)), inference(cnf_transformation, [], [f4])).
fof(f1897, plain, (~ spl34_121 | ~ spl34_116), inference(avatar_split_clause, [], [f1896, f898, f919])).
fof(f1896, plain, (~ (e0 = op(e0, e0)) | ~ spl34_116), inference(forward_demodulation, [], [f215, f900])).
fof(f215, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f4])).
fof(f1875, plain, (~ spl34_96 | ~ spl34_46), inference(avatar_split_clause, [], [f1874, f604, f814])).
fof(f604, plain, (spl34_46 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_46])])).
fof(f1874, plain, (~ (e0 = op(e1, e0)) | ~ spl34_46), inference(forward_demodulation, [], [f170, f606])).
fof(f606, plain, ((e0 = op(e3, e0)) | ~ spl34_46), inference(avatar_component_clause, [], [f604])).
fof(f170, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f1833, plain, (~ spl34_75 | ~ spl34_65), inference(avatar_split_clause, [], [f1832, f683, f725])).
fof(f725, plain, (spl34_75 <=> (e4 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_75])])).
fof(f1832, plain, (~ (e4 = op(e2, e0)) | ~ spl34_65), inference(forward_demodulation, [], [f236, f685])).
fof(f236, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f1802, plain, (~ spl34_37 | ~ spl34_27), inference(avatar_split_clause, [], [f1801, f524, f566])).
fof(f566, plain, (spl34_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_37])])).
fof(f1801, plain, (~ (e1 = op(e3, e2)) | ~ spl34_27), inference(forward_demodulation, [], [f253, f526])).
fof(f253, plain, ~ (op(e3, e2) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1788, plain, (~ spl34_18 | ~ spl34_20), inference(avatar_contradiction_clause, [], [f1787])).
fof(f1787, plain, ($false | (~ spl34_18 | ~ spl34_20)), inference(subsumption_resolution, [], [f1786, f273])).
fof(f1786, plain, ((e2 = e4) | (~ spl34_18 | ~ spl34_20)), inference(forward_demodulation, [], [f496, f488])).
fof(f488, plain, ((e2 = op(e4, e1)) | ~ spl34_18), inference(avatar_component_clause, [], [f486])).
fof(f486, plain, (spl34_18 <=> (e2 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_18])])).
fof(f496, plain, ((e4 = op(e4, e1)) | ~ spl34_20), inference(avatar_component_clause, [], [f494])).
fof(f1782, plain, (~ spl34_11 | ~ spl34_1), inference(avatar_split_clause, [], [f1781, f415, f457])).
fof(f457, plain, (spl34_11 <=> (e0 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_11])])).
fof(f1781, plain, (~ (e0 = op(e4, e2)) | ~ spl34_1), inference(forward_demodulation, [], [f263, f417])).
fof(f263, plain, ~ (op(e4, e2) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1777, plain, (~ spl34_1 | ~ spl34_4), inference(avatar_contradiction_clause, [], [f1776])).
fof(f1776, plain, ($false | (~ spl34_1 | ~ spl34_4)), inference(subsumption_resolution, [], [f1775, f267])).
fof(f267, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f1775, plain, ((e0 = e3) | (~ spl34_1 | ~ spl34_4)), inference(backward_demodulation, [], [f429, f417])).
fof(f429, plain, ((e3 = op(e4, e4)) | ~ spl34_4), inference(avatar_component_clause, [], [f427])).
fof(f427, plain, (spl34_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_4])])).
fof(f1774, plain, (~ spl34_7 | ~ spl34_8), inference(avatar_contradiction_clause, [], [f1773])).
fof(f1773, plain, ($false | (~ spl34_7 | ~ spl34_8)), inference(subsumption_resolution, [], [f1772, f269])).
fof(f269, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f1772, plain, ((e1 = e2) | (~ spl34_7 | ~ spl34_8)), inference(backward_demodulation, [], [f446, f442])).
fof(f442, plain, ((e1 = op(e4, e3)) | ~ spl34_7), inference(avatar_component_clause, [], [f440])).
fof(f440, plain, (spl34_7 <=> (e1 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_7])])).
fof(f1771, plain, (~ spl34_8 | ~ spl34_9), inference(avatar_contradiction_clause, [], [f1770])).
fof(f1770, plain, ($false | (~ spl34_8 | ~ spl34_9)), inference(subsumption_resolution, [], [f1769, f272])).
fof(f272, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f1769, plain, ((e2 = e3) | (~ spl34_8 | ~ spl34_9)), inference(backward_demodulation, [], [f450, f446])).
fof(f450, plain, ((e3 = op(e4, e3)) | ~ spl34_9), inference(avatar_component_clause, [], [f448])).
fof(f448, plain, (spl34_9 <=> (e3 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_9])])).
fof(f1752, plain, (~ spl34_19 | ~ spl34_20), inference(avatar_contradiction_clause, [], [f1751])).
fof(f1751, plain, ($false | (~ spl34_19 | ~ spl34_20)), inference(subsumption_resolution, [], [f1750, f274])).
fof(f274, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f1750, plain, ((e3 = e4) | (~ spl34_19 | ~ spl34_20)), inference(backward_demodulation, [], [f496, f492])).
fof(f492, plain, ((e3 = op(e4, e1)) | ~ spl34_19), inference(avatar_component_clause, [], [f490])).
fof(f490, plain, (spl34_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_19])])).
fof(f1749, plain, (~ spl34_5 | ~ spl34_20), inference(avatar_split_clause, [], [f1746, f494, f431])).
fof(f431, plain, (spl34_5 <=> (e4 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_5])])).
fof(f1746, plain, (~ (e4 = op(e4, e4)) | ~ spl34_20), inference(backward_demodulation, [], [f261, f496])).
fof(f261, plain, ~ (op(e4, e1) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1713, plain, (~ spl34_31 | ~ spl34_36), inference(avatar_split_clause, [], [f1710, f562, f541])).
fof(f541, plain, (spl34_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_31])])).
fof(f1710, plain, (~ (e0 = op(e3, e3)) | ~ spl34_36), inference(backward_demodulation, [], [f252, f564])).
fof(f564, plain, ((e0 = op(e3, e2)) | ~ spl34_36), inference(avatar_component_clause, [], [f562])).
fof(f252, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1677, plain, (~ spl34_64 | ~ spl34_65), inference(avatar_contradiction_clause, [], [f1676])).
fof(f1676, plain, ($false | (~ spl34_64 | ~ spl34_65)), inference(subsumption_resolution, [], [f1675, f274])).
fof(f1675, plain, ((e3 = e4) | (~ spl34_64 | ~ spl34_65)), inference(backward_demodulation, [], [f685, f681])).
fof(f681, plain, ((e3 = op(e2, e2)) | ~ spl34_64), inference(avatar_component_clause, [], [f679])).
fof(f679, plain, (spl34_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_64])])).
fof(f1674, plain, (spl34_27 | ~ spl34_65), inference(avatar_split_clause, [], [f1667, f683, f524])).
fof(f1667, plain, ((e1 = op(e3, e4)) | ~ spl34_65), inference(backward_demodulation, [], [f1011, f685])).
fof(f1011, plain, (e1 = op(e3, op(e2, e2))), inference(forward_demodulation, [], [f276, f277])).
fof(f277, plain, (e3 = op(e2, op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e4 = op(e2, e2)) & (e3 = op(e2, op(e2, e2))) & (e1 = op(op(e2, op(e2, e2)), op(e2, e2))) & (e0 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG056+1.p', ax6)).
fof(f276, plain, (e1 = op(op(e2, op(e2, e2)), op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f1673, plain, (spl34_54 | ~ spl34_65), inference(avatar_split_clause, [], [f1666, f683, f637])).
fof(f1666, plain, ((e3 = op(e2, e4)) | ~ spl34_65), inference(backward_demodulation, [], [f277, f685])).
fof(f1672, plain, (spl34_1 | ~ spl34_65), inference(avatar_split_clause, [], [f1665, f683, f415])).
fof(f1665, plain, ((e0 = op(e4, e4)) | ~ spl34_65), inference(backward_demodulation, [], [f275, f685])).
fof(f275, plain, (e0 = op(op(e2, e2), op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f1580, plain, (~ spl34_96 | spl34_180), inference(avatar_contradiction_clause, [], [f1579])).
fof(f1579, plain, ($false | (~ spl34_96 | spl34_180)), inference(subsumption_resolution, [], [f1568, f816])).
fof(f816, plain, ((e0 = op(e1, e0)) | ~ spl34_96), inference(avatar_component_clause, [], [f814])).
fof(f1568, plain, (~ (e0 = op(e1, e0)) | (~ spl34_96 | spl34_180)), inference(backward_demodulation, [], [f1300, f816])).
fof(f1300, plain, (~ (e0 = op(e1, op(e1, e0))) | spl34_180), inference(avatar_component_clause, [], [f1298])).
fof(f1298, plain, (spl34_180 <=> (e0 = op(e1, op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_180])])).
fof(f1569, plain, (~ spl34_71 | ~ spl34_96), inference(avatar_split_clause, [], [f1560, f814, f709])).
fof(f709, plain, (spl34_71 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_71])])).
fof(f1560, plain, (~ (e0 = op(e2, e0)) | ~ spl34_96), inference(backward_demodulation, [], [f169, f816])).
fof(f169, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f1550, plain, (~ spl34_5 | ~ spl34_105), inference(avatar_split_clause, [], [f1546, f851, f431])).
fof(f1546, plain, (~ (e4 = op(e4, e4)) | ~ spl34_105), inference(backward_demodulation, [], [f208, f853])).
fof(f853, plain, ((e4 = op(e0, e4)) | ~ spl34_105), inference(avatar_component_clause, [], [f851])).
fof(f208, plain, ~ (op(e0, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1516, plain, (~ spl34_111 | ~ spl34_116), inference(avatar_split_clause, [], [f1507, f898, f877])).
fof(f877, plain, (spl34_111 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_111])])).
fof(f1507, plain, (~ (e0 = op(e0, e2)) | ~ spl34_116), inference(backward_demodulation, [], [f219, f900])).
fof(f219, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f4])).
fof(f1512, plain, (~ spl34_91 | ~ spl34_116), inference(avatar_split_clause, [], [f1503, f898, f793])).
fof(f793, plain, (spl34_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_91])])).
fof(f1503, plain, (~ (e0 = op(e1, e1)) | ~ spl34_116), inference(backward_demodulation, [], [f175, f900])).
fof(f1480, plain, (spl34_77 | ~ spl34_130), inference(avatar_split_clause, [], [f1470, f956, f734])).
fof(f956, plain, (spl34_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_130])])).
fof(f1470, plain, ((e1 = op(e1, e4)) | ~ spl34_130), inference(backward_demodulation, [], [f107, f958])).
fof(f958, plain, ((e4 = unit) | ~ spl34_130), inference(avatar_component_clause, [], [f956])).
fof(f1457, plain, (spl34_181 | spl34_178 | spl34_203 | ~ spl34_207 | spl34_205), inference(avatar_split_clause, [], [f376, f1444, f1454, f1436, f1289, f1303])).
fof(f1303, plain, (spl34_181 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl34_181])])).
fof(f1289, plain, (spl34_178 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl34_178])])).
fof(f376, plain, ((e4 = op(e0, op(e4, e0))) | ~ (e0 = op(e3, op(e3, e0))) | (e2 = op(e0, op(e2, e0))) | sP1 | sP0), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | sP9 | sP8) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | sP7 | sP6) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | sP5 | sP4) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | sP3 | sP2) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | sP1 | sP0)), inference(definition_folding, [], [f9, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
fof(f10, plain, (((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, (((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, (((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1)))) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f13, plain, (((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ~ sP3), inference(usedef, [], [e13])).
fof(e13, plain, (sP3 <=> ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f14, plain, (((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2)))) | ~ sP4), inference(usedef, [], [e14])).
fof(e14, plain, (sP4 <=> ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f15, plain, (((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ~ sP5), inference(usedef, [], [e15])).
fof(e15, plain, (sP5 <=> ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f16, plain, (((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3)))) | ~ sP6), inference(usedef, [], [e16])).
fof(e16, plain, (sP6 <=> ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f17, plain, (((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ~ sP7), inference(usedef, [], [e17])).
fof(e17, plain, (sP7 <=> ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f18, plain, (((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4)))) | ~ sP8), inference(usedef, [], [e18])).
fof(e18, plain, (sP8 <=> ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f19, plain, (((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ~ sP9), inference(usedef, [], [e19])).
fof(e19, plain, (sP9 <=> ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f20, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP10), inference(usedef, [], [e20])).
fof(e20, plain, (sP10 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f21, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP11), inference(usedef, [], [e21])).
fof(e21, plain, (sP11 <=> (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f22, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP12), inference(usedef, [], [e22])).
fof(e22, plain, (sP12 <=> (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f23, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP13), inference(usedef, [], [e23])).
fof(e23, plain, (sP13 <=> (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f24, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP14), inference(usedef, [], [e24])).
fof(e24, plain, (sP14 <=> (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f25, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP15), inference(usedef, [], [e25])).
fof(e25, plain, (sP15 <=> (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f26, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP16), inference(usedef, [], [e26])).
fof(e26, plain, (sP16 <=> (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f27, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP17), inference(usedef, [], [e27])).
fof(e27, plain, (sP17 <=> (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f28, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP18), inference(usedef, [], [e28])).
fof(e28, plain, (sP18 <=> (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f29, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP19), inference(usedef, [], [e29])).
fof(e29, plain, (sP19 <=> (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f30, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP20), inference(usedef, [], [e30])).
fof(e30, plain, (sP20 <=> (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f31, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP21), inference(usedef, [], [e31])).
fof(e31, plain, (sP21 <=> (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f32, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP22), inference(usedef, [], [e32])).
fof(e32, plain, (sP22 <=> (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f33, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP23), inference(usedef, [], [e33])).
fof(e33, plain, (sP23 <=> (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f34, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP24), inference(usedef, [], [e34])).
fof(e34, plain, (sP24 <=> (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f35, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP25), inference(usedef, [], [e35])).
fof(e35, plain, (sP25 <=> (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f36, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP26), inference(usedef, [], [e36])).
fof(e36, plain, (sP26 <=> (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f37, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP27), inference(usedef, [], [e37])).
fof(e37, plain, (sP27 <=> (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f38, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP28), inference(usedef, [], [e38])).
fof(e38, plain, (sP28 <=> (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f39, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP29), inference(usedef, [], [e39])).
fof(e39, plain, (sP29 <=> (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f40, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP30), inference(usedef, [], [e40])).
fof(e40, plain, (sP30 <=> (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f41, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP31), inference(usedef, [], [e41])).
fof(e41, plain, (sP31 <=> (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f42, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP32), inference(usedef, [], [e42])).
fof(e42, plain, (sP32 <=> (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f43, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP33), inference(usedef, [], [e43])).
fof(e43, plain, (sP33 <=> (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f9, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG056+1.p', co1)).
fof(f1314, plain, (spl34_154 | spl34_153 | spl34_152 | spl34_151 | spl34_150 | spl34_149 | spl34_148 | spl34_147 | spl34_146 | spl34_145 | spl34_144 | spl34_143 | spl34_142 | spl34_141 | spl34_140 | spl34_139 | spl34_138 | spl34_137 | spl34_136 | spl34_135 | spl34_134 | spl34_133 | spl34_132 | spl34_131 | spl34_5), inference(avatar_split_clause, [], [f411, f431, f1013, f1020, f1027, f1034, f1041, f1048, f1055, f1062, f1069, f1076, f1083, f1090, f1097, f1104, f1111, f1118, f1125, f1132, f1139, f1146, f1153, f1160, f1167, f1174])).
fof(f1174, plain, (spl34_154 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl34_154])])).
fof(f1167, plain, (spl34_153 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl34_153])])).
fof(f1160, plain, (spl34_152 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl34_152])])).
fof(f1153, plain, (spl34_151 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl34_151])])).
fof(f1146, plain, (spl34_150 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl34_150])])).
fof(f1139, plain, (spl34_149 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl34_149])])).
fof(f1132, plain, (spl34_148 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl34_148])])).
fof(f1125, plain, (spl34_147 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl34_147])])).
fof(f1118, plain, (spl34_146 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl34_146])])).
fof(f1111, plain, (spl34_145 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl34_145])])).
fof(f1104, plain, (spl34_144 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl34_144])])).
fof(f1097, plain, (spl34_143 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl34_143])])).
fof(f1090, plain, (spl34_142 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl34_142])])).
fof(f1083, plain, (spl34_141 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl34_141])])).
fof(f1076, plain, (spl34_140 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl34_140])])).
fof(f1069, plain, (spl34_139 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl34_139])])).
fof(f1062, plain, (spl34_138 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl34_138])])).
fof(f1055, plain, (spl34_137 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl34_137])])).
fof(f1048, plain, (spl34_136 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl34_136])])).
fof(f1041, plain, (spl34_135 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl34_135])])).
fof(f1034, plain, (spl34_134 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl34_134])])).
fof(f1027, plain, (spl34_133 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl34_133])])).
fof(f1020, plain, (spl34_132 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl34_132])])).
fof(f1013, plain, (spl34_131 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl34_131])])).
fof(f411, plain, ((e4 = op(e4, e4)) | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10), inference(cnf_transformation, [], [f44])).
fof(f1311, plain, (~ spl34_181 | ~ spl34_182), inference(avatar_split_clause, [], [f369, f1307, f1303])).
fof(f1307, plain, (spl34_182 <=> (e0 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_182])])).
fof(f369, plain, (~ (e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1310, plain, (~ spl34_181 | spl34_182), inference(avatar_split_clause, [], [f370, f1307, f1303])).
fof(f370, plain, ((e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f78])).
fof(f1301, plain, (~ spl34_178 | ~ spl34_180), inference(avatar_split_clause, [], [f367, f1298, f1289])).
fof(f367, plain, (~ (e0 = op(e1, op(e1, e0))) | ~ sP1), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f1179, plain, (~ spl34_154 | spl34_121), inference(avatar_split_clause, [], [f348, f919, f1174])).
fof(f348, plain, ((e0 = op(e0, e0)) | ~ sP10), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP10), inference(nnf_transformation, [], [f20])).
fof(f1177, plain, (~ spl34_154 | ~ spl34_121), inference(avatar_split_clause, [], [f350, f919, f1174])).
fof(f350, plain, (~ (e0 = op(e0, e0)) | ~ sP10), inference(cnf_transformation, [], [f68])).
fof(f1172, plain, (~ spl34_153 | spl34_122), inference(avatar_split_clause, [], [f345, f923, f1167])).
fof(f345, plain, ((op(e0, e0) = e1) | ~ sP11), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP11), inference(nnf_transformation, [], [f21])).
fof(f1170, plain, (~ spl34_153 | ~ spl34_116), inference(avatar_split_clause, [], [f347, f898, f1167])).
fof(f347, plain, (~ (e0 = op(e0, e1)) | ~ sP11), inference(cnf_transformation, [], [f67])).
fof(f1164, plain, (~ spl34_152 | spl34_61), inference(avatar_split_clause, [], [f343, f667, f1160])).
fof(f343, plain, ((e0 = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP12), inference(nnf_transformation, [], [f22])).
fof(f1158, plain, (~ spl34_151 | spl34_124), inference(avatar_split_clause, [], [f339, f931, f1153])).
fof(f339, plain, ((op(e0, e0) = e3) | ~ sP13), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP13), inference(nnf_transformation, [], [f23])).
fof(f1157, plain, (~ spl34_151 | spl34_31), inference(avatar_split_clause, [], [f340, f541, f1153])).
fof(f340, plain, ((e0 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f65])).
fof(f1151, plain, (~ spl34_150 | spl34_125), inference(avatar_split_clause, [], [f336, f935, f1146])).
fof(f336, plain, ((op(e0, e0) = e4) | ~ sP14), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP14), inference(nnf_transformation, [], [f24])).
fof(f1144, plain, (~ spl34_149 | spl34_91), inference(avatar_split_clause, [], [f333, f793, f1139])).
fof(f333, plain, ((e0 = op(e1, e1)) | ~ sP15), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP15), inference(nnf_transformation, [], [f25])).
fof(f1142, plain, (~ spl34_149 | ~ spl34_97), inference(avatar_split_clause, [], [f335, f818, f1139])).
fof(f335, plain, (~ (e1 = op(e1, e0)) | ~ sP15), inference(cnf_transformation, [], [f63])).
fof(f1137, plain, (~ spl34_148 | spl34_92), inference(avatar_split_clause, [], [f330, f797, f1132])).
fof(f330, plain, ((e1 = op(e1, e1)) | ~ sP16), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP16), inference(nnf_transformation, [], [f26])).
fof(f1135, plain, (~ spl34_148 | ~ spl34_92), inference(avatar_split_clause, [], [f332, f797, f1132])).
fof(f332, plain, (~ (e1 = op(e1, e1)) | ~ sP16), inference(cnf_transformation, [], [f62])).
fof(f1129, plain, (~ spl34_147 | spl34_62), inference(avatar_split_clause, [], [f328, f671, f1125])).
fof(f328, plain, ((e1 = op(e2, e2)) | ~ sP17), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP17), inference(nnf_transformation, [], [f27])).
fof(f1122, plain, (~ spl34_146 | spl34_32), inference(avatar_split_clause, [], [f325, f545, f1118])).
fof(f325, plain, ((e1 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP18), inference(nnf_transformation, [], [f28])).
fof(f1115, plain, (~ spl34_145 | spl34_2), inference(avatar_split_clause, [], [f322, f419, f1111])).
fof(f322, plain, ((e1 = op(e4, e4)) | ~ sP19), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP19), inference(nnf_transformation, [], [f29])).
fof(f1109, plain, (~ spl34_144 | spl34_61), inference(avatar_split_clause, [], [f318, f667, f1104])).
fof(f318, plain, ((e0 = op(e2, e2)) | ~ sP20), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP20), inference(nnf_transformation, [], [f30])).
fof(f1102, plain, (~ spl34_143 | spl34_62), inference(avatar_split_clause, [], [f315, f671, f1097])).
fof(f315, plain, ((e1 = op(e2, e2)) | ~ sP21), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP21), inference(nnf_transformation, [], [f31])).
fof(f1095, plain, (~ spl34_142 | spl34_63), inference(avatar_split_clause, [], [f312, f675, f1090])).
fof(f312, plain, ((e2 = op(e2, e2)) | ~ sP22), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP22), inference(nnf_transformation, [], [f32])).
fof(f1088, plain, (~ spl34_141 | spl34_64), inference(avatar_split_clause, [], [f309, f679, f1083])).
fof(f309, plain, ((e3 = op(e2, e2)) | ~ sP23), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP23), inference(nnf_transformation, [], [f33])).
fof(f1080, plain, (~ spl34_140 | spl34_3), inference(avatar_split_clause, [], [f307, f423, f1076])).
fof(f307, plain, ((e2 = op(e4, e4)) | ~ sP24), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP24), inference(nnf_transformation, [], [f34])).
fof(f1074, plain, (~ spl34_139 | spl34_31), inference(avatar_split_clause, [], [f303, f541, f1069])).
fof(f303, plain, ((e0 = op(e3, e3)) | ~ sP25), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP25), inference(nnf_transformation, [], [f35])).
fof(f1072, plain, (~ spl34_139 | ~ spl34_49), inference(avatar_split_clause, [], [f305, f616, f1069])).
fof(f305, plain, (~ (e3 = op(e3, e0)) | ~ sP25), inference(cnf_transformation, [], [f53])).
fof(f1067, plain, (~ spl34_138 | spl34_32), inference(avatar_split_clause, [], [f300, f545, f1062])).
fof(f300, plain, ((e1 = op(e3, e3)) | ~ sP26), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP26), inference(nnf_transformation, [], [f36])).
fof(f1059, plain, (~ spl34_137 | spl34_64), inference(avatar_split_clause, [], [f298, f679, f1055])).
fof(f298, plain, ((e3 = op(e2, e2)) | ~ sP27), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP27), inference(nnf_transformation, [], [f37])).
fof(f1053, plain, (~ spl34_136 | spl34_34), inference(avatar_split_clause, [], [f294, f553, f1048])).
fof(f294, plain, ((e3 = op(e3, e3)) | ~ sP28), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP28), inference(nnf_transformation, [], [f38])).
fof(f1045, plain, (~ spl34_135 | spl34_4), inference(avatar_split_clause, [], [f292, f427, f1041])).
fof(f292, plain, ((e3 = op(e4, e4)) | ~ sP29), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP29), inference(nnf_transformation, [], [f39])).
fof(f1038, plain, (~ spl34_134 | spl34_125), inference(avatar_split_clause, [], [f289, f935, f1034])).
fof(f289, plain, ((op(e0, e0) = e4) | ~ sP30), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP30), inference(nnf_transformation, [], [f40])).
fof(f1037, plain, (~ spl34_134 | ~ spl34_25), inference(avatar_split_clause, [], [f290, f515, f1034])).
fof(f290, plain, (~ (e4 = op(e4, e0)) | ~ sP30), inference(cnf_transformation, [], [f48])).
fof(f1032, plain, (~ spl34_133 | spl34_2), inference(avatar_split_clause, [], [f285, f419, f1027])).
fof(f285, plain, ((e1 = op(e4, e4)) | ~ sP31), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP31), inference(nnf_transformation, [], [f41])).
fof(f1025, plain, (~ spl34_132 | spl34_3), inference(avatar_split_clause, [], [f282, f423, f1020])).
fof(f282, plain, ((e2 = op(e4, e4)) | ~ sP32), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP32), inference(nnf_transformation, [], [f42])).
fof(f1018, plain, (~ spl34_131 | spl34_4), inference(avatar_split_clause, [], [f279, f427, f1013])).
fof(f279, plain, ((e3 = op(e4, e4)) | ~ sP33), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP33), inference(nnf_transformation, [], [f43])).
fof(f1010, plain, spl34_65, inference(avatar_split_clause, [], [f278, f683])).
fof(f278, plain, (e4 = op(e2, e2)), inference(cnf_transformation, [], [f6])).
fof(f988, plain, (spl34_111 | spl34_86 | spl34_61 | spl34_36 | spl34_11), inference(avatar_split_clause, [], [f136, f457, f562, f667, f772, f877])).
fof(f136, plain, ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG056+1.p', ax3)).
fof(f986, plain, (spl34_112 | spl34_87 | spl34_62 | spl34_37 | spl34_12), inference(avatar_split_clause, [], [f138, f461, f566, f671, f776, f881])).
fof(f138, plain, ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f976, plain, (spl34_107 | spl34_82 | spl34_57 | spl34_32 | spl34_7), inference(avatar_split_clause, [], [f148, f440, f545, f650, f755, f860])).
fof(f148, plain, ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f965, plain, (spl34_23 | spl34_18 | spl34_13 | spl34_8 | spl34_3), inference(avatar_split_clause, [], [f159, f423, f444, f465, f486, f507])).
fof(f159, plain, ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f963, plain, (spl34_24 | spl34_19 | spl34_14 | spl34_9 | spl34_4), inference(avatar_split_clause, [], [f161, f427, f448, f469, f490, f511])).
fof(f161, plain, ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f959, plain, (spl34_126 | spl34_127 | spl34_128 | spl34_129 | spl34_130), inference(avatar_split_clause, [], [f114, f956, f952, f948, f944, f940])).
fof(f114, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f728, plain, (spl34_71 | spl34_72 | spl34_73 | spl34_74 | spl34_75), inference(avatar_split_clause, [], [f89, f725, f721, f717, f713, f709])).
fof(f89, plain, ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG056+1.p', ax1)).
fof(f623, plain, (spl34_46 | spl34_47 | spl34_48 | spl34_49 | spl34_50), inference(avatar_split_clause, [], [f94, f620, f616, f612, f608, f604])).
fof(f94, plain, ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).
fof(f560, plain, (spl34_31 | spl34_32 | spl34_33 | spl34_34 | spl34_35), inference(avatar_split_clause, [], [f97, f557, f553, f549, f545, f541])).
fof(f97, plain, ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))), inference(cnf_transformation, [], [f1])).