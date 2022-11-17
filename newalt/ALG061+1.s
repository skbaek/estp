fof(f2568, plain, $false, inference(avatar_sat_refutation, [], [f337, f358, f421, f463, f841, f848, f871, f892, f894, f896, f961, f980, f1097, f1098, f1180, f1181, f1200, f1211, f1216, f1255, f1264, f1309, f1317, f1318, f1319, f1322, f1378, f1379, f1386, f1433, f1471, f1477, f1606, f1645, f1673, f1675, f1727, f1823, f1873, f1888, f1975, f2004, f2046, f2108, f2218, f2232, f2266, f2346, f2348, f2349, f2350, f2351, f2352, f2355, f2356, f2376, f2379, f2402, f2409, f2439, f2453, f2461, f2473, f2547])).
fof(f2547, plain, (~ spl10_12 | ~ spl10_66 | ~ spl10_171), inference(avatar_contradiction_clause, [], [f2546])).
fof(f2546, plain, ($false | (~ spl10_12 | ~ spl10_66 | ~ spl10_171)), inference(subsumption_resolution, [], [f2545, f220])).
fof(f220, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG061+1.p', ax5)).
fof(f2545, plain, ((e0 = e4) | (~ spl10_12 | ~ spl10_66 | ~ spl10_171)), inference(forward_demodulation, [], [f2544, f572])).
fof(f572, plain, ((e0 = op(e2, e1)) | ~ spl10_66), inference(avatar_component_clause, [], [f570])).
fof(f570, plain, (spl10_66 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_66])])).
fof(f2544, plain, ((e4 = op(e2, e1)) | (~ spl10_12 | ~ spl10_171)), inference(forward_demodulation, [], [f1096, f345])).
fof(f345, plain, ((e1 = op(e4, e2)) | ~ spl10_12), inference(avatar_component_clause, [], [f343])).
fof(f343, plain, (spl10_12 <=> (e1 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_12])])).
fof(f1096, plain, ((e4 = op(e2, op(e4, e2))) | ~ spl10_171), inference(avatar_component_clause, [], [f1094])).
fof(f1094, plain, (spl10_171 <=> (e4 = op(e2, op(e4, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl10_171])])).
fof(f2473, plain, (~ spl10_10 | ~ spl10_25), inference(avatar_split_clause, [], [f2472, f397, f334])).
fof(f334, plain, (spl10_10 <=> (e4 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_10])])).
fof(f397, plain, (spl10_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_25])])).
fof(f2472, plain, (~ (e4 = op(e4, e3)) | ~ spl10_25), inference(backward_demodulation, [], [f209, f399])).
fof(f399, plain, ((e4 = op(e4, e0)) | ~ spl10_25), inference(avatar_component_clause, [], [f397])).
fof(f209, plain, ~ (op(e4, e0) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG061+1.p', ax4)).
fof(f2461, plain, (~ spl10_40 | ~ spl10_52 | ~ spl10_170), inference(avatar_contradiction_clause, [], [f2460])).
fof(f2460, plain, ($false | (~ spl10_40 | ~ spl10_52 | ~ spl10_170)), inference(subsumption_resolution, [], [f2459, f222])).
fof(f222, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f2459, plain, ((e1 = e3) | (~ spl10_40 | ~ spl10_52 | ~ spl10_170)), inference(forward_demodulation, [], [f2457, f513])).
fof(f513, plain, ((e1 = op(e2, e4)) | ~ spl10_52), inference(avatar_component_clause, [], [f511])).
fof(f511, plain, (spl10_52 <=> (e1 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_52])])).
fof(f2457, plain, ((e3 = op(e2, e4)) | (~ spl10_40 | ~ spl10_170)), inference(backward_demodulation, [], [f1092, f462])).
fof(f462, plain, ((e4 = op(e3, e2)) | ~ spl10_40), inference(avatar_component_clause, [], [f460])).
fof(f460, plain, (spl10_40 <=> (e4 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_40])])).
fof(f1092, plain, ((e3 = op(e2, op(e3, e2))) | ~ spl10_170), inference(avatar_component_clause, [], [f1090])).
fof(f1090, plain, (spl10_170 <=> (e3 = op(e2, op(e3, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl10_170])])).
fof(f2453, plain, (~ spl10_29 | ~ spl10_49), inference(avatar_split_clause, [], [f2451, f498, f414])).
fof(f414, plain, (spl10_29 <=> (e3 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_29])])).
fof(f498, plain, (spl10_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_49])])).
fof(f2451, plain, (~ (e3 = op(e3, e4)) | ~ spl10_49), inference(backward_demodulation, [], [f200, f500])).
fof(f500, plain, ((e3 = op(e3, e0)) | ~ spl10_49), inference(avatar_component_clause, [], [f498])).
fof(f200, plain, ~ (op(e3, e0) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2439, plain, (~ spl10_62 | ~ spl10_64), inference(avatar_contradiction_clause, [], [f2438])).
fof(f2438, plain, ($false | (~ spl10_62 | ~ spl10_64)), inference(subsumption_resolution, [], [f2437, f222])).
fof(f2437, plain, ((e1 = e3) | (~ spl10_62 | ~ spl10_64)), inference(backward_demodulation, [], [f563, f555])).
fof(f555, plain, ((e1 = op(e2, e2)) | ~ spl10_62), inference(avatar_component_clause, [], [f553])).
fof(f553, plain, (spl10_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_62])])).
fof(f563, plain, ((e3 = op(e2, e2)) | ~ spl10_64), inference(avatar_component_clause, [], [f561])).
fof(f561, plain, (spl10_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_64])])).
fof(f2409, plain, (~ spl10_91 | ~ spl10_95), inference(avatar_contradiction_clause, [], [f2408])).
fof(f2408, plain, ($false | (~ spl10_91 | ~ spl10_95)), inference(subsumption_resolution, [], [f2407, f220])).
fof(f2407, plain, ((e0 = e4) | (~ spl10_91 | ~ spl10_95)), inference(backward_demodulation, [], [f693, f677])).
fof(f677, plain, ((e0 = op(e1, e1)) | ~ spl10_91), inference(avatar_component_clause, [], [f675])).
fof(f675, plain, (spl10_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_91])])).
fof(f693, plain, ((e4 = op(e1, e1)) | ~ spl10_95), inference(avatar_component_clause, [], [f691])).
fof(f691, plain, (spl10_95 <=> (e4 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_95])])).
fof(f2402, plain, (~ spl10_77 | ~ spl10_97), inference(avatar_split_clause, [], [f2395, f700, f616])).
fof(f616, plain, (spl10_77 <=> (e1 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_77])])).
fof(f700, plain, (spl10_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_97])])).
fof(f2395, plain, (~ (e1 = op(e1, e4)) | ~ spl10_97), inference(backward_demodulation, [], [f180, f702])).
fof(f702, plain, ((e1 = op(e1, e0)) | ~ spl10_97), inference(avatar_component_clause, [], [f700])).
fof(f180, plain, ~ (op(e1, e0) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f2379, plain, (~ spl10_113 | spl10_148), inference(avatar_contradiction_clause, [], [f2378])).
fof(f2378, plain, ($false | (~ spl10_113 | spl10_148)), inference(subsumption_resolution, [], [f2375, f769])).
fof(f769, plain, ((e2 = op(e0, e2)) | ~ spl10_113), inference(avatar_component_clause, [], [f767])).
fof(f767, plain, (spl10_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_113])])).
fof(f2375, plain, (~ (e2 = op(e0, e2)) | (~ spl10_113 | spl10_148)), inference(backward_demodulation, [], [f979, f769])).
fof(f979, plain, (~ (e2 = op(e0, op(e0, e2))) | spl10_148), inference(avatar_component_clause, [], [f977])).
fof(f977, plain, (spl10_148 <=> (e2 = op(e0, op(e0, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl10_148])])).
fof(f2376, plain, (~ spl10_38 | ~ spl10_113), inference(avatar_split_clause, [], [f2370, f767, f452])).
fof(f452, plain, (spl10_38 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_38])])).
fof(f2370, plain, (~ (e2 = op(e3, e2)) | ~ spl10_113), inference(backward_demodulation, [], [f139, f769])).
fof(f139, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f2356, plain, (spl10_25 | ~ spl10_126), inference(avatar_split_clause, [], [f2340, f822, f397])).
fof(f822, plain, (spl10_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_126])])).
fof(f2340, plain, ((e4 = op(e4, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f65, f824])).
fof(f824, plain, ((e0 = unit) | ~ spl10_126), inference(avatar_component_clause, [], [f822])).
fof(f65, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG061+1.p', ax2)).
fof(f2355, plain, (~ spl10_102 | ~ spl10_126), inference(avatar_contradiction_clause, [], [f2354])).
fof(f2354, plain, ($false | (~ spl10_102 | ~ spl10_126)), inference(subsumption_resolution, [], [f2353, f223])).
fof(f223, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f2353, plain, ((e1 = e4) | (~ spl10_102 | ~ spl10_126)), inference(forward_demodulation, [], [f2339, f723])).
fof(f723, plain, ((e1 = op(e0, e4)) | ~ spl10_102), inference(avatar_component_clause, [], [f721])).
fof(f721, plain, (spl10_102 <=> (e1 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_102])])).
fof(f2339, plain, ((e4 = op(e0, e4)) | ~ spl10_126), inference(backward_demodulation, [], [f64, f824])).
fof(f64, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f2352, plain, (spl10_49 | ~ spl10_126), inference(avatar_split_clause, [], [f2338, f822, f498])).
fof(f2338, plain, ((e3 = op(e3, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f63, f824])).
fof(f63, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f2351, plain, (spl10_109 | ~ spl10_126), inference(avatar_split_clause, [], [f2337, f822, f750])).
fof(f750, plain, (spl10_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_109])])).
fof(f2337, plain, ((e3 = op(e0, e3)) | ~ spl10_126), inference(backward_demodulation, [], [f62, f824])).
fof(f62, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f2350, plain, (spl10_73 | ~ spl10_126), inference(avatar_split_clause, [], [f2336, f822, f599])).
fof(f599, plain, (spl10_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_73])])).
fof(f2336, plain, ((e2 = op(e2, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f61, f824])).
fof(f61, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f2349, plain, (spl10_113 | ~ spl10_126), inference(avatar_split_clause, [], [f2335, f822, f767])).
fof(f2335, plain, ((e2 = op(e0, e2)) | ~ spl10_126), inference(backward_demodulation, [], [f60, f824])).
fof(f60, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f2348, plain, (spl10_97 | ~ spl10_126), inference(avatar_split_clause, [], [f2334, f822, f700])).
fof(f2334, plain, ((e1 = op(e1, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f59, f824])).
fof(f59, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f2346, plain, (spl10_121 | ~ spl10_126), inference(avatar_split_clause, [], [f2332, f822, f801])).
fof(f801, plain, (spl10_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_121])])).
fof(f2332, plain, ((e0 = op(e0, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f57, f824])).
fof(f57, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f2266, plain, (~ spl10_28 | ~ spl10_3), inference(avatar_split_clause, [], [f1825, f305, f410])).
fof(f410, plain, (spl10_28 <=> (e2 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_28])])).
fof(f305, plain, (spl10_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_3])])).
fof(f1825, plain, (~ (e2 = op(e3, e4)) | ~ spl10_3), inference(forward_demodulation, [], [f166, f307])).
fof(f307, plain, ((e2 = op(e4, e4)) | ~ spl10_3), inference(avatar_component_clause, [], [f305])).
fof(f166, plain, ~ (op(e3, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2232, plain, (~ spl10_92 | ~ spl10_95), inference(avatar_contradiction_clause, [], [f2231])).
fof(f2231, plain, ($false | (~ spl10_92 | ~ spl10_95)), inference(subsumption_resolution, [], [f2230, f223])).
fof(f2230, plain, ((e1 = e4) | (~ spl10_92 | ~ spl10_95)), inference(backward_demodulation, [], [f693, f681])).
fof(f681, plain, ((e1 = op(e1, e1)) | ~ spl10_92), inference(avatar_component_clause, [], [f679])).
fof(f679, plain, (spl10_92 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_92])])).
fof(f2218, plain, (~ spl10_3 | ~ spl10_130), inference(avatar_contradiction_clause, [], [f2217])).
fof(f2217, plain, ($false | (~ spl10_3 | ~ spl10_130)), inference(subsumption_resolution, [], [f2216, f225])).
fof(f225, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f2216, plain, ((e2 = e4) | (~ spl10_3 | ~ spl10_130)), inference(forward_demodulation, [], [f2204, f307])).
fof(f2204, plain, ((e4 = op(e4, e4)) | ~ spl10_130), inference(backward_demodulation, [], [f65, f840])).
fof(f840, plain, ((e4 = unit) | ~ spl10_130), inference(avatar_component_clause, [], [f838])).
fof(f838, plain, (spl10_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_130])])).
fof(f2108, plain, (~ spl10_32 | ~ spl10_37), inference(avatar_split_clause, [], [f2104, f448, f427])).
fof(f427, plain, (spl10_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_32])])).
fof(f448, plain, (spl10_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_37])])).
fof(f2104, plain, (~ (e1 = op(e3, e3)) | ~ spl10_37), inference(backward_demodulation, [], [f204, f450])).
fof(f450, plain, ((e1 = op(e3, e2)) | ~ spl10_37), inference(avatar_component_clause, [], [f448])).
fof(f204, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2046, plain, (spl10_106 | ~ spl10_129), inference(avatar_contradiction_clause, [], [f2045])).
fof(f2045, plain, ($false | (spl10_106 | ~ spl10_129)), inference(subsumption_resolution, [], [f2034, f739])).
fof(f739, plain, (~ (e0 = op(e0, e3)) | spl10_106), inference(avatar_component_clause, [], [f738])).
fof(f738, plain, (spl10_106 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_106])])).
fof(f2034, plain, ((e0 = op(e0, e3)) | ~ spl10_129), inference(backward_demodulation, [], [f57, f836])).
fof(f836, plain, ((e3 = unit) | ~ spl10_129), inference(avatar_component_clause, [], [f834])).
fof(f834, plain, (spl10_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_129])])).
fof(f2004, plain, (~ spl10_109 | ~ spl10_9), inference(avatar_split_clause, [], [f2003, f330, f750])).
fof(f330, plain, (spl10_9 <=> (e3 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_9])])).
fof(f2003, plain, (~ (e3 = op(e0, e3)) | ~ spl10_9), inference(forward_demodulation, [], [f150, f332])).
fof(f332, plain, ((e3 = op(e4, e3)) | ~ spl10_9), inference(avatar_component_clause, [], [f330])).
fof(f150, plain, ~ (op(e0, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f1975, plain, (~ spl10_13 | ~ spl10_3), inference(avatar_split_clause, [], [f1974, f305, f347])).
fof(f347, plain, (spl10_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_13])])).
fof(f1974, plain, (~ (e2 = op(e4, e2)) | ~ spl10_3), inference(forward_demodulation, [], [f215, f307])).
fof(f215, plain, ~ (op(e4, e2) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1888, plain, (~ spl10_121 | ~ spl10_122), inference(avatar_contradiction_clause, [], [f1887])).
fof(f1887, plain, ($false | (~ spl10_121 | ~ spl10_122)), inference(subsumption_resolution, [], [f1886, f217])).
fof(f217, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f1886, plain, ((e0 = e1) | (~ spl10_121 | ~ spl10_122)), inference(forward_demodulation, [], [f807, f803])).
fof(f803, plain, ((e0 = op(e0, e0)) | ~ spl10_121), inference(avatar_component_clause, [], [f801])).
fof(f807, plain, ((op(e0, e0) = e1) | ~ spl10_122), inference(avatar_component_clause, [], [f805])).
fof(f805, plain, (spl10_122 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl10_122])])).
fof(f1873, plain, (~ spl10_86 | ~ spl10_128), inference(avatar_contradiction_clause, [], [f1872])).
fof(f1872, plain, ($false | (~ spl10_86 | ~ spl10_128)), inference(subsumption_resolution, [], [f1871, f217])).
fof(f1871, plain, ((e0 = e1) | (~ spl10_86 | ~ spl10_128)), inference(forward_demodulation, [], [f1859, f656])).
fof(f656, plain, ((e0 = op(e1, e2)) | ~ spl10_86), inference(avatar_component_clause, [], [f654])).
fof(f654, plain, (spl10_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_86])])).
fof(f1859, plain, ((e1 = op(e1, e2)) | ~ spl10_128), inference(backward_demodulation, [], [f59, f832])).
fof(f832, plain, ((e2 = unit) | ~ spl10_128), inference(avatar_component_clause, [], [f830])).
fof(f830, plain, (spl10_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_128])])).
fof(f1823, plain, (~ spl10_51 | ~ spl10_26), inference(avatar_split_clause, [], [f1822, f402, f507])).
fof(f507, plain, (spl10_51 <=> (e0 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_51])])).
fof(f402, plain, (spl10_26 <=> (e0 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_26])])).
fof(f1822, plain, (~ (e0 = op(e2, e4)) | ~ spl10_26), inference(forward_demodulation, [], [f164, f404])).
fof(f404, plain, ((e0 = op(e3, e4)) | ~ spl10_26), inference(avatar_component_clause, [], [f402])).
fof(f164, plain, ~ (op(e2, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1727, plain, (~ spl10_86 | ~ spl10_127), inference(avatar_contradiction_clause, [], [f1726])).
fof(f1726, plain, ($false | (~ spl10_86 | ~ spl10_127)), inference(subsumption_resolution, [], [f1725, f218])).
fof(f218, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f1725, plain, ((e0 = e2) | (~ spl10_86 | ~ spl10_127)), inference(forward_demodulation, [], [f1713, f656])).
fof(f1713, plain, ((e2 = op(e1, e2)) | ~ spl10_127), inference(backward_demodulation, [], [f60, f828])).
fof(f828, plain, ((e1 = unit) | ~ spl10_127), inference(avatar_component_clause, [], [f826])).
fof(f826, plain, (spl10_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_127])])).
fof(f1675, plain, (~ spl10_7 | ~ spl10_32), inference(avatar_split_clause, [], [f1674, f427, f322])).
fof(f322, plain, (spl10_7 <=> (e1 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_7])])).
fof(f1674, plain, (~ (e1 = op(e4, e3)) | ~ spl10_32), inference(forward_demodulation, [], [f156, f429])).
fof(f429, plain, ((e1 = op(e3, e3)) | ~ spl10_32), inference(avatar_component_clause, [], [f427])).
fof(f156, plain, ~ (op(e3, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f1673, plain, (~ spl10_27 | ~ spl10_32), inference(avatar_contradiction_clause, [], [f1672])).
fof(f1672, plain, ($false | (~ spl10_27 | ~ spl10_32)), inference(subsumption_resolution, [], [f1671, f429])).
fof(f1671, plain, (~ (e1 = op(e3, e3)) | ~ spl10_27), inference(forward_demodulation, [], [f206, f408])).
fof(f408, plain, ((e1 = op(e3, e4)) | ~ spl10_27), inference(avatar_component_clause, [], [f406])).
fof(f406, plain, (spl10_27 <=> (e1 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_27])])).
fof(f206, plain, ~ (op(e3, e3) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1645, plain, (~ spl10_8 | ~ spl10_3), inference(avatar_split_clause, [], [f1642, f305, f326])).
fof(f326, plain, (spl10_8 <=> (e2 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_8])])).
fof(f1642, plain, (~ (e2 = op(e4, e3)) | ~ spl10_3), inference(backward_demodulation, [], [f216, f307])).
fof(f216, plain, ~ (op(e4, e3) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1606, plain, (~ spl10_72 | ~ spl10_73), inference(avatar_contradiction_clause, [], [f1605])).
fof(f1605, plain, ($false | (~ spl10_72 | ~ spl10_73)), inference(subsumption_resolution, [], [f1604, f221])).
fof(f221, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f1604, plain, ((e1 = e2) | (~ spl10_72 | ~ spl10_73)), inference(backward_demodulation, [], [f601, f597])).
fof(f597, plain, ((e1 = op(e2, e0)) | ~ spl10_72), inference(avatar_component_clause, [], [f595])).
fof(f595, plain, (spl10_72 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_72])])).
fof(f601, plain, ((e2 = op(e2, e0)) | ~ spl10_73), inference(avatar_component_clause, [], [f599])).
fof(f1477, plain, (~ spl10_1 | ~ spl10_3), inference(avatar_contradiction_clause, [], [f1476])).
fof(f1476, plain, ($false | (~ spl10_1 | ~ spl10_3)), inference(subsumption_resolution, [], [f1475, f218])).
fof(f1475, plain, ((e0 = e2) | (~ spl10_1 | ~ spl10_3)), inference(forward_demodulation, [], [f307, f299])).
fof(f299, plain, ((e0 = op(e4, e4)) | ~ spl10_1), inference(avatar_component_clause, [], [f297])).
fof(f297, plain, (spl10_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_1])])).
fof(f1471, plain, (~ spl10_2 | ~ spl10_3), inference(avatar_contradiction_clause, [], [f1470])).
fof(f1470, plain, ($false | (~ spl10_2 | ~ spl10_3)), inference(subsumption_resolution, [], [f1469, f221])).
fof(f1469, plain, ((e1 = e2) | (~ spl10_2 | ~ spl10_3)), inference(backward_demodulation, [], [f307, f303])).
fof(f303, plain, ((e1 = op(e4, e4)) | ~ spl10_2), inference(avatar_component_clause, [], [f301])).
fof(f301, plain, (spl10_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_2])])).
fof(f1433, plain, (~ spl10_15 | ~ spl10_25), inference(avatar_split_clause, [], [f1429, f397, f355])).
fof(f355, plain, (spl10_15 <=> (e4 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_15])])).
fof(f1429, plain, (~ (e4 = op(e4, e2)) | ~ spl10_25), inference(backward_demodulation, [], [f208, f399])).
fof(f208, plain, ~ (op(e4, e0) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1386, plain, (~ spl10_6 | ~ spl10_56), inference(avatar_split_clause, [], [f1383, f528, f318])).
fof(f318, plain, (spl10_6 <=> (e0 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_6])])).
fof(f528, plain, (spl10_56 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_56])])).
fof(f1383, plain, (~ (e0 = op(e4, e3)) | ~ spl10_56), inference(backward_demodulation, [], [f155, f530])).
fof(f530, plain, ((e0 = op(e2, e3)) | ~ spl10_56), inference(avatar_component_clause, [], [f528])).
fof(f155, plain, ~ (op(e2, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f1379, plain, (~ spl10_14 | ~ spl10_64), inference(avatar_split_clause, [], [f1375, f561, f351])).
fof(f351, plain, (spl10_14 <=> (e3 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_14])])).
fof(f1375, plain, (~ (e3 = op(e4, e2)) | ~ spl10_64), inference(backward_demodulation, [], [f145, f563])).
fof(f145, plain, ~ (op(e2, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1378, plain, (~ spl10_39 | ~ spl10_64), inference(avatar_split_clause, [], [f1374, f561, f456])).
fof(f456, plain, (spl10_39 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_39])])).
fof(f1374, plain, (~ (e3 = op(e3, e2)) | ~ spl10_64), inference(backward_demodulation, [], [f144, f563])).
fof(f144, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f1322, plain, (spl10_72 | ~ spl10_86 | ~ spl10_144), inference(avatar_split_clause, [], [f1315, f958, f654, f595])).
fof(f958, plain, (spl10_144 <=> (e1 = op(e2, op(e1, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl10_144])])).
fof(f1315, plain, ((e1 = op(e2, e0)) | (~ spl10_86 | ~ spl10_144)), inference(backward_demodulation, [], [f960, f656])).
fof(f960, plain, ((e1 = op(e2, op(e1, e2))) | ~ spl10_144), inference(avatar_component_clause, [], [f958])).
fof(f1319, plain, (~ spl10_11 | ~ spl10_86), inference(avatar_split_clause, [], [f1312, f654, f339])).
fof(f339, plain, (spl10_11 <=> (e0 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_11])])).
fof(f1312, plain, (~ (e0 = op(e4, e2)) | ~ spl10_86), inference(backward_demodulation, [], [f143, f656])).
fof(f143, plain, ~ (op(e1, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1318, plain, (~ spl10_36 | ~ spl10_86), inference(avatar_split_clause, [], [f1311, f654, f444])).
fof(f444, plain, (spl10_36 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_36])])).
fof(f1311, plain, (~ (e0 = op(e3, e2)) | ~ spl10_86), inference(backward_demodulation, [], [f142, f656])).
fof(f142, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f1317, plain, (~ spl10_61 | ~ spl10_86), inference(avatar_split_clause, [], [f1310, f654, f549])).
fof(f549, plain, (spl10_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_61])])).
fof(f1310, plain, (~ (e0 = op(e2, e2)) | ~ spl10_86), inference(backward_demodulation, [], [f141, f656])).
fof(f141, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f1309, plain, (spl10_3 | ~ spl10_95), inference(avatar_split_clause, [], [f1302, f691, f305])).
fof(f1302, plain, ((e2 = op(e4, e4)) | ~ spl10_95), inference(backward_demodulation, [], [f228, f693])).
fof(f228, plain, (e2 = op(op(e1, e1), op(e1, e1))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e4 = op(e1, e1)) & (e3 = op(op(op(e1, e1), op(e1, e1)), op(op(e1, e1), op(e1, e1)))) & (e2 = op(op(e1, e1), op(e1, e1))) & (e0 = op(e1, op(op(e1, e1), op(e1, e1))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG061+1.p', ax6)).
fof(f1264, plain, (~ spl10_30 | ~ spl10_105), inference(avatar_split_clause, [], [f1260, f733, f418])).
fof(f418, plain, (spl10_30 <=> (e4 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_30])])).
fof(f733, plain, (spl10_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_105])])).
fof(f1260, plain, (~ (e4 = op(e3, e4)) | ~ spl10_105), inference(backward_demodulation, [], [f159, f735])).
fof(f735, plain, ((e4 = op(e0, e4)) | ~ spl10_105), inference(avatar_component_clause, [], [f733])).
fof(f159, plain, ~ (op(e0, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1255, plain, (~ spl10_31 | ~ spl10_106), inference(avatar_split_clause, [], [f1250, f738, f423])).
fof(f423, plain, (spl10_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_31])])).
fof(f1250, plain, (~ (e0 = op(e3, e3)) | ~ spl10_106), inference(backward_demodulation, [], [f149, f740])).
fof(f740, plain, ((e0 = op(e0, e3)) | ~ spl10_106), inference(avatar_component_clause, [], [f738])).
fof(f149, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1216, plain, (~ spl10_106 | ~ spl10_121), inference(avatar_split_clause, [], [f1208, f801, f738])).
fof(f1208, plain, (~ (e0 = op(e0, e3)) | ~ spl10_121), inference(backward_demodulation, [], [f169, f803])).
fof(f169, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f1211, plain, (~ spl10_71 | ~ spl10_121), inference(avatar_split_clause, [], [f1203, f801, f591])).
fof(f591, plain, (spl10_71 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_71])])).
fof(f1203, plain, (~ (e0 = op(e2, e0)) | ~ spl10_121), inference(backward_demodulation, [], [f118, f803])).
fof(f118, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f1200, plain, (spl10_105 | ~ spl10_126), inference(avatar_split_clause, [], [f1190, f822, f733])).
fof(f1190, plain, ((e4 = op(e0, e4)) | ~ spl10_126), inference(backward_demodulation, [], [f64, f824])).
fof(f1181, plain, (spl10_121 | spl10_91 | spl10_61 | spl10_31 | spl10_1), inference(avatar_split_clause, [], [f251, f297, f423, f549, f675, f801])).
fof(f251, plain, ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0))), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | sP9 | sP8) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | sP7 | sP6) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | sP5 | sP4) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | sP3 | sP2) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | sP1 | sP0) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
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
fof(f9, plain, ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))) & ((e4 = op(e4, e4)) | (e4 = op(e3, e3)) | (e4 = op(e2, e2)) | (e4 = op(e1, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e4)) | (e3 = op(e3, e3)) | (e3 = op(e2, e2)) | (e3 = op(e1, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e4)) | (e2 = op(e3, e3)) | (e2 = op(e2, e2)) | (e2 = op(e1, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e4)) | (e0 = op(e3, e3)) | (e0 = op(e2, e2)) | (e0 = op(e1, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG061+1.p', co1)).
fof(f1180, plain, (spl10_122 | spl10_92 | spl10_62 | spl10_32 | spl10_2), inference(avatar_split_clause, [], [f252, f301, f427, f553, f679, f805])).
fof(f252, plain, ((e1 = op(e4, e4)) | (e1 = op(e3, e3)) | (e1 = op(e2, e2)) | (e1 = op(e1, e1)) | (op(e0, e0) = e1)), inference(cnf_transformation, [], [f20])).
fof(f1098, plain, (spl10_146 | spl10_143 | ~ spl10_169 | spl10_170 | spl10_171), inference(avatar_split_clause, [], [f278, f1094, f1090, f1086, f954, f968])).
fof(f968, plain, (spl10_146 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl10_146])])).
fof(f954, plain, (spl10_143 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl10_143])])).
fof(f1086, plain, (spl10_169 <=> (e2 = op(e2, op(e2, e2)))), introduced(avatar_definition, [new_symbols(naming, [spl10_169])])).
fof(f278, plain, ((e4 = op(e2, op(e4, e2))) | (e3 = op(e2, op(e3, e2))) | ~ (e2 = op(e2, op(e2, e2))) | sP5 | sP4), inference(cnf_transformation, [], [f20])).
fof(f1097, plain, (spl10_146 | spl10_143 | spl10_169 | spl10_170 | spl10_171), inference(avatar_split_clause, [], [f279, f1094, f1090, f1086, f954, f968])).
fof(f279, plain, ((e4 = op(e2, op(e4, e2))) | (e3 = op(e2, op(e3, e2))) | (e2 = op(e2, op(e2, e2))) | sP5 | sP4), inference(cnf_transformation, [], [f20])).
fof(f980, plain, (~ spl10_146 | ~ spl10_148), inference(avatar_split_clause, [], [f241, f977, f968])).
fof(f241, plain, (~ (e2 = op(e0, op(e0, e2))) | ~ sP4), inference(cnf_transformation, [], [f26])).
fof(f26, plain, (((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2)))) | ~ sP4), inference(nnf_transformation, [], [f14])).
fof(f961, plain, (~ spl10_143 | spl10_144), inference(avatar_split_clause, [], [f240, f958, f954])).
fof(f240, plain, ((e1 = op(e2, op(e1, e2))) | ~ sP5), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ~ sP5), inference(nnf_transformation, [], [f15])).
fof(f896, plain, spl10_86, inference(avatar_split_clause, [], [f895, f654])).
fof(f895, plain, (e0 = op(e1, e2)), inference(forward_demodulation, [], [f227, f228])).
fof(f227, plain, (e0 = op(e1, op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f6])).
fof(f894, plain, spl10_64, inference(avatar_split_clause, [], [f893, f561])).
fof(f893, plain, (e3 = op(e2, e2)), inference(backward_demodulation, [], [f229, f228])).
fof(f229, plain, (e3 = op(op(op(e1, e1), op(e1, e1)), op(op(e1, e1), op(e1, e1)))), inference(cnf_transformation, [], [f6])).
fof(f892, plain, spl10_95, inference(avatar_split_clause, [], [f230, f691])).
fof(f230, plain, (e4 = op(e1, e1)), inference(cnf_transformation, [], [f6])).
fof(f871, plain, (spl10_71 | spl10_66 | spl10_61 | spl10_56 | spl10_51), inference(avatar_split_clause, [], [f87, f507, f528, f549, f570, f591])).
fof(f87, plain, ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG061+1.p', ax3)).
fof(f848, plain, (spl10_102 | spl10_77 | spl10_52 | spl10_27 | spl10_2), inference(avatar_split_clause, [], [f110, f301, f406, f511, f616, f721])).
fof(f110, plain, ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f841, plain, (spl10_126 | spl10_127 | spl10_128 | spl10_129 | spl10_130), inference(avatar_split_clause, [], [f66, f838, f834, f830, f826, f822])).
fof(f66, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f463, plain, (spl10_36 | spl10_37 | spl10_38 | spl10_39 | spl10_40), inference(avatar_split_clause, [], [f48, f460, f456, f452, f448, f444])).
fof(f48, plain, ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG061+1.p', ax1)).
fof(f421, plain, (spl10_26 | spl10_27 | spl10_28 | spl10_29 | spl10_30), inference(avatar_split_clause, [], [f50, f418, f414, f410, f406, f402])).
fof(f50, plain, ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))), inference(cnf_transformation, [], [f1])).
fof(f358, plain, (spl10_11 | spl10_12 | spl10_13 | spl10_14 | spl10_15), inference(avatar_split_clause, [], [f53, f355, f351, f347, f343, f339])).
fof(f53, plain, ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))), inference(cnf_transformation, [], [f1])).
fof(f337, plain, (spl10_6 | spl10_7 | spl10_8 | spl10_9 | spl10_10), inference(avatar_split_clause, [], [f54, f334, f330, f326, f322, f318])).
fof(f54, plain, ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))), inference(cnf_transformation, [], [f1])).