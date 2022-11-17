fof(f2449, plain, $false, inference(avatar_sat_refutation, [], [f860, f869, f872, f877, f878, f880, f890, f893, f895, f911, f912, f1034, f1043, f1044, f1192, f1197, f1199, f1206, f1207, f1208, f1231, f1244, f1249, f1296, f1319, f1499, f1546, f1635, f1657, f1658, f1687, f1696, f1715, f1722, f1743, f1750, f1768, f1769, f1782, f1784, f1798, f1825, f1833, f1866, f1880, f1905, f1940, f1968, f1971, f2021, f2056, f2131, f2184, f2223, f2320, f2321, f2322, f2323, f2324, f2325, f2326, f2337, f2414, f2415, f2445])).
fof(f2445, plain, (~ spl10_1 | ~ spl10_25 | spl10_185), inference(avatar_contradiction_clause, [], [f2444])).
fof(f2444, plain, ($false | (~ spl10_1 | ~ spl10_25 | spl10_185)), inference(subsumption_resolution, [], [f2443, f318])).
fof(f318, plain, ((e0 = op(e4, e4)) | ~ spl10_1), inference(avatar_component_clause, [], [f316])).
fof(f316, plain, (spl10_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_1])])).
fof(f2443, plain, (~ (e0 = op(e4, e4)) | (~ spl10_25 | spl10_185)), inference(forward_demodulation, [], [f1188, f418])).
fof(f418, plain, ((e4 = op(e4, e0)) | ~ spl10_25), inference(avatar_component_clause, [], [f416])).
fof(f416, plain, (spl10_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_25])])).
fof(f1188, plain, (~ (e0 = op(e4, op(e4, e0))) | spl10_185), inference(avatar_component_clause, [], [f1186])).
fof(f1186, plain, (spl10_185 <=> (e0 = op(e4, op(e4, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl10_185])])).
fof(f2415, plain, (~ spl10_31 | ~ spl10_49 | spl10_184), inference(avatar_split_clause, [], [f2412, f1180, f517, f442])).
fof(f442, plain, (spl10_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_31])])).
fof(f517, plain, (spl10_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_49])])).
fof(f1180, plain, (spl10_184 <=> (e0 = op(e3, op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl10_184])])).
fof(f2412, plain, (~ (e0 = op(e3, e3)) | (~ spl10_49 | spl10_184)), inference(backward_demodulation, [], [f1182, f519])).
fof(f519, plain, ((e3 = op(e3, e0)) | ~ spl10_49), inference(avatar_component_clause, [], [f517])).
fof(f1182, plain, (~ (e0 = op(e3, op(e3, e0))) | spl10_184), inference(avatar_component_clause, [], [f1180])).
fof(f2414, plain, (~ spl10_44 | ~ spl10_49), inference(avatar_split_clause, [], [f2407, f517, f496])).
fof(f496, plain, (spl10_44 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_44])])).
fof(f2407, plain, (~ (e3 = op(e3, e1)) | ~ spl10_49), inference(backward_demodulation, [], [f197, f519])).
fof(f197, plain, ~ (op(e3, e0) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG065+1.p', ax4)).
fof(f2337, plain, (~ spl10_71 | ~ spl10_121), inference(avatar_split_clause, [], [f2327, f820, f610])).
fof(f610, plain, (spl10_71 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_71])])).
fof(f820, plain, (spl10_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_121])])).
fof(f2327, plain, (~ (e0 = op(e2, e0)) | ~ spl10_121), inference(backward_demodulation, [], [f118, f822])).
fof(f822, plain, ((e0 = op(e0, e0)) | ~ spl10_121), inference(avatar_component_clause, [], [f820])).
fof(f118, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f2326, plain, (spl10_25 | ~ spl10_126), inference(avatar_split_clause, [], [f2314, f841, f416])).
fof(f841, plain, (spl10_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_126])])).
fof(f2314, plain, ((e4 = op(e4, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f65, f843])).
fof(f843, plain, ((e0 = unit) | ~ spl10_126), inference(avatar_component_clause, [], [f841])).
fof(f65, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG065+1.p', ax2)).
fof(f2325, plain, (spl10_49 | ~ spl10_126), inference(avatar_split_clause, [], [f2312, f841, f517])).
fof(f2312, plain, ((e3 = op(e3, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f63, f843])).
fof(f63, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f2324, plain, (spl10_109 | ~ spl10_126), inference(avatar_split_clause, [], [f2311, f841, f769])).
fof(f769, plain, (spl10_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_109])])).
fof(f2311, plain, ((e3 = op(e0, e3)) | ~ spl10_126), inference(backward_demodulation, [], [f62, f843])).
fof(f62, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f2323, plain, (spl10_73 | ~ spl10_126), inference(avatar_split_clause, [], [f2310, f841, f618])).
fof(f618, plain, (spl10_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_73])])).
fof(f2310, plain, ((e2 = op(e2, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f61, f843])).
fof(f61, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f2322, plain, (spl10_113 | ~ spl10_126), inference(avatar_split_clause, [], [f2309, f841, f786])).
fof(f786, plain, (spl10_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_113])])).
fof(f2309, plain, ((e2 = op(e0, e2)) | ~ spl10_126), inference(backward_demodulation, [], [f60, f843])).
fof(f60, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f2321, plain, (spl10_117 | ~ spl10_126), inference(avatar_split_clause, [], [f2307, f841, f803])).
fof(f803, plain, (spl10_117 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_117])])).
fof(f2307, plain, ((e1 = op(e0, e1)) | ~ spl10_126), inference(backward_demodulation, [], [f58, f843])).
fof(f58, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f2320, plain, (spl10_121 | ~ spl10_126), inference(avatar_split_clause, [], [f2306, f841, f820])).
fof(f2306, plain, ((e0 = op(e0, e0)) | ~ spl10_126), inference(backward_demodulation, [], [f57, f843])).
fof(f57, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f2223, plain, (~ spl10_12 | ~ spl10_130), inference(avatar_contradiction_clause, [], [f2222])).
fof(f2222, plain, ($false | (~ spl10_12 | ~ spl10_130)), inference(subsumption_resolution, [], [f2221, f221])).
fof(f221, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG065+1.p', ax5)).
fof(f2221, plain, ((e1 = e2) | (~ spl10_12 | ~ spl10_130)), inference(forward_demodulation, [], [f2207, f364])).
fof(f364, plain, ((e1 = op(e4, e2)) | ~ spl10_12), inference(avatar_component_clause, [], [f362])).
fof(f362, plain, (spl10_12 <=> (e1 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_12])])).
fof(f2207, plain, ((e2 = op(e4, e2)) | ~ spl10_130), inference(backward_demodulation, [], [f60, f859])).
fof(f859, plain, ((e4 = unit) | ~ spl10_130), inference(avatar_component_clause, [], [f857])).
fof(f857, plain, (spl10_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_130])])).
fof(f2184, plain, (~ spl10_87 | ~ spl10_12), inference(avatar_split_clause, [], [f1765, f362, f677])).
fof(f677, plain, (spl10_87 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_87])])).
fof(f1765, plain, (~ (e1 = op(e1, e2)) | ~ spl10_12), inference(forward_demodulation, [], [f143, f364])).
fof(f143, plain, ~ (op(e1, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f2131, plain, (~ spl10_41 | ~ spl10_43), inference(avatar_contradiction_clause, [], [f2130])).
fof(f2130, plain, ($false | (~ spl10_41 | ~ spl10_43)), inference(subsumption_resolution, [], [f2129, f218])).
fof(f218, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f2129, plain, ((e0 = e2) | (~ spl10_41 | ~ spl10_43)), inference(backward_demodulation, [], [f494, f486])).
fof(f486, plain, ((e0 = op(e3, e1)) | ~ spl10_41), inference(avatar_component_clause, [], [f484])).
fof(f484, plain, (spl10_41 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_41])])).
fof(f494, plain, ((e2 = op(e3, e1)) | ~ spl10_43), inference(avatar_component_clause, [], [f492])).
fof(f492, plain, (spl10_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_43])])).
fof(f2056, plain, (spl10_82 | ~ spl10_129), inference(avatar_contradiction_clause, [], [f2055])).
fof(f2055, plain, ($false | (spl10_82 | ~ spl10_129)), inference(subsumption_resolution, [], [f2042, f657])).
fof(f657, plain, (~ (e1 = op(e1, e3)) | spl10_82), inference(avatar_component_clause, [], [f656])).
fof(f656, plain, (spl10_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_82])])).
fof(f2042, plain, ((e1 = op(e1, e3)) | ~ spl10_129), inference(backward_demodulation, [], [f59, f855])).
fof(f855, plain, ((e3 = unit) | ~ spl10_129), inference(avatar_component_clause, [], [f853])).
fof(f853, plain, (spl10_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_129])])).
fof(f59, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f2021, plain, (~ spl10_76 | ~ spl10_91), inference(avatar_split_clause, [], [f1828, f694, f631])).
fof(f631, plain, (spl10_76 <=> (e0 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_76])])).
fof(f694, plain, (spl10_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_91])])).
fof(f1828, plain, (~ (e0 = op(e1, e4)) | ~ spl10_91), inference(backward_demodulation, [], [f183, f696])).
fof(f696, plain, ((e0 = op(e1, e1)) | ~ spl10_91), inference(avatar_component_clause, [], [f694])).
fof(f183, plain, ~ (op(e1, e1) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f1971, plain, (~ spl10_49 | ~ spl10_50), inference(avatar_contradiction_clause, [], [f1970])).
fof(f1970, plain, ($false | (~ spl10_49 | ~ spl10_50)), inference(subsumption_resolution, [], [f1969, f226])).
fof(f226, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f1969, plain, ((e3 = e4) | (~ spl10_49 | ~ spl10_50)), inference(forward_demodulation, [], [f523, f519])).
fof(f523, plain, ((e4 = op(e3, e0)) | ~ spl10_50), inference(avatar_component_clause, [], [f521])).
fof(f521, plain, (spl10_50 <=> (e4 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_50])])).
fof(f1968, plain, (~ spl10_51 | ~ spl10_54), inference(avatar_contradiction_clause, [], [f1967])).
fof(f1967, plain, ($false | (~ spl10_51 | ~ spl10_54)), inference(subsumption_resolution, [], [f1965, f219])).
fof(f219, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f1965, plain, ((e0 = e3) | (~ spl10_51 | ~ spl10_54)), inference(backward_demodulation, [], [f540, f528])).
fof(f528, plain, ((e0 = op(e2, e4)) | ~ spl10_51), inference(avatar_component_clause, [], [f526])).
fof(f526, plain, (spl10_51 <=> (e0 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_51])])).
fof(f540, plain, ((e3 = op(e2, e4)) | ~ spl10_54), inference(avatar_component_clause, [], [f538])).
fof(f538, plain, (spl10_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_54])])).
fof(f1940, plain, (~ spl10_12 | ~ spl10_128), inference(avatar_contradiction_clause, [], [f1939])).
fof(f1939, plain, ($false | (~ spl10_12 | ~ spl10_128)), inference(subsumption_resolution, [], [f1938, f223])).
fof(f223, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f1938, plain, ((e1 = e4) | (~ spl10_12 | ~ spl10_128)), inference(forward_demodulation, [], [f1918, f364])).
fof(f1918, plain, ((e4 = op(e4, e2)) | ~ spl10_128), inference(backward_demodulation, [], [f65, f851])).
fof(f851, plain, ((e2 = unit) | ~ spl10_128), inference(avatar_component_clause, [], [f849])).
fof(f849, plain, (spl10_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_128])])).
fof(f1905, plain, (~ spl10_61 | ~ spl10_73 | spl10_183), inference(avatar_split_clause, [], [f1904, f1175, f618, f568])).
fof(f568, plain, (spl10_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_61])])).
fof(f1175, plain, (spl10_183 <=> (e0 = op(e2, op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl10_183])])).
fof(f1904, plain, (~ (e0 = op(e2, e2)) | (~ spl10_73 | spl10_183)), inference(forward_demodulation, [], [f1177, f620])).
fof(f620, plain, ((e2 = op(e2, e0)) | ~ spl10_73), inference(avatar_component_clause, [], [f618])).
fof(f1177, plain, (~ (e0 = op(e2, op(e2, e0))) | spl10_183), inference(avatar_component_clause, [], [f1175])).
fof(f1880, plain, (~ spl10_36 | ~ spl10_40), inference(avatar_contradiction_clause, [], [f1879])).
fof(f1879, plain, ($false | (~ spl10_36 | ~ spl10_40)), inference(subsumption_resolution, [], [f1877, f220])).
fof(f220, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f1877, plain, ((e0 = e4) | (~ spl10_36 | ~ spl10_40)), inference(backward_demodulation, [], [f481, f465])).
fof(f465, plain, ((e0 = op(e3, e2)) | ~ spl10_36), inference(avatar_component_clause, [], [f463])).
fof(f463, plain, (spl10_36 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_36])])).
fof(f481, plain, ((e4 = op(e3, e2)) | ~ spl10_40), inference(avatar_component_clause, [], [f479])).
fof(f479, plain, (spl10_40 <=> (e4 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_40])])).
fof(f1866, plain, (~ spl10_56 | ~ spl10_57), inference(avatar_contradiction_clause, [], [f1865])).
fof(f1865, plain, ($false | (~ spl10_56 | ~ spl10_57)), inference(subsumption_resolution, [], [f1864, f217])).
fof(f217, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f1864, plain, ((e0 = e1) | (~ spl10_56 | ~ spl10_57)), inference(backward_demodulation, [], [f553, f549])).
fof(f549, plain, ((e0 = op(e2, e3)) | ~ spl10_56), inference(avatar_component_clause, [], [f547])).
fof(f547, plain, (spl10_56 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_56])])).
fof(f553, plain, ((e1 = op(e2, e3)) | ~ spl10_57), inference(avatar_component_clause, [], [f551])).
fof(f551, plain, (spl10_57 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_57])])).
fof(f1833, plain, (~ spl10_66 | ~ spl10_91), inference(avatar_split_clause, [], [f1826, f694, f589])).
fof(f589, plain, (spl10_66 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_66])])).
fof(f1826, plain, (~ (e0 = op(e2, e1)) | ~ spl10_91), inference(backward_demodulation, [], [f131, f696])).
fof(f131, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f1825, plain, (~ spl10_107 | ~ spl10_109), inference(avatar_contradiction_clause, [], [f1824])).
fof(f1824, plain, ($false | (~ spl10_107 | ~ spl10_109)), inference(subsumption_resolution, [], [f1823, f222])).
fof(f222, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f1823, plain, ((e1 = e3) | (~ spl10_107 | ~ spl10_109)), inference(backward_demodulation, [], [f771, f763])).
fof(f763, plain, ((e1 = op(e0, e3)) | ~ spl10_107), inference(avatar_component_clause, [], [f761])).
fof(f761, plain, (spl10_107 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_107])])).
fof(f771, plain, ((e3 = op(e0, e3)) | ~ spl10_109), inference(avatar_component_clause, [], [f769])).
fof(f1798, plain, (spl10_96 | ~ spl10_127), inference(avatar_contradiction_clause, [], [f1797])).
fof(f1797, plain, ($false | (spl10_96 | ~ spl10_127)), inference(subsumption_resolution, [], [f1787, f716])).
fof(f716, plain, (~ (e0 = op(e1, e0)) | spl10_96), inference(avatar_component_clause, [], [f715])).
fof(f715, plain, (spl10_96 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_96])])).
fof(f1787, plain, ((e0 = op(e1, e0)) | ~ spl10_127), inference(backward_demodulation, [], [f56, f847])).
fof(f847, plain, ((e1 = unit) | ~ spl10_127), inference(avatar_component_clause, [], [f845])).
fof(f845, plain, (spl10_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl10_127])])).
fof(f56, plain, (e0 = op(unit, e0)), inference(cnf_transformation, [], [f2])).
fof(f1784, plain, (~ spl10_118 | ~ spl10_113), inference(avatar_split_clause, [], [f1783, f786, f807])).
fof(f807, plain, (spl10_118 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_118])])).
fof(f1783, plain, (~ (e2 = op(e0, e1)) | ~ spl10_113), inference(forward_demodulation, [], [f171, f788])).
fof(f788, plain, ((e2 = op(e0, e2)) | ~ spl10_113), inference(avatar_component_clause, [], [f786])).
fof(f171, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f4])).
fof(f1782, plain, (~ spl10_119 | ~ spl10_109), inference(avatar_split_clause, [], [f1781, f769, f811])).
fof(f811, plain, (spl10_119 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_119])])).
fof(f1781, plain, (~ (e3 = op(e0, e1)) | ~ spl10_109), inference(forward_demodulation, [], [f172, f771])).
fof(f172, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f1769, plain, (spl10_91 | ~ spl10_12), inference(avatar_split_clause, [], [f1522, f362, f694])).
fof(f1522, plain, ((e0 = op(e1, e1)) | ~ spl10_12), inference(forward_demodulation, [], [f227, f364])).
fof(f227, plain, (e0 = op(op(e4, e2), op(e4, e2))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e3 = op(e2, e4)) & (e1 = op(e4, e2)) & (e0 = op(op(e4, e2), op(e4, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG065+1.p', ax6)).
fof(f1768, plain, (~ spl10_91 | ~ spl10_97 | spl10_156), inference(avatar_split_clause, [], [f1655, f1031, f719, f694])).
fof(f719, plain, (spl10_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_97])])).
fof(f1031, plain, (spl10_156 <=> (e0 = op(e1, op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl10_156])])).
fof(f1655, plain, (~ (e0 = op(e1, e1)) | (~ spl10_97 | spl10_156)), inference(backward_demodulation, [], [f1033, f721])).
fof(f721, plain, ((e1 = op(e1, e0)) | ~ spl10_97), inference(avatar_component_clause, [], [f719])).
fof(f1033, plain, (~ (e0 = op(e1, op(e1, e0))) | spl10_156), inference(avatar_component_clause, [], [f1031])).
fof(f1750, plain, (~ spl10_68 | ~ spl10_73), inference(avatar_split_clause, [], [f1748, f618, f597])).
fof(f597, plain, (spl10_68 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_68])])).
fof(f1748, plain, (~ (e2 = op(e2, e1)) | ~ spl10_73), inference(backward_demodulation, [], [f187, f620])).
fof(f187, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f1743, plain, (~ spl10_69 | ~ spl10_54), inference(avatar_split_clause, [], [f1742, f538, f601])).
fof(f601, plain, (spl10_69 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_69])])).
fof(f1742, plain, (~ (e3 = op(e2, e1)) | ~ spl10_54), inference(forward_demodulation, [], [f193, f540])).
fof(f193, plain, ~ (op(e2, e1) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f1722, plain, (~ spl10_29 | ~ spl10_54), inference(avatar_split_clause, [], [f1721, f538, f433])).
fof(f433, plain, (spl10_29 <=> (e3 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_29])])).
fof(f1721, plain, (~ (e3 = op(e3, e4)) | ~ spl10_54), inference(forward_demodulation, [], [f164, f540])).
fof(f164, plain, ~ (op(e2, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1715, plain, (~ spl10_7 | ~ spl10_12), inference(avatar_split_clause, [], [f1714, f362, f341])).
fof(f341, plain, (spl10_7 <=> (e1 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_7])])).
fof(f1714, plain, (~ (e1 = op(e4, e3)) | ~ spl10_12), inference(forward_demodulation, [], [f214, f364])).
fof(f214, plain, ~ (op(e4, e2) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f1696, plain, (~ spl10_26 | ~ spl10_27), inference(avatar_contradiction_clause, [], [f1695])).
fof(f1695, plain, ($false | (~ spl10_26 | ~ spl10_27)), inference(subsumption_resolution, [], [f1694, f217])).
fof(f1694, plain, ((e0 = e1) | (~ spl10_26 | ~ spl10_27)), inference(backward_demodulation, [], [f427, f423])).
fof(f423, plain, ((e0 = op(e3, e4)) | ~ spl10_26), inference(avatar_component_clause, [], [f421])).
fof(f421, plain, (spl10_26 <=> (e0 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_26])])).
fof(f427, plain, ((e1 = op(e3, e4)) | ~ spl10_27), inference(avatar_component_clause, [], [f425])).
fof(f425, plain, (spl10_27 <=> (e1 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_27])])).
fof(f1687, plain, (~ spl10_43 | ~ spl10_45), inference(avatar_contradiction_clause, [], [f1686])).
fof(f1686, plain, ($false | (~ spl10_43 | ~ spl10_45)), inference(subsumption_resolution, [], [f1685, f225])).
fof(f225, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f1685, plain, ((e2 = e4) | (~ spl10_43 | ~ spl10_45)), inference(backward_demodulation, [], [f502, f494])).
fof(f502, plain, ((e4 = op(e3, e1)) | ~ spl10_45), inference(avatar_component_clause, [], [f500])).
fof(f500, plain, (spl10_45 <=> (e4 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_45])])).
fof(f1658, plain, (~ spl10_82 | ~ spl10_97), inference(avatar_split_clause, [], [f1653, f719, f656])).
fof(f1653, plain, (~ (e1 = op(e1, e3)) | ~ spl10_97), inference(backward_demodulation, [], [f179, f721])).
fof(f179, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f1657, plain, (~ spl10_47 | ~ spl10_97), inference(avatar_split_clause, [], [f1652, f719, f509])).
fof(f509, plain, (spl10_47 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_47])])).
fof(f1652, plain, (~ (e1 = op(e3, e0)) | ~ spl10_97), inference(backward_demodulation, [], [f122, f721])).
fof(f122, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f1635, plain, (~ spl10_42 | ~ spl10_117), inference(avatar_split_clause, [], [f1630, f803, f488])).
fof(f488, plain, (spl10_42 <=> (e1 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_42])])).
fof(f1630, plain, (~ (e1 = op(e3, e1)) | ~ spl10_117), inference(backward_demodulation, [], [f129, f805])).
fof(f805, plain, ((e1 = op(e0, e1)) | ~ spl10_117), inference(avatar_component_clause, [], [f803])).
fof(f129, plain, ~ (op(e0, e1) = op(e3, e1)), inference(cnf_transformation, [], [f4])).
fof(f1546, plain, (~ spl10_37 | ~ spl10_12), inference(avatar_split_clause, [], [f1545, f362, f467])).
fof(f467, plain, (spl10_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl10_37])])).
fof(f1545, plain, (~ (e1 = op(e3, e2)) | ~ spl10_12), inference(forward_demodulation, [], [f146, f364])).
fof(f146, plain, ~ (op(e3, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f1499, plain, (~ spl10_18 | ~ spl10_19), inference(avatar_contradiction_clause, [], [f1498])).
fof(f1498, plain, ($false | (~ spl10_18 | ~ spl10_19)), inference(subsumption_resolution, [], [f1497, f224])).
fof(f224, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f1497, plain, ((e2 = e3) | (~ spl10_18 | ~ spl10_19)), inference(backward_demodulation, [], [f393, f389])).
fof(f389, plain, ((e2 = op(e4, e1)) | ~ spl10_18), inference(avatar_component_clause, [], [f387])).
fof(f387, plain, (spl10_18 <=> (e2 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_18])])).
fof(f393, plain, ((e3 = op(e4, e1)) | ~ spl10_19), inference(avatar_component_clause, [], [f391])).
fof(f391, plain, (spl10_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_19])])).
fof(f1319, plain, (~ spl10_91 | ~ spl10_96), inference(avatar_split_clause, [], [f1310, f715, f694])).
fof(f1310, plain, (~ (e0 = op(e1, e1)) | ~ spl10_96), inference(backward_demodulation, [], [f177, f717])).
fof(f717, plain, ((e0 = op(e1, e0)) | ~ spl10_96), inference(avatar_component_clause, [], [f715])).
fof(f177, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f1296, plain, (~ spl10_30 | ~ spl10_105), inference(avatar_split_clause, [], [f1292, f752, f437])).
fof(f437, plain, (spl10_30 <=> (e4 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_30])])).
fof(f752, plain, (spl10_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_105])])).
fof(f1292, plain, (~ (e4 = op(e3, e4)) | ~ spl10_105), inference(backward_demodulation, [], [f159, f754])).
fof(f754, plain, ((e4 = op(e0, e4)) | ~ spl10_105), inference(avatar_component_clause, [], [f752])).
fof(f159, plain, ~ (op(e0, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1249, plain, (~ spl10_101 | ~ spl10_121), inference(avatar_split_clause, [], [f1240, f820, f736])).
fof(f736, plain, (spl10_101 <=> (e0 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl10_101])])).
fof(f1240, plain, (~ (e0 = op(e0, e4)) | ~ spl10_121), inference(backward_demodulation, [], [f170, f822])).
fof(f170, plain, ~ (op(e0, e0) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f1244, plain, (~ spl10_46 | ~ spl10_121), inference(avatar_split_clause, [], [f1235, f820, f505])).
fof(f505, plain, (spl10_46 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl10_46])])).
fof(f1235, plain, (~ (e0 = op(e3, e0)) | ~ spl10_121), inference(backward_demodulation, [], [f119, f822])).
fof(f119, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f1231, plain, (spl10_105 | ~ spl10_126), inference(avatar_split_clause, [], [f1221, f841, f752])).
fof(f1221, plain, ((e4 = op(e0, e4)) | ~ spl10_126), inference(backward_demodulation, [], [f64, f843])).
fof(f64, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f1208, plain, (~ spl10_91 | spl10_97), inference(avatar_split_clause, [], [f255, f719, f694])).
fof(f255, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | sP9 | sP8) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | sP7 | sP6) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | sP5 | sP4) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | sP3 | sP2) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | sP1 | sP0) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
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
fof(f9, plain, ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ ((((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG065+1.p', co1)).
fof(f1207, plain, (~ spl10_93 | spl10_87), inference(avatar_split_clause, [], [f257, f677, f702])).
fof(f702, plain, (spl10_93 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_93])])).
fof(f257, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f20])).
fof(f1206, plain, (~ spl10_94 | spl10_82), inference(avatar_split_clause, [], [f258, f656, f706])).
fof(f706, plain, (spl10_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl10_94])])).
fof(f258, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f20])).
fof(f1199, plain, (~ spl10_32 | spl10_44), inference(avatar_split_clause, [], [f266, f496, f446])).
fof(f446, plain, (spl10_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_32])])).
fof(f266, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f20])).
fof(f1197, plain, (~ spl10_35 | spl10_29), inference(avatar_split_clause, [], [f269, f433, f458])).
fof(f458, plain, (spl10_35 <=> (e4 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl10_35])])).
fof(f269, plain, ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))), inference(cnf_transformation, [], [f20])).
fof(f1192, plain, (spl10_157 | spl10_154 | ~ spl10_183 | ~ spl10_184 | ~ spl10_185), inference(avatar_split_clause, [], [f275, f1186, f1180, f1175, f1022, f1036])).
fof(f1036, plain, (spl10_157 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl10_157])])).
fof(f1022, plain, (spl10_154 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl10_154])])).
fof(f275, plain, (~ (e0 = op(e4, op(e4, e0))) | ~ (e0 = op(e3, op(e3, e0))) | ~ (e0 = op(e2, op(e2, e0))) | sP1 | sP0), inference(cnf_transformation, [], [f20])).
fof(f1044, plain, (~ spl10_157 | ~ spl10_158), inference(avatar_split_clause, [], [f248, f1040, f1036])).
fof(f1040, plain, (spl10_158 <=> (e0 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl10_158])])).
fof(f248, plain, (~ (e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1043, plain, (~ spl10_157 | spl10_158), inference(avatar_split_clause, [], [f249, f1040, f1036])).
fof(f249, plain, ((e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f30])).
fof(f1034, plain, (~ spl10_154 | ~ spl10_156), inference(avatar_split_clause, [], [f246, f1031, f1022])).
fof(f246, plain, (~ (e0 = op(e1, op(e1, e0))) | ~ sP1), inference(cnf_transformation, [], [f29])).
fof(f29, plain, (((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f912, plain, spl10_12, inference(avatar_split_clause, [], [f228, f362])).
fof(f228, plain, (e1 = op(e4, e2)), inference(cnf_transformation, [], [f6])).
fof(f911, plain, spl10_54, inference(avatar_split_clause, [], [f229, f538])).
fof(f229, plain, (e3 = op(e2, e4)), inference(cnf_transformation, [], [f6])).
fof(f895, plain, (spl10_118 | spl10_93 | spl10_68 | spl10_43 | spl10_18), inference(avatar_split_clause, [], [f82, f387, f492, f597, f702, f807])).
fof(f82, plain, ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG065+1.p', ax3)).
fof(f893, plain, (spl10_119 | spl10_94 | spl10_69 | spl10_44 | spl10_19), inference(avatar_split_clause, [], [f84, f391, f496, f601, f706, f811])).
fof(f84, plain, ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))), inference(cnf_transformation, [], [f3])).
fof(f890, plain, (spl10_71 | spl10_66 | spl10_61 | spl10_56 | spl10_51), inference(avatar_split_clause, [], [f87, f526, f547, f568, f589, f610])).
fof(f87, plain, ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f880, plain, (spl10_46 | spl10_41 | spl10_36 | spl10_31 | spl10_26), inference(avatar_split_clause, [], [f97, f421, f442, f463, f484, f505])).
fof(f97, plain, ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f878, plain, (spl10_47 | spl10_42 | spl10_37 | spl10_32 | spl10_27), inference(avatar_split_clause, [], [f99, f425, f446, f467, f488, f509])).
fof(f99, plain, ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f877, plain, (spl10_107 | spl10_82 | spl10_57 | spl10_32 | spl10_7), inference(avatar_split_clause, [], [f100, f341, f446, f551, f656, f761])).
fof(f100, plain, ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f872, plain, (spl10_50 | spl10_45 | spl10_40 | spl10_35 | spl10_30), inference(avatar_split_clause, [], [f105, f437, f458, f479, f500, f521])).
fof(f105, plain, ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f869, plain, (spl10_101 | spl10_76 | spl10_51 | spl10_26 | spl10_1), inference(avatar_split_clause, [], [f108, f316, f421, f526, f631, f736])).
fof(f108, plain, ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f860, plain, (spl10_126 | spl10_127 | spl10_128 | spl10_129 | spl10_130), inference(avatar_split_clause, [], [f66, f857, f853, f849, f845, f841])).
fof(f66, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).