fof(f1946, plain, $false, inference(avatar_sat_refutation, [], [f502, f508, f519, f520, f521, f991, f1091, f1882, f1909, f1945])).
fof(f1945, plain, (~ spl27_4 | spl27_8), inference(avatar_contradiction_clause, [], [f1944])).
fof(f1944, plain, ($false | (~ spl27_4 | spl27_8)), inference(subsumption_resolution, [], [f1940, f484])).
fof(f484, plain, (l3_lattices(sK3) | ~ spl27_4), inference(avatar_component_clause, [], [f483])).
fof(f483, plain, (spl27_4 <=> l3_lattices(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl27_4])])).
fof(f1940, plain, (~ l3_lattices(sK3) | spl27_8), inference(resolution, [], [f501, f294])).
fof(f294, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0] : ((l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : (l3_lattices(X0) => (l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', dt_k1_lattice2)).
fof(f501, plain, (~ l3_lattices(k1_lattice2(sK3)) | spl27_8), inference(avatar_component_clause, [], [f499])).
fof(f499, plain, (spl27_8 <=> l3_lattices(k1_lattice2(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl27_8])])).
fof(f1909, plain, (spl27_1 | ~ spl27_2 | ~ spl27_4 | spl27_6), inference(avatar_contradiction_clause, [], [f1908])).
fof(f1908, plain, ($false | (spl27_1 | ~ spl27_2 | ~ spl27_4 | spl27_6)), inference(subsumption_resolution, [], [f1899, f786])).
fof(f786, plain, (sP0(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_4)), inference(subsumption_resolution, [], [f785, f472])).
fof(f472, plain, (~ v3_struct_0(sK3) | spl27_1), inference(avatar_component_clause, [], [f471])).
fof(f471, plain, (spl27_1 <=> v3_struct_0(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl27_1])])).
fof(f785, plain, (sP0(sK3) | v3_struct_0(sK3) | (~ spl27_2 | ~ spl27_4)), inference(subsumption_resolution, [], [f772, f476])).
fof(f476, plain, (v10_lattices(sK3) | ~ spl27_2), inference(avatar_component_clause, [], [f475])).
fof(f475, plain, (spl27_2 <=> v10_lattices(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl27_2])])).
fof(f772, plain, (sP0(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | ~ spl27_4), inference(resolution, [], [f342, f484])).
fof(f342, plain, ! [X0] : (~ l3_lattices(X0) | sP0(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : (sP0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f145, e165])).
fof(f165, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(usedef, [], [e165])).
fof(e165, plain, ! [X0] : (sP0(X0) <=> (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f145, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f144])).
fof(f144, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', fc6_lattice2)).
fof(f1899, plain, (~ sP0(sK3) | spl27_6), inference(resolution, [], [f493, f341])).
fof(f341, plain, ! [X0] : (v10_lattices(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(nnf_transformation, [], [f165])).
fof(f493, plain, (~ v10_lattices(k1_lattice2(sK3)) | spl27_6), inference(avatar_component_clause, [], [f491])).
fof(f491, plain, (spl27_6 <=> v10_lattices(k1_lattice2(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl27_6])])).
fof(f1882, plain, (spl27_1 | ~ spl27_2 | ~ spl27_3 | ~ spl27_4 | spl27_7), inference(avatar_contradiction_clause, [], [f1881])).
fof(f1881, plain, ($false | (spl27_1 | ~ spl27_2 | ~ spl27_3 | ~ spl27_4 | spl27_7)), inference(subsumption_resolution, [], [f1880, f1076])).
fof(f1076, plain, (v11_lattices(sK3) | (spl27_1 | ~ spl27_3 | ~ spl27_4)), inference(subsumption_resolution, [], [f1075, f484])).
fof(f1075, plain, (v11_lattices(sK3) | ~ l3_lattices(sK3) | (spl27_1 | ~ spl27_3)), inference(subsumption_resolution, [], [f1066, f472])).
fof(f1066, plain, (v11_lattices(sK3) | v3_struct_0(sK3) | ~ l3_lattices(sK3) | ~ spl27_3), inference(resolution, [], [f480, f271])).
fof(f271, plain, ! [X0] : (~ v17_lattices(X0) | v11_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ! [X0] : ((v16_lattices(X0) & v15_lattices(X0) & v14_lattices(X0) & v13_lattices(X0) & v11_lattices(X0) & ~ v3_struct_0(X0)) | ~ v17_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0)), inference(flattening, [], [f110])).
fof(f110, plain, ! [X0] : (((v16_lattices(X0) & v15_lattices(X0) & v14_lattices(X0) & v13_lattices(X0) & v11_lattices(X0) & ~ v3_struct_0(X0)) | (~ v17_lattices(X0) | v3_struct_0(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (l3_lattices(X0) => ((v17_lattices(X0) & ~ v3_struct_0(X0)) => (v16_lattices(X0) & v15_lattices(X0) & v14_lattices(X0) & v13_lattices(X0) & v11_lattices(X0) & ~ v3_struct_0(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', cc5_lattices)).
fof(f480, plain, (v17_lattices(sK3) | ~ spl27_3), inference(avatar_component_clause, [], [f479])).
fof(f479, plain, (spl27_3 <=> v17_lattices(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl27_3])])).
fof(f1880, plain, (~ v11_lattices(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_3 | ~ spl27_4 | spl27_7)), inference(subsumption_resolution, [], [f1879, f823])).
fof(f823, plain, (sP2(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_4)), inference(subsumption_resolution, [], [f822, f472])).
fof(f822, plain, (sP2(sK3) | v3_struct_0(sK3) | (~ spl27_2 | ~ spl27_4)), inference(subsumption_resolution, [], [f809, f476])).
fof(f809, plain, (sP2(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | ~ spl27_4), inference(resolution, [], [f453, f484])).
fof(f453, plain, ! [X0] : (~ l3_lattices(X0) | sP2(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f169])).
fof(f169, plain, ! [X0] : (sP2(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f158, e168, e167])).
fof(f167, plain, ! [X0] : (sP1(X0) <=> (l3_lattices(X0) & v16_lattices(X0) & v15_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(usedef, [], [e167])).
fof(e167, plain, ! [X0] : (sP1(X0) <=> (l3_lattices(X0) & v16_lattices(X0) & v15_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f168, plain, ! [X0] : ((sP1(X0) <=> (l3_lattices(k1_lattice2(X0)) & v16_lattices(k1_lattice2(X0)) & v15_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))) | ~ sP2(X0)), inference(usedef, [], [e168])).
fof(e168, plain, ! [X0] : (sP2(X0) <=> (sP1(X0) <=> (l3_lattices(k1_lattice2(X0)) & v16_lattices(k1_lattice2(X0)) & v15_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f158, plain, ! [X0] : (((l3_lattices(X0) & v16_lattices(X0) & v15_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v16_lattices(k1_lattice2(X0)) & v15_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f157])).
fof(f157, plain, ! [X0] : (((l3_lattices(X0) & v16_lattices(X0) & v15_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v16_lattices(k1_lattice2(X0)) & v15_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ((l3_lattices(X0) & v16_lattices(X0) & v15_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v16_lattices(k1_lattice2(X0)) & v15_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', t53_filter_2)).
fof(f1879, plain, (~ sP2(sK3) | ~ v11_lattices(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_3 | ~ spl27_4 | spl27_7)), inference(subsumption_resolution, [], [f1873, f1082])).
fof(f1082, plain, (sP1(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_3 | ~ spl27_4)), inference(subsumption_resolution, [], [f1081, f472])).
fof(f1081, plain, (sP1(sK3) | v3_struct_0(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_3 | ~ spl27_4)), inference(subsumption_resolution, [], [f1080, f476])).
fof(f1080, plain, (sP1(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | (spl27_1 | ~ spl27_3 | ~ spl27_4)), inference(subsumption_resolution, [], [f1079, f1070])).
fof(f1070, plain, (v15_lattices(sK3) | (spl27_1 | ~ spl27_3 | ~ spl27_4)), inference(subsumption_resolution, [], [f1069, f484])).
fof(f1069, plain, (v15_lattices(sK3) | ~ l3_lattices(sK3) | (spl27_1 | ~ spl27_3)), inference(subsumption_resolution, [], [f1063, f472])).
fof(f1063, plain, (v15_lattices(sK3) | v3_struct_0(sK3) | ~ l3_lattices(sK3) | ~ spl27_3), inference(resolution, [], [f480, f274])).
fof(f274, plain, ! [X0] : (~ v17_lattices(X0) | v15_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f111])).
fof(f1079, plain, (sP1(sK3) | ~ v15_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | (spl27_1 | ~ spl27_3 | ~ spl27_4)), inference(subsumption_resolution, [], [f1077, f484])).
fof(f1077, plain, (~ l3_lattices(sK3) | sP1(sK3) | ~ v15_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | (spl27_1 | ~ spl27_3 | ~ spl27_4)), inference(resolution, [], [f1068, f452])).
fof(f452, plain, ! [X0] : (~ v16_lattices(X0) | ~ l3_lattices(X0) | sP1(X0) | ~ v15_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f227])).
fof(f227, plain, ! [X0] : ((sP1(X0) | ~ l3_lattices(X0) | ~ v16_lattices(X0) | ~ v15_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)) & ((l3_lattices(X0) & v16_lattices(X0) & v15_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) | ~ sP1(X0))), inference(flattening, [], [f226])).
fof(f226, plain, ! [X0] : ((sP1(X0) | (~ l3_lattices(X0) | ~ v16_lattices(X0) | ~ v15_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))) & ((l3_lattices(X0) & v16_lattices(X0) & v15_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) | ~ sP1(X0))), inference(nnf_transformation, [], [f167])).
fof(f1068, plain, (v16_lattices(sK3) | (spl27_1 | ~ spl27_3 | ~ spl27_4)), inference(subsumption_resolution, [], [f1067, f484])).
fof(f1067, plain, (v16_lattices(sK3) | ~ l3_lattices(sK3) | (spl27_1 | ~ spl27_3)), inference(subsumption_resolution, [], [f1062, f472])).
fof(f1062, plain, (v16_lattices(sK3) | v3_struct_0(sK3) | ~ l3_lattices(sK3) | ~ spl27_3), inference(resolution, [], [f480, f275])).
fof(f275, plain, ! [X0] : (~ v17_lattices(X0) | v16_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f111])).
fof(f1873, plain, (~ sP1(sK3) | ~ sP2(sK3) | ~ v11_lattices(sK3) | spl27_7), inference(resolution, [], [f1520, f497])).
fof(f497, plain, (~ v17_lattices(k1_lattice2(sK3)) | spl27_7), inference(avatar_component_clause, [], [f495])).
fof(f495, plain, (spl27_7 <=> v17_lattices(k1_lattice2(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl27_7])])).
fof(f1520, plain, ! [X0] : (v17_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0) | ~ v11_lattices(X0)), inference(subsumption_resolution, [], [f1519, f447])).
fof(f447, plain, ! [X0] : (~ sP1(X0) | ~ v3_struct_0(X0)), inference(cnf_transformation, [], [f227])).
fof(f1519, plain, ! [X0] : (v17_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0) | ~ v11_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f1518, f448])).
fof(f448, plain, ! [X0] : (~ sP1(X0) | v10_lattices(X0)), inference(cnf_transformation, [], [f227])).
fof(f1518, plain, ! [X0] : (v17_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0) | ~ v11_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f1517, f451])).
fof(f451, plain, ! [X0] : (~ sP1(X0) | l3_lattices(X0)), inference(cnf_transformation, [], [f227])).
fof(f1517, plain, ! [X0] : (v17_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0) | ~ l3_lattices(X0) | ~ v11_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(resolution, [], [f885, f468])).
fof(f468, plain, ! [X0] : (v11_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0) | ~ v11_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(duplicate_literal_removal, [], [f457])).
fof(f457, plain, ! [X0] : (v11_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0) | ~ v11_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f229])).
fof(f229, plain, ! [X0] : ((((l3_lattices(X0) & v11_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) | ~ l3_lattices(k1_lattice2(X0)) | ~ v11_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0))) & ((l3_lattices(k1_lattice2(X0)) & v11_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v11_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f228])).
fof(f228, plain, ! [X0] : ((((l3_lattices(X0) & v11_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) | (~ l3_lattices(k1_lattice2(X0)) | ~ v11_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)))) & ((l3_lattices(k1_lattice2(X0)) & v11_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v11_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f161])).
fof(f161, plain, ! [X0] : (((l3_lattices(X0) & v11_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v11_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f160])).
fof(f160, plain, ! [X0] : (((l3_lattices(X0) & v11_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v11_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ((l3_lattices(X0) & v11_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v11_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', t65_lattice2)).
fof(f885, plain, ! [X0] : (~ v11_lattices(k1_lattice2(X0)) | v17_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(subsumption_resolution, [], [f884, f445])).
fof(f445, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f225])).
fof(f225, plain, ! [X0] : (((sP1(X0) | ~ l3_lattices(k1_lattice2(X0)) | ~ v16_lattices(k1_lattice2(X0)) | ~ v15_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0))) & ((l3_lattices(k1_lattice2(X0)) & v16_lattices(k1_lattice2(X0)) & v15_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP1(X0))) | ~ sP2(X0)), inference(flattening, [], [f224])).
fof(f224, plain, ! [X0] : (((sP1(X0) | (~ l3_lattices(k1_lattice2(X0)) | ~ v16_lattices(k1_lattice2(X0)) | ~ v15_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)))) & ((l3_lattices(k1_lattice2(X0)) & v16_lattices(k1_lattice2(X0)) & v15_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP1(X0))) | ~ sP2(X0)), inference(nnf_transformation, [], [f168])).
fof(f884, plain, ! [X0] : (v17_lattices(k1_lattice2(X0)) | ~ v11_lattices(k1_lattice2(X0)) | ~ l3_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(subsumption_resolution, [], [f883, f441])).
fof(f441, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f225])).
fof(f883, plain, ! [X0] : (v17_lattices(k1_lattice2(X0)) | ~ v11_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(subsumption_resolution, [], [f870, f443])).
fof(f443, plain, ! [X0] : (v15_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f225])).
fof(f870, plain, ! [X0] : (v17_lattices(k1_lattice2(X0)) | ~ v15_lattices(k1_lattice2(X0)) | ~ v11_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(resolution, [], [f277, f444])).
fof(f444, plain, ! [X0] : (v16_lattices(k1_lattice2(X0)) | ~ sP1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f225])).
fof(f277, plain, ! [X0] : (~ v16_lattices(X0) | v17_lattices(X0) | ~ v15_lattices(X0) | ~ v11_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ! [X0] : ((v17_lattices(X0) & ~ v3_struct_0(X0)) | ~ v16_lattices(X0) | ~ v15_lattices(X0) | ~ v11_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0)), inference(flattening, [], [f112])).
fof(f112, plain, ! [X0] : (((v17_lattices(X0) & ~ v3_struct_0(X0)) | (~ v16_lattices(X0) | ~ v15_lattices(X0) | ~ v11_lattices(X0) | v3_struct_0(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (l3_lattices(X0) => ((v16_lattices(X0) & v15_lattices(X0) & v11_lattices(X0) & ~ v3_struct_0(X0)) => (v17_lattices(X0) & ~ v3_struct_0(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', cc6_lattices)).
fof(f1091, plain, (spl27_1 | ~ spl27_4 | ~ spl27_5), inference(avatar_contradiction_clause, [], [f1090])).
fof(f1090, plain, ($false | (spl27_1 | ~ spl27_4 | ~ spl27_5)), inference(subsumption_resolution, [], [f1089, f472])).
fof(f1089, plain, (v3_struct_0(sK3) | (~ spl27_4 | ~ spl27_5)), inference(subsumption_resolution, [], [f1084, f484])).
fof(f1084, plain, (~ l3_lattices(sK3) | v3_struct_0(sK3) | ~ spl27_5), inference(resolution, [], [f489, f313])).
fof(f313, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f128])).
fof(f128, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : ((l3_lattices(X0) & ~ v3_struct_0(X0)) => (v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', fc1_lattice2)).
fof(f489, plain, (v3_struct_0(k1_lattice2(sK3)) | ~ spl27_5), inference(avatar_component_clause, [], [f487])).
fof(f487, plain, (spl27_5 <=> v3_struct_0(k1_lattice2(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl27_5])])).
fof(f991, plain, (spl27_1 | ~ spl27_2 | spl27_3 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8), inference(avatar_contradiction_clause, [], [f990])).
fof(f990, plain, ($false | (spl27_1 | ~ spl27_2 | spl27_3 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f989, f484])).
fof(f989, plain, (~ l3_lattices(sK3) | (spl27_1 | ~ spl27_2 | spl27_3 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f988, f472])).
fof(f988, plain, (v3_struct_0(sK3) | ~ l3_lattices(sK3) | (spl27_1 | ~ spl27_2 | spl27_3 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f987, f956])).
fof(f956, plain, (v11_lattices(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f955, f472])).
fof(f955, plain, (v11_lattices(sK3) | v3_struct_0(sK3) | (~ spl27_2 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f954, f476])).
fof(f954, plain, (v11_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | (~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f953, f484])).
fof(f953, plain, (v11_lattices(sK3) | ~ l3_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | (spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f950, f492])).
fof(f492, plain, (v10_lattices(k1_lattice2(sK3)) | ~ spl27_6), inference(avatar_component_clause, [], [f491])).
fof(f950, plain, (v11_lattices(sK3) | ~ v10_lattices(k1_lattice2(sK3)) | ~ l3_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3) | (spl27_5 | ~ spl27_7 | ~ spl27_8)), inference(resolution, [], [f523, f752])).
fof(f752, plain, (v11_lattices(k1_lattice2(sK3)) | (spl27_5 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f751, f500])).
fof(f500, plain, (l3_lattices(k1_lattice2(sK3)) | ~ spl27_8), inference(avatar_component_clause, [], [f499])).
fof(f751, plain, (v11_lattices(k1_lattice2(sK3)) | ~ l3_lattices(k1_lattice2(sK3)) | (spl27_5 | ~ spl27_7)), inference(subsumption_resolution, [], [f750, f488])).
fof(f488, plain, (~ v3_struct_0(k1_lattice2(sK3)) | spl27_5), inference(avatar_component_clause, [], [f487])).
fof(f750, plain, (v11_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ l3_lattices(k1_lattice2(sK3)) | ~ spl27_7), inference(resolution, [], [f271, f496])).
fof(f496, plain, (v17_lattices(k1_lattice2(sK3)) | ~ spl27_7), inference(avatar_component_clause, [], [f495])).
fof(f523, plain, ! [X0] : (~ v11_lattices(k1_lattice2(X0)) | v11_lattices(X0) | ~ v10_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f522, f313])).
fof(f522, plain, ! [X0] : (v11_lattices(X0) | ~ v11_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f461, f294])).
fof(f461, plain, ! [X0] : (v11_lattices(X0) | ~ l3_lattices(k1_lattice2(X0)) | ~ v11_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f229])).
fof(f987, plain, (~ v11_lattices(sK3) | v3_struct_0(sK3) | ~ l3_lattices(sK3) | (spl27_1 | ~ spl27_2 | spl27_3 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f986, f980])).
fof(f980, plain, (v15_lattices(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(resolution, [], [f977, f449])).
fof(f449, plain, ! [X0] : (~ sP1(X0) | v15_lattices(X0)), inference(cnf_transformation, [], [f227])).
fof(f977, plain, (sP1(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f976, f823])).
fof(f976, plain, (sP1(sK3) | ~ sP2(sK3) | (spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f975, f488])).
fof(f975, plain, (sP1(sK3) | v3_struct_0(k1_lattice2(sK3)) | ~ sP2(sK3) | (spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f974, f492])).
fof(f974, plain, (sP1(sK3) | ~ v10_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ sP2(sK3) | (spl27_5 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f973, f764])).
fof(f764, plain, (v15_lattices(k1_lattice2(sK3)) | (spl27_5 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f763, f500])).
fof(f763, plain, (v15_lattices(k1_lattice2(sK3)) | ~ l3_lattices(k1_lattice2(sK3)) | (spl27_5 | ~ spl27_7)), inference(subsumption_resolution, [], [f762, f488])).
fof(f762, plain, (v15_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ l3_lattices(k1_lattice2(sK3)) | ~ spl27_7), inference(resolution, [], [f274, f496])).
fof(f973, plain, (sP1(sK3) | ~ v15_lattices(k1_lattice2(sK3)) | ~ v10_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ sP2(sK3) | (spl27_5 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f971, f500])).
fof(f971, plain, (~ l3_lattices(k1_lattice2(sK3)) | sP1(sK3) | ~ v15_lattices(k1_lattice2(sK3)) | ~ v10_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ sP2(sK3) | (spl27_5 | ~ spl27_7 | ~ spl27_8)), inference(resolution, [], [f446, f768])).
fof(f768, plain, (v16_lattices(k1_lattice2(sK3)) | (spl27_5 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f767, f500])).
fof(f767, plain, (v16_lattices(k1_lattice2(sK3)) | ~ l3_lattices(k1_lattice2(sK3)) | (spl27_5 | ~ spl27_7)), inference(subsumption_resolution, [], [f766, f488])).
fof(f766, plain, (v16_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ l3_lattices(k1_lattice2(sK3)) | ~ spl27_7), inference(resolution, [], [f275, f496])).
fof(f446, plain, ! [X0] : (~ v16_lattices(k1_lattice2(X0)) | ~ l3_lattices(k1_lattice2(X0)) | sP1(X0) | ~ v15_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ sP2(X0)), inference(cnf_transformation, [], [f225])).
fof(f986, plain, (~ v15_lattices(sK3) | ~ v11_lattices(sK3) | v3_struct_0(sK3) | ~ l3_lattices(sK3) | (spl27_1 | ~ spl27_2 | spl27_3 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(subsumption_resolution, [], [f985, f481])).
fof(f481, plain, (~ v17_lattices(sK3) | spl27_3), inference(avatar_component_clause, [], [f479])).
fof(f985, plain, (v17_lattices(sK3) | ~ v15_lattices(sK3) | ~ v11_lattices(sK3) | v3_struct_0(sK3) | ~ l3_lattices(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(resolution, [], [f979, f277])).
fof(f979, plain, (v16_lattices(sK3) | (spl27_1 | ~ spl27_2 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8)), inference(resolution, [], [f977, f450])).
fof(f450, plain, ! [X0] : (~ sP1(X0) | v16_lattices(X0)), inference(cnf_transformation, [], [f227])).
fof(f521, plain, ~ spl27_1, inference(avatar_split_clause, [], [f230, f471])).
fof(f230, plain, ~ v3_struct_0(sK3), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ((~ l3_lattices(k1_lattice2(sK3)) | ~ v17_lattices(k1_lattice2(sK3)) | ~ v10_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ l3_lattices(sK3) | ~ v17_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3)) & ((l3_lattices(k1_lattice2(sK3)) & v17_lattices(k1_lattice2(sK3)) & v10_lattices(k1_lattice2(sK3)) & ~ v3_struct_0(k1_lattice2(sK3))) | (l3_lattices(sK3) & v17_lattices(sK3) & v10_lattices(sK3) & ~ v3_struct_0(sK3))) & l3_lattices(sK3) & v10_lattices(sK3) & ~ v3_struct_0(sK3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f171, f172])).
fof(f172, plain, (? [X0] : ((~ l3_lattices(k1_lattice2(X0)) | ~ v17_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | ~ v17_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)) & ((l3_lattices(k1_lattice2(X0)) & v17_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (l3_lattices(X0) & v17_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ((~ l3_lattices(k1_lattice2(sK3)) | ~ v17_lattices(k1_lattice2(sK3)) | ~ v10_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ l3_lattices(sK3) | ~ v17_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3)) & ((l3_lattices(k1_lattice2(sK3)) & v17_lattices(k1_lattice2(sK3)) & v10_lattices(k1_lattice2(sK3)) & ~ v3_struct_0(k1_lattice2(sK3))) | (l3_lattices(sK3) & v17_lattices(sK3) & v10_lattices(sK3) & ~ v3_struct_0(sK3))) & l3_lattices(sK3) & v10_lattices(sK3) & ~ v3_struct_0(sK3))), introduced(choice_axiom, [])).
fof(f171, plain, ? [X0] : ((~ l3_lattices(k1_lattice2(X0)) | ~ v17_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | ~ v17_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)) & ((l3_lattices(k1_lattice2(X0)) & v17_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (l3_lattices(X0) & v17_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f170])).
fof(f170, plain, ? [X0] : ((((~ l3_lattices(k1_lattice2(X0)) | ~ v17_lattices(k1_lattice2(X0)) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v17_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))) & ((l3_lattices(k1_lattice2(X0)) & v17_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (l3_lattices(X0) & v17_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(nnf_transformation, [], [f94])).
fof(f94, plain, ? [X0] : (~ ((l3_lattices(X0) & v17_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v17_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f93])).
fof(f93, plain, ? [X0] : (~ ((l3_lattices(X0) & v17_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v17_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))) & (l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ((l3_lattices(X0) & v17_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v17_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ((l3_lattices(X0) & v17_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) <=> (l3_lattices(k1_lattice2(X0)) & v17_lattices(k1_lattice2(X0)) & v10_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT319+1.p', t54_filter_2)).
fof(f520, plain, spl27_2, inference(avatar_split_clause, [], [f231, f475])).
fof(f231, plain, v10_lattices(sK3), inference(cnf_transformation, [], [f173])).
fof(f519, plain, spl27_4, inference(avatar_split_clause, [], [f232, f483])).
fof(f232, plain, l3_lattices(sK3), inference(cnf_transformation, [], [f173])).
fof(f508, plain, (spl27_3 | spl27_7), inference(avatar_split_clause, [], [f243, f495, f479])).
fof(f243, plain, (v17_lattices(k1_lattice2(sK3)) | v17_lattices(sK3)), inference(cnf_transformation, [], [f173])).
fof(f502, plain, (spl27_1 | ~ spl27_2 | ~ spl27_3 | ~ spl27_4 | spl27_5 | ~ spl27_6 | ~ spl27_7 | ~ spl27_8), inference(avatar_split_clause, [], [f249, f499, f495, f491, f487, f483, f479, f475, f471])).
fof(f249, plain, (~ l3_lattices(k1_lattice2(sK3)) | ~ v17_lattices(k1_lattice2(sK3)) | ~ v10_lattices(k1_lattice2(sK3)) | v3_struct_0(k1_lattice2(sK3)) | ~ l3_lattices(sK3) | ~ v17_lattices(sK3) | ~ v10_lattices(sK3) | v3_struct_0(sK3)), inference(cnf_transformation, [], [f173])).