fof(f14937, plain, $false, inference(avatar_sat_refutation, [], [f988, f997, f1012, f1152, f1185, f1201, f1650, f1653, f5923, f5926, f5929, f5943, f5983, f14936])).
fof(f14936, plain, (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_21 | ~ spl30_81), inference(avatar_contradiction_clause, [], [f14935])).
fof(f14935, plain, ($false | (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_21 | ~ spl30_81)), inference(subsumption_resolution, [], [f14934, f498])).
fof(f498, plain, ! [X0] : r1_tarski(X0, X0), inference(cnf_transformation, [], [f108])).
fof(f108, plain, ! [X0] : r1_tarski(X0, X0), inference(rectify, [], [f92])).
fof(f92, plain, ! [X0, X1] : r1_tarski(X0, X0), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', reflexivity_r1_tarski)).
fof(f14934, plain, (~ r1_tarski(u1_struct_0(sK1), u1_struct_0(sK1)) | (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_21 | ~ spl30_81)), inference(subsumption_resolution, [], [f14921, f795])).
fof(f795, plain, ~ v1_xboole_0(u1_struct_0(sK1)), inference(subsumption_resolution, [], [f794, f326])).
fof(f326, plain, ~ v3_struct_0(sK1), inference(cnf_transformation, [], [f258])).
fof(f258, plain, (~ r1_filter_2(u1_struct_0(sK1), k17_filter_2(sK1), k18_filter_2(sK1, k6_lattices(sK1))) & v14_lattices(sK1) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f121, f257])).
fof(f257, plain, (? [X0] : (~ r1_filter_2(u1_struct_0(X0), k17_filter_2(X0), k18_filter_2(X0, k6_lattices(X0))) & v14_lattices(X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (~ r1_filter_2(u1_struct_0(sK1), k17_filter_2(sK1), k18_filter_2(sK1, k6_lattices(sK1))) & v14_lattices(sK1) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1))), introduced(choice_axiom, [])).
fof(f121, plain, ? [X0] : (~ r1_filter_2(u1_struct_0(X0), k17_filter_2(X0), k18_filter_2(X0, k6_lattices(X0))) & v14_lattices(X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f120])).
fof(f120, plain, ? [X0] : ((~ r1_filter_2(u1_struct_0(X0), k17_filter_2(X0), k18_filter_2(X0, k6_lattices(X0))) & v14_lattices(X0)) & (l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v14_lattices(X0) => r1_filter_2(u1_struct_0(X0), k17_filter_2(X0), k18_filter_2(X0, k6_lattices(X0))))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v14_lattices(X0) => r1_filter_2(u1_struct_0(X0), k17_filter_2(X0), k18_filter_2(X0, k6_lattices(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', t32_filter_2)).
fof(f794, plain, (~ v1_xboole_0(u1_struct_0(sK1)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f793, f327])).
fof(f327, plain, v10_lattices(sK1), inference(cnf_transformation, [], [f258])).
fof(f793, plain, (~ v1_xboole_0(u1_struct_0(sK1)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f790, f328])).
fof(f328, plain, l3_lattices(sK1), inference(cnf_transformation, [], [f258])).
fof(f790, plain, (~ v1_xboole_0(u1_struct_0(sK1)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f374, f769])).
fof(f769, plain, m1_filter_0(u1_struct_0(sK1), sK1), inference(subsumption_resolution, [], [f768, f326])).
fof(f768, plain, (m1_filter_0(u1_struct_0(sK1), sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f767, f327])).
fof(f767, plain, (m1_filter_0(u1_struct_0(sK1), sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f766, f328])).
fof(f766, plain, (m1_filter_0(u1_struct_0(sK1), sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(superposition, [], [f361, f761])).
fof(f761, plain, (u1_struct_0(sK1) = k1_filter_0(sK1)), inference(subsumption_resolution, [], [f760, f326])).
fof(f760, plain, ((u1_struct_0(sK1) = k1_filter_0(sK1)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f754, f327])).
fof(f754, plain, ((u1_struct_0(sK1) = k1_filter_0(sK1)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f351, f328])).
fof(f351, plain, ! [X0] : (~ l3_lattices(X0) | (u1_struct_0(X0) = k1_filter_0(X0)) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ! [X0] : ((u1_struct_0(X0) = k1_filter_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f137])).
fof(f137, plain, ! [X0] : ((u1_struct_0(X0) = k1_filter_0(X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (u1_struct_0(X0) = k1_filter_0(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', d2_filter_0)).
fof(f361, plain, ! [X0] : (m1_filter_0(k1_filter_0(X0), X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f155])).
fof(f155, plain, ! [X0] : (m1_filter_0(k1_filter_0(X0), X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f154])).
fof(f154, plain, ! [X0] : (m1_filter_0(k1_filter_0(X0), X0) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m1_filter_0(k1_filter_0(X0), X0)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_k1_filter_0)).
fof(f374, plain, ! [X0, X1] : (~ m1_filter_0(X1, X0) | ~ v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ! [X0] : (! [X1] : ((m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X1)) | ~ m1_filter_0(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f172])).
fof(f172, plain, ! [X0] : (! [X1] : ((m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X1)) | ~ m1_filter_0(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_filter_0(X1, X0) => (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X1)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_m1_filter_0)).
fof(f14921, plain, (v1_xboole_0(u1_struct_0(sK1)) | ~ r1_tarski(u1_struct_0(sK1), u1_struct_0(sK1)) | (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_21 | ~ spl30_81)), inference(resolution, [], [f14864, f745])).
fof(f745, plain, ! [X0, X1] : (r1_filter_2(X0, X1, X1) | v1_xboole_0(X0) | ~ r1_tarski(X1, X0)), inference(resolution, [], [f521, f508])).
fof(f508, plain, ! [X0, X1] : (m1_subset_1(X0, k1_zfmisc_1(X1)) | ~ r1_tarski(X0, X1)), inference(cnf_transformation, [], [f244])).
fof(f244, plain, ! [X0, X1] : (m1_subset_1(X0, k1_zfmisc_1(X1)) | ~ r1_tarski(X0, X1)), inference(ennf_transformation, [], [f109])).
fof(f109, plain, ! [X0, X1] : (r1_tarski(X0, X1) => m1_subset_1(X0, k1_zfmisc_1(X1))), inference(unused_predicate_definition_removal, [], [f100])).
fof(f100, plain, ! [X0, X1] : (m1_subset_1(X0, k1_zfmisc_1(X1)) <=> r1_tarski(X0, X1)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', t3_subset)).
fof(f521, plain, ! [X2, X0] : (~ m1_subset_1(X2, k1_zfmisc_1(X0)) | r1_filter_2(X0, X2, X2) | v1_xboole_0(X0)), inference(duplicate_literal_removal, [], [f519])).
fof(f519, plain, ! [X2, X0] : (r1_filter_2(X0, X2, X2) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(equality_resolution, [], [f494])).
fof(f494, plain, ! [X2, X0, X1] : (r1_filter_2(X0, X1, X2) | ~ (X1 = X2) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(cnf_transformation, [], [f320])).
fof(f320, plain, ! [X0, X1, X2] : (((r1_filter_2(X0, X1, X2) | ~ (X1 = X2)) & ((X1 = X2) | ~ r1_filter_2(X0, X1, X2))) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(nnf_transformation, [], [f227])).
fof(f227, plain, ! [X0, X1, X2] : ((r1_filter_2(X0, X1, X2) <=> (X1 = X2)) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(flattening, [], [f226])).
fof(f226, plain, ! [X0, X1, X2] : ((r1_filter_2(X0, X1, X2) <=> (X1 = X2)) | (~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0))), inference(ennf_transformation, [], [f89])).
fof(f89, plain, ! [X0, X1, X2] : ((m1_subset_1(X2, k1_zfmisc_1(X0)) & m1_subset_1(X1, k1_zfmisc_1(X0)) & ~ v1_xboole_0(X0)) => (r1_filter_2(X0, X1, X2) <=> (X1 = X2))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', redefinition_r1_filter_2)).
fof(f14864, plain, (~ r1_filter_2(u1_struct_0(sK1), u1_struct_0(sK1), u1_struct_0(sK1)) | (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_21 | ~ spl30_81)), inference(backward_demodulation, [], [f779, f14863])).
fof(f14863, plain, ((u1_struct_0(sK1) = k18_filter_2(sK1, k6_lattices(sK1))) | (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_21 | ~ spl30_81)), inference(backward_demodulation, [], [f3144, f14862])).
fof(f14862, plain, ((u1_struct_0(sK1) = k2_filter_0(k1_lattice2(sK1), k6_lattices(sK1))) | (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_81)), inference(forward_demodulation, [], [f14861, f9128])).
fof(f9128, plain, ((u1_struct_0(sK1) = k1_filter_0(k1_lattice2(sK1))) | (spl30_11 | ~ spl30_17 | ~ spl30_18 | ~ spl30_81)), inference(backward_demodulation, [], [f1244, f9080])).
fof(f9080, plain, ((u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl30_81), inference(trivial_inequality_removal, [], [f9063])).
fof(f9063, plain, (~ (k1_lattice2(sK1) = k1_lattice2(sK1)) | (u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl30_81), inference(superposition, [], [f1649, f2381])).
fof(f2381, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(k1_lattice2(sK1)), u2_lattices(k1_lattice2(sK1)), u1_lattices(k1_lattice2(sK1)))), inference(resolution, [], [f907, f328])).
fof(f907, plain, ! [X1] : (~ l3_lattices(X1) | (k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1))))), inference(subsumption_resolution, [], [f902, f364])).
fof(f364, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : ((l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (l3_lattices(X0) => (l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_k1_lattice2)).
fof(f902, plain, ! [X1] : ((k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1)))) | ~ l3_lattices(k1_lattice2(X1)) | ~ l3_lattices(X1)), inference(resolution, [], [f331, f363])).
fof(f363, plain, ! [X0] : (v3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f158])).
fof(f331, plain, ! [X0] : (~ v3_lattices(X0) | (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ! [X0] : ((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0) | ~ l3_lattices(X0)), inference(flattening, [], [f122])).
fof(f122, plain, ! [X0] : (((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (l3_lattices(X0) => (v3_lattices(X0) => (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', abstractness_v3_lattices)).
fof(f1649, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15)) | ~ spl30_81), inference(avatar_component_clause, [], [f1648])).
fof(f1648, plain, (spl30_81 <=> ! [X16, X15, X17] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15))), introduced(avatar_definition, [new_symbols(naming, [spl30_81])])).
fof(f1244, plain, ((u1_struct_0(k1_lattice2(sK1)) = k1_filter_0(k1_lattice2(sK1))) | (spl30_11 | ~ spl30_17 | ~ spl30_18)), inference(subsumption_resolution, [], [f1243, f978])).
fof(f978, plain, (~ v3_struct_0(k1_lattice2(sK1)) | spl30_11), inference(avatar_component_clause, [], [f977])).
fof(f977, plain, (spl30_11 <=> v3_struct_0(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_11])])).
fof(f1243, plain, ((u1_struct_0(k1_lattice2(sK1)) = k1_filter_0(k1_lattice2(sK1))) | v3_struct_0(k1_lattice2(sK1)) | (~ spl30_17 | ~ spl30_18)), inference(subsumption_resolution, [], [f1224, f1130])).
fof(f1130, plain, (v10_lattices(k1_lattice2(sK1)) | ~ spl30_17), inference(avatar_component_clause, [], [f1129])).
fof(f1129, plain, (spl30_17 <=> v10_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_17])])).
fof(f1224, plain, ((u1_struct_0(k1_lattice2(sK1)) = k1_filter_0(k1_lattice2(sK1))) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl30_18), inference(resolution, [], [f1134, f351])).
fof(f1134, plain, (l3_lattices(k1_lattice2(sK1)) | ~ spl30_18), inference(avatar_component_clause, [], [f1133])).
fof(f1133, plain, (spl30_18 <=> l3_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_18])])).
fof(f14861, plain, (k2_filter_0(k1_lattice2(sK1), k6_lattices(sK1)) = k1_filter_0(k1_lattice2(sK1))), inference(forward_demodulation, [], [f14860, f955])).
fof(f955, plain, (k6_lattices(sK1) = k5_lattices(k1_lattice2(sK1))), inference(subsumption_resolution, [], [f954, f326])).
fof(f954, plain, ((k6_lattices(sK1) = k5_lattices(k1_lattice2(sK1))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f953, f327])).
fof(f953, plain, ((k6_lattices(sK1) = k5_lattices(k1_lattice2(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f951, f328])).
fof(f951, plain, (~ l3_lattices(sK1) | (k6_lattices(sK1) = k5_lattices(k1_lattice2(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f514, f329])).
fof(f329, plain, v14_lattices(sK1), inference(cnf_transformation, [], [f258])).
fof(f514, plain, ! [X0] : (~ v14_lattices(X0) | ~ l3_lattices(X0) | (k6_lattices(X0) = k5_lattices(k1_lattice2(X0))) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f252])).
fof(f252, plain, ! [X0] : ((k6_lattices(X0) = k5_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f251])).
fof(f251, plain, ! [X0] : ((k6_lattices(X0) = k5_lattices(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v14_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f105])).
fof(f105, plain, ! [X0] : ((l3_lattices(X0) & v14_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k6_lattices(X0) = k5_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', t79_lattice2)).
fof(f14860, plain, (k1_filter_0(k1_lattice2(sK1)) = k2_filter_0(k1_lattice2(sK1), k5_lattices(k1_lattice2(sK1)))), inference(subsumption_resolution, [], [f14859, f326])).
fof(f14859, plain, ((k1_filter_0(k1_lattice2(sK1)) = k2_filter_0(k1_lattice2(sK1), k5_lattices(k1_lattice2(sK1)))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f14858, f327])).
fof(f14858, plain, ((k1_filter_0(k1_lattice2(sK1)) = k2_filter_0(k1_lattice2(sK1), k5_lattices(k1_lattice2(sK1)))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f14855, f328])).
fof(f14855, plain, ((k1_filter_0(k1_lattice2(sK1)) = k2_filter_0(k1_lattice2(sK1), k5_lattices(k1_lattice2(sK1)))) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f3438, f329])).
fof(f3438, plain, ! [X0] : (~ v14_lattices(X0) | (k1_filter_0(k1_lattice2(X0)) = k2_filter_0(k1_lattice2(X0), k5_lattices(k1_lattice2(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f3433, f428])).
fof(f428, plain, ! [X0] : (~ l3_lattices(X0) | sP0(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f256])).
fof(f256, plain, ! [X0] : (sP0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f208, e255])).
fof(f255, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(usedef, [], [e255])).
fof(e255, plain, ! [X0] : (sP0(X0) <=> (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f208, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f207])).
fof(f207, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f67])).
fof(f67, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', fc6_lattice2)).
fof(f3433, plain, ! [X0] : ((k1_filter_0(k1_lattice2(X0)) = k2_filter_0(k1_lattice2(X0), k5_lattices(k1_lattice2(X0)))) | ~ v14_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ sP0(X0)), inference(resolution, [], [f1005, f427])).
fof(f427, plain, ! [X0] : (v10_lattices(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f281])).
fof(f281, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(nnf_transformation, [], [f255])).
fof(f1005, plain, ! [X0] : (~ v10_lattices(k1_lattice2(X0)) | (k1_filter_0(k1_lattice2(X0)) = k2_filter_0(k1_lattice2(X0), k5_lattices(k1_lattice2(X0)))) | ~ v14_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f1004, f399])).
fof(f399, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f191])).
fof(f191, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0] : ((l3_lattices(X0) & ~ v3_struct_0(X0)) => (v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', fc1_lattice2)).
fof(f1004, plain, ! [X0] : ((k1_filter_0(k1_lattice2(X0)) = k2_filter_0(k1_lattice2(X0), k5_lattices(k1_lattice2(X0)))) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ v14_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f999, f364])).
fof(f999, plain, ! [X0] : (~ l3_lattices(k1_lattice2(X0)) | (k1_filter_0(k1_lattice2(X0)) = k2_filter_0(k1_lattice2(X0), k5_lattices(k1_lattice2(X0)))) | ~ v10_lattices(k1_lattice2(X0)) | v3_struct_0(k1_lattice2(X0)) | ~ v14_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(resolution, [], [f520, f511])).
fof(f511, plain, ! [X0] : (v13_lattices(k1_lattice2(X0)) | ~ v14_lattices(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f325])).
fof(f325, plain, ! [X0] : (((v14_lattices(X0) | ~ v13_lattices(k1_lattice2(X0))) & (v13_lattices(k1_lattice2(X0)) | ~ v14_lattices(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f249])).
fof(f249, plain, ! [X0] : ((v14_lattices(X0) <=> v13_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f248])).
fof(f248, plain, ! [X0] : ((v14_lattices(X0) <=> v13_lattices(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v14_lattices(X0) <=> v13_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', t64_lattice2)).
fof(f520, plain, ! [X0] : (~ v13_lattices(X0) | ~ l3_lattices(X0) | (k1_filter_0(X0) = k2_filter_0(X0, k5_lattices(X0))) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(duplicate_literal_removal, [], [f502])).
fof(f502, plain, ! [X0] : ((k1_filter_0(X0) = k2_filter_0(X0, k5_lattices(X0))) | ~ l3_lattices(X0) | ~ v13_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f238])).
fof(f238, plain, ! [X0] : ((k1_filter_0(X0) = k2_filter_0(X0, k5_lattices(X0))) | ~ l3_lattices(X0) | ~ v13_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f237])).
fof(f237, plain, ! [X0] : (((k1_filter_0(X0) = k2_filter_0(X0, k5_lattices(X0))) | (~ l3_lattices(X0) | ~ v13_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f96])).
fof(f96, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ((l3_lattices(X0) & v13_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k1_filter_0(X0) = k2_filter_0(X0, k5_lattices(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', t20_filter_0)).
fof(f3144, plain, ((k18_filter_2(sK1, k6_lattices(sK1)) = k2_filter_0(k1_lattice2(sK1), k6_lattices(sK1))) | ~ spl30_21), inference(backward_demodulation, [], [f1151, f3143])).
fof(f3143, plain, (k18_filter_2(sK1, k6_lattices(sK1)) = k2_filter_2(k1_lattice2(sK1), k6_lattices(sK1))), inference(forward_demodulation, [], [f3142, f2193])).
fof(f2193, plain, (k6_lattices(sK1) = k5_filter_2(sK1, k6_lattices(sK1))), inference(subsumption_resolution, [], [f2192, f326])).
fof(f2192, plain, ((k6_lattices(sK1) = k5_filter_2(sK1, k6_lattices(sK1))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f2180, f327])).
fof(f2180, plain, ((k6_lattices(sK1) = k5_filter_2(sK1, k6_lattices(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f965, f328])).
fof(f965, plain, ! [X0] : (~ l3_lattices(X0) | (k6_lattices(X0) = k5_filter_2(X0, k6_lattices(X0))) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f964, f373])).
fof(f373, plain, ! [X0] : (~ l3_lattices(X0) | l2_lattices(X0)), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ! [X0] : ((l2_lattices(X0) & l1_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : (l3_lattices(X0) => (l2_lattices(X0) & l1_lattices(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_l3_lattices)).
fof(f964, plain, ! [X0] : ((k6_lattices(X0) = k5_filter_2(X0, k6_lattices(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l2_lattices(X0)), inference(duplicate_literal_removal, [], [f960])).
fof(f960, plain, ! [X0] : ((k6_lattices(X0) = k5_filter_2(X0, k6_lattices(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l2_lattices(X0) | v3_struct_0(X0)), inference(resolution, [], [f354, f369])).
fof(f369, plain, ! [X0] : (m1_subset_1(k6_lattices(X0), u1_struct_0(X0)) | ~ l2_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0] : (m1_subset_1(k6_lattices(X0), u1_struct_0(X0)) | ~ l2_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f167])).
fof(f167, plain, ! [X0] : (m1_subset_1(k6_lattices(X0), u1_struct_0(X0)) | (~ l2_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : ((l2_lattices(X0) & ~ v3_struct_0(X0)) => m1_subset_1(k6_lattices(X0), u1_struct_0(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_k6_lattices)).
fof(f354, plain, ! [X0, X1] : (~ m1_subset_1(X1, u1_struct_0(X0)) | (k5_filter_2(X0, X1) = X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f143])).
fof(f143, plain, ! [X0] : (! [X1] : ((k5_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f142])).
fof(f142, plain, ! [X0] : (! [X1] : ((k5_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, u1_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => (k5_filter_2(X0, X1) = X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', d4_filter_2)).
fof(f3142, plain, (k18_filter_2(sK1, k6_lattices(sK1)) = k2_filter_2(k1_lattice2(sK1), k5_filter_2(sK1, k6_lattices(sK1)))), inference(subsumption_resolution, [], [f3141, f326])).
fof(f3141, plain, ((k18_filter_2(sK1, k6_lattices(sK1)) = k2_filter_2(k1_lattice2(sK1), k5_filter_2(sK1, k6_lattices(sK1)))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f3123, f327])).
fof(f3123, plain, ((k18_filter_2(sK1, k6_lattices(sK1)) = k2_filter_2(k1_lattice2(sK1), k5_filter_2(sK1, k6_lattices(sK1)))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f1102, f328])).
fof(f1102, plain, ! [X0] : (~ l3_lattices(X0) | (k18_filter_2(X0, k6_lattices(X0)) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, k6_lattices(X0)))) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(subsumption_resolution, [], [f1101, f373])).
fof(f1101, plain, ! [X0] : ((k18_filter_2(X0, k6_lattices(X0)) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, k6_lattices(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l2_lattices(X0)), inference(duplicate_literal_removal, [], [f1096])).
fof(f1096, plain, ! [X0] : ((k18_filter_2(X0, k6_lattices(X0)) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, k6_lattices(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0) | ~ l2_lattices(X0) | v3_struct_0(X0)), inference(resolution, [], [f506, f369])).
fof(f506, plain, ! [X0, X1] : (~ m1_subset_1(X1, u1_struct_0(X0)) | (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ! [X0] : (! [X1] : (((k2_filter_2(X0, X1) = k18_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) & (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1)))) | ~ m1_subset_1(X1, u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f242])).
fof(f242, plain, ! [X0] : (! [X1] : (((k2_filter_2(X0, X1) = k18_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) & (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1)))) | ~ m1_subset_1(X1, u1_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f99])).
fof(f99, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => ((k2_filter_2(X0, X1) = k18_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) & (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', t30_filter_2)).
fof(f1151, plain, ((k2_filter_2(k1_lattice2(sK1), k6_lattices(sK1)) = k2_filter_0(k1_lattice2(sK1), k6_lattices(sK1))) | ~ spl30_21), inference(avatar_component_clause, [], [f1149])).
fof(f1149, plain, (spl30_21 <=> (k2_filter_2(k1_lattice2(sK1), k6_lattices(sK1)) = k2_filter_0(k1_lattice2(sK1), k6_lattices(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_21])])).
fof(f779, plain, ~ r1_filter_2(u1_struct_0(sK1), u1_struct_0(sK1), k18_filter_2(sK1, k6_lattices(sK1))), inference(backward_demodulation, [], [f330, f778])).
fof(f778, plain, (u1_struct_0(sK1) = k17_filter_2(sK1)), inference(subsumption_resolution, [], [f777, f326])).
fof(f777, plain, ((u1_struct_0(sK1) = k17_filter_2(sK1)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f771, f327])).
fof(f771, plain, ((u1_struct_0(sK1) = k17_filter_2(sK1)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f355, f328])).
fof(f355, plain, ! [X0] : (~ l3_lattices(X0) | (u1_struct_0(X0) = k17_filter_2(X0)) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f145])).
fof(f145, plain, ! [X0] : ((u1_struct_0(X0) = k17_filter_2(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f144])).
fof(f144, plain, ! [X0] : ((u1_struct_0(X0) = k17_filter_2(X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (u1_struct_0(X0) = k17_filter_2(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', d8_filter_2)).
fof(f330, plain, ~ r1_filter_2(u1_struct_0(sK1), k17_filter_2(sK1), k18_filter_2(sK1, k6_lattices(sK1))), inference(cnf_transformation, [], [f258])).
fof(f5983, plain, spl30_78, inference(avatar_contradiction_clause, [], [f5982])).
fof(f5982, plain, ($false | spl30_78), inference(subsumption_resolution, [], [f5981, f535])).
fof(f535, plain, l2_lattices(sK1), inference(resolution, [], [f373, f328])).
fof(f5981, plain, (~ l2_lattices(sK1) | spl30_78), inference(resolution, [], [f1638, f874])).
fof(f874, plain, ! [X1] : (m1_relset_1(u2_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l2_lattices(X1)), inference(resolution, [], [f387, f491])).
fof(f491, plain, ! [X2, X0, X1] : (~ m2_relset_1(X2, X0, X1) | m1_relset_1(X2, X0, X1)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0, X1, X2] : ((m2_relset_1(X2, X0, X1) | ~ m1_relset_1(X2, X0, X1)) & (m1_relset_1(X2, X0, X1) | ~ m2_relset_1(X2, X0, X1))), inference(nnf_transformation, [], [f88])).
fof(f88, plain, ! [X0, X1, X2] : (m2_relset_1(X2, X0, X1) <=> m1_relset_1(X2, X0, X1)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', redefinition_m2_relset_1)).
fof(f387, plain, ! [X0] : (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f182])).
fof(f182, plain, ! [X0] : ((m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0))) | ~ l2_lattices(X0)), inference(ennf_transformation, [], [f45])).
fof(f45, plain, ! [X0] : (l2_lattices(X0) => (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_u2_lattices)).
fof(f1638, plain, (~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_78), inference(avatar_component_clause, [], [f1636])).
fof(f1636, plain, (spl30_78 <=> m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_78])])).
fof(f5943, plain, spl30_77, inference(avatar_contradiction_clause, [], [f5942])).
fof(f5942, plain, ($false | spl30_77), inference(subsumption_resolution, [], [f5941, f535])).
fof(f5941, plain, (~ l2_lattices(sK1) | spl30_77), inference(resolution, [], [f1634, f386])).
fof(f386, plain, ! [X0] : (v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f182])).
fof(f1634, plain, (~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_77), inference(avatar_component_clause, [], [f1632])).
fof(f1632, plain, (spl30_77 <=> v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_77])])).
fof(f5929, plain, spl30_76, inference(avatar_contradiction_clause, [], [f5928])).
fof(f5928, plain, ($false | spl30_76), inference(subsumption_resolution, [], [f5927, f535])).
fof(f5927, plain, (~ l2_lattices(sK1) | spl30_76), inference(resolution, [], [f1630, f385])).
fof(f385, plain, ! [X0] : (v1_funct_1(u2_lattices(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f182])).
fof(f1630, plain, (~ v1_funct_1(u2_lattices(sK1)) | spl30_76), inference(avatar_component_clause, [], [f1628])).
fof(f1628, plain, (spl30_76 <=> v1_funct_1(u2_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_76])])).
fof(f5926, plain, spl30_75, inference(avatar_contradiction_clause, [], [f5925])).
fof(f5925, plain, ($false | spl30_75), inference(subsumption_resolution, [], [f5924, f528])).
fof(f528, plain, l1_lattices(sK1), inference(resolution, [], [f372, f328])).
fof(f372, plain, ! [X0] : (~ l3_lattices(X0) | l1_lattices(X0)), inference(cnf_transformation, [], [f171])).
fof(f5924, plain, (~ l1_lattices(sK1) | spl30_75), inference(resolution, [], [f1626, f868])).
fof(f868, plain, ! [X1] : (m1_relset_1(u1_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l1_lattices(X1)), inference(resolution, [], [f384, f491])).
fof(f384, plain, ! [X0] : (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0] : ((m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0))) | ~ l1_lattices(X0)), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (l1_lattices(X0) => (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_u1_lattices)).
fof(f1626, plain, (~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_75), inference(avatar_component_clause, [], [f1624])).
fof(f1624, plain, (spl30_75 <=> m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_75])])).
fof(f5923, plain, spl30_74, inference(avatar_contradiction_clause, [], [f5922])).
fof(f5922, plain, ($false | spl30_74), inference(subsumption_resolution, [], [f5921, f528])).
fof(f5921, plain, (~ l1_lattices(sK1) | spl30_74), inference(resolution, [], [f1622, f383])).
fof(f383, plain, ! [X0] : (v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f181])).
fof(f1622, plain, (~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl30_74), inference(avatar_component_clause, [], [f1620])).
fof(f1620, plain, (spl30_74 <=> v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_74])])).
fof(f1653, plain, spl30_73, inference(avatar_contradiction_clause, [], [f1652])).
fof(f1652, plain, ($false | spl30_73), inference(subsumption_resolution, [], [f1651, f528])).
fof(f1651, plain, (~ l1_lattices(sK1) | spl30_73), inference(resolution, [], [f1618, f382])).
fof(f382, plain, ! [X0] : (v1_funct_1(u1_lattices(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f181])).
fof(f1618, plain, (~ v1_funct_1(u1_lattices(sK1)) | spl30_73), inference(avatar_component_clause, [], [f1616])).
fof(f1616, plain, (spl30_73 <=> v1_funct_1(u1_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_73])])).
fof(f1650, plain, (~ spl30_73 | ~ spl30_74 | ~ spl30_75 | ~ spl30_76 | ~ spl30_77 | ~ spl30_78 | spl30_81), inference(avatar_split_clause, [], [f1614, f1648, f1636, f1632, f1628, f1624, f1620, f1616])).
fof(f1614, plain, ! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u2_lattices(sK1)) | ~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u1_lattices(sK1))), inference(superposition, [], [f437, f839])).
fof(f839, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(sK1), u1_lattices(sK1), u2_lattices(sK1))), inference(resolution, [], [f352, f328])).
fof(f352, plain, ! [X0] : (~ l3_lattices(X0) | (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), inference(cnf_transformation, [], [f139])).
fof(f139, plain, ! [X0] : ((k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (l3_lattices(X0) => (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', d2_lattice2)).
fof(f437, plain, ! [X4, X2, X0, X5, X3, X1] : (~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) | (X0 = X3) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(cnf_transformation, [], [f214])).
fof(f214, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(flattening, [], [f213])).
fof(f213, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | (~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1))), inference(ennf_transformation, [], [f70])).
fof(f70, plain, ! [X0, X1, X2] : ((m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X2) & m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X1)) => ! [X3, X4, X5] : ((g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) => ((X2 = X5) & (X1 = X4) & (X0 = X3)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', free_g3_lattices)).
fof(f1201, plain, spl30_18, inference(avatar_contradiction_clause, [], [f1200])).
fof(f1200, plain, ($false | spl30_18), inference(subsumption_resolution, [], [f1199, f328])).
fof(f1199, plain, (~ l3_lattices(sK1) | spl30_18), inference(resolution, [], [f1135, f364])).
fof(f1135, plain, (~ l3_lattices(k1_lattice2(sK1)) | spl30_18), inference(avatar_component_clause, [], [f1133])).
fof(f1185, plain, spl30_17, inference(avatar_contradiction_clause, [], [f1184])).
fof(f1184, plain, ($false | spl30_17), inference(subsumption_resolution, [], [f1183, f673])).
fof(f673, plain, sP0(sK1), inference(subsumption_resolution, [], [f672, f326])).
fof(f672, plain, (sP0(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f666, f327])).
fof(f666, plain, (sP0(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f428, f328])).
fof(f1183, plain, (~ sP0(sK1) | spl30_17), inference(resolution, [], [f1131, f427])).
fof(f1131, plain, (~ v10_lattices(k1_lattice2(sK1)) | spl30_17), inference(avatar_component_clause, [], [f1129])).
fof(f1152, plain, (~ spl30_17 | ~ spl30_18 | spl30_21 | spl30_11 | ~ spl30_13), inference(avatar_split_clause, [], [f1147, f985, f977, f1149, f1133, f1129])).
fof(f985, plain, (spl30_13 <=> m1_subset_1(k6_lattices(sK1), u1_struct_0(k1_lattice2(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl30_13])])).
fof(f1147, plain, ((k2_filter_2(k1_lattice2(sK1), k6_lattices(sK1)) = k2_filter_0(k1_lattice2(sK1), k6_lattices(sK1))) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | (spl30_11 | ~ spl30_13)), inference(subsumption_resolution, [], [f1122, f978])).
fof(f1122, plain, ((k2_filter_2(k1_lattice2(sK1), k6_lattices(sK1)) = k2_filter_0(k1_lattice2(sK1), k6_lattices(sK1))) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl30_13), inference(resolution, [], [f987, f488])).
fof(f488, plain, ! [X0, X1] : (~ m1_subset_1(X1, u1_struct_0(X0)) | (k2_filter_0(X0, X1) = k2_filter_2(X0, X1)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f223])).
fof(f223, plain, ! [X0, X1] : ((k2_filter_0(X0, X1) = k2_filter_2(X0, X1)) | ~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ! [X0, X1] : ((k2_filter_0(X0, X1) = k2_filter_2(X0, X1)) | (~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0, X1] : ((m1_subset_1(X1, u1_struct_0(X0)) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k2_filter_0(X0, X1) = k2_filter_2(X0, X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', redefinition_k2_filter_2)).
fof(f987, plain, (m1_subset_1(k6_lattices(sK1), u1_struct_0(k1_lattice2(sK1))) | ~ spl30_13), inference(avatar_component_clause, [], [f985])).
fof(f1012, plain, ~ spl30_11, inference(avatar_contradiction_clause, [], [f1011])).
fof(f1011, plain, ($false | ~ spl30_11), inference(subsumption_resolution, [], [f1007, f673])).
fof(f1007, plain, (~ sP0(sK1) | ~ spl30_11), inference(resolution, [], [f979, f419])).
fof(f419, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f281])).
fof(f979, plain, (v3_struct_0(k1_lattice2(sK1)) | ~ spl30_11), inference(avatar_component_clause, [], [f977])).
fof(f997, plain, spl30_12, inference(avatar_contradiction_clause, [], [f996])).
fof(f996, plain, ($false | spl30_12), inference(subsumption_resolution, [], [f995, f328])).
fof(f995, plain, (~ l3_lattices(sK1) | spl30_12), inference(resolution, [], [f983, f544])).
fof(f544, plain, ! [X1] : (l1_lattices(k1_lattice2(X1)) | ~ l3_lattices(X1)), inference(resolution, [], [f364, f372])).
fof(f983, plain, (~ l1_lattices(k1_lattice2(sK1)) | spl30_12), inference(avatar_component_clause, [], [f981])).
fof(f981, plain, (spl30_12 <=> l1_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl30_12])])).
fof(f988, plain, (spl30_11 | ~ spl30_12 | spl30_13), inference(avatar_split_clause, [], [f975, f985, f981, f977])).
fof(f975, plain, (m1_subset_1(k6_lattices(sK1), u1_struct_0(k1_lattice2(sK1))) | ~ l1_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1))), inference(superposition, [], [f368, f955])).
fof(f368, plain, ! [X0] : (m1_subset_1(k5_lattices(X0), u1_struct_0(X0)) | ~ l1_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : (m1_subset_1(k5_lattices(X0), u1_struct_0(X0)) | ~ l1_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f165])).
fof(f165, plain, ! [X0] : (m1_subset_1(k5_lattices(X0), u1_struct_0(X0)) | (~ l1_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : ((l1_lattices(X0) & ~ v3_struct_0(X0)) => m1_subset_1(k5_lattices(X0), u1_struct_0(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT306+1.p', dt_k5_lattices)).