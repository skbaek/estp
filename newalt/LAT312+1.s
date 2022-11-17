fof(f3772, plain, $false, inference(avatar_sat_refutation, [], [f1015, f1058, f1091, f1097, f1107, f1387, f1666, f1844, f1855, f2076, f2226, f2238, f2242, f2271, f2658, f3771])).
fof(f3771, plain, (spl33_10 | spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85), inference(avatar_contradiction_clause, [], [f3770])).
fof(f3770, plain, ($false | (spl33_10 | spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f3769, f697])).
fof(f697, plain, (~ v1_xboole_0(u1_struct_0(sK1)) | spl33_10), inference(avatar_component_clause, [], [f696])).
fof(f696, plain, (spl33_10 <=> v1_xboole_0(u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl33_10])])).
fof(f3769, plain, (v1_xboole_0(u1_struct_0(sK1)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f3768, f1428])).
fof(f1428, plain, m2_lattice4(k19_filter_2(sK1, sK3), sK1), inference(subsumption_resolution, [], [f1427, f339])).
fof(f339, plain, ~ v3_struct_0(sK1), inference(cnf_transformation, [], [f270])).
fof(f270, plain, (((~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, sK3), k18_filter_2(sK1, sK2)) & r1_filter_2(u1_struct_0(sK1), sK3, k6_domain_1(u1_struct_0(sK1), sK2)) & m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(sK1))) & ~ v1_xboole_0(sK3)) & m1_subset_1(sK2, u1_struct_0(sK1))) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2, sK3])], [f128, f269, f268, f267])).
fof(f267, plain, (? [X0] : (? [X1] : (? [X2] : (~ r1_filter_2(u1_struct_0(X0), k19_filter_2(X0, X2), k18_filter_2(X0, X1)) & r1_filter_2(u1_struct_0(X0), X2, k6_domain_1(u1_struct_0(X0), X1)) & m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X2)) & m1_subset_1(X1, u1_struct_0(X0))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (? [X1] : (? [X2] : (~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, X2), k18_filter_2(sK1, X1)) & r1_filter_2(u1_struct_0(sK1), X2, k6_domain_1(u1_struct_0(sK1), X1)) & m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(sK1))) & ~ v1_xboole_0(X2)) & m1_subset_1(X1, u1_struct_0(sK1))) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1))), introduced(choice_axiom, [])).
fof(f268, plain, (? [X1] : (? [X2] : (~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, X2), k18_filter_2(sK1, X1)) & r1_filter_2(u1_struct_0(sK1), X2, k6_domain_1(u1_struct_0(sK1), X1)) & m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(sK1))) & ~ v1_xboole_0(X2)) & m1_subset_1(X1, u1_struct_0(sK1))) => (? [X2] : (~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, X2), k18_filter_2(sK1, sK2)) & r1_filter_2(u1_struct_0(sK1), X2, k6_domain_1(u1_struct_0(sK1), sK2)) & m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(sK1))) & ~ v1_xboole_0(X2)) & m1_subset_1(sK2, u1_struct_0(sK1)))), introduced(choice_axiom, [])).
fof(f269, plain, (? [X2] : (~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, X2), k18_filter_2(sK1, sK2)) & r1_filter_2(u1_struct_0(sK1), X2, k6_domain_1(u1_struct_0(sK1), sK2)) & m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(sK1))) & ~ v1_xboole_0(X2)) => (~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, sK3), k18_filter_2(sK1, sK2)) & r1_filter_2(u1_struct_0(sK1), sK3, k6_domain_1(u1_struct_0(sK1), sK2)) & m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(sK1))) & ~ v1_xboole_0(sK3))), introduced(choice_axiom, [])).
fof(f128, plain, ? [X0] : (? [X1] : (? [X2] : (~ r1_filter_2(u1_struct_0(X0), k19_filter_2(X0, X2), k18_filter_2(X0, X1)) & r1_filter_2(u1_struct_0(X0), X2, k6_domain_1(u1_struct_0(X0), X1)) & m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X2)) & m1_subset_1(X1, u1_struct_0(X0))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ? [X0] : (? [X1] : (? [X2] : ((~ r1_filter_2(u1_struct_0(X0), k19_filter_2(X0, X2), k18_filter_2(X0, X1)) & r1_filter_2(u1_struct_0(X0), X2, k6_domain_1(u1_struct_0(X0), X1))) & (m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X2))) & m1_subset_1(X1, u1_struct_0(X0))) & (l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => ! [X2] : ((m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X2)) => (r1_filter_2(u1_struct_0(X0), X2, k6_domain_1(u1_struct_0(X0), X1)) => r1_filter_2(u1_struct_0(X0), k19_filter_2(X0, X2), k18_filter_2(X0, X1)))))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => ! [X2] : ((m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X2)) => (r1_filter_2(u1_struct_0(X0), X2, k6_domain_1(u1_struct_0(X0), X1)) => r1_filter_2(u1_struct_0(X0), k19_filter_2(X0, X2), k18_filter_2(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', t41_filter_2)).
fof(f1427, plain, (m2_lattice4(k19_filter_2(sK1, sK3), sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1426, f340])).
fof(f340, plain, v10_lattices(sK1), inference(cnf_transformation, [], [f270])).
fof(f1426, plain, (m2_lattice4(k19_filter_2(sK1, sK3), sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1424, f341])).
fof(f341, plain, l3_lattices(sK1), inference(cnf_transformation, [], [f270])).
fof(f1424, plain, (m2_lattice4(k19_filter_2(sK1, sK3), sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f1124, f392])).
fof(f392, plain, ! [X0, X1] : (~ m2_filter_2(X1, X0) | m2_lattice4(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ! [X0] : (! [X1] : ((m2_lattice4(X1, X0) & ~ v1_xboole_0(X1)) | ~ m2_filter_2(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f182])).
fof(f182, plain, ! [X0] : (! [X1] : ((m2_lattice4(X1, X0) & ~ v1_xboole_0(X1)) | ~ m2_filter_2(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_filter_2(X1, X0) => (m2_lattice4(X1, X0) & ~ v1_xboole_0(X1)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_m2_filter_2)).
fof(f1124, plain, m2_filter_2(k19_filter_2(sK1, sK3), sK1), inference(subsumption_resolution, [], [f1123, f339])).
fof(f1123, plain, (m2_filter_2(k19_filter_2(sK1, sK3), sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1122, f340])).
fof(f1122, plain, (m2_filter_2(k19_filter_2(sK1, sK3), sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1121, f341])).
fof(f1121, plain, (m2_filter_2(k19_filter_2(sK1, sK3), sK1) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1113, f343])).
fof(f343, plain, ~ v1_xboole_0(sK3), inference(cnf_transformation, [], [f270])).
fof(f1113, plain, (m2_filter_2(k19_filter_2(sK1, sK3), sK1) | v1_xboole_0(sK3) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f372, f344])).
fof(f344, plain, m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(sK1))), inference(cnf_transformation, [], [f270])).
fof(f372, plain, ! [X0, X1] : (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | m2_filter_2(k19_filter_2(X0, X1), X0) | v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f157])).
fof(f157, plain, ! [X0, X1] : (m2_filter_2(k19_filter_2(X0, X1), X0) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f156])).
fof(f156, plain, ! [X0, X1] : (m2_filter_2(k19_filter_2(X0, X1), X0) | (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ! [X0, X1] : ((m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X1) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m2_filter_2(k19_filter_2(X0, X1), X0)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_k19_filter_2)).
fof(f3768, plain, (~ m2_lattice4(k19_filter_2(sK1, sK3), sK1) | v1_xboole_0(u1_struct_0(sK1)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f3767, f339])).
fof(f3767, plain, (v3_struct_0(sK1) | ~ m2_lattice4(k19_filter_2(sK1, sK3), sK1) | v1_xboole_0(u1_struct_0(sK1)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f3766, f340])).
fof(f3766, plain, (~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ m2_lattice4(k19_filter_2(sK1, sK3), sK1) | v1_xboole_0(u1_struct_0(sK1)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f3761, f341])).
fof(f3761, plain, (~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ m2_lattice4(k19_filter_2(sK1, sK3), sK1) | v1_xboole_0(u1_struct_0(sK1)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(resolution, [], [f878, f2791])).
fof(f2791, plain, (~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, sK3), k19_filter_2(sK1, sK3)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(backward_demodulation, [], [f346, f2790])).
fof(f2790, plain, ((k18_filter_2(sK1, sK2) = k19_filter_2(sK1, sK3)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_22 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(backward_demodulation, [], [f1305, f2789])).
fof(f2789, plain, ((k19_filter_2(sK1, sK3) = k2_filter_0(k1_lattice2(sK1), sK2)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_26 | ~ spl33_38 | ~ spl33_85)), inference(forward_demodulation, [], [f2788, f2680])).
fof(f2680, plain, ((k19_filter_2(sK1, sK3) = k3_filter_0(k1_lattice2(sK1), sK3)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_26)), inference(backward_demodulation, [], [f1687, f2679])).
fof(f2679, plain, ((k19_filter_2(sK1, sK3) = k3_filter_2(k1_lattice2(sK1), sK3)) | ~ spl33_26), inference(forward_demodulation, [], [f1665, f1682])).
fof(f1682, plain, (sK3 = k8_filter_2(sK1, sK3)), inference(subsumption_resolution, [], [f1681, f339])).
fof(f1681, plain, ((sK3 = k8_filter_2(sK1, sK3)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1680, f340])).
fof(f1680, plain, ((sK3 = k8_filter_2(sK1, sK3)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1643, f341])).
fof(f1643, plain, ((sK3 = k8_filter_2(sK1, sK3)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f1242, f367])).
fof(f367, plain, ! [X0, X1] : (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) | (k8_filter_2(X0, X1) = X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (! [X1] : ((k8_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0))))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : ((k8_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0))))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) => (k8_filter_2(X0, X1) = X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', d7_filter_2)).
fof(f1242, plain, m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(k1_lattice2(sK1)))), inference(subsumption_resolution, [], [f1241, f339])).
fof(f1241, plain, (m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(k1_lattice2(sK1)))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1240, f340])).
fof(f1240, plain, (m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(k1_lattice2(sK1)))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1239, f341])).
fof(f1239, plain, (m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(k1_lattice2(sK1)))) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1224, f344])).
fof(f1224, plain, (m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(k1_lattice2(sK1)))) | ~ m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(sK1))) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(superposition, [], [f381, f935])).
fof(f935, plain, (sK3 = k7_filter_2(sK1, sK3)), inference(subsumption_resolution, [], [f934, f339])).
fof(f934, plain, ((sK3 = k7_filter_2(sK1, sK3)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f933, f340])).
fof(f933, plain, ((sK3 = k7_filter_2(sK1, sK3)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f923, f341])).
fof(f923, plain, ((sK3 = k7_filter_2(sK1, sK3)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f366, f344])).
fof(f366, plain, ! [X0, X1] : (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | (k7_filter_2(X0, X1) = X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (! [X1] : ((k7_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0)))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f146])).
fof(f146, plain, ! [X0] : (! [X1] : ((k7_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0)))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) => (k7_filter_2(X0, X1) = X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', d6_filter_2)).
fof(f381, plain, ! [X0, X1] : (m1_subset_1(k7_filter_2(X0, X1), k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ! [X0, X1] : (m1_subset_1(k7_filter_2(X0, X1), k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f171])).
fof(f171, plain, ! [X0, X1] : (m1_subset_1(k7_filter_2(X0, X1), k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) | (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0, X1] : ((m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m1_subset_1(k7_filter_2(X0, X1), k1_zfmisc_1(u1_struct_0(k1_lattice2(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_k7_filter_2)).
fof(f1665, plain, ((k3_filter_2(k1_lattice2(sK1), sK3) = k19_filter_2(sK1, k8_filter_2(sK1, sK3))) | ~ spl33_26), inference(avatar_component_clause, [], [f1663])).
fof(f1663, plain, (spl33_26 <=> (k3_filter_2(k1_lattice2(sK1), sK3) = k19_filter_2(sK1, k8_filter_2(sK1, sK3)))), introduced(avatar_definition, [new_symbols(naming, [spl33_26])])).
fof(f1687, plain, ((k3_filter_2(k1_lattice2(sK1), sK3) = k3_filter_0(k1_lattice2(sK1), sK3)) | (spl33_13 | ~ spl33_14 | ~ spl33_15)), inference(subsumption_resolution, [], [f1686, f991])).
fof(f991, plain, (~ v3_struct_0(k1_lattice2(sK1)) | spl33_13), inference(avatar_component_clause, [], [f990])).
fof(f990, plain, (spl33_13 <=> v3_struct_0(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl33_13])])).
fof(f1686, plain, ((k3_filter_2(k1_lattice2(sK1), sK3) = k3_filter_0(k1_lattice2(sK1), sK3)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_14 | ~ spl33_15)), inference(subsumption_resolution, [], [f1685, f995])).
fof(f995, plain, (v10_lattices(k1_lattice2(sK1)) | ~ spl33_14), inference(avatar_component_clause, [], [f994])).
fof(f994, plain, (spl33_14 <=> v10_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl33_14])])).
fof(f1685, plain, ((k3_filter_2(k1_lattice2(sK1), sK3) = k3_filter_0(k1_lattice2(sK1), sK3)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl33_15), inference(subsumption_resolution, [], [f1684, f999])).
fof(f999, plain, (l3_lattices(k1_lattice2(sK1)) | ~ spl33_15), inference(avatar_component_clause, [], [f998])).
fof(f998, plain, (spl33_15 <=> l3_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl33_15])])).
fof(f1684, plain, ((k3_filter_2(k1_lattice2(sK1), sK3) = k3_filter_0(k1_lattice2(sK1), sK3)) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1))), inference(subsumption_resolution, [], [f1644, f343])).
fof(f1644, plain, ((k3_filter_2(k1_lattice2(sK1), sK3) = k3_filter_0(k1_lattice2(sK1), sK3)) | v1_xboole_0(sK3) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1))), inference(resolution, [], [f1242, f497])).
fof(f497, plain, ! [X0, X1] : (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | (k3_filter_0(X0, X1) = k3_filter_2(X0, X1)) | v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f233])).
fof(f233, plain, ! [X0, X1] : ((k3_filter_0(X0, X1) = k3_filter_2(X0, X1)) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f232])).
fof(f232, plain, ! [X0, X1] : ((k3_filter_0(X0, X1) = k3_filter_2(X0, X1)) | (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f92])).
fof(f92, plain, ! [X0, X1] : ((m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X1) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k3_filter_0(X0, X1) = k3_filter_2(X0, X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', redefinition_k3_filter_2)).
fof(f2788, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k3_filter_0(k1_lattice2(sK1), sK3)) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f2787, f991])).
fof(f2787, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k3_filter_0(k1_lattice2(sK1), sK3)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_14 | ~ spl33_15 | ~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f2786, f995])).
fof(f2786, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k3_filter_0(k1_lattice2(sK1), sK3)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_15 | ~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f2785, f999])).
fof(f2785, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k3_filter_0(k1_lattice2(sK1), sK3)) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f2784, f983])).
fof(f983, plain, m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))), inference(subsumption_resolution, [], [f982, f339])).
fof(f982, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f981, f340])).
fof(f981, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f980, f341])).
fof(f980, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f973, f342])).
fof(f342, plain, m1_subset_1(sK2, u1_struct_0(sK1)), inference(cnf_transformation, [], [f270])).
fof(f973, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | ~ m1_subset_1(sK2, u1_struct_0(sK1)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(superposition, [], [f379, f896])).
fof(f896, plain, (sK2 = k5_filter_2(sK1, sK2)), inference(subsumption_resolution, [], [f895, f339])).
fof(f895, plain, ((sK2 = k5_filter_2(sK1, sK2)) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f894, f340])).
fof(f894, plain, ((sK2 = k5_filter_2(sK1, sK2)) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f889, f341])).
fof(f889, plain, ((sK2 = k5_filter_2(sK1, sK2)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f365, f342])).
fof(f365, plain, ! [X0, X1] : (~ m1_subset_1(X1, u1_struct_0(X0)) | (k5_filter_2(X0, X1) = X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f145])).
fof(f145, plain, ! [X0] : (! [X1] : ((k5_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f144])).
fof(f144, plain, ! [X0] : (! [X1] : ((k5_filter_2(X0, X1) = X1) | ~ m1_subset_1(X1, u1_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => (k5_filter_2(X0, X1) = X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', d4_filter_2)).
fof(f379, plain, ! [X0, X1] : (m1_subset_1(k5_filter_2(X0, X1), u1_struct_0(k1_lattice2(X0))) | ~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0, X1] : (m1_subset_1(k5_filter_2(X0, X1), u1_struct_0(k1_lattice2(X0))) | ~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f167])).
fof(f167, plain, ! [X0, X1] : (m1_subset_1(k5_filter_2(X0, X1), u1_struct_0(k1_lattice2(X0))) | (~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0, X1] : ((m1_subset_1(X1, u1_struct_0(X0)) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m1_subset_1(k5_filter_2(X0, X1), u1_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_k5_filter_2)).
fof(f2784, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k3_filter_0(k1_lattice2(sK1), sK3)) | ~ m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f2783, f343])).
fof(f2783, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k3_filter_0(k1_lattice2(sK1), sK3)) | v1_xboole_0(sK3) | ~ m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(subsumption_resolution, [], [f2778, f1242])).
fof(f2778, plain, (~ m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(k1_lattice2(sK1)))) | (k2_filter_0(k1_lattice2(sK1), sK2) = k3_filter_0(k1_lattice2(sK1), sK3)) | v1_xboole_0(sK3) | ~ m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(superposition, [], [f531, f2313])).
fof(f2313, plain, ((sK3 = k6_domain_1(u1_struct_0(k1_lattice2(sK1)), sK2)) | (~ spl33_20 | ~ spl33_38 | ~ spl33_85)), inference(backward_demodulation, [], [f1023, f2310])).
fof(f2310, plain, ((sK3 = k1_tarski(sK2)) | (~ spl33_38 | ~ spl33_85)), inference(backward_demodulation, [], [f2237, f1843])).
fof(f1843, plain, ((sK3 = k6_domain_1(u1_struct_0(k1_lattice2(k1_lattice2(sK1))), sK2)) | ~ spl33_38), inference(avatar_component_clause, [], [f1841])).
fof(f1841, plain, (spl33_38 <=> (sK3 = k6_domain_1(u1_struct_0(k1_lattice2(k1_lattice2(sK1))), sK2))), introduced(avatar_definition, [new_symbols(naming, [spl33_38])])).
fof(f2237, plain, ((k1_tarski(sK2) = k6_domain_1(u1_struct_0(k1_lattice2(k1_lattice2(sK1))), sK2)) | ~ spl33_85), inference(avatar_component_clause, [], [f2235])).
fof(f2235, plain, (spl33_85 <=> (k1_tarski(sK2) = k6_domain_1(u1_struct_0(k1_lattice2(k1_lattice2(sK1))), sK2))), introduced(avatar_definition, [new_symbols(naming, [spl33_85])])).
fof(f1023, plain, ((k1_tarski(sK2) = k6_domain_1(u1_struct_0(k1_lattice2(sK1)), sK2)) | ~ spl33_20), inference(avatar_component_clause, [], [f1021])).
fof(f1021, plain, (spl33_20 <=> (k1_tarski(sK2) = k6_domain_1(u1_struct_0(k1_lattice2(sK1)), sK2))), introduced(avatar_definition, [new_symbols(naming, [spl33_20])])).
fof(f531, plain, ! [X0, X1] : (~ m1_subset_1(k6_domain_1(u1_struct_0(X0), X1), k1_zfmisc_1(u1_struct_0(X0))) | (k2_filter_0(X0, X1) = k3_filter_0(X0, k6_domain_1(u1_struct_0(X0), X1))) | v1_xboole_0(k6_domain_1(u1_struct_0(X0), X1)) | ~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(equality_resolution, [], [f515])).
fof(f515, plain, ! [X2, X0, X1] : ((k2_filter_0(X0, X1) = k3_filter_0(X0, X2)) | ~ (k6_domain_1(u1_struct_0(X0), X1) = X2) | ~ m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X2) | ~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f253])).
fof(f253, plain, ! [X0] : (! [X1] : (! [X2] : ((k2_filter_0(X0, X1) = k3_filter_0(X0, X2)) | ~ (k6_domain_1(u1_struct_0(X0), X1) = X2) | ~ m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X2)) | ~ m1_subset_1(X1, u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f252])).
fof(f252, plain, ! [X0] : (! [X1] : (! [X2] : (((k2_filter_0(X0, X1) = k3_filter_0(X0, X2)) | ~ (k6_domain_1(u1_struct_0(X0), X1) = X2)) | (~ m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X2))) | ~ m1_subset_1(X1, u1_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f105])).
fof(f105, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => ! [X2] : ((m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X2)) => ((k6_domain_1(u1_struct_0(X0), X1) = X2) => (k2_filter_0(X0, X1) = k3_filter_0(X0, X2)))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', t30_filter_0)).
fof(f1305, plain, ((k18_filter_2(sK1, sK2) = k2_filter_0(k1_lattice2(sK1), sK2)) | ~ spl33_22), inference(backward_demodulation, [], [f1096, f1304])).
fof(f1304, plain, (k18_filter_2(sK1, sK2) = k2_filter_2(k1_lattice2(sK1), sK2)), inference(forward_demodulation, [], [f1303, f896])).
fof(f1303, plain, (k18_filter_2(sK1, sK2) = k2_filter_2(k1_lattice2(sK1), k5_filter_2(sK1, sK2))), inference(subsumption_resolution, [], [f1302, f339])).
fof(f1302, plain, ((k18_filter_2(sK1, sK2) = k2_filter_2(k1_lattice2(sK1), k5_filter_2(sK1, sK2))) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1301, f340])).
fof(f1301, plain, ((k18_filter_2(sK1, sK2) = k2_filter_2(k1_lattice2(sK1), k5_filter_2(sK1, sK2))) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1293, f341])).
fof(f1293, plain, ((k18_filter_2(sK1, sK2) = k2_filter_2(k1_lattice2(sK1), k5_filter_2(sK1, sK2))) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f516, f342])).
fof(f516, plain, ! [X0, X1] : (~ m1_subset_1(X1, u1_struct_0(X0)) | (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f255])).
fof(f255, plain, ! [X0] : (! [X1] : (((k2_filter_2(X0, X1) = k18_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) & (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1)))) | ~ m1_subset_1(X1, u1_struct_0(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f254])).
fof(f254, plain, ! [X0] : (! [X1] : (((k2_filter_2(X0, X1) = k18_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) & (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1)))) | ~ m1_subset_1(X1, u1_struct_0(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f106])).
fof(f106, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_subset_1(X1, u1_struct_0(X0)) => ((k2_filter_2(X0, X1) = k18_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1))) & (k18_filter_2(X0, X1) = k2_filter_2(k1_lattice2(X0), k5_filter_2(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', t30_filter_2)).
fof(f1096, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k2_filter_2(k1_lattice2(sK1), sK2)) | ~ spl33_22), inference(avatar_component_clause, [], [f1094])).
fof(f1094, plain, (spl33_22 <=> (k2_filter_0(k1_lattice2(sK1), sK2) = k2_filter_2(k1_lattice2(sK1), sK2))), introduced(avatar_definition, [new_symbols(naming, [spl33_22])])).
fof(f346, plain, ~ r1_filter_2(u1_struct_0(sK1), k19_filter_2(sK1, sK3), k18_filter_2(sK1, sK2)), inference(cnf_transformation, [], [f270])).
fof(f878, plain, ! [X0, X1] : (r1_filter_2(u1_struct_0(X1), X0, X0) | ~ l3_lattices(X1) | ~ v10_lattices(X1) | v3_struct_0(X1) | ~ m2_lattice4(X0, X1) | v1_xboole_0(u1_struct_0(X1))), inference(resolution, [], [f393, f532])).
fof(f532, plain, ! [X2, X0] : (~ m1_subset_1(X2, k1_zfmisc_1(X0)) | r1_filter_2(X0, X2, X2) | v1_xboole_0(X0)), inference(duplicate_literal_removal, [], [f530])).
fof(f530, plain, ! [X2, X0] : (r1_filter_2(X0, X2, X2) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(equality_resolution, [], [f504])).
fof(f504, plain, ! [X2, X0, X1] : (r1_filter_2(X0, X1, X2) | ~ (X1 = X2) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(cnf_transformation, [], [f334])).
fof(f334, plain, ! [X0, X1, X2] : (((r1_filter_2(X0, X1, X2) | ~ (X1 = X2)) & ((X1 = X2) | ~ r1_filter_2(X0, X1, X2))) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(nnf_transformation, [], [f239])).
fof(f239, plain, ! [X0, X1, X2] : ((r1_filter_2(X0, X1, X2) <=> (X1 = X2)) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(flattening, [], [f238])).
fof(f238, plain, ! [X0, X1, X2] : ((r1_filter_2(X0, X1, X2) <=> (X1 = X2)) | (~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0))), inference(ennf_transformation, [], [f96])).
fof(f96, plain, ! [X0, X1, X2] : ((m1_subset_1(X2, k1_zfmisc_1(X0)) & m1_subset_1(X1, k1_zfmisc_1(X0)) & ~ v1_xboole_0(X0)) => (r1_filter_2(X0, X1, X2) <=> (X1 = X2))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', redefinition_r1_filter_2)).
fof(f393, plain, ! [X0, X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ m2_lattice4(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ! [X0] : (! [X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ m2_lattice4(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f184])).
fof(f184, plain, ! [X0] : (! [X1] : (m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | ~ m2_lattice4(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f42])).
fof(f42, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_lattice4(X1, X0) => m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_m2_lattice4)).
fof(f2658, plain, ~ spl33_25, inference(avatar_contradiction_clause, [], [f2657])).
fof(f2657, plain, ($false | ~ spl33_25), inference(subsumption_resolution, [], [f2642, f343])).
fof(f2642, plain, (v1_xboole_0(sK3) | ~ spl33_25), inference(resolution, [], [f1661, f344])).
fof(f1661, plain, (! [X0] : (~ m1_subset_1(X0, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(X0)) | ~ spl33_25), inference(avatar_component_clause, [], [f1660])).
fof(f1660, plain, (spl33_25 <=> ! [X0] : (~ m1_subset_1(X0, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(X0))), introduced(avatar_definition, [new_symbols(naming, [spl33_25])])).
fof(f2271, plain, ~ spl33_10, inference(avatar_contradiction_clause, [], [f2270])).
fof(f2270, plain, ($false | ~ spl33_10), inference(subsumption_resolution, [], [f2269, f339])).
fof(f2269, plain, (v3_struct_0(sK1) | ~ spl33_10), inference(subsumption_resolution, [], [f2264, f548])).
fof(f548, plain, l1_struct_0(sK1), inference(resolution, [], [f543, f383])).
fof(f383, plain, ! [X0] : (~ l1_lattices(X0) | l1_struct_0(X0)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (l1_struct_0(X0) | ~ l1_lattices(X0)), inference(ennf_transformation, [], [f33])).
fof(f33, plain, ! [X0] : (l1_lattices(X0) => l1_struct_0(X0)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_l1_lattices)).
fof(f543, plain, l1_lattices(sK1), inference(resolution, [], [f385, f341])).
fof(f385, plain, ! [X0] : (~ l3_lattices(X0) | l1_lattices(X0)), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((l2_lattices(X0) & l1_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (l3_lattices(X0) => (l2_lattices(X0) & l1_lattices(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_l3_lattices)).
fof(f2264, plain, (~ l1_struct_0(sK1) | v3_struct_0(sK1) | ~ spl33_10), inference(resolution, [], [f698, f415])).
fof(f415, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | ~ l1_struct_0(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | ~ l1_struct_0(X0) | v3_struct_0(X0)), inference(flattening, [], [f201])).
fof(f201, plain, ! [X0] : (~ v1_xboole_0(u1_struct_0(X0)) | (~ l1_struct_0(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ! [X0] : ((l1_struct_0(X0) & ~ v3_struct_0(X0)) => ~ v1_xboole_0(u1_struct_0(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', fc1_struct_0)).
fof(f698, plain, (v1_xboole_0(u1_struct_0(sK1)) | ~ spl33_10), inference(avatar_component_clause, [], [f696])).
fof(f2242, plain, (spl33_20 | spl33_19), inference(avatar_split_clause, [], [f1532, f1017, f1021])).
fof(f1017, plain, (spl33_19 <=> v1_xboole_0(u1_struct_0(k1_lattice2(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl33_19])])).
fof(f1532, plain, ((k1_tarski(sK2) = k6_domain_1(u1_struct_0(k1_lattice2(sK1)), sK2)) | spl33_19), inference(subsumption_resolution, [], [f1508, f1018])).
fof(f1018, plain, (~ v1_xboole_0(u1_struct_0(k1_lattice2(sK1))) | spl33_19), inference(avatar_component_clause, [], [f1017])).
fof(f1508, plain, ((k1_tarski(sK2) = k6_domain_1(u1_struct_0(k1_lattice2(sK1)), sK2)) | v1_xboole_0(u1_struct_0(k1_lattice2(sK1)))), inference(resolution, [], [f983, f498])).
fof(f498, plain, ! [X0, X1] : (~ m1_subset_1(X1, X0) | (k6_domain_1(X0, X1) = k1_tarski(X1)) | v1_xboole_0(X0)), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ! [X0, X1] : ((k6_domain_1(X0, X1) = k1_tarski(X1)) | ~ m1_subset_1(X1, X0) | v1_xboole_0(X0)), inference(flattening, [], [f234])).
fof(f234, plain, ! [X0, X1] : ((k6_domain_1(X0, X1) = k1_tarski(X1)) | (~ m1_subset_1(X1, X0) | v1_xboole_0(X0))), inference(ennf_transformation, [], [f93])).
fof(f93, plain, ! [X0, X1] : ((m1_subset_1(X1, X0) & ~ v1_xboole_0(X0)) => (k6_domain_1(X0, X1) = k1_tarski(X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', redefinition_k6_domain_1)).
fof(f2238, plain, (spl33_37 | spl33_85 | spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_18), inference(avatar_split_clause, [], [f1787, f1012, f998, f994, f990, f2235, f1837])).
fof(f1837, plain, (spl33_37 <=> v1_xboole_0(u1_struct_0(k1_lattice2(k1_lattice2(sK1))))), introduced(avatar_definition, [new_symbols(naming, [spl33_37])])).
fof(f1012, plain, (spl33_18 <=> (sK2 = k5_filter_2(k1_lattice2(sK1), sK2))), introduced(avatar_definition, [new_symbols(naming, [spl33_18])])).
fof(f1787, plain, ((k1_tarski(sK2) = k6_domain_1(u1_struct_0(k1_lattice2(k1_lattice2(sK1))), sK2)) | v1_xboole_0(u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_18)), inference(resolution, [], [f1718, f498])).
fof(f1718, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | (spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_18)), inference(subsumption_resolution, [], [f1717, f991])).
fof(f1717, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_14 | ~ spl33_15 | ~ spl33_18)), inference(subsumption_resolution, [], [f1716, f995])).
fof(f1716, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | (~ spl33_15 | ~ spl33_18)), inference(subsumption_resolution, [], [f1715, f999])).
fof(f1715, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl33_18), inference(subsumption_resolution, [], [f1714, f983])).
fof(f1714, plain, (m1_subset_1(sK2, u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | ~ m1_subset_1(sK2, u1_struct_0(k1_lattice2(sK1))) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl33_18), inference(superposition, [], [f379, f1014])).
fof(f1014, plain, ((sK2 = k5_filter_2(k1_lattice2(sK1), sK2)) | ~ spl33_18), inference(avatar_component_clause, [], [f1012])).
fof(f2226, plain, (spl33_28 | ~ spl33_30 | ~ spl33_37), inference(avatar_contradiction_clause, [], [f2225])).
fof(f2225, plain, ($false | (spl33_28 | ~ spl33_30 | ~ spl33_37)), inference(subsumption_resolution, [], [f2224, f1791])).
fof(f1791, plain, (~ v3_struct_0(k1_lattice2(k1_lattice2(sK1))) | spl33_28), inference(avatar_component_clause, [], [f1790])).
fof(f1790, plain, (spl33_28 <=> v3_struct_0(k1_lattice2(k1_lattice2(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl33_28])])).
fof(f2224, plain, (v3_struct_0(k1_lattice2(k1_lattice2(sK1))) | (~ spl33_30 | ~ spl33_37)), inference(subsumption_resolution, [], [f2219, f2169])).
fof(f2169, plain, (l1_struct_0(k1_lattice2(k1_lattice2(sK1))) | ~ spl33_30), inference(resolution, [], [f2152, f383])).
fof(f2152, plain, (l1_lattices(k1_lattice2(k1_lattice2(sK1))) | ~ spl33_30), inference(resolution, [], [f1799, f385])).
fof(f1799, plain, (l3_lattices(k1_lattice2(k1_lattice2(sK1))) | ~ spl33_30), inference(avatar_component_clause, [], [f1798])).
fof(f1798, plain, (spl33_30 <=> l3_lattices(k1_lattice2(k1_lattice2(sK1)))), introduced(avatar_definition, [new_symbols(naming, [spl33_30])])).
fof(f2219, plain, (~ l1_struct_0(k1_lattice2(k1_lattice2(sK1))) | v3_struct_0(k1_lattice2(k1_lattice2(sK1))) | ~ spl33_37), inference(resolution, [], [f1839, f415])).
fof(f1839, plain, (v1_xboole_0(u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | ~ spl33_37), inference(avatar_component_clause, [], [f1837])).
fof(f2076, plain, (spl33_13 | ~ spl33_15 | ~ spl33_28), inference(avatar_contradiction_clause, [], [f2075])).
fof(f2075, plain, ($false | (spl33_13 | ~ spl33_15 | ~ spl33_28)), inference(subsumption_resolution, [], [f2074, f991])).
fof(f2074, plain, (v3_struct_0(k1_lattice2(sK1)) | (~ spl33_15 | ~ spl33_28)), inference(subsumption_resolution, [], [f2072, f999])).
fof(f2072, plain, (~ l3_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl33_28), inference(resolution, [], [f1792, f413])).
fof(f413, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f199])).
fof(f199, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f59])).
fof(f59, plain, ! [X0] : ((l3_lattices(X0) & ~ v3_struct_0(X0)) => (v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', fc1_lattice2)).
fof(f1792, plain, (v3_struct_0(k1_lattice2(k1_lattice2(sK1))) | ~ spl33_28), inference(avatar_component_clause, [], [f1790])).
fof(f1855, plain, (~ spl33_15 | spl33_30), inference(avatar_contradiction_clause, [], [f1854])).
fof(f1854, plain, ($false | (~ spl33_15 | spl33_30)), inference(subsumption_resolution, [], [f1853, f999])).
fof(f1853, plain, (~ l3_lattices(k1_lattice2(sK1)) | spl33_30), inference(resolution, [], [f1800, f374])).
fof(f374, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : ((l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (l3_lattices(X0) => (l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_k1_lattice2)).
fof(f1800, plain, (~ l3_lattices(k1_lattice2(k1_lattice2(sK1))) | spl33_30), inference(avatar_component_clause, [], [f1798])).
fof(f1844, plain, (spl33_37 | spl33_38 | spl33_10 | spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_18), inference(avatar_split_clause, [], [f1835, f1012, f998, f994, f990, f696, f1841, f1837])).
fof(f1835, plain, ((sK3 = k6_domain_1(u1_struct_0(k1_lattice2(k1_lattice2(sK1))), sK2)) | v1_xboole_0(u1_struct_0(k1_lattice2(k1_lattice2(sK1)))) | (spl33_10 | spl33_13 | ~ spl33_14 | ~ spl33_15 | ~ spl33_18)), inference(forward_demodulation, [], [f1787, f1079])).
fof(f1079, plain, ((sK3 = k1_tarski(sK2)) | spl33_10), inference(subsumption_resolution, [], [f1078, f697])).
fof(f1078, plain, ((sK3 = k1_tarski(sK2)) | v1_xboole_0(u1_struct_0(sK1)) | spl33_10), inference(subsumption_resolution, [], [f1077, f344])).
fof(f1077, plain, ((sK3 = k1_tarski(sK2)) | ~ m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(u1_struct_0(sK1)) | spl33_10), inference(subsumption_resolution, [], [f1073, f794])).
fof(f794, plain, (m1_subset_1(k1_tarski(sK2), k1_zfmisc_1(u1_struct_0(sK1))) | spl33_10), inference(subsumption_resolution, [], [f793, f697])).
fof(f793, plain, (m1_subset_1(k1_tarski(sK2), k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(u1_struct_0(sK1)) | spl33_10), inference(subsumption_resolution, [], [f792, f342])).
fof(f792, plain, (m1_subset_1(k1_tarski(sK2), k1_zfmisc_1(u1_struct_0(sK1))) | ~ m1_subset_1(sK2, u1_struct_0(sK1)) | v1_xboole_0(u1_struct_0(sK1)) | spl33_10), inference(superposition, [], [f380, f787])).
fof(f787, plain, ((k6_domain_1(u1_struct_0(sK1), sK2) = k1_tarski(sK2)) | spl33_10), inference(subsumption_resolution, [], [f777, f697])).
fof(f777, plain, ((k6_domain_1(u1_struct_0(sK1), sK2) = k1_tarski(sK2)) | v1_xboole_0(u1_struct_0(sK1))), inference(resolution, [], [f498, f342])).
fof(f380, plain, ! [X0, X1] : (m1_subset_1(k6_domain_1(X0, X1), k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, X0) | v1_xboole_0(X0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0, X1] : (m1_subset_1(k6_domain_1(X0, X1), k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, X0) | v1_xboole_0(X0)), inference(flattening, [], [f169])).
fof(f169, plain, ! [X0, X1] : (m1_subset_1(k6_domain_1(X0, X1), k1_zfmisc_1(X0)) | (~ m1_subset_1(X1, X0) | v1_xboole_0(X0))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((m1_subset_1(X1, X0) & ~ v1_xboole_0(X0)) => m1_subset_1(k6_domain_1(X0, X1), k1_zfmisc_1(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', dt_k6_domain_1)).
fof(f1073, plain, ((sK3 = k1_tarski(sK2)) | ~ m1_subset_1(k1_tarski(sK2), k1_zfmisc_1(u1_struct_0(sK1))) | ~ m1_subset_1(sK3, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(u1_struct_0(sK1)) | spl33_10), inference(resolution, [], [f503, f788])).
fof(f788, plain, (r1_filter_2(u1_struct_0(sK1), sK3, k1_tarski(sK2)) | spl33_10), inference(backward_demodulation, [], [f345, f787])).
fof(f345, plain, r1_filter_2(u1_struct_0(sK1), sK3, k6_domain_1(u1_struct_0(sK1), sK2)), inference(cnf_transformation, [], [f270])).
fof(f503, plain, ! [X2, X0, X1] : (~ r1_filter_2(X0, X1, X2) | (X1 = X2) | ~ m1_subset_1(X2, k1_zfmisc_1(X0)) | ~ m1_subset_1(X1, k1_zfmisc_1(X0)) | v1_xboole_0(X0)), inference(cnf_transformation, [], [f334])).
fof(f1666, plain, (spl33_25 | spl33_26), inference(avatar_split_clause, [], [f1658, f1663, f1660])).
fof(f1658, plain, ! [X0] : ((k3_filter_2(k1_lattice2(sK1), sK3) = k19_filter_2(sK1, k8_filter_2(sK1, sK3))) | ~ m1_subset_1(X0, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(X0)), inference(subsumption_resolution, [], [f1657, f339])).
fof(f1657, plain, ! [X0] : ((k3_filter_2(k1_lattice2(sK1), sK3) = k19_filter_2(sK1, k8_filter_2(sK1, sK3))) | ~ m1_subset_1(X0, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(X0) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1656, f340])).
fof(f1656, plain, ! [X0] : ((k3_filter_2(k1_lattice2(sK1), sK3) = k19_filter_2(sK1, k8_filter_2(sK1, sK3))) | ~ m1_subset_1(X0, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(X0) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1655, f341])).
fof(f1655, plain, ! [X0] : ((k3_filter_2(k1_lattice2(sK1), sK3) = k19_filter_2(sK1, k8_filter_2(sK1, sK3))) | ~ m1_subset_1(X0, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(X0) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f1638, f343])).
fof(f1638, plain, ! [X0] : ((k3_filter_2(k1_lattice2(sK1), sK3) = k19_filter_2(sK1, k8_filter_2(sK1, sK3))) | v1_xboole_0(sK3) | ~ m1_subset_1(X0, k1_zfmisc_1(u1_struct_0(sK1))) | v1_xboole_0(X0) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f1242, f521])).
fof(f521, plain, ! [X2, X0, X1] : (~ m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) | (k3_filter_2(k1_lattice2(X0), X2) = k19_filter_2(X0, k8_filter_2(X0, X2))) | v1_xboole_0(X2) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X1) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f257])).
fof(f257, plain, ! [X0] : (! [X1] : (! [X2] : (((k3_filter_2(k1_lattice2(X0), X2) = k19_filter_2(X0, k8_filter_2(X0, X2))) & (k3_filter_2(X0, k8_filter_2(X0, X2)) = k19_filter_2(k1_lattice2(X0), X2)) & (k3_filter_2(X0, X1) = k19_filter_2(k1_lattice2(X0), k7_filter_2(X0, X1))) & (k19_filter_2(X0, X1) = k3_filter_2(k1_lattice2(X0), k7_filter_2(X0, X1)))) | ~ m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) | v1_xboole_0(X2)) | ~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X1)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f256])).
fof(f256, plain, ! [X0] : (! [X1] : (! [X2] : (((k3_filter_2(k1_lattice2(X0), X2) = k19_filter_2(X0, k8_filter_2(X0, X2))) & (k3_filter_2(X0, k8_filter_2(X0, X2)) = k19_filter_2(k1_lattice2(X0), X2)) & (k3_filter_2(X0, X1) = k19_filter_2(k1_lattice2(X0), k7_filter_2(X0, X1))) & (k19_filter_2(X0, X1) = k3_filter_2(k1_lattice2(X0), k7_filter_2(X0, X1)))) | (~ m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) | v1_xboole_0(X2))) | (~ m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) | v1_xboole_0(X1))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f107])).
fof(f107, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : ((m1_subset_1(X1, k1_zfmisc_1(u1_struct_0(X0))) & ~ v1_xboole_0(X1)) => ! [X2] : ((m1_subset_1(X2, k1_zfmisc_1(u1_struct_0(k1_lattice2(X0)))) & ~ v1_xboole_0(X2)) => ((k3_filter_2(k1_lattice2(X0), X2) = k19_filter_2(X0, k8_filter_2(X0, X2))) & (k3_filter_2(X0, k8_filter_2(X0, X2)) = k19_filter_2(k1_lattice2(X0), X2)) & (k3_filter_2(X0, X1) = k19_filter_2(k1_lattice2(X0), k7_filter_2(X0, X1))) & (k19_filter_2(X0, X1) = k3_filter_2(k1_lattice2(X0), k7_filter_2(X0, X1))))))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', t37_filter_2)).
fof(f1387, plain, (spl33_13 | ~ spl33_15 | ~ spl33_19), inference(avatar_contradiction_clause, [], [f1386])).
fof(f1386, plain, ($false | (spl33_13 | ~ spl33_15 | ~ spl33_19)), inference(subsumption_resolution, [], [f1385, f991])).
fof(f1385, plain, (v3_struct_0(k1_lattice2(sK1)) | (~ spl33_15 | ~ spl33_19)), inference(subsumption_resolution, [], [f1380, f1189])).
fof(f1189, plain, (l1_struct_0(k1_lattice2(sK1)) | ~ spl33_15), inference(resolution, [], [f1153, f383])).
fof(f1153, plain, (l1_lattices(k1_lattice2(sK1)) | ~ spl33_15), inference(resolution, [], [f999, f385])).
fof(f1380, plain, (~ l1_struct_0(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl33_19), inference(resolution, [], [f1019, f415])).
fof(f1019, plain, (v1_xboole_0(u1_struct_0(k1_lattice2(sK1))) | ~ spl33_19), inference(avatar_component_clause, [], [f1017])).
fof(f1107, plain, ~ spl33_13, inference(avatar_contradiction_clause, [], [f1106])).
fof(f1106, plain, ($false | ~ spl33_13), inference(subsumption_resolution, [], [f1102, f678])).
fof(f678, plain, sP0(sK1), inference(subsumption_resolution, [], [f677, f339])).
fof(f677, plain, (sP0(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f672, f340])).
fof(f672, plain, (sP0(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f446, f341])).
fof(f446, plain, ! [X0] : (~ l3_lattices(X0) | sP0(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f266])).
fof(f266, plain, ! [X0] : (sP0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f218, e265])).
fof(f265, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(usedef, [], [e265])).
fof(e265, plain, ! [X0] : (sP0(X0) <=> (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f218, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f217])).
fof(f217, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f72])).
fof(f72, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', fc6_lattice2)).
fof(f1102, plain, (~ sP0(sK1) | ~ spl33_13), inference(resolution, [], [f992, f437])).
fof(f437, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f293])).
fof(f293, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(nnf_transformation, [], [f265])).
fof(f992, plain, (v3_struct_0(k1_lattice2(sK1)) | ~ spl33_13), inference(avatar_component_clause, [], [f990])).
fof(f1097, plain, (spl33_13 | ~ spl33_15 | spl33_22 | ~ spl33_14), inference(avatar_split_clause, [], [f1092, f994, f1094, f998, f990])).
fof(f1092, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k2_filter_2(k1_lattice2(sK1), sK2)) | ~ l3_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1)) | ~ spl33_14), inference(subsumption_resolution, [], [f1062, f995])).
fof(f1062, plain, ((k2_filter_0(k1_lattice2(sK1), sK2) = k2_filter_2(k1_lattice2(sK1), sK2)) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1))), inference(resolution, [], [f496, f983])).
fof(f496, plain, ! [X0, X1] : (~ m1_subset_1(X1, u1_struct_0(X0)) | (k2_filter_0(X0, X1) = k2_filter_2(X0, X1)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ! [X0, X1] : ((k2_filter_0(X0, X1) = k2_filter_2(X0, X1)) | ~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f230])).
fof(f230, plain, ! [X0, X1] : ((k2_filter_0(X0, X1) = k2_filter_2(X0, X1)) | (~ m1_subset_1(X1, u1_struct_0(X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f91])).
fof(f91, plain, ! [X0, X1] : ((m1_subset_1(X1, u1_struct_0(X0)) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (k2_filter_0(X0, X1) = k2_filter_2(X0, X1))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT312+1.p', redefinition_k2_filter_2)).
fof(f1091, plain, spl33_15, inference(avatar_contradiction_clause, [], [f1090])).
fof(f1090, plain, ($false | spl33_15), inference(subsumption_resolution, [], [f1089, f341])).
fof(f1089, plain, (~ l3_lattices(sK1) | spl33_15), inference(resolution, [], [f1000, f374])).
fof(f1000, plain, (~ l3_lattices(k1_lattice2(sK1)) | spl33_15), inference(avatar_component_clause, [], [f998])).
fof(f1058, plain, spl33_14, inference(avatar_contradiction_clause, [], [f1057])).
fof(f1057, plain, ($false | spl33_14), inference(subsumption_resolution, [], [f1056, f678])).
fof(f1056, plain, (~ sP0(sK1) | spl33_14), inference(resolution, [], [f996, f445])).
fof(f445, plain, ! [X0] : (v10_lattices(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f293])).
fof(f996, plain, (~ v10_lattices(k1_lattice2(sK1)) | spl33_14), inference(avatar_component_clause, [], [f994])).
fof(f1015, plain, (spl33_13 | ~ spl33_14 | ~ spl33_15 | spl33_18), inference(avatar_split_clause, [], [f986, f1012, f998, f994, f990])).
fof(f986, plain, ((sK2 = k5_filter_2(k1_lattice2(sK1), sK2)) | ~ l3_lattices(k1_lattice2(sK1)) | ~ v10_lattices(k1_lattice2(sK1)) | v3_struct_0(k1_lattice2(sK1))), inference(resolution, [], [f983, f365])).