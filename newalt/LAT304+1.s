fof(f3574, plain, $false, inference(avatar_sat_refutation, [], [f888, f1214, f1217, f1220, f1290, f1295, f1308, f2930, f3573])).
fof(f3573, plain, (~ spl26_48 | ~ spl26_202), inference(avatar_contradiction_clause, [], [f3572])).
fof(f3572, plain, ($false | (~ spl26_48 | ~ spl26_202)), inference(subsumption_resolution, [], [f3571, f227])).
fof(f227, plain, ~ v3_struct_0(sK1), inference(cnf_transformation, [], [f174])).
fof(f174, plain, (~ m2_filter_2(u1_struct_0(sK1), sK1) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f92, f173])).
fof(f173, plain, (? [X0] : (~ m2_filter_2(u1_struct_0(X0), X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (~ m2_filter_2(u1_struct_0(sK1), sK1) & l3_lattices(sK1) & v10_lattices(sK1) & ~ v3_struct_0(sK1))), introduced(choice_axiom, [])).
fof(f92, plain, ? [X0] : (~ m2_filter_2(u1_struct_0(X0), X0) & l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)), inference(flattening, [], [f91])).
fof(f91, plain, ? [X0] : (~ m2_filter_2(u1_struct_0(X0), X0) & (l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m2_filter_2(u1_struct_0(X0), X0)), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m2_filter_2(u1_struct_0(X0), X0)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', t28_filter_2)).
fof(f3571, plain, (v3_struct_0(sK1) | (~ spl26_48 | ~ spl26_202)), inference(subsumption_resolution, [], [f3570, f228])).
fof(f228, plain, v10_lattices(sK1), inference(cnf_transformation, [], [f174])).
fof(f3570, plain, (~ v10_lattices(sK1) | v3_struct_0(sK1) | (~ spl26_48 | ~ spl26_202)), inference(subsumption_resolution, [], [f3569, f229])).
fof(f229, plain, l3_lattices(sK1), inference(cnf_transformation, [], [f174])).
fof(f3569, plain, (~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | (~ spl26_48 | ~ spl26_202)), inference(subsumption_resolution, [], [f3568, f2850])).
fof(f2850, plain, (v10_lattices(k1_lattice2(sK1)) | ~ spl26_202), inference(avatar_component_clause, [], [f2849])).
fof(f2849, plain, (spl26_202 <=> v10_lattices(k1_lattice2(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl26_202])])).
fof(f3568, plain, (~ v10_lattices(k1_lattice2(sK1)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ spl26_48), inference(subsumption_resolution, [], [f3565, f230])).
fof(f230, plain, ~ m2_filter_2(u1_struct_0(sK1), sK1), inference(cnf_transformation, [], [f174])).
fof(f3565, plain, (m2_filter_2(u1_struct_0(sK1), sK1) | ~ v10_lattices(k1_lattice2(sK1)) | ~ l3_lattices(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1) | ~ spl26_48), inference(superposition, [], [f996, f2168])).
fof(f2168, plain, ((u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl26_48), inference(trivial_inequality_removal, [], [f2154])).
fof(f2154, plain, (~ (k1_lattice2(sK1) = k1_lattice2(sK1)) | (u1_struct_0(sK1) = u1_struct_0(k1_lattice2(sK1))) | ~ spl26_48), inference(superposition, [], [f884, f1144])).
fof(f1144, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(k1_lattice2(sK1)), u2_lattices(k1_lattice2(sK1)), u1_lattices(k1_lattice2(sK1)))), inference(resolution, [], [f605, f229])).
fof(f605, plain, ! [X1] : (~ l3_lattices(X1) | (k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1))))), inference(subsumption_resolution, [], [f601, f250])).
fof(f250, plain, ! [X0] : (l3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ! [X0] : ((l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (l3_lattices(X0) => (l3_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', dt_k1_lattice2)).
fof(f601, plain, ! [X1] : ((k1_lattice2(X1) = g3_lattices(u1_struct_0(k1_lattice2(X1)), u2_lattices(k1_lattice2(X1)), u1_lattices(k1_lattice2(X1)))) | ~ l3_lattices(k1_lattice2(X1)) | ~ l3_lattices(X1)), inference(resolution, [], [f231, f249])).
fof(f249, plain, ! [X0] : (v3_lattices(k1_lattice2(X0)) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f107])).
fof(f231, plain, ! [X0] : (~ v3_lattices(X0) | (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ l3_lattices(X0)), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ! [X0] : ((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0) | ~ l3_lattices(X0)), inference(flattening, [], [f93])).
fof(f93, plain, ! [X0] : (((g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0) | ~ v3_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (l3_lattices(X0) => (v3_lattices(X0) => (g3_lattices(u1_struct_0(X0), u2_lattices(X0), u1_lattices(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', abstractness_v3_lattices)).
fof(f884, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15)) | ~ spl26_48), inference(avatar_component_clause, [], [f883])).
fof(f883, plain, (spl26_48 <=> ! [X16, X15, X17] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15))), introduced(avatar_definition, [new_symbols(naming, [spl26_48])])).
fof(f996, plain, ! [X3] : (m2_filter_2(u1_struct_0(k1_lattice2(X3)), X3) | ~ v10_lattices(k1_lattice2(X3)) | ~ l3_lattices(X3) | ~ v10_lattices(X3) | v3_struct_0(X3)), inference(subsumption_resolution, [], [f995, f280])).
fof(f280, plain, ! [X0] : (~ v3_struct_0(k1_lattice2(X0)) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : ((l3_lattices(X0) & ~ v3_struct_0(X0)) => (v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', fc1_lattice2)).
fof(f995, plain, ! [X3] : (~ v10_lattices(k1_lattice2(X3)) | v3_struct_0(k1_lattice2(X3)) | m2_filter_2(u1_struct_0(k1_lattice2(X3)), X3) | ~ l3_lattices(X3) | ~ v10_lattices(X3) | v3_struct_0(X3)), inference(subsumption_resolution, [], [f991, f250])).
fof(f991, plain, ! [X3] : (~ l3_lattices(k1_lattice2(X3)) | ~ v10_lattices(k1_lattice2(X3)) | v3_struct_0(k1_lattice2(X3)) | m2_filter_2(u1_struct_0(k1_lattice2(X3)), X3) | ~ l3_lattices(X3) | ~ v10_lattices(X3) | v3_struct_0(X3)), inference(resolution, [], [f598, f355])).
fof(f355, plain, ! [X0, X1] : (~ m1_filter_2(X1, k1_lattice2(X0)) | m2_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f226])).
fof(f226, plain, ! [X0] : (! [X1] : ((m2_filter_2(X1, X0) | ~ m1_filter_2(X1, k1_lattice2(X0))) & (m1_filter_2(X1, k1_lattice2(X0)) | ~ m2_filter_2(X1, X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f161])).
fof(f161, plain, ! [X0] : (! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f160])).
fof(f160, plain, ! [X0] : (! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m2_filter_2(X1, X0) <=> m1_filter_2(X1, k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', t21_filter_2)).
fof(f598, plain, ! [X1] : (m1_filter_2(u1_struct_0(X1), X1) | ~ l3_lattices(X1) | ~ v10_lattices(X1) | v3_struct_0(X1)), inference(duplicate_literal_removal, [], [f597])).
fof(f597, plain, ! [X1] : (m1_filter_2(u1_struct_0(X1), X1) | ~ l3_lattices(X1) | ~ v10_lattices(X1) | v3_struct_0(X1) | ~ l3_lattices(X1) | ~ v10_lattices(X1) | v3_struct_0(X1)), inference(resolution, [], [f348, f352])).
fof(f352, plain, ! [X0] : (m1_filter_0(u1_struct_0(X0), X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : (m1_filter_0(u1_struct_0(X0), X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f157])).
fof(f157, plain, ! [X0] : (m1_filter_0(u1_struct_0(X0), X0) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f69])).
fof(f69, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => m1_filter_0(u1_struct_0(X0), X0)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', t15_filter_0)).
fof(f348, plain, ! [X0, X1] : (~ m1_filter_0(X1, X0) | m1_filter_2(X1, X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f224])).
fof(f224, plain, ! [X0] : (! [X1] : ((m1_filter_2(X1, X0) | ~ m1_filter_0(X1, X0)) & (m1_filter_0(X1, X0) | ~ m1_filter_2(X1, X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(nnf_transformation, [], [f156])).
fof(f156, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0] : (! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0)) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => ! [X1] : (m1_filter_2(X1, X0) <=> m1_filter_0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', redefinition_m1_filter_2)).
fof(f2930, plain, spl26_202, inference(avatar_contradiction_clause, [], [f2929])).
fof(f2929, plain, ($false | spl26_202), inference(subsumption_resolution, [], [f2928, f501])).
fof(f501, plain, sP0(sK1), inference(subsumption_resolution, [], [f500, f227])).
fof(f500, plain, (sP0(sK1) | v3_struct_0(sK1)), inference(subsumption_resolution, [], [f495, f228])).
fof(f495, plain, (sP0(sK1) | ~ v10_lattices(sK1) | v3_struct_0(sK1)), inference(resolution, [], [f309, f229])).
fof(f309, plain, ! [X0] : (~ l3_lattices(X0) | sP0(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ! [X0] : (sP0(X0) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(definition_folding, [], [f147, e171])).
fof(f171, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(usedef, [], [e171])).
fof(e171, plain, ! [X0] : (sP0(X0) <=> (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f147, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0)), inference(flattening, [], [f146])).
fof(f146, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | (~ l3_lattices(X0) | ~ v10_lattices(X0) | v3_struct_0(X0))), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : ((l3_lattices(X0) & v10_lattices(X0) & ~ v3_struct_0(X0)) => (v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', fc6_lattice2)).
fof(f2928, plain, (~ sP0(sK1) | spl26_202), inference(resolution, [], [f2851, f308])).
fof(f308, plain, ! [X0] : (v10_lattices(k1_lattice2(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f197])).
fof(f197, plain, ! [X0] : ((v10_lattices(k1_lattice2(X0)) & v9_lattices(k1_lattice2(X0)) & v8_lattices(k1_lattice2(X0)) & v7_lattices(k1_lattice2(X0)) & v6_lattices(k1_lattice2(X0)) & v5_lattices(k1_lattice2(X0)) & v4_lattices(k1_lattice2(X0)) & v3_lattices(k1_lattice2(X0)) & ~ v3_struct_0(k1_lattice2(X0))) | ~ sP0(X0)), inference(nnf_transformation, [], [f171])).
fof(f2851, plain, (~ v10_lattices(k1_lattice2(sK1)) | spl26_202), inference(avatar_component_clause, [], [f2849])).
fof(f1308, plain, spl26_45, inference(avatar_contradiction_clause, [], [f1307])).
fof(f1307, plain, ($false | spl26_45), inference(subsumption_resolution, [], [f1306, f375])).
fof(f375, plain, l2_lattices(sK1), inference(resolution, [], [f254, f229])).
fof(f254, plain, ! [X0] : (~ l3_lattices(X0) | l2_lattices(X0)), inference(cnf_transformation, [], [f110])).
fof(f110, plain, ! [X0] : ((l2_lattices(X0) & l1_lattices(X0)) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ! [X0] : (l3_lattices(X0) => (l2_lattices(X0) & l1_lattices(X0))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', dt_l3_lattices)).
fof(f1306, plain, (~ l2_lattices(sK1) | spl26_45), inference(resolution, [], [f873, f592])).
fof(f592, plain, ! [X1] : (m1_relset_1(u2_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l2_lattices(X1)), inference(resolution, [], [f268, f349])).
fof(f349, plain, ! [X2, X0, X1] : (~ m2_relset_1(X2, X0, X1) | m1_relset_1(X2, X0, X1)), inference(cnf_transformation, [], [f225])).
fof(f225, plain, ! [X0, X1, X2] : ((m2_relset_1(X2, X0, X1) | ~ m1_relset_1(X2, X0, X1)) & (m1_relset_1(X2, X0, X1) | ~ m2_relset_1(X2, X0, X1))), inference(nnf_transformation, [], [f67])).
fof(f67, plain, ! [X0, X1, X2] : (m2_relset_1(X2, X0, X1) <=> m1_relset_1(X2, X0, X1)), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', redefinition_m2_relset_1)).
fof(f268, plain, ! [X0] : (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0] : ((m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0))) | ~ l2_lattices(X0)), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (l2_lattices(X0) => (m2_relset_1(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', dt_u2_lattices)).
fof(f873, plain, (~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl26_45), inference(avatar_component_clause, [], [f871])).
fof(f871, plain, (spl26_45 <=> m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl26_45])])).
fof(f1295, plain, (~ spl26_44 | ~ spl26_45 | spl26_48 | ~ spl26_40 | ~ spl26_41 | ~ spl26_42 | ~ spl26_43), inference(avatar_split_clause, [], [f1294, f863, f859, f855, f851, f883, f871, f867])).
fof(f867, plain, (spl26_44 <=> v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl26_44])])).
fof(f851, plain, (spl26_40 <=> v1_funct_1(u1_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl26_40])])).
fof(f855, plain, (spl26_41 <=> v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl26_41])])).
fof(f859, plain, (spl26_42 <=> m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl26_42])])).
fof(f863, plain, (spl26_43 <=> v1_funct_1(u2_lattices(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl26_43])])).
fof(f1294, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1))) | (~ spl26_40 | ~ spl26_41 | ~ spl26_42 | ~ spl26_43)), inference(subsumption_resolution, [], [f1293, f852])).
fof(f852, plain, (v1_funct_1(u1_lattices(sK1)) | ~ spl26_40), inference(avatar_component_clause, [], [f851])).
fof(f1293, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u1_lattices(sK1))) | (~ spl26_41 | ~ spl26_42 | ~ spl26_43)), inference(subsumption_resolution, [], [f1292, f856])).
fof(f856, plain, (v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ spl26_41), inference(avatar_component_clause, [], [f855])).
fof(f1292, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u1_lattices(sK1))) | (~ spl26_42 | ~ spl26_43)), inference(subsumption_resolution, [], [f1291, f860])).
fof(f860, plain, (m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ spl26_42), inference(avatar_component_clause, [], [f859])).
fof(f1291, plain, (! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u1_lattices(sK1))) | ~ spl26_43), inference(subsumption_resolution, [], [f1283, f864])).
fof(f864, plain, (v1_funct_1(u2_lattices(sK1)) | ~ spl26_43), inference(avatar_component_clause, [], [f863])).
fof(f1283, plain, ! [X17, X15, X16] : (~ (k1_lattice2(sK1) = g3_lattices(X15, X16, X17)) | (u1_struct_0(sK1) = X15) | ~ m1_relset_1(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u2_lattices(sK1)) | ~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | ~ v1_funct_1(u1_lattices(sK1))), inference(superposition, [], [f310, f569])).
fof(f569, plain, (k1_lattice2(sK1) = g3_lattices(u1_struct_0(sK1), u1_lattices(sK1), u2_lattices(sK1))), inference(resolution, [], [f246, f229])).
fof(f246, plain, ! [X0] : (~ l3_lattices(X0) | (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : ((k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0))) | ~ l3_lattices(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (l3_lattices(X0) => (k1_lattice2(X0) = g3_lattices(u1_struct_0(X0), u1_lattices(X0), u2_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', d2_lattice2)).
fof(f310, plain, ! [X4, X2, X0, X5, X3, X1] : (~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) | (X0 = X3) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | ~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1)), inference(flattening, [], [f148])).
fof(f148, plain, ! [X0, X1, X2] : (! [X3, X4, X5] : (((X2 = X5) & (X1 = X4) & (X0 = X3)) | ~ (g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5))) | (~ m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X2) | ~ m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) | ~ v1_funct_1(X1))), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1, X2] : ((m1_relset_1(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X2, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X2) & m1_relset_1(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_2(X1, k2_zfmisc_1(X0, X0), X0) & v1_funct_1(X1)) => ! [X3, X4, X5] : ((g3_lattices(X0, X1, X2) = g3_lattices(X3, X4, X5)) => ((X2 = X5) & (X1 = X4) & (X0 = X3)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', free_g3_lattices)).
fof(f1290, plain, spl26_44, inference(avatar_contradiction_clause, [], [f1289])).
fof(f1289, plain, ($false | spl26_44), inference(subsumption_resolution, [], [f1288, f375])).
fof(f1288, plain, (~ l2_lattices(sK1) | spl26_44), inference(resolution, [], [f869, f267])).
fof(f267, plain, ! [X0] : (v1_funct_2(u2_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f121])).
fof(f869, plain, (~ v1_funct_2(u2_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl26_44), inference(avatar_component_clause, [], [f867])).
fof(f1220, plain, spl26_43, inference(avatar_contradiction_clause, [], [f1219])).
fof(f1219, plain, ($false | spl26_43), inference(subsumption_resolution, [], [f1218, f375])).
fof(f1218, plain, (~ l2_lattices(sK1) | spl26_43), inference(resolution, [], [f865, f266])).
fof(f266, plain, ! [X0] : (v1_funct_1(u2_lattices(X0)) | ~ l2_lattices(X0)), inference(cnf_transformation, [], [f121])).
fof(f865, plain, (~ v1_funct_1(u2_lattices(sK1)) | spl26_43), inference(avatar_component_clause, [], [f863])).
fof(f1217, plain, spl26_42, inference(avatar_contradiction_clause, [], [f1216])).
fof(f1216, plain, ($false | spl26_42), inference(subsumption_resolution, [], [f1215, f369])).
fof(f369, plain, l1_lattices(sK1), inference(resolution, [], [f253, f229])).
fof(f253, plain, ! [X0] : (~ l3_lattices(X0) | l1_lattices(X0)), inference(cnf_transformation, [], [f110])).
fof(f1215, plain, (~ l1_lattices(sK1) | spl26_42), inference(resolution, [], [f861, f588])).
fof(f588, plain, ! [X1] : (m1_relset_1(u1_lattices(X1), k2_zfmisc_1(u1_struct_0(X1), u1_struct_0(X1)), u1_struct_0(X1)) | ~ l1_lattices(X1)), inference(resolution, [], [f265, f349])).
fof(f265, plain, ! [X0] : (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : ((m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0))) | ~ l1_lattices(X0)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0] : (l1_lattices(X0) => (m2_relset_1(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) & v1_funct_1(u1_lattices(X0)))), file('/home/ubuntu/library/tptp/Problems/LAT/LAT304+1.p', dt_u1_lattices)).
fof(f861, plain, (~ m1_relset_1(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl26_42), inference(avatar_component_clause, [], [f859])).
fof(f1214, plain, spl26_41, inference(avatar_contradiction_clause, [], [f1213])).
fof(f1213, plain, ($false | spl26_41), inference(subsumption_resolution, [], [f1212, f369])).
fof(f1212, plain, (~ l1_lattices(sK1) | spl26_41), inference(resolution, [], [f857, f264])).
fof(f264, plain, ! [X0] : (v1_funct_2(u1_lattices(X0), k2_zfmisc_1(u1_struct_0(X0), u1_struct_0(X0)), u1_struct_0(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f120])).
fof(f857, plain, (~ v1_funct_2(u1_lattices(sK1), k2_zfmisc_1(u1_struct_0(sK1), u1_struct_0(sK1)), u1_struct_0(sK1)) | spl26_41), inference(avatar_component_clause, [], [f855])).
fof(f888, plain, spl26_40, inference(avatar_contradiction_clause, [], [f887])).
fof(f887, plain, ($false | spl26_40), inference(subsumption_resolution, [], [f886, f369])).
fof(f886, plain, (~ l1_lattices(sK1) | spl26_40), inference(resolution, [], [f853, f263])).
fof(f263, plain, ! [X0] : (v1_funct_1(u1_lattices(X0)) | ~ l1_lattices(X0)), inference(cnf_transformation, [], [f120])).
fof(f853, plain, (~ v1_funct_1(u1_lattices(sK1)) | spl26_40), inference(avatar_component_clause, [], [f851])).