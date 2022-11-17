fof(f168723, plain, $false, inference(avatar_sat_refutation, [], [f322, f337, f348, f1433, f1435, f168714])).
fof(f168714, plain, (~ spl17_2 | ~ spl17_31 | ~ spl17_34), inference(avatar_contradiction_clause, [], [f168713])).
fof(f168713, plain, ($false | (~ spl17_2 | ~ spl17_31 | ~ spl17_34)), inference(subsumption_resolution, [], [f168712, f1397])).
fof(f1397, plain, (aNaturalNumber0(xp) | ~ spl17_34), inference(avatar_component_clause, [], [f1396])).
fof(f1396, plain, (spl17_34 <=> aNaturalNumber0(xp)), introduced(avatar_definition, [new_symbols(naming, [spl17_34])])).
fof(f168712, plain, (~ aNaturalNumber0(xp) | (~ spl17_2 | ~ spl17_31 | ~ spl17_34)), inference(subsumption_resolution, [], [f168708, f289])).
fof(f289, plain, ~ doDivides0(xp, xn), inference(cnf_transformation, [], [f123])).
fof(f123, plain, (~ doDivides0(xp, xm) & ! [X0] : (~ (xm = sdtasdt0(xp, X0)) | ~ aNaturalNumber0(X0)) & ~ doDivides0(xp, xn) & ! [X1] : (~ (xn = sdtasdt0(xp, X1)) | ~ aNaturalNumber0(X1))), inference(ennf_transformation, [], [f54])).
fof(f54, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xn) | ? [X1] : ((xn = sdtasdt0(xp, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f48])).
fof(f48, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xn) | ? [X0] : ((xn = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))), inference(negated_conjecture, [], [f47])).
fof(f47, plain, ~ (doDivides0(xp, xm) | ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) | doDivides0(xp, xn) | ? [X0] : ((xn = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', m__)).
fof(f168708, plain, (doDivides0(xp, xn) | ~ aNaturalNumber0(xp) | (~ spl17_2 | ~ spl17_31 | ~ spl17_34)), inference(resolution, [], [f7934, f6988])).
fof(f6988, plain, (doDivides0(xp, xp) | ~ spl17_34), inference(subsumption_resolution, [], [f6987, f1397])).
fof(f6987, plain, (doDivides0(xp, xp) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f4267, f175])).
fof(f175, plain, aNaturalNumber0(sz10), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (sz00 = sz10) & aNaturalNumber0(sz10)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', mSortsC_01)).
fof(f4267, plain, (doDivides0(xp, xp) | ~ aNaturalNumber0(sz10) | ~ aNaturalNumber0(xp)), inference(superposition, [], [f308, f382])).
fof(f382, plain, (xp = sdtasdt0(xp, sz10)), inference(resolution, [], [f185, f244])).
fof(f244, plain, aNaturalNumber0(xp), inference(cnf_transformation, [], [f39])).
fof(f39, plain, (aNaturalNumber0(xp) & aNaturalNumber0(xm) & aNaturalNumber0(xn)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', m__1837)).
fof(f185, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sdtasdt0(X0, sz10) = X0)), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ! [X0] : (((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : (aNaturalNumber0(X0) => ((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', m_MulUnit)).
fof(f308, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f298, f178])).
fof(f178, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f57])).
fof(f57, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', mSortsB_02)).
fof(f298, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f223])).
fof(f223, plain, ! [X2, X0, X1] : (doDivides0(X0, X1) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtasdt0(X0, sK4(X0, X1)) = X1) & aNaturalNumber0(sK4(X0, X1))) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f136, f137])).
fof(f137, plain, ! [X1, X0] : (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X0, sK4(X0, X1)) = X1) & aNaturalNumber0(sK4(X0, X1)))), introduced(choice_axiom, [])).
fof(f136, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f135])).
fof(f135, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f101])).
fof(f101, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', mDefDiv)).
fof(f7934, plain, (! [X8] : (~ doDivides0(X8, xp) | doDivides0(X8, xn) | ~ aNaturalNumber0(X8)) | (~ spl17_2 | ~ spl17_31 | ~ spl17_34)), inference(subsumption_resolution, [], [f7933, f7049])).
fof(f7049, plain, (! [X1] : (~ doDivides0(X1, xp) | doDivides0(X1, xr) | ~ aNaturalNumber0(X1)) | (~ spl17_2 | ~ spl17_31 | ~ spl17_34)), inference(subsumption_resolution, [], [f7048, f1397])).
fof(f7048, plain, (! [X1] : (doDivides0(X1, xr) | ~ doDivides0(X1, xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X1)) | (~ spl17_2 | ~ spl17_31)), inference(subsumption_resolution, [], [f7035, f1384])).
fof(f1384, plain, (aNaturalNumber0(xr) | ~ spl17_31), inference(avatar_component_clause, [], [f1383])).
fof(f1383, plain, (spl17_31 <=> aNaturalNumber0(xr)), introduced(avatar_definition, [new_symbols(naming, [spl17_31])])).
fof(f7035, plain, (! [X1] : (doDivides0(X1, xr) | ~ doDivides0(X1, xp) | ~ aNaturalNumber0(xr) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X1)) | ~ spl17_2), inference(resolution, [], [f321, f227])).
fof(f227, plain, ! [X2, X0, X1] : (~ doDivides0(X1, X2) | doDivides0(X0, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ! [X0, X1, X2] : (doDivides0(X0, X2) | ~ doDivides0(X1, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f105])).
fof(f105, plain, ! [X0, X1, X2] : ((doDivides0(X0, X2) | (~ doDivides0(X1, X2) | ~ doDivides0(X0, X1))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f32])).
fof(f32, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X1, X2) & doDivides0(X0, X1)) => doDivides0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', mDivTrans)).
fof(f321, plain, (doDivides0(xp, xr) | ~ spl17_2), inference(avatar_component_clause, [], [f319])).
fof(f319, plain, (spl17_2 <=> doDivides0(xp, xr)), introduced(avatar_definition, [new_symbols(naming, [spl17_2])])).
fof(f7933, plain, (! [X8] : (doDivides0(X8, xn) | ~ doDivides0(X8, xr) | ~ doDivides0(X8, xp) | ~ aNaturalNumber0(X8)) | (~ spl17_31 | ~ spl17_34)), inference(subsumption_resolution, [], [f7932, f1397])).
fof(f7932, plain, (! [X8] : (doDivides0(X8, xn) | ~ doDivides0(X8, xr) | ~ doDivides0(X8, xp) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X8)) | (~ spl17_31 | ~ spl17_34)), inference(subsumption_resolution, [], [f7914, f1384])).
fof(f7914, plain, (! [X8] : (doDivides0(X8, xn) | ~ doDivides0(X8, xr) | ~ doDivides0(X8, xp) | ~ aNaturalNumber0(xr) | ~ aNaturalNumber0(xp) | ~ aNaturalNumber0(X8)) | ~ spl17_34), inference(superposition, [], [f228, f7006])).
fof(f7006, plain, ((xn = sdtpldt0(xp, xr)) | ~ spl17_34), inference(backward_demodulation, [], [f3806, f274])).
fof(f274, plain, (xr = sdtmndt0(xn, xp)), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((xr = sdtmndt0(xn, xp)) & (xn = sdtpldt0(xp, xr)) & aNaturalNumber0(xr)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', m__1883)).
fof(f3806, plain, ((xn = sdtpldt0(xp, sdtmndt0(xn, xp))) | ~ spl17_34), inference(subsumption_resolution, [], [f3805, f1397])).
fof(f3805, plain, ((xn = sdtpldt0(xp, sdtmndt0(xn, xp))) | ~ aNaturalNumber0(xp)), inference(subsumption_resolution, [], [f3801, f242])).
fof(f242, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f39])).
fof(f3801, plain, ((xn = sdtpldt0(xp, sdtmndt0(xn, xp))) | ~ aNaturalNumber0(xn) | ~ aNaturalNumber0(xp)), inference(resolution, [], [f271, f294])).
fof(f294, plain, ! [X0, X1] : (~ sdtlseqdt0(X0, X1) | (sdtpldt0(X0, sdtmndt0(X1, X0)) = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f202])).
fof(f202, plain, ! [X2, X0, X1] : ((sdtpldt0(X0, X2) = X1) | ~ (sdtmndt0(X1, X0) = X2) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X1, X0) = X2) | ~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2)) & (((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtmndt0(X1, X0) = X2))) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f133])).
fof(f133, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X1, X0) = X2) | (~ (sdtpldt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ (sdtmndt0(X1, X0) = X2))) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f83])).
fof(f83, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X1, X0) = X2) <=> ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f82])).
fof(f82, plain, ! [X0, X1] : ((! [X2] : ((sdtmndt0(X1, X0) = X2) <=> ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ sdtlseqdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (sdtlseqdt0(X0, X1) => ! [X2] : ((sdtmndt0(X1, X0) = X2) <=> ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', mDefDiff)).
fof(f271, plain, sdtlseqdt0(xp, xn), inference(cnf_transformation, [], [f163])).
fof(f163, plain, (sdtlseqdt0(xp, xn) & ((xn = sdtpldt0(xp, sK12)) & aNaturalNumber0(sK12))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f42, f162])).
fof(f162, plain, (? [X0] : ((xn = sdtpldt0(xp, X0)) & aNaturalNumber0(X0)) => ((xn = sdtpldt0(xp, sK12)) & aNaturalNumber0(sK12))), introduced(choice_axiom, [])).
fof(f42, plain, (sdtlseqdt0(xp, xn) & ? [X0] : ((xn = sdtpldt0(xp, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', m__1870)).
fof(f228, plain, ! [X2, X0, X1] : (doDivides0(X0, sdtpldt0(X1, X2)) | ~ doDivides0(X0, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f108])).
fof(f108, plain, ! [X0, X1, X2] : (doDivides0(X0, sdtpldt0(X1, X2)) | ~ doDivides0(X0, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f107])).
fof(f107, plain, ! [X0, X1, X2] : ((doDivides0(X0, sdtpldt0(X1, X2)) | (~ doDivides0(X0, X2) | ~ doDivides0(X0, X1))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f33])).
fof(f33, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X0, X2) & doDivides0(X0, X1)) => doDivides0(X0, sdtpldt0(X1, X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', mDivSum)).
fof(f1435, plain, spl17_31, inference(avatar_split_clause, [], [f272, f1383])).
fof(f272, plain, aNaturalNumber0(xr), inference(cnf_transformation, [], [f43])).
fof(f1433, plain, spl17_34, inference(avatar_split_clause, [], [f244, f1396])).
fof(f348, plain, ~ spl17_5, inference(avatar_split_clause, [], [f291, f334])).
fof(f334, plain, (spl17_5 <=> doDivides0(xp, xm)), introduced(avatar_definition, [new_symbols(naming, [spl17_5])])).
fof(f291, plain, ~ doDivides0(xp, xm), inference(cnf_transformation, [], [f123])).
fof(f337, plain, (spl17_1 | spl17_5), inference(avatar_split_clause, [], [f287, f334, f315])).
fof(f315, plain, (spl17_1 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl17_1])])).
fof(f287, plain, (doDivides0(xp, xm) | sP2), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ((doDivides0(xp, xm) & ((xm = sdtasdt0(xp, sK16)) & aNaturalNumber0(sK16))) | sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16])], [f128, f172])).
fof(f172, plain, (? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0)) => ((xm = sdtasdt0(xp, sK16)) & aNaturalNumber0(sK16))), introduced(choice_axiom, [])).
fof(f128, plain, ((doDivides0(xp, xm) & ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))) | sP2), inference(definition_folding, [], [f53, e127])).
fof(f127, plain, ((doDivides0(xp, xr) & ? [X1] : ((xr = sdtasdt0(xp, X1)) & aNaturalNumber0(X1))) | ~ sP2), inference(usedef, [], [e127])).
fof(e127, plain, (sP2 <=> (doDivides0(xp, xr) & ? [X1] : ((xr = sdtasdt0(xp, X1)) & aNaturalNumber0(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f53, plain, ((doDivides0(xp, xm) & ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))) | (doDivides0(xp, xr) & ? [X1] : ((xr = sdtasdt0(xp, X1)) & aNaturalNumber0(X1)))), inference(rectify, [], [f46])).
fof(f46, plain, ((doDivides0(xp, xm) & ? [X0] : ((xm = sdtasdt0(xp, X0)) & aNaturalNumber0(X0))) | (doDivides0(xp, xr) & ? [X0] : ((sdtasdt0(xp, X0) = xr) & aNaturalNumber0(X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM496+3.p', m__2027)).
fof(f322, plain, (~ spl17_1 | spl17_2), inference(avatar_split_clause, [], [f284, f319, f315])).
fof(f284, plain, (doDivides0(xp, xr) | ~ sP2), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ((doDivides0(xp, xr) & ((xr = sdtasdt0(xp, sK15)) & aNaturalNumber0(sK15))) | ~ sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15])], [f169, f170])).
fof(f170, plain, (? [X0] : ((sdtasdt0(xp, X0) = xr) & aNaturalNumber0(X0)) => ((xr = sdtasdt0(xp, sK15)) & aNaturalNumber0(sK15))), introduced(choice_axiom, [])).
fof(f169, plain, ((doDivides0(xp, xr) & ? [X0] : ((sdtasdt0(xp, X0) = xr) & aNaturalNumber0(X0))) | ~ sP2), inference(rectify, [], [f168])).
fof(f168, plain, ((doDivides0(xp, xr) & ? [X1] : ((xr = sdtasdt0(xp, X1)) & aNaturalNumber0(X1))) | ~ sP2), inference(nnf_transformation, [], [f127])).