fof(f3359, plain, $false, inference(avatar_sat_refutation, [], [f1381, f1404, f1506, f1513, f1555, f1572, f1576, f1665, f1666, f1668, f3271, f3297, f3316, f3339, f3353])).
fof(f3353, plain, (spl18_67 | spl18_51 | spl18_55 | ~ spl18_56), inference(avatar_split_clause, [], [f3352, f1401, f1397, f1333, f1503])).
fof(f1503, plain, (spl18_67 <=> in(sK4(sK15(sK2, sK3)), sK2)), introduced(avatar_definition, [new_symbols(naming, [spl18_67])])).
fof(f1333, plain, (spl18_51 <=> sP1(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl18_51])])).
fof(f1397, plain, (spl18_55 <=> (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_55])])).
fof(f1401, plain, (spl18_56 <=> (sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3))))), introduced(avatar_definition, [new_symbols(naming, [spl18_56])])).
fof(f3352, plain, (in(sK4(sK15(sK2, sK3)), sK2) | (spl18_51 | spl18_55 | ~ spl18_56)), inference(subsumption_resolution, [], [f3351, f1398])).
fof(f1398, plain, (~ (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | spl18_55), inference(avatar_component_clause, [], [f1397])).
fof(f3351, plain, (in(sK4(sK15(sK2, sK3)), sK2) | (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | (spl18_51 | ~ spl18_56)), inference(subsumption_resolution, [], [f3350, f60])).
fof(f60, plain, ordinal(sK3), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (! [X2] : ((! [X4] : (~ in(X4, sK2) | ~ (sK4(X2) = X4) | ~ ordinal(X4)) | ~ in(sK4(X2), succ(sK3)) | ~ in(sK4(X2), X2)) & (((in(sK5(X2), sK2) & (sK4(X2) = sK5(X2)) & ordinal(sK5(X2))) & in(sK4(X2), succ(sK3))) | in(sK4(X2), X2))) & ordinal(sK3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4, sK5])], [f32, f35, f34, f33])).
fof(f33, plain, (? [X0, X1] : (! [X2] : ? [X3] : ((! [X4] : (~ in(X4, X0) | ~ (X3 = X4) | ~ ordinal(X4)) | ~ in(X3, succ(X1)) | ~ in(X3, X2)) & ((? [X5] : (in(X5, X0) & (X3 = X5) & ordinal(X5)) & in(X3, succ(X1))) | in(X3, X2))) & ordinal(X1)) => (! [X2] : ? [X3] : ((! [X4] : (~ in(X4, sK2) | ~ (X3 = X4) | ~ ordinal(X4)) | ~ in(X3, succ(sK3)) | ~ in(X3, X2)) & ((? [X5] : (in(X5, sK2) & (X3 = X5) & ordinal(X5)) & in(X3, succ(sK3))) | in(X3, X2))) & ordinal(sK3))), introduced(choice_axiom, [])).
fof(f34, plain, ! [X2] : (? [X3] : ((! [X4] : (~ in(X4, sK2) | ~ (X3 = X4) | ~ ordinal(X4)) | ~ in(X3, succ(sK3)) | ~ in(X3, X2)) & ((? [X5] : (in(X5, sK2) & (X3 = X5) & ordinal(X5)) & in(X3, succ(sK3))) | in(X3, X2))) => ((! [X4] : (~ in(X4, sK2) | ~ (sK4(X2) = X4) | ~ ordinal(X4)) | ~ in(sK4(X2), succ(sK3)) | ~ in(sK4(X2), X2)) & ((? [X5] : (in(X5, sK2) & (sK4(X2) = X5) & ordinal(X5)) & in(sK4(X2), succ(sK3))) | in(sK4(X2), X2)))), introduced(choice_axiom, [])).
fof(f35, plain, ! [X2] : (? [X5] : (in(X5, sK2) & (sK4(X2) = X5) & ordinal(X5)) => (in(sK5(X2), sK2) & (sK4(X2) = sK5(X2)) & ordinal(sK5(X2)))), introduced(choice_axiom, [])).
fof(f32, plain, ? [X0, X1] : (! [X2] : ? [X3] : ((! [X4] : (~ in(X4, X0) | ~ (X3 = X4) | ~ ordinal(X4)) | ~ in(X3, succ(X1)) | ~ in(X3, X2)) & ((? [X5] : (in(X5, X0) & (X3 = X5) & ordinal(X5)) & in(X3, succ(X1))) | in(X3, X2))) & ordinal(X1)), inference(rectify, [], [f31])).
fof(f31, plain, ? [X0, X1] : (! [X2] : ? [X3] : ((! [X4] : (~ in(X4, X0) | ~ (X3 = X4) | ~ ordinal(X4)) | ~ in(X3, succ(X1)) | ~ in(X3, X2)) & ((? [X4] : (in(X4, X0) & (X3 = X4) & ordinal(X4)) & in(X3, succ(X1))) | in(X3, X2))) & ordinal(X1)), inference(flattening, [], [f30])).
fof(f30, plain, ? [X0, X1] : (! [X2] : ? [X3] : (((! [X4] : (~ in(X4, X0) | ~ (X3 = X4) | ~ ordinal(X4)) | ~ in(X3, succ(X1))) | ~ in(X3, X2)) & ((? [X4] : (in(X4, X0) & (X3 = X4) & ordinal(X4)) & in(X3, succ(X1))) | in(X3, X2))) & ordinal(X1)), inference(nnf_transformation, [], [f18])).
fof(f18, plain, ? [X0, X1] : (! [X2] : ? [X3] : ~ (in(X3, X2) <=> (? [X4] : (in(X4, X0) & (X3 = X4) & ordinal(X4)) & in(X3, succ(X1)))) & ordinal(X1)), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0, X1] : (ordinal(X1) => ? [X2] : ! [X3] : (in(X3, X2) <=> (? [X4] : (in(X4, X0) & (X3 = X4) & ordinal(X4)) & in(X3, succ(X1))))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0, X1] : (ordinal(X1) => ? [X2] : ! [X3] : (in(X3, X2) <=> (? [X4] : (in(X4, X0) & (X3 = X4) & ordinal(X4)) & in(X3, succ(X1))))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU272+1.p', s1_xboole_0__e8_6__wellord2__1)).
fof(f3350, plain, (in(sK4(sK15(sK2, sK3)), sK2) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | (spl18_51 | ~ spl18_56)), inference(subsumption_resolution, [], [f2820, f1334])).
fof(f1334, plain, (~ sP1(sK2) | spl18_51), inference(avatar_component_clause, [], [f1333])).
fof(f2820, plain, (in(sK4(sK15(sK2, sK3)), sK2) | sP1(sK2) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | ~ spl18_56), inference(superposition, [], [f154, f1403])).
fof(f1403, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | ~ spl18_56), inference(avatar_component_clause, [], [f1401])).
fof(f154, plain, ! [X2, X3] : (in(sK17(X2, sK4(sK15(X2, X3))), X2) | sP1(X2) | ~ ordinal(X3) | (sK4(sK15(X2, X3)) = sK5(sK15(X2, X3)))), inference(resolution, [], [f101, f63])).
fof(f63, plain, ! [X2] : (in(sK4(X2), X2) | (sK4(X2) = sK5(X2))), inference(cnf_transformation, [], [f36])).
fof(f101, plain, ! [X0, X3, X1] : (~ in(X3, sK15(X0, X1)) | in(sK17(X0, X3), X0) | sP1(X0) | ~ ordinal(X1)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0, X1] : (! [X3] : ((in(X3, sK15(X0, X1)) | ! [X4] : (! [X5] : (~ in(X5, X0) | ~ (X3 = X5) | ~ ordinal(X5)) | ~ (X3 = X4) | ~ in(X4, succ(X1)))) & (((in(sK17(X0, X3), X0) & (sK17(X0, X3) = X3) & ordinal(sK17(X0, X3))) & (sK16(X0, X1, X3) = X3) & in(sK16(X0, X1, X3), succ(X1))) | ~ in(X3, sK15(X0, X1)))) | sP1(X0) | ~ ordinal(X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16, sK17])], [f55, f58, f57, f56])).
fof(f56, plain, ! [X1, X0] : (? [X2] : ! [X3] : ((in(X3, X2) | ! [X4] : (! [X5] : (~ in(X5, X0) | ~ (X3 = X5) | ~ ordinal(X5)) | ~ (X3 = X4) | ~ in(X4, succ(X1)))) & (? [X6] : (? [X7] : (in(X7, X0) & (X3 = X7) & ordinal(X7)) & (X3 = X6) & in(X6, succ(X1))) | ~ in(X3, X2))) => ! [X3] : ((in(X3, sK15(X0, X1)) | ! [X4] : (! [X5] : (~ in(X5, X0) | ~ (X3 = X5) | ~ ordinal(X5)) | ~ (X3 = X4) | ~ in(X4, succ(X1)))) & (? [X6] : (? [X7] : (in(X7, X0) & (X3 = X7) & ordinal(X7)) & (X3 = X6) & in(X6, succ(X1))) | ~ in(X3, sK15(X0, X1))))), introduced(choice_axiom, [])).
fof(f57, plain, ! [X3, X1, X0] : (? [X6] : (? [X7] : (in(X7, X0) & (X3 = X7) & ordinal(X7)) & (X3 = X6) & in(X6, succ(X1))) => (? [X7] : (in(X7, X0) & (X3 = X7) & ordinal(X7)) & (sK16(X0, X1, X3) = X3) & in(sK16(X0, X1, X3), succ(X1)))), introduced(choice_axiom, [])).
fof(f58, plain, ! [X3, X0] : (? [X7] : (in(X7, X0) & (X3 = X7) & ordinal(X7)) => (in(sK17(X0, X3), X0) & (sK17(X0, X3) = X3) & ordinal(sK17(X0, X3)))), introduced(choice_axiom, [])).
fof(f55, plain, ! [X0, X1] : (? [X2] : ! [X3] : ((in(X3, X2) | ! [X4] : (! [X5] : (~ in(X5, X0) | ~ (X3 = X5) | ~ ordinal(X5)) | ~ (X3 = X4) | ~ in(X4, succ(X1)))) & (? [X6] : (? [X7] : (in(X7, X0) & (X3 = X7) & ordinal(X7)) & (X3 = X6) & in(X6, succ(X1))) | ~ in(X3, X2))) | sP1(X0) | ~ ordinal(X1)), inference(rectify, [], [f54])).
fof(f54, plain, ! [X0, X1] : (? [X7] : ! [X8] : ((in(X8, X7) | ! [X9] : (! [X10] : (~ in(X10, X0) | ~ (X8 = X10) | ~ ordinal(X10)) | ~ (X8 = X9) | ~ in(X9, succ(X1)))) & (? [X9] : (? [X10] : (in(X10, X0) & (X8 = X10) & ordinal(X10)) & (X8 = X9) & in(X9, succ(X1))) | ~ in(X8, X7))) | sP1(X0) | ~ ordinal(X1)), inference(nnf_transformation, [], [f29])).
fof(f29, plain, ! [X0, X1] : (? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (in(X10, X0) & (X8 = X10) & ordinal(X10)) & (X8 = X9) & in(X9, succ(X1)))) | sP1(X0) | ~ ordinal(X1)), inference(definition_folding, [], [f26, e28, e27])).
fof(f27, plain, ! [X0, X3] : (? [X6] : (in(X6, X0) & (X3 = X6) & ordinal(X6)) | ~ sP0(X0, X3)), inference(usedef, [], [e27])).
fof(e27, plain, ! [X0, X3] : (sP0(X0, X3) <=> ? [X6] : (in(X6, X0) & (X3 = X6) & ordinal(X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f28, plain, ! [X0] : (? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (in(X5, X0) & (X4 = X5) & ordinal(X5)) & (X2 = X4) & sP0(X0, X3) & (X2 = X3)) | ~ sP1(X0)), inference(usedef, [], [e28])).
fof(e28, plain, ! [X0] : (sP1(X0) <=> ? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (in(X5, X0) & (X4 = X5) & ordinal(X5)) & (X2 = X4) & sP0(X0, X3) & (X2 = X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f26, plain, ! [X0, X1] : (? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (in(X10, X0) & (X8 = X10) & ordinal(X10)) & (X8 = X9) & in(X9, succ(X1)))) | ? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (in(X5, X0) & (X4 = X5) & ordinal(X5)) & (X2 = X4) & ? [X6] : (in(X6, X0) & (X3 = X6) & ordinal(X6)) & (X2 = X3)) | ~ ordinal(X1)), inference(flattening, [], [f25])).
fof(f25, plain, ! [X0, X1] : ((? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (in(X10, X0) & (X8 = X10) & ordinal(X10)) & (X8 = X9) & in(X9, succ(X1)))) | ? [X2, X3, X4] : (~ (X3 = X4) & (? [X5] : (in(X5, X0) & (X4 = X5) & ordinal(X5)) & (X2 = X4) & ? [X6] : (in(X6, X0) & (X3 = X6) & ordinal(X6)) & (X2 = X3)))) | ~ ordinal(X1)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : (ordinal(X1) => (! [X2, X3, X4] : ((? [X5] : (in(X5, X0) & (X4 = X5) & ordinal(X5)) & (X2 = X4) & ? [X6] : (in(X6, X0) & (X3 = X6) & ordinal(X6)) & (X2 = X3)) => (X3 = X4)) => ? [X7] : ! [X8] : (in(X8, X7) <=> ? [X9] : (? [X10] : (in(X10, X0) & (X8 = X10) & ordinal(X10)) & (X8 = X9) & in(X9, succ(X1)))))), inference(rectify, [], [f15])).
fof(f15, plain, ! [X0, X1] : (ordinal(X1) => (! [X2, X3, X4] : ((? [X6] : (in(X6, X0) & (X4 = X6) & ordinal(X6)) & (X2 = X4) & ? [X5] : (in(X5, X0) & (X3 = X5) & ordinal(X5)) & (X2 = X3)) => (X3 = X4)) => ? [X2] : ! [X3] : (in(X3, X2) <=> ? [X4] : (? [X7] : (in(X7, X0) & (X3 = X7) & ordinal(X7)) & (X3 = X4) & in(X4, succ(X1)))))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU272+1.p', s1_tarski__e8_6__wellord2__1)).
fof(f3339, plain, (spl18_55 | spl18_68 | spl18_51 | ~ spl18_56), inference(avatar_split_clause, [], [f3338, f1401, f1333, f1510, f1397])).
fof(f1510, plain, (spl18_68 <=> ordinal(sK4(sK15(sK2, sK3)))), introduced(avatar_definition, [new_symbols(naming, [spl18_68])])).
fof(f3338, plain, (ordinal(sK4(sK15(sK2, sK3))) | (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | (spl18_51 | ~ spl18_56)), inference(subsumption_resolution, [], [f3337, f60])).
fof(f3337, plain, (ordinal(sK4(sK15(sK2, sK3))) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | (spl18_51 | ~ spl18_56)), inference(subsumption_resolution, [], [f2816, f1334])).
fof(f2816, plain, (ordinal(sK4(sK15(sK2, sK3))) | sP1(sK2) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | ~ spl18_56), inference(superposition, [], [f142, f1403])).
fof(f142, plain, ! [X2, X3] : (ordinal(sK17(X2, sK4(sK15(X2, X3)))) | sP1(X2) | ~ ordinal(X3) | (sK4(sK15(X2, X3)) = sK5(sK15(X2, X3)))), inference(resolution, [], [f99, f63])).
fof(f99, plain, ! [X0, X3, X1] : (~ in(X3, sK15(X0, X1)) | ordinal(sK17(X0, X3)) | sP1(X0) | ~ ordinal(X1)), inference(cnf_transformation, [], [f59])).
fof(f3316, plain, (spl18_63 | spl18_51 | ~ spl18_72), inference(avatar_split_clause, [], [f3315, f1551, f1333, f1433])).
fof(f1433, plain, (spl18_63 <=> (sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3))))), introduced(avatar_definition, [new_symbols(naming, [spl18_63])])).
fof(f1551, plain, (spl18_72 <=> in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3))), introduced(avatar_definition, [new_symbols(naming, [spl18_72])])).
fof(f3315, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | (spl18_51 | ~ spl18_72)), inference(subsumption_resolution, [], [f3314, f60])).
fof(f3314, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ ordinal(sK3) | (spl18_51 | ~ spl18_72)), inference(subsumption_resolution, [], [f1791, f1334])).
fof(f1791, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | sP1(sK2) | ~ ordinal(sK3) | ~ spl18_72), inference(resolution, [], [f1553, f98])).
fof(f98, plain, ! [X0, X3, X1] : (~ in(X3, sK15(X0, X1)) | (sK16(X0, X1, X3) = X3) | sP1(X0) | ~ ordinal(X1)), inference(cnf_transformation, [], [f59])).
fof(f1553, plain, (in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ spl18_72), inference(avatar_component_clause, [], [f1551])).
fof(f3297, plain, (~ spl18_67 | ~ spl18_68 | ~ spl18_72 | ~ spl18_73), inference(avatar_contradiction_clause, [], [f3296])).
fof(f3296, plain, ($false | (~ spl18_67 | ~ spl18_68 | ~ spl18_72 | ~ spl18_73)), inference(subsumption_resolution, [], [f3295, f1553])).
fof(f3295, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | (~ spl18_67 | ~ spl18_68 | ~ spl18_73)), inference(subsumption_resolution, [], [f3294, f1505])).
fof(f1505, plain, (in(sK4(sK15(sK2, sK3)), sK2) | ~ spl18_67), inference(avatar_component_clause, [], [f1503])).
fof(f3294, plain, (~ in(sK4(sK15(sK2, sK3)), sK2) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | (~ spl18_68 | ~ spl18_73)), inference(subsumption_resolution, [], [f3279, f1512])).
fof(f1512, plain, (ordinal(sK4(sK15(sK2, sK3))) | ~ spl18_68), inference(avatar_component_clause, [], [f1510])).
fof(f3279, plain, (~ ordinal(sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), sK2) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ spl18_73), inference(resolution, [], [f1663, f103])).
fof(f103, plain, ! [X2] : (~ in(sK4(X2), succ(sK3)) | ~ ordinal(sK4(X2)) | ~ in(sK4(X2), sK2) | ~ in(sK4(X2), X2)), inference(equality_resolution, [], [f65])).
fof(f65, plain, ! [X4, X2] : (~ in(X4, sK2) | ~ (sK4(X2) = X4) | ~ ordinal(X4) | ~ in(sK4(X2), succ(sK3)) | ~ in(sK4(X2), X2)), inference(cnf_transformation, [], [f36])).
fof(f1663, plain, (in(sK4(sK15(sK2, sK3)), succ(sK3)) | ~ spl18_73), inference(avatar_component_clause, [], [f1662])).
fof(f1662, plain, (spl18_73 <=> in(sK4(sK15(sK2, sK3)), succ(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl18_73])])).
fof(f3271, plain, (spl18_73 | spl18_51 | ~ spl18_63 | ~ spl18_72), inference(avatar_split_clause, [], [f3270, f1551, f1433, f1333, f1662])).
fof(f3270, plain, (in(sK4(sK15(sK2, sK3)), succ(sK3)) | (spl18_51 | ~ spl18_63 | ~ spl18_72)), inference(subsumption_resolution, [], [f3269, f60])).
fof(f3269, plain, (in(sK4(sK15(sK2, sK3)), succ(sK3)) | ~ ordinal(sK3) | (spl18_51 | ~ spl18_63 | ~ spl18_72)), inference(subsumption_resolution, [], [f3268, f1334])).
fof(f3268, plain, (in(sK4(sK15(sK2, sK3)), succ(sK3)) | sP1(sK2) | ~ ordinal(sK3) | (~ spl18_63 | ~ spl18_72)), inference(subsumption_resolution, [], [f3248, f1553])).
fof(f3248, plain, (in(sK4(sK15(sK2, sK3)), succ(sK3)) | ~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | sP1(sK2) | ~ ordinal(sK3) | ~ spl18_63), inference(superposition, [], [f97, f1435])).
fof(f1435, plain, ((sK4(sK15(sK2, sK3)) = sK16(sK2, sK3, sK4(sK15(sK2, sK3)))) | ~ spl18_63), inference(avatar_component_clause, [], [f1433])).
fof(f97, plain, ! [X0, X3, X1] : (in(sK16(X0, X1, X3), succ(X1)) | ~ in(X3, sK15(X0, X1)) | sP1(X0) | ~ ordinal(X1)), inference(cnf_transformation, [], [f59])).
fof(f1668, plain, (spl18_68 | ~ spl18_55 | spl18_72), inference(avatar_split_clause, [], [f1667, f1551, f1397, f1510])).
fof(f1667, plain, (ordinal(sK4(sK15(sK2, sK3))) | (~ spl18_55 | spl18_72)), inference(forward_demodulation, [], [f1649, f1399])).
fof(f1399, plain, ((sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | ~ spl18_55), inference(avatar_component_clause, [], [f1397])).
fof(f1649, plain, (ordinal(sK5(sK15(sK2, sK3))) | spl18_72), inference(resolution, [], [f1552, f62])).
fof(f62, plain, ! [X2] : (in(sK4(X2), X2) | ordinal(sK5(X2))), inference(cnf_transformation, [], [f36])).
fof(f1552, plain, (~ in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | spl18_72), inference(avatar_component_clause, [], [f1551])).
fof(f1666, plain, (spl18_73 | spl18_72), inference(avatar_split_clause, [], [f1648, f1551, f1662])).
fof(f1648, plain, (in(sK4(sK15(sK2, sK3)), succ(sK3)) | spl18_72), inference(resolution, [], [f1552, f61])).
fof(f61, plain, ! [X2] : (in(sK4(X2), succ(sK3)) | in(sK4(X2), X2)), inference(cnf_transformation, [], [f36])).
fof(f1665, plain, (~ spl18_73 | ~ spl18_68 | spl18_51 | ~ spl18_67 | spl18_72), inference(avatar_split_clause, [], [f1660, f1551, f1503, f1333, f1510, f1662])).
fof(f1660, plain, (~ ordinal(sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), succ(sK3)) | (spl18_51 | ~ spl18_67 | spl18_72)), inference(subsumption_resolution, [], [f1659, f60])).
fof(f1659, plain, (~ ordinal(sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), succ(sK3)) | ~ ordinal(sK3) | (spl18_51 | ~ spl18_67 | spl18_72)), inference(subsumption_resolution, [], [f1658, f1334])).
fof(f1658, plain, (~ ordinal(sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), succ(sK3)) | sP1(sK2) | ~ ordinal(sK3) | (~ spl18_67 | spl18_72)), inference(subsumption_resolution, [], [f1647, f1505])).
fof(f1647, plain, (~ in(sK4(sK15(sK2, sK3)), sK2) | ~ ordinal(sK4(sK15(sK2, sK3))) | ~ in(sK4(sK15(sK2, sK3)), succ(sK3)) | sP1(sK2) | ~ ordinal(sK3) | spl18_72), inference(resolution, [], [f1552, f105])).
fof(f105, plain, ! [X0, X5, X1] : (in(X5, sK15(X0, X1)) | ~ in(X5, X0) | ~ ordinal(X5) | ~ in(X5, succ(X1)) | sP1(X0) | ~ ordinal(X1)), inference(equality_resolution, [], [f104])).
fof(f104, plain, ! [X4, X0, X5, X1] : (in(X5, sK15(X0, X1)) | ~ in(X5, X0) | ~ ordinal(X5) | ~ (X4 = X5) | ~ in(X4, succ(X1)) | sP1(X0) | ~ ordinal(X1)), inference(equality_resolution, [], [f102])).
fof(f102, plain, ! [X4, X0, X5, X3, X1] : (in(X3, sK15(X0, X1)) | ~ in(X5, X0) | ~ (X3 = X5) | ~ ordinal(X5) | ~ (X3 = X4) | ~ in(X4, succ(X1)) | sP1(X0) | ~ ordinal(X1)), inference(cnf_transformation, [], [f59])).
fof(f1576, plain, (spl18_68 | spl18_51 | ~ spl18_56 | ~ spl18_72), inference(avatar_split_clause, [], [f1575, f1551, f1401, f1333, f1510])).
fof(f1575, plain, (ordinal(sK4(sK15(sK2, sK3))) | (spl18_51 | ~ spl18_56 | ~ spl18_72)), inference(forward_demodulation, [], [f1574, f1403])).
fof(f1574, plain, (ordinal(sK17(sK2, sK4(sK15(sK2, sK3)))) | (spl18_51 | ~ spl18_72)), inference(subsumption_resolution, [], [f1573, f60])).
fof(f1573, plain, (ordinal(sK17(sK2, sK4(sK15(sK2, sK3)))) | ~ ordinal(sK3) | (spl18_51 | ~ spl18_72)), inference(subsumption_resolution, [], [f1567, f1334])).
fof(f1567, plain, (ordinal(sK17(sK2, sK4(sK15(sK2, sK3)))) | sP1(sK2) | ~ ordinal(sK3) | ~ spl18_72), inference(resolution, [], [f1553, f99])).
fof(f1572, plain, (spl18_67 | spl18_51 | ~ spl18_56 | ~ spl18_72), inference(avatar_split_clause, [], [f1571, f1551, f1401, f1333, f1503])).
fof(f1571, plain, (in(sK4(sK15(sK2, sK3)), sK2) | (spl18_51 | ~ spl18_56 | ~ spl18_72)), inference(forward_demodulation, [], [f1570, f1403])).
fof(f1570, plain, (in(sK17(sK2, sK4(sK15(sK2, sK3))), sK2) | (spl18_51 | ~ spl18_72)), inference(subsumption_resolution, [], [f1569, f60])).
fof(f1569, plain, (in(sK17(sK2, sK4(sK15(sK2, sK3))), sK2) | ~ ordinal(sK3) | (spl18_51 | ~ spl18_72)), inference(subsumption_resolution, [], [f1565, f1334])).
fof(f1565, plain, (in(sK17(sK2, sK4(sK15(sK2, sK3))), sK2) | sP1(sK2) | ~ ordinal(sK3) | ~ spl18_72), inference(resolution, [], [f1553, f101])).
fof(f1555, plain, (spl18_72 | spl18_67 | ~ spl18_55), inference(avatar_split_clause, [], [f1499, f1397, f1503, f1551])).
fof(f1499, plain, (in(sK4(sK15(sK2, sK3)), sK2) | in(sK4(sK15(sK2, sK3)), sK15(sK2, sK3)) | ~ spl18_55), inference(superposition, [], [f64, f1399])).
fof(f64, plain, ! [X2] : (in(sK5(X2), sK2) | in(sK4(X2), X2)), inference(cnf_transformation, [], [f36])).
fof(f1513, plain, (spl18_56 | spl18_68 | spl18_51 | ~ spl18_55), inference(avatar_split_clause, [], [f1508, f1397, f1333, f1510, f1401])).
fof(f1508, plain, (ordinal(sK4(sK15(sK2, sK3))) | (sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | (spl18_51 | ~ spl18_55)), inference(subsumption_resolution, [], [f1507, f60])).
fof(f1507, plain, (ordinal(sK4(sK15(sK2, sK3))) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | (spl18_51 | ~ spl18_55)), inference(subsumption_resolution, [], [f1489, f1334])).
fof(f1489, plain, (ordinal(sK4(sK15(sK2, sK3))) | sP1(sK2) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | ~ spl18_55), inference(superposition, [], [f151, f1399])).
fof(f151, plain, ! [X8, X9] : (ordinal(sK5(sK15(X8, X9))) | sP1(X8) | ~ ordinal(X9) | (sK4(sK15(X8, X9)) = sK17(X8, sK4(sK15(X8, X9))))), inference(resolution, [], [f100, f62])).
fof(f100, plain, ! [X0, X3, X1] : (~ in(X3, sK15(X0, X1)) | (sK17(X0, X3) = X3) | sP1(X0) | ~ ordinal(X1)), inference(cnf_transformation, [], [f59])).
fof(f1506, plain, (spl18_56 | spl18_67 | spl18_51 | ~ spl18_55), inference(avatar_split_clause, [], [f1501, f1397, f1333, f1503, f1401])).
fof(f1501, plain, (in(sK4(sK15(sK2, sK3)), sK2) | (sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | (spl18_51 | ~ spl18_55)), inference(subsumption_resolution, [], [f1500, f60])).
fof(f1500, plain, (in(sK4(sK15(sK2, sK3)), sK2) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | (spl18_51 | ~ spl18_55)), inference(subsumption_resolution, [], [f1488, f1334])).
fof(f1488, plain, (in(sK4(sK15(sK2, sK3)), sK2) | sP1(sK2) | ~ ordinal(sK3) | (sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | ~ spl18_55), inference(superposition, [], [f150, f1399])).
fof(f150, plain, ! [X6, X7] : (in(sK5(sK15(X6, X7)), sK2) | sP1(X6) | ~ ordinal(X7) | (sK4(sK15(X6, X7)) = sK17(X6, sK4(sK15(X6, X7))))), inference(resolution, [], [f100, f64])).
fof(f1404, plain, (spl18_55 | spl18_56 | spl18_51), inference(avatar_split_clause, [], [f1388, f1333, f1401, f1397])).
fof(f1388, plain, ((sK4(sK15(sK2, sK3)) = sK17(sK2, sK4(sK15(sK2, sK3)))) | (sK4(sK15(sK2, sK3)) = sK5(sK15(sK2, sK3))) | spl18_51), inference(resolution, [], [f1334, f514])).
fof(f514, plain, ! [X2] : (sP1(X2) | (sK4(sK15(X2, sK3)) = sK17(X2, sK4(sK15(X2, sK3)))) | (sK4(sK15(X2, sK3)) = sK5(sK15(X2, sK3)))), inference(resolution, [], [f148, f60])).
fof(f148, plain, ! [X2, X3] : (~ ordinal(X3) | sP1(X2) | (sK4(sK15(X2, X3)) = sK17(X2, sK4(sK15(X2, X3)))) | (sK4(sK15(X2, X3)) = sK5(sK15(X2, X3)))), inference(resolution, [], [f100, f63])).
fof(f1381, plain, ~ spl18_51, inference(avatar_contradiction_clause, [], [f1380])).
fof(f1380, plain, ($false | ~ spl18_51), inference(subsumption_resolution, [], [f1379, f1335])).
fof(f1335, plain, (sP1(sK2) | ~ spl18_51), inference(avatar_component_clause, [], [f1333])).
fof(f1379, plain, (~ sP1(sK2) | ~ spl18_51), inference(subsumption_resolution, [], [f1378, f1371])).
fof(f1371, plain, ((sK11(sK2) = sK10(sK2)) | ~ spl18_51), inference(resolution, [], [f1335, f87])).
fof(f87, plain, ! [X0] : (~ sP1(X0) | (sK10(X0) = sK11(X0))), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : ((~ (sK11(X0) = sK12(X0)) & (in(sK13(X0), X0) & (sK12(X0) = sK13(X0)) & ordinal(sK13(X0))) & (sK10(X0) = sK12(X0)) & sP0(X0, sK11(X0)) & (sK10(X0) = sK11(X0))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12, sK13])], [f46, f48, f47])).
fof(f47, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & ? [X4] : (in(X4, X0) & (X3 = X4) & ordinal(X4)) & (X1 = X3) & sP0(X0, X2) & (X1 = X2)) => (~ (sK11(X0) = sK12(X0)) & ? [X4] : (in(X4, X0) & (sK12(X0) = X4) & ordinal(X4)) & (sK10(X0) = sK12(X0)) & sP0(X0, sK11(X0)) & (sK10(X0) = sK11(X0)))), introduced(choice_axiom, [])).
fof(f48, plain, ! [X0] : (? [X4] : (in(X4, X0) & (sK12(X0) = X4) & ordinal(X4)) => (in(sK13(X0), X0) & (sK12(X0) = sK13(X0)) & ordinal(sK13(X0)))), introduced(choice_axiom, [])).
fof(f46, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & ? [X4] : (in(X4, X0) & (X3 = X4) & ordinal(X4)) & (X1 = X3) & sP0(X0, X2) & (X1 = X2)) | ~ sP1(X0)), inference(rectify, [], [f45])).
fof(f45, plain, ! [X0] : (? [X2, X3, X4] : (~ (X3 = X4) & ? [X5] : (in(X5, X0) & (X4 = X5) & ordinal(X5)) & (X2 = X4) & sP0(X0, X3) & (X2 = X3)) | ~ sP1(X0)), inference(nnf_transformation, [], [f28])).
fof(f1378, plain, (~ (sK11(sK2) = sK10(sK2)) | ~ sP1(sK2) | ~ spl18_51), inference(superposition, [], [f93, f1370])).
fof(f1370, plain, ((sK12(sK2) = sK10(sK2)) | ~ spl18_51), inference(resolution, [], [f1335, f89])).
fof(f89, plain, ! [X0] : (~ sP1(X0) | (sK10(X0) = sK12(X0))), inference(cnf_transformation, [], [f49])).
fof(f93, plain, ! [X0] : (~ (sK11(X0) = sK12(X0)) | ~ sP1(X0)), inference(cnf_transformation, [], [f49])).