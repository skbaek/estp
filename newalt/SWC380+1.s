fof(f196584, plain, $false, inference(avatar_sat_refutation, [], [f612, f618, f2966, f3317, f3430, f3442, f3760, f3836, f4197, f61955, f69302, f196577])).
fof(f196577, plain, (~ spl60_114 | spl60_122 | ~ spl60_144 | ~ spl60_1205), inference(avatar_contradiction_clause, [], [f196576])).
fof(f196576, plain, ($false | (~ spl60_114 | spl60_122 | ~ spl60_144 | ~ spl60_1205)), inference(subsumption_resolution, [], [f196575, f3349])).
fof(f3349, plain, (~ (nil = sK50(sK56)) | spl60_122), inference(avatar_component_clause, [], [f3348])).
fof(f3348, plain, (spl60_122 <=> (nil = sK50(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_122])])).
fof(f196575, plain, ((nil = sK50(sK56)) | (~ spl60_114 | ~ spl60_144 | ~ spl60_1205)), inference(subsumption_resolution, [], [f196574, f3483])).
fof(f3483, plain, (ssItem(hd(sK56)) | ~ spl60_144), inference(avatar_component_clause, [], [f3482])).
fof(f3482, plain, (spl60_144 <=> ssItem(hd(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_144])])).
fof(f196574, plain, (~ ssItem(hd(sK56)) | (nil = sK50(sK56)) | (~ spl60_114 | ~ spl60_1205)), inference(subsumption_resolution, [], [f196560, f448])).
fof(f448, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax17)).
fof(f196560, plain, (~ ssList(nil) | ~ ssItem(hd(sK56)) | (nil = sK50(sK56)) | (~ spl60_114 | ~ spl60_1205)), inference(trivial_inequality_removal, [], [f196543])).
fof(f196543, plain, (~ (sK56 = sK56) | ~ ssList(nil) | ~ ssItem(hd(sK56)) | (nil = sK50(sK56)) | (~ spl60_114 | ~ spl60_1205)), inference(superposition, [], [f3315, f28811])).
fof(f28811, plain, ((sK56 = cons(hd(sK56), nil)) | ~ spl60_1205), inference(avatar_component_clause, [], [f28809])).
fof(f28809, plain, (spl60_1205 <=> (sK56 = cons(hd(sK56), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_1205])])).
fof(f3315, plain, (! [X4, X5] : (~ (sK56 = cons(X4, X5)) | ~ ssList(X5) | ~ ssItem(X4) | (sK50(sK56) = X5)) | ~ spl60_114), inference(avatar_component_clause, [], [f3314])).
fof(f3314, plain, (spl60_114 <=> ! [X5, X4] : (~ (sK56 = cons(X4, X5)) | ~ ssList(X5) | ~ ssItem(X4) | (sK50(sK56) = X5))), introduced(avatar_definition, [new_symbols(naming, [spl60_114])])).
fof(f69302, plain, (~ spl60_1 | ~ spl60_111 | ~ spl60_140 | ~ spl60_193), inference(avatar_contradiction_clause, [], [f69301])).
fof(f69301, plain, ($false | (~ spl60_1 | ~ spl60_111 | ~ spl60_140 | ~ spl60_193)), inference(subsumption_resolution, [], [f69300, f68550])).
fof(f68550, plain, (memberP(sK57, sK51(sK56)) | (~ spl60_1 | ~ spl60_111 | ~ spl60_140)), inference(subsumption_resolution, [], [f68519, f3301])).
fof(f3301, plain, (ssItem(sK51(sK56)) | ~ spl60_111), inference(avatar_component_clause, [], [f3300])).
fof(f3300, plain, (spl60_111 <=> ssItem(sK51(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_111])])).
fof(f68519, plain, (memberP(sK57, sK51(sK56)) | ~ ssItem(sK51(sK56)) | (~ spl60_1 | ~ spl60_140)), inference(resolution, [], [f3429, f4059])).
fof(f4059, plain, (! [X1] : (~ memberP(sK56, X1) | memberP(sK57, X1) | ~ ssItem(X1)) | ~ spl60_1), inference(subsumption_resolution, [], [f4058, f615])).
fof(f615, plain, ssList(sK56), inference(forward_demodulation, [], [f563, f566])).
fof(f566, plain, (sK56 = sK58), inference(cnf_transformation, [], [f355])).
fof(f355, plain, ((((((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK59, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57, sK58, sK59])], [f234, f354, f353, f352, f351])).
fof(f351, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK56)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK56)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, X2, sK57, sK56)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, X2, sK57, sK56)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) => (((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK59, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2, X1, X0] : ((? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (~ memberP(X1, X6) | ~ (cons(X6, nil) = X0) | ~ ssItem(X6)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2, X1, X0] : (sP6(X3, X2, X1, X0) <=> (? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (~ memberP(X1, X6) | ~ (cons(X6, nil) = X0) | ~ ssItem(X6)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (~ memberP(X1, X6) | ~ (cons(X6, nil) = X0) | ~ ssItem(X6)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : (((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2)) & ssList(X5)) & ssItem(X4)) & ! [X6] : (~ memberP(X1, X6) | ~ (cons(X6, nil) = X0) | ~ ssItem(X6)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(cons(X4, nil), X5) = X3) | ~ (cons(X4, nil) = X2)))) | ? [X6] : (memberP(X1, X6) & (cons(X6, nil) = X0) & ssItem(X6)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => (~ (app(cons(X5, nil), X6) = X3) | ~ (cons(X5, nil) = X2)))) | ? [X4] : (memberP(X1, X4) & (cons(X4, nil) = X0) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => (~ (app(cons(X5, nil), X6) = X3) | ~ (cons(X5, nil) = X2)))) | ? [X4] : (memberP(X1, X4) & (cons(X4, nil) = X0) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', co1)).
fof(f563, plain, ssList(sK58), inference(cnf_transformation, [], [f355])).
fof(f4058, plain, (! [X1] : (memberP(sK57, X1) | ~ memberP(sK56, X1) | ~ ssList(sK56) | ~ ssItem(X1)) | ~ spl60_1), inference(subsumption_resolution, [], [f4042, f989])).
fof(f989, plain, (ssList(sK55(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f558, f621])).
fof(f621, plain, (sP6(sK57, sK56, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f620, f565])).
fof(f565, plain, (sK57 = sK59), inference(cnf_transformation, [], [f355])).
fof(f620, plain, (sP6(sK59, sK56, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f607, f566])).
fof(f607, plain, (sP6(sK59, sK58, sK57, sK56) | ~ spl60_1), inference(avatar_component_clause, [], [f605])).
fof(f605, plain, (spl60_1 <=> sP6(sK59, sK58, sK57, sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_1])])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X0, X1))), inference(cnf_transformation, [], [f350])).
fof(f350, plain, ! [X0, X1, X2, X3] : (((((app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) & ! [X6] : (~ memberP(X2, X6) | ~ (cons(X6, nil) = X3) | ~ ssItem(X6)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55])], [f347, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X0) & (cons(X4, nil) = X1) & ssList(X5)) & ssItem(X4)) => (? [X5] : ((app(cons(sK54(X0, X1), nil), X5) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X5)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X5] : ((app(cons(sK54(X0, X1), nil), X5) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X5)) => ((app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : ((? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X0) & (cons(X4, nil) = X1) & ssList(X5)) & ssItem(X4)) & ! [X6] : (~ memberP(X2, X6) | ~ (cons(X6, nil) = X3) | ~ ssItem(X6)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2, X1, X0] : ((? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (~ memberP(X1, X6) | ~ (cons(X6, nil) = X0) | ~ ssItem(X6)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(nnf_transformation, [], [f233])).
fof(f4042, plain, (! [X1] : (memberP(sK57, X1) | ~ memberP(sK56, X1) | ~ ssList(sK55(sK57, sK56)) | ~ ssList(sK56) | ~ ssItem(X1)) | ~ spl60_1), inference(superposition, [], [f473, f3045])).
fof(f3045, plain, ((sK57 = app(sK56, sK55(sK57, sK56))) | ~ spl60_1), inference(backward_demodulation, [], [f3040, f3041])).
fof(f3041, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f621, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f350])).
fof(f3040, plain, ((sK57 = app(cons(sK54(sK57, sK56), nil), sK55(sK57, sK56))) | ~ spl60_1), inference(resolution, [], [f621, f560])).
fof(f560, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X0)), inference(cnf_transformation, [], [f350])).
fof(f473, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X1, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f326])).
fof(f326, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & (memberP(X2, X0) | memberP(X1, X0) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f325])).
fof(f325, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & ((memberP(X2, X0) | memberP(X1, X0)) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax36)).
fof(f3429, plain, (memberP(sK56, sK51(sK56)) | ~ spl60_140), inference(avatar_component_clause, [], [f3427])).
fof(f3427, plain, (spl60_140 <=> memberP(sK56, sK51(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_140])])).
fof(f69300, plain, (~ memberP(sK57, sK51(sK56)) | (~ spl60_1 | ~ spl60_193)), inference(resolution, [], [f3835, f621])).
fof(f3835, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, X2, sK56) | ~ memberP(X2, sK51(sK56))) | ~ spl60_193), inference(avatar_component_clause, [], [f3834])).
fof(f3834, plain, (spl60_193 <=> ! [X1, X0, X2] : (~ sP6(X0, X1, X2, sK56) | ~ memberP(X2, sK51(sK56)))), introduced(avatar_definition, [new_symbols(naming, [spl60_193])])).
fof(f61955, plain, (spl60_1205 | ~ spl60_1), inference(avatar_split_clause, [], [f56262, f605, f28809])).
fof(f56262, plain, ((sK56 = cons(hd(sK56), nil)) | ~ spl60_1), inference(forward_demodulation, [], [f56254, f5082])).
fof(f5082, plain, ((sK54(sK57, sK56) = hd(sK56)) | ~ spl60_1), inference(forward_demodulation, [], [f4965, f3041])).
fof(f4965, plain, ((sK54(sK57, sK56) = hd(cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f1051, f988])).
fof(f988, plain, (ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f557, f621])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X0, X1))), inference(cnf_transformation, [], [f350])).
fof(f1051, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f457, f448])).
fof(f457, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax23)).
fof(f56254, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f621, f559])).
fof(f4197, plain, (spl60_30 | spl60_144), inference(avatar_contradiction_clause, [], [f4196])).
fof(f4196, plain, ($false | (spl60_30 | spl60_144)), inference(subsumption_resolution, [], [f4195, f615])).
fof(f4195, plain, (~ ssList(sK56) | (spl60_30 | spl60_144)), inference(subsumption_resolution, [], [f4194, f1164])).
fof(f1164, plain, (~ (nil = sK56) | spl60_30), inference(avatar_component_clause, [], [f1163])).
fof(f1163, plain, (spl60_30 <=> (nil = sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_30])])).
fof(f4194, plain, ((nil = sK56) | ~ ssList(sK56) | spl60_144), inference(resolution, [], [f3484, f456])).
fof(f456, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax22)).
fof(f3484, plain, (~ ssItem(hd(sK56)) | spl60_144), inference(avatar_component_clause, [], [f3482])).
fof(f3836, plain, (~ spl60_111 | spl60_193 | spl60_30 | ~ spl60_122), inference(avatar_split_clause, [], [f3721, f3348, f1163, f3834, f3300])).
fof(f3721, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, X2, sK56) | ~ ssItem(sK51(sK56)) | ~ memberP(X2, sK51(sK56))) | (spl60_30 | ~ spl60_122)), inference(superposition, [], [f596, f3695])).
fof(f3695, plain, ((sK56 = cons(sK51(sK56), nil)) | (spl60_30 | ~ spl60_122)), inference(backward_demodulation, [], [f3034, f3350])).
fof(f3350, plain, ((nil = sK50(sK56)) | ~ spl60_122), inference(avatar_component_clause, [], [f3348])).
fof(f3034, plain, ((sK56 = cons(sK51(sK56), sK50(sK56))) | spl60_30), inference(subsumption_resolution, [], [f2994, f1164])).
fof(f2994, plain, ((nil = sK56) | (sK56 = cons(sK51(sK56), sK50(sK56)))), inference(resolution, [], [f615, f454])).
fof(f454, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(sK51(X0), sK50(X0)) = X0)), inference(cnf_transformation, [], [f322])).
fof(f322, plain, ! [X0] : ((((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0))) & ssList(sK50(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK50, sK51])], [f125, f321, f320])).
fof(f320, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) => (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) & ssList(sK50(X0)))), introduced(choice_axiom, [])).
fof(f321, plain, ! [X0] : (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) => ((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0)))), introduced(choice_axiom, [])).
fof(f125, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0] : ((? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (ssList(X0) => (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax20)).
fof(f596, plain, ! [X6, X2, X0, X1] : (~ sP6(X0, X1, X2, cons(X6, nil)) | ~ ssItem(X6) | ~ memberP(X2, X6)), inference(equality_resolution, [], [f556])).
fof(f556, plain, ! [X6, X2, X0, X3, X1] : (~ memberP(X2, X6) | ~ (cons(X6, nil) = X3) | ~ ssItem(X6) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f350])).
fof(f3760, plain, (spl60_30 | spl60_111), inference(avatar_contradiction_clause, [], [f3759])).
fof(f3759, plain, ($false | (spl60_30 | spl60_111)), inference(subsumption_resolution, [], [f3758, f615])).
fof(f3758, plain, (~ ssList(sK56) | (spl60_30 | spl60_111)), inference(subsumption_resolution, [], [f3757, f1164])).
fof(f3757, plain, ((nil = sK56) | ~ ssList(sK56) | spl60_111), inference(resolution, [], [f3302, f453])).
fof(f453, plain, ! [X0] : (ssItem(sK51(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f3302, plain, (~ ssItem(sK51(sK56)) | spl60_111), inference(avatar_component_clause, [], [f3300])).
fof(f3442, plain, (spl60_30 | spl60_110), inference(avatar_contradiction_clause, [], [f3441])).
fof(f3441, plain, ($false | (spl60_30 | spl60_110)), inference(subsumption_resolution, [], [f3440, f615])).
fof(f3440, plain, (~ ssList(sK56) | (spl60_30 | spl60_110)), inference(subsumption_resolution, [], [f3439, f1164])).
fof(f3439, plain, ((nil = sK56) | ~ ssList(sK56) | spl60_110), inference(resolution, [], [f3298, f452])).
fof(f452, plain, ! [X0] : (ssList(sK50(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f3298, plain, (~ ssList(sK50(sK56)) | spl60_110), inference(avatar_component_clause, [], [f3296])).
fof(f3296, plain, (spl60_110 <=> ssList(sK50(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_110])])).
fof(f3430, plain, (~ spl60_111 | ~ spl60_110 | spl60_140 | spl60_30), inference(avatar_split_clause, [], [f3292, f1163, f3427, f3296, f3300])).
fof(f3292, plain, (memberP(sK56, sK51(sK56)) | ~ ssList(sK50(sK56)) | ~ ssItem(sK51(sK56)) | spl60_30), inference(superposition, [], [f600, f3034])).
fof(f600, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1)), inference(duplicate_literal_removal, [], [f584])).
fof(f584, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X1)), inference(equality_resolution, [], [f476])).
fof(f476, plain, ! [X2, X0, X1] : (memberP(cons(X1, X2), X0) | ~ (X0 = X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f328])).
fof(f328, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & (memberP(X2, X0) | (X0 = X1) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f327])).
fof(f327, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & ((memberP(X2, X0) | (X0 = X1)) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => (memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax37)).
fof(f3317, plain, (~ spl60_110 | ~ spl60_111 | spl60_114 | spl60_30), inference(avatar_split_clause, [], [f3266, f1163, f3314, f3300, f3296])).
fof(f3266, plain, (! [X6, X7] : (~ (sK56 = cons(X6, X7)) | (sK50(sK56) = X7) | ~ ssItem(sK51(sK56)) | ~ ssItem(X6) | ~ ssList(sK50(sK56)) | ~ ssList(X7)) | spl60_30), inference(superposition, [], [f451, f3034])).
fof(f451, plain, ! [X2, X0, X3, X1] : (~ (cons(X2, X0) = cons(X3, X1)) | (X0 = X1) | ~ ssItem(X3) | ~ ssItem(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((X0 = X1) & (X2 = X3)) | ~ (cons(X2, X0) = cons(X3, X1)) | ~ ssItem(X3)) | ~ ssItem(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f122])).
fof(f122, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((((X0 = X1) & (X2 = X3)) | ~ (cons(X2, X0) = cons(X3, X1))) | ~ ssItem(X3)) | ~ ssItem(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssItem(X3) => ((cons(X2, X0) = cons(X3, X1)) => ((X0 = X1) & (X2 = X3))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax19)).
fof(f2966, plain, (~ spl60_1 | ~ spl60_30), inference(avatar_contradiction_clause, [], [f2965])).
fof(f2965, plain, ($false | (~ spl60_1 | ~ spl60_30)), inference(subsumption_resolution, [], [f2964, f448])).
fof(f2964, plain, (~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(subsumption_resolution, [], [f2951, f1425])).
fof(f1425, plain, (ssItem(sK54(sK57, nil)) | (~ spl60_1 | ~ spl60_30)), inference(forward_demodulation, [], [f988, f1165])).
fof(f1165, plain, ((nil = sK56) | ~ spl60_30), inference(avatar_component_clause, [], [f1163])).
fof(f2951, plain, (~ ssItem(sK54(sK57, nil)) | ~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(trivial_inequality_removal, [], [f2934])).
fof(f2934, plain, (~ (nil = nil) | ~ ssItem(sK54(sK57, nil)) | ~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(superposition, [], [f455, f1590])).
fof(f1590, plain, ((nil = cons(sK54(sK57, nil), nil)) | (~ spl60_1 | ~ spl60_30)), inference(resolution, [], [f559, f1426])).
fof(f1426, plain, (sP6(sK57, nil, sK57, nil) | (~ spl60_1 | ~ spl60_30)), inference(forward_demodulation, [], [f621, f1165])).
fof(f455, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC380+1.p', ax21)).
fof(f618, plain, spl60_2, inference(avatar_contradiction_clause, [], [f617])).
fof(f617, plain, ($false | spl60_2), inference(subsumption_resolution, [], [f616, f613])).
fof(f613, plain, neq(sK57, nil), inference(subsumption_resolution, [], [f567, f555])).
fof(f555, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X2, nil)), inference(cnf_transformation, [], [f350])).
fof(f567, plain, (neq(sK57, nil) | sP6(sK59, sK58, sK57, sK56)), inference(cnf_transformation, [], [f355])).
fof(f616, plain, (~ neq(sK57, nil) | spl60_2), inference(forward_demodulation, [], [f611, f565])).
fof(f611, plain, (~ neq(sK59, nil) | spl60_2), inference(avatar_component_clause, [], [f609])).
fof(f609, plain, (spl60_2 <=> neq(sK59, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_2])])).
fof(f612, plain, (spl60_1 | ~ spl60_2), inference(avatar_split_clause, [], [f568, f609, f605])).
fof(f568, plain, (~ neq(sK59, nil) | sP6(sK59, sK58, sK57, sK56)), inference(cnf_transformation, [], [f355])).