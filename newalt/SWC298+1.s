fof(f1308575, plain, $false, inference(avatar_sat_refutation, [], [f626, f631, f1708, f1720, f2251, f2398, f2650, f4050, f4088, f4201, f4260, f5868, f5873, f5944, f7217, f13341, f17345, f18067, f18340, f18785, f19115, f19572, f23028, f32860, f42305, f42331, f54205, f54549, f55739, f61226, f65043, f70291, f83393, f89542, f91496, f98019, f112908, f157323, f157336, f162532, f168149, f196152, f201029, f223614, f249932, f249936, f257780, f263593, f264934, f305087, f314315, f322407, f335236, f338238, f342975, f343101, f346855, f366033, f389310, f568770, f587566, f634890, f635219, f674770, f677347, f688494, f692660, f692671, f692683, f695711, f701549, f702994, f717443, f717506, f717549, f717674, f744445, f744463, f744466, f747940, f752275, f820619, f1245066, f1245610, f1263522, f1263841, f1267573, f1272250, f1275689, f1295788])).
fof(f1295788, plain, (~ spl63_347 | ~ spl63_3715 | spl63_5187), inference(avatar_contradiction_clause, [], [f1295787])).
fof(f1295787, plain, ($false | (~ spl63_347 | ~ spl63_3715 | spl63_5187)), inference(subsumption_resolution, [], [f1295786, f99932])).
fof(f99932, plain, (~ frontsegP(nil, sK60) | spl63_5187), inference(avatar_component_clause, [], [f99930])).
fof(f99930, plain, (spl63_5187 <=> frontsegP(nil, sK60)), introduced(avatar_definition, [new_symbols(naming, [spl63_5187])])).
fof(f1295786, plain, (frontsegP(nil, sK60) | (~ spl63_347 | ~ spl63_3715)), inference(forward_demodulation, [], [f72471, f5862])).
fof(f5862, plain, ((nil = sK53) | ~ spl63_347), inference(avatar_component_clause, [], [f5860])).
fof(f5860, plain, (spl63_347 <=> (nil = sK53)), introduced(avatar_definition, [new_symbols(naming, [spl63_347])])).
fof(f72471, plain, (frontsegP(sK53, sK60) | ~ spl63_3715), inference(avatar_component_clause, [], [f72470])).
fof(f72470, plain, (spl63_3715 <=> frontsegP(sK53, sK60)), introduced(avatar_definition, [new_symbols(naming, [spl63_3715])])).
fof(f1275689, plain, (spl63_2 | ~ spl63_347), inference(avatar_split_clause, [], [f1272445, f5860, f613])).
fof(f613, plain, (spl63_2 <=> (nil = sK55)), introduced(avatar_definition, [new_symbols(naming, [spl63_2])])).
fof(f1272445, plain, ((nil = sK55) | ~ spl63_347), inference(backward_demodulation, [], [f559, f5862])).
fof(f559, plain, (sK53 = sK55), inference(cnf_transformation, [], [f354])).
fof(f354, plain, (((((((nil = sK55) & (nil = sK56)) | (! [X5] : (~ leq(X5, sK57) | ~ memberP(sK56, X5) | (sK57 = X5) | ~ ssItem(X5)) & memberP(sK56, sK57) & (sK55 = cons(sK57, nil)) & ssItem(sK57))) & (((((lt(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)) & ssList(sK62)) & ssList(sK61)) & ssList(sK60)) & ssItem(sK59)) & ssItem(sK58)) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56)) & ssList(sK55)) & ssList(sK54)) & ssList(sK53)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK53, sK54, sK55, sK56, sK57, sK58, sK59, sK60, sK61, sK62])], [f223, f353, f352, f351, f350, f349, f348, f347, f346, f345, f344])).
fof(f344, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = X0) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK53))), introduced(choice_axiom, [])).
fof(f345, plain, (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK54))), introduced(choice_axiom, [])).
fof(f346, plain, (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((nil = sK55) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) & ssList(sK55))), introduced(choice_axiom, [])).
fof(f347, plain, (? [X3] : ((((nil = sK55) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) => ((((nil = sK55) & (nil = sK56)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(sK56, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(sK56, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f348, plain, (? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(sK56, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(sK56, X4) & (cons(X4, nil) = sK55) & ssItem(X4)) => (! [X5] : (~ leq(X5, sK57) | ~ memberP(sK56, X5) | (sK57 = X5) | ~ ssItem(X5)) & memberP(sK56, sK57) & (sK55 = cons(sK57, nil)) & ssItem(sK57))), introduced(choice_axiom, [])).
fof(f349, plain, (? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) => (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(X7, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(sK58))), introduced(choice_axiom, [])).
fof(f350, plain, (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(X7, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) => (? [X8] : (? [X9] : (? [X10] : (lt(sK59, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(sK59))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X8] : (? [X9] : (? [X10] : (lt(sK59, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) => (? [X9] : (? [X10] : (lt(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X9] : (? [X10] : (lt(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) => (? [X10] : (lt(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(sK61))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X10] : (lt(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X10)) & ssList(X10)) => (lt(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)) & ssList(sK62))), introduced(choice_axiom, [])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : (lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = X0) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((((nil = X2) & (nil = X3)) | ? [X4] : ((! [X5] : (~ leq(X5, X4) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2)) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((lt(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = X0)) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => (? [X5] : (leq(X5, X4) & memberP(X3, X5) & ~ (X4 = X5) & ssItem(X5)) | ~ memberP(X3, X4) | ~ (cons(X4, nil) = X2)))) | ! [X6] : (ssItem(X6) => ! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => ! [X9] : (ssList(X9) => ! [X10] : (ssList(X10) => (~ lt(X7, X6) | ~ (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = X0))))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X9] : (ssItem(X9) => (? [X10] : (leq(X10, X9) & memberP(X3, X10) & ~ (X9 = X10) & ssItem(X10)) | ~ memberP(X3, X9) | ~ (cons(X9, nil) = X2)))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ! [X8] : (ssList(X8) => (~ lt(X5, X4) | ~ (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0))))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X9] : (ssItem(X9) => (? [X10] : (leq(X10, X9) & memberP(X3, X10) & ~ (X9 = X10) & ssItem(X10)) | ~ memberP(X3, X9) | ~ (cons(X9, nil) = X2)))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ! [X8] : (ssList(X8) => (~ lt(X5, X4) | ~ (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0))))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', co1)).
fof(f1272250, plain, (~ spl63_3713 | ~ spl63_10054 | spl63_11977 | ~ spl63_12129 | ~ spl63_13530), inference(avatar_contradiction_clause, [], [f1272249])).
fof(f1272249, plain, ($false | (~ spl63_3713 | ~ spl63_10054 | spl63_11977 | ~ spl63_12129 | ~ spl63_13530)), inference(subsumption_resolution, [], [f1271511, f1270243])).
fof(f1270243, plain, (~ frontsegP(nil, sK53) | spl63_11977), inference(subsumption_resolution, [], [f1270242, f560])).
fof(f560, plain, ssItem(sK58), inference(cnf_transformation, [], [f354])).
fof(f1270242, plain, (~ frontsegP(nil, sK53) | ~ ssItem(sK58) | spl63_11977), inference(subsumption_resolution, [], [f1270241, f447])).
fof(f447, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax17)).
fof(f1270241, plain, (~ frontsegP(nil, sK53) | ~ ssList(nil) | ~ ssItem(sK58) | spl63_11977), inference(subsumption_resolution, [], [f1009576, f641])).
fof(f641, plain, ssList(sK53), inference(forward_demodulation, [], [f556, f559])).
fof(f556, plain, ssList(sK55), inference(cnf_transformation, [], [f354])).
fof(f1009576, plain, (~ frontsegP(nil, sK53) | ~ ssList(sK53) | ~ ssList(nil) | ~ ssItem(sK58) | spl63_11977), inference(resolution, [], [f223613, f604])).
fof(f604, plain, ! [X2, X3, X1] : (frontsegP(cons(X1, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1)), inference(duplicate_literal_removal, [], [f591])).
fof(f591, plain, ! [X2, X3, X1] : (frontsegP(cons(X1, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X1)), inference(equality_resolution, [], [f485])).
fof(f485, plain, ! [X2, X0, X3, X1] : (frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f328])).
fof(f328, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1)) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f327])).
fof(f327, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | (~ frontsegP(X2, X3) | ~ (X0 = X1))) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax44)).
fof(f223613, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | spl63_11977), inference(avatar_component_clause, [], [f223611])).
fof(f223611, plain, (spl63_11977 <=> frontsegP(cons(sK58, nil), cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_11977])])).
fof(f1271511, plain, (frontsegP(nil, sK53) | (~ spl63_3713 | ~ spl63_10054 | ~ spl63_12129 | ~ spl63_13530)), inference(backward_demodulation, [], [f1246399, f1270991])).
fof(f1270991, plain, ((nil = app(sK53, cons(sK58, nil))) | (~ spl63_3713 | ~ spl63_13530)), inference(forward_demodulation, [], [f257542, f72453])).
fof(f72453, plain, ((sK53 = sK60) | ~ spl63_3713), inference(avatar_component_clause, [], [f72452])).
fof(f72452, plain, (spl63_3713 <=> (sK53 = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl63_3713])])).
fof(f257542, plain, ((nil = app(sK60, cons(sK58, nil))) | ~ spl63_13530), inference(avatar_component_clause, [], [f257540])).
fof(f257540, plain, (spl63_13530 <=> (nil = app(sK60, cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_13530])])).
fof(f1246399, plain, (frontsegP(app(sK53, cons(sK58, nil)), sK53) | (~ spl63_3713 | ~ spl63_10054 | ~ spl63_12129)), inference(backward_demodulation, [], [f820625, f798813])).
fof(f798813, plain, ((app(sK53, cons(sK58, nil)) = app(app(sK53, cons(sK58, nil)), nil)) | (~ spl63_3713 | ~ spl63_12129)), inference(resolution, [], [f750432, f540])).
fof(f540, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax84)).
fof(f750432, plain, (ssList(app(sK53, cons(sK58, nil))) | (~ spl63_3713 | ~ spl63_12129)), inference(backward_demodulation, [], [f226258, f72453])).
fof(f226258, plain, (ssList(app(sK60, cons(sK58, nil))) | ~ spl63_12129), inference(avatar_component_clause, [], [f226257])).
fof(f226257, plain, (spl63_12129 <=> ssList(app(sK60, cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_12129])])).
fof(f820625, plain, (frontsegP(app(app(sK53, cons(sK58, nil)), nil), sK53) | (~ spl63_3713 | ~ spl63_10054)), inference(forward_demodulation, [], [f161158, f72453])).
fof(f161158, plain, (frontsegP(app(app(sK60, cons(sK58, nil)), nil), sK53) | ~ spl63_10054), inference(avatar_component_clause, [], [f161157])).
fof(f161157, plain, (spl63_10054 <=> frontsegP(app(app(sK60, cons(sK58, nil)), nil), sK53)), introduced(avatar_definition, [new_symbols(naming, [spl63_10054])])).
fof(f1267573, plain, (spl63_13522 | ~ spl63_13574 | ~ spl63_27153), inference(avatar_contradiction_clause, [], [f1267572])).
fof(f1267572, plain, ($false | (spl63_13522 | ~ spl63_13574 | ~ spl63_27153)), inference(subsumption_resolution, [], [f1267571, f259529])).
fof(f259529, plain, ((sK53 = cons(sK57, nil)) | ~ spl63_13574), inference(avatar_component_clause, [], [f259527])).
fof(f259527, plain, (spl63_13574 <=> (sK53 = cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_13574])])).
fof(f1267571, plain, (~ (sK53 = cons(sK57, nil)) | (spl63_13522 | ~ spl63_27153)), inference(forward_demodulation, [], [f257156, f1245603])).
fof(f1245603, plain, ((sK57 = sK50(sK53)) | ~ spl63_27153), inference(avatar_component_clause, [], [f1245601])).
fof(f1245601, plain, (spl63_27153 <=> (sK57 = sK50(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_27153])])).
fof(f257156, plain, (~ (sK53 = cons(sK50(sK53), nil)) | spl63_13522), inference(avatar_component_clause, [], [f257155])).
fof(f257155, plain, (spl63_13522 <=> (sK53 = cons(sK50(sK53), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_13522])])).
fof(f1263841, plain, (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | spl63_3424 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13543 | ~ spl63_13574 | ~ spl63_14403 | ~ spl63_25443), inference(avatar_contradiction_clause, [], [f1263840])).
fof(f1263840, plain, ($false | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | spl63_3424 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13543 | ~ spl63_13574 | ~ spl63_14403 | ~ spl63_25443)), inference(subsumption_resolution, [], [f1263839, f60759])).
fof(f60759, plain, (~ (sK53 = cons(sK57, sK53)) | spl63_3424), inference(avatar_component_clause, [], [f60758])).
fof(f60758, plain, (spl63_3424 <=> (sK53 = cons(sK57, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_3424])])).
fof(f1263839, plain, ((sK53 = cons(sK57, sK53)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13543 | ~ spl63_13574 | ~ spl63_14403 | ~ spl63_25443)), inference(backward_demodulation, [], [f829025, f1263838])).
fof(f1263838, plain, ((sK53 = app(sK53, sK53)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13543 | ~ spl63_13574 | ~ spl63_14403 | ~ spl63_25443)), inference(forward_demodulation, [], [f1263611, f1143622])).
fof(f1143622, plain, ((sK53 = app(cons(sK57, cons(sK58, nil)), sK61)) | ~ spl63_25443), inference(avatar_component_clause, [], [f1143621])).
fof(f1143621, plain, (spl63_25443 <=> (sK53 = app(cons(sK57, cons(sK58, nil)), sK61))), introduced(avatar_definition, [new_symbols(naming, [spl63_25443])])).
fof(f1263611, plain, ((sK53 = app(app(cons(sK57, cons(sK58, nil)), sK61), sK53)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13543 | ~ spl63_13574 | ~ spl63_14403)), inference(backward_demodulation, [], [f750793, f1263497])).
fof(f1263497, plain, ((app(sK53, cons(sK58, nil)) = cons(sK57, cons(sK58, nil))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13543 | ~ spl63_13574)), inference(backward_demodulation, [], [f1262378, f1263274])).
fof(f1263274, plain, ((sK57 = hd(sK53)) | (~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_349 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_13522 | ~ spl63_13574)), inference(forward_demodulation, [], [f1259240, f446882])).
fof(f446882, plain, ((sK57 = hd(cons(sK57, sK53))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_2212)), inference(forward_demodulation, [], [f446881, f42298])).
fof(f42298, plain, ((sK57 = sK10(sK53)) | ~ spl63_2212), inference(avatar_component_clause, [], [f42296])).
fof(f42296, plain, (spl63_2212 <=> (sK57 = sK10(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_2212])])).
fof(f446881, plain, ((sK10(sK53) = hd(cons(sK10(sK53), sK53))) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f446879, f641])).
fof(f446879, plain, ((sK10(sK53) = hd(cons(sK10(sK53), sK53))) | ~ ssList(sK53) | (~ spl63_4 | ~ spl63_5)), inference(resolution, [], [f22629, f20691])).
fof(f20691, plain, (singletonP(sK53) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20690, f630])).
fof(f630, plain, (ssItem(sK57) | ~ spl63_5), inference(avatar_component_clause, [], [f628])).
fof(f628, plain, (spl63_5 <=> ssItem(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl63_5])])).
fof(f20690, plain, (~ ssItem(sK57) | singletonP(sK53) | ~ spl63_4), inference(subsumption_resolution, [], [f20651, f641])).
fof(f20651, plain, (~ ssList(sK53) | ~ ssItem(sK57) | singletonP(sK53) | ~ spl63_4), inference(superposition, [], [f577, f20224])).
fof(f20224, plain, ((sK53 = cons(sK57, nil)) | ~ spl63_4), inference(forward_demodulation, [], [f625, f559])).
fof(f625, plain, ((sK55 = cons(sK57, nil)) | ~ spl63_4), inference(avatar_component_clause, [], [f623])).
fof(f623, plain, (spl63_4 <=> (sK55 = cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_4])])).
fof(f577, plain, ! [X1] : (~ ssList(cons(X1, nil)) | ~ ssItem(X1) | singletonP(cons(X1, nil))), inference(equality_resolution, [], [f366])).
fof(f366, plain, ! [X0, X1] : (singletonP(X0) | ~ (cons(X1, nil) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f245])).
fof(f245, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (((cons(sK10(X0), nil) = X0) & ssItem(sK10(X0))) | ~ singletonP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f243, f244])).
fof(f244, plain, ! [X0] : (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) => ((cons(sK10(X0), nil) = X0) & ssItem(sK10(X0)))), introduced(choice_axiom, [])).
fof(f243, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(rectify, [], [f242])).
fof(f242, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f101])).
fof(f101, plain, ! [X0] : ((singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (ssList(X0) => (singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax4)).
fof(f22629, plain, ! [X1] : (~ singletonP(X1) | (sK10(X1) = hd(cons(sK10(X1), sK53))) | ~ ssList(X1)), inference(resolution, [], [f5717, f364])).
fof(f364, plain, ! [X0] : (ssItem(sK10(X0)) | ~ singletonP(X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f245])).
fof(f5717, plain, ! [X0] : (~ ssItem(X0) | (hd(cons(X0, sK53)) = X0)), inference(resolution, [], [f641, f456])).
fof(f456, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax23)).
fof(f1259240, plain, ((hd(sK53) = hd(cons(sK57, sK53))) | (~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_349 | ~ spl63_3713 | ~ spl63_13522 | ~ spl63_13574)), inference(backward_demodulation, [], [f1245668, f829025])).
fof(f1245668, plain, ((hd(sK53) = hd(app(sK53, sK53))) | (spl63_347 | ~ spl63_3713 | ~ spl63_13522)), inference(backward_demodulation, [], [f1245661, f1245666])).
fof(f1245666, plain, ((app(sK53, sK53) = cons(hd(sK53), sK53)) | (spl63_347 | ~ spl63_13522)), inference(forward_demodulation, [], [f1245631, f1245629])).
fof(f1245629, plain, ((sK50(sK53) = hd(sK53)) | (spl63_347 | ~ spl63_13522)), inference(backward_demodulation, [], [f631318, f257157])).
fof(f257157, plain, ((sK53 = cons(sK50(sK53), nil)) | ~ spl63_13522), inference(avatar_component_clause, [], [f257155])).
fof(f631318, plain, ((sK50(sK53) = hd(cons(sK50(sK53), nil))) | spl63_347), inference(subsumption_resolution, [], [f631286, f5861])).
fof(f5861, plain, (~ (nil = sK53) | spl63_347), inference(avatar_component_clause, [], [f5860])).
fof(f631286, plain, ((nil = sK53) | (sK50(sK53) = hd(cons(sK50(sK53), nil)))), inference(resolution, [], [f7646, f641])).
fof(f7646, plain, ! [X16] : (~ ssList(X16) | (nil = X16) | (sK50(X16) = hd(cons(sK50(X16), nil)))), inference(resolution, [], [f1068, f452])).
fof(f452, plain, ! [X0] : (ssItem(sK50(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f320])).
fof(f320, plain, ! [X0] : ((((cons(sK50(X0), sK49(X0)) = X0) & ssItem(sK50(X0))) & ssList(sK49(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK49, sK50])], [f125, f319, f318])).
fof(f318, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) => (? [X2] : ((cons(X2, sK49(X0)) = X0) & ssItem(X2)) & ssList(sK49(X0)))), introduced(choice_axiom, [])).
fof(f319, plain, ! [X0] : (? [X2] : ((cons(X2, sK49(X0)) = X0) & ssItem(X2)) => ((cons(sK50(X0), sK49(X0)) = X0) & ssItem(sK50(X0)))), introduced(choice_axiom, [])).
fof(f125, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0] : ((? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (ssList(X0) => (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax20)).
fof(f1068, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f456, f447])).
fof(f1245631, plain, ((app(sK53, sK53) = cons(sK50(sK53), sK53)) | (spl63_347 | ~ spl63_13522)), inference(backward_demodulation, [], [f828950, f257157])).
fof(f828950, plain, ((cons(sK50(sK53), sK53) = app(cons(sK50(sK53), nil), sK53)) | spl63_347), inference(subsumption_resolution, [], [f828926, f5861])).
fof(f828926, plain, ((nil = sK53) | (cons(sK50(sK53), sK53) = app(cons(sK50(sK53), nil), sK53))), inference(resolution, [], [f25553, f641])).
fof(f25553, plain, ! [X16] : (~ ssList(X16) | (nil = X16) | (cons(sK50(X16), sK53) = app(cons(sK50(X16), nil), sK53))), inference(resolution, [], [f5724, f452])).
fof(f5724, plain, ! [X4] : (~ ssItem(X4) | (cons(X4, sK53) = app(cons(X4, nil), sK53))), inference(resolution, [], [f641, f535])).
fof(f535, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax81)).
fof(f1245661, plain, ((hd(sK53) = hd(cons(hd(sK53), sK53))) | (spl63_347 | ~ spl63_3713 | ~ spl63_13522)), inference(backward_demodulation, [], [f1245611, f1245629])).
fof(f1245611, plain, ((sK50(sK53) = hd(cons(sK50(sK53), sK53))) | (spl63_347 | ~ spl63_3713)), inference(forward_demodulation, [], [f732671, f72453])).
fof(f732671, plain, ((sK50(sK53) = hd(cons(sK50(sK53), sK60))) | spl63_347), inference(subsumption_resolution, [], [f732668, f5861])).
fof(f732668, plain, ((nil = sK53) | (sK50(sK53) = hd(cons(sK50(sK53), sK60)))), inference(resolution, [], [f4078, f641])).
fof(f4078, plain, ! [X16] : (~ ssList(X16) | (nil = X16) | (sK50(X16) = hd(cons(sK50(X16), sK60)))), inference(resolution, [], [f1092, f452])).
fof(f1092, plain, ! [X53] : (~ ssItem(X53) | (hd(cons(X53, sK60)) = X53)), inference(resolution, [], [f456, f562])).
fof(f562, plain, ssList(sK60), inference(cnf_transformation, [], [f354])).
fof(f1262378, plain, ((app(sK53, cons(sK58, nil)) = cons(hd(sK53), cons(sK58, nil))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13543)), inference(backward_demodulation, [], [f957358, f1257732])).
fof(f1257732, plain, ((hd(sK53) = hd(app(sK53, cons(sK58, nil)))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_3899)), inference(forward_demodulation, [], [f902803, f957333])).
fof(f957333, plain, ((cons(sK58, nil) = tl(app(sK53, cons(sK58, nil)))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883)), inference(forward_demodulation, [], [f957332, f97358])).
fof(f97358, plain, ((cons(sK58, nil) = app(nil, cons(sK58, nil))) | ~ spl63_883), inference(resolution, [], [f18727, f461])).
fof(f461, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax28)).
fof(f18727, plain, (ssList(cons(sK58, nil)) | ~ spl63_883), inference(avatar_component_clause, [], [f18726])).
fof(f18726, plain, (spl63_883 <=> ssList(cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_883])])).
fof(f957332, plain, ((app(nil, cons(sK58, nil)) = tl(app(sK53, cons(sK58, nil)))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883)), inference(forward_demodulation, [], [f957331, f20226])).
fof(f20226, plain, ((nil = tl(sK53)) | (~ spl63_4 | ~ spl63_5)), inference(backward_demodulation, [], [f7686, f20224])).
fof(f7686, plain, ((nil = tl(cons(sK57, nil))) | ~ spl63_5), inference(resolution, [], [f1099, f630])).
fof(f1099, plain, ! [X6] : (~ ssItem(X6) | (nil = tl(cons(X6, nil)))), inference(resolution, [], [f458, f447])).
fof(f458, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax25)).
fof(f957331, plain, ((tl(app(sK53, cons(sK58, nil))) = app(tl(sK53), cons(sK58, nil))) | (~ spl63_66 | spl63_347 | ~ spl63_883)), inference(subsumption_resolution, [], [f957312, f5861])).
fof(f957312, plain, ((nil = sK53) | (tl(app(sK53, cons(sK58, nil))) = app(tl(sK53), cons(sK58, nil))) | (~ spl63_66 | ~ spl63_883)), inference(resolution, [], [f661769, f641])).
fof(f661769, plain, (! [X8] : (~ ssList(X8) | (nil = X8) | (tl(app(X8, cons(sK58, nil))) = app(tl(X8), cons(sK58, nil)))) | (~ spl63_66 | ~ spl63_883)), inference(forward_demodulation, [], [f18985, f657283])).
fof(f657283, plain, ((cons(sK58, nil) = app(cons(sK58, nil), nil)) | ~ spl63_883), inference(forward_demodulation, [], [f657229, f441512])).
fof(f441512, plain, ((cons(sK58, nil) = sK12(cons(sK58, nil), nil)) | ~ spl63_883), inference(forward_demodulation, [], [f441411, f97515])).
fof(f97515, plain, ((cons(sK58, nil) = app(sK12(cons(sK58, nil), nil), nil)) | ~ spl63_883), inference(resolution, [], [f18727, f1703])).
fof(f1703, plain, ! [X1] : (~ ssList(X1) | (app(sK12(X1, nil), nil) = X1)), inference(subsumption_resolution, [], [f1701, f447])).
fof(f1701, plain, ! [X1] : ((app(sK12(X1, nil), nil) = X1) | ~ ssList(nil) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f1698])).
fof(f1698, plain, ! [X1] : ((app(sK12(X1, nil), nil) = X1) | ~ ssList(nil) | ~ ssList(X1) | ~ ssList(X1)), inference(resolution, [], [f371, f493])).
fof(f493, plain, ! [X0] : (rearsegP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ! [X0] : (rearsegP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (ssList(X0) => rearsegP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax51)).
fof(f371, plain, ! [X0, X1] : (~ rearsegP(X0, X1) | (app(sK12(X0, X1), X1) = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f253])).
fof(f253, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (((app(sK12(X0, X1), X1) = X0) & ssList(sK12(X0, X1))) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f251, f252])).
fof(f252, plain, ! [X1, X0] : (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) => ((app(sK12(X0, X1), X1) = X0) & ssList(sK12(X0, X1)))), introduced(choice_axiom, [])).
fof(f251, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f250])).
fof(f250, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X2, X1) = X0) & ssList(X2)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : (! [X1] : ((rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax6)).
fof(f441411, plain, ((sK12(cons(sK58, nil), nil) = app(sK12(cons(sK58, nil), nil), nil)) | ~ spl63_883), inference(resolution, [], [f38353, f18727])).
fof(f38353, plain, ! [X1] : (~ ssList(X1) | (sK12(X1, nil) = app(sK12(X1, nil), nil))), inference(subsumption_resolution, [], [f38351, f447])).
fof(f38351, plain, ! [X1] : (~ ssList(nil) | ~ ssList(X1) | (sK12(X1, nil) = app(sK12(X1, nil), nil))), inference(duplicate_literal_removal, [], [f38341])).
fof(f38341, plain, ! [X1] : (~ ssList(nil) | ~ ssList(X1) | (sK12(X1, nil) = app(sK12(X1, nil), nil)) | ~ ssList(X1)), inference(resolution, [], [f1435, f493])).
fof(f1435, plain, ! [X21, X20] : (~ rearsegP(X20, X21) | ~ ssList(X21) | ~ ssList(X20) | (sK12(X20, X21) = app(sK12(X20, X21), nil))), inference(resolution, [], [f370, f540])).
fof(f370, plain, ! [X0, X1] : (ssList(sK12(X0, X1)) | ~ rearsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f253])).
fof(f657229, plain, ((sK12(cons(sK58, nil), nil) = app(sK12(cons(sK58, nil), nil), nil)) | ~ spl63_883), inference(resolution, [], [f18727, f38353])).
fof(f18985, plain, (! [X8] : ((tl(app(X8, app(cons(sK58, nil), nil))) = app(tl(X8), app(cons(sK58, nil), nil))) | (nil = X8) | ~ ssList(X8)) | (~ spl63_66 | ~ spl63_883)), inference(backward_demodulation, [], [f5319, f18827])).
fof(f18827, plain, ((cons(sK58, nil) = app(nil, cons(sK58, nil))) | ~ spl63_883), inference(resolution, [], [f18727, f461])).
fof(f5319, plain, (! [X8] : ((nil = X8) | (tl(app(X8, app(app(nil, cons(sK58, nil)), nil))) = app(tl(X8), app(app(nil, cons(sK58, nil)), nil))) | ~ ssList(X8)) | ~ spl63_66), inference(resolution, [], [f2242, f542])).
fof(f542, plain, ! [X0, X1] : (~ ssList(X1) | (nil = X0) | (tl(app(X0, X1)) = app(tl(X0), X1)) | ~ ssList(X0)), inference(cnf_transformation, [], [f206])).
fof(f206, plain, ! [X0] : (! [X1] : ((tl(app(X0, X1)) = app(tl(X0), X1)) | (nil = X0) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f205])).
fof(f205, plain, ! [X0] : (! [X1] : (((tl(app(X0, X1)) = app(tl(X0), X1)) | (nil = X0)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (~ (nil = X0) => (tl(app(X0, X1)) = app(tl(X0), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax86)).
fof(f2242, plain, (ssList(app(app(nil, cons(sK58, nil)), nil)) | ~ spl63_66), inference(avatar_component_clause, [], [f2241])).
fof(f2241, plain, (spl63_66 <=> ssList(app(app(nil, cons(sK58, nil)), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_66])])).
fof(f902803, plain, ((hd(sK53) = hd(app(sK53, tl(app(sK53, cons(sK58, nil)))))) | (spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_3899)), inference(subsumption_resolution, [], [f902792, f5861])).
fof(f902792, plain, ((nil = sK53) | (hd(sK53) = hd(app(sK53, tl(app(sK53, cons(sK58, nil)))))) | (~ spl63_883 | ~ spl63_3713 | ~ spl63_3899)), inference(resolution, [], [f839001, f641])).
fof(f839001, plain, (! [X82] : (~ ssList(X82) | (nil = X82) | (hd(X82) = hd(app(X82, tl(app(sK53, cons(sK58, nil))))))) | (~ spl63_883 | ~ spl63_3713 | ~ spl63_3899)), inference(forward_demodulation, [], [f249774, f72453])).
fof(f249774, plain, (! [X82] : ((hd(X82) = hd(app(X82, tl(app(sK60, cons(sK58, nil)))))) | (nil = X82) | ~ ssList(X82)) | (~ spl63_883 | ~ spl63_3899)), inference(backward_demodulation, [], [f74303, f249707])).
fof(f249707, plain, ((app(sK60, cons(sK58, nil)) = app(app(sK60, cons(sK58, nil)), nil)) | ~ spl63_883), inference(resolution, [], [f71298, f18727])).
fof(f71298, plain, ! [X11] : (~ ssList(X11) | (app(sK60, X11) = app(app(sK60, X11), nil))), inference(resolution, [], [f562, f948])).
fof(f948, plain, ! [X0, X1] : (~ ssList(X1) | ~ ssList(X0) | (app(X1, X0) = app(app(X1, X0), nil))), inference(resolution, [], [f459, f540])).
fof(f459, plain, ! [X0, X1] : (ssList(app(X0, X1)) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (ssList(app(X0, X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ssList(app(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax26)).
fof(f74303, plain, (! [X82] : ((hd(X82) = hd(app(X82, tl(app(app(sK60, cons(sK58, nil)), nil))))) | (nil = X82) | ~ ssList(X82)) | ~ spl63_3899), inference(avatar_component_clause, [], [f74302])).
fof(f74302, plain, (spl63_3899 <=> ! [X82] : ((hd(X82) = hd(app(X82, tl(app(app(sK60, cons(sK58, nil)), nil))))) | (nil = X82) | ~ ssList(X82))), introduced(avatar_definition, [new_symbols(naming, [spl63_3899])])).
fof(f957358, plain, ((app(sK53, cons(sK58, nil)) = cons(hd(app(sK53, cons(sK58, nil))), cons(sK58, nil))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_13543)), inference(backward_demodulation, [], [f839015, f957333])).
fof(f839015, plain, ((app(sK53, cons(sK58, nil)) = cons(hd(app(sK53, cons(sK58, nil))), tl(app(sK53, cons(sK58, nil))))) | (~ spl63_3713 | ~ spl63_13543)), inference(forward_demodulation, [], [f257676, f72453])).
fof(f257676, plain, ((app(sK60, cons(sK58, nil)) = cons(hd(app(sK60, cons(sK58, nil))), tl(app(sK60, cons(sK58, nil))))) | ~ spl63_13543), inference(avatar_component_clause, [], [f257674])).
fof(f257674, plain, (spl63_13543 <=> (app(sK60, cons(sK58, nil)) = cons(hd(app(sK60, cons(sK58, nil))), tl(app(sK60, cons(sK58, nil)))))), introduced(avatar_definition, [new_symbols(naming, [spl63_13543])])).
fof(f750793, plain, ((sK53 = app(app(app(sK53, cons(sK58, nil)), sK61), sK53)) | (~ spl63_3713 | ~ spl63_14403)), inference(backward_demodulation, [], [f346854, f72453])).
fof(f346854, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | ~ spl63_14403), inference(avatar_component_clause, [], [f346852])).
fof(f346852, plain, (spl63_14403 <=> (sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_14403])])).
fof(f829025, plain, ((cons(sK57, sK53) = app(sK53, sK53)) | (~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_349 | ~ spl63_13574)), inference(forward_demodulation, [], [f829024, f259529])).
fof(f829024, plain, ((cons(sK57, sK53) = app(cons(sK57, nil), sK53)) | (~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_349)), inference(forward_demodulation, [], [f829023, f20278])).
fof(f20278, plain, ((sK57 = sK51(sK53)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_349)), inference(backward_demodulation, [], [f5872, f20225])).
fof(f20225, plain, ((sK57 = hd(sK53)) | (~ spl63_4 | ~ spl63_5)), inference(backward_demodulation, [], [f7649, f20224])).
fof(f7649, plain, ((sK57 = hd(cons(sK57, nil))) | ~ spl63_5), inference(resolution, [], [f1068, f630])).
fof(f5872, plain, ((hd(sK53) = sK51(sK53)) | ~ spl63_349), inference(avatar_component_clause, [], [f5870])).
fof(f5870, plain, (spl63_349 <=> (hd(sK53) = sK51(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_349])])).
fof(f829023, plain, ((cons(sK51(sK53), sK53) = app(cons(sK51(sK53), nil), sK53)) | spl63_347), inference(subsumption_resolution, [], [f828994, f5861])).
fof(f828994, plain, ((nil = sK53) | (cons(sK51(sK53), sK53) = app(cons(sK51(sK53), nil), sK53))), inference(resolution, [], [f25556, f641])).
fof(f25556, plain, ! [X17] : (~ ssList(X17) | (nil = X17) | (cons(sK51(X17), sK53) = app(cons(sK51(X17), nil), sK53))), inference(resolution, [], [f5724, f527])).
fof(f527, plain, ! [X0] : (ssItem(sK51(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f337])).
fof(f337, plain, ! [X0] : (((hd(X0) = sK51(X0)) & ssItem(sK51(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK51])], [f188, f336])).
fof(f336, plain, ! [X0] : (? [X1] : ((hd(X0) = X1) & ssItem(X1)) => ((hd(X0) = sK51(X0)) & ssItem(sK51(X0)))), introduced(choice_axiom, [])).
fof(f188, plain, ! [X0] : (? [X1] : ((hd(X0) = X1) & ssItem(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f187])).
fof(f187, plain, ! [X0] : ((? [X1] : ((hd(X0) = X1) & ssItem(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ? [X1] : ((hd(X0) = X1) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax75)).
fof(f1263522, plain, (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13574 | spl63_22322), inference(avatar_contradiction_clause, [], [f1263521])).
fof(f1263521, plain, ($false | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13574 | spl63_22322)), inference(subsumption_resolution, [], [f1263452, f838223])).
fof(f838223, plain, (~ (sK57 = hd(app(sK53, cons(sK58, nil)))) | spl63_22322), inference(avatar_component_clause, [], [f838222])).
fof(f838222, plain, (spl63_22322 <=> (sK57 = hd(app(sK53, cons(sK58, nil))))), introduced(avatar_definition, [new_symbols(naming, [spl63_22322])])).
fof(f1263452, plain, ((sK57 = hd(app(sK53, cons(sK58, nil)))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_349 | ~ spl63_883 | ~ spl63_2212 | ~ spl63_3713 | ~ spl63_3899 | ~ spl63_13522 | ~ spl63_13574)), inference(backward_demodulation, [], [f1257732, f1263274])).
fof(f1245610, plain, (spl63_27153 | ~ spl63_3713 | ~ spl63_5008), inference(avatar_split_clause, [], [f750106, f93772, f72452, f1245601])).
fof(f93772, plain, (spl63_5008 <=> (sK57 = sK50(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_5008])])).
fof(f750106, plain, ((sK57 = sK50(sK53)) | (~ spl63_3713 | ~ spl63_5008)), inference(backward_demodulation, [], [f93774, f72453])).
fof(f93774, plain, ((sK57 = sK50(sK60)) | ~ spl63_5008), inference(avatar_component_clause, [], [f93772])).
fof(f1245066, plain, (spl63_25443 | ~ spl63_4 | ~ spl63_5 | ~ spl63_65 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_12129 | ~ spl63_12130 | ~ spl63_13543 | ~ spl63_18875 | ~ spl63_20953 | ~ spl63_20967 | ~ spl63_22322), inference(avatar_split_clause, [], [f1245065, f838222, f686762, f686674, f566095, f257674, f226261, f226257, f72452, f18726, f5860, f2241, f2237, f628, f623, f1143621])).
fof(f2237, plain, (spl63_65 <=> ssList(cons(sK59, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_65])])).
fof(f226261, plain, (spl63_12130 <=> frontsegP(app(sK60, cons(sK58, nil)), sK53)), introduced(avatar_definition, [new_symbols(naming, [spl63_12130])])).
fof(f566095, plain, (spl63_18875 <=> ssList(sK61)), introduced(avatar_definition, [new_symbols(naming, [spl63_18875])])).
fof(f686674, plain, (spl63_20953 <=> ssList(app(app(sK60, cons(sK58, nil)), sK61))), introduced(avatar_definition, [new_symbols(naming, [spl63_20953])])).
fof(f686762, plain, (spl63_20967 <=> frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_20967])])).
fof(f1245065, plain, ((sK53 = app(cons(sK57, cons(sK58, nil)), sK61)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_65 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_12129 | ~ spl63_12130 | ~ spl63_13543 | ~ spl63_18875 | ~ spl63_20953 | ~ spl63_20967 | ~ spl63_22322)), inference(subsumption_resolution, [], [f1245064, f957774])).
fof(f957774, plain, (frontsegP(cons(sK57, cons(sK58, nil)), sK53) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_12130 | ~ spl63_13543 | ~ spl63_22322)), inference(backward_demodulation, [], [f820624, f957498])).
fof(f957498, plain, ((app(sK53, cons(sK58, nil)) = cons(sK57, cons(sK58, nil))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_13543 | ~ spl63_22322)), inference(backward_demodulation, [], [f957358, f838224])).
fof(f838224, plain, ((sK57 = hd(app(sK53, cons(sK58, nil)))) | ~ spl63_22322), inference(avatar_component_clause, [], [f838222])).
fof(f820624, plain, (frontsegP(app(sK53, cons(sK58, nil)), sK53) | (~ spl63_3713 | ~ spl63_12130)), inference(forward_demodulation, [], [f226262, f72453])).
fof(f226262, plain, (frontsegP(app(sK60, cons(sK58, nil)), sK53) | ~ spl63_12130), inference(avatar_component_clause, [], [f226261])).
fof(f1245064, plain, ((sK53 = app(cons(sK57, cons(sK58, nil)), sK61)) | ~ frontsegP(cons(sK57, cons(sK58, nil)), sK53) | (~ spl63_4 | ~ spl63_5 | ~ spl63_65 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_12129 | ~ spl63_13543 | ~ spl63_18875 | ~ spl63_20953 | ~ spl63_20967 | ~ spl63_22322)), inference(subsumption_resolution, [], [f1245063, f957504])).
fof(f957504, plain, (ssList(cons(sK57, cons(sK58, nil))) | (~ spl63_4 | ~ spl63_5 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_12129 | ~ spl63_13543 | ~ spl63_22322)), inference(backward_demodulation, [], [f750432, f957498])).
fof(f1245063, plain, (~ ssList(cons(sK57, cons(sK58, nil))) | (sK53 = app(cons(sK57, cons(sK58, nil)), sK61)) | ~ frontsegP(cons(sK57, cons(sK58, nil)), sK53) | (~ spl63_4 | ~ spl63_5 | ~ spl63_65 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_13543 | ~ spl63_18875 | ~ spl63_20953 | ~ spl63_20967 | ~ spl63_22322)), inference(subsumption_resolution, [], [f1245062, f641])).
fof(f1245062, plain, (~ ssList(sK53) | ~ ssList(cons(sK57, cons(sK58, nil))) | (sK53 = app(cons(sK57, cons(sK58, nil)), sK61)) | ~ frontsegP(cons(sK57, cons(sK58, nil)), sK53) | (~ spl63_4 | ~ spl63_5 | ~ spl63_65 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_13543 | ~ spl63_18875 | ~ spl63_20953 | ~ spl63_20967 | ~ spl63_22322)), inference(subsumption_resolution, [], [f1245039, f566096])).
fof(f566096, plain, (ssList(sK61) | ~ spl63_18875), inference(avatar_component_clause, [], [f566095])).
fof(f1245039, plain, (~ ssList(sK61) | ~ ssList(sK53) | ~ ssList(cons(sK57, cons(sK58, nil))) | (sK53 = app(cons(sK57, cons(sK58, nil)), sK61)) | ~ frontsegP(cons(sK57, cons(sK58, nil)), sK53) | (~ spl63_4 | ~ spl63_5 | ~ spl63_65 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_13543 | ~ spl63_20953 | ~ spl63_20967 | ~ spl63_22322)), inference(resolution, [], [f1057128, f1733])).
fof(f1733, plain, ! [X4, X5, X3] : (~ frontsegP(X4, app(X3, X5)) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X3) | (app(X3, X5) = X4) | ~ frontsegP(X3, X4)), inference(subsumption_resolution, [], [f1730, f459])).
fof(f1730, plain, ! [X4, X5, X3] : (~ frontsegP(X3, X4) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X3) | (app(X3, X5) = X4) | ~ frontsegP(X4, app(X3, X5)) | ~ ssList(app(X3, X5))), inference(duplicate_literal_removal, [], [f1725])).
fof(f1725, plain, ! [X4, X5, X3] : (~ frontsegP(X3, X4) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X3) | (app(X3, X5) = X4) | ~ frontsegP(X4, app(X3, X5)) | ~ ssList(app(X3, X5)) | ~ ssList(X4)), inference(resolution, [], [f482, f480])).
fof(f480, plain, ! [X0, X1] : (~ frontsegP(X1, X0) | (X0 = X1) | ~ frontsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f153])).
fof(f153, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ frontsegP(X1, X0) | ~ frontsegP(X0, X1) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f152])).
fof(f152, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ frontsegP(X1, X0) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((frontsegP(X1, X0) & frontsegP(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax41)).
fof(f482, plain, ! [X2, X0, X1] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (frontsegP(X0, X1) => frontsegP(app(X0, X2), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax43)).
fof(f1057128, plain, (frontsegP(sK53, app(cons(sK57, cons(sK58, nil)), sK61)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_65 | ~ spl63_66 | spl63_347 | ~ spl63_883 | ~ spl63_3713 | ~ spl63_13543 | ~ spl63_20953 | ~ spl63_20967 | ~ spl63_22322)), inference(forward_demodulation, [], [f751600, f957498])).
fof(f751600, plain, (frontsegP(sK53, app(app(sK53, cons(sK58, nil)), sK61)) | (~ spl63_65 | ~ spl63_3713 | ~ spl63_20953 | ~ spl63_20967)), inference(backward_demodulation, [], [f694981, f72453])).
fof(f694981, plain, (frontsegP(sK53, app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl63_65 | ~ spl63_20953 | ~ spl63_20967)), inference(subsumption_resolution, [], [f694980, f2238])).
fof(f2238, plain, (ssList(cons(sK59, nil)) | ~ spl63_65), inference(avatar_component_clause, [], [f2237])).
fof(f694980, plain, (frontsegP(sK53, app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | (~ spl63_20953 | ~ spl63_20967)), inference(subsumption_resolution, [], [f694979, f641])).
fof(f694979, plain, (frontsegP(sK53, app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(sK53) | ~ ssList(cons(sK59, nil)) | (~ spl63_20953 | ~ spl63_20967)), inference(subsumption_resolution, [], [f694953, f686675])).
fof(f686675, plain, (ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ spl63_20953), inference(avatar_component_clause, [], [f686674])).
fof(f694953, plain, (frontsegP(sK53, app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(sK53) | ~ ssList(cons(sK59, nil)) | ~ spl63_20967), inference(resolution, [], [f686764, f1940])).
fof(f1940, plain, ! [X6, X4, X5] : (~ frontsegP(X4, app(X5, X6)) | frontsegP(X4, X5) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X6)), inference(subsumption_resolution, [], [f1937, f459])).
fof(f1937, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6)), inference(duplicate_literal_removal, [], [f1934])).
fof(f1934, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6) | ~ ssList(X5) | ~ ssList(app(X5, X6))), inference(resolution, [], [f479, f578])).
fof(f578, plain, ! [X2, X1] : (frontsegP(app(X1, X2), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X1, X2))), inference(equality_resolution, [], [f369])).
fof(f369, plain, ! [X2, X0, X1] : (frontsegP(X0, X1) | ~ (app(X1, X2) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f249])).
fof(f249, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (((app(X1, sK11(X0, X1)) = X0) & ssList(sK11(X0, X1))) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f247, f248])).
fof(f248, plain, ! [X1, X0] : (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) => ((app(X1, sK11(X0, X1)) = X0) & ssList(sK11(X0, X1)))), introduced(choice_axiom, [])).
fof(f247, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f246])).
fof(f246, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X1, X2) = X0) & ssList(X2)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax5)).
fof(f479, plain, ! [X2, X0, X1] : (~ frontsegP(X1, X2) | frontsegP(X0, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(X0, X2) | ~ frontsegP(X1, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f150])).
fof(f150, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(X0, X2) | (~ frontsegP(X1, X2) | ~ frontsegP(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((frontsegP(X1, X2) & frontsegP(X0, X1)) => frontsegP(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax40)).
fof(f686764, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl63_20967), inference(avatar_component_clause, [], [f686762])).
fof(f820619, plain, (~ spl63_883 | ~ spl63_3713 | ~ spl63_12129 | spl63_12130), inference(avatar_contradiction_clause, [], [f820618])).
fof(f820618, plain, ($false | (~ spl63_883 | ~ spl63_3713 | ~ spl63_12129 | spl63_12130)), inference(subsumption_resolution, [], [f820617, f750432])).
fof(f820617, plain, (~ ssList(app(sK53, cons(sK58, nil))) | (~ spl63_883 | ~ spl63_3713 | spl63_12130)), inference(subsumption_resolution, [], [f820616, f641])).
fof(f820616, plain, (~ ssList(sK53) | ~ ssList(app(sK53, cons(sK58, nil))) | (~ spl63_883 | ~ spl63_3713 | spl63_12130)), inference(subsumption_resolution, [], [f820613, f18727])).
fof(f820613, plain, (~ ssList(cons(sK58, nil)) | ~ ssList(sK53) | ~ ssList(app(sK53, cons(sK58, nil))) | (~ spl63_3713 | spl63_12130)), inference(resolution, [], [f750433, f578])).
fof(f750433, plain, (~ frontsegP(app(sK53, cons(sK58, nil)), sK53) | (~ spl63_3713 | spl63_12130)), inference(backward_demodulation, [], [f226263, f72453])).
fof(f226263, plain, (~ frontsegP(app(sK60, cons(sK58, nil)), sK53) | spl63_12130), inference(avatar_component_clause, [], [f226261])).
fof(f752275, plain, (~ spl63_11976 | ~ spl63_3713 | spl63_11908), inference(avatar_split_clause, [], [f750420, f221223, f72452, f223607])).
fof(f223607, plain, (spl63_11976 <=> (cons(sK58, nil) = cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_11976])])).
fof(f221223, plain, (spl63_11908 <=> (cons(sK58, nil) = cons(sK58, sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_11908])])).
fof(f750420, plain, (~ (cons(sK58, nil) = cons(sK58, sK53)) | (~ spl63_3713 | spl63_11908)), inference(backward_demodulation, [], [f221224, f72453])).
fof(f221224, plain, (~ (cons(sK58, nil) = cons(sK58, sK60)) | spl63_11908), inference(avatar_component_clause, [], [f221223])).
fof(f747940, plain, (spl63_3713 | ~ spl63_58 | ~ spl63_169 | ~ spl63_3712 | ~ spl63_13574), inference(avatar_split_clause, [], [f746811, f259527, f72448, f3604, f1800, f72452])).
fof(f1800, plain, (spl63_58 <=> (sK60 = cons(hd(sK60), tl(sK60)))), introduced(avatar_definition, [new_symbols(naming, [spl63_58])])).
fof(f3604, plain, (spl63_169 <=> (nil = tl(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_169])])).
fof(f72448, plain, (spl63_3712 <=> (sK57 = hd(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_3712])])).
fof(f746811, plain, ((sK53 = sK60) | (~ spl63_58 | ~ spl63_169 | ~ spl63_3712 | ~ spl63_13574)), inference(forward_demodulation, [], [f745555, f259529])).
fof(f745555, plain, ((cons(sK57, nil) = sK60) | (~ spl63_58 | ~ spl63_169 | ~ spl63_3712)), inference(backward_demodulation, [], [f744909, f3606])).
fof(f3606, plain, ((nil = tl(sK60)) | ~ spl63_169), inference(avatar_component_clause, [], [f3604])).
fof(f744909, plain, ((sK60 = cons(sK57, tl(sK60))) | (~ spl63_58 | ~ spl63_3712)), inference(backward_demodulation, [], [f1802, f72450])).
fof(f72450, plain, ((sK57 = hd(sK60)) | ~ spl63_3712), inference(avatar_component_clause, [], [f72448])).
fof(f1802, plain, ((sK60 = cons(hd(sK60), tl(sK60))) | ~ spl63_58), inference(avatar_component_clause, [], [f1800])).
fof(f744466, plain, (spl63_3712 | ~ spl63_3715 | ~ spl63_4 | ~ spl63_5 | ~ spl63_58 | ~ spl63_157 | ~ spl63_158), inference(avatar_split_clause, [], [f744465, f3556, f3552, f1800, f628, f623, f72470, f72448])).
fof(f3552, plain, (spl63_157 <=> ssList(tl(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_157])])).
fof(f3556, plain, (spl63_158 <=> ssItem(hd(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_158])])).
fof(f744465, plain, (~ frontsegP(sK53, sK60) | (sK57 = hd(sK60)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_58 | ~ spl63_157 | ~ spl63_158)), inference(subsumption_resolution, [], [f744464, f3557])).
fof(f3557, plain, (ssItem(hd(sK60)) | ~ spl63_158), inference(avatar_component_clause, [], [f3556])).
fof(f744464, plain, (~ frontsegP(sK53, sK60) | (sK57 = hd(sK60)) | ~ ssItem(hd(sK60)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_58 | ~ spl63_157)), inference(subsumption_resolution, [], [f687099, f3553])).
fof(f3553, plain, (ssList(tl(sK60)) | ~ spl63_157), inference(avatar_component_clause, [], [f3552])).
fof(f687099, plain, (~ frontsegP(sK53, sK60) | (sK57 = hd(sK60)) | ~ ssList(tl(sK60)) | ~ ssItem(hd(sK60)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_58)), inference(superposition, [], [f20702, f1802])).
fof(f20702, plain, (! [X12, X13] : (~ frontsegP(sK53, cons(X12, X13)) | (sK57 = X12) | ~ ssList(X13) | ~ ssItem(X12)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20701, f630])).
fof(f20701, plain, (! [X12, X13] : (~ frontsegP(sK53, cons(X12, X13)) | (sK57 = X12) | ~ ssList(X13) | ~ ssItem(X12) | ~ ssItem(sK57)) | ~ spl63_4), inference(subsumption_resolution, [], [f20663, f447])).
fof(f20663, plain, (! [X12, X13] : (~ frontsegP(sK53, cons(X12, X13)) | (sK57 = X12) | ~ ssList(X13) | ~ ssList(nil) | ~ ssItem(X12) | ~ ssItem(sK57)) | ~ spl63_4), inference(superposition, [], [f483, f20224])).
fof(f483, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f328])).
fof(f744463, plain, (spl63_3716 | ~ spl63_3715 | ~ spl63_4 | ~ spl63_5 | ~ spl63_58 | ~ spl63_157 | ~ spl63_158), inference(avatar_split_clause, [], [f744462, f3556, f3552, f1800, f628, f623, f72470, f72477])).
fof(f72477, plain, (spl63_3716 <=> frontsegP(nil, tl(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_3716])])).
fof(f744462, plain, (~ frontsegP(sK53, sK60) | frontsegP(nil, tl(sK60)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_58 | ~ spl63_157 | ~ spl63_158)), inference(subsumption_resolution, [], [f744461, f3557])).
fof(f744461, plain, (~ frontsegP(sK53, sK60) | frontsegP(nil, tl(sK60)) | ~ ssItem(hd(sK60)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_58 | ~ spl63_157)), inference(subsumption_resolution, [], [f687100, f3553])).
fof(f687100, plain, (~ frontsegP(sK53, sK60) | frontsegP(nil, tl(sK60)) | ~ ssList(tl(sK60)) | ~ ssItem(hd(sK60)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_58)), inference(superposition, [], [f20704, f1802])).
fof(f20704, plain, (! [X17, X16] : (~ frontsegP(sK53, cons(X16, X17)) | frontsegP(nil, X17) | ~ ssList(X17) | ~ ssItem(X16)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20703, f630])).
fof(f20703, plain, (! [X17, X16] : (~ frontsegP(sK53, cons(X16, X17)) | frontsegP(nil, X17) | ~ ssList(X17) | ~ ssItem(X16) | ~ ssItem(sK57)) | ~ spl63_4), inference(subsumption_resolution, [], [f20665, f447])).
fof(f20665, plain, (! [X17, X16] : (~ frontsegP(sK53, cons(X16, X17)) | frontsegP(nil, X17) | ~ ssList(X17) | ~ ssList(nil) | ~ ssItem(X16) | ~ ssItem(sK57)) | ~ spl63_4), inference(superposition, [], [f484, f20224])).
fof(f484, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f328])).
fof(f744445, plain, (spl63_3715 | ~ spl63_65 | ~ spl63_883 | ~ spl63_14322 | ~ spl63_18875 | ~ spl63_20953 | ~ spl63_20967), inference(avatar_split_clause, [], [f744444, f686762, f686674, f566095, f335074, f18726, f2237, f72470])).
fof(f335074, plain, (spl63_14322 <=> ssList(sK60)), introduced(avatar_definition, [new_symbols(naming, [spl63_14322])])).
fof(f744444, plain, (frontsegP(sK53, sK60) | (~ spl63_65 | ~ spl63_883 | ~ spl63_14322 | ~ spl63_18875 | ~ spl63_20953 | ~ spl63_20967)), inference(subsumption_resolution, [], [f744443, f566096])).
fof(f744443, plain, (frontsegP(sK53, sK60) | ~ ssList(sK61) | (~ spl63_65 | ~ spl63_883 | ~ spl63_14322 | ~ spl63_20953 | ~ spl63_20967)), inference(subsumption_resolution, [], [f744353, f18727])).
fof(f744353, plain, (frontsegP(sK53, sK60) | ~ ssList(cons(sK58, nil)) | ~ ssList(sK61) | (~ spl63_65 | ~ spl63_14322 | ~ spl63_20953 | ~ spl63_20967)), inference(subsumption_resolution, [], [f744352, f335075])).
fof(f335075, plain, (ssList(sK60) | ~ spl63_14322), inference(avatar_component_clause, [], [f335074])).
fof(f744352, plain, (~ ssList(sK60) | frontsegP(sK53, sK60) | ~ ssList(cons(sK58, nil)) | ~ ssList(sK61) | (~ spl63_65 | ~ spl63_20953 | ~ spl63_20967)), inference(duplicate_literal_removal, [], [f744347])).
fof(f744347, plain, (~ ssList(sK60) | frontsegP(sK53, sK60) | ~ ssList(sK60) | ~ ssList(cons(sK58, nil)) | ~ ssList(sK61) | (~ spl63_65 | ~ spl63_20953 | ~ spl63_20967)), inference(resolution, [], [f694974, f16296])).
fof(f16296, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(X3) | ~ ssList(X4)), inference(subsumption_resolution, [], [f16295, f459])).
fof(f16295, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3))), inference(subsumption_resolution, [], [f16293, f459])).
fof(f16293, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(app(app(X2, X3), X4)) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3))), inference(duplicate_literal_removal, [], [f16286])).
fof(f16286, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(app(app(X2, X3), X4)) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3)) | ~ ssList(app(app(X2, X3), X4))), inference(resolution, [], [f1940, f578])).
fof(f694974, plain, (! [X0] : (~ frontsegP(app(app(sK60, cons(sK58, nil)), sK61), X0) | ~ ssList(X0) | frontsegP(sK53, X0)) | (~ spl63_65 | ~ spl63_20953 | ~ spl63_20967)), inference(subsumption_resolution, [], [f694973, f686675])).
fof(f694973, plain, (! [X0] : (frontsegP(sK53, X0) | ~ ssList(X0) | ~ frontsegP(app(app(sK60, cons(sK58, nil)), sK61), X0) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61))) | (~ spl63_65 | ~ spl63_20967)), inference(subsumption_resolution, [], [f694972, f2238])).
fof(f694972, plain, (! [X0] : (frontsegP(sK53, X0) | ~ ssList(X0) | ~ frontsegP(app(app(sK60, cons(sK58, nil)), sK61), X0) | ~ ssList(cons(sK59, nil)) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61))) | ~ spl63_20967), inference(subsumption_resolution, [], [f694951, f641])).
fof(f694951, plain, (! [X0] : (frontsegP(sK53, X0) | ~ ssList(X0) | ~ ssList(sK53) | ~ frontsegP(app(app(sK60, cons(sK58, nil)), sK61), X0) | ~ ssList(cons(sK59, nil)) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61))) | ~ spl63_20967), inference(resolution, [], [f686764, f1941])).
fof(f1941, plain, ! [X10, X8, X7, X9] : (~ frontsegP(X7, app(X9, X10)) | frontsegP(X7, X8) | ~ ssList(X8) | ~ ssList(X7) | ~ frontsegP(X9, X8) | ~ ssList(X10) | ~ ssList(X9)), inference(subsumption_resolution, [], [f1936, f459])).
fof(f1936, plain, ! [X10, X8, X7, X9] : (frontsegP(X7, X8) | ~ frontsegP(X7, app(X9, X10)) | ~ ssList(X8) | ~ ssList(app(X9, X10)) | ~ ssList(X7) | ~ frontsegP(X9, X8) | ~ ssList(X10) | ~ ssList(X9)), inference(duplicate_literal_removal, [], [f1935])).
fof(f1935, plain, ! [X10, X8, X7, X9] : (frontsegP(X7, X8) | ~ frontsegP(X7, app(X9, X10)) | ~ ssList(X8) | ~ ssList(app(X9, X10)) | ~ ssList(X7) | ~ frontsegP(X9, X8) | ~ ssList(X10) | ~ ssList(X8) | ~ ssList(X9)), inference(resolution, [], [f479, f482])).
fof(f717674, plain, (spl63_14402 | ~ spl63_3370 | ~ spl63_13574 | ~ spl63_20961), inference(avatar_split_clause, [], [f698067, f686728, f259527, f59357, f346848])).
fof(f346848, plain, (spl63_14402 <=> ssList(app(app(app(sK60, cons(sK58, nil)), sK61), sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_14402])])).
fof(f59357, plain, (spl63_3370 <=> (sK57 = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl63_3370])])).
fof(f686728, plain, (spl63_20961 <=> ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_20961])])).
fof(f698067, plain, (ssList(app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | (~ spl63_3370 | ~ spl63_13574 | ~ spl63_20961)), inference(forward_demodulation, [], [f696744, f259529])).
fof(f696744, plain, (ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK57, nil))) | (~ spl63_3370 | ~ spl63_20961)), inference(backward_demodulation, [], [f686729, f59359])).
fof(f59359, plain, ((sK57 = sK59) | ~ spl63_3370), inference(avatar_component_clause, [], [f59357])).
fof(f686729, plain, (ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl63_20961), inference(avatar_component_clause, [], [f686728])).
fof(f717549, plain, (~ spl63_14402 | spl63_4146 | ~ spl63_3370 | ~ spl63_10127 | ~ spl63_13574), inference(avatar_split_clause, [], [f717446, f259527, f162224, f59357, f75810, f346848])).
fof(f75810, plain, (spl63_4146 <=> ! [X8] : (memberP(sK53, X8) | ~ ssItem(X8) | ~ memberP(sK62, X8))), introduced(avatar_definition, [new_symbols(naming, [spl63_4146])])).
fof(f162224, plain, (spl63_10127 <=> ssList(sK62)), introduced(avatar_definition, [new_symbols(naming, [spl63_10127])])).
fof(f717446, plain, (! [X8] : (memberP(sK53, X8) | ~ memberP(sK62, X8) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | ~ ssItem(X8)) | (~ spl63_3370 | ~ spl63_10127 | ~ spl63_13574)), inference(subsumption_resolution, [], [f706811, f162225])).
fof(f162225, plain, (ssList(sK62) | ~ spl63_10127), inference(avatar_component_clause, [], [f162224])).
fof(f706811, plain, (! [X8] : (memberP(sK53, X8) | ~ memberP(sK62, X8) | ~ ssList(sK62) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | ~ ssItem(X8)) | (~ spl63_3370 | ~ spl63_13574)), inference(superposition, [], [f473, f697035])).
fof(f697035, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), sK53), sK62)) | (~ spl63_3370 | ~ spl63_13574)), inference(forward_demodulation, [], [f695735, f259529])).
fof(f695735, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK57, nil)), sK62)) | ~ spl63_3370), inference(backward_demodulation, [], [f565, f59359])).
fof(f565, plain, (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)), inference(cnf_transformation, [], [f354])).
fof(f473, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X2, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f324])).
fof(f324, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & (memberP(X2, X0) | memberP(X1, X0) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f323])).
fof(f323, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & ((memberP(X2, X0) | memberP(X1, X0)) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax36)).
fof(f717506, plain, (spl63_269 | spl63_36 | ~ spl63_10127), inference(avatar_split_clause, [], [f671661, f162224, f1204, f4778])).
fof(f4778, plain, (spl63_269 <=> (sK62 = cons(hd(sK62), tl(sK62)))), introduced(avatar_definition, [new_symbols(naming, [spl63_269])])).
fof(f1204, plain, (spl63_36 <=> (nil = sK62)), introduced(avatar_definition, [new_symbols(naming, [spl63_36])])).
fof(f671661, plain, ((sK62 = cons(hd(sK62), tl(sK62))) | (spl63_36 | ~ spl63_10127)), inference(subsumption_resolution, [], [f671394, f1205])).
fof(f1205, plain, (~ (nil = sK62) | spl63_36), inference(avatar_component_clause, [], [f1204])).
fof(f671394, plain, ((nil = sK62) | (sK62 = cons(hd(sK62), tl(sK62))) | ~ spl63_10127), inference(resolution, [], [f162225, f532])).
fof(f532, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax78)).
fof(f717443, plain, (~ spl63_4 | ~ spl63_871 | ~ spl63_3370 | ~ spl63_3483 | ~ spl63_13574 | ~ spl63_20953), inference(avatar_contradiction_clause, [], [f717442])).
fof(f717442, plain, ($false | (~ spl63_4 | ~ spl63_871 | ~ spl63_3370 | ~ spl63_3483 | ~ spl63_13574 | ~ spl63_20953)), inference(subsumption_resolution, [], [f717441, f686675])).
fof(f717441, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl63_4 | ~ spl63_871 | ~ spl63_3370 | ~ spl63_3483 | ~ spl63_13574)), inference(subsumption_resolution, [], [f717438, f20268])).
fof(f20268, plain, (sP0(sK53) | (~ spl63_4 | ~ spl63_871)), inference(backward_demodulation, [], [f18064, f20224])).
fof(f18064, plain, (sP0(cons(sK57, nil)) | ~ spl63_871), inference(avatar_component_clause, [], [f18063])).
fof(f18063, plain, (spl63_871 <=> sP0(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_871])])).
fof(f717438, plain, (~ sP0(sK53) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl63_3370 | ~ spl63_3483 | ~ spl63_13574)), inference(superposition, [], [f63707, f697035])).
fof(f63707, plain, (! [X62] : (~ sP0(app(app(X62, sK53), sK62)) | ~ ssList(X62)) | ~ spl63_3483), inference(avatar_component_clause, [], [f63706])).
fof(f63706, plain, (spl63_3483 <=> ! [X62] : (~ sP0(app(app(X62, sK53), sK62)) | ~ ssList(X62))), introduced(avatar_definition, [new_symbols(naming, [spl63_3483])])).
fof(f702994, plain, (~ spl63_4 | ~ spl63_797 | ~ spl63_884 | spl63_13585), inference(avatar_contradiction_clause, [], [f702993])).
fof(f702993, plain, ($false | (~ spl63_4 | ~ spl63_797 | ~ spl63_884 | spl63_13585)), inference(subsumption_resolution, [], [f702992, f259690])).
fof(f259690, plain, (~ (sK53 = cons(sK58, sK53)) | spl63_13585), inference(avatar_component_clause, [], [f259689])).
fof(f259689, plain, (spl63_13585 <=> (sK53 = cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_13585])])).
fof(f702992, plain, ((sK53 = cons(sK58, sK53)) | (~ spl63_4 | ~ spl63_797 | ~ spl63_884)), inference(forward_demodulation, [], [f702239, f673651])).
fof(f673651, plain, ((sK53 = app(nil, sK53)) | (~ spl63_4 | ~ spl63_797)), inference(backward_demodulation, [], [f441311, f673650])).
fof(f673650, plain, ((sK53 = sK12(sK53, nil)) | (~ spl63_4 | ~ spl63_797)), inference(forward_demodulation, [], [f441460, f20257])).
fof(f20257, plain, ((sK53 = app(sK12(sK53, nil), nil)) | (~ spl63_4 | ~ spl63_797)), inference(backward_demodulation, [], [f17537, f20224])).
fof(f17537, plain, ((cons(sK57, nil) = app(sK12(cons(sK57, nil), nil), nil)) | ~ spl63_797), inference(resolution, [], [f17221, f1703])).
fof(f17221, plain, (ssList(cons(sK57, nil)) | ~ spl63_797), inference(avatar_component_clause, [], [f17220])).
fof(f17220, plain, (spl63_797 <=> ssList(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_797])])).
fof(f441460, plain, (sK12(sK53, nil) = app(sK12(sK53, nil), nil)), inference(resolution, [], [f38353, f641])).
fof(f441311, plain, (sK12(sK53, nil) = app(nil, sK12(sK53, nil))), inference(resolution, [], [f38329, f641])).
fof(f38329, plain, ! [X1] : (~ ssList(X1) | (sK12(X1, nil) = app(nil, sK12(X1, nil)))), inference(subsumption_resolution, [], [f38327, f447])).
fof(f38327, plain, ! [X1] : (~ ssList(nil) | ~ ssList(X1) | (sK12(X1, nil) = app(nil, sK12(X1, nil)))), inference(duplicate_literal_removal, [], [f38317])).
fof(f38317, plain, ! [X1] : (~ ssList(nil) | ~ ssList(X1) | (sK12(X1, nil) = app(nil, sK12(X1, nil))) | ~ ssList(X1)), inference(resolution, [], [f1432, f493])).
fof(f1432, plain, ! [X14, X15] : (~ rearsegP(X14, X15) | ~ ssList(X15) | ~ ssList(X14) | (sK12(X14, X15) = app(nil, sK12(X14, X15)))), inference(resolution, [], [f370, f461])).
fof(f702239, plain, ((app(nil, sK53) = cons(sK58, sK53)) | ~ spl63_884), inference(backward_demodulation, [], [f25558, f18732])).
fof(f18732, plain, ((nil = cons(sK58, nil)) | ~ spl63_884), inference(avatar_component_clause, [], [f18730])).
fof(f18730, plain, (spl63_884 <=> (nil = cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_884])])).
fof(f25558, plain, (cons(sK58, sK53) = app(cons(sK58, nil), sK53)), inference(resolution, [], [f5724, f560])).
fof(f701549, plain, (spl63_13844 | ~ spl63_14309), inference(avatar_split_clause, [], [f701548, f321713, f262097])).
fof(f262097, plain, (spl63_13844 <=> (nil = sK52(cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_13844])])).
fof(f321713, plain, (spl63_14309 <=> (nil = tl(cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_14309])])).
fof(f701548, plain, ((nil = sK52(cons(sK58, nil))) | ~ spl63_14309), inference(forward_demodulation, [], [f440021, f321715])).
fof(f321715, plain, ((nil = tl(cons(sK58, nil))) | ~ spl63_14309), inference(avatar_component_clause, [], [f321713])).
fof(f440021, plain, (tl(cons(sK58, nil)) = sK52(cons(sK58, nil))), inference(resolution, [], [f14655, f560])).
fof(f14655, plain, ! [X12] : (~ ssItem(X12) | (tl(cons(X12, nil)) = sK52(cons(X12, nil)))), inference(resolution, [], [f1238, f447])).
fof(f1238, plain, ! [X0, X1] : (~ ssList(X1) | ~ ssItem(X0) | (tl(cons(X0, X1)) = sK52(cons(X0, X1)))), inference(subsumption_resolution, [], [f1209, f454])).
fof(f454, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax21)).
fof(f1209, plain, ! [X0, X1] : ((nil = cons(X0, X1)) | (tl(cons(X0, X1)) = sK52(cons(X0, X1))) | ~ ssItem(X0) | ~ ssList(X1)), inference(resolution, [], [f530, f446])).
fof(f446, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax16)).
fof(f530, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (tl(X0) = sK52(X0))), inference(cnf_transformation, [], [f339])).
fof(f339, plain, ! [X0] : (((tl(X0) = sK52(X0)) & ssList(sK52(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK52])], [f190, f338])).
fof(f338, plain, ! [X0] : (? [X1] : ((tl(X0) = X1) & ssList(X1)) => ((tl(X0) = sK52(X0)) & ssList(sK52(X0)))), introduced(choice_axiom, [])).
fof(f190, plain, ! [X0] : (? [X1] : ((tl(X0) = X1) & ssList(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f189])).
fof(f189, plain, ! [X0] : ((? [X1] : ((tl(X0) = X1) & ssList(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ? [X1] : ((tl(X0) = X1) & ssList(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax76)).
fof(f695711, plain, (spl63_3370 | ~ spl63_4 | ~ spl63_5 | ~ spl63_9813 | ~ spl63_20953 | ~ spl63_20961 | ~ spl63_20963), inference(avatar_split_clause, [], [f695710, f686737, f686728, f686674, f156693, f628, f623, f59357])).
fof(f156693, plain, (spl63_9813 <=> ssItem(sK59)), introduced(avatar_definition, [new_symbols(naming, [spl63_9813])])).
fof(f686737, plain, (spl63_20963 <=> ! [X7] : (memberP(sK53, X7) | ~ ssItem(X7) | ~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7))), introduced(avatar_definition, [new_symbols(naming, [spl63_20963])])).
fof(f695710, plain, ((sK57 = sK59) | (~ spl63_4 | ~ spl63_5 | ~ spl63_9813 | ~ spl63_20953 | ~ spl63_20961 | ~ spl63_20963)), inference(subsumption_resolution, [], [f695602, f156694])).
fof(f156694, plain, (ssItem(sK59) | ~ spl63_9813), inference(avatar_component_clause, [], [f156693])).
fof(f695602, plain, ((sK57 = sK59) | ~ ssItem(sK59) | (~ spl63_4 | ~ spl63_5 | ~ spl63_9813 | ~ spl63_20953 | ~ spl63_20961 | ~ spl63_20963)), inference(resolution, [], [f695597, f20698])).
fof(f20698, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | ~ ssItem(X8)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20697, f477])).
fof(f477, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (ssItem(X0) => ~ memberP(nil, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax38)).
fof(f20697, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssItem(X8)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20696, f630])).
fof(f20696, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssItem(sK57) | ~ ssItem(X8)) | ~ spl63_4), inference(subsumption_resolution, [], [f20660, f447])).
fof(f20660, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssList(nil) | ~ ssItem(sK57) | ~ ssItem(X8)) | ~ spl63_4), inference(superposition, [], [f474, f20224])).
fof(f474, plain, ! [X2, X0, X1] : (~ memberP(cons(X1, X2), X0) | (X0 = X1) | memberP(X2, X0) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f326])).
fof(f326, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & (memberP(X2, X0) | (X0 = X1) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f325])).
fof(f325, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & ((memberP(X2, X0) | (X0 = X1)) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => (memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax37)).
fof(f695597, plain, (memberP(sK53, sK59) | (~ spl63_9813 | ~ spl63_20953 | ~ spl63_20961 | ~ spl63_20963)), inference(subsumption_resolution, [], [f695596, f686729])).
fof(f695596, plain, (memberP(sK53, sK59) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl63_9813 | ~ spl63_20953 | ~ spl63_20963)), inference(subsumption_resolution, [], [f695595, f686675])).
fof(f695595, plain, (memberP(sK53, sK59) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl63_9813 | ~ spl63_20963)), inference(subsumption_resolution, [], [f695594, f447])).
fof(f695594, plain, (memberP(sK53, sK59) | ~ ssList(nil) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl63_9813 | ~ spl63_20963)), inference(subsumption_resolution, [], [f695593, f156694])).
fof(f695593, plain, (~ ssItem(sK59) | memberP(sK53, sK59) | ~ ssList(nil) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl63_20963), inference(duplicate_literal_removal, [], [f695588])).
fof(f695588, plain, (~ ssItem(sK59) | memberP(sK53, sK59) | ~ ssList(nil) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssItem(sK59) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl63_20963), inference(resolution, [], [f686738, f576])).
fof(f576, plain, ! [X2, X3, X1] : (memberP(app(X2, cons(X1, X3)), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(app(X2, cons(X1, X3)))), inference(equality_resolution, [], [f363])).
fof(f363, plain, ! [X2, X0, X3, X1] : (memberP(X0, X1) | ~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f241])).
fof(f241, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(sK8(X0, X1), cons(X1, sK9(X0, X1))) = X0) & ssList(sK9(X0, X1))) & ssList(sK8(X0, X1))) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f238, f240, f239])).
fof(f239, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(sK8(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) & ssList(sK8(X0, X1)))), introduced(choice_axiom, [])).
fof(f240, plain, ! [X1, X0] : (? [X5] : ((app(sK8(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) => ((app(sK8(X0, X1), cons(X1, sK9(X0, X1))) = X0) & ssList(sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f238, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(rectify, [], [f237])).
fof(f237, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f100])).
fof(f100, plain, ! [X0] : (! [X1] : ((memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax3)).
fof(f686738, plain, (! [X7] : (~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7) | ~ ssItem(X7) | memberP(sK53, X7)) | ~ spl63_20963), inference(avatar_component_clause, [], [f686737])).
fof(f692683, plain, (~ spl63_20961 | spl63_20963 | ~ spl63_10127), inference(avatar_split_clause, [], [f692682, f162224, f686737, f686728])).
fof(f692682, plain, (! [X7] : (memberP(sK53, X7) | ~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssItem(X7)) | ~ spl63_10127), inference(subsumption_resolution, [], [f690533, f162225])).
fof(f690533, plain, ! [X7] : (memberP(sK53, X7) | ~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7) | ~ ssList(sK62) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssItem(X7)), inference(superposition, [], [f472, f565])).
fof(f472, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X1, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f324])).
fof(f692671, plain, (~ spl63_20961 | spl63_20967 | ~ spl63_10127), inference(avatar_split_clause, [], [f692670, f162224, f686762, f686728])).
fof(f692670, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl63_10127), inference(subsumption_resolution, [], [f692669, f641])).
fof(f692669, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(sK53) | ~ spl63_10127), inference(subsumption_resolution, [], [f690544, f162225])).
fof(f690544, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(sK62) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(sK53)), inference(superposition, [], [f578, f565])).
fof(f692660, plain, (~ spl63_65 | ~ spl63_20953 | spl63_20961), inference(avatar_contradiction_clause, [], [f692659])).
fof(f692659, plain, ($false | (~ spl63_65 | ~ spl63_20953 | spl63_20961)), inference(subsumption_resolution, [], [f692658, f686675])).
fof(f692658, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl63_65 | spl63_20961)), inference(subsumption_resolution, [], [f692657, f2238])).
fof(f692657, plain, (~ ssList(cons(sK59, nil)) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | spl63_20961), inference(resolution, [], [f686730, f459])).
fof(f686730, plain, (~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | spl63_20961), inference(avatar_component_clause, [], [f686728])).
fof(f688494, plain, (~ spl63_12129 | ~ spl63_18875 | spl63_20953), inference(avatar_contradiction_clause, [], [f688493])).
fof(f688493, plain, ($false | (~ spl63_12129 | ~ spl63_18875 | spl63_20953)), inference(subsumption_resolution, [], [f688492, f226258])).
fof(f688492, plain, (~ ssList(app(sK60, cons(sK58, nil))) | (~ spl63_18875 | spl63_20953)), inference(subsumption_resolution, [], [f688491, f566096])).
fof(f688491, plain, (~ ssList(sK61) | ~ ssList(app(sK60, cons(sK58, nil))) | spl63_20953), inference(resolution, [], [f686676, f459])).
fof(f686676, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | spl63_20953), inference(avatar_component_clause, [], [f686674])).
fof(f677347, plain, (spl63_58 | spl63_32 | ~ spl63_4436 | spl63_4437), inference(avatar_split_clause, [], [f677346, f80779, f80775, f1186, f1800])).
fof(f1186, plain, (spl63_32 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl63_32])])).
fof(f80775, plain, (spl63_4436 <=> ssList(cons(sK58, sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_4436])])).
fof(f80779, plain, (spl63_4437 <=> (nil = cons(sK58, sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_4437])])).
fof(f677346, plain, ((sK60 = cons(hd(sK60), tl(sK60))) | (spl63_32 | ~ spl63_4436 | spl63_4437)), inference(subsumption_resolution, [], [f631734, f1187])).
fof(f1187, plain, (~ (nil = sK60) | spl63_32), inference(avatar_component_clause, [], [f1186])).
fof(f631734, plain, ((nil = sK60) | (sK60 = cons(hd(sK60), tl(sK60))) | (~ spl63_4436 | spl63_4437)), inference(subsumption_resolution, [], [f343059, f80780])).
fof(f80780, plain, (~ (nil = cons(sK58, sK60)) | spl63_4437), inference(avatar_component_clause, [], [f80779])).
fof(f343059, plain, ((nil = sK60) | (sK60 = cons(hd(sK60), tl(sK60))) | (nil = cons(sK58, sK60)) | ~ spl63_4436), inference(forward_demodulation, [], [f343058, f7522])).
fof(f7522, plain, (sK60 = tl(cons(sK58, sK60))), inference(resolution, [], [f1123, f560])).
fof(f1123, plain, ! [X53] : (~ ssItem(X53) | (sK60 = tl(cons(X53, sK60)))), inference(resolution, [], [f458, f562])).
fof(f343058, plain, ((sK60 = cons(hd(sK60), tl(sK60))) | (nil = cons(sK58, sK60)) | (nil = tl(cons(sK58, sK60))) | ~ spl63_4436), inference(forward_demodulation, [], [f114694, f7522])).
fof(f114694, plain, ((tl(cons(sK58, sK60)) = cons(hd(tl(cons(sK58, sK60))), tl(tl(cons(sK58, sK60))))) | (nil = cons(sK58, sK60)) | (nil = tl(cons(sK58, sK60))) | ~ spl63_4436), inference(resolution, [], [f80776, f1494])).
fof(f1494, plain, ! [X4] : (~ ssList(X4) | (tl(X4) = cons(hd(tl(X4)), tl(tl(X4)))) | (nil = X4) | (nil = tl(X4))), inference(resolution, [], [f532, f457])).
fof(f457, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax24)).
fof(f80776, plain, (ssList(cons(sK58, sK60)) | ~ spl63_4436), inference(avatar_component_clause, [], [f80775])).
fof(f674770, plain, (~ spl63_43 | ~ spl63_44 | spl63_5008 | ~ spl63_3715 | ~ spl63_4 | ~ spl63_5 | ~ spl63_59), inference(avatar_split_clause, [], [f624496, f1805, f628, f623, f72470, f93772, f1662, f1658])).
fof(f1658, plain, (spl63_43 <=> ssItem(sK50(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_43])])).
fof(f1662, plain, (spl63_44 <=> ssList(sK49(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl63_44])])).
fof(f1805, plain, (spl63_59 <=> (sK60 = cons(sK50(sK60), sK49(sK60)))), introduced(avatar_definition, [new_symbols(naming, [spl63_59])])).
fof(f624496, plain, (~ frontsegP(sK53, sK60) | (sK57 = sK50(sK60)) | ~ ssList(sK49(sK60)) | ~ ssItem(sK50(sK60)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_59)), inference(superposition, [], [f20702, f1807])).
fof(f1807, plain, ((sK60 = cons(sK50(sK60), sK49(sK60))) | ~ spl63_59), inference(avatar_component_clause, [], [f1805])).
fof(f635219, plain, (spl63_32 | ~ spl63_6175 | ~ spl63_11908 | ~ spl63_13844), inference(avatar_split_clause, [], [f635218, f262097, f221223, f113302, f1186])).
fof(f113302, plain, (spl63_6175 <=> (sK60 = sK52(cons(sK58, sK60)))), introduced(avatar_definition, [new_symbols(naming, [spl63_6175])])).
fof(f635218, plain, ((nil = sK60) | (~ spl63_6175 | ~ spl63_11908 | ~ spl63_13844)), inference(forward_demodulation, [], [f635008, f262099])).
fof(f262099, plain, ((nil = sK52(cons(sK58, nil))) | ~ spl63_13844), inference(avatar_component_clause, [], [f262097])).
fof(f635008, plain, ((sK60 = sK52(cons(sK58, nil))) | (~ spl63_6175 | ~ spl63_11908)), inference(backward_demodulation, [], [f113304, f221225])).
fof(f221225, plain, ((cons(sK58, nil) = cons(sK58, sK60)) | ~ spl63_11908), inference(avatar_component_clause, [], [f221223])).
fof(f113304, plain, ((sK60 = sK52(cons(sK58, sK60))) | ~ spl63_6175), inference(avatar_component_clause, [], [f113302])).
fof(f634890, plain, (~ spl63_5187 | spl63_32), inference(avatar_split_clause, [], [f634889, f1186, f99930])).
fof(f634889, plain, ((nil = sK60) | ~ frontsegP(nil, sK60)), inference(subsumption_resolution, [], [f434112, f447])).
fof(f434112, plain, (~ ssList(nil) | (nil = sK60) | ~ frontsegP(nil, sK60)), inference(duplicate_literal_removal, [], [f434108])).
fof(f434108, plain, (~ ssList(nil) | (nil = sK60) | ~ frontsegP(nil, sK60) | ~ ssList(nil)), inference(resolution, [], [f71810, f481])).
fof(f481, plain, ! [X0] : (frontsegP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f154])).
fof(f154, plain, ! [X0] : (frontsegP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f42])).
fof(f42, plain, ! [X0] : (ssList(X0) => frontsegP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax42)).
fof(f71810, plain, ! [X13] : (~ frontsegP(nil, X13) | ~ ssList(X13) | (sK60 = X13) | ~ frontsegP(X13, sK60)), inference(subsumption_resolution, [], [f71809, f447])).
fof(f71809, plain, ! [X13] : (~ frontsegP(X13, sK60) | ~ ssList(X13) | ~ ssList(nil) | (sK60 = X13) | ~ frontsegP(nil, X13)), inference(subsumption_resolution, [], [f71790, f562])).
fof(f71790, plain, ! [X13] : (~ frontsegP(X13, sK60) | ~ ssList(sK60) | ~ ssList(X13) | ~ ssList(nil) | (sK60 = X13) | ~ frontsegP(nil, X13)), inference(superposition, [], [f1733, f904])).
fof(f904, plain, (sK60 = app(nil, sK60)), inference(resolution, [], [f461, f562])).
fof(f587566, plain, (~ spl63_157 | spl63_169 | ~ spl63_3716), inference(avatar_contradiction_clause, [], [f587565])).
fof(f587565, plain, ($false | (~ spl63_157 | spl63_169 | ~ spl63_3716)), inference(subsumption_resolution, [], [f587564, f3553])).
fof(f587564, plain, (~ ssList(tl(sK60)) | (spl63_169 | ~ spl63_3716)), inference(subsumption_resolution, [], [f587553, f3605])).
fof(f3605, plain, (~ (nil = tl(sK60)) | spl63_169), inference(avatar_component_clause, [], [f3604])).
fof(f587553, plain, ((nil = tl(sK60)) | ~ ssList(tl(sK60)) | ~ spl63_3716), inference(resolution, [], [f72479, f487])).
fof(f487, plain, ! [X0] : (~ frontsegP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f329])).
fof(f329, plain, ! [X0] : (((frontsegP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ frontsegP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f159])).
fof(f159, plain, ! [X0] : ((frontsegP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f46])).
fof(f46, plain, ! [X0] : (ssList(X0) => (frontsegP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax46)).
fof(f72479, plain, (frontsegP(nil, tl(sK60)) | ~ spl63_3716), inference(avatar_component_clause, [], [f72477])).
fof(f568770, plain, spl63_18875, inference(avatar_split_clause, [], [f563, f566095])).
fof(f563, plain, ssList(sK61), inference(cnf_transformation, [], [f354])).
fof(f389310, plain, (~ spl63_2901 | spl63_347 | ~ spl63_883), inference(avatar_split_clause, [], [f389309, f18726, f5860, f54157])).
fof(f54157, plain, (spl63_2901 <=> (nil = cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_2901])])).
fof(f389309, plain, (~ (nil = cons(sK58, sK53)) | (spl63_347 | ~ spl63_883)), inference(subsumption_resolution, [], [f387414, f18727])).
fof(f387414, plain, (~ (nil = cons(sK58, sK53)) | ~ ssList(cons(sK58, nil)) | spl63_347), inference(subsumption_resolution, [], [f387413, f641])).
fof(f387413, plain, (~ (nil = cons(sK58, sK53)) | ~ ssList(sK53) | ~ ssList(cons(sK58, nil)) | spl63_347), inference(subsumption_resolution, [], [f223538, f5861])).
fof(f223538, plain, (~ (nil = cons(sK58, sK53)) | (nil = sK53) | ~ ssList(sK53) | ~ ssList(cons(sK58, nil))), inference(superposition, [], [f537, f25558])).
fof(f537, plain, ! [X0, X1] : (~ (nil = app(X0, X1)) | (nil = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f341])).
fof(f341, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | ~ (nil = X0) | ~ (nil = X1)) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f340])).
fof(f340, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | (~ (nil = X0) | ~ (nil = X1))) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f201])).
fof(f201, plain, ! [X0] : (! [X1] : (((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax83)).
fof(f366033, plain, (~ spl63_561 | ~ spl63_883 | spl63_884), inference(avatar_contradiction_clause, [], [f366032])).
fof(f366032, plain, ($false | (~ spl63_561 | ~ spl63_883 | spl63_884)), inference(subsumption_resolution, [], [f366031, f18727])).
fof(f366031, plain, (~ ssList(cons(sK58, nil)) | (~ spl63_561 | spl63_884)), inference(subsumption_resolution, [], [f366030, f563])).
fof(f366030, plain, (~ ssList(sK61) | ~ ssList(cons(sK58, nil)) | (~ spl63_561 | spl63_884)), inference(subsumption_resolution, [], [f366008, f18731])).
fof(f18731, plain, (~ (nil = cons(sK58, nil)) | spl63_884), inference(avatar_component_clause, [], [f18730])).
fof(f366008, plain, ((nil = cons(sK58, nil)) | ~ ssList(sK61) | ~ ssList(cons(sK58, nil)) | ~ spl63_561), inference(trivial_inequality_removal, [], [f365982])).
fof(f365982, plain, (~ (nil = nil) | (nil = cons(sK58, nil)) | ~ ssList(sK61) | ~ ssList(cons(sK58, nil)) | ~ spl63_561), inference(superposition, [], [f538, f321036])).
fof(f321036, plain, ((nil = app(cons(sK58, nil), sK61)) | ~ spl63_561), inference(backward_demodulation, [], [f10438, f13277])).
fof(f13277, plain, ((nil = cons(sK58, sK61)) | ~ spl63_561), inference(avatar_component_clause, [], [f13275])).
fof(f13275, plain, (spl63_561 <=> (nil = cons(sK58, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl63_561])])).
fof(f10438, plain, (cons(sK58, sK61) = app(cons(sK58, nil), sK61)), inference(resolution, [], [f2668, f560])).
fof(f2668, plain, ! [X4] : (~ ssItem(X4) | (cons(X4, sK61) = app(cons(X4, nil), sK61))), inference(resolution, [], [f563, f535])).
fof(f538, plain, ! [X0, X1] : (~ (nil = app(X0, X1)) | (nil = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f341])).
fof(f346855, plain, (~ spl63_14402 | spl63_14403 | ~ spl63_36 | ~ spl63_3370 | ~ spl63_13574), inference(avatar_split_clause, [], [f346840, f259527, f59357, f1204, f346852, f346848])).
fof(f346840, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | (~ spl63_36 | ~ spl63_3370 | ~ spl63_13574)), inference(trivial_inequality_removal, [], [f346801])).
fof(f346801, plain, (~ (sK53 = sK53) | (sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | (~ spl63_36 | ~ spl63_3370 | ~ spl63_13574)), inference(superposition, [], [f21084, f290655])).
fof(f290655, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), sK53), nil)) | (~ spl63_36 | ~ spl63_3370 | ~ spl63_13574)), inference(forward_demodulation, [], [f262851, f259529])).
fof(f262851, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK57, nil)), nil)) | (~ spl63_36 | ~ spl63_3370)), inference(forward_demodulation, [], [f262850, f59359])).
fof(f262850, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), nil)) | ~ spl63_36), inference(forward_demodulation, [], [f565, f1206])).
fof(f1206, plain, ((nil = sK62) | ~ spl63_36), inference(avatar_component_clause, [], [f1204])).
fof(f21084, plain, ! [X7] : (~ (sK53 = app(X7, nil)) | (sK53 = X7) | ~ ssList(X7)), inference(subsumption_resolution, [], [f21083, f447])).
fof(f21083, plain, ! [X7] : (~ (sK53 = app(X7, nil)) | (sK53 = X7) | ~ ssList(nil) | ~ ssList(X7)), inference(subsumption_resolution, [], [f21065, f641])).
fof(f21065, plain, ! [X7] : (~ (sK53 = app(X7, nil)) | (sK53 = X7) | ~ ssList(sK53) | ~ ssList(nil) | ~ ssList(X7)), inference(superposition, [], [f533, f5726])).
fof(f5726, plain, (sK53 = app(sK53, nil)), inference(resolution, [], [f641, f540])).
fof(f533, plain, ! [X2, X0, X1] : (~ (app(X2, X1) = app(X0, X1)) | (X0 = X2) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ! [X0] : (! [X1] : (! [X2] : ((X0 = X2) | ~ (app(X2, X1) = app(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f195])).
fof(f195, plain, ! [X0] : (! [X1] : (! [X2] : (((X0 = X2) | ~ (app(X2, X1) = app(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((app(X2, X1) = app(X0, X1)) => (X0 = X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax79)).
fof(f343101, plain, (spl63_4437 | spl63_6175 | ~ spl63_4436), inference(avatar_split_clause, [], [f343100, f80775, f113302, f80779])).
fof(f343100, plain, ((sK60 = sK52(cons(sK58, sK60))) | (nil = cons(sK58, sK60)) | ~ spl63_4436), inference(forward_demodulation, [], [f114574, f7522])).
fof(f114574, plain, ((nil = cons(sK58, sK60)) | (tl(cons(sK58, sK60)) = sK52(cons(sK58, sK60))) | ~ spl63_4436), inference(resolution, [], [f80776, f530])).
fof(f342975, plain, (~ spl63_4437 | ~ spl63_883 | spl63_884 | ~ spl63_14322), inference(avatar_split_clause, [], [f342974, f335074, f18730, f18726, f80779])).
fof(f342974, plain, (~ (nil = cons(sK58, sK60)) | (~ spl63_883 | spl63_884 | ~ spl63_14322)), inference(subsumption_resolution, [], [f321537, f335075])).
fof(f321537, plain, (~ (nil = cons(sK58, sK60)) | ~ ssList(sK60) | (~ spl63_883 | spl63_884)), inference(subsumption_resolution, [], [f321536, f18727])).
fof(f321536, plain, (~ (nil = cons(sK58, sK60)) | ~ ssList(sK60) | ~ ssList(cons(sK58, nil)) | spl63_884), inference(subsumption_resolution, [], [f221167, f18731])).
fof(f221167, plain, (~ (nil = cons(sK58, sK60)) | (nil = cons(sK58, nil)) | ~ ssList(sK60) | ~ ssList(cons(sK58, nil))), inference(superposition, [], [f538, f8025])).
fof(f8025, plain, (cons(sK58, sK60) = app(cons(sK58, nil), sK60)), inference(resolution, [], [f1605, f560])).
fof(f1605, plain, ! [X71] : (~ ssItem(X71) | (cons(X71, sK60) = app(cons(X71, nil), sK60))), inference(resolution, [], [f535, f562])).
fof(f338238, plain, spl63_14322, inference(avatar_split_clause, [], [f562, f335074])).
fof(f335236, plain, (spl63_59 | spl63_32 | ~ spl63_4436 | spl63_4437 | ~ spl63_6175), inference(avatar_split_clause, [], [f335235, f113302, f80779, f80775, f1186, f1805])).
fof(f335235, plain, ((nil = sK60) | (sK60 = cons(sK50(sK60), sK49(sK60))) | (~ spl63_4436 | spl63_4437 | ~ spl63_6175)), inference(forward_demodulation, [], [f335234, f335105])).
fof(f335105, plain, ((sK60 = tl(cons(sK58, sK60))) | (~ spl63_4436 | spl63_4437 | ~ spl63_6175)), inference(forward_demodulation, [], [f335104, f113304])).
fof(f335104, plain, ((tl(cons(sK58, sK60)) = sK52(cons(sK58, sK60))) | (~ spl63_4436 | spl63_4437)), inference(subsumption_resolution, [], [f114574, f80780])).
fof(f335234, plain, ((sK60 = cons(sK50(sK60), sK49(sK60))) | (nil = tl(cons(sK58, sK60))) | (~ spl63_4436 | spl63_4437)), inference(subsumption_resolution, [], [f113658, f80780])).
fof(f113658, plain, ((sK60 = cons(sK50(sK60), sK49(sK60))) | (nil = cons(sK58, sK60)) | (nil = tl(cons(sK58, sK60))) | ~ spl63_4436), inference(forward_demodulation, [], [f113090, f7522])).
fof(f113090, plain, ((tl(cons(sK58, sK60)) = cons(sK50(tl(cons(sK58, sK60))), sK49(tl(cons(sK58, sK60))))) | (nil = cons(sK58, sK60)) | (nil = tl(cons(sK58, sK60))) | ~ spl63_4436), inference(resolution, [], [f80776, f1459])).
fof(f1459, plain, ! [X4] : (~ ssList(X4) | (tl(X4) = cons(sK50(tl(X4)), sK49(tl(X4)))) | (nil = X4) | (nil = tl(X4))), inference(resolution, [], [f453, f457])).
fof(f453, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(sK50(X0), sK49(X0)) = X0)), inference(cnf_transformation, [], [f320])).
fof(f322407, plain, spl63_14309, inference(avatar_split_clause, [], [f71224, f321713])).
fof(f71224, plain, (nil = tl(cons(sK58, nil))), inference(resolution, [], [f560, f1099])).
fof(f314315, plain, (~ spl63_4 | ~ spl63_5 | ~ spl63_349 | ~ spl63_997 | ~ spl63_2112 | spl63_3108 | ~ spl63_3370 | ~ spl63_13574), inference(avatar_contradiction_clause, [], [f314314])).
fof(f314314, plain, ($false | (~ spl63_4 | ~ spl63_5 | ~ spl63_349 | ~ spl63_997 | ~ spl63_2112 | spl63_3108 | ~ spl63_3370 | ~ spl63_13574)), inference(subsumption_resolution, [], [f314313, f56388])).
fof(f56388, plain, (~ (sK57 = sK58) | spl63_3108), inference(avatar_component_clause, [], [f56387])).
fof(f56387, plain, (spl63_3108 <=> (sK57 = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl63_3108])])).
fof(f314313, plain, ((sK57 = sK58) | (~ spl63_4 | ~ spl63_5 | ~ spl63_349 | ~ spl63_997 | ~ spl63_2112 | ~ spl63_3370 | ~ spl63_13574)), inference(forward_demodulation, [], [f300673, f313005])).
fof(f313005, plain, ((sK57 = hd(sK53)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_349)), inference(backward_demodulation, [], [f5872, f20278])).
fof(f300673, plain, ((sK58 = hd(sK53)) | (~ spl63_997 | ~ spl63_2112 | ~ spl63_3370 | ~ spl63_13574)), inference(forward_demodulation, [], [f32859, f292088])).
fof(f292088, plain, ((sK53 = app(cons(sK58, sK61), sK53)) | (~ spl63_997 | ~ spl63_3370 | ~ spl63_13574)), inference(forward_demodulation, [], [f269723, f259529])).
fof(f269723, plain, ((sK53 = app(cons(sK58, sK61), cons(sK57, nil))) | (~ spl63_997 | ~ spl63_3370)), inference(forward_demodulation, [], [f20809, f59359])).
fof(f20809, plain, ((sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | ~ spl63_997), inference(avatar_component_clause, [], [f20807])).
fof(f20807, plain, (spl63_997 <=> (sK53 = app(cons(sK58, sK61), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_997])])).
fof(f32859, plain, ((sK58 = hd(app(cons(sK58, sK61), sK53))) | ~ spl63_2112), inference(avatar_component_clause, [], [f32857])).
fof(f32857, plain, (spl63_2112 <=> (sK58 = hd(app(cons(sK58, sK61), sK53)))), introduced(avatar_definition, [new_symbols(naming, [spl63_2112])])).
fof(f305087, plain, (~ spl63_4 | ~ spl63_5 | ~ spl63_348 | spl63_2967 | ~ spl63_13585), inference(avatar_contradiction_clause, [], [f305086])).
fof(f305086, plain, ($false | (~ spl63_4 | ~ spl63_5 | ~ spl63_348 | spl63_2967 | ~ spl63_13585)), inference(subsumption_resolution, [], [f304863, f20280])).
fof(f20280, plain, ((nil = sK52(sK53)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_348)), inference(backward_demodulation, [], [f5867, f20226])).
fof(f5867, plain, ((tl(sK53) = sK52(sK53)) | ~ spl63_348), inference(avatar_component_clause, [], [f5865])).
fof(f5865, plain, (spl63_348 <=> (tl(sK53) = sK52(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_348])])).
fof(f304863, plain, (~ (nil = sK52(sK53)) | (spl63_2967 | ~ spl63_13585)), inference(backward_demodulation, [], [f54823, f259691])).
fof(f259691, plain, ((sK53 = cons(sK58, sK53)) | ~ spl63_13585), inference(avatar_component_clause, [], [f259689])).
fof(f54823, plain, (~ (nil = sK52(cons(sK58, sK53))) | spl63_2967), inference(avatar_component_clause, [], [f54822])).
fof(f54822, plain, (spl63_2967 <=> (nil = sK52(cons(sK58, sK53)))), introduced(avatar_definition, [new_symbols(naming, [spl63_2967])])).
fof(f264934, plain, (spl63_347 | ~ spl63_3755 | ~ spl63_10054), inference(avatar_contradiction_clause, [], [f264933])).
fof(f264933, plain, ($false | (spl63_347 | ~ spl63_3755 | ~ spl63_10054)), inference(subsumption_resolution, [], [f264932, f259615])).
fof(f259615, plain, (~ frontsegP(nil, sK53) | spl63_347), inference(subsumption_resolution, [], [f259614, f5861])).
fof(f259614, plain, ((nil = sK53) | ~ frontsegP(nil, sK53)), inference(subsumption_resolution, [], [f248721, f447])).
fof(f248721, plain, (~ ssList(nil) | (nil = sK53) | ~ frontsegP(nil, sK53)), inference(duplicate_literal_removal, [], [f248719])).
fof(f248719, plain, (~ ssList(nil) | (nil = sK53) | ~ frontsegP(nil, sK53) | ~ ssList(nil)), inference(resolution, [], [f44448, f486])).
fof(f486, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f45])).
fof(f45, plain, ! [X0] : (ssList(X0) => frontsegP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax45)).
fof(f44448, plain, ! [X4] : (~ frontsegP(nil, X4) | ~ ssList(X4) | (sK53 = X4) | ~ frontsegP(X4, sK53)), inference(subsumption_resolution, [], [f44447, f447])).
fof(f44447, plain, ! [X4] : (~ frontsegP(X4, sK53) | ~ ssList(X4) | ~ ssList(nil) | (sK53 = X4) | ~ frontsegP(nil, X4)), inference(subsumption_resolution, [], [f44440, f641])).
fof(f44440, plain, ! [X4] : (~ frontsegP(X4, sK53) | ~ ssList(sK53) | ~ ssList(X4) | ~ ssList(nil) | (sK53 = X4) | ~ frontsegP(nil, X4)), inference(superposition, [], [f1733, f5720])).
fof(f5720, plain, (sK53 = app(nil, sK53)), inference(resolution, [], [f641, f461])).
fof(f264932, plain, (frontsegP(nil, sK53) | (~ spl63_3755 | ~ spl63_10054)), inference(forward_demodulation, [], [f161158, f73640])).
fof(f73640, plain, ((nil = app(app(sK60, cons(sK58, nil)), nil)) | ~ spl63_3755), inference(avatar_component_clause, [], [f73638])).
fof(f73638, plain, (spl63_3755 <=> (nil = app(app(sK60, cons(sK58, nil)), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_3755])])).
fof(f263593, plain, (spl63_13574 | ~ spl63_4), inference(avatar_split_clause, [], [f20224, f623, f259527])).
fof(f257780, plain, (spl63_13530 | spl63_13543 | ~ spl63_144 | ~ spl63_883), inference(avatar_split_clause, [], [f257779, f18726, f3220, f257674, f257540])).
fof(f3220, plain, (spl63_144 <=> ssList(app(app(sK60, cons(sK58, nil)), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_144])])).
fof(f257779, plain, ((app(sK60, cons(sK58, nil)) = cons(hd(app(sK60, cons(sK58, nil))), tl(app(sK60, cons(sK58, nil))))) | (nil = app(sK60, cons(sK58, nil))) | (~ spl63_144 | ~ spl63_883)), inference(forward_demodulation, [], [f257778, f249707])).
fof(f257778, plain, ((nil = app(sK60, cons(sK58, nil))) | (app(app(sK60, cons(sK58, nil)), nil) = cons(hd(app(app(sK60, cons(sK58, nil)), nil)), tl(app(app(sK60, cons(sK58, nil)), nil)))) | (~ spl63_144 | ~ spl63_883)), inference(forward_demodulation, [], [f170459, f249707])).
fof(f170459, plain, ((nil = app(app(sK60, cons(sK58, nil)), nil)) | (app(app(sK60, cons(sK58, nil)), nil) = cons(hd(app(app(sK60, cons(sK58, nil)), nil)), tl(app(app(sK60, cons(sK58, nil)), nil)))) | ~ spl63_144), inference(resolution, [], [f3221, f532])).
fof(f3221, plain, (ssList(app(app(sK60, cons(sK58, nil)), nil)) | ~ spl63_144), inference(avatar_component_clause, [], [f3220])).
fof(f249936, plain, (spl63_12129 | ~ spl63_144 | ~ spl63_883), inference(avatar_split_clause, [], [f249755, f18726, f3220, f226257])).
fof(f249755, plain, (ssList(app(sK60, cons(sK58, nil))) | (~ spl63_144 | ~ spl63_883)), inference(backward_demodulation, [], [f3221, f249707])).
fof(f249932, plain, (~ spl63_12130 | ~ spl63_883 | spl63_10054), inference(avatar_split_clause, [], [f249875, f161157, f18726, f226261])).
fof(f249875, plain, (~ frontsegP(app(sK60, cons(sK58, nil)), sK53) | (~ spl63_883 | spl63_10054)), inference(backward_demodulation, [], [f161159, f249707])).
fof(f161159, plain, (~ frontsegP(app(app(sK60, cons(sK58, nil)), nil), sK53) | spl63_10054), inference(avatar_component_clause, [], [f161157])).
fof(f223614, plain, (spl63_11976 | ~ spl63_11977 | ~ spl63_883), inference(avatar_split_clause, [], [f223605, f18726, f223611, f223607])).
fof(f223605, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | (cons(sK58, nil) = cons(sK58, sK53)) | ~ spl63_883), inference(subsumption_resolution, [], [f223604, f641])).
fof(f223604, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | (cons(sK58, nil) = cons(sK58, sK53)) | ~ ssList(sK53) | ~ spl63_883), inference(subsumption_resolution, [], [f223547, f18727])).
fof(f223547, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | ~ ssList(cons(sK58, nil)) | (cons(sK58, nil) = cons(sK58, sK53)) | ~ ssList(sK53)), inference(superposition, [], [f1631, f25558])).
fof(f1631, plain, ! [X0, X1] : (~ frontsegP(X1, app(X1, X0)) | ~ ssList(X1) | (app(X1, X0) = X1) | ~ ssList(X0)), inference(subsumption_resolution, [], [f1630, f459])).
fof(f1630, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0))), inference(duplicate_literal_removal, [], [f1623])).
fof(f1623, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0)) | ~ ssList(app(X1, X0)) | ~ ssList(X1)), inference(resolution, [], [f578, f480])).
fof(f201029, plain, (spl63_3483 | ~ spl63_4 | ~ spl63_5 | ~ spl63_223 | ~ spl63_269 | ~ spl63_5004 | ~ spl63_10191), inference(avatar_split_clause, [], [f201028, f172390, f92926, f4778, f3916, f628, f623, f63706])).
fof(f3916, plain, (spl63_223 <=> ssList(tl(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_223])])).
fof(f92926, plain, (spl63_5004 <=> leq(sK57, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl63_5004])])).
fof(f172390, plain, (spl63_10191 <=> (sK57 = hd(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_10191])])).
fof(f201028, plain, (! [X64] : (~ sP0(app(app(X64, sK53), sK62)) | ~ ssList(X64)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_223 | ~ spl63_269 | ~ spl63_5004 | ~ spl63_10191)), inference(subsumption_resolution, [], [f201027, f630])).
fof(f201027, plain, (! [X64] : (~ sP0(app(app(X64, sK53), sK62)) | ~ ssList(X64) | ~ ssItem(sK57)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_223 | ~ spl63_269 | ~ spl63_5004 | ~ spl63_10191)), inference(subsumption_resolution, [], [f201026, f3917])).
fof(f3917, plain, (ssList(tl(sK62)) | ~ spl63_223), inference(avatar_component_clause, [], [f3916])).
fof(f201026, plain, (! [X64] : (~ sP0(app(app(X64, sK53), sK62)) | ~ ssList(tl(sK62)) | ~ ssList(X64) | ~ ssItem(sK57)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_269 | ~ spl63_5004 | ~ spl63_10191)), inference(subsumption_resolution, [], [f197309, f92927])).
fof(f92927, plain, (leq(sK57, sK57) | ~ spl63_5004), inference(avatar_component_clause, [], [f92926])).
fof(f197309, plain, (! [X64] : (~ sP0(app(app(X64, sK53), sK62)) | ~ leq(sK57, sK57) | ~ ssList(tl(sK62)) | ~ ssList(X64) | ~ ssItem(sK57)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_269 | ~ spl63_10191)), inference(duplicate_literal_removal, [], [f197294])).
fof(f197294, plain, (! [X64] : (~ sP0(app(app(X64, sK53), sK62)) | ~ leq(sK57, sK57) | ~ ssList(tl(sK62)) | ~ ssList(X64) | ~ ssItem(sK57) | ~ leq(sK57, sK57)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_269 | ~ spl63_10191)), inference(superposition, [], [f20708, f196200])).
fof(f196200, plain, ((sK62 = cons(sK57, tl(sK62))) | (~ spl63_269 | ~ spl63_10191)), inference(backward_demodulation, [], [f4780, f172392])).
fof(f172392, plain, ((sK57 = hd(sK62)) | ~ spl63_10191), inference(avatar_component_clause, [], [f172390])).
fof(f4780, plain, ((sK62 = cons(hd(sK62), tl(sK62))) | ~ spl63_269), inference(avatar_component_clause, [], [f4778])).
fof(f20708, plain, (! [X21, X19, X20] : (~ sP0(app(app(X19, sK53), cons(X20, X21))) | ~ leq(sK57, X20) | ~ ssList(X21) | ~ ssList(X19) | ~ ssItem(X20) | ~ leq(X20, sK57)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20707, f630])).
fof(f20707, plain, (! [X21, X19, X20] : (~ sP0(app(app(X19, sK53), cons(X20, X21))) | ~ leq(sK57, X20) | ~ ssList(X21) | ~ ssList(X19) | ~ ssItem(X20) | ~ ssItem(sK57) | ~ leq(X20, sK57)) | ~ spl63_4), inference(subsumption_resolution, [], [f20671, f447])).
fof(f20671, plain, (! [X21, X19, X20] : (~ sP0(app(app(X19, sK53), cons(X20, X21))) | ~ leq(sK57, X20) | ~ ssList(X21) | ~ ssList(nil) | ~ ssList(X19) | ~ ssItem(X20) | ~ ssItem(sK57) | ~ leq(X20, sK57)) | ~ spl63_4), inference(superposition, [], [f581, f20224])).
fof(f581, plain, ! [X6, X10, X8, X7, X9] : (~ sP0(app(app(X8, cons(X6, X9)), cons(X7, X10))) | ~ leq(X6, X7) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ leq(X7, X6)), inference(equality_resolution, [], [f379])).
fof(f379, plain, ! [X6, X0, X10, X8, X7, X9] : (~ leq(X7, X6) | ~ leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ sP0(X0)), inference(cnf_transformation, [], [f267])).
fof(f267, plain, ! [X0] : ((sP0(X0) | (((((leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), sK19(X0))) = X0) & ssList(sK19(X0))) & ssList(sK18(X0))) & ssList(sK17(X0))) & ssItem(sK16(X0))) & ssItem(sK15(X0)))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (~ leq(X7, X6) | ~ leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ sP0(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16, sK17, sK18, sK19])], [f261, f266, f265, f264, f263, f262])).
fof(f262, plain, ! [X0] : (? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, X1) & leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1)) => (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, sK15(X0)) & leq(sK15(X0), X2) & (app(app(X3, cons(sK15(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(sK15(X0)))), introduced(choice_axiom, [])).
fof(f263, plain, ! [X0] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, sK15(X0)) & leq(sK15(X0), X2) & (app(app(X3, cons(sK15(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(X3, cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(sK16(X0)))), introduced(choice_axiom, [])).
fof(f264, plain, ! [X0] : (? [X3] : (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(X3, cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) => (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(sK17(X0)))), introduced(choice_axiom, [])).
fof(f265, plain, ! [X0] : (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(sK18(X0)))), introduced(choice_axiom, [])).
fof(f266, plain, ! [X0] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), X5)) = X0) & ssList(X5)) => (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), sK19(X0))) = X0) & ssList(sK19(X0)))), introduced(choice_axiom, [])).
fof(f261, plain, ! [X0] : ((sP0(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, X1) & leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (~ leq(X7, X6) | ~ leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ sP0(X0))), inference(rectify, [], [f260])).
fof(f260, plain, ! [X0] : ((sP0(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, X1) & leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1)) | ~ sP0(X0))), inference(nnf_transformation, [], [f224])).
fof(f224, plain, ! [X0] : (sP0(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), inference(usedef, [], [e224])).
fof(e224, plain, ! [X0] : (sP0(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f196152, plain, (spl63_10191 | ~ spl63_4 | ~ spl63_5 | ~ spl63_224 | ~ spl63_253 | ~ spl63_4146), inference(avatar_split_clause, [], [f196151, f75810, f4047, f3920, f628, f623, f172390])).
fof(f3920, plain, (spl63_224 <=> ssItem(hd(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_224])])).
fof(f4047, plain, (spl63_253 <=> memberP(sK62, hd(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_253])])).
fof(f196151, plain, ((sK57 = hd(sK62)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_224 | ~ spl63_253 | ~ spl63_4146)), inference(subsumption_resolution, [], [f195972, f3921])).
fof(f3921, plain, (ssItem(hd(sK62)) | ~ spl63_224), inference(avatar_component_clause, [], [f3920])).
fof(f195972, plain, ((sK57 = hd(sK62)) | ~ ssItem(hd(sK62)) | (~ spl63_4 | ~ spl63_5 | ~ spl63_224 | ~ spl63_253 | ~ spl63_4146)), inference(resolution, [], [f171892, f20698])).
fof(f171892, plain, (memberP(sK53, hd(sK62)) | (~ spl63_224 | ~ spl63_253 | ~ spl63_4146)), inference(subsumption_resolution, [], [f171861, f3921])).
fof(f171861, plain, (~ ssItem(hd(sK62)) | memberP(sK53, hd(sK62)) | (~ spl63_253 | ~ spl63_4146)), inference(resolution, [], [f4049, f75811])).
fof(f75811, plain, (! [X8] : (~ memberP(sK62, X8) | ~ ssItem(X8) | memberP(sK53, X8)) | ~ spl63_4146), inference(avatar_component_clause, [], [f75810])).
fof(f4049, plain, (memberP(sK62, hd(sK62)) | ~ spl63_253), inference(avatar_component_clause, [], [f4047])).
fof(f168149, plain, (spl63_3755 | spl63_3899 | ~ spl63_144), inference(avatar_split_clause, [], [f98345, f3220, f74302, f73638])).
fof(f98345, plain, (! [X82] : ((hd(X82) = hd(app(X82, tl(app(app(sK60, cons(sK58, nil)), nil))))) | ~ ssList(X82) | (nil = app(app(sK60, cons(sK58, nil)), nil)) | (nil = X82)) | ~ spl63_144), inference(resolution, [], [f3221, f1845])).
fof(f1845, plain, ! [X8, X7] : (~ ssList(X8) | (hd(X7) = hd(app(X7, tl(X8)))) | ~ ssList(X7) | (nil = X8) | (nil = X7)), inference(resolution, [], [f541, f457])).
fof(f541, plain, ! [X0, X1] : (~ ssList(X1) | (nil = X0) | (hd(X0) = hd(app(X0, X1))) | ~ ssList(X0)), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ! [X0] : (! [X1] : ((hd(X0) = hd(app(X0, X1))) | (nil = X0) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f203])).
fof(f203, plain, ! [X0] : (! [X1] : (((hd(X0) = hd(app(X0, X1))) | (nil = X0)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (~ (nil = X0) => (hd(X0) = hd(app(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax85)).
fof(f162532, plain, spl63_10127, inference(avatar_split_clause, [], [f564, f162224])).
fof(f564, plain, ssList(sK62), inference(cnf_transformation, [], [f354])).
fof(f157336, plain, (spl63_5004 | ~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_4989), inference(avatar_split_clause, [], [f120606, f91410, f5860, f628, f623, f92926])).
fof(f91410, plain, (spl63_4989 <=> totalorderedP(cons(sK57, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_4989])])).
fof(f120606, plain, (leq(sK57, sK57) | (~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_4989)), inference(forward_demodulation, [], [f120605, f20225])).
fof(f120605, plain, (leq(sK57, hd(sK53)) | (~ spl63_5 | spl63_347 | ~ spl63_4989)), inference(subsumption_resolution, [], [f120604, f630])).
fof(f120604, plain, (leq(sK57, hd(sK53)) | ~ ssItem(sK57) | (spl63_347 | ~ spl63_4989)), inference(subsumption_resolution, [], [f120603, f641])).
fof(f120603, plain, (leq(sK57, hd(sK53)) | ~ ssList(sK53) | ~ ssItem(sK57) | (spl63_347 | ~ spl63_4989)), inference(subsumption_resolution, [], [f120601, f5861])).
fof(f120601, plain, ((nil = sK53) | leq(sK57, hd(sK53)) | ~ ssList(sK53) | ~ ssItem(sK57) | ~ spl63_4989), inference(resolution, [], [f91412, f513])).
fof(f513, plain, ! [X0, X1] : (~ totalorderedP(cons(X0, X1)) | (nil = X1) | leq(X0, hd(X1)) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f333])).
fof(f333, plain, ! [X0] : (! [X1] : (((totalorderedP(cons(X0, X1)) | ((~ leq(X0, hd(X1)) | ~ totalorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & ((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1) | ~ totalorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f332])).
fof(f332, plain, ! [X0] : (! [X1] : (((totalorderedP(cons(X0, X1)) | ((~ leq(X0, hd(X1)) | ~ totalorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & (((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1)) | ~ totalorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f182])).
fof(f182, plain, ! [X0] : (! [X1] : ((totalorderedP(cons(X0, X1)) <=> ((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f67])).
fof(f67, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => (totalorderedP(cons(X0, X1)) <=> ((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax67)).
fof(f91412, plain, (totalorderedP(cons(sK57, sK53)) | ~ spl63_4989), inference(avatar_component_clause, [], [f91410])).
fof(f157323, plain, spl63_9813, inference(avatar_split_clause, [], [f561, f156693])).
fof(f561, plain, ssItem(sK59), inference(cnf_transformation, [], [f354])).
fof(f112908, plain, spl63_4436, inference(avatar_contradiction_clause, [], [f112907])).
fof(f112907, plain, ($false | spl63_4436), inference(subsumption_resolution, [], [f112906, f562])).
fof(f112906, plain, (~ ssList(sK60) | spl63_4436), inference(subsumption_resolution, [], [f112905, f560])).
fof(f112905, plain, (~ ssItem(sK58) | ~ ssList(sK60) | spl63_4436), inference(resolution, [], [f80777, f446])).
fof(f80777, plain, (~ ssList(cons(sK58, sK60)) | spl63_4436), inference(avatar_component_clause, [], [f80775])).
fof(f98019, plain, (spl63_144 | ~ spl63_883), inference(avatar_contradiction_clause, [], [f98018])).
fof(f98018, plain, ($false | (spl63_144 | ~ spl63_883)), inference(subsumption_resolution, [], [f98017, f562])).
fof(f98017, plain, (~ ssList(sK60) | (spl63_144 | ~ spl63_883)), inference(subsumption_resolution, [], [f98016, f18727])).
fof(f98016, plain, (~ ssList(cons(sK58, nil)) | ~ ssList(sK60) | spl63_144), inference(resolution, [], [f74566, f459])).
fof(f74566, plain, (~ ssList(app(sK60, cons(sK58, nil))) | spl63_144), inference(subsumption_resolution, [], [f74565, f447])).
fof(f74565, plain, (~ ssList(nil) | ~ ssList(app(sK60, cons(sK58, nil))) | spl63_144), inference(resolution, [], [f3222, f459])).
fof(f3222, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), nil)) | spl63_144), inference(avatar_component_clause, [], [f3220])).
fof(f91496, plain, (~ spl63_358 | spl63_4989 | ~ spl63_4 | ~ spl63_5 | spl63_347), inference(avatar_split_clause, [], [f91495, f5860, f628, f623, f91410, f5912])).
fof(f5912, plain, (spl63_358 <=> totalorderedP(sK53)), introduced(avatar_definition, [new_symbols(naming, [spl63_358])])).
fof(f91495, plain, (totalorderedP(cons(sK57, sK53)) | ~ totalorderedP(sK53) | (~ spl63_4 | ~ spl63_5 | spl63_347)), inference(subsumption_resolution, [], [f91494, f641])).
fof(f91494, plain, (totalorderedP(cons(sK57, sK53)) | ~ totalorderedP(sK53) | ~ ssList(sK53) | (~ spl63_4 | ~ spl63_5 | spl63_347)), inference(subsumption_resolution, [], [f49131, f5861])).
fof(f49131, plain, (totalorderedP(cons(sK57, sK53)) | ~ totalorderedP(sK53) | (nil = sK53) | ~ ssList(sK53) | (~ spl63_4 | ~ spl63_5)), inference(superposition, [], [f2132, f20225])).
fof(f2132, plain, ! [X0] : (totalorderedP(cons(hd(X0), X0)) | ~ totalorderedP(X0) | (nil = X0) | ~ ssList(X0)), inference(subsumption_resolution, [], [f2131, f455])).
fof(f455, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax22)).
fof(f2131, plain, ! [X0] : (totalorderedP(cons(hd(X0), X0)) | ~ totalorderedP(X0) | (nil = X0) | ~ ssList(X0) | ~ ssItem(hd(X0))), inference(duplicate_literal_removal, [], [f2130])).
fof(f2130, plain, ! [X0] : (totalorderedP(cons(hd(X0), X0)) | ~ totalorderedP(X0) | (nil = X0) | ~ ssList(X0) | ~ ssItem(hd(X0)) | ~ ssItem(hd(X0))), inference(resolution, [], [f515, f464])).
fof(f464, plain, ! [X0] : (leq(X0, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0] : (leq(X0, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (ssItem(X0) => leq(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax31)).
fof(f515, plain, ! [X0, X1] : (~ leq(X0, hd(X1)) | totalorderedP(cons(X0, X1)) | ~ totalorderedP(X1) | (nil = X1) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f333])).
fof(f89542, plain, (spl63_358 | ~ spl63_4 | ~ spl63_5), inference(avatar_split_clause, [], [f89541, f628, f623, f5912])).
fof(f89541, plain, (totalorderedP(sK53) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20647, f630])).
fof(f20647, plain, (totalorderedP(sK53) | ~ ssItem(sK57) | ~ spl63_4), inference(superposition, [], [f509, f20224])).
fof(f509, plain, ! [X0] : (totalorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0] : (totalorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (ssItem(X0) => totalorderedP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax65)).
fof(f83393, plain, (spl63_36 | spl63_223), inference(avatar_contradiction_clause, [], [f83392])).
fof(f83392, plain, ($false | (spl63_36 | spl63_223)), inference(subsumption_resolution, [], [f83391, f564])).
fof(f83391, plain, (~ ssList(sK62) | (spl63_36 | spl63_223)), inference(subsumption_resolution, [], [f83390, f1205])).
fof(f83390, plain, ((nil = sK62) | ~ ssList(sK62) | spl63_223), inference(resolution, [], [f3918, f457])).
fof(f3918, plain, (~ ssList(tl(sK62)) | spl63_223), inference(avatar_component_clause, [], [f3916])).
fof(f70291, plain, (~ spl63_3108 | ~ spl63_3370), inference(avatar_contradiction_clause, [], [f70290])).
fof(f70290, plain, ($false | (~ spl63_3108 | ~ spl63_3370)), inference(subsumption_resolution, [], [f70071, f70070])).
fof(f70070, plain, (gt(sK57, sK57) | (~ spl63_3108 | ~ spl63_3370)), inference(backward_demodulation, [], [f67241, f56389])).
fof(f56389, plain, ((sK57 = sK58) | ~ spl63_3108), inference(avatar_component_clause, [], [f56387])).
fof(f67241, plain, (gt(sK58, sK57) | ~ spl63_3370), inference(backward_demodulation, [], [f1149, f59359])).
fof(f1149, plain, gt(sK58, sK59), inference(subsumption_resolution, [], [f1148, f560])).
fof(f1148, plain, (gt(sK58, sK59) | ~ ssItem(sK58)), inference(subsumption_resolution, [], [f1147, f561])).
fof(f1147, plain, (gt(sK58, sK59) | ~ ssItem(sK59) | ~ ssItem(sK58)), inference(resolution, [], [f470, f566])).
fof(f566, plain, lt(sK59, sK58), inference(cnf_transformation, [], [f354])).
fof(f470, plain, ! [X0, X1] : (~ lt(X1, X0) | gt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f322])).
fof(f322, plain, ! [X0] : (! [X1] : (((gt(X0, X1) | ~ lt(X1, X0)) & (lt(X1, X0) | ~ gt(X0, X1))) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f146])).
fof(f146, plain, ! [X0] : (! [X1] : ((gt(X0, X1) <=> lt(X1, X0)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (gt(X0, X1) <=> lt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax35)).
fof(f70071, plain, (~ gt(sK57, sK57) | (~ spl63_3108 | ~ spl63_3370)), inference(backward_demodulation, [], [f67243, f56389])).
fof(f67243, plain, (~ gt(sK57, sK58) | ~ spl63_3370), inference(backward_demodulation, [], [f1388, f59359])).
fof(f1388, plain, ~ gt(sK59, sK58), inference(subsumption_resolution, [], [f1387, f561])).
fof(f1387, plain, (~ gt(sK59, sK58) | ~ ssItem(sK59)), inference(subsumption_resolution, [], [f1386, f560])).
fof(f1386, plain, (~ gt(sK59, sK58) | ~ ssItem(sK58) | ~ ssItem(sK59)), inference(resolution, [], [f552, f1149])).
fof(f552, plain, ! [X0, X1] : (~ gt(X1, X0) | ~ gt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f219])).
fof(f219, plain, ! [X0] : (! [X1] : (~ gt(X1, X0) | ~ gt(X0, X1) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f218])).
fof(f218, plain, ! [X0] : (! [X1] : ((~ gt(X1, X0) | ~ gt(X0, X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f94])).
fof(f94, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (gt(X0, X1) => ~ gt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax94)).
fof(f65043, plain, (spl63_36 | spl63_224), inference(avatar_contradiction_clause, [], [f65042])).
fof(f65042, plain, ($false | (spl63_36 | spl63_224)), inference(subsumption_resolution, [], [f65041, f564])).
fof(f65041, plain, (~ ssList(sK62) | (spl63_36 | spl63_224)), inference(subsumption_resolution, [], [f65040, f1205])).
fof(f65040, plain, ((nil = sK62) | ~ ssList(sK62) | spl63_224), inference(resolution, [], [f3922, f455])).
fof(f3922, plain, (~ ssItem(hd(sK62)) | spl63_224), inference(avatar_component_clause, [], [f3920])).
fof(f61226, plain, (~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_3424), inference(avatar_contradiction_clause, [], [f61225])).
fof(f61225, plain, ($false | (~ spl63_4 | ~ spl63_5 | spl63_347 | ~ spl63_3424)), inference(subsumption_resolution, [], [f61224, f5861])).
fof(f61224, plain, ((nil = sK53) | (~ spl63_4 | ~ spl63_5 | ~ spl63_3424)), inference(forward_demodulation, [], [f61022, f20226])).
fof(f61022, plain, ((sK53 = tl(sK53)) | (~ spl63_5 | ~ spl63_3424)), inference(backward_demodulation, [], [f22674, f60760])).
fof(f60760, plain, ((sK53 = cons(sK57, sK53)) | ~ spl63_3424), inference(avatar_component_clause, [], [f60758])).
fof(f22674, plain, ((sK53 = tl(cons(sK57, sK53))) | ~ spl63_5), inference(resolution, [], [f5718, f630])).
fof(f5718, plain, ! [X1] : (~ ssItem(X1) | (sK53 = tl(cons(X1, sK53)))), inference(resolution, [], [f641, f458])).
fof(f55739, plain, (spl63_347 | ~ spl63_2914 | ~ spl63_2967), inference(avatar_contradiction_clause, [], [f55738])).
fof(f55738, plain, ($false | (spl63_347 | ~ spl63_2914 | ~ spl63_2967)), inference(subsumption_resolution, [], [f55737, f5861])).
fof(f55737, plain, ((nil = sK53) | (~ spl63_2914 | ~ spl63_2967)), inference(forward_demodulation, [], [f54548, f54824])).
fof(f54824, plain, ((nil = sK52(cons(sK58, sK53))) | ~ spl63_2967), inference(avatar_component_clause, [], [f54822])).
fof(f54548, plain, ((sK53 = sK52(cons(sK58, sK53))) | ~ spl63_2914), inference(avatar_component_clause, [], [f54546])).
fof(f54546, plain, (spl63_2914 <=> (sK53 = sK52(cons(sK58, sK53)))), introduced(avatar_definition, [new_symbols(naming, [spl63_2914])])).
fof(f54549, plain, (spl63_2901 | spl63_2914 | ~ spl63_2900), inference(avatar_split_clause, [], [f54544, f54153, f54546, f54157])).
fof(f54153, plain, (spl63_2900 <=> ssList(cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_2900])])).
fof(f54544, plain, ((sK53 = sK52(cons(sK58, sK53))) | (nil = cons(sK58, sK53)) | ~ spl63_2900), inference(forward_demodulation, [], [f54240, f22675])).
fof(f22675, plain, (sK53 = tl(cons(sK58, sK53))), inference(resolution, [], [f5718, f560])).
fof(f54240, plain, ((nil = cons(sK58, sK53)) | (tl(cons(sK58, sK53)) = sK52(cons(sK58, sK53))) | ~ spl63_2900), inference(resolution, [], [f54154, f530])).
fof(f54154, plain, (ssList(cons(sK58, sK53)) | ~ spl63_2900), inference(avatar_component_clause, [], [f54153])).
fof(f54205, plain, spl63_2900, inference(avatar_contradiction_clause, [], [f54204])).
fof(f54204, plain, ($false | spl63_2900), inference(subsumption_resolution, [], [f54203, f641])).
fof(f54203, plain, (~ ssList(sK53) | spl63_2900), inference(subsumption_resolution, [], [f54202, f560])).
fof(f54202, plain, (~ ssItem(sK58) | ~ ssList(sK53) | spl63_2900), inference(resolution, [], [f54155, f446])).
fof(f54155, plain, (~ ssList(cons(sK58, sK53)) | spl63_2900), inference(avatar_component_clause, [], [f54153])).
fof(f42331, plain, (~ spl63_4 | ~ spl63_5 | spl63_2190), inference(avatar_contradiction_clause, [], [f42330])).
fof(f42330, plain, ($false | (~ spl63_4 | ~ spl63_5 | spl63_2190)), inference(subsumption_resolution, [], [f42329, f641])).
fof(f42329, plain, (~ ssList(sK53) | (~ spl63_4 | ~ spl63_5 | spl63_2190)), inference(subsumption_resolution, [], [f42328, f20691])).
fof(f42328, plain, (~ singletonP(sK53) | ~ ssList(sK53) | spl63_2190), inference(resolution, [], [f42184, f364])).
fof(f42184, plain, (~ ssItem(sK10(sK53)) | spl63_2190), inference(avatar_component_clause, [], [f42182])).
fof(f42182, plain, (spl63_2190 <=> ssItem(sK10(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl63_2190])])).
fof(f42305, plain, (~ spl63_2190 | spl63_2212 | ~ spl63_4 | ~ spl63_5), inference(avatar_split_clause, [], [f42304, f628, f623, f42296, f42182])).
fof(f42304, plain, ((sK57 = sK10(sK53)) | ~ ssItem(sK10(sK53)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f42303, f447])).
fof(f42303, plain, ((sK57 = sK10(sK53)) | ~ ssList(nil) | ~ ssItem(sK10(sK53)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f42177, f22622])).
fof(f22622, plain, (frontsegP(sK53, sK53) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f22618, f447])).
fof(f22618, plain, (frontsegP(sK53, sK53) | ~ ssList(nil) | (~ spl63_4 | ~ spl63_5)), inference(superposition, [], [f20731, f20224])).
fof(f20731, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ ssList(X51)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20730, f486])).
fof(f20730, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ frontsegP(X51, nil) | ~ ssList(X51)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20729, f630])).
fof(f20729, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ frontsegP(X51, nil) | ~ ssList(X51) | ~ ssItem(sK57)) | ~ spl63_4), inference(subsumption_resolution, [], [f20682, f447])).
fof(f20682, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ frontsegP(X51, nil) | ~ ssList(nil) | ~ ssList(X51) | ~ ssItem(sK57)) | ~ spl63_4), inference(superposition, [], [f604, f20224])).
fof(f42177, plain, (~ frontsegP(sK53, sK53) | (sK57 = sK10(sK53)) | ~ ssList(nil) | ~ ssItem(sK10(sK53)) | (~ spl63_4 | ~ spl63_5)), inference(superposition, [], [f20702, f20821])).
fof(f20821, plain, ((sK53 = cons(sK10(sK53), nil)) | (~ spl63_4 | ~ spl63_5)), inference(subsumption_resolution, [], [f20820, f641])).
fof(f20820, plain, ((sK53 = cons(sK10(sK53), nil)) | ~ ssList(sK53) | (~ spl63_4 | ~ spl63_5)), inference(resolution, [], [f20691, f365])).
fof(f365, plain, ! [X0] : (~ singletonP(X0) | (cons(sK10(X0), nil) = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f245])).
fof(f32860, plain, (spl63_561 | spl63_2112 | ~ spl63_560), inference(avatar_split_clause, [], [f32855, f13271, f32857, f13275])).
fof(f13271, plain, (spl63_560 <=> ssList(cons(sK58, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl63_560])])).
fof(f32855, plain, ((sK58 = hd(app(cons(sK58, sK61), sK53))) | (nil = cons(sK58, sK61)) | ~ spl63_560), inference(forward_demodulation, [], [f31706, f5183])).
fof(f5183, plain, (sK58 = hd(cons(sK58, sK61))), inference(resolution, [], [f1093, f560])).
fof(f1093, plain, ! [X54] : (~ ssItem(X54) | (hd(cons(X54, sK61)) = X54)), inference(resolution, [], [f456, f563])).
fof(f31706, plain, ((hd(cons(sK58, sK61)) = hd(app(cons(sK58, sK61), sK53))) | (nil = cons(sK58, sK61)) | ~ spl63_560), inference(resolution, [], [f13272, f5727])).
fof(f5727, plain, ! [X7] : (~ ssList(X7) | (hd(X7) = hd(app(X7, sK53))) | (nil = X7)), inference(resolution, [], [f641, f541])).
fof(f13272, plain, (ssList(cons(sK58, sK61)) | ~ spl63_560), inference(avatar_component_clause, [], [f13271])).
fof(f23028, plain, (spl63_997 | ~ spl63_32 | ~ spl63_36 | ~ spl63_95 | ~ spl63_883), inference(avatar_split_clause, [], [f23027, f18726, f2743, f1204, f1186, f20807])).
fof(f2743, plain, (spl63_95 <=> ssList(app(app(app(nil, cons(sK58, nil)), sK61), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_95])])).
fof(f23027, plain, ((sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | (~ spl63_32 | ~ spl63_36 | ~ spl63_95 | ~ spl63_883)), inference(forward_demodulation, [], [f22881, f19666])).
fof(f19666, plain, ((sK53 = app(app(cons(sK58, sK61), cons(sK59, nil)), nil)) | (~ spl63_32 | ~ spl63_36 | ~ spl63_883)), inference(forward_demodulation, [], [f19665, f10438])).
fof(f19665, plain, ((sK53 = app(app(app(cons(sK58, nil), sK61), cons(sK59, nil)), nil)) | (~ spl63_32 | ~ spl63_36 | ~ spl63_883)), inference(forward_demodulation, [], [f19664, f18827])).
fof(f19664, plain, ((sK53 = app(app(app(app(nil, cons(sK58, nil)), sK61), cons(sK59, nil)), nil)) | (~ spl63_32 | ~ spl63_36)), inference(forward_demodulation, [], [f19663, f1188])).
fof(f1188, plain, ((nil = sK60) | ~ spl63_32), inference(avatar_component_clause, [], [f1186])).
fof(f19663, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), nil)) | ~ spl63_36), inference(forward_demodulation, [], [f565, f1206])).
fof(f22881, plain, ((app(cons(sK58, sK61), cons(sK59, nil)) = app(app(cons(sK58, sK61), cons(sK59, nil)), nil)) | (~ spl63_95 | ~ spl63_883)), inference(resolution, [], [f20296, f540])).
fof(f20296, plain, (ssList(app(cons(sK58, sK61), cons(sK59, nil))) | (~ spl63_95 | ~ spl63_883)), inference(forward_demodulation, [], [f20295, f10438])).
fof(f20295, plain, (ssList(app(app(cons(sK58, nil), sK61), cons(sK59, nil))) | (~ spl63_95 | ~ spl63_883)), inference(forward_demodulation, [], [f2744, f18827])).
fof(f2744, plain, (ssList(app(app(app(nil, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl63_95), inference(avatar_component_clause, [], [f2743])).
fof(f19572, plain, (~ spl63_65 | spl63_95 | ~ spl63_560 | ~ spl63_883), inference(avatar_contradiction_clause, [], [f19571])).
fof(f19571, plain, ($false | (~ spl63_65 | spl63_95 | ~ spl63_560 | ~ spl63_883)), inference(subsumption_resolution, [], [f19570, f13272])).
fof(f19570, plain, (~ ssList(cons(sK58, sK61)) | (~ spl63_65 | spl63_95 | ~ spl63_883)), inference(forward_demodulation, [], [f19569, f10438])).
fof(f19569, plain, (~ ssList(app(cons(sK58, nil), sK61)) | (~ spl63_65 | spl63_95 | ~ spl63_883)), inference(forward_demodulation, [], [f19568, f18827])).
fof(f19568, plain, (~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | (~ spl63_65 | spl63_95)), inference(subsumption_resolution, [], [f11664, f2238])).
fof(f11664, plain, (~ ssList(cons(sK59, nil)) | ~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | spl63_95), inference(resolution, [], [f2745, f459])).
fof(f2745, plain, (~ ssList(app(app(app(nil, cons(sK58, nil)), sK61), cons(sK59, nil))) | spl63_95), inference(avatar_component_clause, [], [f2743])).
fof(f19115, plain, (spl63_93 | ~ spl63_883), inference(avatar_contradiction_clause, [], [f19114])).
fof(f19114, plain, ($false | (spl63_93 | ~ spl63_883)), inference(subsumption_resolution, [], [f18996, f18727])).
fof(f18996, plain, (~ ssList(cons(sK58, nil)) | (spl63_93 | ~ spl63_883)), inference(backward_demodulation, [], [f10340, f18827])).
fof(f10340, plain, (~ ssList(app(nil, cons(sK58, nil))) | spl63_93), inference(subsumption_resolution, [], [f10339, f563])).
fof(f10339, plain, (~ ssList(sK61) | ~ ssList(app(nil, cons(sK58, nil))) | spl63_93), inference(resolution, [], [f2731, f459])).
fof(f2731, plain, (~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | spl63_93), inference(avatar_component_clause, [], [f2729])).
fof(f2729, plain, (spl63_93 <=> ssList(app(app(nil, cons(sK58, nil)), sK61))), introduced(avatar_definition, [new_symbols(naming, [spl63_93])])).
fof(f18785, plain, spl63_883, inference(avatar_contradiction_clause, [], [f18784])).
fof(f18784, plain, ($false | spl63_883), inference(subsumption_resolution, [], [f18783, f447])).
fof(f18783, plain, (~ ssList(nil) | spl63_883), inference(subsumption_resolution, [], [f18782, f560])).
fof(f18782, plain, (~ ssItem(sK58) | ~ ssList(nil) | spl63_883), inference(resolution, [], [f18728, f446])).
fof(f18728, plain, (~ ssList(cons(sK58, nil)) | spl63_883), inference(avatar_component_clause, [], [f18726])).
fof(f18340, plain, (~ spl63_5 | spl63_870), inference(avatar_contradiction_clause, [], [f18339])).
fof(f18339, plain, ($false | (~ spl63_5 | spl63_870)), inference(subsumption_resolution, [], [f18338, f630])).
fof(f18338, plain, (~ ssItem(sK57) | spl63_870), inference(resolution, [], [f18060, f503])).
fof(f503, plain, ! [X0] : (cyclefreeP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ! [X0] : (cyclefreeP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f59])).
fof(f59, plain, ! [X0] : (ssItem(X0) => cyclefreeP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax59)).
fof(f18060, plain, (~ cyclefreeP(cons(sK57, nil)) | spl63_870), inference(avatar_component_clause, [], [f18059])).
fof(f18059, plain, (spl63_870 <=> cyclefreeP(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_870])])).
fof(f18067, plain, (spl63_871 | ~ spl63_870 | ~ spl63_797), inference(avatar_split_clause, [], [f18057, f17220, f18059, f18063])).
fof(f18057, plain, (~ cyclefreeP(cons(sK57, nil)) | sP0(cons(sK57, nil)) | ~ spl63_797), inference(resolution, [], [f17443, f377])).
fof(f377, plain, ! [X0] : (~ sP1(X0) | ~ cyclefreeP(X0) | sP0(X0)), inference(cnf_transformation, [], [f259])).
fof(f259, plain, ! [X0] : (((cyclefreeP(X0) | ~ sP0(X0)) & (sP0(X0) | ~ cyclefreeP(X0))) | ~ sP1(X0)), inference(nnf_transformation, [], [f225])).
fof(f225, plain, ! [X0] : ((cyclefreeP(X0) <=> sP0(X0)) | ~ sP1(X0)), inference(usedef, [], [e225])).
fof(e225, plain, ! [X0] : (sP1(X0) <=> (cyclefreeP(X0) <=> sP0(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f17443, plain, (sP1(cons(sK57, nil)) | ~ spl63_797), inference(resolution, [], [f17221, f388])).
fof(f388, plain, ! [X0] : (~ ssList(X0) | sP1(X0)), inference(cnf_transformation, [], [f226])).
fof(f226, plain, ! [X0] : (sP1(X0) | ~ ssList(X0)), inference(definition_folding, [], [f106, e225, e224])).
fof(f106, plain, ! [X0] : ((cyclefreeP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(flattening, [], [f105])).
fof(f105, plain, ! [X0] : ((cyclefreeP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (((~ leq(X2, X1) | ~ leq(X1, X2)) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0)) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : (ssList(X0) => (cyclefreeP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => ~ (leq(X2, X1) & leq(X1, X2)))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax8)).
fof(f17345, plain, (~ spl63_5 | spl63_797), inference(avatar_contradiction_clause, [], [f17344])).
fof(f17344, plain, ($false | (~ spl63_5 | spl63_797)), inference(subsumption_resolution, [], [f17343, f447])).
fof(f17343, plain, (~ ssList(nil) | (~ spl63_5 | spl63_797)), inference(subsumption_resolution, [], [f17342, f630])).
fof(f17342, plain, (~ ssItem(sK57) | ~ ssList(nil) | spl63_797), inference(resolution, [], [f17222, f446])).
fof(f17222, plain, (~ ssList(cons(sK57, nil)) | spl63_797), inference(avatar_component_clause, [], [f17220])).
fof(f13341, plain, spl63_560, inference(avatar_contradiction_clause, [], [f13340])).
fof(f13340, plain, ($false | spl63_560), inference(subsumption_resolution, [], [f13339, f563])).
fof(f13339, plain, (~ ssList(sK61) | spl63_560), inference(subsumption_resolution, [], [f13338, f560])).
fof(f13338, plain, (~ ssItem(sK58) | ~ ssList(sK61) | spl63_560), inference(resolution, [], [f13273, f446])).
fof(f13273, plain, (~ ssList(cons(sK58, sK61)) | spl63_560), inference(avatar_component_clause, [], [f13271])).
fof(f7217, plain, (spl63_32 | spl63_158), inference(avatar_contradiction_clause, [], [f7216])).
fof(f7216, plain, ($false | (spl63_32 | spl63_158)), inference(subsumption_resolution, [], [f7215, f562])).
fof(f7215, plain, (~ ssList(sK60) | (spl63_32 | spl63_158)), inference(subsumption_resolution, [], [f7214, f1187])).
fof(f7214, plain, ((nil = sK60) | ~ ssList(sK60) | spl63_158), inference(resolution, [], [f3558, f455])).
fof(f3558, plain, (~ ssItem(hd(sK60)) | spl63_158), inference(avatar_component_clause, [], [f3556])).
fof(f5944, plain, (spl63_347 | ~ spl63_2), inference(avatar_split_clause, [], [f5943, f613, f5860])).
fof(f5943, plain, ((nil = sK53) | ~ spl63_2), inference(backward_demodulation, [], [f559, f615])).
fof(f615, plain, ((nil = sK55) | ~ spl63_2), inference(avatar_component_clause, [], [f613])).
fof(f5873, plain, (spl63_349 | spl63_347), inference(avatar_split_clause, [], [f5721, f5860, f5870])).
fof(f5721, plain, ((nil = sK53) | (hd(sK53) = sK51(sK53))), inference(resolution, [], [f641, f528])).
fof(f528, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (hd(X0) = sK51(X0))), inference(cnf_transformation, [], [f337])).
fof(f5868, plain, (spl63_348 | spl63_347), inference(avatar_split_clause, [], [f5722, f5860, f5865])).
fof(f5722, plain, ((nil = sK53) | (tl(sK53) = sK52(sK53))), inference(resolution, [], [f641, f530])).
fof(f4260, plain, (~ spl63_93 | ~ spl63_2 | ~ spl63_32 | ~ spl63_65 | spl63_94), inference(avatar_split_clause, [], [f4259, f2733, f2237, f1186, f613, f2729])).
fof(f2733, plain, (spl63_94 <=> segmentP(nil, cons(sK59, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_94])])).
fof(f4259, plain, (~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | (~ spl63_2 | ~ spl63_32 | ~ spl63_65 | spl63_94)), inference(subsumption_resolution, [], [f4258, f447])).
fof(f4258, plain, (~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | ~ ssList(nil) | (~ spl63_2 | ~ spl63_32 | ~ spl63_65 | spl63_94)), inference(subsumption_resolution, [], [f4253, f2238])).
fof(f4253, plain, (~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | ~ ssList(nil) | (~ spl63_2 | ~ spl63_32 | spl63_94)), inference(subsumption_resolution, [], [f4252, f564])).
fof(f4252, plain, (~ ssList(sK62) | ~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | ~ ssList(nil) | (~ spl63_2 | ~ spl63_32 | spl63_94)), inference(subsumption_resolution, [], [f4232, f2734])).
fof(f2734, plain, (~ segmentP(nil, cons(sK59, nil)) | spl63_94), inference(avatar_component_clause, [], [f2733])).
fof(f4232, plain, (segmentP(nil, cons(sK59, nil)) | ~ ssList(sK62) | ~ ssList(app(app(nil, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | ~ ssList(nil) | (~ spl63_2 | ~ spl63_32)), inference(superposition, [], [f580, f4210])).
fof(f4210, plain, ((nil = app(app(app(app(nil, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)) | (~ spl63_2 | ~ spl63_32)), inference(forward_demodulation, [], [f646, f1188])).
fof(f646, plain, ((nil = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)) | ~ spl63_2), inference(backward_demodulation, [], [f565, f644])).
fof(f644, plain, ((nil = sK53) | ~ spl63_2), inference(backward_demodulation, [], [f559, f615])).
fof(f580, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f376])).
fof(f376, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK13(X0, X1), X1), sK14(X0, X1)) = X0) & ssList(sK14(X0, X1))) & ssList(sK13(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14])], [f255, f257, f256])).
fof(f256, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK13(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK13(X0, X1)))), introduced(choice_axiom, [])).
fof(f257, plain, ! [X1, X0] : (? [X5] : ((app(app(sK13(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK13(X0, X1), X1), sK14(X0, X1)) = X0) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f255, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f254])).
fof(f254, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax7)).
fof(f4201, plain, (~ spl63_65 | spl63_73 | ~ spl63_94), inference(avatar_contradiction_clause, [], [f4200])).
fof(f4200, plain, ($false | (~ spl63_65 | spl63_73 | ~ spl63_94)), inference(subsumption_resolution, [], [f4199, f2238])).
fof(f4199, plain, (~ ssList(cons(sK59, nil)) | (spl63_73 | ~ spl63_94)), inference(subsumption_resolution, [], [f4195, f2299])).
fof(f2299, plain, (~ (nil = cons(sK59, nil)) | spl63_73), inference(avatar_component_clause, [], [f2298])).
fof(f2298, plain, (spl63_73 <=> (nil = cons(sK59, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_73])])).
fof(f4195, plain, ((nil = cons(sK59, nil)) | ~ ssList(cons(sK59, nil)) | ~ spl63_94), inference(resolution, [], [f2735, f501])).
fof(f501, plain, ! [X0] : (~ segmentP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f331])).
fof(f331, plain, ! [X0] : (((segmentP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ segmentP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((segmentP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (ssList(X0) => (segmentP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC298+1.p', ax58)).
fof(f2735, plain, (segmentP(nil, cons(sK59, nil)) | ~ spl63_94), inference(avatar_component_clause, [], [f2733])).
fof(f4088, plain, (spl63_32 | spl63_157), inference(avatar_split_clause, [], [f4087, f3552, f1186])).
fof(f4087, plain, ((nil = sK60) | spl63_157), inference(subsumption_resolution, [], [f4083, f562])).
fof(f4083, plain, ((nil = sK60) | ~ ssList(sK60) | spl63_157), inference(resolution, [], [f3554, f457])).
fof(f3554, plain, (~ ssList(tl(sK60)) | spl63_157), inference(avatar_component_clause, [], [f3552])).
fof(f4050, plain, (~ spl63_224 | ~ spl63_223 | spl63_253 | spl63_36), inference(avatar_split_clause, [], [f3909, f1204, f4047, f3916, f3920])).
fof(f3909, plain, (memberP(sK62, hd(sK62)) | ~ ssList(tl(sK62)) | ~ ssItem(hd(sK62)) | spl63_36), inference(superposition, [], [f605, f3126])).
fof(f3126, plain, ((sK62 = cons(hd(sK62), tl(sK62))) | spl63_36), inference(subsumption_resolution, [], [f3092, f1205])).
fof(f3092, plain, ((nil = sK62) | (sK62 = cons(hd(sK62), tl(sK62)))), inference(resolution, [], [f564, f532])).
fof(f605, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1)), inference(duplicate_literal_removal, [], [f590])).
fof(f590, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X1)), inference(equality_resolution, [], [f475])).
fof(f475, plain, ! [X2, X0, X1] : (memberP(cons(X1, X2), X0) | ~ (X0 = X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f326])).
fof(f2650, plain, spl63_66, inference(avatar_contradiction_clause, [], [f2649])).
fof(f2649, plain, ($false | spl63_66), inference(subsumption_resolution, [], [f2648, f447])).
fof(f2648, plain, (~ ssList(nil) | spl63_66), inference(subsumption_resolution, [], [f2647, f560])).
fof(f2647, plain, (~ ssItem(sK58) | ~ ssList(nil) | spl63_66), inference(resolution, [], [f2644, f446])).
fof(f2644, plain, (~ ssList(cons(sK58, nil)) | spl63_66), inference(subsumption_resolution, [], [f2643, f447])).
fof(f2643, plain, (~ ssList(cons(sK58, nil)) | ~ ssList(nil) | spl63_66), inference(resolution, [], [f2638, f459])).
fof(f2638, plain, (~ ssList(app(nil, cons(sK58, nil))) | spl63_66), inference(subsumption_resolution, [], [f2637, f447])).
fof(f2637, plain, (~ ssList(nil) | ~ ssList(app(nil, cons(sK58, nil))) | spl63_66), inference(resolution, [], [f2243, f459])).
fof(f2243, plain, (~ ssList(app(app(nil, cons(sK58, nil)), nil)) | spl63_66), inference(avatar_component_clause, [], [f2241])).
fof(f2398, plain, ~ spl63_73, inference(avatar_contradiction_clause, [], [f2397])).
fof(f2397, plain, ($false | ~ spl63_73), inference(subsumption_resolution, [], [f2396, f447])).
fof(f2396, plain, (~ ssList(nil) | ~ spl63_73), inference(subsumption_resolution, [], [f2384, f561])).
fof(f2384, plain, (~ ssItem(sK59) | ~ ssList(nil) | ~ spl63_73), inference(trivial_inequality_removal, [], [f2369])).
fof(f2369, plain, (~ (nil = nil) | ~ ssItem(sK59) | ~ ssList(nil) | ~ spl63_73), inference(superposition, [], [f454, f2300])).
fof(f2300, plain, ((nil = cons(sK59, nil)) | ~ spl63_73), inference(avatar_component_clause, [], [f2298])).
fof(f2251, plain, spl63_65, inference(avatar_contradiction_clause, [], [f2250])).
fof(f2250, plain, ($false | spl63_65), inference(subsumption_resolution, [], [f2249, f447])).
fof(f2249, plain, (~ ssList(nil) | spl63_65), inference(subsumption_resolution, [], [f2248, f561])).
fof(f2248, plain, (~ ssItem(sK59) | ~ ssList(nil) | spl63_65), inference(resolution, [], [f2239, f446])).
fof(f2239, plain, (~ ssList(cons(sK59, nil)) | spl63_65), inference(avatar_component_clause, [], [f2237])).
fof(f1720, plain, (spl63_32 | spl63_44), inference(avatar_contradiction_clause, [], [f1719])).
fof(f1719, plain, ($false | (spl63_32 | spl63_44)), inference(subsumption_resolution, [], [f1718, f562])).
fof(f1718, plain, (~ ssList(sK60) | (spl63_32 | spl63_44)), inference(subsumption_resolution, [], [f1717, f1187])).
fof(f1717, plain, ((nil = sK60) | ~ ssList(sK60) | spl63_44), inference(resolution, [], [f1664, f451])).
fof(f451, plain, ! [X0] : (ssList(sK49(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f320])).
fof(f1664, plain, (~ ssList(sK49(sK60)) | spl63_44), inference(avatar_component_clause, [], [f1662])).
fof(f1708, plain, (spl63_32 | spl63_43), inference(avatar_contradiction_clause, [], [f1707])).
fof(f1707, plain, ($false | (spl63_32 | spl63_43)), inference(subsumption_resolution, [], [f1706, f562])).
fof(f1706, plain, (~ ssList(sK60) | (spl63_32 | spl63_43)), inference(subsumption_resolution, [], [f1705, f1187])).
fof(f1705, plain, ((nil = sK60) | ~ ssList(sK60) | spl63_43), inference(resolution, [], [f1660, f452])).
fof(f1660, plain, (~ ssItem(sK50(sK60)) | spl63_43), inference(avatar_component_clause, [], [f1658])).
fof(f631, plain, (spl63_5 | spl63_2), inference(avatar_split_clause, [], [f571, f613, f628])).
fof(f571, plain, ((nil = sK55) | ssItem(sK57)), inference(cnf_transformation, [], [f354])).
fof(f626, plain, (spl63_4 | spl63_2), inference(avatar_split_clause, [], [f572, f613, f623])).
fof(f572, plain, ((nil = sK55) | (sK55 = cons(sK57, nil))), inference(cnf_transformation, [], [f354])).