fof(f603082, plain, $false, inference(avatar_sat_refutation, [], [f624, f629, f1983, f2277, f3729, f4153, f4165, f4317, f4439, f4879, f5336, f6631, f8766, f8771, f8822, f8852, f12786, f14043, f14329, f15568, f34402, f58009, f58044, f69760, f74042, f88056, f103824, f109112, f109120, f109623, f109626, f113255, f115198, f117230, f120491, f120605, f129458, f129664, f135555, f136010, f140898, f145993, f150987, f153620, f153624, f153812, f154639, f154642, f154878, f170658, f172906, f175394, f175395, f178254, f179017, f179018, f180865, f200269, f200469, f200477, f200480, f201083, f201862, f201865, f202934, f211193, f215465, f226130, f226191, f231576, f231607, f231610, f231618, f231628, f258384, f273855, f285651, f302206, f302229, f333205, f342397, f386897, f398934, f400177, f416028, f442294, f443485, f456689, f490931, f493635, f530798, f535128, f542848, f567728, f569079, f573507, f599416, f599838, f600251])).
fof(f600251, plain, (~ spl65_62 | ~ spl65_1947 | ~ spl65_6872 | ~ spl65_7832 | ~ spl65_7956 | spl65_16416 | ~ spl65_21242), inference(avatar_contradiction_clause, [], [f600250])).
fof(f600250, plain, ($false | (~ spl65_62 | ~ spl65_1947 | ~ spl65_6872 | ~ spl65_7832 | ~ spl65_7956 | spl65_16416 | ~ spl65_21242)), inference(subsumption_resolution, [], [f597084, f588729])).
fof(f588729, plain, (~ memberP(nil, sK61) | (~ spl65_1947 | ~ spl65_6872 | ~ spl65_7832 | ~ spl65_7956 | spl65_16416)), inference(backward_demodulation, [], [f586706, f588570])).
fof(f588570, plain, ((nil = sK13(sK57, sK58)) | (~ spl65_1947 | ~ spl65_6872)), inference(forward_demodulation, [], [f39897, f102275])).
fof(f102275, plain, ((sK58 = sK64) | ~ spl65_6872), inference(avatar_component_clause, [], [f102273])).
fof(f102273, plain, (spl65_6872 <=> (sK58 = sK64)), introduced(avatar_definition, [new_symbols(naming, [spl65_6872])])).
fof(f39897, plain, ((nil = sK13(sK57, sK64)) | ~ spl65_1947), inference(avatar_component_clause, [], [f39895])).
fof(f39895, plain, (spl65_1947 <=> (nil = sK13(sK57, sK64))), introduced(avatar_definition, [new_symbols(naming, [spl65_1947])])).
fof(f586706, plain, (~ memberP(sK13(sK57, sK58), sK61) | (~ spl65_6872 | ~ spl65_7832 | ~ spl65_7956 | spl65_16416)), inference(backward_demodulation, [], [f580806, f102275])).
fof(f580806, plain, (~ memberP(sK13(sK57, sK64), sK61) | (~ spl65_7832 | ~ spl65_7956 | spl65_16416)), inference(backward_demodulation, [], [f579066, f127704])).
fof(f127704, plain, ((sK61 = hd(sK57)) | ~ spl65_7832), inference(avatar_component_clause, [], [f127702])).
fof(f127702, plain, (spl65_7832 <=> (sK61 = hd(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_7832])])).
fof(f579066, plain, (~ memberP(sK13(sK57, sK64), hd(sK57)) | (~ spl65_7956 | spl65_16416)), inference(backward_demodulation, [], [f355261, f129583])).
fof(f129583, plain, ((hd(sK57) = hd(sK13(sK57, sK64))) | ~ spl65_7956), inference(avatar_component_clause, [], [f129581])).
fof(f129581, plain, (spl65_7956 <=> (hd(sK57) = hd(sK13(sK57, sK64)))), introduced(avatar_definition, [new_symbols(naming, [spl65_7956])])).
fof(f355261, plain, (~ memberP(sK13(sK57, sK64), hd(sK13(sK57, sK64))) | spl65_16416), inference(avatar_component_clause, [], [f355260])).
fof(f355260, plain, (spl65_16416 <=> memberP(sK13(sK57, sK64), hd(sK13(sK57, sK64)))), introduced(avatar_definition, [new_symbols(naming, [spl65_16416])])).
fof(f597084, plain, (memberP(nil, sK61) | (~ spl65_62 | ~ spl65_21242)), inference(backward_demodulation, [], [f493334, f2261])).
fof(f2261, plain, ((nil = sK57) | ~ spl65_62), inference(avatar_component_clause, [], [f2259])).
fof(f2259, plain, (spl65_62 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl65_62])])).
fof(f493334, plain, (memberP(sK57, sK61) | ~ spl65_21242), inference(avatar_component_clause, [], [f493332])).
fof(f493332, plain, (spl65_21242 <=> memberP(sK57, sK61)), introduced(avatar_definition, [new_symbols(naming, [spl65_21242])])).
fof(f599838, plain, (~ spl65_28 | ~ spl65_62 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775 | spl65_12086), inference(avatar_contradiction_clause, [], [f599837])).
fof(f599837, plain, ($false | (~ spl65_28 | ~ spl65_62 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775 | spl65_12086)), inference(subsumption_resolution, [], [f595166, f269830])).
fof(f269830, plain, (~ frontsegP(nil, sK63) | spl65_12086), inference(avatar_component_clause, [], [f269828])).
fof(f269828, plain, (spl65_12086 <=> frontsegP(nil, sK63)), introduced(avatar_definition, [new_symbols(naming, [spl65_12086])])).
fof(f595166, plain, (frontsegP(nil, sK63) | (~ spl65_28 | ~ spl65_62 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(backward_demodulation, [], [f277776, f2261])).
fof(f277776, plain, (frontsegP(sK57, sK63) | (~ spl65_28 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f277775, f1934])).
fof(f1934, plain, (ssList(cons(sK61, nil)) | ~ spl65_28), inference(avatar_component_clause, [], [f1933])).
fof(f1933, plain, (spl65_28 <=> ssList(cons(sK61, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_28])])).
fof(f277775, plain, (frontsegP(sK57, sK63) | ~ ssList(cons(sK61, nil)) | (~ spl65_28 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f277774, f200512])).
fof(f200512, plain, (ssList(sK57) | ~ spl65_8397), inference(avatar_component_clause, [], [f200511])).
fof(f200511, plain, (spl65_8397 <=> ssList(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl65_8397])])).
fof(f277774, plain, (frontsegP(sK57, sK63) | ~ ssList(sK57) | ~ ssList(cons(sK61, nil)) | (~ spl65_28 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f277752, f576])).
fof(f576, plain, ssList(sK63), inference(cnf_transformation, [], [f361])).
fof(f361, plain, (((((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & ((~ neq(sK61, sK62) & (((sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK62, nil)), sK64)) & ssList(sK64)) & ssList(sK63)) & ssItem(sK62)) & ssItem(sK61)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60, sK61, sK62, sK63, sK64])], [f352, f360, f359, f358, f357, f356, f355, f354, f353])).
fof(f353, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = X0) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f356, plain, (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => ((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f357, plain, (? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = sK57) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) => (? [X5] : (~ neq(sK61, X5) & ? [X6] : (? [X7] : ((sK57 = app(app(app(X6, cons(sK61, nil)), cons(X5, nil)), X7)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(sK61))), introduced(choice_axiom, [])).
fof(f358, plain, (? [X5] : (~ neq(sK61, X5) & ? [X6] : (? [X7] : ((sK57 = app(app(app(X6, cons(sK61, nil)), cons(X5, nil)), X7)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) => (~ neq(sK61, sK62) & ? [X6] : (? [X7] : ((sK57 = app(app(app(X6, cons(sK61, nil)), cons(sK62, nil)), X7)) & ssList(X7)) & ssList(X6)) & ssItem(sK62))), introduced(choice_axiom, [])).
fof(f359, plain, (? [X6] : (? [X7] : ((sK57 = app(app(app(X6, cons(sK61, nil)), cons(sK62, nil)), X7)) & ssList(X7)) & ssList(X6)) => (? [X7] : ((sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK62, nil)), X7)) & ssList(X7)) & ssList(sK63))), introduced(choice_axiom, [])).
fof(f360, plain, (? [X7] : ((sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK62, nil)), X7)) & ssList(X7)) => ((sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK62, nil)), sK64)) & ssList(sK64))), introduced(choice_axiom, [])).
fof(f352, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (~ neq(X4, X5) & ? [X6] : (? [X7] : ((app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = X0) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(rectify, [], [f234])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X9] : (? [X10] : (~ neq(X9, X10) & ? [X11] : (? [X12] : ((app(app(app(X11, cons(X9, nil)), cons(X10, nil)), X12) = X0) & ssList(X12)) & ssList(X11)) & ssItem(X10)) & ssItem(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & ? [X9] : (? [X10] : (~ neq(X9, X10) & ? [X11] : (? [X12] : ((app(app(app(X11, cons(X9, nil)), cons(X10, nil)), X12) = X0) & ssList(X12)) & ssList(X11)) & ssItem(X10)) & ssItem(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : ((! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2)) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & ? [X9] : (? [X10] : ((~ neq(X9, X10) & ? [X11] : (? [X12] : ((app(app(app(X11, cons(X9, nil)), cons(X10, nil)), X12) = X0) & ssList(X12)) & ssList(X11))) & ssItem(X10)) & ssItem(X9)) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | ! [X9] : (ssItem(X9) => ! [X10] : (ssItem(X10) => (neq(X9, X10) | ! [X11] : (ssList(X11) => ! [X12] : (ssList(X12) => ~ (app(app(app(X11, cons(X9, nil)), cons(X10, nil)), X12) = X0)))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X8] : (ssItem(X8) => ! [X9] : (ssList(X9) => ! [X10] : (ssList(X10) => (? [X12] : (lt(X12, X8) & memberP(X10, X12) & ssItem(X12)) | ? [X11] : (lt(X8, X11) & memberP(X9, X11) & ssItem(X11)) | ~ (app(app(X9, X2), X10) = X3) | ~ (cons(X8, nil) = X2)))))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => (neq(X4, X5) | ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ~ (app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = X0)))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X8] : (ssItem(X8) => ! [X9] : (ssList(X9) => ! [X10] : (ssList(X10) => (? [X12] : (lt(X12, X8) & memberP(X10, X12) & ssItem(X12)) | ? [X11] : (lt(X8, X11) & memberP(X9, X11) & ssItem(X11)) | ~ (app(app(X9, X2), X10) = X3) | ~ (cons(X8, nil) = X2)))))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => (neq(X4, X5) | ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ~ (app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = X0)))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', co1)).
fof(f277752, plain, (frontsegP(sK57, sK63) | ~ ssList(sK63) | ~ ssList(sK57) | ~ ssList(cons(sK61, nil)) | (~ spl65_28 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(resolution, [], [f234533, f1698])).
fof(f1698, plain, ! [X6, X4, X5] : (~ frontsegP(X4, app(X5, X6)) | frontsegP(X4, X5) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X6)), inference(subsumption_resolution, [], [f1695, f466])).
fof(f466, plain, ! [X0, X1] : (ssList(app(X0, X1)) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (ssList(app(X0, X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ssList(app(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax26)).
fof(f1695, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6)), inference(duplicate_literal_removal, [], [f1692])).
fof(f1692, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6) | ~ ssList(X5) | ~ ssList(app(X5, X6))), inference(resolution, [], [f486, f585])).
fof(f585, plain, ! [X2, X1] : (frontsegP(app(X1, X2), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X1, X2))), inference(equality_resolution, [], [f376])).
fof(f376, plain, ! [X2, X0, X1] : (frontsegP(X0, X1) | ~ (app(X1, X2) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1))) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f249, f250])).
fof(f250, plain, ! [X1, X0] : (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) => ((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1)))), introduced(choice_axiom, [])).
fof(f249, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f248])).
fof(f248, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X1, X2) = X0) & ssList(X2)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax5)).
fof(f486, plain, ! [X2, X0, X1] : (~ frontsegP(X1, X2) | frontsegP(X0, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(X0, X2) | ~ frontsegP(X1, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f150])).
fof(f150, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(X0, X2) | (~ frontsegP(X1, X2) | ~ frontsegP(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((frontsegP(X1, X2) & frontsegP(X0, X1)) => frontsegP(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax40)).
fof(f234533, plain, (frontsegP(sK57, app(sK63, cons(sK61, nil))) | (~ spl65_28 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f234532, f1934])).
fof(f234532, plain, (frontsegP(sK57, app(sK63, cons(sK61, nil))) | ~ ssList(cons(sK61, nil)) | (~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f234531, f200512])).
fof(f234531, plain, (frontsegP(sK57, app(sK63, cons(sK61, nil))) | ~ ssList(sK57) | ~ ssList(cons(sK61, nil)) | (~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f234501, f224357])).
fof(f224357, plain, (ssList(app(sK63, cons(sK61, nil))) | ~ spl65_8761), inference(avatar_component_clause, [], [f224356])).
fof(f224356, plain, (spl65_8761 <=> ssList(app(sK63, cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl65_8761])])).
fof(f234501, plain, (frontsegP(sK57, app(sK63, cons(sK61, nil))) | ~ ssList(app(sK63, cons(sK61, nil))) | ~ ssList(sK57) | ~ ssList(cons(sK61, nil)) | ~ spl65_8775), inference(resolution, [], [f224457, f1698])).
fof(f224457, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ spl65_8775), inference(avatar_component_clause, [], [f224455])).
fof(f224455, plain, (spl65_8775 <=> frontsegP(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl65_8775])])).
fof(f599416, plain, (spl65_23 | ~ spl65_62 | ~ spl65_88), inference(avatar_split_clause, [], [f594035, f2718, f2259, f1171])).
fof(f1171, plain, (spl65_23 <=> (nil = sK64)), introduced(avatar_definition, [new_symbols(naming, [spl65_23])])).
fof(f2718, plain, (spl65_88 <=> (sK57 = sK64)), introduced(avatar_definition, [new_symbols(naming, [spl65_88])])).
fof(f594035, plain, ((nil = sK64) | (~ spl65_62 | ~ spl65_88)), inference(backward_demodulation, [], [f2720, f2261])).
fof(f2720, plain, ((sK57 = sK64) | ~ spl65_88), inference(avatar_component_clause, [], [f2718])).
fof(f573507, plain, (spl65_7956 | ~ spl65_1885 | ~ spl65_6300 | ~ spl65_7957 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8769 | ~ spl65_8773 | ~ spl65_8775 | ~ spl65_18649), inference(avatar_split_clause, [], [f573504, f386890, f224455, f224440, f224415, f224171, f200511, f129586, f94590, f34276, f129581])).
fof(f34276, plain, (spl65_1885 <=> ssList(sK64)), introduced(avatar_definition, [new_symbols(naming, [spl65_1885])])).
fof(f94590, plain, (spl65_6300 <=> ssList(sK13(sK57, sK64))), introduced(avatar_definition, [new_symbols(naming, [spl65_6300])])).
fof(f129586, plain, (spl65_7957 <=> (hd(sK13(sK57, sK64)) = hd(app(sK13(sK57, sK64), sK57)))), introduced(avatar_definition, [new_symbols(naming, [spl65_7957])])).
fof(f224171, plain, (spl65_8745 <=> rearsegP(sK57, sK64)), introduced(avatar_definition, [new_symbols(naming, [spl65_8745])])).
fof(f224415, plain, (spl65_8769 <=> ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl65_8769])])).
fof(f224440, plain, (spl65_8773 <=> ! [X13] : (~ (sK57 = app(X13, sK64)) | ~ ssList(X13) | (app(app(sK63, cons(sK61, nil)), cons(sK61, nil)) = X13))), introduced(avatar_definition, [new_symbols(naming, [spl65_8773])])).
fof(f386890, plain, (spl65_18649 <=> (sK57 = sK12(sK57, sK13(sK57, sK64)))), introduced(avatar_definition, [new_symbols(naming, [spl65_18649])])).
fof(f573504, plain, ((hd(sK57) = hd(sK13(sK57, sK64))) | (~ spl65_1885 | ~ spl65_6300 | ~ spl65_7957 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8769 | ~ spl65_8773 | ~ spl65_8775 | ~ spl65_18649)), inference(backward_demodulation, [], [f129588, f573470])).
fof(f573470, plain, ((sK57 = app(sK13(sK57, sK64), sK57)) | (~ spl65_1885 | ~ spl65_6300 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8769 | ~ spl65_8773 | ~ spl65_8775 | ~ spl65_18649)), inference(backward_demodulation, [], [f277583, f386892])).
fof(f386892, plain, ((sK57 = sK12(sK57, sK13(sK57, sK64))) | ~ spl65_18649), inference(avatar_component_clause, [], [f386890])).
fof(f277583, plain, ((sK57 = app(sK13(sK57, sK64), sK12(sK57, sK13(sK57, sK64)))) | (~ spl65_1885 | ~ spl65_6300 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8769 | ~ spl65_8773 | ~ spl65_8775)), inference(backward_demodulation, [], [f234535, f277458])).
fof(f277458, plain, ((app(app(sK63, cons(sK61, nil)), cons(sK61, nil)) = sK13(sK57, sK64)) | (~ spl65_1885 | ~ spl65_6300 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8773)), inference(subsumption_resolution, [], [f277457, f94591])).
fof(f94591, plain, (ssList(sK13(sK57, sK64)) | ~ spl65_6300), inference(avatar_component_clause, [], [f94590])).
fof(f277457, plain, (~ ssList(sK13(sK57, sK64)) | (app(app(sK63, cons(sK61, nil)), cons(sK61, nil)) = sK13(sK57, sK64)) | (~ spl65_1885 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8773)), inference(trivial_inequality_removal, [], [f277418])).
fof(f277418, plain, (~ (sK57 = sK57) | ~ ssList(sK13(sK57, sK64)) | (app(app(sK63, cons(sK61, nil)), cons(sK61, nil)) = sK13(sK57, sK64)) | (~ spl65_1885 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8773)), inference(superposition, [], [f224441, f231649])).
fof(f231649, plain, ((sK57 = app(sK13(sK57, sK64), sK64)) | (~ spl65_1885 | ~ spl65_8397 | ~ spl65_8745)), inference(subsumption_resolution, [], [f231648, f200512])).
fof(f231648, plain, ((sK57 = app(sK13(sK57, sK64), sK64)) | ~ ssList(sK57) | (~ spl65_1885 | ~ spl65_8745)), inference(subsumption_resolution, [], [f231631, f34277])).
fof(f34277, plain, (ssList(sK64) | ~ spl65_1885), inference(avatar_component_clause, [], [f34276])).
fof(f231631, plain, ((sK57 = app(sK13(sK57, sK64), sK64)) | ~ ssList(sK64) | ~ ssList(sK57) | ~ spl65_8745), inference(resolution, [], [f224173, f378])).
fof(f378, plain, ! [X0, X1] : (~ rearsegP(X0, X1) | (app(sK13(X0, X1), X1) = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f255, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1))) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f253, f254])).
fof(f254, plain, ! [X1, X0] : (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) => ((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1)))), introduced(choice_axiom, [])).
fof(f253, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f252])).
fof(f252, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X2, X1) = X0) & ssList(X2)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : (! [X1] : ((rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax6)).
fof(f224173, plain, (rearsegP(sK57, sK64) | ~ spl65_8745), inference(avatar_component_clause, [], [f224171])).
fof(f224441, plain, (! [X13] : (~ (sK57 = app(X13, sK64)) | ~ ssList(X13) | (app(app(sK63, cons(sK61, nil)), cons(sK61, nil)) = X13)) | ~ spl65_8773), inference(avatar_component_clause, [], [f224440])).
fof(f234535, plain, ((sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), sK12(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil))))) | (~ spl65_8397 | ~ spl65_8769 | ~ spl65_8775)), inference(subsumption_resolution, [], [f234534, f200512])).
fof(f234534, plain, ((sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), sK12(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil))))) | ~ ssList(sK57) | (~ spl65_8769 | ~ spl65_8775)), inference(subsumption_resolution, [], [f234502, f224416])).
fof(f224416, plain, (ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ spl65_8769), inference(avatar_component_clause, [], [f224415])).
fof(f234502, plain, ((sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), sK12(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil))))) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(sK57) | ~ spl65_8775), inference(resolution, [], [f224457, f375])).
fof(f375, plain, ! [X0, X1] : (~ frontsegP(X0, X1) | (app(X1, sK12(X0, X1)) = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f129588, plain, ((hd(sK13(sK57, sK64)) = hd(app(sK13(sK57, sK64), sK57))) | ~ spl65_7957), inference(avatar_component_clause, [], [f129586])).
fof(f569079, plain, (~ spl65_87 | spl65_88 | ~ spl65_1885 | ~ spl65_8397 | ~ spl65_8745), inference(avatar_split_clause, [], [f569078, f224171, f200511, f34276, f2718, f2714])).
fof(f2714, plain, (spl65_87 <=> rearsegP(sK64, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl65_87])])).
fof(f569078, plain, ((sK57 = sK64) | ~ rearsegP(sK64, sK57) | (~ spl65_1885 | ~ spl65_8397 | ~ spl65_8745)), inference(subsumption_resolution, [], [f569077, f34277])).
fof(f569077, plain, ((sK57 = sK64) | ~ rearsegP(sK64, sK57) | ~ ssList(sK64) | (~ spl65_8397 | ~ spl65_8745)), inference(subsumption_resolution, [], [f556094, f200512])).
fof(f556094, plain, ((sK57 = sK64) | ~ rearsegP(sK64, sK57) | ~ ssList(sK57) | ~ ssList(sK64) | ~ spl65_8745), inference(resolution, [], [f224173, f497])).
fof(f497, plain, ! [X0, X1] : (~ rearsegP(X1, X0) | (X0 = X1) | ~ rearsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f163])).
fof(f163, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ rearsegP(X1, X0) | ~ rearsegP(X0, X1) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f162])).
fof(f162, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ rearsegP(X1, X0) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f48])).
fof(f48, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((rearsegP(X1, X0) & rearsegP(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax48)).
fof(f567728, plain, (~ spl65_280 | ~ spl65_287 | spl65_7832 | ~ spl65_11734 | ~ spl65_21242), inference(avatar_contradiction_clause, [], [f567727])).
fof(f567727, plain, ($false | (~ spl65_280 | ~ spl65_287 | spl65_7832 | ~ spl65_11734 | ~ spl65_21242)), inference(subsumption_resolution, [], [f567726, f127703])).
fof(f127703, plain, (~ (sK61 = hd(sK57)) | spl65_7832), inference(avatar_component_clause, [], [f127702])).
fof(f567726, plain, ((sK61 = hd(sK57)) | (~ spl65_280 | ~ spl65_287 | ~ spl65_11734 | ~ spl65_21242)), inference(subsumption_resolution, [], [f567145, f258377])).
fof(f258377, plain, (ssItem(sK61) | ~ spl65_11734), inference(avatar_component_clause, [], [f258376])).
fof(f258376, plain, (spl65_11734 <=> ssItem(sK61)), introduced(avatar_definition, [new_symbols(naming, [spl65_11734])])).
fof(f567145, plain, (~ ssItem(sK61) | (sK61 = hd(sK57)) | (~ spl65_280 | ~ spl65_287 | ~ spl65_21242)), inference(resolution, [], [f170706, f493334])).
fof(f170706, plain, (! [X8] : (~ memberP(sK57, X8) | ~ ssItem(X8) | (hd(sK57) = X8)) | (~ spl65_280 | ~ spl65_287)), inference(subsumption_resolution, [], [f14336, f484])).
fof(f484, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (ssItem(X0) => ~ memberP(nil, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax38)).
fof(f14336, plain, (! [X8] : (memberP(nil, X8) | ~ memberP(sK57, X8) | ~ ssItem(X8) | (hd(sK57) = X8)) | (~ spl65_280 | ~ spl65_287)), inference(backward_demodulation, [], [f4152, f4181])).
fof(f4181, plain, ((nil = tl(sK57)) | ~ spl65_287), inference(avatar_component_clause, [], [f4179])).
fof(f4179, plain, (spl65_287 <=> (nil = tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_287])])).
fof(f4152, plain, (! [X8] : (~ memberP(sK57, X8) | ~ ssItem(X8) | memberP(tl(sK57), X8) | (hd(sK57) = X8)) | ~ spl65_280), inference(avatar_component_clause, [], [f4151])).
fof(f4151, plain, (spl65_280 <=> ! [X8] : (~ memberP(sK57, X8) | ~ ssItem(X8) | memberP(tl(sK57), X8) | (hd(sK57) = X8))), introduced(avatar_definition, [new_symbols(naming, [spl65_280])])).
fof(f542848, plain, (~ spl65_88 | ~ spl65_2995 | ~ spl65_5385 | ~ spl65_11734 | ~ spl65_16416), inference(avatar_contradiction_clause, [], [f542847])).
fof(f542847, plain, ($false | (~ spl65_88 | ~ spl65_2995 | ~ spl65_5385 | ~ spl65_11734 | ~ spl65_16416)), inference(subsumption_resolution, [], [f542816, f258377])).
fof(f542816, plain, (~ ssItem(sK61) | (~ spl65_88 | ~ spl65_2995 | ~ spl65_5385 | ~ spl65_16416)), inference(resolution, [], [f531096, f484])).
fof(f531096, plain, (memberP(nil, sK61) | (~ spl65_88 | ~ spl65_2995 | ~ spl65_5385 | ~ spl65_16416)), inference(forward_demodulation, [], [f531095, f57826])).
fof(f57826, plain, ((sK61 = hd(nil)) | ~ spl65_2995), inference(avatar_component_clause, [], [f57824])).
fof(f57824, plain, (spl65_2995 <=> (sK61 = hd(nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_2995])])).
fof(f531095, plain, (memberP(nil, hd(nil)) | (~ spl65_88 | ~ spl65_5385 | ~ spl65_16416)), inference(forward_demodulation, [], [f531094, f88397])).
fof(f88397, plain, ((nil = sK13(sK57, sK57)) | ~ spl65_5385), inference(avatar_component_clause, [], [f88395])).
fof(f88395, plain, (spl65_5385 <=> (nil = sK13(sK57, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_5385])])).
fof(f531094, plain, (memberP(sK13(sK57, sK57), hd(sK13(sK57, sK57))) | (~ spl65_88 | ~ spl65_16416)), inference(forward_demodulation, [], [f355262, f2720])).
fof(f355262, plain, (memberP(sK13(sK57, sK64), hd(sK13(sK57, sK64))) | ~ spl65_16416), inference(avatar_component_clause, [], [f355260])).
fof(f535128, plain, (~ spl65_801 | ~ spl65_88 | spl65_6872), inference(avatar_split_clause, [], [f535127, f102273, f2718, f13066])).
fof(f13066, plain, (spl65_801 <=> (sK57 = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl65_801])])).
fof(f535127, plain, (~ (sK57 = sK58) | (~ spl65_88 | spl65_6872)), inference(forward_demodulation, [], [f102274, f2720])).
fof(f102274, plain, (~ (sK58 = sK64) | spl65_6872), inference(avatar_component_clause, [], [f102273])).
fof(f530798, plain, (~ spl65_8769 | spl65_21242 | ~ spl65_8761 | ~ spl65_8771 | ~ spl65_11734), inference(avatar_split_clause, [], [f530797, f258376, f224424, f224356, f493332, f224415])).
fof(f224424, plain, (spl65_8771 <=> ! [X7] : (memberP(sK57, X7) | ~ ssItem(X7) | ~ memberP(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), X7))), introduced(avatar_definition, [new_symbols(naming, [spl65_8771])])).
fof(f530797, plain, (memberP(sK57, sK61) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | (~ spl65_8761 | ~ spl65_8771 | ~ spl65_11734)), inference(subsumption_resolution, [], [f493337, f224357])).
fof(f493337, plain, (memberP(sK57, sK61) | ~ ssList(app(sK63, cons(sK61, nil))) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | (~ spl65_8771 | ~ spl65_11734)), inference(subsumption_resolution, [], [f493336, f454])).
fof(f454, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax17)).
fof(f493336, plain, (memberP(sK57, sK61) | ~ ssList(nil) | ~ ssList(app(sK63, cons(sK61, nil))) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | (~ spl65_8771 | ~ spl65_11734)), inference(subsumption_resolution, [], [f263180, f258377])).
fof(f263180, plain, (~ ssItem(sK61) | memberP(sK57, sK61) | ~ ssList(nil) | ~ ssList(app(sK63, cons(sK61, nil))) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ spl65_8771), inference(duplicate_literal_removal, [], [f263179])).
fof(f263179, plain, (~ ssItem(sK61) | memberP(sK57, sK61) | ~ ssList(nil) | ~ ssList(app(sK63, cons(sK61, nil))) | ~ ssItem(sK61) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ spl65_8771), inference(resolution, [], [f224425, f583])).
fof(f583, plain, ! [X2, X3, X1] : (memberP(app(X2, cons(X1, X3)), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(app(X2, cons(X1, X3)))), inference(equality_resolution, [], [f370])).
fof(f370, plain, ! [X2, X0, X3, X1] : (memberP(X0, X1) | ~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(sK9(X0, X1), cons(X1, sK10(X0, X1))) = X0) & ssList(sK10(X0, X1))) & ssList(sK9(X0, X1))) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9, sK10])], [f240, f242, f241])).
fof(f241, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(sK9(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) & ssList(sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f242, plain, ! [X1, X0] : (? [X5] : ((app(sK9(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) => ((app(sK9(X0, X1), cons(X1, sK10(X0, X1))) = X0) & ssList(sK10(X0, X1)))), introduced(choice_axiom, [])).
fof(f240, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(rectify, [], [f239])).
fof(f239, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f100])).
fof(f100, plain, ! [X0] : (! [X1] : ((memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax3)).
fof(f224425, plain, (! [X7] : (~ memberP(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), X7) | ~ ssItem(X7) | memberP(sK57, X7)) | ~ spl65_8771), inference(avatar_component_clause, [], [f224424])).
fof(f493635, plain, (~ spl65_382 | ~ spl65_88 | ~ spl65_5385 | spl65_6301), inference(avatar_split_clause, [], [f493634, f94594, f88395, f2718, f6427])).
fof(f6427, plain, (spl65_382 <=> (nil = app(cons(sK61, nil), cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl65_382])])).
fof(f94594, plain, (spl65_6301 <=> (app(cons(sK61, nil), cons(sK61, nil)) = sK13(sK57, sK64))), introduced(avatar_definition, [new_symbols(naming, [spl65_6301])])).
fof(f493634, plain, (~ (nil = app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_88 | ~ spl65_5385 | spl65_6301)), inference(forward_demodulation, [], [f493633, f88397])).
fof(f493633, plain, (~ (app(cons(sK61, nil), cons(sK61, nil)) = sK13(sK57, sK57)) | (~ spl65_88 | spl65_6301)), inference(forward_demodulation, [], [f94595, f2720])).
fof(f94595, plain, (~ (app(cons(sK61, nil), cons(sK61, nil)) = sK13(sK57, sK64)) | spl65_6301), inference(avatar_component_clause, [], [f94594])).
fof(f490931, plain, (~ spl65_28 | spl65_86 | ~ spl65_133 | ~ spl65_283 | ~ spl65_371 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8679 | ~ spl65_8761 | ~ spl65_8775), inference(avatar_contradiction_clause, [], [f490930])).
fof(f490930, plain, ($false | (~ spl65_28 | spl65_86 | ~ spl65_133 | ~ spl65_283 | ~ spl65_371 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8679 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f490929, f2701])).
fof(f2701, plain, (~ (sK57 = sK63) | spl65_86), inference(avatar_component_clause, [], [f2700])).
fof(f2700, plain, (spl65_86 <=> (sK57 = sK63)), introduced(avatar_definition, [new_symbols(naming, [spl65_86])])).
fof(f490929, plain, ((sK57 = sK63) | (~ spl65_28 | ~ spl65_133 | ~ spl65_283 | ~ spl65_371 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8679 | ~ spl65_8761 | ~ spl65_8775)), inference(forward_demodulation, [], [f490148, f6310])).
fof(f6310, plain, ((sK57 = cons(sK61, nil)) | ~ spl65_371), inference(avatar_component_clause, [], [f6308])).
fof(f6308, plain, (spl65_371 <=> (sK57 = cons(sK61, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_371])])).
fof(f490148, plain, ((cons(sK61, nil) = sK63) | (~ spl65_28 | ~ spl65_133 | ~ spl65_283 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8679 | ~ spl65_8761 | ~ spl65_8775)), inference(backward_demodulation, [], [f217389, f490127])).
fof(f490127, plain, ((sK61 = hd(sK63)) | (~ spl65_28 | ~ spl65_133 | ~ spl65_283 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8679 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f490126, f454])).
fof(f490126, plain, ((sK61 = hd(sK63)) | ~ ssList(nil) | (~ spl65_28 | ~ spl65_133 | ~ spl65_283 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8679 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f490121, f3337])).
fof(f3337, plain, (ssItem(hd(sK63)) | ~ spl65_133), inference(avatar_component_clause, [], [f3336])).
fof(f3336, plain, (spl65_133 <=> ssItem(hd(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl65_133])])).
fof(f490121, plain, ((sK61 = hd(sK63)) | ~ ssItem(hd(sK63)) | ~ ssList(nil) | (~ spl65_28 | ~ spl65_283 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8679 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f490114, f277776])).
fof(f490114, plain, (~ frontsegP(sK57, sK63) | (sK61 = hd(sK63)) | ~ ssItem(hd(sK63)) | ~ ssList(nil) | (~ spl65_283 | ~ spl65_7832 | ~ spl65_8679)), inference(superposition, [], [f331732, f217389])).
fof(f331732, plain, (! [X12, X13] : (~ frontsegP(sK57, cons(X12, X13)) | (sK61 = X12) | ~ ssItem(X12) | ~ ssList(X13)) | (~ spl65_283 | ~ spl65_7832)), inference(backward_demodulation, [], [f4164, f127704])).
fof(f4164, plain, (! [X12, X13] : (~ frontsegP(sK57, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (hd(sK57) = X12)) | ~ spl65_283), inference(avatar_component_clause, [], [f4163])).
fof(f4163, plain, (spl65_283 <=> ! [X13, X12] : (~ frontsegP(sK57, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (hd(sK57) = X12))), introduced(avatar_definition, [new_symbols(naming, [spl65_283])])).
fof(f217389, plain, ((sK63 = cons(hd(sK63), nil)) | ~ spl65_8679), inference(avatar_component_clause, [], [f217387])).
fof(f217387, plain, (spl65_8679 <=> (sK63 = cons(hd(sK63), nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_8679])])).
fof(f456689, plain, (~ spl65_28 | ~ spl65_86 | ~ spl65_371 | ~ spl65_7832 | spl65_8064 | ~ spl65_8397 | ~ spl65_8462 | ~ spl65_8503 | ~ spl65_8761 | ~ spl65_8775), inference(avatar_contradiction_clause, [], [f456688])).
fof(f456688, plain, ($false | (~ spl65_28 | ~ spl65_86 | ~ spl65_371 | ~ spl65_7832 | spl65_8064 | ~ spl65_8397 | ~ spl65_8462 | ~ spl65_8503 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f456687, f147390])).
fof(f147390, plain, (~ frontsegP(sK57, cons(sK61, sK57)) | spl65_8064), inference(avatar_component_clause, [], [f147389])).
fof(f147389, plain, (spl65_8064 <=> frontsegP(sK57, cons(sK61, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_8064])])).
fof(f456687, plain, (frontsegP(sK57, cons(sK61, sK57)) | (~ spl65_28 | ~ spl65_86 | ~ spl65_371 | ~ spl65_7832 | ~ spl65_8397 | ~ spl65_8462 | ~ spl65_8503 | ~ spl65_8761 | ~ spl65_8775)), inference(forward_demodulation, [], [f453165, f331883])).
fof(f331883, plain, ((cons(sK61, sK57) = app(sK57, sK57)) | (~ spl65_7832 | ~ spl65_8462 | ~ spl65_8503)), inference(backward_demodulation, [], [f216188, f127704])).
fof(f216188, plain, ((cons(hd(sK57), sK57) = app(sK57, sK57)) | (~ spl65_8462 | ~ spl65_8503)), inference(forward_demodulation, [], [f201809, f201247])).
fof(f201247, plain, ((hd(sK57) = hd(app(sK57, sK57))) | ~ spl65_8462), inference(avatar_component_clause, [], [f201245])).
fof(f201245, plain, (spl65_8462 <=> (hd(sK57) = hd(app(sK57, sK57)))), introduced(avatar_definition, [new_symbols(naming, [spl65_8462])])).
fof(f201809, plain, ((app(sK57, sK57) = cons(hd(app(sK57, sK57)), sK57)) | ~ spl65_8503), inference(avatar_component_clause, [], [f201807])).
fof(f201807, plain, (spl65_8503 <=> (app(sK57, sK57) = cons(hd(app(sK57, sK57)), sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_8503])])).
fof(f453165, plain, (frontsegP(sK57, app(sK57, sK57)) | (~ spl65_28 | ~ spl65_86 | ~ spl65_371 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(backward_demodulation, [], [f332980, f2702])).
fof(f2702, plain, ((sK57 = sK63) | ~ spl65_86), inference(avatar_component_clause, [], [f2700])).
fof(f332980, plain, (frontsegP(sK57, app(sK63, sK57)) | (~ spl65_28 | ~ spl65_371 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(backward_demodulation, [], [f234533, f6310])).
fof(f443485, plain, (spl65_111 | spl65_21 | ~ spl65_100 | ~ spl65_133 | ~ spl65_144 | ~ spl65_366), inference(avatar_split_clause, [], [f443178, f5333, f3383, f3336, f3153, f1162, f3200])).
fof(f3200, plain, (spl65_111 <=> (nil = sK50(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl65_111])])).
fof(f1162, plain, (spl65_21 <=> (nil = sK63)), introduced(avatar_definition, [new_symbols(naming, [spl65_21])])).
fof(f3153, plain, (spl65_100 <=> ssItem(sK51(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl65_100])])).
fof(f3383, plain, (spl65_144 <=> (nil = tl(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl65_144])])).
fof(f5333, plain, (spl65_366 <=> (sK63 = cons(sK51(sK63), sK50(sK63)))), introduced(avatar_definition, [new_symbols(naming, [spl65_366])])).
fof(f443178, plain, ((nil = sK50(sK63)) | (spl65_21 | ~ spl65_100 | ~ spl65_133 | ~ spl65_144 | ~ spl65_366)), inference(backward_demodulation, [], [f355375, f3385])).
fof(f3385, plain, ((nil = tl(sK63)) | ~ spl65_144), inference(avatar_component_clause, [], [f3383])).
fof(f355375, plain, ((tl(sK63) = sK50(sK63)) | (spl65_21 | ~ spl65_100 | ~ spl65_133 | ~ spl65_366)), inference(forward_demodulation, [], [f355351, f336791])).
fof(f336791, plain, ((sK63 = cons(hd(sK63), sK50(sK63))) | (spl65_21 | ~ spl65_100 | ~ spl65_366)), inference(backward_demodulation, [], [f5335, f336766])).
fof(f336766, plain, ((hd(sK63) = sK51(sK63)) | (spl65_21 | ~ spl65_100 | ~ spl65_366)), inference(forward_demodulation, [], [f336761, f5335])).
fof(f336761, plain, ((sK51(sK63) = hd(cons(sK51(sK63), sK50(sK63)))) | (spl65_21 | ~ spl65_100)), inference(resolution, [], [f223635, f3154])).
fof(f3154, plain, (ssItem(sK51(sK63)) | ~ spl65_100), inference(avatar_component_clause, [], [f3153])).
fof(f223635, plain, (! [X29] : (~ ssItem(X29) | (hd(cons(X29, sK50(sK63))) = X29)) | spl65_21), inference(subsumption_resolution, [], [f223376, f1163])).
fof(f1163, plain, (~ (nil = sK63) | spl65_21), inference(avatar_component_clause, [], [f1162])).
fof(f223376, plain, ! [X29] : ((hd(cons(X29, sK50(sK63))) = X29) | (nil = sK63) | ~ ssItem(X29)), inference(resolution, [], [f576, f1066])).
fof(f1066, plain, ! [X50, X49] : (~ ssList(X50) | (hd(cons(X49, sK50(X50))) = X49) | (nil = X50) | ~ ssItem(X49)), inference(resolution, [], [f463, f458])).
fof(f458, plain, ! [X0] : (ssList(sK50(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f322, plain, ! [X0] : ((((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0))) & ssList(sK50(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK50, sK51])], [f125, f321, f320])).
fof(f320, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) => (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) & ssList(sK50(X0)))), introduced(choice_axiom, [])).
fof(f321, plain, ! [X0] : (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) => ((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0)))), introduced(choice_axiom, [])).
fof(f125, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0] : ((? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (ssList(X0) => (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax20)).
fof(f463, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax23)).
fof(f5335, plain, ((sK63 = cons(sK51(sK63), sK50(sK63))) | ~ spl65_366), inference(avatar_component_clause, [], [f5333])).
fof(f355351, plain, ((sK50(sK63) = tl(cons(hd(sK63), sK50(sK63)))) | (spl65_21 | ~ spl65_133)), inference(resolution, [], [f223650, f3337])).
fof(f223650, plain, (! [X47] : (~ ssItem(X47) | (sK50(sK63) = tl(cons(X47, sK50(sK63))))) | spl65_21), inference(subsumption_resolution, [], [f223392, f1163])).
fof(f223392, plain, ! [X47] : ((sK50(sK63) = tl(cons(X47, sK50(sK63)))) | (nil = sK63) | ~ ssItem(X47)), inference(resolution, [], [f576, f1098])).
fof(f1098, plain, ! [X50, X49] : (~ ssList(X50) | (sK50(X50) = tl(cons(X49, sK50(X50)))) | (nil = X50) | ~ ssItem(X49)), inference(resolution, [], [f465, f458])).
fof(f465, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax25)).
fof(f442294, plain, (spl65_144 | spl65_21 | ~ spl65_28 | ~ spl65_132 | ~ spl65_133 | ~ spl65_448 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775), inference(avatar_split_clause, [], [f442293, f224455, f224356, f200511, f8129, f3336, f3332, f1933, f1162, f3383])).
fof(f3332, plain, (spl65_132 <=> ssList(tl(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl65_132])])).
fof(f8129, plain, (spl65_448 <=> ! [X16, X17] : (~ frontsegP(sK57, cons(X16, X17)) | ~ ssItem(X16) | ~ ssList(X17) | frontsegP(nil, X17))), introduced(avatar_definition, [new_symbols(naming, [spl65_448])])).
fof(f442293, plain, ((nil = tl(sK63)) | (spl65_21 | ~ spl65_28 | ~ spl65_132 | ~ spl65_133 | ~ spl65_448 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f441666, f3333])).
fof(f3333, plain, (ssList(tl(sK63)) | ~ spl65_132), inference(avatar_component_clause, [], [f3332])).
fof(f441666, plain, ((nil = tl(sK63)) | ~ ssList(tl(sK63)) | (spl65_21 | ~ spl65_28 | ~ spl65_132 | ~ spl65_133 | ~ spl65_448 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(resolution, [], [f439603, f494])).
fof(f494, plain, ! [X0] : (~ frontsegP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f331])).
fof(f331, plain, ! [X0] : (((frontsegP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ frontsegP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f159])).
fof(f159, plain, ! [X0] : ((frontsegP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f46])).
fof(f46, plain, ! [X0] : (ssList(X0) => (frontsegP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax46)).
fof(f439603, plain, (frontsegP(nil, tl(sK63)) | (spl65_21 | ~ spl65_28 | ~ spl65_132 | ~ spl65_133 | ~ spl65_448 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f439602, f3333])).
fof(f439602, plain, (~ ssList(tl(sK63)) | frontsegP(nil, tl(sK63)) | (spl65_21 | ~ spl65_28 | ~ spl65_133 | ~ spl65_448 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f439601, f3337])).
fof(f439601, plain, (~ ssItem(hd(sK63)) | ~ ssList(tl(sK63)) | frontsegP(nil, tl(sK63)) | (spl65_21 | ~ spl65_28 | ~ spl65_448 | ~ spl65_8397 | ~ spl65_8761 | ~ spl65_8775)), inference(subsumption_resolution, [], [f439591, f277776])).
fof(f439591, plain, (~ frontsegP(sK57, sK63) | ~ ssItem(hd(sK63)) | ~ ssList(tl(sK63)) | frontsegP(nil, tl(sK63)) | (spl65_21 | ~ spl65_448)), inference(superposition, [], [f8130, f222906])).
fof(f222906, plain, ((sK63 = cons(hd(sK63), tl(sK63))) | spl65_21), inference(subsumption_resolution, [], [f2122, f1163])).
fof(f2122, plain, ((nil = sK63) | (sK63 = cons(hd(sK63), tl(sK63)))), inference(resolution, [], [f576, f539])).
fof(f539, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax78)).
fof(f8130, plain, (! [X17, X16] : (~ frontsegP(sK57, cons(X16, X17)) | ~ ssItem(X16) | ~ ssList(X17) | frontsegP(nil, X17)) | ~ spl65_448), inference(avatar_component_clause, [], [f8129])).
fof(f416028, plain, (~ spl65_371 | ~ spl65_958 | ~ spl65_7832 | spl65_7939 | ~ spl65_8462 | ~ spl65_8503), inference(avatar_contradiction_clause, [], [f416027])).
fof(f416027, plain, ($false | (~ spl65_371 | ~ spl65_958 | ~ spl65_7832 | spl65_7939 | ~ spl65_8462 | ~ spl65_8503)), inference(subsumption_resolution, [], [f416026, f129249])).
fof(f129249, plain, (~ (sK57 = cons(sK61, sK57)) | spl65_7939), inference(avatar_component_clause, [], [f129248])).
fof(f129248, plain, (spl65_7939 <=> (sK57 = cons(sK61, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_7939])])).
fof(f416026, plain, ((sK57 = cons(sK61, sK57)) | (~ spl65_371 | ~ spl65_958 | ~ spl65_7832 | ~ spl65_8462 | ~ spl65_8503)), inference(forward_demodulation, [], [f331883, f413879])).
fof(f413879, plain, ((sK57 = app(sK57, sK57)) | (~ spl65_371 | ~ spl65_958)), inference(forward_demodulation, [], [f15567, f6310])).
fof(f15567, plain, ((sK57 = app(cons(sK61, nil), cons(sK61, nil))) | ~ spl65_958), inference(avatar_component_clause, [], [f15565])).
fof(f15565, plain, (spl65_958 <=> (sK57 = app(cons(sK61, nil), cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl65_958])])).
fof(f400177, plain, (~ spl65_1947 | ~ spl65_4231 | ~ spl65_4233 | spl65_18650), inference(avatar_contradiction_clause, [], [f400176])).
fof(f400176, plain, ($false | (~ spl65_1947 | ~ spl65_4231 | ~ spl65_4233 | spl65_18650)), inference(subsumption_resolution, [], [f400175, f273877])).
fof(f273877, plain, (rearsegP(sK57, sK57) | (~ spl65_4231 | ~ spl65_4233)), inference(backward_demodulation, [], [f69759, f69770])).
fof(f69770, plain, ((sK57 = sK12(sK57, nil)) | ~ spl65_4233), inference(avatar_component_clause, [], [f69768])).
fof(f69768, plain, (spl65_4233 <=> (sK57 = sK12(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_4233])])).
fof(f69759, plain, (rearsegP(sK57, sK12(sK57, nil)) | ~ spl65_4231), inference(avatar_component_clause, [], [f69757])).
fof(f69757, plain, (spl65_4231 <=> rearsegP(sK57, sK12(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_4231])])).
fof(f400175, plain, (~ rearsegP(sK57, sK57) | (~ spl65_1947 | ~ spl65_4233 | spl65_18650)), inference(forward_demodulation, [], [f400174, f69770])).
fof(f400174, plain, (~ rearsegP(sK12(sK57, nil), sK57) | (~ spl65_1947 | spl65_18650)), inference(forward_demodulation, [], [f386896, f39897])).
fof(f386896, plain, (~ rearsegP(sK12(sK57, sK13(sK57, sK64)), sK57) | spl65_18650), inference(avatar_component_clause, [], [f386894])).
fof(f386894, plain, (spl65_18650 <=> rearsegP(sK12(sK57, sK13(sK57, sK64)), sK57)), introduced(avatar_definition, [new_symbols(naming, [spl65_18650])])).
fof(f398934, plain, (~ spl65_1947 | ~ spl65_4233 | ~ spl65_8397 | spl65_18640), inference(avatar_contradiction_clause, [], [f398933])).
fof(f398933, plain, ($false | (~ spl65_1947 | ~ spl65_4233 | ~ spl65_8397 | spl65_18640)), inference(subsumption_resolution, [], [f398932, f200512])).
fof(f398932, plain, (~ ssList(sK57) | (~ spl65_1947 | ~ spl65_4233 | spl65_18640)), inference(forward_demodulation, [], [f398831, f69770])).
fof(f398831, plain, (~ ssList(sK12(sK57, nil)) | (~ spl65_1947 | spl65_18640)), inference(backward_demodulation, [], [f386842, f39897])).
fof(f386842, plain, (~ ssList(sK12(sK57, sK13(sK57, sK64))) | spl65_18640), inference(avatar_component_clause, [], [f386840])).
fof(f386840, plain, (spl65_18640 <=> ssList(sK12(sK57, sK13(sK57, sK64)))), introduced(avatar_definition, [new_symbols(naming, [spl65_18640])])).
fof(f386897, plain, (spl65_18649 | ~ spl65_18640 | ~ spl65_18650 | ~ spl65_1885 | ~ spl65_6300 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8769 | ~ spl65_8773 | ~ spl65_8775), inference(avatar_split_clause, [], [f386888, f224455, f224440, f224415, f224171, f200511, f94590, f34276, f386894, f386840, f386890])).
fof(f386888, plain, (~ rearsegP(sK12(sK57, sK13(sK57, sK64)), sK57) | ~ ssList(sK12(sK57, sK13(sK57, sK64))) | (sK57 = sK12(sK57, sK13(sK57, sK64))) | (~ spl65_1885 | ~ spl65_6300 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8769 | ~ spl65_8773 | ~ spl65_8775)), inference(subsumption_resolution, [], [f386821, f94591])).
fof(f386821, plain, (~ rearsegP(sK12(sK57, sK13(sK57, sK64)), sK57) | ~ ssList(sK12(sK57, sK13(sK57, sK64))) | (sK57 = sK12(sK57, sK13(sK57, sK64))) | ~ ssList(sK13(sK57, sK64)) | (~ spl65_1885 | ~ spl65_6300 | ~ spl65_8397 | ~ spl65_8745 | ~ spl65_8769 | ~ spl65_8773 | ~ spl65_8775)), inference(superposition, [], [f1574, f277583])).
fof(f1574, plain, ! [X0, X1] : (~ rearsegP(X1, app(X0, X1)) | ~ ssList(X1) | (app(X0, X1) = X1) | ~ ssList(X0)), inference(subsumption_resolution, [], [f1573, f466])).
fof(f1573, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X0, X1)) | (app(X0, X1) = X1) | ~ rearsegP(X1, app(X0, X1))), inference(duplicate_literal_removal, [], [f1568])).
fof(f1568, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X0, X1)) | (app(X0, X1) = X1) | ~ rearsegP(X1, app(X0, X1)) | ~ ssList(app(X0, X1)) | ~ ssList(X1)), inference(resolution, [], [f586, f497])).
fof(f586, plain, ! [X2, X1] : (rearsegP(app(X2, X1), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X2, X1))), inference(equality_resolution, [], [f379])).
fof(f379, plain, ! [X2, X0, X1] : (rearsegP(X0, X1) | ~ (app(X2, X1) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f342397, plain, (spl65_5385 | ~ spl65_4485), inference(avatar_split_clause, [], [f342396, f71414, f88395])).
fof(f71414, plain, (spl65_4485 <=> ssList(sK13(sK57, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_4485])])).
fof(f342396, plain, ((nil = sK13(sK57, sK57)) | ~ spl65_4485), inference(subsumption_resolution, [], [f342386, f71415])).
fof(f71415, plain, (ssList(sK13(sK57, sK57)) | ~ spl65_4485), inference(avatar_component_clause, [], [f71414])).
fof(f342386, plain, ((nil = sK13(sK57, sK57)) | ~ ssList(sK13(sK57, sK57))), inference(trivial_inequality_removal, [], [f342385])).
fof(f342385, plain, (~ (sK57 = sK57) | (nil = sK13(sK57, sK57)) | ~ ssList(sK13(sK57, sK57))), inference(superposition, [], [f3009, f164470])).
fof(f164470, plain, (sK57 = app(sK13(sK57, sK57), sK57)), inference(resolution, [], [f631, f1592])).
fof(f1592, plain, ! [X0] : (~ ssList(X0) | (app(sK13(X0, X0), X0) = X0)), inference(duplicate_literal_removal, [], [f1587])).
fof(f1587, plain, ! [X0] : ((app(sK13(X0, X0), X0) = X0) | ~ ssList(X0) | ~ ssList(X0) | ~ ssList(X0)), inference(resolution, [], [f378, f498])).
fof(f498, plain, ! [X0] : (rearsegP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : (rearsegP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (ssList(X0) => rearsegP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax49)).
fof(f631, plain, ssList(sK57), inference(forward_demodulation, [], [f570, f573])).
fof(f573, plain, (sK57 = sK59), inference(cnf_transformation, [], [f361])).
fof(f570, plain, ssList(sK59), inference(cnf_transformation, [], [f361])).
fof(f3009, plain, ! [X7] : (~ (sK57 = app(X7, sK57)) | (nil = X7) | ~ ssList(X7)), inference(subsumption_resolution, [], [f3008, f631])).
fof(f3008, plain, ! [X7] : (~ (sK57 = app(X7, sK57)) | (nil = X7) | ~ ssList(sK57) | ~ ssList(X7)), inference(subsumption_resolution, [], [f2990, f454])).
fof(f2990, plain, ! [X7] : (~ (sK57 = app(X7, sK57)) | (nil = X7) | ~ ssList(nil) | ~ ssList(sK57) | ~ ssList(X7)), inference(superposition, [], [f540, f2228])).
fof(f2228, plain, (sK57 = app(nil, sK57)), inference(resolution, [], [f631, f468])).
fof(f468, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax28)).
fof(f540, plain, ! [X2, X0, X1] : (~ (app(X2, X1) = app(X0, X1)) | (X0 = X2) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ! [X0] : (! [X1] : (! [X2] : ((X0 = X2) | ~ (app(X2, X1) = app(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f195])).
fof(f195, plain, ! [X0] : (! [X1] : (! [X2] : (((X0 = X2) | ~ (app(X2, X1) = app(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((app(X2, X1) = app(X0, X1)) => (X0 = X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax79)).
fof(f333205, plain, (~ spl65_371 | spl65_957 | ~ spl65_7832 | ~ spl65_8059 | ~ spl65_8462 | ~ spl65_8503), inference(avatar_contradiction_clause, [], [f333204])).
fof(f333204, plain, ($false | (~ spl65_371 | spl65_957 | ~ spl65_7832 | ~ spl65_8059 | ~ spl65_8462 | ~ spl65_8503)), inference(subsumption_resolution, [], [f333203, f147313])).
fof(f147313, plain, (frontsegP(cons(sK61, sK57), sK57) | ~ spl65_8059), inference(avatar_component_clause, [], [f147312])).
fof(f147312, plain, (spl65_8059 <=> frontsegP(cons(sK61, sK57), sK57)), introduced(avatar_definition, [new_symbols(naming, [spl65_8059])])).
fof(f333203, plain, (~ frontsegP(cons(sK61, sK57), sK57) | (~ spl65_371 | spl65_957 | ~ spl65_7832 | ~ spl65_8462 | ~ spl65_8503)), inference(forward_demodulation, [], [f332633, f331883])).
fof(f332633, plain, (~ frontsegP(app(sK57, sK57), sK57) | (~ spl65_371 | spl65_957)), inference(backward_demodulation, [], [f15563, f6310])).
fof(f15563, plain, (~ frontsegP(app(cons(sK61, nil), cons(sK61, nil)), sK57) | spl65_957), inference(avatar_component_clause, [], [f15561])).
fof(f15561, plain, (spl65_957 <=> frontsegP(app(cons(sK61, nil), cons(sK61, nil)), sK57)), introduced(avatar_definition, [new_symbols(naming, [spl65_957])])).
fof(f302229, plain, (~ spl65_12077 | ~ spl65_12086 | spl65_21), inference(avatar_split_clause, [], [f302228, f1162, f269828, f269777])).
fof(f269777, plain, (spl65_12077 <=> ssList(sK12(sK63, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_12077])])).
fof(f302228, plain, (~ frontsegP(nil, sK63) | ~ ssList(sK12(sK63, nil)) | spl65_21), inference(subsumption_resolution, [], [f302227, f1163])).
fof(f302227, plain, (~ frontsegP(nil, sK63) | (nil = sK63) | ~ ssList(sK12(sK63, nil))), inference(subsumption_resolution, [], [f299942, f454])).
fof(f299942, plain, (~ frontsegP(nil, sK63) | ~ ssList(nil) | (nil = sK63) | ~ ssList(sK12(sK63, nil))), inference(superposition, [], [f1565, f5060])).
fof(f5060, plain, (sK63 = app(nil, sK12(sK63, nil))), inference(resolution, [], [f1583, f576])).
fof(f1583, plain, ! [X1] : (~ ssList(X1) | (app(nil, sK12(X1, nil)) = X1)), inference(subsumption_resolution, [], [f1581, f454])).
fof(f1581, plain, ! [X1] : ((app(nil, sK12(X1, nil)) = X1) | ~ ssList(nil) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f1578])).
fof(f1578, plain, ! [X1] : ((app(nil, sK12(X1, nil)) = X1) | ~ ssList(nil) | ~ ssList(X1) | ~ ssList(X1)), inference(resolution, [], [f375, f493])).
fof(f493, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f45])).
fof(f45, plain, ! [X0] : (ssList(X0) => frontsegP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax45)).
fof(f1565, plain, ! [X0, X1] : (~ frontsegP(X1, app(X1, X0)) | ~ ssList(X1) | (app(X1, X0) = X1) | ~ ssList(X0)), inference(subsumption_resolution, [], [f1564, f466])).
fof(f1564, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0))), inference(duplicate_literal_removal, [], [f1559])).
fof(f1559, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0)) | ~ ssList(app(X1, X0)) | ~ ssList(X1)), inference(resolution, [], [f585, f487])).
fof(f487, plain, ! [X0, X1] : (~ frontsegP(X1, X0) | (X0 = X1) | ~ frontsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f153])).
fof(f153, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ frontsegP(X1, X0) | ~ frontsegP(X0, X1) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f152])).
fof(f152, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ frontsegP(X1, X0) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((frontsegP(X1, X0) & frontsegP(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax41)).
fof(f302206, plain, spl65_12077, inference(avatar_contradiction_clause, [], [f302205])).
fof(f302205, plain, ($false | spl65_12077), inference(subsumption_resolution, [], [f302204, f576])).
fof(f302204, plain, (~ ssList(sK63) | spl65_12077), inference(resolution, [], [f302161, f493])).
fof(f302161, plain, (~ frontsegP(sK63, nil) | spl65_12077), inference(subsumption_resolution, [], [f302160, f576])).
fof(f302160, plain, (~ frontsegP(sK63, nil) | ~ ssList(sK63) | spl65_12077), inference(subsumption_resolution, [], [f302159, f454])).
fof(f302159, plain, (~ frontsegP(sK63, nil) | ~ ssList(nil) | ~ ssList(sK63) | spl65_12077), inference(resolution, [], [f269779, f374])).
fof(f374, plain, ! [X0, X1] : (ssList(sK12(X0, X1)) | ~ frontsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f269779, plain, (~ ssList(sK12(sK63, nil)) | spl65_12077), inference(avatar_component_clause, [], [f269777])).
fof(f285651, plain, (~ spl65_100 | ~ spl65_111 | ~ spl65_366 | spl65_8679), inference(avatar_contradiction_clause, [], [f285650])).
fof(f285650, plain, ($false | (~ spl65_100 | ~ spl65_111 | ~ spl65_366 | spl65_8679)), inference(subsumption_resolution, [], [f285608, f217388])).
fof(f217388, plain, (~ (sK63 = cons(hd(sK63), nil)) | spl65_8679), inference(avatar_component_clause, [], [f217387])).
fof(f285608, plain, ((sK63 = cons(hd(sK63), nil)) | (~ spl65_100 | ~ spl65_111 | ~ spl65_366)), inference(backward_demodulation, [], [f285398, f285534])).
fof(f285534, plain, ((hd(sK63) = sK51(sK63)) | (~ spl65_100 | ~ spl65_111 | ~ spl65_366)), inference(backward_demodulation, [], [f230424, f285398])).
fof(f230424, plain, ((sK51(sK63) = hd(cons(sK51(sK63), nil))) | ~ spl65_100), inference(resolution, [], [f3154, f1044])).
fof(f1044, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f463, f454])).
fof(f285398, plain, ((sK63 = cons(sK51(sK63), nil)) | (~ spl65_111 | ~ spl65_366)), inference(backward_demodulation, [], [f5335, f3202])).
fof(f3202, plain, ((nil = sK50(sK63)) | ~ spl65_111), inference(avatar_component_clause, [], [f3200])).
fof(f273855, plain, (spl65_4224 | ~ spl65_6925 | ~ spl65_6926 | ~ spl65_8397), inference(avatar_contradiction_clause, [], [f273854])).
fof(f273854, plain, ($false | (spl65_4224 | ~ spl65_6925 | ~ spl65_6926 | ~ spl65_8397)), inference(subsumption_resolution, [], [f273853, f200512])).
fof(f273853, plain, (~ ssList(sK57) | (spl65_4224 | ~ spl65_6925 | ~ spl65_6926)), inference(subsumption_resolution, [], [f273852, f454])).
fof(f273852, plain, (~ ssList(nil) | ~ ssList(sK57) | (spl65_4224 | ~ spl65_6925 | ~ spl65_6926)), inference(subsumption_resolution, [], [f273851, f141922])).
fof(f141922, plain, (frontsegP(sK57, nil) | (~ spl65_6925 | ~ spl65_6926)), inference(subsumption_resolution, [], [f120624, f454])).
fof(f120624, plain, (frontsegP(sK57, nil) | ~ ssList(nil) | (~ spl65_6925 | ~ spl65_6926)), inference(resolution, [], [f120606, f3005])).
fof(f3005, plain, ! [X3] : (~ frontsegP(nil, X3) | frontsegP(sK57, X3) | ~ ssList(X3)), inference(subsumption_resolution, [], [f3004, f454])).
fof(f3004, plain, ! [X3] : (frontsegP(sK57, X3) | ~ frontsegP(nil, X3) | ~ ssList(X3) | ~ ssList(nil)), inference(subsumption_resolution, [], [f2987, f631])).
fof(f2987, plain, ! [X3] : (frontsegP(sK57, X3) | ~ frontsegP(nil, X3) | ~ ssList(sK57) | ~ ssList(X3) | ~ ssList(nil)), inference(superposition, [], [f489, f2228])).
fof(f489, plain, ! [X2, X0, X1] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (frontsegP(X0, X1) => frontsegP(app(X0, X2), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax43)).
fof(f120606, plain, (frontsegP(nil, nil) | (~ spl65_6925 | ~ spl65_6926)), inference(backward_demodulation, [], [f109622, f109616])).
fof(f109616, plain, ((nil = sK13(nil, nil)) | ~ spl65_6925), inference(avatar_component_clause, [], [f109614])).
fof(f109614, plain, (spl65_6925 <=> (nil = sK13(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_6925])])).
fof(f109622, plain, (frontsegP(nil, sK13(nil, nil)) | ~ spl65_6926), inference(avatar_component_clause, [], [f109620])).
fof(f109620, plain, (spl65_6926 <=> frontsegP(nil, sK13(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_6926])])).
fof(f273851, plain, (~ frontsegP(sK57, nil) | ~ ssList(nil) | ~ ssList(sK57) | spl65_4224), inference(resolution, [], [f69720, f374])).
fof(f69720, plain, (~ ssList(sK12(sK57, nil)) | spl65_4224), inference(avatar_component_clause, [], [f69718])).
fof(f69718, plain, (spl65_4224 <=> ssList(sK12(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_4224])])).
fof(f258384, plain, spl65_11734, inference(avatar_split_clause, [], [f574, f258376])).
fof(f574, plain, ssItem(sK61), inference(cnf_transformation, [], [f361])).
fof(f231628, plain, (~ spl65_8769 | spl65_8771 | ~ spl65_1885), inference(avatar_split_clause, [], [f231627, f34276, f224424, f224415])).
fof(f231627, plain, (! [X7] : (memberP(sK57, X7) | ~ memberP(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), X7) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssItem(X7)) | ~ spl65_1885), inference(subsumption_resolution, [], [f228219, f34277])).
fof(f228219, plain, ! [X7] : (memberP(sK57, X7) | ~ memberP(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), X7) | ~ ssList(sK64) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssItem(X7)), inference(superposition, [], [f479, f2213])).
fof(f2213, plain, (sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK61, nil)), sK64)), inference(forward_demodulation, [], [f578, f1028])).
fof(f1028, plain, (sK61 = sK62), inference(subsumption_resolution, [], [f1027, f574])).
fof(f1027, plain, ((sK61 = sK62) | ~ ssItem(sK61)), inference(subsumption_resolution, [], [f1022, f575])).
fof(f575, plain, ssItem(sK62), inference(cnf_transformation, [], [f361])).
fof(f1022, plain, ((sK61 = sK62) | ~ ssItem(sK62) | ~ ssItem(sK61)), inference(resolution, [], [f363, f579])).
fof(f579, plain, ~ neq(sK61, sK62), inference(cnf_transformation, [], [f361])).
fof(f363, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f99])).
fof(f99, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax1)).
fof(f578, plain, (sK57 = app(app(app(sK63, cons(sK61, nil)), cons(sK62, nil)), sK64)), inference(cnf_transformation, [], [f361])).
fof(f479, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X1, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f326])).
fof(f326, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & (memberP(X2, X0) | memberP(X1, X0) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f325])).
fof(f325, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & ((memberP(X2, X0) | memberP(X1, X0)) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax36)).
fof(f231618, plain, (~ spl65_8769 | spl65_8773 | ~ spl65_1885), inference(avatar_split_clause, [], [f231617, f34276, f224440, f224415])).
fof(f231617, plain, (! [X13] : (~ (sK57 = app(X13, sK64)) | (app(app(sK63, cons(sK61, nil)), cons(sK61, nil)) = X13) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(X13)) | ~ spl65_1885), inference(subsumption_resolution, [], [f228224, f34277])).
fof(f228224, plain, ! [X13] : (~ (sK57 = app(X13, sK64)) | (app(app(sK63, cons(sK61, nil)), cons(sK61, nil)) = X13) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(X13)), inference(superposition, [], [f540, f2213])).
fof(f231610, plain, (~ spl65_8769 | spl65_8775 | ~ spl65_1885 | ~ spl65_8397), inference(avatar_split_clause, [], [f231609, f200511, f34276, f224455, f224415])).
fof(f231609, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | (~ spl65_1885 | ~ spl65_8397)), inference(subsumption_resolution, [], [f231608, f200512])).
fof(f231608, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(sK57) | ~ spl65_1885), inference(subsumption_resolution, [], [f228230, f34277])).
fof(f228230, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(sK57)), inference(superposition, [], [f585, f2213])).
fof(f231607, plain, (~ spl65_8769 | spl65_8745 | ~ spl65_1885 | ~ spl65_8397), inference(avatar_split_clause, [], [f231606, f200511, f34276, f224171, f224415])).
fof(f231606, plain, (rearsegP(sK57, sK64) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | (~ spl65_1885 | ~ spl65_8397)), inference(subsumption_resolution, [], [f231605, f200512])).
fof(f231605, plain, (rearsegP(sK57, sK64) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(sK57) | ~ spl65_1885), inference(subsumption_resolution, [], [f228231, f34277])).
fof(f228231, plain, (rearsegP(sK57, sK64) | ~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(sK57)), inference(superposition, [], [f586, f2213])).
fof(f231576, plain, (~ spl65_28 | ~ spl65_8761 | spl65_8769), inference(avatar_contradiction_clause, [], [f231575])).
fof(f231575, plain, ($false | (~ spl65_28 | ~ spl65_8761 | spl65_8769)), inference(subsumption_resolution, [], [f231574, f224357])).
fof(f231574, plain, (~ ssList(app(sK63, cons(sK61, nil))) | (~ spl65_28 | spl65_8769)), inference(subsumption_resolution, [], [f231573, f1934])).
fof(f231573, plain, (~ ssList(cons(sK61, nil)) | ~ ssList(app(sK63, cons(sK61, nil))) | spl65_8769), inference(resolution, [], [f224417, f466])).
fof(f224417, plain, (~ ssList(app(app(sK63, cons(sK61, nil)), cons(sK61, nil))) | spl65_8769), inference(avatar_component_clause, [], [f224415])).
fof(f226191, plain, (~ spl65_28 | spl65_8761), inference(avatar_contradiction_clause, [], [f226190])).
fof(f226190, plain, ($false | (~ spl65_28 | spl65_8761)), inference(subsumption_resolution, [], [f226189, f576])).
fof(f226189, plain, (~ ssList(sK63) | (~ spl65_28 | spl65_8761)), inference(subsumption_resolution, [], [f226188, f1934])).
fof(f226188, plain, (~ ssList(cons(sK61, nil)) | ~ ssList(sK63) | spl65_8761), inference(resolution, [], [f224358, f466])).
fof(f224358, plain, (~ ssList(app(sK63, cons(sK61, nil))) | spl65_8761), inference(avatar_component_clause, [], [f224356])).
fof(f226130, plain, (~ spl65_8059 | ~ spl65_2996 | spl65_7939 | ~ spl65_8064 | ~ spl65_8397), inference(avatar_split_clause, [], [f226129, f200511, f147389, f129248, f57978, f147312])).
fof(f57978, plain, (spl65_2996 <=> ssList(cons(sK61, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_2996])])).
fof(f226129, plain, (~ frontsegP(cons(sK61, sK57), sK57) | (~ spl65_2996 | spl65_7939 | ~ spl65_8064 | ~ spl65_8397)), inference(subsumption_resolution, [], [f226128, f57979])).
fof(f57979, plain, (ssList(cons(sK61, sK57)) | ~ spl65_2996), inference(avatar_component_clause, [], [f57978])).
fof(f226128, plain, (~ frontsegP(cons(sK61, sK57), sK57) | ~ ssList(cons(sK61, sK57)) | (spl65_7939 | ~ spl65_8064 | ~ spl65_8397)), inference(subsumption_resolution, [], [f226069, f200512])).
fof(f226069, plain, (~ frontsegP(cons(sK61, sK57), sK57) | ~ ssList(sK57) | ~ ssList(cons(sK61, sK57)) | (spl65_7939 | ~ spl65_8064)), inference(subsumption_resolution, [], [f226048, f129249])).
fof(f226048, plain, ((sK57 = cons(sK61, sK57)) | ~ frontsegP(cons(sK61, sK57), sK57) | ~ ssList(sK57) | ~ ssList(cons(sK61, sK57)) | ~ spl65_8064), inference(resolution, [], [f147391, f487])).
fof(f147391, plain, (frontsegP(sK57, cons(sK61, sK57)) | ~ spl65_8064), inference(avatar_component_clause, [], [f147389])).
fof(f215465, plain, (spl65_26 | ~ spl65_28 | ~ spl65_1884), inference(avatar_contradiction_clause, [], [f215464])).
fof(f215464, plain, ($false | (spl65_26 | ~ spl65_28 | ~ spl65_1884)), inference(subsumption_resolution, [], [f214586, f34273])).
fof(f34273, plain, (ssList(app(cons(sK61, nil), cons(sK61, nil))) | ~ spl65_1884), inference(avatar_component_clause, [], [f34272])).
fof(f34272, plain, (spl65_1884 <=> ssList(app(cons(sK61, nil), cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl65_1884])])).
fof(f214586, plain, (~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | (spl65_26 | ~ spl65_28)), inference(backward_demodulation, [], [f1472, f170140])).
fof(f170140, plain, ((cons(sK61, nil) = app(nil, cons(sK61, nil))) | ~ spl65_28), inference(resolution, [], [f1934, f468])).
fof(f1472, plain, (~ ssList(app(app(nil, cons(sK61, nil)), cons(sK61, nil))) | spl65_26), inference(avatar_component_clause, [], [f1470])).
fof(f1470, plain, (spl65_26 <=> ssList(app(app(nil, cons(sK61, nil)), cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl65_26])])).
fof(f211193, plain, (~ spl65_5830 | ~ spl65_801 | spl65_5837), inference(avatar_split_clause, [], [f211192, f91033, f13066, f91000])).
fof(f91000, plain, (spl65_5830 <=> (nil = app(sK57, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_5830])])).
fof(f91033, plain, (spl65_5837 <=> (nil = app(sK57, sK58))), introduced(avatar_definition, [new_symbols(naming, [spl65_5837])])).
fof(f211192, plain, (~ (nil = app(sK57, sK57)) | (~ spl65_801 | spl65_5837)), inference(forward_demodulation, [], [f91034, f13068])).
fof(f13068, plain, ((sK57 = sK58) | ~ spl65_801), inference(avatar_component_clause, [], [f13066])).
fof(f91034, plain, (~ (nil = app(sK57, sK58)) | spl65_5837), inference(avatar_component_clause, [], [f91033])).
fof(f202934, plain, spl65_8397, inference(avatar_split_clause, [], [f631, f200511])).
fof(f201865, plain, (spl65_62 | spl65_8462), inference(avatar_split_clause, [], [f10271, f201245, f2259])).
fof(f10271, plain, ((hd(sK57) = hd(app(sK57, sK57))) | (nil = sK57)), inference(resolution, [], [f2234, f631])).
fof(f2234, plain, ! [X5] : (~ ssList(X5) | (hd(X5) = hd(app(X5, sK57))) | (nil = X5)), inference(resolution, [], [f631, f548])).
fof(f548, plain, ! [X0, X1] : (~ ssList(X1) | (nil = X0) | (hd(X0) = hd(app(X0, X1))) | ~ ssList(X0)), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ! [X0] : (! [X1] : ((hd(X0) = hd(app(X0, X1))) | (nil = X0) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f203])).
fof(f203, plain, ! [X0] : (! [X1] : (((hd(X0) = hd(app(X0, X1))) | (nil = X0)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (~ (nil = X0) => (hd(X0) = hd(app(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax85)).
fof(f201862, plain, (spl65_5830 | spl65_8503 | ~ spl65_5829 | ~ spl65_8058), inference(avatar_split_clause, [], [f172899, f146930, f90996, f201807, f91000])).
fof(f90996, plain, (spl65_5829 <=> ssList(app(sK57, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_5829])])).
fof(f146930, plain, (spl65_8058 <=> (sK57 = tl(app(sK57, sK57)))), introduced(avatar_definition, [new_symbols(naming, [spl65_8058])])).
fof(f172899, plain, ((app(sK57, sK57) = cons(hd(app(sK57, sK57)), sK57)) | (nil = app(sK57, sK57)) | (~ spl65_5829 | ~ spl65_8058)), inference(forward_demodulation, [], [f114772, f146932])).
fof(f146932, plain, ((sK57 = tl(app(sK57, sK57))) | ~ spl65_8058), inference(avatar_component_clause, [], [f146930])).
fof(f114772, plain, ((nil = app(sK57, sK57)) | (app(sK57, sK57) = cons(hd(app(sK57, sK57)), tl(app(sK57, sK57)))) | ~ spl65_5829), inference(resolution, [], [f90997, f539])).
fof(f90997, plain, (ssList(app(sK57, sK57)) | ~ spl65_5829), inference(avatar_component_clause, [], [f90996])).
fof(f201083, plain, (~ spl65_62 | ~ spl65_21 | ~ spl65_28 | spl65_382 | ~ spl65_1884 | ~ spl65_1885), inference(avatar_split_clause, [], [f201082, f34276, f34272, f6427, f1933, f1162, f2259])).
fof(f201082, plain, (~ (nil = sK57) | (~ spl65_21 | ~ spl65_28 | spl65_382 | ~ spl65_1884 | ~ spl65_1885)), inference(subsumption_resolution, [], [f201081, f34273])).
fof(f201081, plain, (~ (nil = sK57) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_21 | ~ spl65_28 | spl65_382 | ~ spl65_1885)), inference(subsumption_resolution, [], [f201080, f34277])).
fof(f201080, plain, (~ (nil = sK57) | ~ ssList(sK64) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_21 | ~ spl65_28 | spl65_382)), inference(subsumption_resolution, [], [f104092, f6428])).
fof(f6428, plain, (~ (nil = app(cons(sK61, nil), cons(sK61, nil))) | spl65_382), inference(avatar_component_clause, [], [f6427])).
fof(f104092, plain, (~ (nil = sK57) | (nil = app(cons(sK61, nil), cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_21 | ~ spl65_28)), inference(superposition, [], [f545, f8182])).
fof(f8182, plain, ((sK57 = app(app(cons(sK61, nil), cons(sK61, nil)), sK64)) | (~ spl65_21 | ~ spl65_28)), inference(forward_demodulation, [], [f8181, f2022])).
fof(f2022, plain, ((cons(sK61, nil) = app(nil, cons(sK61, nil))) | ~ spl65_28), inference(resolution, [], [f1934, f468])).
fof(f8181, plain, ((sK57 = app(app(app(nil, cons(sK61, nil)), cons(sK61, nil)), sK64)) | ~ spl65_21), inference(forward_demodulation, [], [f2213, f1164])).
fof(f1164, plain, ((nil = sK63) | ~ spl65_21), inference(avatar_component_clause, [], [f1162])).
fof(f545, plain, ! [X0, X1] : (~ (nil = app(X0, X1)) | (nil = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f343])).
fof(f343, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | ~ (nil = X0) | ~ (nil = X1)) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f342])).
fof(f342, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | (~ (nil = X0) | ~ (nil = X1))) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f201])).
fof(f201, plain, ! [X0] : (! [X1] : (((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax83)).
fof(f200480, plain, (~ spl65_62 | spl65_87), inference(avatar_contradiction_clause, [], [f200479])).
fof(f200479, plain, ($false | (~ spl65_62 | spl65_87)), inference(subsumption_resolution, [], [f200478, f110934])).
fof(f110934, plain, rearsegP(sK64, nil), inference(subsumption_resolution, [], [f110933, f454])).
fof(f110933, plain, (rearsegP(sK64, nil) | ~ ssList(nil)), inference(duplicate_literal_removal, [], [f110930])).
fof(f110930, plain, (rearsegP(sK64, nil) | ~ ssList(nil) | ~ ssList(nil)), inference(resolution, [], [f2839, f498])).
fof(f2839, plain, ! [X4] : (~ rearsegP(nil, X4) | rearsegP(sK64, X4) | ~ ssList(X4)), inference(subsumption_resolution, [], [f2838, f454])).
fof(f2838, plain, ! [X4] : (rearsegP(sK64, X4) | ~ rearsegP(nil, X4) | ~ ssList(X4) | ~ ssList(nil)), inference(subsumption_resolution, [], [f2822, f577])).
fof(f577, plain, ssList(sK64), inference(cnf_transformation, [], [f361])).
fof(f2822, plain, ! [X4] : (rearsegP(sK64, X4) | ~ rearsegP(nil, X4) | ~ ssList(sK64) | ~ ssList(X4) | ~ ssList(nil)), inference(superposition, [], [f499, f888])).
fof(f888, plain, (sK64 = app(sK64, nil)), inference(resolution, [], [f547, f577])).
fof(f547, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax84)).
fof(f499, plain, ! [X2, X0, X1] : (rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : (! [X1] : (! [X2] : (rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f165])).
fof(f165, plain, ! [X0] : (! [X1] : (! [X2] : ((rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f50])).
fof(f50, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (rearsegP(X0, X1) => rearsegP(app(X2, X0), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax50)).
fof(f200478, plain, (~ rearsegP(sK64, nil) | (~ spl65_62 | spl65_87)), inference(forward_demodulation, [], [f2716, f2261])).
fof(f2716, plain, (~ rearsegP(sK64, sK57) | spl65_87), inference(avatar_component_clause, [], [f2714])).
fof(f200477, plain, (~ spl65_23 | ~ spl65_62 | spl65_88), inference(avatar_split_clause, [], [f200476, f2718, f2259, f1171])).
fof(f200476, plain, (~ (nil = sK64) | (~ spl65_62 | spl65_88)), inference(forward_demodulation, [], [f2719, f2261])).
fof(f2719, plain, (~ (sK57 = sK64) | spl65_88), inference(avatar_component_clause, [], [f2718])).
fof(f200469, plain, (~ spl65_29 | ~ spl65_62 | spl65_371), inference(avatar_split_clause, [], [f200468, f6308, f2259, f1937])).
fof(f1937, plain, (spl65_29 <=> (nil = cons(sK61, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_29])])).
fof(f200468, plain, (~ (nil = cons(sK61, nil)) | (~ spl65_62 | spl65_371)), inference(forward_demodulation, [], [f6309, f2261])).
fof(f6309, plain, (~ (sK57 = cons(sK61, nil)) | spl65_371), inference(avatar_component_clause, [], [f6308])).
fof(f200269, plain, (~ spl65_23 | ~ spl65_465 | spl65_6872), inference(avatar_split_clause, [], [f200268, f102273, f8615, f1171])).
fof(f8615, plain, (spl65_465 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl65_465])])).
fof(f200268, plain, (~ (nil = sK64) | (~ spl65_465 | spl65_6872)), inference(forward_demodulation, [], [f102274, f8617])).
fof(f8617, plain, ((nil = sK58) | ~ spl65_465), inference(avatar_component_clause, [], [f8615])).
fof(f180865, plain, (spl65_2995 | ~ spl65_29 | ~ spl65_62), inference(avatar_split_clause, [], [f178013, f2259, f1937, f57824])).
fof(f178013, plain, ((sK61 = hd(nil)) | (~ spl65_29 | ~ spl65_62)), inference(backward_demodulation, [], [f175617, f1939])).
fof(f1939, plain, ((nil = cons(sK61, nil)) | ~ spl65_29), inference(avatar_component_clause, [], [f1937])).
fof(f175617, plain, ((sK61 = hd(cons(sK61, nil))) | ~ spl65_62), inference(backward_demodulation, [], [f5267, f2261])).
fof(f5267, plain, (sK61 = hd(cons(sK61, sK57))), inference(resolution, [], [f574, f2225])).
fof(f2225, plain, ! [X0] : (~ ssItem(X0) | (hd(cons(X0, sK57)) = X0)), inference(resolution, [], [f631, f463])).
fof(f179018, plain, (~ spl65_465 | ~ spl65_62 | spl65_801), inference(avatar_split_clause, [], [f175759, f13066, f2259, f8615])).
fof(f175759, plain, (~ (nil = sK58) | (~ spl65_62 | spl65_801)), inference(backward_demodulation, [], [f13067, f2261])).
fof(f13067, plain, (~ (sK57 = sK58) | spl65_801), inference(avatar_component_clause, [], [f13066])).
fof(f179017, plain, (spl65_465 | ~ spl65_3), inference(avatar_split_clause, [], [f178191, f626, f8615])).
fof(f626, plain, (spl65_3 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl65_3])])).
fof(f178191, plain, ((nil = sK58) | ~ spl65_3), inference(backward_demodulation, [], [f572, f628])).
fof(f628, plain, ((nil = sK60) | ~ spl65_3), inference(avatar_component_clause, [], [f626])).
fof(f572, plain, (sK58 = sK60), inference(cnf_transformation, [], [f361])).
fof(f178254, plain, (~ spl65_62 | ~ spl65_382 | spl65_1577), inference(avatar_contradiction_clause, [], [f178253])).
fof(f178253, plain, ($false | (~ spl65_62 | ~ spl65_382 | spl65_1577)), inference(trivial_inequality_removal, [], [f178252])).
fof(f178252, plain, (~ (hd(nil) = hd(nil)) | (~ spl65_62 | ~ spl65_382 | spl65_1577)), inference(forward_demodulation, [], [f178251, f843])).
fof(f843, plain, (nil = app(nil, nil)), inference(resolution, [], [f468, f454])).
fof(f178251, plain, (~ (hd(nil) = hd(app(nil, nil))) | (~ spl65_62 | ~ spl65_382 | spl65_1577)), inference(forward_demodulation, [], [f175190, f2261])).
fof(f175190, plain, (~ (hd(nil) = hd(app(nil, sK57))) | (~ spl65_382 | spl65_1577)), inference(backward_demodulation, [], [f25758, f6429])).
fof(f6429, plain, ((nil = app(cons(sK61, nil), cons(sK61, nil))) | ~ spl65_382), inference(avatar_component_clause, [], [f6427])).
fof(f25758, plain, (~ (hd(app(cons(sK61, nil), cons(sK61, nil))) = hd(app(app(cons(sK61, nil), cons(sK61, nil)), sK57))) | spl65_1577), inference(avatar_component_clause, [], [f25757])).
fof(f25757, plain, (spl65_1577 <=> (hd(app(cons(sK61, nil), cons(sK61, nil))) = hd(app(app(cons(sK61, nil), cons(sK61, nil)), sK57)))), introduced(avatar_definition, [new_symbols(naming, [spl65_1577])])).
fof(f175395, plain, (spl65_7832 | ~ spl65_21 | ~ spl65_371), inference(avatar_split_clause, [], [f175206, f6308, f1162, f127702])).
fof(f175206, plain, ((sK61 = hd(sK57)) | (~ spl65_21 | ~ spl65_371)), inference(backward_demodulation, [], [f6242, f6310])).
fof(f6242, plain, ((sK61 = hd(cons(sK61, nil))) | ~ spl65_21), inference(forward_demodulation, [], [f5261, f1164])).
fof(f5261, plain, (sK61 = hd(cons(sK61, sK63))), inference(resolution, [], [f574, f1070])).
fof(f1070, plain, ! [X59] : (~ ssItem(X59) | (hd(cons(X59, sK63)) = X59)), inference(resolution, [], [f463, f576])).
fof(f175394, plain, (~ spl65_371 | spl65_2997 | ~ spl65_5830), inference(avatar_contradiction_clause, [], [f175393])).
fof(f175393, plain, ($false | (~ spl65_371 | spl65_2997 | ~ spl65_5830)), inference(subsumption_resolution, [], [f175392, f57983])).
fof(f57983, plain, (~ (nil = cons(sK61, sK57)) | spl65_2997), inference(avatar_component_clause, [], [f57982])).
fof(f57982, plain, (spl65_2997 <=> (nil = cons(sK61, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_2997])])).
fof(f175392, plain, ((nil = cons(sK61, sK57)) | (~ spl65_371 | ~ spl65_5830)), inference(forward_demodulation, [], [f175205, f91002])).
fof(f91002, plain, ((nil = app(sK57, sK57)) | ~ spl65_5830), inference(avatar_component_clause, [], [f91000])).
fof(f175205, plain, ((cons(sK61, sK57) = app(sK57, sK57)) | ~ spl65_371), inference(backward_demodulation, [], [f6116, f6310])).
fof(f6116, plain, (cons(sK61, sK57) = app(cons(sK61, nil), sK57)), inference(resolution, [], [f2232, f574])).
fof(f2232, plain, ! [X4] : (~ ssItem(X4) | (cons(X4, sK57) = app(cons(X4, nil), sK57))), inference(resolution, [], [f631, f542])).
fof(f542, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax81)).
fof(f172906, plain, (~ spl65_5853 | ~ spl65_23 | spl65_1947), inference(avatar_split_clause, [], [f139372, f39895, f1171, f91395])).
fof(f91395, plain, (spl65_5853 <=> (nil = sK13(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_5853])])).
fof(f139372, plain, (~ (nil = sK13(sK57, nil)) | (~ spl65_23 | spl65_1947)), inference(backward_demodulation, [], [f39896, f1173])).
fof(f1173, plain, ((nil = sK64) | ~ spl65_23), inference(avatar_component_clause, [], [f1171])).
fof(f39896, plain, (~ (nil = sK13(sK57, sK64)) | spl65_1947), inference(avatar_component_clause, [], [f39895])).
fof(f170658, plain, (~ spl65_28 | spl65_1884), inference(avatar_contradiction_clause, [], [f170657])).
fof(f170657, plain, ($false | (~ spl65_28 | spl65_1884)), inference(subsumption_resolution, [], [f170656, f1934])).
fof(f170656, plain, (~ ssList(cons(sK61, nil)) | spl65_1884), inference(duplicate_literal_removal, [], [f170655])).
fof(f170655, plain, (~ ssList(cons(sK61, nil)) | ~ ssList(cons(sK61, nil)) | spl65_1884), inference(resolution, [], [f34274, f466])).
fof(f34274, plain, (~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | spl65_1884), inference(avatar_component_clause, [], [f34272])).
fof(f154878, plain, (~ spl65_778 | ~ spl65_62 | ~ spl65_1), inference(avatar_split_clause, [], [f146954, f617, f2259, f12696])).
fof(f12696, plain, (spl65_778 <=> ssItem(sK54(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_778])])).
fof(f617, plain, (spl65_1 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl65_1])])).
fof(f146954, plain, (~ (nil = sK57) | ~ ssItem(sK54(sK58, sK57)) | ~ spl65_1), inference(subsumption_resolution, [], [f9416, f454])).
fof(f9416, plain, (~ (nil = sK57) | ~ ssItem(sK54(sK58, sK57)) | ~ ssList(nil) | ~ spl65_1), inference(superposition, [], [f461, f8946])).
fof(f8946, plain, ((sK57 = cons(sK54(sK58, sK57), nil)) | ~ spl65_1), inference(resolution, [], [f8521, f564])).
fof(f564, plain, ! [X0, X1] : (~ sP6(X0, X1) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f347, f350, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f233])).
fof(f8521, plain, (sP6(sK58, sK57) | ~ spl65_1), inference(backward_demodulation, [], [f2217, f572])).
fof(f2217, plain, (sP6(sK60, sK57) | ~ spl65_1), inference(forward_demodulation, [], [f619, f573])).
fof(f619, plain, (sP6(sK60, sK59) | ~ spl65_1), inference(avatar_component_clause, [], [f617])).
fof(f461, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax21)).
fof(f154642, plain, (spl65_8058 | ~ spl65_1 | ~ spl65_778), inference(avatar_split_clause, [], [f154641, f12696, f617, f146930])).
fof(f154641, plain, ((sK57 = tl(app(sK57, sK57))) | (~ spl65_1 | ~ spl65_778)), inference(forward_demodulation, [], [f14319, f14322])).
fof(f14322, plain, ((cons(hd(sK57), sK57) = app(sK57, sK57)) | (~ spl65_1 | ~ spl65_778)), inference(forward_demodulation, [], [f14321, f14252])).
fof(f14252, plain, ((sK57 = cons(hd(sK57), nil)) | (~ spl65_1 | ~ spl65_778)), inference(backward_demodulation, [], [f8946, f14251])).
fof(f14251, plain, ((hd(sK57) = sK54(sK58, sK57)) | (~ spl65_1 | ~ spl65_778)), inference(forward_demodulation, [], [f14224, f8946])).
fof(f14224, plain, ((sK54(sK58, sK57) = hd(cons(sK54(sK58, sK57), nil))) | ~ spl65_778), inference(resolution, [], [f12697, f1044])).
fof(f12697, plain, (ssItem(sK54(sK58, sK57)) | ~ spl65_778), inference(avatar_component_clause, [], [f12696])).
fof(f14321, plain, ((cons(hd(sK57), sK57) = app(cons(hd(sK57), nil), sK57)) | (~ spl65_1 | ~ spl65_778)), inference(forward_demodulation, [], [f14247, f14251])).
fof(f14247, plain, ((cons(sK54(sK58, sK57), sK57) = app(cons(sK54(sK58, sK57), nil), sK57)) | ~ spl65_778), inference(resolution, [], [f12697, f2232])).
fof(f14319, plain, ((sK57 = tl(cons(hd(sK57), sK57))) | (~ spl65_1 | ~ spl65_778)), inference(forward_demodulation, [], [f14246, f14251])).
fof(f14246, plain, ((sK57 = tl(cons(sK54(sK58, sK57), sK57))) | ~ spl65_778), inference(resolution, [], [f12697, f2226])).
fof(f2226, plain, ! [X1] : (~ ssItem(X1) | (sK57 = tl(cons(X1, sK57)))), inference(resolution, [], [f631, f465])).
fof(f154639, plain, (spl65_276 | ~ spl65_1 | ~ spl65_778), inference(avatar_split_clause, [], [f14253, f12696, f617, f4132])).
fof(f4132, plain, (spl65_276 <=> ssItem(hd(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_276])])).
fof(f14253, plain, (ssItem(hd(sK57)) | (~ spl65_1 | ~ spl65_778)), inference(backward_demodulation, [], [f12697, f14251])).
fof(f153812, plain, (spl65_62 | ~ spl65_2), inference(avatar_split_clause, [], [f153811, f621, f2259])).
fof(f621, plain, (spl65_2 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl65_2])])).
fof(f153811, plain, ((nil = sK57) | ~ spl65_2), inference(forward_demodulation, [], [f573, f623])).
fof(f623, plain, ((nil = sK59) | ~ spl65_2), inference(avatar_component_clause, [], [f621])).
fof(f153624, plain, (~ spl65_62 | spl65_5853 | ~ spl65_6925), inference(avatar_contradiction_clause, [], [f153623])).
fof(f153623, plain, ($false | (~ spl65_62 | spl65_5853 | ~ spl65_6925)), inference(subsumption_resolution, [], [f152629, f109616])).
fof(f152629, plain, (~ (nil = sK13(nil, nil)) | (~ spl65_62 | spl65_5853)), inference(backward_demodulation, [], [f91396, f2261])).
fof(f91396, plain, (~ (nil = sK13(sK57, nil)) | spl65_5853), inference(avatar_component_clause, [], [f91395])).
fof(f153620, plain, (~ spl65_62 | spl65_5830), inference(avatar_contradiction_clause, [], [f153619])).
fof(f153619, plain, ($false | (~ spl65_62 | spl65_5830)), inference(subsumption_resolution, [], [f152627, f843])).
fof(f152627, plain, (~ (nil = app(nil, nil)) | (~ spl65_62 | spl65_5830)), inference(backward_demodulation, [], [f91001, f2261])).
fof(f91001, plain, (~ (nil = app(sK57, sK57)) | spl65_5830), inference(avatar_component_clause, [], [f91000])).
fof(f150987, plain, (spl65_8059 | ~ spl65_28 | ~ spl65_371 | ~ spl65_2996), inference(avatar_split_clause, [], [f136343, f57978, f6308, f1933, f147312])).
fof(f136343, plain, (frontsegP(cons(sK61, sK57), sK57) | (~ spl65_28 | ~ spl65_371 | ~ spl65_2996)), inference(backward_demodulation, [], [f129228, f6310])).
fof(f129228, plain, (frontsegP(cons(sK61, sK57), cons(sK61, nil)) | (~ spl65_28 | ~ spl65_2996)), inference(subsumption_resolution, [], [f129227, f57979])).
fof(f129227, plain, (frontsegP(cons(sK61, sK57), cons(sK61, nil)) | ~ ssList(cons(sK61, sK57)) | ~ spl65_28), inference(subsumption_resolution, [], [f129226, f1934])).
fof(f129226, plain, (frontsegP(cons(sK61, sK57), cons(sK61, nil)) | ~ ssList(cons(sK61, nil)) | ~ ssList(cons(sK61, sK57))), inference(subsumption_resolution, [], [f129184, f631])).
fof(f129184, plain, (frontsegP(cons(sK61, sK57), cons(sK61, nil)) | ~ ssList(sK57) | ~ ssList(cons(sK61, nil)) | ~ ssList(cons(sK61, sK57))), inference(superposition, [], [f585, f6116])).
fof(f145993, plain, (spl65_62 | ~ spl65_287 | ~ spl65_7939), inference(avatar_contradiction_clause, [], [f145992])).
fof(f145992, plain, ($false | (spl65_62 | ~ spl65_287 | ~ spl65_7939)), inference(subsumption_resolution, [], [f145991, f2260])).
fof(f2260, plain, (~ (nil = sK57) | spl65_62), inference(avatar_component_clause, [], [f2259])).
fof(f145991, plain, ((nil = sK57) | (~ spl65_287 | ~ spl65_7939)), inference(forward_demodulation, [], [f145784, f4181])).
fof(f145784, plain, ((sK57 = tl(sK57)) | ~ spl65_7939), inference(backward_demodulation, [], [f5268, f129250])).
fof(f129250, plain, ((sK57 = cons(sK61, sK57)) | ~ spl65_7939), inference(avatar_component_clause, [], [f129248])).
fof(f5268, plain, (sK57 = tl(cons(sK61, sK57))), inference(resolution, [], [f574, f2226])).
fof(f140898, plain, (~ spl65_23 | spl65_62 | ~ spl65_3823), inference(avatar_contradiction_clause, [], [f140897])).
fof(f140897, plain, ($false | (~ spl65_23 | spl65_62 | ~ spl65_3823)), inference(subsumption_resolution, [], [f140896, f2260])).
fof(f140896, plain, ((nil = sK57) | (~ spl65_23 | ~ spl65_3823)), inference(backward_demodulation, [], [f2233, f139819])).
fof(f139819, plain, ((nil = app(sK57, nil)) | (~ spl65_23 | ~ spl65_3823)), inference(backward_demodulation, [], [f66120, f1173])).
fof(f66120, plain, ((nil = app(sK57, sK64)) | ~ spl65_3823), inference(avatar_component_clause, [], [f66118])).
fof(f66118, plain, (spl65_3823 <=> (nil = app(sK57, sK64))), introduced(avatar_definition, [new_symbols(naming, [spl65_3823])])).
fof(f2233, plain, (sK57 = app(sK57, nil)), inference(resolution, [], [f631, f547])).
fof(f136010, plain, (spl65_62 | ~ spl65_276 | ~ spl65_3000 | ~ spl65_7832), inference(avatar_contradiction_clause, [], [f136009])).
fof(f136009, plain, ($false | (spl65_62 | ~ spl65_276 | ~ spl65_3000 | ~ spl65_7832)), inference(subsumption_resolution, [], [f135475, f135374])).
fof(f135374, plain, (lt(sK61, sK61) | (spl65_62 | ~ spl65_3000 | ~ spl65_7832)), inference(backward_demodulation, [], [f59294, f127704])).
fof(f59294, plain, (lt(sK61, hd(sK57)) | (spl65_62 | ~ spl65_3000)), inference(subsumption_resolution, [], [f59293, f574])).
fof(f59293, plain, (lt(sK61, hd(sK57)) | ~ ssItem(sK61) | (spl65_62 | ~ spl65_3000)), inference(subsumption_resolution, [], [f59292, f631])).
fof(f59292, plain, (lt(sK61, hd(sK57)) | ~ ssList(sK57) | ~ ssItem(sK61) | (spl65_62 | ~ spl65_3000)), inference(subsumption_resolution, [], [f59290, f2260])).
fof(f59290, plain, ((nil = sK57) | lt(sK61, hd(sK57)) | ~ ssList(sK57) | ~ ssItem(sK61) | ~ spl65_3000), inference(resolution, [], [f57996, f527])).
fof(f527, plain, ! [X0, X1] : (~ strictorderedP(cons(X0, X1)) | (nil = X1) | lt(X0, hd(X1)) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f337])).
fof(f337, plain, ! [X0] : (! [X1] : (((strictorderedP(cons(X0, X1)) | ((~ lt(X0, hd(X1)) | ~ strictorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & ((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1) | ~ strictorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f336])).
fof(f336, plain, ! [X0] : (! [X1] : (((strictorderedP(cons(X0, X1)) | ((~ lt(X0, hd(X1)) | ~ strictorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & (((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1)) | ~ strictorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f184])).
fof(f184, plain, ! [X0] : (! [X1] : ((strictorderedP(cons(X0, X1)) <=> ((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f70])).
fof(f70, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => (strictorderedP(cons(X0, X1)) <=> ((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax70)).
fof(f57996, plain, (strictorderedP(cons(sK61, sK57)) | ~ spl65_3000), inference(avatar_component_clause, [], [f57995])).
fof(f57995, plain, (spl65_3000 <=> strictorderedP(cons(sK61, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_3000])])).
fof(f135475, plain, (~ lt(sK61, sK61) | (spl65_62 | ~ spl65_276 | ~ spl65_3000 | ~ spl65_7832)), inference(backward_demodulation, [], [f127741, f127704])).
fof(f127741, plain, (~ lt(hd(sK57), sK61) | (spl65_62 | ~ spl65_276 | ~ spl65_3000)), inference(subsumption_resolution, [], [f127740, f4133])).
fof(f4133, plain, (ssItem(hd(sK57)) | ~ spl65_276), inference(avatar_component_clause, [], [f4132])).
fof(f127740, plain, (~ lt(hd(sK57), sK61) | ~ ssItem(hd(sK57)) | (spl65_62 | ~ spl65_3000)), inference(subsumption_resolution, [], [f127733, f574])).
fof(f127733, plain, (~ lt(hd(sK57), sK61) | ~ ssItem(sK61) | ~ ssItem(hd(sK57)) | (spl65_62 | ~ spl65_3000)), inference(resolution, [], [f59294, f474])).
fof(f474, plain, ! [X0, X1] : (~ lt(X1, X0) | ~ lt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f143])).
fof(f143, plain, ! [X0] : (! [X1] : (~ lt(X1, X0) | ~ lt(X0, X1) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f142])).
fof(f142, plain, ! [X0] : (! [X1] : ((~ lt(X1, X0) | ~ lt(X0, X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f33])).
fof(f33, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (lt(X0, X1) => ~ lt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax33)).
fof(f135555, plain, (spl65_371 | ~ spl65_1 | ~ spl65_778 | ~ spl65_7832), inference(avatar_split_clause, [], [f135299, f127702, f12696, f617, f6308])).
fof(f135299, plain, ((sK57 = cons(sK61, nil)) | (~ spl65_1 | ~ spl65_778 | ~ spl65_7832)), inference(backward_demodulation, [], [f14252, f127704])).
fof(f129664, plain, (spl65_7957 | ~ spl65_1577 | ~ spl65_6301), inference(avatar_split_clause, [], [f129663, f94594, f25757, f129586])).
fof(f129663, plain, ((hd(sK13(sK57, sK64)) = hd(app(sK13(sK57, sK64), sK57))) | (~ spl65_1577 | ~ spl65_6301)), inference(forward_demodulation, [], [f25759, f94596])).
fof(f94596, plain, ((app(cons(sK61, nil), cons(sK61, nil)) = sK13(sK57, sK64)) | ~ spl65_6301), inference(avatar_component_clause, [], [f94594])).
fof(f25759, plain, ((hd(app(cons(sK61, nil), cons(sK61, nil))) = hd(app(app(cons(sK61, nil), cons(sK61, nil)), sK57))) | ~ spl65_1577), inference(avatar_component_clause, [], [f25757])).
fof(f129458, plain, (~ spl65_28 | spl65_29 | ~ spl65_382), inference(avatar_contradiction_clause, [], [f129457])).
fof(f129457, plain, ($false | (~ spl65_28 | spl65_29 | ~ spl65_382)), inference(subsumption_resolution, [], [f129456, f1934])).
fof(f129456, plain, (~ ssList(cons(sK61, nil)) | (spl65_29 | ~ spl65_382)), inference(subsumption_resolution, [], [f129406, f1938])).
fof(f1938, plain, (~ (nil = cons(sK61, nil)) | spl65_29), inference(avatar_component_clause, [], [f1937])).
fof(f129406, plain, ((nil = cons(sK61, nil)) | ~ ssList(cons(sK61, nil)) | ~ spl65_382), inference(trivial_inequality_removal, [], [f129405])).
fof(f129405, plain, (~ (nil = nil) | (nil = cons(sK61, nil)) | ~ ssList(cons(sK61, nil)) | ~ spl65_382), inference(duplicate_literal_removal, [], [f129354])).
fof(f129354, plain, (~ (nil = nil) | (nil = cons(sK61, nil)) | ~ ssList(cons(sK61, nil)) | ~ ssList(cons(sK61, nil)) | ~ spl65_382), inference(superposition, [], [f545, f6429])).
fof(f120605, plain, (~ spl65_6909 | ~ spl65_6910 | spl65_6921), inference(avatar_contradiction_clause, [], [f120604])).
fof(f120604, plain, ($false | (~ spl65_6909 | ~ spl65_6910 | spl65_6921)), inference(subsumption_resolution, [], [f120603, f454])).
fof(f120603, plain, (~ ssList(nil) | (~ spl65_6909 | ~ spl65_6910 | spl65_6921)), inference(subsumption_resolution, [], [f120602, f120493])).
fof(f120493, plain, (rearsegP(nil, nil) | (~ spl65_6909 | ~ spl65_6910)), inference(backward_demodulation, [], [f109111, f109105])).
fof(f109105, plain, ((nil = sK12(nil, nil)) | ~ spl65_6909), inference(avatar_component_clause, [], [f109103])).
fof(f109103, plain, (spl65_6909 <=> (nil = sK12(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_6909])])).
fof(f109111, plain, (rearsegP(nil, sK12(nil, nil)) | ~ spl65_6910), inference(avatar_component_clause, [], [f109109])).
fof(f109109, plain, (spl65_6910 <=> rearsegP(nil, sK12(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_6910])])).
fof(f120602, plain, (~ rearsegP(nil, nil) | ~ ssList(nil) | spl65_6921), inference(duplicate_literal_removal, [], [f120601])).
fof(f120601, plain, (~ rearsegP(nil, nil) | ~ ssList(nil) | ~ ssList(nil) | spl65_6921), inference(resolution, [], [f109595, f377])).
fof(f377, plain, ! [X0, X1] : (ssList(sK13(X0, X1)) | ~ rearsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f109595, plain, (~ ssList(sK13(nil, nil)) | spl65_6921), inference(avatar_component_clause, [], [f109593])).
fof(f109593, plain, (spl65_6921 <=> ssList(sK13(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_6921])])).
fof(f120491, plain, spl65_6904, inference(avatar_contradiction_clause, [], [f120490])).
fof(f120490, plain, ($false | spl65_6904), inference(subsumption_resolution, [], [f120487, f454])).
fof(f120487, plain, (~ ssList(nil) | spl65_6904), inference(resolution, [], [f120481, f488])).
fof(f488, plain, ! [X0] : (frontsegP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f154])).
fof(f154, plain, ! [X0] : (frontsegP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f42])).
fof(f42, plain, ! [X0] : (ssList(X0) => frontsegP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax42)).
fof(f120481, plain, (~ frontsegP(nil, nil) | spl65_6904), inference(subsumption_resolution, [], [f120480, f454])).
fof(f120480, plain, (~ frontsegP(nil, nil) | ~ ssList(nil) | spl65_6904), inference(duplicate_literal_removal, [], [f120479])).
fof(f120479, plain, (~ frontsegP(nil, nil) | ~ ssList(nil) | ~ ssList(nil) | spl65_6904), inference(resolution, [], [f109079, f374])).
fof(f109079, plain, (~ ssList(sK12(nil, nil)) | spl65_6904), inference(avatar_component_clause, [], [f109077])).
fof(f109077, plain, (spl65_6904 <=> ssList(sK12(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl65_6904])])).
fof(f117230, plain, (~ spl65_21 | ~ spl65_26 | ~ spl65_28 | ~ spl65_1885 | spl65_6300), inference(avatar_contradiction_clause, [], [f117229])).
fof(f117229, plain, ($false | (~ spl65_21 | ~ spl65_26 | ~ spl65_28 | ~ spl65_1885 | spl65_6300)), inference(subsumption_resolution, [], [f117228, f631])).
fof(f117228, plain, (~ ssList(sK57) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28 | ~ spl65_1885 | spl65_6300)), inference(subsumption_resolution, [], [f117227, f34277])).
fof(f117227, plain, (~ ssList(sK64) | ~ ssList(sK57) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28 | spl65_6300)), inference(subsumption_resolution, [], [f117226, f14812])).
fof(f14812, plain, (rearsegP(sK57, sK64) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(subsumption_resolution, [], [f14811, f631])).
fof(f14811, plain, (rearsegP(sK57, sK64) | ~ ssList(sK57) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(subsumption_resolution, [], [f14810, f577])).
fof(f14810, plain, (rearsegP(sK57, sK64) | ~ ssList(sK64) | ~ ssList(sK57) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(subsumption_resolution, [], [f14779, f6247])).
fof(f6247, plain, (ssList(app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_26 | ~ spl65_28)), inference(backward_demodulation, [], [f1471, f2022])).
fof(f1471, plain, (ssList(app(app(nil, cons(sK61, nil)), cons(sK61, nil))) | ~ spl65_26), inference(avatar_component_clause, [], [f1470])).
fof(f14779, plain, (rearsegP(sK57, sK64) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(sK57) | (~ spl65_21 | ~ spl65_28)), inference(superposition, [], [f586, f8182])).
fof(f117226, plain, (~ rearsegP(sK57, sK64) | ~ ssList(sK64) | ~ ssList(sK57) | spl65_6300), inference(resolution, [], [f94592, f377])).
fof(f94592, plain, (~ ssList(sK13(sK57, sK64)) | spl65_6300), inference(avatar_component_clause, [], [f94590])).
fof(f115198, plain, (spl65_388 | ~ spl65_389), inference(avatar_contradiction_clause, [], [f115197])).
fof(f115197, plain, ($false | (spl65_388 | ~ spl65_389)), inference(subsumption_resolution, [], [f115196, f6624])).
fof(f6624, plain, (~ segmentP(nil, sK57) | spl65_388), inference(avatar_component_clause, [], [f6623])).
fof(f6623, plain, (spl65_388 <=> segmentP(nil, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl65_388])])).
fof(f115196, plain, (segmentP(nil, sK57) | ~ spl65_389), inference(subsumption_resolution, [], [f115193, f631])).
fof(f115193, plain, (~ ssList(sK57) | segmentP(nil, sK57) | ~ spl65_389), inference(duplicate_literal_removal, [], [f115191])).
fof(f115191, plain, (~ ssList(sK57) | segmentP(nil, sK57) | ~ ssList(sK57) | ~ spl65_389), inference(resolution, [], [f6630, f505])).
fof(f505, plain, ! [X0] : (segmentP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ! [X0] : (segmentP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0] : (ssList(X0) => segmentP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax55)).
fof(f6630, plain, (! [X0] : (~ segmentP(sK57, X0) | ~ ssList(X0) | segmentP(nil, X0)) | ~ spl65_389), inference(avatar_component_clause, [], [f6629])).
fof(f6629, plain, (spl65_389 <=> ! [X0] : (segmentP(nil, X0) | ~ ssList(X0) | ~ segmentP(sK57, X0))), introduced(avatar_definition, [new_symbols(naming, [spl65_389])])).
fof(f113255, plain, spl65_5829, inference(avatar_contradiction_clause, [], [f113254])).
fof(f113254, plain, ($false | spl65_5829), inference(subsumption_resolution, [], [f113253, f631])).
fof(f113253, plain, (~ ssList(sK57) | spl65_5829), inference(duplicate_literal_removal, [], [f113252])).
fof(f113252, plain, (~ ssList(sK57) | ~ ssList(sK57) | spl65_5829), inference(resolution, [], [f90998, f466])).
fof(f90998, plain, (~ ssList(app(sK57, sK57)) | spl65_5829), inference(avatar_component_clause, [], [f90996])).
fof(f109626, plain, (spl65_6925 | ~ spl65_6921), inference(avatar_split_clause, [], [f109625, f109593, f109614])).
fof(f109625, plain, (~ ssList(sK13(nil, nil)) | (nil = sK13(nil, nil))), inference(subsumption_resolution, [], [f109624, f493])).
fof(f109624, plain, (~ frontsegP(sK13(nil, nil), nil) | ~ ssList(sK13(nil, nil)) | (nil = sK13(nil, nil))), inference(subsumption_resolution, [], [f109569, f454])).
fof(f109569, plain, (~ frontsegP(sK13(nil, nil), nil) | ~ ssList(sK13(nil, nil)) | (nil = sK13(nil, nil)) | ~ ssList(nil)), inference(superposition, [], [f1565, f5068])).
fof(f5068, plain, (nil = app(sK13(nil, nil), nil)), inference(resolution, [], [f1592, f454])).
fof(f109623, plain, (~ spl65_6921 | spl65_6926), inference(avatar_split_clause, [], [f109618, f109620, f109593])).
fof(f109618, plain, (frontsegP(nil, sK13(nil, nil)) | ~ ssList(sK13(nil, nil))), inference(subsumption_resolution, [], [f109588, f454])).
fof(f109588, plain, (frontsegP(nil, sK13(nil, nil)) | ~ ssList(nil) | ~ ssList(sK13(nil, nil))), inference(duplicate_literal_removal, [], [f109562])).
fof(f109562, plain, (frontsegP(nil, sK13(nil, nil)) | ~ ssList(nil) | ~ ssList(sK13(nil, nil)) | ~ ssList(nil)), inference(superposition, [], [f585, f5068])).
fof(f109120, plain, (spl65_6909 | ~ spl65_6904), inference(avatar_split_clause, [], [f109119, f109077, f109103])).
fof(f109119, plain, (~ ssList(sK12(nil, nil)) | (nil = sK12(nil, nil))), inference(subsumption_resolution, [], [f109118, f500])).
fof(f500, plain, ! [X0] : (rearsegP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ! [X0] : (rearsegP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (ssList(X0) => rearsegP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax51)).
fof(f109118, plain, (~ rearsegP(sK12(nil, nil), nil) | ~ ssList(sK12(nil, nil)) | (nil = sK12(nil, nil))), inference(subsumption_resolution, [], [f109054, f454])).
fof(f109054, plain, (~ rearsegP(sK12(nil, nil), nil) | ~ ssList(sK12(nil, nil)) | (nil = sK12(nil, nil)) | ~ ssList(nil)), inference(superposition, [], [f1574, f4977])).
fof(f4977, plain, (nil = app(nil, sK12(nil, nil))), inference(resolution, [], [f1582, f454])).
fof(f1582, plain, ! [X0] : (~ ssList(X0) | (app(X0, sK12(X0, X0)) = X0)), inference(duplicate_literal_removal, [], [f1577])).
fof(f1577, plain, ! [X0] : ((app(X0, sK12(X0, X0)) = X0) | ~ ssList(X0) | ~ ssList(X0) | ~ ssList(X0)), inference(resolution, [], [f375, f488])).
fof(f109112, plain, (~ spl65_6904 | spl65_6910), inference(avatar_split_clause, [], [f109107, f109109, f109077])).
fof(f109107, plain, (rearsegP(nil, sK12(nil, nil)) | ~ ssList(sK12(nil, nil))), inference(subsumption_resolution, [], [f109071, f454])).
fof(f109071, plain, (rearsegP(nil, sK12(nil, nil)) | ~ ssList(nil) | ~ ssList(sK12(nil, nil))), inference(duplicate_literal_removal, [], [f109047])).
fof(f109047, plain, (rearsegP(nil, sK12(nil, nil)) | ~ ssList(nil) | ~ ssList(sK12(nil, nil)) | ~ ssList(nil)), inference(superposition, [], [f586, f4977])).
fof(f103824, plain, (~ spl65_5837 | spl65_3823 | ~ spl65_6872), inference(avatar_split_clause, [], [f103372, f102273, f66118, f91033])).
fof(f103372, plain, (~ (nil = app(sK57, sK58)) | (spl65_3823 | ~ spl65_6872)), inference(backward_demodulation, [], [f66119, f102275])).
fof(f66119, plain, (~ (nil = app(sK57, sK64)) | spl65_3823), inference(avatar_component_clause, [], [f66118])).
fof(f88056, plain, (~ spl65_4231 | ~ spl65_4233 | spl65_4485), inference(avatar_contradiction_clause, [], [f88055])).
fof(f88055, plain, ($false | (~ spl65_4231 | ~ spl65_4233 | spl65_4485)), inference(subsumption_resolution, [], [f88054, f631])).
fof(f88054, plain, (~ ssList(sK57) | (~ spl65_4231 | ~ spl65_4233 | spl65_4485)), inference(subsumption_resolution, [], [f88053, f75216])).
fof(f75216, plain, (rearsegP(sK57, sK57) | (~ spl65_4231 | ~ spl65_4233)), inference(forward_demodulation, [], [f69759, f69770])).
fof(f88053, plain, (~ rearsegP(sK57, sK57) | ~ ssList(sK57) | spl65_4485), inference(duplicate_literal_removal, [], [f88052])).
fof(f88052, plain, (~ rearsegP(sK57, sK57) | ~ ssList(sK57) | ~ ssList(sK57) | spl65_4485), inference(resolution, [], [f71416, f377])).
fof(f71416, plain, (~ ssList(sK13(sK57, sK57)) | spl65_4485), inference(avatar_component_clause, [], [f71414])).
fof(f74042, plain, (spl65_4233 | ~ spl65_4224), inference(avatar_split_clause, [], [f74041, f69718, f69768])).
fof(f74041, plain, ((sK57 = sK12(sK57, nil)) | ~ spl65_4224), inference(forward_demodulation, [], [f73711, f5059])).
fof(f5059, plain, (sK57 = app(nil, sK12(sK57, nil))), inference(resolution, [], [f1583, f631])).
fof(f73711, plain, ((sK12(sK57, nil) = app(nil, sK12(sK57, nil))) | ~ spl65_4224), inference(resolution, [], [f69719, f468])).
fof(f69719, plain, (ssList(sK12(sK57, nil)) | ~ spl65_4224), inference(avatar_component_clause, [], [f69718])).
fof(f69760, plain, (~ spl65_4224 | spl65_4231), inference(avatar_split_clause, [], [f69755, f69757, f69718])).
fof(f69755, plain, (rearsegP(sK57, sK12(sK57, nil)) | ~ ssList(sK12(sK57, nil))), inference(subsumption_resolution, [], [f69754, f631])).
fof(f69754, plain, (rearsegP(sK57, sK12(sK57, nil)) | ~ ssList(sK12(sK57, nil)) | ~ ssList(sK57)), inference(subsumption_resolution, [], [f69691, f454])).
fof(f69691, plain, (rearsegP(sK57, sK12(sK57, nil)) | ~ ssList(nil) | ~ ssList(sK12(sK57, nil)) | ~ ssList(sK57)), inference(superposition, [], [f586, f5059])).
fof(f58044, plain, (~ spl65_2997 | spl65_3000), inference(avatar_contradiction_clause, [], [f58043])).
fof(f58043, plain, ($false | (~ spl65_2997 | spl65_3000)), inference(subsumption_resolution, [], [f58036, f524])).
fof(f524, plain, strictorderedP(nil), inference(cnf_transformation, [], [f69])).
fof(f69, plain, strictorderedP(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax69)).
fof(f58036, plain, (~ strictorderedP(nil) | (~ spl65_2997 | spl65_3000)), inference(backward_demodulation, [], [f57997, f57984])).
fof(f57984, plain, ((nil = cons(sK61, sK57)) | ~ spl65_2997), inference(avatar_component_clause, [], [f57982])).
fof(f57997, plain, (~ strictorderedP(cons(sK61, sK57)) | spl65_3000), inference(avatar_component_clause, [], [f57995])).
fof(f58009, plain, spl65_2996, inference(avatar_contradiction_clause, [], [f58008])).
fof(f58008, plain, ($false | spl65_2996), inference(subsumption_resolution, [], [f58007, f631])).
fof(f58007, plain, (~ ssList(sK57) | spl65_2996), inference(subsumption_resolution, [], [f58006, f574])).
fof(f58006, plain, (~ ssItem(sK61) | ~ ssList(sK57) | spl65_2996), inference(resolution, [], [f57980, f453])).
fof(f453, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax16)).
fof(f57980, plain, (~ ssList(cons(sK61, sK57)) | spl65_2996), inference(avatar_component_clause, [], [f57978])).
fof(f34402, plain, spl65_1885, inference(avatar_split_clause, [], [f577, f34276])).
fof(f15568, plain, (~ spl65_957 | spl65_958 | ~ spl65_21 | ~ spl65_26 | ~ spl65_28), inference(avatar_split_clause, [], [f15559, f1933, f1470, f1162, f15565, f15561])).
fof(f15559, plain, ((sK57 = app(cons(sK61, nil), cons(sK61, nil))) | ~ frontsegP(app(cons(sK61, nil), cons(sK61, nil)), sK57) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(subsumption_resolution, [], [f15558, f6247])).
fof(f15558, plain, ((sK57 = app(cons(sK61, nil), cons(sK61, nil))) | ~ frontsegP(app(cons(sK61, nil), cons(sK61, nil)), sK57) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(subsumption_resolution, [], [f15553, f631])).
fof(f15553, plain, ((sK57 = app(cons(sK61, nil), cons(sK61, nil))) | ~ frontsegP(app(cons(sK61, nil), cons(sK61, nil)), sK57) | ~ ssList(sK57) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(resolution, [], [f14809, f487])).
fof(f14809, plain, (frontsegP(sK57, app(cons(sK61, nil), cons(sK61, nil))) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(subsumption_resolution, [], [f14808, f631])).
fof(f14808, plain, (frontsegP(sK57, app(cons(sK61, nil), cons(sK61, nil))) | ~ ssList(sK57) | (~ spl65_21 | ~ spl65_26 | ~ spl65_28)), inference(subsumption_resolution, [], [f14807, f6247])).
fof(f14807, plain, (frontsegP(sK57, app(cons(sK61, nil), cons(sK61, nil))) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | ~ ssList(sK57) | (~ spl65_21 | ~ spl65_28)), inference(subsumption_resolution, [], [f14778, f577])).
fof(f14778, plain, (frontsegP(sK57, app(cons(sK61, nil), cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(app(cons(sK61, nil), cons(sK61, nil))) | ~ ssList(sK57) | (~ spl65_21 | ~ spl65_28)), inference(superposition, [], [f585, f8182])).
fof(f14329, plain, (spl65_287 | ~ spl65_1 | ~ spl65_778), inference(avatar_split_clause, [], [f14303, f12696, f617, f4179])).
fof(f14303, plain, ((nil = tl(sK57)) | (~ spl65_1 | ~ spl65_778)), inference(forward_demodulation, [], [f14302, f14252])).
fof(f14302, plain, ((nil = tl(cons(hd(sK57), nil))) | (~ spl65_1 | ~ spl65_778)), inference(forward_demodulation, [], [f14234, f14251])).
fof(f14234, plain, ((nil = tl(cons(sK54(sK58, sK57), nil))) | ~ spl65_778), inference(resolution, [], [f12697, f1076])).
fof(f1076, plain, ! [X6] : (~ ssItem(X6) | (nil = tl(cons(X6, nil)))), inference(resolution, [], [f465, f454])).
fof(f14043, plain, (~ spl65_1 | spl65_778), inference(avatar_contradiction_clause, [], [f14042])).
fof(f14042, plain, ($false | (~ spl65_1 | spl65_778)), inference(subsumption_resolution, [], [f14041, f8521])).
fof(f14041, plain, (~ sP6(sK58, sK57) | spl65_778), inference(resolution, [], [f12698, f561])).
fof(f561, plain, ! [X0, X1] : (ssItem(sK54(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f12698, plain, (~ ssItem(sK54(sK58, sK57)) | spl65_778), inference(avatar_component_clause, [], [f12696])).
fof(f12786, plain, (~ spl65_778 | spl65_448 | ~ spl65_1), inference(avatar_split_clause, [], [f12785, f617, f8129, f12696])).
fof(f12785, plain, (! [X17, X16] : (~ frontsegP(sK57, cons(X16, X17)) | frontsegP(nil, X17) | ~ ssList(X17) | ~ ssItem(X16) | ~ ssItem(sK54(sK58, sK57))) | ~ spl65_1), inference(subsumption_resolution, [], [f9422, f454])).
fof(f9422, plain, (! [X17, X16] : (~ frontsegP(sK57, cons(X16, X17)) | frontsegP(nil, X17) | ~ ssList(X17) | ~ ssList(nil) | ~ ssItem(X16) | ~ ssItem(sK54(sK58, sK57))) | ~ spl65_1), inference(superposition, [], [f491, f8946])).
fof(f491, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f330])).
fof(f330, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1)) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f329])).
fof(f329, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | (~ frontsegP(X2, X3) | ~ (X0 = X1))) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax44)).
fof(f8852, plain, (spl65_62 | ~ spl65_388), inference(avatar_contradiction_clause, [], [f8851])).
fof(f8851, plain, ($false | (spl65_62 | ~ spl65_388)), inference(subsumption_resolution, [], [f8850, f631])).
fof(f8850, plain, (~ ssList(sK57) | (spl65_62 | ~ spl65_388)), inference(subsumption_resolution, [], [f8846, f2260])).
fof(f8846, plain, ((nil = sK57) | ~ ssList(sK57) | ~ spl65_388), inference(resolution, [], [f6625, f508])).
fof(f508, plain, ! [X0] : (~ segmentP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f333])).
fof(f333, plain, ! [X0] : (((segmentP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ segmentP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((segmentP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (ssList(X0) => (segmentP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax58)).
fof(f6625, plain, (segmentP(nil, sK57) | ~ spl65_388), inference(avatar_component_clause, [], [f6623])).
fof(f8822, plain, (~ spl65_1 | spl65_386 | ~ spl65_465), inference(avatar_contradiction_clause, [], [f8821])).
fof(f8821, plain, ($false | (~ spl65_1 | spl65_386 | ~ spl65_465)), inference(subsumption_resolution, [], [f8820, f8746])).
fof(f8746, plain, (sP6(nil, sK57) | (~ spl65_1 | ~ spl65_465)), inference(backward_demodulation, [], [f8521, f8617])).
fof(f8820, plain, (~ sP6(nil, sK57) | spl65_386), inference(resolution, [], [f6617, f562])).
fof(f562, plain, ! [X0, X1] : (ssList(sK55(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f6617, plain, (~ ssList(sK55(nil, sK57)) | spl65_386), inference(avatar_component_clause, [], [f6615])).
fof(f6615, plain, (spl65_386 <=> ssList(sK55(nil, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_386])])).
fof(f8771, plain, (spl65_3 | ~ spl65_465), inference(avatar_split_clause, [], [f8744, f8615, f626])).
fof(f8744, plain, ((nil = sK60) | ~ spl65_465), inference(backward_demodulation, [], [f572, f8617])).
fof(f8766, plain, (~ spl65_1 | spl65_387 | ~ spl65_465), inference(avatar_contradiction_clause, [], [f8765])).
fof(f8765, plain, ($false | (~ spl65_1 | spl65_387 | ~ spl65_465)), inference(subsumption_resolution, [], [f8746, f8518])).
fof(f8518, plain, (~ sP6(nil, sK57) | spl65_387), inference(resolution, [], [f6621, f563])).
fof(f563, plain, ! [X0, X1] : (ssList(sK56(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f6621, plain, (~ ssList(sK56(nil, sK57)) | spl65_387), inference(avatar_component_clause, [], [f6619])).
fof(f6619, plain, (spl65_387 <=> ssList(sK56(nil, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_387])])).
fof(f6631, plain, (~ spl65_386 | ~ spl65_387 | spl65_389 | ~ spl65_1 | ~ spl65_3), inference(avatar_split_clause, [], [f6627, f626, f617, f6629, f6619, f6615])).
fof(f6627, plain, (! [X0] : (segmentP(nil, X0) | ~ segmentP(sK57, X0) | ~ ssList(sK56(nil, sK57)) | ~ ssList(sK55(nil, sK57)) | ~ ssList(X0)) | (~ spl65_1 | ~ spl65_3)), inference(subsumption_resolution, [], [f6590, f631])).
fof(f6590, plain, (! [X0] : (segmentP(nil, X0) | ~ segmentP(sK57, X0) | ~ ssList(sK56(nil, sK57)) | ~ ssList(sK55(nil, sK57)) | ~ ssList(X0) | ~ ssList(sK57)) | (~ spl65_1 | ~ spl65_3)), inference(superposition, [], [f506, f2321])).
fof(f2321, plain, ((nil = app(app(sK55(nil, sK57), sK57), sK56(nil, sK57))) | (~ spl65_1 | ~ spl65_3)), inference(resolution, [], [f2218, f565])).
fof(f565, plain, ! [X0, X1] : (~ sP6(X0, X1) | (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f351])).
fof(f2218, plain, (sP6(nil, sK57) | (~ spl65_1 | ~ spl65_3)), inference(forward_demodulation, [], [f2217, f628])).
fof(f506, plain, ! [X2, X0, X3, X1] : (segmentP(app(app(X2, X0), X3), X1) | ~ segmentP(X0, X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (segmentP(app(app(X2, X0), X3), X1) | ~ segmentP(X0, X1) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((segmentP(app(app(X2, X0), X3), X1) | ~ segmentP(X0, X1)) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (segmentP(X0, X1) => segmentP(app(app(X2, X0), X3), X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax56)).
fof(f5336, plain, (spl65_366 | spl65_21), inference(avatar_split_clause, [], [f2115, f1162, f5333])).
fof(f2115, plain, ((nil = sK63) | (sK63 = cons(sK51(sK63), sK50(sK63)))), inference(resolution, [], [f576, f460])).
fof(f460, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(sK51(X0), sK50(X0)) = X0)), inference(cnf_transformation, [], [f322])).
fof(f4879, plain, (spl65_62 | spl65_275), inference(avatar_contradiction_clause, [], [f4878])).
fof(f4878, plain, ($false | (spl65_62 | spl65_275)), inference(subsumption_resolution, [], [f4877, f631])).
fof(f4877, plain, (~ ssList(sK57) | (spl65_62 | spl65_275)), inference(subsumption_resolution, [], [f4876, f2260])).
fof(f4876, plain, ((nil = sK57) | ~ ssList(sK57) | spl65_275), inference(resolution, [], [f4130, f464])).
fof(f464, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax24)).
fof(f4130, plain, (~ ssList(tl(sK57)) | spl65_275), inference(avatar_component_clause, [], [f4128])).
fof(f4128, plain, (spl65_275 <=> ssList(tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl65_275])])).
fof(f4439, plain, (spl65_21 | spl65_133), inference(avatar_contradiction_clause, [], [f4438])).
fof(f4438, plain, ($false | (spl65_21 | spl65_133)), inference(subsumption_resolution, [], [f4437, f576])).
fof(f4437, plain, (~ ssList(sK63) | (spl65_21 | spl65_133)), inference(subsumption_resolution, [], [f4436, f1163])).
fof(f4436, plain, ((nil = sK63) | ~ ssList(sK63) | spl65_133), inference(resolution, [], [f3338, f462])).
fof(f462, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax22)).
fof(f3338, plain, (~ ssItem(hd(sK63)) | spl65_133), inference(avatar_component_clause, [], [f3336])).
fof(f4317, plain, (spl65_21 | spl65_132), inference(avatar_contradiction_clause, [], [f4316])).
fof(f4316, plain, ($false | (spl65_21 | spl65_132)), inference(subsumption_resolution, [], [f4315, f576])).
fof(f4315, plain, (~ ssList(sK63) | (spl65_21 | spl65_132)), inference(subsumption_resolution, [], [f4314, f1163])).
fof(f4314, plain, ((nil = sK63) | ~ ssList(sK63) | spl65_132), inference(resolution, [], [f3334, f464])).
fof(f3334, plain, (~ ssList(tl(sK63)) | spl65_132), inference(avatar_component_clause, [], [f3332])).
fof(f4165, plain, (~ spl65_276 | ~ spl65_275 | spl65_283 | ~ spl65_65), inference(avatar_split_clause, [], [f4100, f2274, f4163, f4128, f4132])).
fof(f2274, plain, (spl65_65 <=> (sK57 = cons(hd(sK57), tl(sK57)))), introduced(avatar_definition, [new_symbols(naming, [spl65_65])])).
fof(f4100, plain, (! [X12, X13] : (~ frontsegP(sK57, cons(X12, X13)) | (hd(sK57) = X12) | ~ ssList(X13) | ~ ssList(tl(sK57)) | ~ ssItem(X12) | ~ ssItem(hd(sK57))) | ~ spl65_65), inference(superposition, [], [f490, f2276])).
fof(f2276, plain, ((sK57 = cons(hd(sK57), tl(sK57))) | ~ spl65_65), inference(avatar_component_clause, [], [f2274])).
fof(f490, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f330])).
fof(f4153, plain, (~ spl65_276 | ~ spl65_275 | spl65_280 | ~ spl65_65), inference(avatar_split_clause, [], [f4097, f2274, f4151, f4128, f4132])).
fof(f4097, plain, (! [X8] : (~ memberP(sK57, X8) | (hd(sK57) = X8) | memberP(tl(sK57), X8) | ~ ssList(tl(sK57)) | ~ ssItem(hd(sK57)) | ~ ssItem(X8)) | ~ spl65_65), inference(superposition, [], [f481, f2276])).
fof(f481, plain, ! [X2, X0, X1] : (~ memberP(cons(X1, X2), X0) | (X0 = X1) | memberP(X2, X0) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f328])).
fof(f328, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & (memberP(X2, X0) | (X0 = X1) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f327])).
fof(f327, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & ((memberP(X2, X0) | (X0 = X1)) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => (memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC173+1.p', ax37)).
fof(f3729, plain, (spl65_21 | spl65_100), inference(avatar_contradiction_clause, [], [f3728])).
fof(f3728, plain, ($false | (spl65_21 | spl65_100)), inference(subsumption_resolution, [], [f3727, f576])).
fof(f3727, plain, (~ ssList(sK63) | (spl65_21 | spl65_100)), inference(subsumption_resolution, [], [f3726, f1163])).
fof(f3726, plain, ((nil = sK63) | ~ ssList(sK63) | spl65_100), inference(resolution, [], [f3155, f459])).
fof(f459, plain, ! [X0] : (ssItem(sK51(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f3155, plain, (~ ssItem(sK51(sK63)) | spl65_100), inference(avatar_component_clause, [], [f3153])).
fof(f2277, plain, (spl65_65 | spl65_62), inference(avatar_split_clause, [], [f2231, f2259, f2274])).
fof(f2231, plain, ((nil = sK57) | (sK57 = cons(hd(sK57), tl(sK57)))), inference(resolution, [], [f631, f539])).
fof(f1983, plain, spl65_28, inference(avatar_contradiction_clause, [], [f1982])).
fof(f1982, plain, ($false | spl65_28), inference(subsumption_resolution, [], [f1981, f454])).
fof(f1981, plain, (~ ssList(nil) | spl65_28), inference(subsumption_resolution, [], [f1980, f574])).
fof(f1980, plain, (~ ssItem(sK61) | ~ ssList(nil) | spl65_28), inference(resolution, [], [f1935, f453])).
fof(f1935, plain, (~ ssList(cons(sK61, nil)) | spl65_28), inference(avatar_component_clause, [], [f1933])).
fof(f629, plain, (spl65_1 | spl65_3), inference(avatar_split_clause, [], [f580, f626, f617])).
fof(f580, plain, ((nil = sK60) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f361])).
fof(f624, plain, (spl65_1 | spl65_2), inference(avatar_split_clause, [], [f581, f621, f617])).
fof(f581, plain, ((nil = sK59) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f361])).