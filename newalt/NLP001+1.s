fof(f250, plain, $false, inference(avatar_sat_refutation, [], [f56, f61, f66, f71, f76, f81, f86, f91, f96, f101, f106, f111, f116, f121, f130, f135, f140, f145, f150, f155, f160, f165, f170, f175, f180, f185, f190, f195, f199, f202, f218, f225, f226, f237, f242, f249])).
fof(f249, plain, (~ spl10_4 | ~ spl10_8 | ~ spl10_9 | ~ spl10_10 | ~ spl10_11 | ~ spl10_12 | ~ spl10_35), inference(avatar_contradiction_clause, [], [f248])).
fof(f248, plain, ($false | (~ spl10_4 | ~ spl10_8 | ~ spl10_9 | ~ spl10_10 | ~ spl10_11 | ~ spl10_12 | ~ spl10_35)), inference(subsumption_resolution, [], [f247, f100])).
fof(f100, plain, (car(sK4) | ~ spl10_11), inference(avatar_component_clause, [], [f98])).
fof(f98, plain, (spl10_11 <=> car(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_11])])).
fof(f247, plain, (~ car(sK4) | (~ spl10_4 | ~ spl10_8 | ~ spl10_9 | ~ spl10_10 | ~ spl10_12 | ~ spl10_35)), inference(subsumption_resolution, [], [f246, f95])).
fof(f95, plain, (white(sK4) | ~ spl10_10), inference(avatar_component_clause, [], [f93])).
fof(f93, plain, (spl10_10 <=> white(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_10])])).
fof(f246, plain, (~ white(sK4) | ~ car(sK4) | (~ spl10_4 | ~ spl10_8 | ~ spl10_9 | ~ spl10_12 | ~ spl10_35)), inference(subsumption_resolution, [], [f245, f90])).
fof(f90, plain, (dirty(sK4) | ~ spl10_9), inference(avatar_component_clause, [], [f88])).
fof(f88, plain, (spl10_9 <=> dirty(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_9])])).
fof(f245, plain, (~ dirty(sK4) | ~ white(sK4) | ~ car(sK4) | (~ spl10_4 | ~ spl10_8 | ~ spl10_12 | ~ spl10_35)), inference(subsumption_resolution, [], [f244, f85])).
fof(f85, plain, (old(sK4) | ~ spl10_8), inference(avatar_component_clause, [], [f83])).
fof(f83, plain, (spl10_8 <=> old(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_8])])).
fof(f244, plain, (~ old(sK4) | ~ dirty(sK4) | ~ white(sK4) | ~ car(sK4) | (~ spl10_4 | ~ spl10_12 | ~ spl10_35)), inference(subsumption_resolution, [], [f243, f105])).
fof(f105, plain, (chevy(sK4) | ~ spl10_12), inference(avatar_component_clause, [], [f103])).
fof(f103, plain, (spl10_12 <=> chevy(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_12])])).
fof(f243, plain, (~ chevy(sK4) | ~ old(sK4) | ~ dirty(sK4) | ~ white(sK4) | ~ car(sK4) | (~ spl10_4 | ~ spl10_35)), inference(resolution, [], [f236, f65])).
fof(f65, plain, (barrel(sK3, sK4) | ~ spl10_4), inference(avatar_component_clause, [], [f63])).
fof(f63, plain, (spl10_4 <=> barrel(sK3, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl10_4])])).
fof(f236, plain, (! [X0] : (~ barrel(sK3, X0) | ~ chevy(X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0)) | ~ spl10_35), inference(avatar_component_clause, [], [f235])).
fof(f235, plain, (spl10_35 <=> ! [X0] : (~ chevy(X0) | ~ barrel(sK3, X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0))), introduced(avatar_definition, [new_symbols(naming, [spl10_35])])).
fof(f242, plain, (~ spl10_3 | ~ spl10_5 | ~ spl10_6 | ~ spl10_7 | ~ spl10_34), inference(avatar_contradiction_clause, [], [f241])).
fof(f241, plain, ($false | (~ spl10_3 | ~ spl10_5 | ~ spl10_6 | ~ spl10_7 | ~ spl10_34)), inference(subsumption_resolution, [], [f240, f75])).
fof(f75, plain, (way(sK5) | ~ spl10_6), inference(avatar_component_clause, [], [f73])).
fof(f73, plain, (spl10_6 <=> way(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl10_6])])).
fof(f240, plain, (~ way(sK5) | (~ spl10_3 | ~ spl10_5 | ~ spl10_7 | ~ spl10_34)), inference(subsumption_resolution, [], [f239, f70])).
fof(f70, plain, (lonely(sK5) | ~ spl10_5), inference(avatar_component_clause, [], [f68])).
fof(f68, plain, (spl10_5 <=> lonely(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl10_5])])).
fof(f239, plain, (~ lonely(sK5) | ~ way(sK5) | (~ spl10_3 | ~ spl10_7 | ~ spl10_34)), inference(subsumption_resolution, [], [f238, f80])).
fof(f80, plain, (street(sK5) | ~ spl10_7), inference(avatar_component_clause, [], [f78])).
fof(f78, plain, (spl10_7 <=> street(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl10_7])])).
fof(f238, plain, (~ street(sK5) | ~ lonely(sK5) | ~ way(sK5) | (~ spl10_3 | ~ spl10_34)), inference(resolution, [], [f233, f60])).
fof(f60, plain, (down(sK3, sK5) | ~ spl10_3), inference(avatar_component_clause, [], [f58])).
fof(f58, plain, (spl10_3 <=> down(sK3, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl10_3])])).
fof(f233, plain, (! [X1] : (~ down(sK3, X1) | ~ street(X1) | ~ lonely(X1) | ~ way(X1)) | ~ spl10_34), inference(avatar_component_clause, [], [f232])).
fof(f232, plain, (spl10_34 <=> ! [X1] : (~ street(X1) | ~ down(sK3, X1) | ~ lonely(X1) | ~ way(X1))), introduced(avatar_definition, [new_symbols(naming, [spl10_34])])).
fof(f237, plain, (spl10_34 | spl10_35 | ~ spl10_2 | ~ spl10_13 | ~ spl10_14 | ~ spl10_15 | ~ spl10_31), inference(avatar_split_clause, [], [f230, f197, f118, f113, f108, f53, f235, f232])).
fof(f53, plain, (spl10_2 <=> in(sK3, sK2)), introduced(avatar_definition, [new_symbols(naming, [spl10_2])])).
fof(f108, plain, (spl10_13 <=> event(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl10_13])])).
fof(f113, plain, (spl10_14 <=> city(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl10_14])])).
fof(f118, plain, (spl10_15 <=> hollywood(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl10_15])])).
fof(f197, plain, (spl10_31 <=> ! [X5, X7, X4, X6] : (~ in(X5, X4) | ~ hollywood(X4) | ~ city(X4) | ~ event(X5) | ~ chevy(X6) | ~ car(X6) | ~ white(X6) | ~ dirty(X6) | ~ old(X6) | ~ street(X7) | ~ way(X7) | ~ lonely(X7) | ~ barrel(X5, X6) | ~ down(X5, X7))), introduced(avatar_definition, [new_symbols(naming, [spl10_31])])).
fof(f230, plain, (! [X0, X1] : (~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl10_2 | ~ spl10_13 | ~ spl10_14 | ~ spl10_15 | ~ spl10_31)), inference(subsumption_resolution, [], [f229, f110])).
fof(f110, plain, (event(sK3) | ~ spl10_13), inference(avatar_component_clause, [], [f108])).
fof(f229, plain, (! [X0, X1] : (~ event(sK3) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl10_2 | ~ spl10_14 | ~ spl10_15 | ~ spl10_31)), inference(subsumption_resolution, [], [f228, f115])).
fof(f115, plain, (city(sK2) | ~ spl10_14), inference(avatar_component_clause, [], [f113])).
fof(f228, plain, (! [X0, X1] : (~ city(sK2) | ~ event(sK3) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl10_2 | ~ spl10_15 | ~ spl10_31)), inference(subsumption_resolution, [], [f227, f120])).
fof(f120, plain, (hollywood(sK2) | ~ spl10_15), inference(avatar_component_clause, [], [f118])).
fof(f227, plain, (! [X0, X1] : (~ hollywood(sK2) | ~ city(sK2) | ~ event(sK3) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl10_2 | ~ spl10_31)), inference(resolution, [], [f55, f198])).
fof(f198, plain, (! [X6, X4, X7, X5] : (~ in(X5, X4) | ~ hollywood(X4) | ~ city(X4) | ~ event(X5) | ~ chevy(X6) | ~ car(X6) | ~ white(X6) | ~ dirty(X6) | ~ old(X6) | ~ street(X7) | ~ way(X7) | ~ lonely(X7) | ~ barrel(X5, X6) | ~ down(X5, X7)) | ~ spl10_31), inference(avatar_component_clause, [], [f197])).
fof(f55, plain, (in(sK3, sK2) | ~ spl10_2), inference(avatar_component_clause, [], [f53])).
fof(f226, plain, (spl10_32 | spl10_33 | ~ spl10_28 | ~ spl10_17 | ~ spl10_29 | ~ spl10_30 | ~ spl10_31), inference(avatar_split_clause, [], [f205, f197, f192, f187, f127, f182, f211, f208])).
fof(f208, plain, (spl10_32 <=> ! [X1] : (~ street(X1) | ~ down(sK7, X1) | ~ lonely(X1) | ~ way(X1))), introduced(avatar_definition, [new_symbols(naming, [spl10_32])])).
fof(f211, plain, (spl10_33 <=> ! [X0] : (~ chevy(X0) | ~ barrel(sK7, X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0))), introduced(avatar_definition, [new_symbols(naming, [spl10_33])])).
fof(f182, plain, (spl10_28 <=> event(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl10_28])])).
fof(f127, plain, (spl10_17 <=> in(sK7, sK6)), introduced(avatar_definition, [new_symbols(naming, [spl10_17])])).
fof(f187, plain, (spl10_29 <=> city(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl10_29])])).
fof(f192, plain, (spl10_30 <=> hollywood(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl10_30])])).
fof(f205, plain, (! [X0, X1] : (~ event(sK7) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK7, X0) | ~ down(sK7, X1)) | (~ spl10_17 | ~ spl10_29 | ~ spl10_30 | ~ spl10_31)), inference(subsumption_resolution, [], [f204, f189])).
fof(f189, plain, (city(sK6) | ~ spl10_29), inference(avatar_component_clause, [], [f187])).
fof(f204, plain, (! [X0, X1] : (~ city(sK6) | ~ event(sK7) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK7, X0) | ~ down(sK7, X1)) | (~ spl10_17 | ~ spl10_30 | ~ spl10_31)), inference(subsumption_resolution, [], [f203, f194])).
fof(f194, plain, (hollywood(sK6) | ~ spl10_30), inference(avatar_component_clause, [], [f192])).
fof(f203, plain, (! [X0, X1] : (~ hollywood(sK6) | ~ city(sK6) | ~ event(sK7) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK7, X0) | ~ down(sK7, X1)) | (~ spl10_17 | ~ spl10_31)), inference(resolution, [], [f198, f129])).
fof(f129, plain, (in(sK7, sK6) | ~ spl10_17), inference(avatar_component_clause, [], [f127])).
fof(f225, plain, (~ spl10_19 | ~ spl10_20 | ~ spl10_21 | ~ spl10_22 | ~ spl10_23 | ~ spl10_24 | ~ spl10_33), inference(avatar_contradiction_clause, [], [f224])).
fof(f224, plain, ($false | (~ spl10_19 | ~ spl10_20 | ~ spl10_21 | ~ spl10_22 | ~ spl10_23 | ~ spl10_24 | ~ spl10_33)), inference(subsumption_resolution, [], [f223, f159])).
fof(f159, plain, (car(sK9) | ~ spl10_23), inference(avatar_component_clause, [], [f157])).
fof(f157, plain, (spl10_23 <=> car(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_23])])).
fof(f223, plain, (~ car(sK9) | (~ spl10_19 | ~ spl10_20 | ~ spl10_21 | ~ spl10_22 | ~ spl10_24 | ~ spl10_33)), inference(subsumption_resolution, [], [f222, f154])).
fof(f154, plain, (white(sK9) | ~ spl10_22), inference(avatar_component_clause, [], [f152])).
fof(f152, plain, (spl10_22 <=> white(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_22])])).
fof(f222, plain, (~ white(sK9) | ~ car(sK9) | (~ spl10_19 | ~ spl10_20 | ~ spl10_21 | ~ spl10_24 | ~ spl10_33)), inference(subsumption_resolution, [], [f221, f149])).
fof(f149, plain, (dirty(sK9) | ~ spl10_21), inference(avatar_component_clause, [], [f147])).
fof(f147, plain, (spl10_21 <=> dirty(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_21])])).
fof(f221, plain, (~ dirty(sK9) | ~ white(sK9) | ~ car(sK9) | (~ spl10_19 | ~ spl10_20 | ~ spl10_24 | ~ spl10_33)), inference(subsumption_resolution, [], [f220, f144])).
fof(f144, plain, (old(sK9) | ~ spl10_20), inference(avatar_component_clause, [], [f142])).
fof(f142, plain, (spl10_20 <=> old(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_20])])).
fof(f220, plain, (~ old(sK9) | ~ dirty(sK9) | ~ white(sK9) | ~ car(sK9) | (~ spl10_19 | ~ spl10_24 | ~ spl10_33)), inference(subsumption_resolution, [], [f219, f164])).
fof(f164, plain, (chevy(sK9) | ~ spl10_24), inference(avatar_component_clause, [], [f162])).
fof(f162, plain, (spl10_24 <=> chevy(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_24])])).
fof(f219, plain, (~ chevy(sK9) | ~ old(sK9) | ~ dirty(sK9) | ~ white(sK9) | ~ car(sK9) | (~ spl10_19 | ~ spl10_33)), inference(resolution, [], [f212, f139])).
fof(f139, plain, (barrel(sK7, sK9) | ~ spl10_19), inference(avatar_component_clause, [], [f137])).
fof(f137, plain, (spl10_19 <=> barrel(sK7, sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_19])])).
fof(f212, plain, (! [X0] : (~ barrel(sK7, X0) | ~ chevy(X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0)) | ~ spl10_33), inference(avatar_component_clause, [], [f211])).
fof(f218, plain, (~ spl10_18 | ~ spl10_25 | ~ spl10_26 | ~ spl10_27 | ~ spl10_32), inference(avatar_contradiction_clause, [], [f217])).
fof(f217, plain, ($false | (~ spl10_18 | ~ spl10_25 | ~ spl10_26 | ~ spl10_27 | ~ spl10_32)), inference(subsumption_resolution, [], [f216, f174])).
fof(f174, plain, (way(sK8) | ~ spl10_26), inference(avatar_component_clause, [], [f172])).
fof(f172, plain, (spl10_26 <=> way(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_26])])).
fof(f216, plain, (~ way(sK8) | (~ spl10_18 | ~ spl10_25 | ~ spl10_27 | ~ spl10_32)), inference(subsumption_resolution, [], [f215, f169])).
fof(f169, plain, (lonely(sK8) | ~ spl10_25), inference(avatar_component_clause, [], [f167])).
fof(f167, plain, (spl10_25 <=> lonely(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_25])])).
fof(f215, plain, (~ lonely(sK8) | ~ way(sK8) | (~ spl10_18 | ~ spl10_27 | ~ spl10_32)), inference(subsumption_resolution, [], [f214, f179])).
fof(f179, plain, (street(sK8) | ~ spl10_27), inference(avatar_component_clause, [], [f177])).
fof(f177, plain, (spl10_27 <=> street(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_27])])).
fof(f214, plain, (~ street(sK8) | ~ lonely(sK8) | ~ way(sK8) | (~ spl10_18 | ~ spl10_32)), inference(resolution, [], [f209, f134])).
fof(f134, plain, (down(sK7, sK8) | ~ spl10_18), inference(avatar_component_clause, [], [f132])).
fof(f132, plain, (spl10_18 <=> down(sK7, sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_18])])).
fof(f209, plain, (! [X1] : (~ down(sK7, X1) | ~ street(X1) | ~ lonely(X1) | ~ way(X1)) | ~ spl10_32), inference(avatar_component_clause, [], [f208])).
fof(f202, plain, (spl10_16 | spl10_1), inference(avatar_split_clause, [], [f44, f49, f123])).
fof(f123, plain, (spl10_16 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl10_16])])).
fof(f49, plain, (spl10_1 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl10_1])])).
fof(f44, plain, (sP1 | sP0), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((! [X0, X1, X2, X3] : (~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0)) & sP1) | (! [X4, X5, X6, X7] : (~ in(X5, X4) | ~ down(X5, X7) | ~ barrel(X5, X6) | ~ lonely(X7) | ~ way(X7) | ~ street(X7) | ~ old(X6) | ~ dirty(X6) | ~ white(X6) | ~ car(X6) | ~ chevy(X6) | ~ event(X5) | ~ city(X4) | ~ hollywood(X4)) & sP0)), inference(rectify, [], [f7])).
fof(f7, plain, ((! [X4, X5, X6, X7] : (~ in(X5, X4) | ~ down(X5, X6) | ~ barrel(X5, X7) | ~ old(X7) | ~ dirty(X7) | ~ white(X7) | ~ car(X7) | ~ chevy(X7) | ~ lonely(X6) | ~ way(X6) | ~ street(X6) | ~ event(X5) | ~ city(X4) | ~ hollywood(X4)) & sP1) | (! [X12, X13, X14, X15] : (~ in(X13, X12) | ~ down(X13, X15) | ~ barrel(X13, X14) | ~ lonely(X15) | ~ way(X15) | ~ street(X15) | ~ old(X14) | ~ dirty(X14) | ~ white(X14) | ~ car(X14) | ~ chevy(X14) | ~ event(X13) | ~ city(X12) | ~ hollywood(X12)) & sP0)), inference(definition_folding, [], [f4, e6, e5])).
fof(f5, plain, (? [X8, X9, X10, X11] : (in(X9, X8) & down(X9, X10) & barrel(X9, X11) & old(X11) & dirty(X11) & white(X11) & car(X11) & chevy(X11) & lonely(X10) & way(X10) & street(X10) & event(X9) & city(X8) & hollywood(X8)) | ~ sP0), inference(usedef, [], [e5])).
fof(e5, plain, (sP0 <=> ? [X8, X9, X10, X11] : (in(X9, X8) & down(X9, X10) & barrel(X9, X11) & old(X11) & dirty(X11) & white(X11) & car(X11) & chevy(X11) & lonely(X10) & way(X10) & street(X10) & event(X9) & city(X8) & hollywood(X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f6, plain, (? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) | ~ sP1), inference(usedef, [], [e6])).
fof(e6, plain, (sP1 <=> ? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f4, plain, ((! [X4, X5, X6, X7] : (~ in(X5, X4) | ~ down(X5, X6) | ~ barrel(X5, X7) | ~ old(X7) | ~ dirty(X7) | ~ white(X7) | ~ car(X7) | ~ chevy(X7) | ~ lonely(X6) | ~ way(X6) | ~ street(X6) | ~ event(X5) | ~ city(X4) | ~ hollywood(X4)) & ? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0))) | (! [X12, X13, X14, X15] : (~ in(X13, X12) | ~ down(X13, X15) | ~ barrel(X13, X14) | ~ lonely(X15) | ~ way(X15) | ~ street(X15) | ~ old(X14) | ~ dirty(X14) | ~ white(X14) | ~ car(X14) | ~ chevy(X14) | ~ event(X13) | ~ city(X12) | ~ hollywood(X12)) & ? [X8, X9, X10, X11] : (in(X9, X8) & down(X9, X10) & barrel(X9, X11) & old(X11) & dirty(X11) & white(X11) & car(X11) & chevy(X11) & lonely(X10) & way(X10) & street(X10) & event(X9) & city(X8) & hollywood(X8)))), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ~ ((? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) => ? [X4, X5, X6, X7] : (in(X5, X4) & down(X5, X6) & barrel(X5, X7) & old(X7) & dirty(X7) & white(X7) & car(X7) & chevy(X7) & lonely(X6) & way(X6) & street(X6) & event(X5) & city(X4) & hollywood(X4))) & (? [X8, X9, X10, X11] : (in(X9, X8) & down(X9, X10) & barrel(X9, X11) & old(X11) & dirty(X11) & white(X11) & car(X11) & chevy(X11) & lonely(X10) & way(X10) & street(X10) & event(X9) & city(X8) & hollywood(X8)) => ? [X12, X13, X14, X15] : (in(X13, X12) & down(X13, X15) & barrel(X13, X14) & lonely(X15) & way(X15) & street(X15) & old(X14) & dirty(X14) & white(X14) & car(X14) & chevy(X14) & event(X13) & city(X12) & hollywood(X12)))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ((? [X8, X9, X10, X11] : (in(X9, X8) & down(X9, X11) & barrel(X9, X10) & lonely(X11) & way(X11) & street(X11) & old(X10) & dirty(X10) & white(X10) & car(X10) & chevy(X10) & event(X9) & city(X8) & hollywood(X8)) => ? [X12, X13, X14, X15] : (in(X13, X12) & down(X13, X14) & barrel(X13, X15) & old(X15) & dirty(X15) & white(X15) & car(X15) & chevy(X15) & lonely(X14) & way(X14) & street(X14) & event(X13) & city(X12) & hollywood(X12))) & (? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) => ? [X4, X5, X6, X7] : (in(X5, X4) & down(X5, X7) & barrel(X5, X6) & lonely(X7) & way(X7) & street(X7) & old(X6) & dirty(X6) & white(X6) & car(X6) & chevy(X6) & event(X5) & city(X4) & hollywood(X4)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ((? [X8, X9, X10, X11] : (in(X9, X8) & down(X9, X11) & barrel(X9, X10) & lonely(X11) & way(X11) & street(X11) & old(X10) & dirty(X10) & white(X10) & car(X10) & chevy(X10) & event(X9) & city(X8) & hollywood(X8)) => ? [X12, X13, X14, X15] : (in(X13, X12) & down(X13, X14) & barrel(X13, X15) & old(X15) & dirty(X15) & white(X15) & car(X15) & chevy(X15) & lonely(X14) & way(X14) & street(X14) & event(X13) & city(X12) & hollywood(X12))) & (? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) => ? [X4, X5, X6, X7] : (in(X5, X4) & down(X5, X7) & barrel(X5, X6) & lonely(X7) & way(X7) & street(X7) & old(X6) & dirty(X6) & white(X6) & car(X6) & chevy(X6) & event(X5) & city(X4) & hollywood(X4)))), file('/home/ubuntu/library/tptp/Problems/NLP/NLP001+1.p', co1)).
fof(f199, plain, (spl10_31 | spl10_31), inference(avatar_split_clause, [], [f47, f197, f197])).
fof(f47, plain, ! [X6, X4, X2, X0, X7, X5, X3, X1] : (~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0) | ~ in(X5, X4) | ~ down(X5, X7) | ~ barrel(X5, X6) | ~ lonely(X7) | ~ way(X7) | ~ street(X7) | ~ old(X6) | ~ dirty(X6) | ~ white(X6) | ~ car(X6) | ~ chevy(X6) | ~ event(X5) | ~ city(X4) | ~ hollywood(X4)), inference(cnf_transformation, [], [f15])).
fof(f195, plain, (~ spl10_16 | spl10_30), inference(avatar_split_clause, [], [f30, f192, f123])).
fof(f30, plain, (hollywood(sK6) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((in(sK7, sK6) & down(sK7, sK8) & barrel(sK7, sK9) & old(sK9) & dirty(sK9) & white(sK9) & car(sK9) & chevy(sK9) & lonely(sK8) & way(sK8) & street(sK8) & event(sK7) & city(sK6) & hollywood(sK6)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7, sK8, sK9])], [f12, f13])).
fof(f13, plain, (? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) => (in(sK7, sK6) & down(sK7, sK8) & barrel(sK7, sK9) & old(sK9) & dirty(sK9) & white(sK9) & car(sK9) & chevy(sK9) & lonely(sK8) & way(sK8) & street(sK8) & event(sK7) & city(sK6) & hollywood(sK6))), introduced(choice_axiom, [])).
fof(f12, plain, (? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) | ~ sP0), inference(rectify, [], [f11])).
fof(f11, plain, (? [X8, X9, X10, X11] : (in(X9, X8) & down(X9, X10) & barrel(X9, X11) & old(X11) & dirty(X11) & white(X11) & car(X11) & chevy(X11) & lonely(X10) & way(X10) & street(X10) & event(X9) & city(X8) & hollywood(X8)) | ~ sP0), inference(nnf_transformation, [], [f5])).
fof(f190, plain, (~ spl10_16 | spl10_29), inference(avatar_split_clause, [], [f31, f187, f123])).
fof(f31, plain, (city(sK6) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f185, plain, (~ spl10_16 | spl10_28), inference(avatar_split_clause, [], [f32, f182, f123])).
fof(f32, plain, (event(sK7) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f180, plain, (~ spl10_16 | spl10_27), inference(avatar_split_clause, [], [f33, f177, f123])).
fof(f33, plain, (street(sK8) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f175, plain, (~ spl10_16 | spl10_26), inference(avatar_split_clause, [], [f34, f172, f123])).
fof(f34, plain, (way(sK8) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f170, plain, (~ spl10_16 | spl10_25), inference(avatar_split_clause, [], [f35, f167, f123])).
fof(f35, plain, (lonely(sK8) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f165, plain, (~ spl10_16 | spl10_24), inference(avatar_split_clause, [], [f36, f162, f123])).
fof(f36, plain, (chevy(sK9) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f160, plain, (~ spl10_16 | spl10_23), inference(avatar_split_clause, [], [f37, f157, f123])).
fof(f37, plain, (car(sK9) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f155, plain, (~ spl10_16 | spl10_22), inference(avatar_split_clause, [], [f38, f152, f123])).
fof(f38, plain, (white(sK9) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f150, plain, (~ spl10_16 | spl10_21), inference(avatar_split_clause, [], [f39, f147, f123])).
fof(f39, plain, (dirty(sK9) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f145, plain, (~ spl10_16 | spl10_20), inference(avatar_split_clause, [], [f40, f142, f123])).
fof(f40, plain, (old(sK9) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f140, plain, (~ spl10_16 | spl10_19), inference(avatar_split_clause, [], [f41, f137, f123])).
fof(f41, plain, (barrel(sK7, sK9) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f135, plain, (~ spl10_16 | spl10_18), inference(avatar_split_clause, [], [f42, f132, f123])).
fof(f42, plain, (down(sK7, sK8) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f130, plain, (~ spl10_16 | spl10_17), inference(avatar_split_clause, [], [f43, f127, f123])).
fof(f43, plain, (in(sK7, sK6) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f121, plain, (~ spl10_1 | spl10_15), inference(avatar_split_clause, [], [f16, f118, f49])).
fof(f16, plain, (hollywood(sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ((in(sK3, sK2) & down(sK3, sK5) & barrel(sK3, sK4) & lonely(sK5) & way(sK5) & street(sK5) & old(sK4) & dirty(sK4) & white(sK4) & car(sK4) & chevy(sK4) & event(sK3) & city(sK2) & hollywood(sK2)) | ~ sP1), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4, sK5])], [f8, f9])).
fof(f9, plain, (? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) => (in(sK3, sK2) & down(sK3, sK5) & barrel(sK3, sK4) & lonely(sK5) & way(sK5) & street(sK5) & old(sK4) & dirty(sK4) & white(sK4) & car(sK4) & chevy(sK4) & event(sK3) & city(sK2) & hollywood(sK2))), introduced(choice_axiom, [])).
fof(f8, plain, (? [X0, X1, X2, X3] : (in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) | ~ sP1), inference(nnf_transformation, [], [f6])).
fof(f116, plain, (~ spl10_1 | spl10_14), inference(avatar_split_clause, [], [f17, f113, f49])).
fof(f17, plain, (city(sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f111, plain, (~ spl10_1 | spl10_13), inference(avatar_split_clause, [], [f18, f108, f49])).
fof(f18, plain, (event(sK3) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f106, plain, (~ spl10_1 | spl10_12), inference(avatar_split_clause, [], [f19, f103, f49])).
fof(f19, plain, (chevy(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f101, plain, (~ spl10_1 | spl10_11), inference(avatar_split_clause, [], [f20, f98, f49])).
fof(f20, plain, (car(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f96, plain, (~ spl10_1 | spl10_10), inference(avatar_split_clause, [], [f21, f93, f49])).
fof(f21, plain, (white(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f91, plain, (~ spl10_1 | spl10_9), inference(avatar_split_clause, [], [f22, f88, f49])).
fof(f22, plain, (dirty(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f86, plain, (~ spl10_1 | spl10_8), inference(avatar_split_clause, [], [f23, f83, f49])).
fof(f23, plain, (old(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f81, plain, (~ spl10_1 | spl10_7), inference(avatar_split_clause, [], [f24, f78, f49])).
fof(f24, plain, (street(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f76, plain, (~ spl10_1 | spl10_6), inference(avatar_split_clause, [], [f25, f73, f49])).
fof(f25, plain, (way(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f71, plain, (~ spl10_1 | spl10_5), inference(avatar_split_clause, [], [f26, f68, f49])).
fof(f26, plain, (lonely(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f66, plain, (~ spl10_1 | spl10_4), inference(avatar_split_clause, [], [f27, f63, f49])).
fof(f27, plain, (barrel(sK3, sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f61, plain, (~ spl10_1 | spl10_3), inference(avatar_split_clause, [], [f28, f58, f49])).
fof(f28, plain, (down(sK3, sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f56, plain, (~ spl10_1 | spl10_2), inference(avatar_split_clause, [], [f29, f53, f49])).
fof(f29, plain, (in(sK3, sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).