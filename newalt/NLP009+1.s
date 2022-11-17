fof(f611, plain, $false, inference(avatar_sat_refutation, [], [f92, f97, f102, f107, f112, f117, f122, f127, f132, f137, f142, f147, f152, f157, f162, f167, f172, f177, f182, f187, f192, f197, f202, f207, f212, f217, f222, f227, f236, f241, f246, f251, f256, f261, f266, f271, f276, f281, f286, f291, f296, f301, f306, f311, f316, f321, f326, f331, f336, f341, f346, f351, f356, f361, f366, f371, f378, f381, f476, f484, f488, f495, f508, f545, f552, f610])).
fof(f610, plain, (~ spl20_2 | ~ spl20_3 | ~ spl20_4 | ~ spl20_5 | ~ spl20_6 | ~ spl20_7 | ~ spl20_8 | ~ spl20_9 | ~ spl20_10 | ~ spl20_11 | spl20_12 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60), inference(avatar_contradiction_clause, [], [f609])).
fof(f609, plain, ($false | (~ spl20_2 | ~ spl20_3 | ~ spl20_4 | ~ spl20_5 | ~ spl20_6 | ~ spl20_7 | ~ spl20_8 | ~ spl20_9 | ~ spl20_10 | ~ spl20_11 | spl20_12 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f608, f141])).
fof(f141, plain, (~ (sK6 = sK7) | spl20_12), inference(avatar_component_clause, [], [f139])).
fof(f139, plain, (spl20_12 <=> (sK6 = sK7)), introduced(avatar_definition, [new_symbols(naming, [spl20_12])])).
fof(f608, plain, ((sK6 = sK7) | (~ spl20_2 | ~ spl20_3 | ~ spl20_4 | ~ spl20_5 | ~ spl20_6 | ~ spl20_7 | ~ spl20_8 | ~ spl20_9 | ~ spl20_10 | ~ spl20_11 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f607, f111])).
fof(f111, plain, (young(sK7) | ~ spl20_6), inference(avatar_component_clause, [], [f109])).
fof(f109, plain, (spl20_6 <=> young(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl20_6])])).
fof(f607, plain, (~ young(sK7) | (sK6 = sK7) | (~ spl20_2 | ~ spl20_3 | ~ spl20_4 | ~ spl20_5 | ~ spl20_7 | ~ spl20_8 | ~ spl20_9 | ~ spl20_10 | ~ spl20_11 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f606, f116])).
fof(f116, plain, (man(sK7) | ~ spl20_7), inference(avatar_component_clause, [], [f114])).
fof(f114, plain, (spl20_7 <=> man(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl20_7])])).
fof(f606, plain, (~ man(sK7) | ~ young(sK7) | (sK6 = sK7) | (~ spl20_2 | ~ spl20_3 | ~ spl20_4 | ~ spl20_5 | ~ spl20_8 | ~ spl20_9 | ~ spl20_10 | ~ spl20_11 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f605, f121])).
fof(f121, plain, (fellow(sK7) | ~ spl20_8), inference(avatar_component_clause, [], [f119])).
fof(f119, plain, (spl20_8 <=> fellow(sK7)), introduced(avatar_definition, [new_symbols(naming, [spl20_8])])).
fof(f605, plain, (~ fellow(sK7) | ~ man(sK7) | ~ young(sK7) | (sK6 = sK7) | (~ spl20_2 | ~ spl20_3 | ~ spl20_4 | ~ spl20_5 | ~ spl20_9 | ~ spl20_10 | ~ spl20_11 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(resolution, [], [f591, f497])).
fof(f497, plain, (in(sK7, sK8) | (~ spl20_2 | ~ spl20_3)), inference(forward_demodulation, [], [f91, f96])).
fof(f96, plain, ((sK7 = sK10) | ~ spl20_3), inference(avatar_component_clause, [], [f94])).
fof(f94, plain, (spl20_3 <=> (sK7 = sK10)), introduced(avatar_definition, [new_symbols(naming, [spl20_3])])).
fof(f91, plain, (in(sK10, sK8) | ~ spl20_2), inference(avatar_component_clause, [], [f89])).
fof(f89, plain, (spl20_2 <=> in(sK10, sK8)), introduced(avatar_definition, [new_symbols(naming, [spl20_2])])).
fof(f591, plain, (! [X1] : (~ in(X1, sK8) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | (sK6 = X1)) | (~ spl20_4 | ~ spl20_5 | ~ spl20_9 | ~ spl20_10 | ~ spl20_11 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f590, f126])).
fof(f126, plain, (young(sK6) | ~ spl20_9), inference(avatar_component_clause, [], [f124])).
fof(f124, plain, (spl20_9 <=> young(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl20_9])])).
fof(f590, plain, (! [X1] : ((sK6 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ young(sK6) | ~ in(X1, sK8)) | (~ spl20_4 | ~ spl20_5 | ~ spl20_10 | ~ spl20_11 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f589, f131])).
fof(f131, plain, (man(sK6) | ~ spl20_10), inference(avatar_component_clause, [], [f129])).
fof(f129, plain, (spl20_10 <=> man(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl20_10])])).
fof(f589, plain, (! [X1] : ((sK6 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ man(sK6) | ~ young(sK6) | ~ in(X1, sK8)) | (~ spl20_4 | ~ spl20_5 | ~ spl20_11 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f588, f136])).
fof(f136, plain, (fellow(sK6) | ~ spl20_11), inference(avatar_component_clause, [], [f134])).
fof(f134, plain, (spl20_11 <=> fellow(sK6)), introduced(avatar_definition, [new_symbols(naming, [spl20_11])])).
fof(f588, plain, (! [X1] : ((sK6 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK6) | ~ man(sK6) | ~ young(sK6) | ~ in(X1, sK8)) | (~ spl20_4 | ~ spl20_5 | ~ spl20_13 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f587, f146])).
fof(f146, plain, (front(sK8) | ~ spl20_13), inference(avatar_component_clause, [], [f144])).
fof(f144, plain, (spl20_13 <=> front(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl20_13])])).
fof(f587, plain, (! [X1] : (~ front(sK8) | (sK6 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK6) | ~ man(sK6) | ~ young(sK6) | ~ in(X1, sK8)) | (~ spl20_4 | ~ spl20_5 | ~ spl20_14 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f586, f151])).
fof(f151, plain, (furniture(sK8) | ~ spl20_14), inference(avatar_component_clause, [], [f149])).
fof(f149, plain, (spl20_14 <=> furniture(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl20_14])])).
fof(f586, plain, (! [X1] : (~ furniture(sK8) | ~ front(sK8) | (sK6 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK6) | ~ man(sK6) | ~ young(sK6) | ~ in(X1, sK8)) | (~ spl20_4 | ~ spl20_5 | ~ spl20_15 | ~ spl20_60)), inference(subsumption_resolution, [], [f554, f156])).
fof(f156, plain, (seat(sK8) | ~ spl20_15), inference(avatar_component_clause, [], [f154])).
fof(f154, plain, (spl20_15 <=> seat(sK8)), introduced(avatar_definition, [new_symbols(naming, [spl20_15])])).
fof(f554, plain, (! [X1] : (~ seat(sK8) | ~ furniture(sK8) | ~ front(sK8) | (sK6 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK6) | ~ man(sK6) | ~ young(sK6) | ~ in(X1, sK8)) | (~ spl20_4 | ~ spl20_5 | ~ spl20_60)), inference(resolution, [], [f377, f496])).
fof(f496, plain, (in(sK6, sK8) | (~ spl20_4 | ~ spl20_5)), inference(forward_demodulation, [], [f101, f106])).
fof(f106, plain, ((sK6 = sK9) | ~ spl20_5), inference(avatar_component_clause, [], [f104])).
fof(f104, plain, (spl20_5 <=> (sK6 = sK9)), introduced(avatar_definition, [new_symbols(naming, [spl20_5])])).
fof(f101, plain, (in(sK9, sK8) | ~ spl20_4), inference(avatar_component_clause, [], [f99])).
fof(f99, plain, (spl20_4 <=> in(sK9, sK8)), introduced(avatar_definition, [new_symbols(naming, [spl20_4])])).
fof(f377, plain, (! [X17, X15, X16] : (~ in(X17, X15) | ~ seat(X15) | ~ furniture(X15) | ~ front(X15) | (X16 = X17) | ~ fellow(X16) | ~ man(X16) | ~ young(X16) | ~ fellow(X17) | ~ man(X17) | ~ young(X17) | ~ in(X16, X15)) | ~ spl20_60), inference(avatar_component_clause, [], [f376])).
fof(f376, plain, (spl20_60 <=> ! [X16, X15, X17] : (~ in(X17, X15) | ~ seat(X15) | ~ furniture(X15) | ~ front(X15) | (X16 = X17) | ~ fellow(X16) | ~ man(X16) | ~ young(X16) | ~ fellow(X17) | ~ man(X17) | ~ young(X17) | ~ in(X16, X15))), introduced(avatar_definition, [new_symbols(naming, [spl20_60])])).
fof(f552, plain, (~ spl20_18 | ~ spl20_22 | ~ spl20_23 | ~ spl20_24 | ~ spl20_25 | ~ spl20_26 | ~ spl20_79), inference(avatar_contradiction_clause, [], [f551])).
fof(f551, plain, ($false | (~ spl20_18 | ~ spl20_22 | ~ spl20_23 | ~ spl20_24 | ~ spl20_25 | ~ spl20_26 | ~ spl20_79)), inference(subsumption_resolution, [], [f550, f206])).
fof(f206, plain, (car(sK4) | ~ spl20_25), inference(avatar_component_clause, [], [f204])).
fof(f204, plain, (spl20_25 <=> car(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl20_25])])).
fof(f550, plain, (~ car(sK4) | (~ spl20_18 | ~ spl20_22 | ~ spl20_23 | ~ spl20_24 | ~ spl20_26 | ~ spl20_79)), inference(subsumption_resolution, [], [f549, f201])).
fof(f201, plain, (white(sK4) | ~ spl20_24), inference(avatar_component_clause, [], [f199])).
fof(f199, plain, (spl20_24 <=> white(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl20_24])])).
fof(f549, plain, (~ white(sK4) | ~ car(sK4) | (~ spl20_18 | ~ spl20_22 | ~ spl20_23 | ~ spl20_26 | ~ spl20_79)), inference(subsumption_resolution, [], [f548, f196])).
fof(f196, plain, (dirty(sK4) | ~ spl20_23), inference(avatar_component_clause, [], [f194])).
fof(f194, plain, (spl20_23 <=> dirty(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl20_23])])).
fof(f548, plain, (~ dirty(sK4) | ~ white(sK4) | ~ car(sK4) | (~ spl20_18 | ~ spl20_22 | ~ spl20_26 | ~ spl20_79)), inference(subsumption_resolution, [], [f547, f191])).
fof(f191, plain, (old(sK4) | ~ spl20_22), inference(avatar_component_clause, [], [f189])).
fof(f189, plain, (spl20_22 <=> old(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl20_22])])).
fof(f547, plain, (~ old(sK4) | ~ dirty(sK4) | ~ white(sK4) | ~ car(sK4) | (~ spl20_18 | ~ spl20_26 | ~ spl20_79)), inference(subsumption_resolution, [], [f546, f211])).
fof(f211, plain, (chevy(sK4) | ~ spl20_26), inference(avatar_component_clause, [], [f209])).
fof(f209, plain, (spl20_26 <=> chevy(sK4)), introduced(avatar_definition, [new_symbols(naming, [spl20_26])])).
fof(f546, plain, (~ chevy(sK4) | ~ old(sK4) | ~ dirty(sK4) | ~ white(sK4) | ~ car(sK4) | (~ spl20_18 | ~ spl20_79)), inference(resolution, [], [f507, f171])).
fof(f171, plain, (barrel(sK3, sK4) | ~ spl20_18), inference(avatar_component_clause, [], [f169])).
fof(f169, plain, (spl20_18 <=> barrel(sK3, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl20_18])])).
fof(f507, plain, (! [X0] : (~ barrel(sK3, X0) | ~ chevy(X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0)) | ~ spl20_79), inference(avatar_component_clause, [], [f506])).
fof(f506, plain, (spl20_79 <=> ! [X0] : (~ chevy(X0) | ~ barrel(sK3, X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0))), introduced(avatar_definition, [new_symbols(naming, [spl20_79])])).
fof(f545, plain, (~ spl20_17 | ~ spl20_19 | ~ spl20_20 | ~ spl20_21 | ~ spl20_78), inference(avatar_contradiction_clause, [], [f544])).
fof(f544, plain, ($false | (~ spl20_17 | ~ spl20_19 | ~ spl20_20 | ~ spl20_21 | ~ spl20_78)), inference(subsumption_resolution, [], [f543, f181])).
fof(f181, plain, (way(sK5) | ~ spl20_20), inference(avatar_component_clause, [], [f179])).
fof(f179, plain, (spl20_20 <=> way(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl20_20])])).
fof(f543, plain, (~ way(sK5) | (~ spl20_17 | ~ spl20_19 | ~ spl20_21 | ~ spl20_78)), inference(subsumption_resolution, [], [f542, f176])).
fof(f176, plain, (lonely(sK5) | ~ spl20_19), inference(avatar_component_clause, [], [f174])).
fof(f174, plain, (spl20_19 <=> lonely(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl20_19])])).
fof(f542, plain, (~ lonely(sK5) | ~ way(sK5) | (~ spl20_17 | ~ spl20_21 | ~ spl20_78)), inference(subsumption_resolution, [], [f541, f186])).
fof(f186, plain, (street(sK5) | ~ spl20_21), inference(avatar_component_clause, [], [f184])).
fof(f184, plain, (spl20_21 <=> street(sK5)), introduced(avatar_definition, [new_symbols(naming, [spl20_21])])).
fof(f541, plain, (~ street(sK5) | ~ lonely(sK5) | ~ way(sK5) | (~ spl20_17 | ~ spl20_78)), inference(resolution, [], [f504, f166])).
fof(f166, plain, (down(sK3, sK5) | ~ spl20_17), inference(avatar_component_clause, [], [f164])).
fof(f164, plain, (spl20_17 <=> down(sK3, sK5)), introduced(avatar_definition, [new_symbols(naming, [spl20_17])])).
fof(f504, plain, (! [X1] : (~ down(sK3, X1) | ~ street(X1) | ~ lonely(X1) | ~ way(X1)) | ~ spl20_78), inference(avatar_component_clause, [], [f503])).
fof(f503, plain, (spl20_78 <=> ! [X1] : (~ street(X1) | ~ down(sK3, X1) | ~ lonely(X1) | ~ way(X1))), introduced(avatar_definition, [new_symbols(naming, [spl20_78])])).
fof(f508, plain, (spl20_78 | spl20_79 | ~ spl20_16 | ~ spl20_27 | ~ spl20_28 | ~ spl20_29 | ~ spl20_59), inference(avatar_split_clause, [], [f501, f373, f224, f219, f214, f159, f506, f503])).
fof(f159, plain, (spl20_16 <=> in(sK3, sK2)), introduced(avatar_definition, [new_symbols(naming, [spl20_16])])).
fof(f214, plain, (spl20_27 <=> event(sK3)), introduced(avatar_definition, [new_symbols(naming, [spl20_27])])).
fof(f219, plain, (spl20_28 <=> city(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl20_28])])).
fof(f224, plain, (spl20_29 <=> hollywood(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl20_29])])).
fof(f373, plain, (spl20_59 <=> ! [X9, X11, X10, X12] : (~ in(X10, X9) | ~ hollywood(X9) | ~ city(X9) | ~ event(X10) | ~ chevy(X11) | ~ car(X11) | ~ white(X11) | ~ dirty(X11) | ~ old(X11) | ~ street(X12) | ~ way(X12) | ~ lonely(X12) | ~ barrel(X10, X11) | ~ down(X10, X12))), introduced(avatar_definition, [new_symbols(naming, [spl20_59])])).
fof(f501, plain, (! [X0, X1] : (~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl20_16 | ~ spl20_27 | ~ spl20_28 | ~ spl20_29 | ~ spl20_59)), inference(subsumption_resolution, [], [f500, f216])).
fof(f216, plain, (event(sK3) | ~ spl20_27), inference(avatar_component_clause, [], [f214])).
fof(f500, plain, (! [X0, X1] : (~ event(sK3) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl20_16 | ~ spl20_28 | ~ spl20_29 | ~ spl20_59)), inference(subsumption_resolution, [], [f499, f221])).
fof(f221, plain, (city(sK2) | ~ spl20_28), inference(avatar_component_clause, [], [f219])).
fof(f499, plain, (! [X0, X1] : (~ city(sK2) | ~ event(sK3) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl20_16 | ~ spl20_29 | ~ spl20_59)), inference(subsumption_resolution, [], [f498, f226])).
fof(f226, plain, (hollywood(sK2) | ~ spl20_29), inference(avatar_component_clause, [], [f224])).
fof(f498, plain, (! [X0, X1] : (~ hollywood(sK2) | ~ city(sK2) | ~ event(sK3) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK3, X0) | ~ down(sK3, X1)) | (~ spl20_16 | ~ spl20_59)), inference(resolution, [], [f161, f374])).
fof(f374, plain, (! [X12, X10, X11, X9] : (~ in(X10, X9) | ~ hollywood(X9) | ~ city(X9) | ~ event(X10) | ~ chevy(X11) | ~ car(X11) | ~ white(X11) | ~ dirty(X11) | ~ old(X11) | ~ street(X12) | ~ way(X12) | ~ lonely(X12) | ~ barrel(X10, X11) | ~ down(X10, X12)) | ~ spl20_59), inference(avatar_component_clause, [], [f373])).
fof(f161, plain, (in(sK3, sK2) | ~ spl20_16), inference(avatar_component_clause, [], [f159])).
fof(f495, plain, (~ spl20_47 | ~ spl20_48 | ~ spl20_49 | ~ spl20_50 | ~ spl20_51 | ~ spl20_52 | ~ spl20_62), inference(avatar_contradiction_clause, [], [f494])).
fof(f494, plain, ($false | (~ spl20_47 | ~ spl20_48 | ~ spl20_49 | ~ spl20_50 | ~ spl20_51 | ~ spl20_52 | ~ spl20_62)), inference(subsumption_resolution, [], [f493, f335])).
fof(f335, plain, (car(sK14) | ~ spl20_51), inference(avatar_component_clause, [], [f333])).
fof(f333, plain, (spl20_51 <=> car(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl20_51])])).
fof(f493, plain, (~ car(sK14) | (~ spl20_47 | ~ spl20_48 | ~ spl20_49 | ~ spl20_50 | ~ spl20_52 | ~ spl20_62)), inference(subsumption_resolution, [], [f492, f330])).
fof(f330, plain, (white(sK14) | ~ spl20_50), inference(avatar_component_clause, [], [f328])).
fof(f328, plain, (spl20_50 <=> white(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl20_50])])).
fof(f492, plain, (~ white(sK14) | ~ car(sK14) | (~ spl20_47 | ~ spl20_48 | ~ spl20_49 | ~ spl20_52 | ~ spl20_62)), inference(subsumption_resolution, [], [f491, f325])).
fof(f325, plain, (dirty(sK14) | ~ spl20_49), inference(avatar_component_clause, [], [f323])).
fof(f323, plain, (spl20_49 <=> dirty(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl20_49])])).
fof(f491, plain, (~ dirty(sK14) | ~ white(sK14) | ~ car(sK14) | (~ spl20_47 | ~ spl20_48 | ~ spl20_52 | ~ spl20_62)), inference(subsumption_resolution, [], [f490, f320])).
fof(f320, plain, (old(sK14) | ~ spl20_48), inference(avatar_component_clause, [], [f318])).
fof(f318, plain, (spl20_48 <=> old(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl20_48])])).
fof(f490, plain, (~ old(sK14) | ~ dirty(sK14) | ~ white(sK14) | ~ car(sK14) | (~ spl20_47 | ~ spl20_52 | ~ spl20_62)), inference(subsumption_resolution, [], [f489, f340])).
fof(f340, plain, (chevy(sK14) | ~ spl20_52), inference(avatar_component_clause, [], [f338])).
fof(f338, plain, (spl20_52 <=> chevy(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl20_52])])).
fof(f489, plain, (~ chevy(sK14) | ~ old(sK14) | ~ dirty(sK14) | ~ white(sK14) | ~ car(sK14) | (~ spl20_47 | ~ spl20_62)), inference(resolution, [], [f395, f315])).
fof(f315, plain, (barrel(sK12, sK14) | ~ spl20_47), inference(avatar_component_clause, [], [f313])).
fof(f313, plain, (spl20_47 <=> barrel(sK12, sK14)), introduced(avatar_definition, [new_symbols(naming, [spl20_47])])).
fof(f395, plain, (! [X0] : (~ barrel(sK12, X0) | ~ chevy(X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0)) | ~ spl20_62), inference(avatar_component_clause, [], [f394])).
fof(f394, plain, (spl20_62 <=> ! [X0] : (~ chevy(X0) | ~ barrel(sK12, X0) | ~ old(X0) | ~ dirty(X0) | ~ white(X0) | ~ car(X0))), introduced(avatar_definition, [new_symbols(naming, [spl20_62])])).
fof(f488, plain, (spl20_61 | spl20_62 | ~ spl20_45 | ~ spl20_56 | ~ spl20_57 | ~ spl20_58 | ~ spl20_59), inference(avatar_split_clause, [], [f487, f373, f368, f363, f358, f303, f394, f391])).
fof(f391, plain, (spl20_61 <=> ! [X1] : (~ street(X1) | ~ down(sK12, X1) | ~ lonely(X1) | ~ way(X1))), introduced(avatar_definition, [new_symbols(naming, [spl20_61])])).
fof(f303, plain, (spl20_45 <=> in(sK12, sK11)), introduced(avatar_definition, [new_symbols(naming, [spl20_45])])).
fof(f358, plain, (spl20_56 <=> event(sK12)), introduced(avatar_definition, [new_symbols(naming, [spl20_56])])).
fof(f363, plain, (spl20_57 <=> city(sK11)), introduced(avatar_definition, [new_symbols(naming, [spl20_57])])).
fof(f368, plain, (spl20_58 <=> hollywood(sK11)), introduced(avatar_definition, [new_symbols(naming, [spl20_58])])).
fof(f487, plain, (! [X0, X1] : (~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK12, X0) | ~ down(sK12, X1)) | (~ spl20_45 | ~ spl20_56 | ~ spl20_57 | ~ spl20_58 | ~ spl20_59)), inference(subsumption_resolution, [], [f486, f360])).
fof(f360, plain, (event(sK12) | ~ spl20_56), inference(avatar_component_clause, [], [f358])).
fof(f486, plain, (! [X0, X1] : (~ event(sK12) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK12, X0) | ~ down(sK12, X1)) | (~ spl20_45 | ~ spl20_57 | ~ spl20_58 | ~ spl20_59)), inference(subsumption_resolution, [], [f485, f365])).
fof(f365, plain, (city(sK11) | ~ spl20_57), inference(avatar_component_clause, [], [f363])).
fof(f485, plain, (! [X0, X1] : (~ city(sK11) | ~ event(sK12) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK12, X0) | ~ down(sK12, X1)) | (~ spl20_45 | ~ spl20_58 | ~ spl20_59)), inference(subsumption_resolution, [], [f477, f370])).
fof(f370, plain, (hollywood(sK11) | ~ spl20_58), inference(avatar_component_clause, [], [f368])).
fof(f477, plain, (! [X0, X1] : (~ hollywood(sK11) | ~ city(sK11) | ~ event(sK12) | ~ chevy(X0) | ~ car(X0) | ~ white(X0) | ~ dirty(X0) | ~ old(X0) | ~ street(X1) | ~ way(X1) | ~ lonely(X1) | ~ barrel(sK12, X0) | ~ down(sK12, X1)) | (~ spl20_45 | ~ spl20_59)), inference(resolution, [], [f374, f305])).
fof(f305, plain, (in(sK12, sK11) | ~ spl20_45), inference(avatar_component_clause, [], [f303])).
fof(f484, plain, (~ spl20_46 | ~ spl20_53 | ~ spl20_54 | ~ spl20_55 | ~ spl20_61), inference(avatar_contradiction_clause, [], [f483])).
fof(f483, plain, ($false | (~ spl20_46 | ~ spl20_53 | ~ spl20_54 | ~ spl20_55 | ~ spl20_61)), inference(subsumption_resolution, [], [f482, f350])).
fof(f350, plain, (way(sK13) | ~ spl20_54), inference(avatar_component_clause, [], [f348])).
fof(f348, plain, (spl20_54 <=> way(sK13)), introduced(avatar_definition, [new_symbols(naming, [spl20_54])])).
fof(f482, plain, (~ way(sK13) | (~ spl20_46 | ~ spl20_53 | ~ spl20_55 | ~ spl20_61)), inference(subsumption_resolution, [], [f481, f345])).
fof(f345, plain, (lonely(sK13) | ~ spl20_53), inference(avatar_component_clause, [], [f343])).
fof(f343, plain, (spl20_53 <=> lonely(sK13)), introduced(avatar_definition, [new_symbols(naming, [spl20_53])])).
fof(f481, plain, (~ lonely(sK13) | ~ way(sK13) | (~ spl20_46 | ~ spl20_55 | ~ spl20_61)), inference(subsumption_resolution, [], [f480, f355])).
fof(f355, plain, (street(sK13) | ~ spl20_55), inference(avatar_component_clause, [], [f353])).
fof(f353, plain, (spl20_55 <=> street(sK13)), introduced(avatar_definition, [new_symbols(naming, [spl20_55])])).
fof(f480, plain, (~ street(sK13) | ~ lonely(sK13) | ~ way(sK13) | (~ spl20_46 | ~ spl20_61)), inference(resolution, [], [f392, f310])).
fof(f310, plain, (down(sK12, sK13) | ~ spl20_46), inference(avatar_component_clause, [], [f308])).
fof(f308, plain, (spl20_46 <=> down(sK12, sK13)), introduced(avatar_definition, [new_symbols(naming, [spl20_46])])).
fof(f392, plain, (! [X1] : (~ down(sK12, X1) | ~ street(X1) | ~ lonely(X1) | ~ way(X1)) | ~ spl20_61), inference(avatar_component_clause, [], [f391])).
fof(f476, plain, (~ spl20_31 | ~ spl20_32 | ~ spl20_33 | ~ spl20_34 | ~ spl20_35 | ~ spl20_36 | ~ spl20_37 | ~ spl20_38 | ~ spl20_39 | ~ spl20_40 | spl20_41 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60), inference(avatar_contradiction_clause, [], [f475])).
fof(f475, plain, ($false | (~ spl20_31 | ~ spl20_32 | ~ spl20_33 | ~ spl20_34 | ~ spl20_35 | ~ spl20_36 | ~ spl20_37 | ~ spl20_38 | ~ spl20_39 | ~ spl20_40 | spl20_41 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f474, f285])).
fof(f285, plain, (~ (sK15 = sK16) | spl20_41), inference(avatar_component_clause, [], [f283])).
fof(f283, plain, (spl20_41 <=> (sK15 = sK16)), introduced(avatar_definition, [new_symbols(naming, [spl20_41])])).
fof(f474, plain, ((sK15 = sK16) | (~ spl20_31 | ~ spl20_32 | ~ spl20_33 | ~ spl20_34 | ~ spl20_35 | ~ spl20_36 | ~ spl20_37 | ~ spl20_38 | ~ spl20_39 | ~ spl20_40 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f473, f255])).
fof(f255, plain, (young(sK16) | ~ spl20_35), inference(avatar_component_clause, [], [f253])).
fof(f253, plain, (spl20_35 <=> young(sK16)), introduced(avatar_definition, [new_symbols(naming, [spl20_35])])).
fof(f473, plain, (~ young(sK16) | (sK15 = sK16) | (~ spl20_31 | ~ spl20_32 | ~ spl20_33 | ~ spl20_34 | ~ spl20_36 | ~ spl20_37 | ~ spl20_38 | ~ spl20_39 | ~ spl20_40 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f472, f260])).
fof(f260, plain, (man(sK16) | ~ spl20_36), inference(avatar_component_clause, [], [f258])).
fof(f258, plain, (spl20_36 <=> man(sK16)), introduced(avatar_definition, [new_symbols(naming, [spl20_36])])).
fof(f472, plain, (~ man(sK16) | ~ young(sK16) | (sK15 = sK16) | (~ spl20_31 | ~ spl20_32 | ~ spl20_33 | ~ spl20_34 | ~ spl20_37 | ~ spl20_38 | ~ spl20_39 | ~ spl20_40 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f471, f265])).
fof(f265, plain, (fellow(sK16) | ~ spl20_37), inference(avatar_component_clause, [], [f263])).
fof(f263, plain, (spl20_37 <=> fellow(sK16)), introduced(avatar_definition, [new_symbols(naming, [spl20_37])])).
fof(f471, plain, (~ fellow(sK16) | ~ man(sK16) | ~ young(sK16) | (sK15 = sK16) | (~ spl20_31 | ~ spl20_32 | ~ spl20_33 | ~ spl20_34 | ~ spl20_38 | ~ spl20_39 | ~ spl20_40 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(resolution, [], [f463, f383])).
fof(f383, plain, (in(sK16, sK17) | (~ spl20_31 | ~ spl20_32)), inference(forward_demodulation, [], [f235, f240])).
fof(f240, plain, ((sK16 = sK19) | ~ spl20_32), inference(avatar_component_clause, [], [f238])).
fof(f238, plain, (spl20_32 <=> (sK16 = sK19)), introduced(avatar_definition, [new_symbols(naming, [spl20_32])])).
fof(f235, plain, (in(sK19, sK17) | ~ spl20_31), inference(avatar_component_clause, [], [f233])).
fof(f233, plain, (spl20_31 <=> in(sK19, sK17)), introduced(avatar_definition, [new_symbols(naming, [spl20_31])])).
fof(f463, plain, (! [X1] : (~ in(X1, sK17) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | (sK15 = X1)) | (~ spl20_33 | ~ spl20_34 | ~ spl20_38 | ~ spl20_39 | ~ spl20_40 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f462, f270])).
fof(f270, plain, (young(sK15) | ~ spl20_38), inference(avatar_component_clause, [], [f268])).
fof(f268, plain, (spl20_38 <=> young(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl20_38])])).
fof(f462, plain, (! [X1] : ((sK15 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ young(sK15) | ~ in(X1, sK17)) | (~ spl20_33 | ~ spl20_34 | ~ spl20_39 | ~ spl20_40 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f461, f275])).
fof(f275, plain, (man(sK15) | ~ spl20_39), inference(avatar_component_clause, [], [f273])).
fof(f273, plain, (spl20_39 <=> man(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl20_39])])).
fof(f461, plain, (! [X1] : ((sK15 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ man(sK15) | ~ young(sK15) | ~ in(X1, sK17)) | (~ spl20_33 | ~ spl20_34 | ~ spl20_40 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f460, f280])).
fof(f280, plain, (fellow(sK15) | ~ spl20_40), inference(avatar_component_clause, [], [f278])).
fof(f278, plain, (spl20_40 <=> fellow(sK15)), introduced(avatar_definition, [new_symbols(naming, [spl20_40])])).
fof(f460, plain, (! [X1] : ((sK15 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK15) | ~ man(sK15) | ~ young(sK15) | ~ in(X1, sK17)) | (~ spl20_33 | ~ spl20_34 | ~ spl20_42 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f459, f290])).
fof(f290, plain, (front(sK17) | ~ spl20_42), inference(avatar_component_clause, [], [f288])).
fof(f288, plain, (spl20_42 <=> front(sK17)), introduced(avatar_definition, [new_symbols(naming, [spl20_42])])).
fof(f459, plain, (! [X1] : (~ front(sK17) | (sK15 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK15) | ~ man(sK15) | ~ young(sK15) | ~ in(X1, sK17)) | (~ spl20_33 | ~ spl20_34 | ~ spl20_43 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f458, f295])).
fof(f295, plain, (furniture(sK17) | ~ spl20_43), inference(avatar_component_clause, [], [f293])).
fof(f293, plain, (spl20_43 <=> furniture(sK17)), introduced(avatar_definition, [new_symbols(naming, [spl20_43])])).
fof(f458, plain, (! [X1] : (~ furniture(sK17) | ~ front(sK17) | (sK15 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK15) | ~ man(sK15) | ~ young(sK15) | ~ in(X1, sK17)) | (~ spl20_33 | ~ spl20_34 | ~ spl20_44 | ~ spl20_60)), inference(subsumption_resolution, [], [f428, f300])).
fof(f300, plain, (seat(sK17) | ~ spl20_44), inference(avatar_component_clause, [], [f298])).
fof(f298, plain, (spl20_44 <=> seat(sK17)), introduced(avatar_definition, [new_symbols(naming, [spl20_44])])).
fof(f428, plain, (! [X1] : (~ seat(sK17) | ~ furniture(sK17) | ~ front(sK17) | (sK15 = X1) | ~ fellow(X1) | ~ man(X1) | ~ young(X1) | ~ fellow(sK15) | ~ man(sK15) | ~ young(sK15) | ~ in(X1, sK17)) | (~ spl20_33 | ~ spl20_34 | ~ spl20_60)), inference(resolution, [], [f377, f382])).
fof(f382, plain, (in(sK15, sK17) | (~ spl20_33 | ~ spl20_34)), inference(forward_demodulation, [], [f245, f250])).
fof(f250, plain, ((sK15 = sK18) | ~ spl20_34), inference(avatar_component_clause, [], [f248])).
fof(f248, plain, (spl20_34 <=> (sK15 = sK18)), introduced(avatar_definition, [new_symbols(naming, [spl20_34])])).
fof(f245, plain, (in(sK18, sK17) | ~ spl20_33), inference(avatar_component_clause, [], [f243])).
fof(f243, plain, (spl20_33 <=> in(sK18, sK17)), introduced(avatar_definition, [new_symbols(naming, [spl20_33])])).
fof(f381, plain, (spl20_30 | spl20_1), inference(avatar_split_clause, [], [f72, f85, f229])).
fof(f229, plain, (spl20_30 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl20_30])])).
fof(f85, plain, (spl20_1 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl20_1])])).
fof(f72, plain, (sP1 | sP0), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((! [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (~ in(X8, X6) | ~ (X5 = X8) | ~ in(X7, X6) | ~ (X4 = X7) | ~ young(X5) | ~ man(X5) | ~ fellow(X5) | ~ young(X4) | ~ man(X4) | ~ fellow(X4) | (X4 = X5) | ~ front(X6) | ~ furniture(X6) | ~ seat(X6) | ~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0)) & sP1) | (! [X9, X10, X11, X12, X13, X14, X15, X16, X17] : (~ in(X17, X15) | ~ (X14 = X17) | ~ in(X16, X15) | ~ (X13 = X16) | ~ young(X14) | ~ man(X14) | ~ fellow(X14) | ~ young(X13) | ~ man(X13) | ~ fellow(X13) | (X13 = X14) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X12) | ~ barrel(X10, X11) | ~ lonely(X12) | ~ way(X12) | ~ street(X12) | ~ old(X11) | ~ dirty(X11) | ~ white(X11) | ~ car(X11) | ~ chevy(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)) & sP0)), inference(rectify, [], [f7])).
fof(f7, plain, ((! [X9, X10, X11, X12, X13, X14, X15, X16, X17] : (~ in(X17, X15) | ~ (X14 = X17) | ~ in(X16, X15) | ~ (X13 = X16) | ~ young(X14) | ~ man(X14) | ~ fellow(X14) | ~ young(X13) | ~ man(X13) | ~ fellow(X13) | (X13 = X14) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X11) | ~ barrel(X10, X12) | ~ old(X12) | ~ dirty(X12) | ~ white(X12) | ~ car(X12) | ~ chevy(X12) | ~ lonely(X11) | ~ way(X11) | ~ street(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)) & sP1) | (! [X27, X28, X29, X30, X31, X32, X33, X34, X35] : (~ in(X35, X33) | ~ (X32 = X35) | ~ in(X34, X33) | ~ (X31 = X34) | ~ young(X32) | ~ man(X32) | ~ fellow(X32) | ~ young(X31) | ~ man(X31) | ~ fellow(X31) | (X31 = X32) | ~ front(X33) | ~ furniture(X33) | ~ seat(X33) | ~ in(X28, X27) | ~ down(X28, X30) | ~ barrel(X28, X29) | ~ lonely(X30) | ~ way(X30) | ~ street(X30) | ~ old(X29) | ~ dirty(X29) | ~ white(X29) | ~ car(X29) | ~ chevy(X29) | ~ event(X28) | ~ city(X27) | ~ hollywood(X27)) & sP0)), inference(definition_folding, [], [f4, e6, e5])).
fof(f5, plain, (? [X18, X19, X20, X21, X22, X23, X24, X25, X26] : (in(X26, X24) & (X23 = X26) & in(X25, X24) & (X22 = X25) & young(X23) & man(X23) & fellow(X23) & young(X22) & man(X22) & fellow(X22) & ~ (X22 = X23) & front(X24) & furniture(X24) & seat(X24) & in(X19, X18) & down(X19, X20) & barrel(X19, X21) & old(X21) & dirty(X21) & white(X21) & car(X21) & chevy(X21) & lonely(X20) & way(X20) & street(X20) & event(X19) & city(X18) & hollywood(X18)) | ~ sP0), inference(usedef, [], [e5])).
fof(e5, plain, (sP0 <=> ? [X18, X19, X20, X21, X22, X23, X24, X25, X26] : (in(X26, X24) & (X23 = X26) & in(X25, X24) & (X22 = X25) & young(X23) & man(X23) & fellow(X23) & young(X22) & man(X22) & fellow(X22) & ~ (X22 = X23) & front(X24) & furniture(X24) & seat(X24) & in(X19, X18) & down(X19, X20) & barrel(X19, X21) & old(X21) & dirty(X21) & white(X21) & car(X21) & chevy(X21) & lonely(X20) & way(X20) & street(X20) & event(X19) & city(X18) & hollywood(X18))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f6, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) | ~ sP1), inference(usedef, [], [e6])).
fof(e6, plain, (sP1 <=> ? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f4, plain, ((! [X9, X10, X11, X12, X13, X14, X15, X16, X17] : (~ in(X17, X15) | ~ (X14 = X17) | ~ in(X16, X15) | ~ (X13 = X16) | ~ young(X14) | ~ man(X14) | ~ fellow(X14) | ~ young(X13) | ~ man(X13) | ~ fellow(X13) | (X13 = X14) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X11) | ~ barrel(X10, X12) | ~ old(X12) | ~ dirty(X12) | ~ white(X12) | ~ car(X12) | ~ chevy(X12) | ~ lonely(X11) | ~ way(X11) | ~ street(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)) & ? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0))) | (! [X27, X28, X29, X30, X31, X32, X33, X34, X35] : (~ in(X35, X33) | ~ (X32 = X35) | ~ in(X34, X33) | ~ (X31 = X34) | ~ young(X32) | ~ man(X32) | ~ fellow(X32) | ~ young(X31) | ~ man(X31) | ~ fellow(X31) | (X31 = X32) | ~ front(X33) | ~ furniture(X33) | ~ seat(X33) | ~ in(X28, X27) | ~ down(X28, X30) | ~ barrel(X28, X29) | ~ lonely(X30) | ~ way(X30) | ~ street(X30) | ~ old(X29) | ~ dirty(X29) | ~ white(X29) | ~ car(X29) | ~ chevy(X29) | ~ event(X28) | ~ city(X27) | ~ hollywood(X27)) & ? [X18, X19, X20, X21, X22, X23, X24, X25, X26] : (in(X26, X24) & (X23 = X26) & in(X25, X24) & (X22 = X25) & young(X23) & man(X23) & fellow(X23) & young(X22) & man(X22) & fellow(X22) & ~ (X22 = X23) & front(X24) & furniture(X24) & seat(X24) & in(X19, X18) & down(X19, X20) & barrel(X19, X21) & old(X21) & dirty(X21) & white(X21) & car(X21) & chevy(X21) & lonely(X20) & way(X20) & street(X20) & event(X19) & city(X18) & hollywood(X18)))), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ~ ((? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) => ? [X9, X10, X11, X12, X13, X14, X15, X16, X17] : (in(X17, X15) & (X14 = X17) & in(X16, X15) & (X13 = X16) & young(X14) & man(X14) & fellow(X14) & young(X13) & man(X13) & fellow(X13) & ~ (X13 = X14) & front(X15) & furniture(X15) & seat(X15) & in(X10, X9) & down(X10, X11) & barrel(X10, X12) & old(X12) & dirty(X12) & white(X12) & car(X12) & chevy(X12) & lonely(X11) & way(X11) & street(X11) & event(X10) & city(X9) & hollywood(X9))) & (? [X18, X19, X20, X21, X22, X23, X24, X25, X26] : (in(X26, X24) & (X23 = X26) & in(X25, X24) & (X22 = X25) & young(X23) & man(X23) & fellow(X23) & young(X22) & man(X22) & fellow(X22) & ~ (X22 = X23) & front(X24) & furniture(X24) & seat(X24) & in(X19, X18) & down(X19, X20) & barrel(X19, X21) & old(X21) & dirty(X21) & white(X21) & car(X21) & chevy(X21) & lonely(X20) & way(X20) & street(X20) & event(X19) & city(X18) & hollywood(X18)) => ? [X27, X28, X29, X30, X31, X32, X33, X34, X35] : (in(X35, X33) & (X32 = X35) & in(X34, X33) & (X31 = X34) & young(X32) & man(X32) & fellow(X32) & young(X31) & man(X31) & fellow(X31) & ~ (X31 = X32) & front(X33) & furniture(X33) & seat(X33) & in(X28, X27) & down(X28, X30) & barrel(X28, X29) & lonely(X30) & way(X30) & street(X30) & old(X29) & dirty(X29) & white(X29) & car(X29) & chevy(X29) & event(X28) & city(X27) & hollywood(X27)))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ((? [X18, X19, X20, X21, X22, X23, X24, X25, X26] : (in(X26, X24) & (X23 = X26) & in(X25, X24) & (X22 = X25) & young(X23) & man(X23) & fellow(X23) & young(X22) & man(X22) & fellow(X22) & ~ (X22 = X23) & front(X24) & furniture(X24) & seat(X24) & in(X19, X18) & down(X19, X21) & barrel(X19, X20) & lonely(X21) & way(X21) & street(X21) & old(X20) & dirty(X20) & white(X20) & car(X20) & chevy(X20) & event(X19) & city(X18) & hollywood(X18)) => ? [X27, X28, X29, X30, X31, X32, X33, X34, X35] : (in(X35, X33) & (X32 = X35) & in(X34, X33) & (X31 = X34) & young(X32) & man(X32) & fellow(X32) & young(X31) & man(X31) & fellow(X31) & ~ (X31 = X32) & front(X33) & furniture(X33) & seat(X33) & in(X28, X27) & down(X28, X29) & barrel(X28, X30) & old(X30) & dirty(X30) & white(X30) & car(X30) & chevy(X30) & lonely(X29) & way(X29) & street(X29) & event(X28) & city(X27) & hollywood(X27))) & (? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) => ? [X9, X10, X11, X12, X13, X14, X15, X16, X17] : (in(X17, X15) & (X14 = X17) & in(X16, X15) & (X13 = X16) & young(X14) & man(X14) & fellow(X14) & young(X13) & man(X13) & fellow(X13) & ~ (X13 = X14) & front(X15) & furniture(X15) & seat(X15) & in(X10, X9) & down(X10, X12) & barrel(X10, X11) & lonely(X12) & way(X12) & street(X12) & old(X11) & dirty(X11) & white(X11) & car(X11) & chevy(X11) & event(X10) & city(X9) & hollywood(X9)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ((? [X18, X19, X20, X21, X22, X23, X24, X25, X26] : (in(X26, X24) & (X23 = X26) & in(X25, X24) & (X22 = X25) & young(X23) & man(X23) & fellow(X23) & young(X22) & man(X22) & fellow(X22) & ~ (X22 = X23) & front(X24) & furniture(X24) & seat(X24) & in(X19, X18) & down(X19, X21) & barrel(X19, X20) & lonely(X21) & way(X21) & street(X21) & old(X20) & dirty(X20) & white(X20) & car(X20) & chevy(X20) & event(X19) & city(X18) & hollywood(X18)) => ? [X27, X28, X29, X30, X31, X32, X33, X34, X35] : (in(X35, X33) & (X32 = X35) & in(X34, X33) & (X31 = X34) & young(X32) & man(X32) & fellow(X32) & young(X31) & man(X31) & fellow(X31) & ~ (X31 = X32) & front(X33) & furniture(X33) & seat(X33) & in(X28, X27) & down(X28, X29) & barrel(X28, X30) & old(X30) & dirty(X30) & white(X30) & car(X30) & chevy(X30) & lonely(X29) & way(X29) & street(X29) & event(X28) & city(X27) & hollywood(X27))) & (? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) => ? [X9, X10, X11, X12, X13, X14, X15, X16, X17] : (in(X17, X15) & (X14 = X17) & in(X16, X15) & (X13 = X16) & young(X14) & man(X14) & fellow(X14) & young(X13) & man(X13) & fellow(X13) & ~ (X13 = X14) & front(X15) & furniture(X15) & seat(X15) & in(X10, X9) & down(X10, X12) & barrel(X10, X11) & lonely(X12) & way(X12) & street(X12) & old(X11) & dirty(X11) & white(X11) & car(X11) & chevy(X11) & event(X10) & city(X9) & hollywood(X9)))), file('/home/ubuntu/library/tptp/Problems/NLP/NLP009+1.p', co1)).
fof(f378, plain, (spl20_59 | spl20_60 | spl20_59 | spl20_60), inference(avatar_split_clause, [], [f79, f376, f373, f376, f373])).
fof(f79, plain, ! [X6, X2, X0, X12, X10, X8, X17, X7, X3, X1, X15, X11, X9, X16] : (~ in(X8, X6) | ~ in(X7, X6) | ~ young(X8) | ~ man(X8) | ~ fellow(X8) | ~ young(X7) | ~ man(X7) | ~ fellow(X7) | (X7 = X8) | ~ front(X6) | ~ furniture(X6) | ~ seat(X6) | ~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0) | ~ in(X17, X15) | ~ in(X16, X15) | ~ young(X17) | ~ man(X17) | ~ fellow(X17) | ~ young(X16) | ~ man(X16) | ~ fellow(X16) | (X16 = X17) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X12) | ~ barrel(X10, X11) | ~ lonely(X12) | ~ way(X12) | ~ street(X12) | ~ old(X11) | ~ dirty(X11) | ~ white(X11) | ~ car(X11) | ~ chevy(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)), inference(equality_resolution, [], [f78])).
fof(f78, plain, ! [X6, X2, X0, X12, X10, X8, X17, X7, X3, X1, X15, X13, X11, X9, X16] : (~ in(X8, X6) | ~ in(X7, X6) | ~ young(X8) | ~ man(X8) | ~ fellow(X8) | ~ young(X7) | ~ man(X7) | ~ fellow(X7) | (X7 = X8) | ~ front(X6) | ~ furniture(X6) | ~ seat(X6) | ~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0) | ~ in(X17, X15) | ~ in(X16, X15) | ~ (X13 = X16) | ~ young(X17) | ~ man(X17) | ~ fellow(X17) | ~ young(X13) | ~ man(X13) | ~ fellow(X13) | (X13 = X17) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X12) | ~ barrel(X10, X11) | ~ lonely(X12) | ~ way(X12) | ~ street(X12) | ~ old(X11) | ~ dirty(X11) | ~ white(X11) | ~ car(X11) | ~ chevy(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)), inference(equality_resolution, [], [f77])).
fof(f77, plain, ! [X6, X2, X0, X14, X12, X10, X8, X17, X7, X3, X1, X15, X13, X11, X9, X16] : (~ in(X8, X6) | ~ in(X7, X6) | ~ young(X8) | ~ man(X8) | ~ fellow(X8) | ~ young(X7) | ~ man(X7) | ~ fellow(X7) | (X7 = X8) | ~ front(X6) | ~ furniture(X6) | ~ seat(X6) | ~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0) | ~ in(X17, X15) | ~ (X14 = X17) | ~ in(X16, X15) | ~ (X13 = X16) | ~ young(X14) | ~ man(X14) | ~ fellow(X14) | ~ young(X13) | ~ man(X13) | ~ fellow(X13) | (X13 = X14) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X12) | ~ barrel(X10, X11) | ~ lonely(X12) | ~ way(X12) | ~ street(X12) | ~ old(X11) | ~ dirty(X11) | ~ white(X11) | ~ car(X11) | ~ chevy(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)), inference(equality_resolution, [], [f76])).
fof(f76, plain, ! [X6, X4, X2, X0, X14, X12, X10, X8, X17, X7, X3, X1, X15, X13, X11, X9, X16] : (~ in(X8, X6) | ~ in(X7, X6) | ~ (X4 = X7) | ~ young(X8) | ~ man(X8) | ~ fellow(X8) | ~ young(X4) | ~ man(X4) | ~ fellow(X4) | (X4 = X8) | ~ front(X6) | ~ furniture(X6) | ~ seat(X6) | ~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0) | ~ in(X17, X15) | ~ (X14 = X17) | ~ in(X16, X15) | ~ (X13 = X16) | ~ young(X14) | ~ man(X14) | ~ fellow(X14) | ~ young(X13) | ~ man(X13) | ~ fellow(X13) | (X13 = X14) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X12) | ~ barrel(X10, X11) | ~ lonely(X12) | ~ way(X12) | ~ street(X12) | ~ old(X11) | ~ dirty(X11) | ~ white(X11) | ~ car(X11) | ~ chevy(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)), inference(equality_resolution, [], [f75])).
fof(f75, plain, ! [X6, X4, X2, X0, X14, X12, X10, X8, X17, X7, X5, X3, X1, X15, X13, X11, X9, X16] : (~ in(X8, X6) | ~ (X5 = X8) | ~ in(X7, X6) | ~ (X4 = X7) | ~ young(X5) | ~ man(X5) | ~ fellow(X5) | ~ young(X4) | ~ man(X4) | ~ fellow(X4) | (X4 = X5) | ~ front(X6) | ~ furniture(X6) | ~ seat(X6) | ~ in(X1, X0) | ~ down(X1, X2) | ~ barrel(X1, X3) | ~ old(X3) | ~ dirty(X3) | ~ white(X3) | ~ car(X3) | ~ chevy(X3) | ~ lonely(X2) | ~ way(X2) | ~ street(X2) | ~ event(X1) | ~ city(X0) | ~ hollywood(X0) | ~ in(X17, X15) | ~ (X14 = X17) | ~ in(X16, X15) | ~ (X13 = X16) | ~ young(X14) | ~ man(X14) | ~ fellow(X14) | ~ young(X13) | ~ man(X13) | ~ fellow(X13) | (X13 = X14) | ~ front(X15) | ~ furniture(X15) | ~ seat(X15) | ~ in(X10, X9) | ~ down(X10, X12) | ~ barrel(X10, X11) | ~ lonely(X12) | ~ way(X12) | ~ street(X12) | ~ old(X11) | ~ dirty(X11) | ~ white(X11) | ~ car(X11) | ~ chevy(X11) | ~ event(X10) | ~ city(X9) | ~ hollywood(X9)), inference(cnf_transformation, [], [f15])).
fof(f371, plain, (~ spl20_30 | spl20_58), inference(avatar_split_clause, [], [f44, f368, f229])).
fof(f44, plain, (hollywood(sK11) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((in(sK19, sK17) & (sK16 = sK19) & in(sK18, sK17) & (sK15 = sK18) & young(sK16) & man(sK16) & fellow(sK16) & young(sK15) & man(sK15) & fellow(sK15) & ~ (sK15 = sK16) & front(sK17) & furniture(sK17) & seat(sK17) & in(sK12, sK11) & down(sK12, sK13) & barrel(sK12, sK14) & old(sK14) & dirty(sK14) & white(sK14) & car(sK14) & chevy(sK14) & lonely(sK13) & way(sK13) & street(sK13) & event(sK12) & city(sK11) & hollywood(sK11)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11, sK12, sK13, sK14, sK15, sK16, sK17, sK18, sK19])], [f12, f13])).
fof(f13, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) => (in(sK19, sK17) & (sK16 = sK19) & in(sK18, sK17) & (sK15 = sK18) & young(sK16) & man(sK16) & fellow(sK16) & young(sK15) & man(sK15) & fellow(sK15) & ~ (sK15 = sK16) & front(sK17) & furniture(sK17) & seat(sK17) & in(sK12, sK11) & down(sK12, sK13) & barrel(sK12, sK14) & old(sK14) & dirty(sK14) & white(sK14) & car(sK14) & chevy(sK14) & lonely(sK13) & way(sK13) & street(sK13) & event(sK12) & city(sK11) & hollywood(sK11))), introduced(choice_axiom, [])).
fof(f12, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X2) & barrel(X1, X3) & old(X3) & dirty(X3) & white(X3) & car(X3) & chevy(X3) & lonely(X2) & way(X2) & street(X2) & event(X1) & city(X0) & hollywood(X0)) | ~ sP0), inference(rectify, [], [f11])).
fof(f11, plain, (? [X18, X19, X20, X21, X22, X23, X24, X25, X26] : (in(X26, X24) & (X23 = X26) & in(X25, X24) & (X22 = X25) & young(X23) & man(X23) & fellow(X23) & young(X22) & man(X22) & fellow(X22) & ~ (X22 = X23) & front(X24) & furniture(X24) & seat(X24) & in(X19, X18) & down(X19, X20) & barrel(X19, X21) & old(X21) & dirty(X21) & white(X21) & car(X21) & chevy(X21) & lonely(X20) & way(X20) & street(X20) & event(X19) & city(X18) & hollywood(X18)) | ~ sP0), inference(nnf_transformation, [], [f5])).
fof(f366, plain, (~ spl20_30 | spl20_57), inference(avatar_split_clause, [], [f45, f363, f229])).
fof(f45, plain, (city(sK11) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f361, plain, (~ spl20_30 | spl20_56), inference(avatar_split_clause, [], [f46, f358, f229])).
fof(f46, plain, (event(sK12) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f356, plain, (~ spl20_30 | spl20_55), inference(avatar_split_clause, [], [f47, f353, f229])).
fof(f47, plain, (street(sK13) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f351, plain, (~ spl20_30 | spl20_54), inference(avatar_split_clause, [], [f48, f348, f229])).
fof(f48, plain, (way(sK13) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f346, plain, (~ spl20_30 | spl20_53), inference(avatar_split_clause, [], [f49, f343, f229])).
fof(f49, plain, (lonely(sK13) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f341, plain, (~ spl20_30 | spl20_52), inference(avatar_split_clause, [], [f50, f338, f229])).
fof(f50, plain, (chevy(sK14) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f336, plain, (~ spl20_30 | spl20_51), inference(avatar_split_clause, [], [f51, f333, f229])).
fof(f51, plain, (car(sK14) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f331, plain, (~ spl20_30 | spl20_50), inference(avatar_split_clause, [], [f52, f328, f229])).
fof(f52, plain, (white(sK14) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f326, plain, (~ spl20_30 | spl20_49), inference(avatar_split_clause, [], [f53, f323, f229])).
fof(f53, plain, (dirty(sK14) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f321, plain, (~ spl20_30 | spl20_48), inference(avatar_split_clause, [], [f54, f318, f229])).
fof(f54, plain, (old(sK14) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f316, plain, (~ spl20_30 | spl20_47), inference(avatar_split_clause, [], [f55, f313, f229])).
fof(f55, plain, (barrel(sK12, sK14) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f311, plain, (~ spl20_30 | spl20_46), inference(avatar_split_clause, [], [f56, f308, f229])).
fof(f56, plain, (down(sK12, sK13) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f306, plain, (~ spl20_30 | spl20_45), inference(avatar_split_clause, [], [f57, f303, f229])).
fof(f57, plain, (in(sK12, sK11) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f301, plain, (~ spl20_30 | spl20_44), inference(avatar_split_clause, [], [f58, f298, f229])).
fof(f58, plain, (seat(sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f296, plain, (~ spl20_30 | spl20_43), inference(avatar_split_clause, [], [f59, f293, f229])).
fof(f59, plain, (furniture(sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f291, plain, (~ spl20_30 | spl20_42), inference(avatar_split_clause, [], [f60, f288, f229])).
fof(f60, plain, (front(sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f286, plain, (~ spl20_30 | ~ spl20_41), inference(avatar_split_clause, [], [f61, f283, f229])).
fof(f61, plain, (~ (sK15 = sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f281, plain, (~ spl20_30 | spl20_40), inference(avatar_split_clause, [], [f62, f278, f229])).
fof(f62, plain, (fellow(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f276, plain, (~ spl20_30 | spl20_39), inference(avatar_split_clause, [], [f63, f273, f229])).
fof(f63, plain, (man(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f271, plain, (~ spl20_30 | spl20_38), inference(avatar_split_clause, [], [f64, f268, f229])).
fof(f64, plain, (young(sK15) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f266, plain, (~ spl20_30 | spl20_37), inference(avatar_split_clause, [], [f65, f263, f229])).
fof(f65, plain, (fellow(sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f261, plain, (~ spl20_30 | spl20_36), inference(avatar_split_clause, [], [f66, f258, f229])).
fof(f66, plain, (man(sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f256, plain, (~ spl20_30 | spl20_35), inference(avatar_split_clause, [], [f67, f253, f229])).
fof(f67, plain, (young(sK16) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f251, plain, (~ spl20_30 | spl20_34), inference(avatar_split_clause, [], [f68, f248, f229])).
fof(f68, plain, ((sK15 = sK18) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f246, plain, (~ spl20_30 | spl20_33), inference(avatar_split_clause, [], [f69, f243, f229])).
fof(f69, plain, (in(sK18, sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f241, plain, (~ spl20_30 | spl20_32), inference(avatar_split_clause, [], [f70, f238, f229])).
fof(f70, plain, ((sK16 = sK19) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f236, plain, (~ spl20_30 | spl20_31), inference(avatar_split_clause, [], [f71, f233, f229])).
fof(f71, plain, (in(sK19, sK17) | ~ sP0), inference(cnf_transformation, [], [f14])).
fof(f227, plain, (~ spl20_1 | spl20_29), inference(avatar_split_clause, [], [f16, f224, f85])).
fof(f16, plain, (hollywood(sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ((in(sK10, sK8) & (sK7 = sK10) & in(sK9, sK8) & (sK6 = sK9) & young(sK7) & man(sK7) & fellow(sK7) & young(sK6) & man(sK6) & fellow(sK6) & ~ (sK6 = sK7) & front(sK8) & furniture(sK8) & seat(sK8) & in(sK3, sK2) & down(sK3, sK5) & barrel(sK3, sK4) & lonely(sK5) & way(sK5) & street(sK5) & old(sK4) & dirty(sK4) & white(sK4) & car(sK4) & chevy(sK4) & event(sK3) & city(sK2) & hollywood(sK2)) | ~ sP1), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3, sK4, sK5, sK6, sK7, sK8, sK9, sK10])], [f8, f9])).
fof(f9, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) => (in(sK10, sK8) & (sK7 = sK10) & in(sK9, sK8) & (sK6 = sK9) & young(sK7) & man(sK7) & fellow(sK7) & young(sK6) & man(sK6) & fellow(sK6) & ~ (sK6 = sK7) & front(sK8) & furniture(sK8) & seat(sK8) & in(sK3, sK2) & down(sK3, sK5) & barrel(sK3, sK4) & lonely(sK5) & way(sK5) & street(sK5) & old(sK4) & dirty(sK4) & white(sK4) & car(sK4) & chevy(sK4) & event(sK3) & city(sK2) & hollywood(sK2))), introduced(choice_axiom, [])).
fof(f8, plain, (? [X0, X1, X2, X3, X4, X5, X6, X7, X8] : (in(X8, X6) & (X5 = X8) & in(X7, X6) & (X4 = X7) & young(X5) & man(X5) & fellow(X5) & young(X4) & man(X4) & fellow(X4) & ~ (X4 = X5) & front(X6) & furniture(X6) & seat(X6) & in(X1, X0) & down(X1, X3) & barrel(X1, X2) & lonely(X3) & way(X3) & street(X3) & old(X2) & dirty(X2) & white(X2) & car(X2) & chevy(X2) & event(X1) & city(X0) & hollywood(X0)) | ~ sP1), inference(nnf_transformation, [], [f6])).
fof(f222, plain, (~ spl20_1 | spl20_28), inference(avatar_split_clause, [], [f17, f219, f85])).
fof(f17, plain, (city(sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f217, plain, (~ spl20_1 | spl20_27), inference(avatar_split_clause, [], [f18, f214, f85])).
fof(f18, plain, (event(sK3) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f212, plain, (~ spl20_1 | spl20_26), inference(avatar_split_clause, [], [f19, f209, f85])).
fof(f19, plain, (chevy(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f207, plain, (~ spl20_1 | spl20_25), inference(avatar_split_clause, [], [f20, f204, f85])).
fof(f20, plain, (car(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f202, plain, (~ spl20_1 | spl20_24), inference(avatar_split_clause, [], [f21, f199, f85])).
fof(f21, plain, (white(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f197, plain, (~ spl20_1 | spl20_23), inference(avatar_split_clause, [], [f22, f194, f85])).
fof(f22, plain, (dirty(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f192, plain, (~ spl20_1 | spl20_22), inference(avatar_split_clause, [], [f23, f189, f85])).
fof(f23, plain, (old(sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f187, plain, (~ spl20_1 | spl20_21), inference(avatar_split_clause, [], [f24, f184, f85])).
fof(f24, plain, (street(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f182, plain, (~ spl20_1 | spl20_20), inference(avatar_split_clause, [], [f25, f179, f85])).
fof(f25, plain, (way(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f177, plain, (~ spl20_1 | spl20_19), inference(avatar_split_clause, [], [f26, f174, f85])).
fof(f26, plain, (lonely(sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f172, plain, (~ spl20_1 | spl20_18), inference(avatar_split_clause, [], [f27, f169, f85])).
fof(f27, plain, (barrel(sK3, sK4) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f167, plain, (~ spl20_1 | spl20_17), inference(avatar_split_clause, [], [f28, f164, f85])).
fof(f28, plain, (down(sK3, sK5) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f162, plain, (~ spl20_1 | spl20_16), inference(avatar_split_clause, [], [f29, f159, f85])).
fof(f29, plain, (in(sK3, sK2) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f157, plain, (~ spl20_1 | spl20_15), inference(avatar_split_clause, [], [f30, f154, f85])).
fof(f30, plain, (seat(sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f152, plain, (~ spl20_1 | spl20_14), inference(avatar_split_clause, [], [f31, f149, f85])).
fof(f31, plain, (furniture(sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f147, plain, (~ spl20_1 | spl20_13), inference(avatar_split_clause, [], [f32, f144, f85])).
fof(f32, plain, (front(sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f142, plain, (~ spl20_1 | ~ spl20_12), inference(avatar_split_clause, [], [f33, f139, f85])).
fof(f33, plain, (~ (sK6 = sK7) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f137, plain, (~ spl20_1 | spl20_11), inference(avatar_split_clause, [], [f34, f134, f85])).
fof(f34, plain, (fellow(sK6) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f132, plain, (~ spl20_1 | spl20_10), inference(avatar_split_clause, [], [f35, f129, f85])).
fof(f35, plain, (man(sK6) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f127, plain, (~ spl20_1 | spl20_9), inference(avatar_split_clause, [], [f36, f124, f85])).
fof(f36, plain, (young(sK6) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f122, plain, (~ spl20_1 | spl20_8), inference(avatar_split_clause, [], [f37, f119, f85])).
fof(f37, plain, (fellow(sK7) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f117, plain, (~ spl20_1 | spl20_7), inference(avatar_split_clause, [], [f38, f114, f85])).
fof(f38, plain, (man(sK7) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f112, plain, (~ spl20_1 | spl20_6), inference(avatar_split_clause, [], [f39, f109, f85])).
fof(f39, plain, (young(sK7) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f107, plain, (~ spl20_1 | spl20_5), inference(avatar_split_clause, [], [f40, f104, f85])).
fof(f40, plain, ((sK6 = sK9) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f102, plain, (~ spl20_1 | spl20_4), inference(avatar_split_clause, [], [f41, f99, f85])).
fof(f41, plain, (in(sK9, sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f97, plain, (~ spl20_1 | spl20_3), inference(avatar_split_clause, [], [f42, f94, f85])).
fof(f42, plain, ((sK7 = sK10) | ~ sP1), inference(cnf_transformation, [], [f10])).
fof(f92, plain, (~ spl20_1 | spl20_2), inference(avatar_split_clause, [], [f43, f89, f85])).
fof(f43, plain, (in(sK10, sK8) | ~ sP1), inference(cnf_transformation, [], [f10])).