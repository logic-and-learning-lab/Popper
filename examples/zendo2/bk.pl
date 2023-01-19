%% :-style_check(-discontiguous).



max_size(10).
%% small(X) :- number(X), max_size(M), X >= 0, X =< M//3.
%% medium(X) :- number(X), max_size(M), X>M//3, X =< 2*M//3.
%% large(X) :- number(X), max_size(M), X>2*M//3, X =< M.

small(0).
small(1).
small(2).
small(3).
medium(4).
medium(5).
medium(6).
large(10).
large(7).
large(8).
large(9).

piece(0, p0_0).
coord1(p0_0, 5).
coord2(p0_0, 10).
size(p0_0, 4).

red(p0_0).
lhs(p0_0).
piece(0, p0_1).
coord1(p0_1, 9).
coord2(p0_1, 1).
size(p0_1, 8).

green(p0_1).
lhs(p0_1).
piece(0, p0_2).
coord1(p0_2, 9).
coord2(p0_2, 4).
size(p0_2, 6).

blue(p0_2).
lhs(p0_2).
piece(1, p1_0).
coord1(p1_0, 3).
coord2(p1_0, 5).
size(p1_0, 4).

blue(p1_0).
strange(p1_0).
piece(1, p1_1).
coord1(p1_1, 9).
coord2(p1_1, 0).
size(p1_1, 1).

green(p1_1).
lhs(p1_1).
piece(1, p1_2).
coord1(p1_2, 9).
coord2(p1_2, 3).
size(p1_2, 7).

red(p1_2).
lhs(p1_2).
piece(1, p1_3).
coord1(p1_3, 10).
coord2(p1_3, 3).
size(p1_3, 2).

green(p1_3).
lhs(p1_3).
piece(2, p2_0).
coord1(p2_0, 4).
coord2(p2_0, 2).
size(p2_0, 5).

green(p2_0).
lhs(p2_0).
piece(2, p2_1).
coord1(p2_1, 5).
coord2(p2_1, 4).
size(p2_1, 1).

blue(p2_1).
rhs(p2_1).
piece(2, p2_2).
coord1(p2_2, 2).
coord2(p2_2, 7).
size(p2_2, 4).

green(p2_2).
lhs(p2_2).
piece(2, p2_3).
coord1(p2_3, 4).
coord2(p2_3, 8).
size(p2_3, 7).

green(p2_3).
upright(p2_3).
piece(3, p3_0).
coord1(p3_0, 8).
coord2(p3_0, 8).
size(p3_0, 2).

green(p3_0).
rhs(p3_0).
piece(3, p3_1).
coord1(p3_1, 3).
coord2(p3_1, 4).
size(p3_1, 3).

green(p3_1).
lhs(p3_1).
piece(3, p3_2).
coord1(p3_2, 4).
coord2(p3_2, 5).
size(p3_2, 1).

blue(p3_2).
strange(p3_2).
piece(3, p3_3).
coord1(p3_3, 7).
coord2(p3_3, 0).
size(p3_3, 6).

red(p3_3).
lhs(p3_3).
piece(3, p3_4).
coord1(p3_4, 5).
coord2(p3_4, 5).
size(p3_4, 3).

blue(p3_4).
rhs(p3_4).
contact(p3_2, p3_4).
contact(p3_2, p3_4).
contact(p3_4, p3_2).
contact(p3_4, p3_2).
piece(4, p4_0).
coord1(p4_0, 4).
coord2(p4_0, 4).
size(p4_0, 10).

green(p4_0).
lhs(p4_0).
piece(4, p4_1).
coord1(p4_1, 5).
coord2(p4_1, 5).
size(p4_1, 2).

red(p4_1).
strange(p4_1).
piece(4, p4_2).
coord1(p4_2, 4).
coord2(p4_2, 2).
size(p4_2, 2).

green(p4_2).
lhs(p4_2).
piece(4, p4_3).
coord1(p4_3, 2).
coord2(p4_3, 7).
size(p4_3, 1).

blue(p4_3).
strange(p4_3).
piece(4, p4_4).
coord1(p4_4, 3).
coord2(p4_4, 6).
size(p4_4, 9).

green(p4_4).
upright(p4_4).
piece(5, p5_0).
coord1(p5_0, 0).
coord2(p5_0, 5).
size(p5_0, 7).

green(p5_0).
lhs(p5_0).
piece(5, p5_1).
coord1(p5_1, 3).
coord2(p5_1, 4).
size(p5_1, 3).

red(p5_1).
lhs(p5_1).
piece(5, p5_2).
coord1(p5_2, 0).
coord2(p5_2, 7).
size(p5_2, 9).

green(p5_2).
strange(p5_2).
piece(5, p5_3).
coord1(p5_3, 1).
coord2(p5_3, 1).
size(p5_3, 6).

green(p5_3).
rhs(p5_3).
piece(6, p6_0).
coord1(p6_0, 9).
coord2(p6_0, 7).
size(p6_0, 6).

green(p6_0).
lhs(p6_0).
piece(6, p6_1).
coord1(p6_1, 9).
coord2(p6_1, 7).
size(p6_1, 7).

green(p6_1).
rhs(p6_1).
piece(6, p6_2).
coord1(p6_2, 10).
coord2(p6_2, 6).
size(p6_2, 8).

red(p6_2).
upright(p6_2).
piece(6, p6_3).
coord1(p6_3, 5).
coord2(p6_3, 9).
size(p6_3, 0).

red(p6_3).
strange(p6_3).
piece(7, p7_0).
coord1(p7_0, 5).
coord2(p7_0, 0).
size(p7_0, 6).

green(p7_0).
strange(p7_0).
piece(7, p7_1).
coord1(p7_1, 4).
coord2(p7_1, 5).
size(p7_1, 7).

red(p7_1).
lhs(p7_1).
piece(7, p7_2).
coord1(p7_2, 5).
coord2(p7_2, 0).
size(p7_2, 9).

blue(p7_2).
lhs(p7_2).
contact(p7_0, p7_2).
contact(p7_0, p7_2).
contact(p7_2, p7_0).
contact(p7_2, p7_0).
piece(8, p8_0).
coord1(p8_0, 6).
coord2(p8_0, 6).
size(p8_0, 3).

red(p8_0).
upright(p8_0).
piece(8, p8_1).
coord1(p8_1, 3).
coord2(p8_1, 9).
size(p8_1, 1).

blue(p8_1).
lhs(p8_1).
piece(8, p8_2).
coord1(p8_2, 3).
coord2(p8_2, 6).
size(p8_2, 6).

green(p8_2).
strange(p8_2).
piece(9, p9_0).
coord1(p9_0, 5).
coord2(p9_0, 5).
size(p9_0, 3).

blue(p9_0).
upright(p9_0).
piece(9, p9_1).
coord1(p9_1, 9).
coord2(p9_1, 9).
size(p9_1, 10).

blue(p9_1).
lhs(p9_1).
piece(9, p9_2).
coord1(p9_2, 10).
coord2(p9_2, 2).
size(p9_2, 4).

red(p9_2).
strange(p9_2).
piece(9, p9_3).
coord1(p9_3, 9).
coord2(p9_3, 8).
size(p9_3, 5).

green(p9_3).
strange(p9_3).
piece(9, p9_4).
coord1(p9_4, 9).
coord2(p9_4, 8).
size(p9_4, 10).

green(p9_4).
rhs(p9_4).
contact(p9_3, p9_4).
contact(p9_3, p9_4).
contact(p9_4, p9_3).
contact(p9_4, p9_3).
piece(10, p10_0).
coord1(p10_0, 5).
coord2(p10_0, 6).
size(p10_0, 7).

green(p10_0).
strange(p10_0).
piece(10, p10_1).
coord1(p10_1, 3).
coord2(p10_1, 7).
size(p10_1, 8).

red(p10_1).
upright(p10_1).
piece(10, p10_2).
coord1(p10_2, 5).
coord2(p10_2, 9).
size(p10_2, 2).

green(p10_2).
lhs(p10_2).
piece(11, p11_0).
coord1(p11_0, 4).
coord2(p11_0, 2).
size(p11_0, 4).

red(p11_0).
rhs(p11_0).
piece(11, p11_1).
coord1(p11_1, 4).
coord2(p11_1, 5).
size(p11_1, 10).

green(p11_1).
strange(p11_1).
piece(11, p11_2).
coord1(p11_2, 9).
coord2(p11_2, 3).
size(p11_2, 8).

green(p11_2).
rhs(p11_2).
piece(11, p11_3).
coord1(p11_3, 0).
coord2(p11_3, 6).
size(p11_3, 8).

blue(p11_3).
upright(p11_3).
piece(11, p11_4).
coord1(p11_4, 9).
coord2(p11_4, 9).
size(p11_4, 9).

blue(p11_4).
lhs(p11_4).
piece(12, p12_0).
coord1(p12_0, 3).
coord2(p12_0, 3).
size(p12_0, 0).

green(p12_0).
lhs(p12_0).
piece(12, p12_1).
coord1(p12_1, 4).
coord2(p12_1, 2).
size(p12_1, 6).

blue(p12_1).
lhs(p12_1).
piece(12, p12_2).
coord1(p12_2, 0).
coord2(p12_2, 9).
size(p12_2, 5).

red(p12_2).
strange(p12_2).
piece(12, p12_3).
coord1(p12_3, 4).
coord2(p12_3, 5).
size(p12_3, 8).

green(p12_3).
strange(p12_3).
piece(12, p12_4).
coord1(p12_4, 6).
coord2(p12_4, 5).
size(p12_4, 6).

green(p12_4).
lhs(p12_4).
contact(p12_0, p12_1).
contact(p12_0, p12_1).
contact(p12_1, p12_0).
contact(p12_1, p12_0).
piece(13, p13_0).
coord1(p13_0, 0).
coord2(p13_0, 9).
size(p13_0, 4).

green(p13_0).
lhs(p13_0).
piece(13, p13_1).
coord1(p13_1, 10).
coord2(p13_1, 9).
size(p13_1, 6).

green(p13_1).
upright(p13_1).
piece(13, p13_2).
coord1(p13_2, 0).
coord2(p13_2, 0).
size(p13_2, 0).

blue(p13_2).
rhs(p13_2).
piece(13, p13_3).
coord1(p13_3, 9).
coord2(p13_3, 6).
size(p13_3, 10).

green(p13_3).
upright(p13_3).
piece(13, p13_4).
coord1(p13_4, 6).
coord2(p13_4, 2).
size(p13_4, 10).

red(p13_4).
upright(p13_4).
piece(14, p14_0).
coord1(p14_0, 0).
coord2(p14_0, 3).
size(p14_0, 7).

blue(p14_0).
upright(p14_0).
piece(14, p14_1).
coord1(p14_1, 6).
coord2(p14_1, 2).
size(p14_1, 0).

red(p14_1).
rhs(p14_1).
piece(14, p14_2).
coord1(p14_2, 1).
coord2(p14_2, 1).
size(p14_2, 4).

green(p14_2).
rhs(p14_2).
piece(15, p15_0).
coord1(p15_0, 1).
coord2(p15_0, 2).
size(p15_0, 7).

green(p15_0).
upright(p15_0).
piece(15, p15_1).
coord1(p15_1, 4).
coord2(p15_1, 0).
size(p15_1, 9).

red(p15_1).
lhs(p15_1).
piece(15, p15_2).
coord1(p15_2, 4).
coord2(p15_2, 6).
size(p15_2, 9).

red(p15_2).
strange(p15_2).
piece(15, p15_3).
coord1(p15_3, 9).
coord2(p15_3, 2).
size(p15_3, 5).

blue(p15_3).
rhs(p15_3).
piece(16, p16_0).
coord1(p16_0, 6).
coord2(p16_0, 7).
size(p16_0, 7).

red(p16_0).
rhs(p16_0).
piece(16, p16_1).
coord1(p16_1, 0).
coord2(p16_1, 6).
size(p16_1, 10).

blue(p16_1).
strange(p16_1).
piece(16, p16_2).
coord1(p16_2, 10).
coord2(p16_2, 6).
size(p16_2, 7).

blue(p16_2).
lhs(p16_2).
piece(16, p16_3).
coord1(p16_3, 10).
coord2(p16_3, 4).
size(p16_3, 9).

green(p16_3).
strange(p16_3).
contact(p16_0, p16_2).
contact(p16_0, p16_2).
contact(p16_2, p16_0).
contact(p16_2, p16_0).
piece(17, p17_0).
coord1(p17_0, 1).
coord2(p17_0, 5).
size(p17_0, 8).

green(p17_0).
lhs(p17_0).
piece(17, p17_1).
coord1(p17_1, 1).
coord2(p17_1, 10).
size(p17_1, 5).

red(p17_1).
rhs(p17_1).
piece(17, p17_2).
coord1(p17_2, 1).
coord2(p17_2, 3).
size(p17_2, 6).

red(p17_2).
lhs(p17_2).
piece(18, p18_0).
coord1(p18_0, 4).
coord2(p18_0, 8).
size(p18_0, 6).

green(p18_0).
lhs(p18_0).
piece(18, p18_1).
coord1(p18_1, 0).
coord2(p18_1, 10).
size(p18_1, 6).

green(p18_1).
lhs(p18_1).
piece(18, p18_2).
coord1(p18_2, 0).
coord2(p18_2, 7).
size(p18_2, 6).

green(p18_2).
lhs(p18_2).
piece(19, p19_0).
coord1(p19_0, 10).
coord2(p19_0, 0).
size(p19_0, 2).

green(p19_0).
rhs(p19_0).
piece(19, p19_1).
coord1(p19_1, 4).
coord2(p19_1, 10).
size(p19_1, 7).

red(p19_1).
upright(p19_1).
piece(19, p19_2).
coord1(p19_2, 0).
coord2(p19_2, 5).
size(p19_2, 4).

red(p19_2).
rhs(p19_2).
piece(19, p19_3).
coord1(p19_3, 0).
coord2(p19_3, 9).
size(p19_3, 8).

blue(p19_3).
upright(p19_3).
piece(19, p19_4).
coord1(p19_4, 6).
coord2(p19_4, 9).
size(p19_4, 2).

green(p19_4).
strange(p19_4).
piece(20, p20_0).
coord1(p20_0, 3).
coord2(p20_0, 6).
size(p20_0, 8).

red(p20_0).
rhs(p20_0).
piece(20, p20_1).
coord1(p20_1, 3).
coord2(p20_1, 3).
size(p20_1, 9).

green(p20_1).
upright(p20_1).
piece(21, p21_0).
coord1(p21_0, 4).
coord2(p21_0, 4).
size(p21_0, 7).

green(p21_0).
upright(p21_0).
piece(21, p21_1).
coord1(p21_1, 2).
coord2(p21_1, 10).
size(p21_1, 8).

green(p21_1).
rhs(p21_1).
piece(21, p21_2).
coord1(p21_2, 4).
coord2(p21_2, 9).
size(p21_2, 7).

green(p21_2).
upright(p21_2).
piece(21, p21_3).
coord1(p21_3, 9).
coord2(p21_3, 9).
size(p21_3, 2).

red(p21_3).
strange(p21_3).
piece(22, p22_0).
coord1(p22_0, 9).
coord2(p22_0, 5).
size(p22_0, 7).

blue(p22_0).
upright(p22_0).
piece(22, p22_1).
coord1(p22_1, 9).
coord2(p22_1, 0).
size(p22_1, 7).

blue(p22_1).
upright(p22_1).
piece(22, p22_2).
coord1(p22_2, 2).
coord2(p22_2, 6).
size(p22_2, 9).

red(p22_2).
strange(p22_2).
piece(23, p23_0).
coord1(p23_0, 0).
coord2(p23_0, 3).
size(p23_0, 4).

green(p23_0).
rhs(p23_0).
piece(24, p24_0).
coord1(p24_0, 3).
coord2(p24_0, 2).
size(p24_0, 3).

blue(p24_0).
strange(p24_0).
piece(24, p24_1).
coord1(p24_1, 3).
coord2(p24_1, 1).
size(p24_1, 1).

blue(p24_1).
lhs(p24_1).
piece(24, p24_2).
coord1(p24_2, 8).
coord2(p24_2, 3).
size(p24_2, 4).

red(p24_2).
upright(p24_2).
piece(24, p24_3).
coord1(p24_3, 1).
coord2(p24_3, 1).
size(p24_3, 7).

red(p24_3).
upright(p24_3).
contact(p24_0, p24_1).
contact(p24_0, p24_1).
contact(p24_1, p24_0).
contact(p24_1, p24_0).
piece(25, p25_0).
coord1(p25_0, 5).
coord2(p25_0, 8).
size(p25_0, 5).

green(p25_0).
strange(p25_0).
piece(25, p25_1).
coord1(p25_1, 1).
coord2(p25_1, 3).
size(p25_1, 3).

green(p25_1).
rhs(p25_1).
piece(25, p25_2).
coord1(p25_2, 1).
coord2(p25_2, 2).
size(p25_2, 10).

blue(p25_2).
strange(p25_2).
contact(p25_1, p25_2).
contact(p25_1, p25_2).
contact(p25_2, p25_1).
contact(p25_2, p25_1).
piece(26, p26_0).
coord1(p26_0, 8).
coord2(p26_0, 8).
size(p26_0, 7).

blue(p26_0).
upright(p26_0).
piece(26, p26_1).
coord1(p26_1, 3).
coord2(p26_1, 3).
size(p26_1, 6).

blue(p26_1).
strange(p26_1).
piece(26, p26_2).
coord1(p26_2, 4).
coord2(p26_2, 10).
size(p26_2, 4).

blue(p26_2).
rhs(p26_2).
piece(27, p27_0).
coord1(p27_0, 6).
coord2(p27_0, 10).
size(p27_0, 4).

red(p27_0).
lhs(p27_0).
piece(27, p27_1).
coord1(p27_1, 7).
coord2(p27_1, 3).
size(p27_1, 9).

blue(p27_1).
strange(p27_1).
piece(27, p27_2).
coord1(p27_2, 7).
coord2(p27_2, 9).
size(p27_2, 2).

red(p27_2).
rhs(p27_2).
piece(27, p27_3).
coord1(p27_3, 1).
coord2(p27_3, 4).
size(p27_3, 6).

blue(p27_3).
rhs(p27_3).
piece(27, p27_4).
coord1(p27_4, 9).
coord2(p27_4, 7).
size(p27_4, 1).

red(p27_4).
strange(p27_4).
piece(28, p28_0).
coord1(p28_0, 4).
coord2(p28_0, 3).
size(p28_0, 1).

blue(p28_0).
rhs(p28_0).
piece(29, p29_0).
coord1(p29_0, 7).
coord2(p29_0, 4).
size(p29_0, 2).

green(p29_0).
strange(p29_0).
piece(29, p29_1).
coord1(p29_1, 6).
coord2(p29_1, 4).
size(p29_1, 9).

blue(p29_1).
lhs(p29_1).
contact(p29_0, p29_1).
contact(p29_0, p29_1).
contact(p29_1, p29_0).
contact(p29_1, p29_0).
piece(30, p30_0).
coord1(p30_0, 2).
coord2(p30_0, 4).
size(p30_0, 7).

red(p30_0).
strange(p30_0).
piece(31, p31_0).
coord1(p31_0, 4).
coord2(p31_0, 9).
size(p31_0, 4).

blue(p31_0).
lhs(p31_0).
piece(31, p31_1).
coord1(p31_1, 0).
coord2(p31_1, 4).
size(p31_1, 10).

green(p31_1).
rhs(p31_1).
piece(31, p31_2).
coord1(p31_2, 3).
coord2(p31_2, 9).
size(p31_2, 2).

blue(p31_2).
strange(p31_2).
piece(31, p31_3).
coord1(p31_3, 8).
coord2(p31_3, 7).
size(p31_3, 4).

blue(p31_3).
lhs(p31_3).
piece(31, p31_4).
coord1(p31_4, 9).
coord2(p31_4, 8).
size(p31_4, 9).

blue(p31_4).
rhs(p31_4).
contact(p31_0, p31_2).
contact(p31_0, p31_2).
contact(p31_2, p31_0).
contact(p31_2, p31_0).
piece(32, p32_0).
coord1(p32_0, 1).
coord2(p32_0, 1).
size(p32_0, 6).

red(p32_0).
strange(p32_0).
piece(32, p32_1).
coord1(p32_1, 10).
coord2(p32_1, 8).
size(p32_1, 4).

blue(p32_1).
lhs(p32_1).
piece(32, p32_2).
coord1(p32_2, 6).
coord2(p32_2, 6).
size(p32_2, 9).

blue(p32_2).
rhs(p32_2).
piece(33, p33_0).
coord1(p33_0, 6).
coord2(p33_0, 0).
size(p33_0, 1).

blue(p33_0).
strange(p33_0).
piece(33, p33_1).
coord1(p33_1, 1).
coord2(p33_1, 1).
size(p33_1, 9).

blue(p33_1).
upright(p33_1).
piece(34, p34_0).
coord1(p34_0, 7).
coord2(p34_0, 8).
size(p34_0, 8).

red(p34_0).
upright(p34_0).
piece(34, p34_1).
coord1(p34_1, 0).
coord2(p34_1, 7).
size(p34_1, 9).

red(p34_1).
lhs(p34_1).
piece(34, p34_2).
coord1(p34_2, 0).
coord2(p34_2, 4).
size(p34_2, 9).

red(p34_2).
rhs(p34_2).
piece(34, p34_3).
coord1(p34_3, 2).
coord2(p34_3, 7).
size(p34_3, 9).

red(p34_3).
lhs(p34_3).
piece(35, p35_0).
coord1(p35_0, 9).
coord2(p35_0, 4).
size(p35_0, 10).

green(p35_0).
upright(p35_0).
piece(35, p35_1).
coord1(p35_1, 8).
coord2(p35_1, 0).
size(p35_1, 8).

green(p35_1).
upright(p35_1).
piece(35, p35_2).
coord1(p35_2, 1).
coord2(p35_2, 7).
size(p35_2, 5).

blue(p35_2).
strange(p35_2).
piece(36, p36_0).
coord1(p36_0, 6).
coord2(p36_0, 5).
size(p36_0, 9).

blue(p36_0).
rhs(p36_0).
piece(37, p37_0).
coord1(p37_0, 5).
coord2(p37_0, 7).
size(p37_0, 2).

green(p37_0).
rhs(p37_0).
piece(37, p37_1).
coord1(p37_1, 4).
coord2(p37_1, 7).
size(p37_1, 3).

blue(p37_1).
rhs(p37_1).
contact(p37_0, p37_1).
contact(p37_0, p37_1).
contact(p37_1, p37_0).
contact(p37_1, p37_0).
piece(38, p38_0).
coord1(p38_0, 0).
coord2(p38_0, 10).
size(p38_0, 5).

blue(p38_0).
strange(p38_0).
piece(38, p38_1).
coord1(p38_1, 8).
coord2(p38_1, 4).
size(p38_1, 8).

blue(p38_1).
lhs(p38_1).
piece(38, p38_2).
coord1(p38_2, 10).
coord2(p38_2, 0).
size(p38_2, 7).

green(p38_2).
rhs(p38_2).
piece(39, p39_0).
coord1(p39_0, 7).
coord2(p39_0, 8).
size(p39_0, 6).

blue(p39_0).
strange(p39_0).
piece(39, p39_1).
coord1(p39_1, 3).
coord2(p39_1, 1).
size(p39_1, 10).

blue(p39_1).
lhs(p39_1).
piece(39, p39_2).
coord1(p39_2, 8).
coord2(p39_2, 7).
size(p39_2, 4).

green(p39_2).
rhs(p39_2).