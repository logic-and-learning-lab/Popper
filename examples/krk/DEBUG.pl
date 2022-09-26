sat(1):-cell(A,C,B,D),cell(A,E,B,D),distance(C,E,F),distance(E,C,F).
sat(10):-cell(A,B,C,D),cell(A,F,C,D),distance(F,B,E),one(E).
sat(100):-cell(A,C,D,F),cell(A,E,D,F),distance(C,E,B),one(B).
sat(101):-cell(A,E,C,D),cell(A,F,C,D),distance(E,F,B),distance(F,E,B).
sat(102):-cell(A,E,C,D),cell(A,F,C,D),distance(F,E,B),one(B).
sat(103):-cell(A,C,E,D),cell(A,F,E,D),distance(C,F,B),distance(F,C,B).
sat(104):-cell(A,C,E,D),cell(A,F,E,D),distance(F,C,B),one(B).
sat(105):-cell(A,C,E,D),cell(A,F,E,D),distance(C,F,B),one(B).
sat(106):-cell(A,D,E,C),cell(A,F,E,C),distance(D,F,B),distance(F,D,B).
sat(107):-cell(A,D,E,C),cell(A,F,E,C),distance(D,F,B),one(B).
sat(108):-cell(A,D,E,C),cell(A,F,E,C),distance(F,D,B),one(B).
sat(109):-cell(A,C,E,F),cell(A,D,E,F),distance(C,D,B),distance(D,C,B).
sat(11):-cell(A,B,D,E),cell(A,C,D,E),distance(B,C,F),distance(C,B,F).
sat(110):-cell(A,C,E,F),cell(A,D,E,F),distance(D,C,B),one(B).
sat(111):-cell(A,C,E,F),cell(A,D,E,F),distance(C,D,B),one(B).
sat(112):-cell(A,D,C,E),cell(A,F,C,E),distance(D,F,B),distance(F,D,B).
sat(113):-cell(A,D,C,E),cell(A,F,C,E),distance(D,F,B),one(B).
sat(114):-cell(A,D,C,E),cell(A,F,C,E),distance(F,D,B),one(B).
sat(115):-cell(A,E,C,D),cell(A,F,C,D),distance(E,F,B),one(B).
sat(116):-cell(A,B,E,D),cell(A,F,E,D),distance(B,F,C),distance(F,B,C).
sat(117):-cell(A,B,E,D),cell(A,F,E,D),distance(B,F,C),one(C).
sat(118):-cell(A,B,E,D),cell(A,F,E,D),distance(F,B,C),one(C).
sat(119):-cell(A,B,E,D),cell(A,C,E,D),distance(B,C,F),distance(C,B,F).
sat(12):-cell(A,B,D,E),cell(A,C,D,E),distance(B,C,F),one(F).
sat(120):-cell(A,B,E,D),cell(A,C,E,D),distance(B,C,F),one(F).
sat(121):-cell(A,B,E,D),cell(A,C,E,D),distance(C,B,F),one(F).
sat(122):-cell(A,D,B,C),cell(A,E,B,C),distance(D,E,F),distance(E,D,F).
sat(123):-cell(A,D,B,C),cell(A,E,B,C),distance(D,E,F),one(F).
sat(124):-cell(A,D,B,C),cell(A,E,B,C),distance(E,D,F),one(F).
sat(125):-cell(A,D,B,C),cell(A,F,B,C),distance(D,F,E),distance(F,D,E).
sat(126):-cell(A,D,B,C),cell(A,F,B,C),distance(F,D,E),one(E).
sat(127):-cell(A,D,B,C),cell(A,F,B,C),distance(D,F,E),one(E).
sat(128):-cell(A,B,D,E),cell(A,F,D,E),distance(B,F,C),distance(F,B,C).
sat(129):-cell(A,B,D,E),cell(A,F,D,E),distance(F,B,C),one(C).
sat(13):-cell(A,B,D,E),cell(A,C,D,E),distance(C,B,F),one(F).
sat(130):-cell(A,B,D,E),cell(A,F,D,E),distance(B,F,C),one(C).
sat(131):-cell(A,E,D,B),cell(A,F,D,B),distance(E,F,C),distance(F,E,C).
sat(132):-cell(A,E,D,B),cell(A,F,D,B),distance(F,E,C),one(C).
sat(133):-cell(A,E,D,B),cell(A,F,D,B),distance(E,F,C),one(C).
sat(134):-cell(A,C,D,B),cell(A,E,D,B),distance(C,E,F),distance(E,C,F).
sat(135):-cell(A,C,D,B),cell(A,E,D,B),distance(C,E,F),one(F).
sat(136):-cell(A,C,D,B),cell(A,E,D,B),distance(E,C,F),one(F).
sat(137):-cell(A,C,D,B),cell(A,F,D,B),distance(C,F,E),distance(F,C,E).
sat(138):-cell(A,C,D,B),cell(A,F,D,B),distance(C,F,E),one(E).
sat(139):-cell(A,C,D,B),cell(A,F,D,B),distance(F,C,E),one(E).
sat(14):-cell(A,C,D,E),cell(A,F,D,E),distance(C,F,B),distance(F,C,B).
sat(140):-cell(A,C,B,D),cell(A,F,B,D),distance(C,F,E),distance(F,C,E).
sat(141):-cell(A,C,B,D),cell(A,F,B,D),distance(C,F,E),one(E).
sat(142):-cell(A,C,B,D),cell(A,F,B,D),distance(F,C,E),one(E).
sat(143):-cell(A,C,F,B),cell(A,E,F,B),distance(C,E,D),distance(E,C,D).
sat(144):-cell(A,C,F,B),cell(A,E,F,B),distance(C,E,D),one(D).
sat(145):-cell(A,C,F,B),cell(A,D,F,B),distance(C,D,E),distance(D,C,E).
sat(146):-cell(A,C,F,B),cell(A,D,F,B),distance(C,D,E),one(E).
sat(147):-cell(A,C,F,B),cell(A,D,F,B),distance(D,C,E),one(E).
sat(148):-cell(A,C,F,B),cell(A,E,F,B),distance(E,C,D),one(D).
sat(149):-cell(A,D,C,B),cell(A,F,C,B),distance(D,F,E),distance(F,D,E).
sat(15):-cell(A,C,D,E),cell(A,F,D,E),distance(C,F,B),one(B).
sat(150):-cell(A,D,C,B),cell(A,F,C,B),distance(F,D,E),one(E).
sat(151):-cell(A,D,C,B),cell(A,F,C,B),distance(D,F,E),one(E).
sat(152):-cell(A,E,C,B),cell(A,F,C,B),distance(E,F,D),distance(F,E,D).
sat(153):-cell(A,E,C,B),cell(A,F,C,B),distance(E,F,D),one(D).
sat(154):-cell(A,E,C,B),cell(A,F,C,B),distance(F,E,D),one(D).
sat(155):-cell(A,E,B,C),cell(A,F,B,C),distance(E,F,D),distance(F,E,D).
sat(156):-cell(A,E,B,C),cell(A,F,B,C),distance(F,E,D),one(D).
sat(157):-cell(A,E,B,C),cell(A,F,B,C),distance(E,F,D),one(D).
sat(158):-cell(A,D,B,F),cell(A,E,B,F),distance(D,E,C),distance(E,D,C).
sat(159):-cell(A,D,B,F),cell(A,E,B,F),distance(E,D,C),one(C).
sat(16):-cell(A,C,D,E),cell(A,F,D,E),distance(F,C,B),one(B).
sat(160):-cell(A,D,B,F),cell(A,E,B,F),distance(D,E,C),one(C).
sat(161):-cell(A,B,C,D),cell(A,E,C,D),distance(B,E,F),distance(E,B,F).
sat(162):-cell(A,B,C,D),cell(A,E,C,D),distance(B,E,F),one(F).
sat(163):-cell(A,B,C,D),cell(A,E,C,D),distance(E,B,F),one(F).
sat(164):-cell(A,B,C,D),cell(A,F,C,D),distance(B,F,E),one(E).
sat(165):-cell(A,B,F,D),cell(A,E,F,D),distance(B,E,C),distance(E,B,C).
sat(166):-cell(A,B,F,D),cell(A,E,F,D),distance(E,B,C),one(C).
sat(167):-cell(A,B,F,D),cell(A,E,F,D),distance(B,E,C),one(C).
sat(168):-cell(A,D,F,B),cell(A,E,F,B),distance(D,E,C),distance(E,D,C).
sat(169):-cell(A,D,F,B),cell(A,E,F,B),distance(E,D,C),one(C).
sat(17):-cell(A,C,B,E),cell(A,F,B,E),distance(C,F,D),distance(F,C,D).
sat(170):-cell(A,D,F,B),cell(A,E,F,B),distance(D,E,C),one(C).
sat(171):-cell(A,B,F,C),cell(A,D,F,C),distance(B,D,E),distance(D,B,E).
sat(172):-cell(A,B,F,C),cell(A,D,F,C),distance(B,D,E),one(E).
sat(173):-cell(A,B,F,C),cell(A,D,F,C),distance(D,B,E),one(E).
sat(174):-cell(A,D,E,B),cell(A,F,E,B),distance(D,F,C),distance(F,D,C).
sat(175):-cell(A,D,E,B),cell(A,F,E,B),distance(F,D,C),one(C).
sat(176):-cell(A,D,E,B),cell(A,F,E,B),distance(D,F,C),one(C).
sat(177):-cell(A,D,C,B),cell(A,E,C,B),distance(D,E,F),distance(E,D,F).
sat(178):-cell(A,D,C,B),cell(A,E,C,B),distance(D,E,F),one(F).
sat(179):-cell(A,D,C,B),cell(A,E,C,B),distance(E,D,F),one(F).
sat(18):-cell(A,C,B,E),cell(A,F,B,E),distance(F,C,D),one(D).
sat(180):-cell(A,B,D,C),cell(A,F,D,C),distance(B,F,E),one(E).
sat(19):-cell(A,C,B,E),cell(A,F,B,E),distance(C,F,D),one(D).
sat(2):-cell(A,C,B,D),cell(A,E,B,D),distance(E,C,F),one(F).
sat(20):-cell(A,C,B,E),cell(A,D,B,E),distance(C,D,F),distance(D,C,F).
sat(21):-cell(A,C,B,E),cell(A,D,B,E),distance(D,C,F),one(F).
sat(22):-cell(A,C,B,E),cell(A,D,B,E),distance(C,D,F),one(F).
sat(23):-cell(A,B,D,C),cell(A,E,D,C),distance(B,E,F),distance(E,B,F).
sat(24):-cell(A,B,D,C),cell(A,E,D,C),distance(B,E,F),one(F).
sat(25):-cell(A,B,D,C),cell(A,E,D,C),distance(E,B,F),one(F).
sat(26):-cell(A,E,D,C),cell(A,F,D,C),distance(E,F,B),distance(F,E,B).
sat(27):-cell(A,E,D,C),cell(A,F,D,C),distance(E,F,B),one(B).
sat(28):-cell(A,E,D,C),cell(A,F,D,C),distance(F,E,B),one(B).
sat(29):-cell(A,D,B,E),cell(A,F,B,E),distance(D,F,C),distance(F,D,C).
sat(3):-cell(A,C,B,D),cell(A,E,B,D),distance(C,E,F),one(F).
sat(30):-cell(A,D,B,E),cell(A,F,B,E),distance(D,F,C),one(C).
sat(31):-cell(A,D,B,E),cell(A,F,B,E),distance(F,D,C),one(C).
sat(32):-cell(A,D,F,C),cell(A,E,F,C),distance(D,E,B),distance(E,D,B).
sat(33):-cell(A,D,F,C),cell(A,E,F,C),distance(E,D,B),one(B).
sat(34):-cell(A,D,F,C),cell(A,E,F,C),distance(D,E,B),one(B).
sat(35):-cell(A,B,F,C),cell(A,E,F,C),distance(B,E,D),distance(E,B,D).
sat(36):-cell(A,B,F,C),cell(A,E,F,C),distance(E,B,D),one(D).
sat(37):-cell(A,B,F,C),cell(A,E,F,C),distance(B,E,D),one(D).
sat(38):-cell(A,B,E,C),cell(A,D,E,C),distance(B,D,F),distance(D,B,F).
sat(39):-cell(A,B,E,C),cell(A,D,E,C),distance(B,D,F),one(F).
sat(4):-cell(A,E,B,D),cell(A,F,B,D),distance(E,F,C),distance(F,E,C).
sat(40):-cell(A,B,E,C),cell(A,D,E,C),distance(D,B,F),one(F).
sat(41):-cell(A,B,E,C),cell(A,F,E,C),distance(B,F,D),distance(F,B,D).
sat(42):-cell(A,B,E,C),cell(A,F,E,C),distance(F,B,D),one(D).
sat(43):-cell(A,B,E,C),cell(A,F,E,C),distance(B,F,D),one(D).
sat(44):-cell(A,B,C,E),cell(A,F,C,E),distance(B,F,D),distance(F,B,D).
sat(45):-cell(A,B,C,E),cell(A,F,C,E),distance(F,B,D),one(D).
sat(46):-cell(A,C,B,F),cell(A,D,B,F),distance(C,D,E),distance(D,C,E).
sat(47):-cell(A,C,B,F),cell(A,D,B,F),distance(D,C,E),one(E).
sat(48):-cell(A,C,B,F),cell(A,D,B,F),distance(C,D,E),one(E).
sat(49):-cell(A,C,B,F),cell(A,E,B,F),distance(C,E,D),distance(E,C,D).
sat(5):-cell(A,E,B,D),cell(A,F,B,D),distance(E,F,C),one(C).
sat(50):-cell(A,C,B,F),cell(A,E,B,F),distance(C,E,D),one(D).
sat(51):-cell(A,C,B,F),cell(A,E,B,F),distance(E,C,D),one(D).
sat(52):-cell(A,C,E,B),cell(A,D,E,B),distance(C,D,F),distance(D,C,F).
sat(53):-cell(A,C,E,B),cell(A,D,E,B),distance(C,D,F),one(F).
sat(54):-cell(A,C,E,B),cell(A,D,E,B),distance(D,C,F),one(F).
sat(55):-cell(A,C,E,B),cell(A,F,E,B),distance(C,F,D),distance(F,C,D).
sat(56):-cell(A,C,E,B),cell(A,F,E,B),distance(F,C,D),one(D).
sat(57):-cell(A,C,E,B),cell(A,F,E,B),distance(C,F,D),one(D).
sat(58):-cell(A,B,E,F),cell(A,D,E,F),distance(B,D,C),distance(D,B,C).
sat(59):-cell(A,B,E,F),cell(A,D,E,F),distance(D,B,C),one(C).
sat(6):-cell(A,E,B,D),cell(A,F,B,D),distance(F,E,C),one(C).
sat(60):-cell(A,B,E,F),cell(A,D,E,F),distance(B,D,C),one(C).
sat(61):-cell(A,B,E,F),cell(A,C,E,F),distance(B,C,D),distance(C,B,D).
sat(62):-cell(A,B,E,F),cell(A,C,E,F),distance(C,B,D),one(D).
sat(63):-cell(A,B,E,F),cell(A,C,E,F),distance(B,C,D),one(D).
sat(64):-cell(A,C,F,D),cell(A,E,F,D),distance(C,E,B),distance(E,C,B).
sat(65):-cell(A,C,F,D),cell(A,E,F,D),distance(E,C,B),one(B).
sat(66):-cell(A,C,F,D),cell(A,E,F,D),distance(C,E,B),one(B).
sat(67):-cell(A,B,F,D),cell(A,C,F,D),distance(B,C,E),distance(C,B,E).
sat(68):-cell(A,B,F,D),cell(A,C,F,D),distance(C,B,E),one(E).
sat(69):-cell(A,B,F,D),cell(A,C,F,D),distance(B,C,E),one(E).
sat(7):-cell(A,B,C,D),cell(A,F,C,D),distance(B,F,E),distance(F,B,E).
sat(70):-cell(A,B,C,E),cell(A,F,C,E),distance(B,F,D),one(D).
sat(71):-cell(A,B,C,E),cell(A,D,C,E),distance(B,D,F),distance(D,B,F).
sat(72):-cell(A,B,C,E),cell(A,D,C,E),distance(D,B,F),one(F).
sat(73):-cell(A,B,C,E),cell(A,D,C,E),distance(B,D,F),one(F).
sat(74):-cell(A,C,F,E),cell(A,D,F,E),distance(C,D,B),distance(D,C,B).
sat(75):-cell(A,C,F,E),cell(A,D,F,E),distance(C,D,B),one(B).
sat(76):-cell(A,C,F,E),cell(A,D,F,E),distance(D,C,B),one(B).
sat(77):-cell(A,B,F,E),cell(A,D,F,E),distance(B,D,C),distance(D,B,C).
sat(78):-cell(A,B,F,E),cell(A,D,F,E),distance(B,D,C),one(C).
sat(79):-cell(A,B,F,E),cell(A,D,F,E),distance(D,B,C),one(C).
sat(8):-cell(A,B,D,C),cell(A,F,D,C),distance(B,F,E),distance(F,B,E).
sat(80):-cell(A,B,F,E),cell(A,C,F,E),distance(B,C,D),distance(C,B,D).
sat(81):-cell(A,B,F,E),cell(A,C,F,E),distance(B,C,D),one(D).
sat(82):-cell(A,B,F,E),cell(A,C,F,E),distance(C,B,D),one(D).
sat(83):-cell(A,B,C,F),cell(A,E,C,F),distance(B,E,D),distance(E,B,D).
sat(84):-cell(A,B,C,F),cell(A,E,C,F),distance(B,E,D),one(D).
sat(85):-cell(A,B,C,F),cell(A,E,C,F),distance(E,B,D),one(D).
sat(86):-cell(A,D,C,F),cell(A,E,C,F),distance(D,E,B),distance(E,D,B).
sat(87):-cell(A,D,C,F),cell(A,E,C,F),distance(E,D,B),one(B).
sat(88):-cell(A,D,C,F),cell(A,E,C,F),distance(D,E,B),one(B).
sat(89):-cell(A,B,C,F),cell(A,D,C,F),distance(B,D,E),distance(D,B,E).
sat(9):-cell(A,B,D,C),cell(A,F,D,C),distance(F,B,E),one(E).
sat(90):-cell(A,B,C,F),cell(A,D,C,F),distance(B,D,E),one(E).
sat(91):-cell(A,B,C,F),cell(A,D,C,F),distance(D,B,E),one(E).
sat(92):-cell(A,B,D,F),cell(A,C,D,F),distance(B,C,E),distance(C,B,E).
sat(93):-cell(A,B,D,F),cell(A,C,D,F),distance(C,B,E),one(E).
sat(94):-cell(A,B,D,F),cell(A,C,D,F),distance(B,C,E),one(E).
sat(95):-cell(A,B,D,F),cell(A,E,D,F),distance(B,E,C),distance(E,B,C).
sat(96):-cell(A,B,D,F),cell(A,E,D,F),distance(E,B,C),one(C).
sat(97):-cell(A,B,D,F),cell(A,E,D,F),distance(B,E,C),one(C).
sat(98):-cell(A,C,D,F),cell(A,E,D,F),distance(C,E,B),distance(E,C,B).
sat(99):-cell(A,C,D,F),cell(A,E,D,F),distance(E,C,B),one(B).
unsat(1):-not sat(1).
unsat(10):-not sat(10).
unsat(100):-not sat(100).
unsat(101):-not sat(101).
unsat(102):-not sat(102).
unsat(103):-not sat(103).
unsat(104):-not sat(104).
unsat(105):-not sat(105).
unsat(106):-not sat(106).
unsat(107):-not sat(107).
unsat(108):-not sat(108).
unsat(109):-not sat(109).
unsat(11):-not sat(11).
unsat(110):-not sat(110).
unsat(111):-not sat(111).
unsat(112):-not sat(112).
unsat(113):-not sat(113).
unsat(114):-not sat(114).
unsat(115):-not sat(115).
unsat(116):-not sat(116).
unsat(117):-not sat(117).
unsat(118):-not sat(118).
unsat(119):-not sat(119).
unsat(12):-not sat(12).
unsat(120):-not sat(120).
unsat(121):-not sat(121).
unsat(122):-not sat(122).
unsat(123):-not sat(123).
unsat(124):-not sat(124).
unsat(125):-not sat(125).
unsat(126):-not sat(126).
unsat(127):-not sat(127).
unsat(128):-not sat(128).
unsat(129):-not sat(129).
unsat(13):-not sat(13).
unsat(130):-not sat(130).
unsat(131):-not sat(131).
unsat(132):-not sat(132).
unsat(133):-not sat(133).
unsat(134):-not sat(134).
unsat(135):-not sat(135).
unsat(136):-not sat(136).
unsat(137):-not sat(137).
unsat(138):-not sat(138).
unsat(139):-not sat(139).
unsat(14):-not sat(14).
unsat(140):-not sat(140).
unsat(141):-not sat(141).
unsat(142):-not sat(142).
unsat(143):-not sat(143).
unsat(144):-not sat(144).
unsat(145):-not sat(145).
unsat(146):-not sat(146).
unsat(147):-not sat(147).
unsat(148):-not sat(148).
unsat(149):-not sat(149).
unsat(15):-not sat(15).
unsat(150):-not sat(150).
unsat(151):-not sat(151).
unsat(152):-not sat(152).
unsat(153):-not sat(153).
unsat(154):-not sat(154).
unsat(155):-not sat(155).
unsat(156):-not sat(156).
unsat(157):-not sat(157).
unsat(158):-not sat(158).
unsat(159):-not sat(159).
unsat(16):-not sat(16).
unsat(160):-not sat(160).
unsat(161):-not sat(161).
unsat(162):-not sat(162).
unsat(163):-not sat(163).
unsat(164):-not sat(164).
unsat(165):-not sat(165).
unsat(166):-not sat(166).
unsat(167):-not sat(167).
unsat(168):-not sat(168).
unsat(169):-not sat(169).
unsat(17):-not sat(17).
unsat(170):-not sat(170).
unsat(171):-not sat(171).
unsat(172):-not sat(172).
unsat(173):-not sat(173).
unsat(174):-not sat(174).
unsat(175):-not sat(175).
unsat(176):-not sat(176).
unsat(177):-not sat(177).
unsat(178):-not sat(178).
unsat(179):-not sat(179).
unsat(18):-not sat(18).
unsat(180):-not sat(180).
unsat(19):-not sat(19).
unsat(2):-not sat(2).
unsat(20):-not sat(20).
unsat(21):-not sat(21).
unsat(22):-not sat(22).
unsat(23):-not sat(23).
unsat(24):-not sat(24).
unsat(25):-not sat(25).
unsat(26):-not sat(26).
unsat(27):-not sat(27).
unsat(28):-not sat(28).
unsat(29):-not sat(29).
unsat(3):-not sat(3).
unsat(30):-not sat(30).
unsat(31):-not sat(31).
unsat(32):-not sat(32).
unsat(33):-not sat(33).
unsat(34):-not sat(34).
unsat(35):-not sat(35).
unsat(36):-not sat(36).
unsat(37):-not sat(37).
unsat(38):-not sat(38).
unsat(39):-not sat(39).
unsat(4):-not sat(4).
unsat(40):-not sat(40).
unsat(41):-not sat(41).
unsat(42):-not sat(42).
unsat(43):-not sat(43).
unsat(44):-not sat(44).
unsat(45):-not sat(45).
unsat(46):-not sat(46).
unsat(47):-not sat(47).
unsat(48):-not sat(48).
unsat(49):-not sat(49).
unsat(5):-not sat(5).
unsat(50):-not sat(50).
unsat(51):-not sat(51).
unsat(52):-not sat(52).
unsat(53):-not sat(53).
unsat(54):-not sat(54).
unsat(55):-not sat(55).
unsat(56):-not sat(56).
unsat(57):-not sat(57).
unsat(58):-not sat(58).
unsat(59):-not sat(59).
unsat(6):-not sat(6).
unsat(60):-not sat(60).
unsat(61):-not sat(61).
unsat(62):-not sat(62).
unsat(63):-not sat(63).
unsat(64):-not sat(64).
unsat(65):-not sat(65).
unsat(66):-not sat(66).
unsat(67):-not sat(67).
unsat(68):-not sat(68).
unsat(69):-not sat(69).
unsat(7):-not sat(7).
unsat(70):-not sat(70).
unsat(71):-not sat(71).
unsat(72):-not sat(72).
unsat(73):-not sat(73).
unsat(74):-not sat(74).
unsat(75):-not sat(75).
unsat(76):-not sat(76).
unsat(77):-not sat(77).
unsat(78):-not sat(78).
unsat(79):-not sat(79).
unsat(8):-not sat(8).
unsat(80):-not sat(80).
unsat(81):-not sat(81).
unsat(82):-not sat(82).
unsat(83):-not sat(83).
unsat(84):-not sat(84).
unsat(85):-not sat(85).
unsat(86):-not sat(86).
unsat(87):-not sat(87).
unsat(88):-not sat(88).
unsat(89):-not sat(89).
unsat(9):-not sat(9).
unsat(90):-not sat(90).
unsat(91):-not sat(91).
unsat(92):-not sat(92).
unsat(93):-not sat(93).
unsat(94):-not sat(94).
unsat(95):-not sat(95).
unsat(96):-not sat(96).
unsat(97):-not sat(97).
unsat(98):-not sat(98).
unsat(99):-not sat(99).
#show unsat/1.