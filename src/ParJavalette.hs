{-# OPTIONS_GHC -w #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ParJavalette where
import AbsJavalette
import LexJavalette
import ErrM

-- parser produced by Happy Version 1.18.10

data HappyAbsSyn 
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 (Ident)
	| HappyAbsSyn5 (Integer)
	| HappyAbsSyn6 (Double)
	| HappyAbsSyn7 (String)
	| HappyAbsSyn8 (Program)
	| HappyAbsSyn9 (TopDef)
	| HappyAbsSyn10 ([TopDef])
	| HappyAbsSyn11 (Arg)
	| HappyAbsSyn12 ([Arg])
	| HappyAbsSyn13 (Block)
	| HappyAbsSyn14 ([Stmt])
	| HappyAbsSyn15 (Stmt)
	| HappyAbsSyn16 (Item)
	| HappyAbsSyn17 ([Item])
	| HappyAbsSyn18 (Type)
	| HappyAbsSyn19 (Expr)
	| HappyAbsSyn20 ([Type])
	| HappyAbsSyn27 ([Expr])
	| HappyAbsSyn28 (AddOp)
	| HappyAbsSyn29 (MulOp)
	| HappyAbsSyn30 (RelOp)

{- to allow type-synonyms as our monads (likely
 - with explicitly-specified bind and return)
 - in Haskell98, it seems that with
 - /type M a = .../, then /(HappyReduction M)/
 - is not allowed.  But Happy is a
 - code-generator that can just substitute it.
type HappyReduction m = 
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> m HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> m HappyAbsSyn
-}

action_0,
 action_1,
 action_2,
 action_3,
 action_4,
 action_5,
 action_6,
 action_7,
 action_8,
 action_9,
 action_10,
 action_11,
 action_12,
 action_13,
 action_14,
 action_15,
 action_16,
 action_17,
 action_18,
 action_19,
 action_20,
 action_21,
 action_22,
 action_23,
 action_24,
 action_25,
 action_26,
 action_27,
 action_28,
 action_29,
 action_30,
 action_31,
 action_32,
 action_33,
 action_34,
 action_35,
 action_36,
 action_37,
 action_38,
 action_39,
 action_40,
 action_41,
 action_42,
 action_43,
 action_44,
 action_45,
 action_46,
 action_47,
 action_48,
 action_49,
 action_50,
 action_51,
 action_52,
 action_53,
 action_54,
 action_55,
 action_56,
 action_57,
 action_58,
 action_59,
 action_60,
 action_61,
 action_62,
 action_63,
 action_64,
 action_65,
 action_66,
 action_67,
 action_68,
 action_69,
 action_70,
 action_71,
 action_72,
 action_73,
 action_74,
 action_75,
 action_76,
 action_77,
 action_78,
 action_79,
 action_80,
 action_81,
 action_82,
 action_83,
 action_84,
 action_85,
 action_86,
 action_87,
 action_88,
 action_89,
 action_90,
 action_91,
 action_92,
 action_93,
 action_94,
 action_95,
 action_96,
 action_97,
 action_98,
 action_99,
 action_100,
 action_101,
 action_102,
 action_103,
 action_104,
 action_105,
 action_106,
 action_107,
 action_108,
 action_109,
 action_110,
 action_111 :: () => Int -> ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

happyReduce_1,
 happyReduce_2,
 happyReduce_3,
 happyReduce_4,
 happyReduce_5,
 happyReduce_6,
 happyReduce_7,
 happyReduce_8,
 happyReduce_9,
 happyReduce_10,
 happyReduce_11,
 happyReduce_12,
 happyReduce_13,
 happyReduce_14,
 happyReduce_15,
 happyReduce_16,
 happyReduce_17,
 happyReduce_18,
 happyReduce_19,
 happyReduce_20,
 happyReduce_21,
 happyReduce_22,
 happyReduce_23,
 happyReduce_24,
 happyReduce_25,
 happyReduce_26,
 happyReduce_27,
 happyReduce_28,
 happyReduce_29,
 happyReduce_30,
 happyReduce_31,
 happyReduce_32,
 happyReduce_33,
 happyReduce_34,
 happyReduce_35,
 happyReduce_36,
 happyReduce_37,
 happyReduce_38,
 happyReduce_39,
 happyReduce_40,
 happyReduce_41,
 happyReduce_42,
 happyReduce_43,
 happyReduce_44,
 happyReduce_45,
 happyReduce_46,
 happyReduce_47,
 happyReduce_48,
 happyReduce_49,
 happyReduce_50,
 happyReduce_51,
 happyReduce_52,
 happyReduce_53,
 happyReduce_54,
 happyReduce_55,
 happyReduce_56,
 happyReduce_57,
 happyReduce_58,
 happyReduce_59,
 happyReduce_60,
 happyReduce_61,
 happyReduce_62,
 happyReduce_63,
 happyReduce_64,
 happyReduce_65,
 happyReduce_66,
 happyReduce_67,
 happyReduce_68,
 happyReduce_69,
 happyReduce_70,
 happyReduce_71,
 happyReduce_72,
 happyReduce_73 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (54) = happyShift action_7
action_0 (55) = happyShift action_8
action_0 (59) = happyShift action_9
action_0 (62) = happyShift action_10
action_0 (8) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (18) = happyGoto action_6
action_0 _ = happyFail

action_1 (67) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (71) = happyAccept
action_3 _ = happyFail

action_4 (54) = happyShift action_7
action_4 (55) = happyShift action_8
action_4 (59) = happyShift action_9
action_4 (62) = happyShift action_10
action_4 (9) = happyGoto action_4
action_4 (10) = happyGoto action_12
action_4 (18) = happyGoto action_6
action_4 _ = happyReduce_7

action_5 _ = happyReduce_5

action_6 (67) = happyShift action_2
action_6 (4) = happyGoto action_11
action_6 _ = happyFail

action_7 _ = happyReduce_34

action_8 _ = happyReduce_33

action_9 _ = happyReduce_32

action_10 _ = happyReduce_35

action_11 (35) = happyShift action_13
action_11 _ = happyFail

action_12 _ = happyReduce_8

action_13 (54) = happyShift action_7
action_13 (55) = happyShift action_8
action_13 (59) = happyShift action_9
action_13 (62) = happyShift action_10
action_13 (11) = happyGoto action_14
action_13 (12) = happyGoto action_15
action_13 (18) = happyGoto action_16
action_13 _ = happyReduce_10

action_14 (40) = happyShift action_19
action_14 _ = happyReduce_11

action_15 (36) = happyShift action_18
action_15 _ = happyFail

action_16 (67) = happyShift action_2
action_16 (4) = happyGoto action_17
action_16 _ = happyFail

action_17 _ = happyReduce_9

action_18 (64) = happyShift action_22
action_18 (13) = happyGoto action_21
action_18 _ = happyFail

action_19 (54) = happyShift action_7
action_19 (55) = happyShift action_8
action_19 (59) = happyShift action_9
action_19 (62) = happyShift action_10
action_19 (11) = happyGoto action_14
action_19 (12) = happyGoto action_20
action_19 (18) = happyGoto action_16
action_19 _ = happyReduce_10

action_20 _ = happyReduce_12

action_21 _ = happyReduce_6

action_22 (14) = happyGoto action_23
action_22 _ = happyReduce_14

action_23 (31) = happyShift action_38
action_23 (35) = happyShift action_39
action_23 (41) = happyShift action_40
action_23 (45) = happyShift action_41
action_23 (54) = happyShift action_7
action_23 (55) = happyShift action_8
action_23 (57) = happyShift action_42
action_23 (58) = happyShift action_43
action_23 (59) = happyShift action_9
action_23 (60) = happyShift action_44
action_23 (61) = happyShift action_45
action_23 (62) = happyShift action_10
action_23 (63) = happyShift action_46
action_23 (64) = happyShift action_22
action_23 (66) = happyShift action_47
action_23 (67) = happyShift action_2
action_23 (68) = happyShift action_48
action_23 (69) = happyShift action_49
action_23 (70) = happyShift action_50
action_23 (4) = happyGoto action_24
action_23 (5) = happyGoto action_25
action_23 (6) = happyGoto action_26
action_23 (7) = happyGoto action_27
action_23 (13) = happyGoto action_28
action_23 (15) = happyGoto action_29
action_23 (18) = happyGoto action_30
action_23 (19) = happyGoto action_31
action_23 (21) = happyGoto action_32
action_23 (22) = happyGoto action_33
action_23 (23) = happyGoto action_34
action_23 (24) = happyGoto action_35
action_23 (25) = happyGoto action_36
action_23 (26) = happyGoto action_37
action_23 _ = happyFail

action_24 (35) = happyShift action_79
action_24 (39) = happyShift action_80
action_24 (42) = happyShift action_81
action_24 (48) = happyShift action_82
action_24 _ = happyReduce_41

action_25 _ = happyReduce_42

action_26 _ = happyReduce_43

action_27 _ = happyReduce_47

action_28 _ = happyReduce_17

action_29 _ = happyReduce_15

action_30 (67) = happyShift action_2
action_30 (4) = happyGoto action_76
action_30 (16) = happyGoto action_77
action_30 (17) = happyGoto action_78
action_30 _ = happyFail

action_31 (45) = happyShift action_75
action_31 _ = happyFail

action_32 _ = happyReduce_51

action_33 _ = happyReduce_53

action_34 (33) = happyShift action_72
action_34 (37) = happyShift action_73
action_34 (43) = happyShift action_74
action_34 (29) = happyGoto action_71
action_34 _ = happyReduce_55

action_35 (38) = happyShift action_69
action_35 (41) = happyShift action_70
action_35 (28) = happyGoto action_68
action_35 _ = happyReduce_57

action_36 (32) = happyShift action_61
action_36 (34) = happyShift action_62
action_36 (46) = happyShift action_63
action_36 (47) = happyShift action_64
action_36 (49) = happyShift action_65
action_36 (50) = happyShift action_66
action_36 (51) = happyShift action_67
action_36 (30) = happyGoto action_60
action_36 _ = happyReduce_59

action_37 (65) = happyShift action_59
action_37 _ = happyReduce_37

action_38 (35) = happyShift action_39
action_38 (57) = happyShift action_42
action_38 (61) = happyShift action_45
action_38 (67) = happyShift action_2
action_38 (68) = happyShift action_48
action_38 (69) = happyShift action_49
action_38 (70) = happyShift action_50
action_38 (4) = happyGoto action_52
action_38 (5) = happyGoto action_25
action_38 (6) = happyGoto action_26
action_38 (7) = happyGoto action_27
action_38 (21) = happyGoto action_58
action_38 _ = happyFail

action_39 (31) = happyShift action_38
action_39 (35) = happyShift action_39
action_39 (41) = happyShift action_40
action_39 (57) = happyShift action_42
action_39 (61) = happyShift action_45
action_39 (67) = happyShift action_2
action_39 (68) = happyShift action_48
action_39 (69) = happyShift action_49
action_39 (70) = happyShift action_50
action_39 (4) = happyGoto action_52
action_39 (5) = happyGoto action_25
action_39 (6) = happyGoto action_26
action_39 (7) = happyGoto action_27
action_39 (19) = happyGoto action_57
action_39 (21) = happyGoto action_32
action_39 (22) = happyGoto action_33
action_39 (23) = happyGoto action_34
action_39 (24) = happyGoto action_35
action_39 (25) = happyGoto action_36
action_39 (26) = happyGoto action_37
action_39 _ = happyFail

action_40 (35) = happyShift action_39
action_40 (57) = happyShift action_42
action_40 (61) = happyShift action_45
action_40 (67) = happyShift action_2
action_40 (68) = happyShift action_48
action_40 (69) = happyShift action_49
action_40 (70) = happyShift action_50
action_40 (4) = happyGoto action_52
action_40 (5) = happyGoto action_25
action_40 (6) = happyGoto action_26
action_40 (7) = happyGoto action_27
action_40 (21) = happyGoto action_56
action_40 _ = happyFail

action_41 _ = happyReduce_16

action_42 _ = happyReduce_45

action_43 (35) = happyShift action_55
action_43 _ = happyFail

action_44 (31) = happyShift action_38
action_44 (35) = happyShift action_39
action_44 (41) = happyShift action_40
action_44 (45) = happyShift action_54
action_44 (57) = happyShift action_42
action_44 (61) = happyShift action_45
action_44 (67) = happyShift action_2
action_44 (68) = happyShift action_48
action_44 (69) = happyShift action_49
action_44 (70) = happyShift action_50
action_44 (4) = happyGoto action_52
action_44 (5) = happyGoto action_25
action_44 (6) = happyGoto action_26
action_44 (7) = happyGoto action_27
action_44 (19) = happyGoto action_53
action_44 (21) = happyGoto action_32
action_44 (22) = happyGoto action_33
action_44 (23) = happyGoto action_34
action_44 (24) = happyGoto action_35
action_44 (25) = happyGoto action_36
action_44 (26) = happyGoto action_37
action_44 _ = happyFail

action_45 _ = happyReduce_44

action_46 (35) = happyShift action_51
action_46 _ = happyFail

action_47 _ = happyReduce_13

action_48 _ = happyReduce_2

action_49 _ = happyReduce_3

action_50 _ = happyReduce_4

action_51 (31) = happyShift action_38
action_51 (35) = happyShift action_39
action_51 (41) = happyShift action_40
action_51 (57) = happyShift action_42
action_51 (61) = happyShift action_45
action_51 (67) = happyShift action_2
action_51 (68) = happyShift action_48
action_51 (69) = happyShift action_49
action_51 (70) = happyShift action_50
action_51 (4) = happyGoto action_52
action_51 (5) = happyGoto action_25
action_51 (6) = happyGoto action_26
action_51 (7) = happyGoto action_27
action_51 (19) = happyGoto action_99
action_51 (21) = happyGoto action_32
action_51 (22) = happyGoto action_33
action_51 (23) = happyGoto action_34
action_51 (24) = happyGoto action_35
action_51 (25) = happyGoto action_36
action_51 (26) = happyGoto action_37
action_51 _ = happyFail

action_52 (35) = happyShift action_79
action_52 _ = happyReduce_41

action_53 (45) = happyShift action_98
action_53 _ = happyFail

action_54 _ = happyReduce_23

action_55 (31) = happyShift action_38
action_55 (35) = happyShift action_39
action_55 (41) = happyShift action_40
action_55 (57) = happyShift action_42
action_55 (61) = happyShift action_45
action_55 (67) = happyShift action_2
action_55 (68) = happyShift action_48
action_55 (69) = happyShift action_49
action_55 (70) = happyShift action_50
action_55 (4) = happyGoto action_52
action_55 (5) = happyGoto action_25
action_55 (6) = happyGoto action_26
action_55 (7) = happyGoto action_27
action_55 (19) = happyGoto action_97
action_55 (21) = happyGoto action_32
action_55 (22) = happyGoto action_33
action_55 (23) = happyGoto action_34
action_55 (24) = happyGoto action_35
action_55 (25) = happyGoto action_36
action_55 (26) = happyGoto action_37
action_55 _ = happyFail

action_56 _ = happyReduce_49

action_57 (36) = happyShift action_96
action_57 _ = happyFail

action_58 _ = happyReduce_50

action_59 (31) = happyShift action_38
action_59 (35) = happyShift action_39
action_59 (41) = happyShift action_40
action_59 (57) = happyShift action_42
action_59 (61) = happyShift action_45
action_59 (67) = happyShift action_2
action_59 (68) = happyShift action_48
action_59 (69) = happyShift action_49
action_59 (70) = happyShift action_50
action_59 (4) = happyGoto action_52
action_59 (5) = happyGoto action_25
action_59 (6) = happyGoto action_26
action_59 (7) = happyGoto action_27
action_59 (19) = happyGoto action_95
action_59 (21) = happyGoto action_32
action_59 (22) = happyGoto action_33
action_59 (23) = happyGoto action_34
action_59 (24) = happyGoto action_35
action_59 (25) = happyGoto action_36
action_59 (26) = happyGoto action_37
action_59 _ = happyFail

action_60 (31) = happyShift action_38
action_60 (35) = happyShift action_39
action_60 (41) = happyShift action_40
action_60 (57) = happyShift action_42
action_60 (61) = happyShift action_45
action_60 (67) = happyShift action_2
action_60 (68) = happyShift action_48
action_60 (69) = happyShift action_49
action_60 (70) = happyShift action_50
action_60 (4) = happyGoto action_52
action_60 (5) = happyGoto action_25
action_60 (6) = happyGoto action_26
action_60 (7) = happyGoto action_27
action_60 (21) = happyGoto action_32
action_60 (22) = happyGoto action_33
action_60 (23) = happyGoto action_34
action_60 (24) = happyGoto action_94
action_60 _ = happyFail

action_61 _ = happyReduce_73

action_62 (31) = happyShift action_38
action_62 (35) = happyShift action_39
action_62 (41) = happyShift action_40
action_62 (57) = happyShift action_42
action_62 (61) = happyShift action_45
action_62 (67) = happyShift action_2
action_62 (68) = happyShift action_48
action_62 (69) = happyShift action_49
action_62 (70) = happyShift action_50
action_62 (4) = happyGoto action_52
action_62 (5) = happyGoto action_25
action_62 (6) = happyGoto action_26
action_62 (7) = happyGoto action_27
action_62 (21) = happyGoto action_32
action_62 (22) = happyGoto action_33
action_62 (23) = happyGoto action_34
action_62 (24) = happyGoto action_35
action_62 (25) = happyGoto action_36
action_62 (26) = happyGoto action_93
action_62 _ = happyFail

action_63 _ = happyReduce_68

action_64 _ = happyReduce_69

action_65 _ = happyReduce_72

action_66 _ = happyReduce_70

action_67 _ = happyReduce_71

action_68 (31) = happyShift action_38
action_68 (35) = happyShift action_39
action_68 (41) = happyShift action_40
action_68 (57) = happyShift action_42
action_68 (61) = happyShift action_45
action_68 (67) = happyShift action_2
action_68 (68) = happyShift action_48
action_68 (69) = happyShift action_49
action_68 (70) = happyShift action_50
action_68 (4) = happyGoto action_52
action_68 (5) = happyGoto action_25
action_68 (6) = happyGoto action_26
action_68 (7) = happyGoto action_27
action_68 (21) = happyGoto action_32
action_68 (22) = happyGoto action_33
action_68 (23) = happyGoto action_92
action_68 _ = happyFail

action_69 _ = happyReduce_63

action_70 _ = happyReduce_64

action_71 (31) = happyShift action_38
action_71 (35) = happyShift action_39
action_71 (41) = happyShift action_40
action_71 (57) = happyShift action_42
action_71 (61) = happyShift action_45
action_71 (67) = happyShift action_2
action_71 (68) = happyShift action_48
action_71 (69) = happyShift action_49
action_71 (70) = happyShift action_50
action_71 (4) = happyGoto action_52
action_71 (5) = happyGoto action_25
action_71 (6) = happyGoto action_26
action_71 (7) = happyGoto action_27
action_71 (21) = happyGoto action_32
action_71 (22) = happyGoto action_91
action_71 _ = happyFail

action_72 _ = happyReduce_67

action_73 _ = happyReduce_65

action_74 _ = happyReduce_66

action_75 _ = happyReduce_27

action_76 (48) = happyShift action_90
action_76 _ = happyReduce_28

action_77 (40) = happyShift action_89
action_77 _ = happyReduce_30

action_78 (45) = happyShift action_88
action_78 _ = happyFail

action_79 (31) = happyShift action_38
action_79 (35) = happyShift action_39
action_79 (41) = happyShift action_40
action_79 (57) = happyShift action_42
action_79 (61) = happyShift action_45
action_79 (67) = happyShift action_2
action_79 (68) = happyShift action_48
action_79 (69) = happyShift action_49
action_79 (70) = happyShift action_50
action_79 (4) = happyGoto action_52
action_79 (5) = happyGoto action_25
action_79 (6) = happyGoto action_26
action_79 (7) = happyGoto action_27
action_79 (19) = happyGoto action_86
action_79 (21) = happyGoto action_32
action_79 (22) = happyGoto action_33
action_79 (23) = happyGoto action_34
action_79 (24) = happyGoto action_35
action_79 (25) = happyGoto action_36
action_79 (26) = happyGoto action_37
action_79 (27) = happyGoto action_87
action_79 _ = happyReduce_60

action_80 (45) = happyShift action_85
action_80 _ = happyFail

action_81 (45) = happyShift action_84
action_81 _ = happyFail

action_82 (31) = happyShift action_38
action_82 (35) = happyShift action_39
action_82 (41) = happyShift action_40
action_82 (57) = happyShift action_42
action_82 (61) = happyShift action_45
action_82 (67) = happyShift action_2
action_82 (68) = happyShift action_48
action_82 (69) = happyShift action_49
action_82 (70) = happyShift action_50
action_82 (4) = happyGoto action_52
action_82 (5) = happyGoto action_25
action_82 (6) = happyGoto action_26
action_82 (7) = happyGoto action_27
action_82 (19) = happyGoto action_83
action_82 (21) = happyGoto action_32
action_82 (22) = happyGoto action_33
action_82 (23) = happyGoto action_34
action_82 (24) = happyGoto action_35
action_82 (25) = happyGoto action_36
action_82 (26) = happyGoto action_37
action_82 _ = happyFail

action_83 (45) = happyShift action_106
action_83 _ = happyFail

action_84 _ = happyReduce_21

action_85 _ = happyReduce_20

action_86 (40) = happyShift action_105
action_86 _ = happyReduce_61

action_87 (36) = happyShift action_104
action_87 _ = happyFail

action_88 _ = happyReduce_18

action_89 (67) = happyShift action_2
action_89 (4) = happyGoto action_76
action_89 (16) = happyGoto action_77
action_89 (17) = happyGoto action_103
action_89 _ = happyFail

action_90 (31) = happyShift action_38
action_90 (35) = happyShift action_39
action_90 (41) = happyShift action_40
action_90 (57) = happyShift action_42
action_90 (61) = happyShift action_45
action_90 (67) = happyShift action_2
action_90 (68) = happyShift action_48
action_90 (69) = happyShift action_49
action_90 (70) = happyShift action_50
action_90 (4) = happyGoto action_52
action_90 (5) = happyGoto action_25
action_90 (6) = happyGoto action_26
action_90 (7) = happyGoto action_27
action_90 (19) = happyGoto action_102
action_90 (21) = happyGoto action_32
action_90 (22) = happyGoto action_33
action_90 (23) = happyGoto action_34
action_90 (24) = happyGoto action_35
action_90 (25) = happyGoto action_36
action_90 (26) = happyGoto action_37
action_90 _ = happyFail

action_91 _ = happyReduce_52

action_92 (33) = happyShift action_72
action_92 (37) = happyShift action_73
action_92 (43) = happyShift action_74
action_92 (29) = happyGoto action_71
action_92 _ = happyReduce_54

action_93 _ = happyReduce_58

action_94 (38) = happyShift action_69
action_94 (41) = happyShift action_70
action_94 (28) = happyGoto action_68
action_94 _ = happyReduce_56

action_95 _ = happyReduce_36

action_96 _ = happyReduce_48

action_97 (36) = happyShift action_101
action_97 _ = happyFail

action_98 _ = happyReduce_22

action_99 (36) = happyShift action_100
action_99 _ = happyFail

action_100 (31) = happyShift action_38
action_100 (35) = happyShift action_39
action_100 (41) = happyShift action_40
action_100 (45) = happyShift action_41
action_100 (54) = happyShift action_7
action_100 (55) = happyShift action_8
action_100 (57) = happyShift action_42
action_100 (58) = happyShift action_43
action_100 (59) = happyShift action_9
action_100 (60) = happyShift action_44
action_100 (61) = happyShift action_45
action_100 (62) = happyShift action_10
action_100 (63) = happyShift action_46
action_100 (64) = happyShift action_22
action_100 (67) = happyShift action_2
action_100 (68) = happyShift action_48
action_100 (69) = happyShift action_49
action_100 (70) = happyShift action_50
action_100 (4) = happyGoto action_24
action_100 (5) = happyGoto action_25
action_100 (6) = happyGoto action_26
action_100 (7) = happyGoto action_27
action_100 (13) = happyGoto action_28
action_100 (15) = happyGoto action_109
action_100 (18) = happyGoto action_30
action_100 (19) = happyGoto action_31
action_100 (21) = happyGoto action_32
action_100 (22) = happyGoto action_33
action_100 (23) = happyGoto action_34
action_100 (24) = happyGoto action_35
action_100 (25) = happyGoto action_36
action_100 (26) = happyGoto action_37
action_100 _ = happyFail

action_101 (31) = happyShift action_38
action_101 (35) = happyShift action_39
action_101 (41) = happyShift action_40
action_101 (45) = happyShift action_41
action_101 (54) = happyShift action_7
action_101 (55) = happyShift action_8
action_101 (57) = happyShift action_42
action_101 (58) = happyShift action_43
action_101 (59) = happyShift action_9
action_101 (60) = happyShift action_44
action_101 (61) = happyShift action_45
action_101 (62) = happyShift action_10
action_101 (63) = happyShift action_46
action_101 (64) = happyShift action_22
action_101 (67) = happyShift action_2
action_101 (68) = happyShift action_48
action_101 (69) = happyShift action_49
action_101 (70) = happyShift action_50
action_101 (4) = happyGoto action_24
action_101 (5) = happyGoto action_25
action_101 (6) = happyGoto action_26
action_101 (7) = happyGoto action_27
action_101 (13) = happyGoto action_28
action_101 (15) = happyGoto action_108
action_101 (18) = happyGoto action_30
action_101 (19) = happyGoto action_31
action_101 (21) = happyGoto action_32
action_101 (22) = happyGoto action_33
action_101 (23) = happyGoto action_34
action_101 (24) = happyGoto action_35
action_101 (25) = happyGoto action_36
action_101 (26) = happyGoto action_37
action_101 _ = happyFail

action_102 _ = happyReduce_29

action_103 _ = happyReduce_31

action_104 _ = happyReduce_46

action_105 (31) = happyShift action_38
action_105 (35) = happyShift action_39
action_105 (41) = happyShift action_40
action_105 (57) = happyShift action_42
action_105 (61) = happyShift action_45
action_105 (67) = happyShift action_2
action_105 (68) = happyShift action_48
action_105 (69) = happyShift action_49
action_105 (70) = happyShift action_50
action_105 (4) = happyGoto action_52
action_105 (5) = happyGoto action_25
action_105 (6) = happyGoto action_26
action_105 (7) = happyGoto action_27
action_105 (19) = happyGoto action_86
action_105 (21) = happyGoto action_32
action_105 (22) = happyGoto action_33
action_105 (23) = happyGoto action_34
action_105 (24) = happyGoto action_35
action_105 (25) = happyGoto action_36
action_105 (26) = happyGoto action_37
action_105 (27) = happyGoto action_107
action_105 _ = happyReduce_60

action_106 _ = happyReduce_19

action_107 _ = happyReduce_62

action_108 (56) = happyShift action_110
action_108 _ = happyReduce_24

action_109 _ = happyReduce_26

action_110 (31) = happyShift action_38
action_110 (35) = happyShift action_39
action_110 (41) = happyShift action_40
action_110 (45) = happyShift action_41
action_110 (54) = happyShift action_7
action_110 (55) = happyShift action_8
action_110 (57) = happyShift action_42
action_110 (58) = happyShift action_43
action_110 (59) = happyShift action_9
action_110 (60) = happyShift action_44
action_110 (61) = happyShift action_45
action_110 (62) = happyShift action_10
action_110 (63) = happyShift action_46
action_110 (64) = happyShift action_22
action_110 (67) = happyShift action_2
action_110 (68) = happyShift action_48
action_110 (69) = happyShift action_49
action_110 (70) = happyShift action_50
action_110 (4) = happyGoto action_24
action_110 (5) = happyGoto action_25
action_110 (6) = happyGoto action_26
action_110 (7) = happyGoto action_27
action_110 (13) = happyGoto action_28
action_110 (15) = happyGoto action_111
action_110 (18) = happyGoto action_30
action_110 (19) = happyGoto action_31
action_110 (21) = happyGoto action_32
action_110 (22) = happyGoto action_33
action_110 (23) = happyGoto action_34
action_110 (24) = happyGoto action_35
action_110 (25) = happyGoto action_36
action_110 (26) = happyGoto action_37
action_110 _ = happyFail

action_111 _ = happyReduce_25

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyTerminal (PT _ (TV happy_var_1)))
	 =  HappyAbsSyn4
		 (Ident happy_var_1
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_1  5 happyReduction_2
happyReduction_2 (HappyTerminal (PT _ (TI happy_var_1)))
	 =  HappyAbsSyn5
		 ((read ( happy_var_1)) :: Integer
	)
happyReduction_2 _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  6 happyReduction_3
happyReduction_3 (HappyTerminal (PT _ (TD happy_var_1)))
	 =  HappyAbsSyn6
		 ((read ( happy_var_1)) :: Double
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_1  7 happyReduction_4
happyReduction_4 (HappyTerminal (PT _ (TL happy_var_1)))
	 =  HappyAbsSyn7
		 (happy_var_1
	)
happyReduction_4 _  = notHappyAtAll 

happyReduce_5 = happySpecReduce_1  8 happyReduction_5
happyReduction_5 (HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn8
		 (Program happy_var_1
	)
happyReduction_5 _  = notHappyAtAll 

happyReduce_6 = happyReduce 6 9 happyReduction_6
happyReduction_6 ((HappyAbsSyn13  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_2) `HappyStk`
	(HappyAbsSyn18  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn9
		 (FnDef happy_var_1 happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  10 happyReduction_7
happyReduction_7 (HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:[]) happy_var_1
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  10 happyReduction_8
happyReduction_8 (HappyAbsSyn10  happy_var_2)
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn10
		 ((:) happy_var_1 happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_2  11 happyReduction_9
happyReduction_9 (HappyAbsSyn4  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn11
		 (Arg happy_var_1 happy_var_2
	)
happyReduction_9 _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_0  12 happyReduction_10
happyReduction_10  =  HappyAbsSyn12
		 ([]
	)

happyReduce_11 = happySpecReduce_1  12 happyReduction_11
happyReduction_11 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:[]) happy_var_1
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  12 happyReduction_12
happyReduction_12 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  13 happyReduction_13
happyReduction_13 _
	(HappyAbsSyn14  happy_var_2)
	_
	 =  HappyAbsSyn13
		 (Block (reverse happy_var_2)
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_0  14 happyReduction_14
happyReduction_14  =  HappyAbsSyn14
		 ([]
	)

happyReduce_15 = happySpecReduce_2  14 happyReduction_15
happyReduction_15 (HappyAbsSyn15  happy_var_2)
	(HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn14
		 (flip (:) happy_var_1 happy_var_2
	)
happyReduction_15 _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_1  15 happyReduction_16
happyReduction_16 _
	 =  HappyAbsSyn15
		 (Empty
	)

happyReduce_17 = happySpecReduce_1  15 happyReduction_17
happyReduction_17 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn15
		 (BStmt happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  15 happyReduction_18
happyReduction_18 _
	(HappyAbsSyn17  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn15
		 (Decl happy_var_1 happy_var_2
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happyReduce 4 15 happyReduction_19
happyReduction_19 (_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Ass happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_20 = happySpecReduce_3  15 happyReduction_20
happyReduction_20 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (Incr happy_var_1
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  15 happyReduction_21
happyReduction_21 _
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn15
		 (Decr happy_var_1
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  15 happyReduction_22
happyReduction_22 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn15
		 (Ret happy_var_2
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_2  15 happyReduction_23
happyReduction_23 _
	_
	 =  HappyAbsSyn15
		 (VRet
	)

happyReduce_24 = happyReduce 5 15 happyReduction_24
happyReduction_24 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Cond happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_25 = happyReduce 7 15 happyReduction_25
happyReduction_25 ((HappyAbsSyn15  happy_var_7) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (CondElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 15 happyReduction_26
happyReduction_26 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn19  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happySpecReduce_2  15 happyReduction_27
happyReduction_27 _
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn15
		 (SExp happy_var_1
	)
happyReduction_27 _ _  = notHappyAtAll 

happyReduce_28 = happySpecReduce_1  16 happyReduction_28
happyReduction_28 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (NoInit happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  16 happyReduction_29
happyReduction_29 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (Init happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  17 happyReduction_30
happyReduction_30 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_3  17 happyReduction_31
happyReduction_31 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_31 _ _ _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  18 happyReduction_32
happyReduction_32 _
	 =  HappyAbsSyn18
		 (Int
	)

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn18
		 (Doub
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn18
		 (Bool
	)

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn18
		 (Void
	)

happyReduce_36 = happySpecReduce_3  19 happyReduction_36
happyReduction_36 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_36 _ _ _  = notHappyAtAll 

happyReduce_37 = happySpecReduce_1  19 happyReduction_37
happyReduction_37 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_37 _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_0  20 happyReduction_38
happyReduction_38  =  HappyAbsSyn20
		 ([]
	)

happyReduce_39 = happySpecReduce_1  20 happyReduction_39
happyReduction_39 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn20
		 ((:[]) happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  20 happyReduction_40
happyReduction_40 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn20
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_1  21 happyReduction_41
happyReduction_41 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn19
		 (EVar happy_var_1
	)
happyReduction_41 _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  21 happyReduction_42
happyReduction_42 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn19
		 (ELitInt happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_1  21 happyReduction_43
happyReduction_43 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn19
		 (ELitDoub happy_var_1
	)
happyReduction_43 _  = notHappyAtAll 

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 _
	 =  HappyAbsSyn19
		 (ELitTrue
	)

happyReduce_45 = happySpecReduce_1  21 happyReduction_45
happyReduction_45 _
	 =  HappyAbsSyn19
		 (ELitFalse
	)

happyReduce_46 = happyReduce 4 21 happyReduction_46
happyReduction_46 (_ `HappyStk`
	(HappyAbsSyn27  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn19
		 (EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_1  21 happyReduction_47
happyReduction_47 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn19
		 (EString happy_var_1
	)
happyReduction_47 _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_3  21 happyReduction_48
happyReduction_48 _
	(HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (happy_var_2
	)
happyReduction_48 _ _ _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_2  22 happyReduction_49
happyReduction_49 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Neg happy_var_2
	)
happyReduction_49 _ _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_2  22 happyReduction_50
happyReduction_50 (HappyAbsSyn19  happy_var_2)
	_
	 =  HappyAbsSyn19
		 (Not happy_var_2
	)
happyReduction_50 _ _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  22 happyReduction_51
happyReduction_51 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_51 _  = notHappyAtAll 

happyReduce_52 = happySpecReduce_3  23 happyReduction_52
happyReduction_52 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn29  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_52 _ _ _  = notHappyAtAll 

happyReduce_53 = happySpecReduce_1  23 happyReduction_53
happyReduction_53 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_53 _  = notHappyAtAll 

happyReduce_54 = happySpecReduce_3  24 happyReduction_54
happyReduction_54 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn28  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_54 _ _ _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_1  24 happyReduction_55
happyReduction_55 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_55 _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  25 happyReduction_56
happyReduction_56 (HappyAbsSyn19  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_1  25 happyReduction_57
happyReduction_57 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_57 _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_3  26 happyReduction_58
happyReduction_58 (HappyAbsSyn19  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_58 _ _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_1  26 happyReduction_59
happyReduction_59 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn19
		 (happy_var_1
	)
happyReduction_59 _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_0  27 happyReduction_60
happyReduction_60  =  HappyAbsSyn27
		 ([]
	)

happyReduce_61 = happySpecReduce_1  27 happyReduction_61
happyReduction_61 (HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn27
		 ((:[]) happy_var_1
	)
happyReduction_61 _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_3  27 happyReduction_62
happyReduction_62 (HappyAbsSyn27  happy_var_3)
	_
	(HappyAbsSyn19  happy_var_1)
	 =  HappyAbsSyn27
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_62 _ _ _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_1  28 happyReduction_63
happyReduction_63 _
	 =  HappyAbsSyn28
		 (Plus
	)

happyReduce_64 = happySpecReduce_1  28 happyReduction_64
happyReduction_64 _
	 =  HappyAbsSyn28
		 (Minus
	)

happyReduce_65 = happySpecReduce_1  29 happyReduction_65
happyReduction_65 _
	 =  HappyAbsSyn29
		 (Times
	)

happyReduce_66 = happySpecReduce_1  29 happyReduction_66
happyReduction_66 _
	 =  HappyAbsSyn29
		 (Div
	)

happyReduce_67 = happySpecReduce_1  29 happyReduction_67
happyReduction_67 _
	 =  HappyAbsSyn29
		 (Mod
	)

happyReduce_68 = happySpecReduce_1  30 happyReduction_68
happyReduction_68 _
	 =  HappyAbsSyn30
		 (LTH
	)

happyReduce_69 = happySpecReduce_1  30 happyReduction_69
happyReduction_69 _
	 =  HappyAbsSyn30
		 (LE
	)

happyReduce_70 = happySpecReduce_1  30 happyReduction_70
happyReduction_70 _
	 =  HappyAbsSyn30
		 (GTH
	)

happyReduce_71 = happySpecReduce_1  30 happyReduction_71
happyReduction_71 _
	 =  HappyAbsSyn30
		 (GE
	)

happyReduce_72 = happySpecReduce_1  30 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn30
		 (EQU
	)

happyReduce_73 = happySpecReduce_1  30 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn30
		 (NE
	)

happyNewToken action sts stk [] =
	action 71 71 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 31;
	PT _ (TS _ 2) -> cont 32;
	PT _ (TS _ 3) -> cont 33;
	PT _ (TS _ 4) -> cont 34;
	PT _ (TS _ 5) -> cont 35;
	PT _ (TS _ 6) -> cont 36;
	PT _ (TS _ 7) -> cont 37;
	PT _ (TS _ 8) -> cont 38;
	PT _ (TS _ 9) -> cont 39;
	PT _ (TS _ 10) -> cont 40;
	PT _ (TS _ 11) -> cont 41;
	PT _ (TS _ 12) -> cont 42;
	PT _ (TS _ 13) -> cont 43;
	PT _ (TS _ 14) -> cont 44;
	PT _ (TS _ 15) -> cont 45;
	PT _ (TS _ 16) -> cont 46;
	PT _ (TS _ 17) -> cont 47;
	PT _ (TS _ 18) -> cont 48;
	PT _ (TS _ 19) -> cont 49;
	PT _ (TS _ 20) -> cont 50;
	PT _ (TS _ 21) -> cont 51;
	PT _ (TS _ 22) -> cont 52;
	PT _ (TS _ 23) -> cont 53;
	PT _ (TS _ 24) -> cont 54;
	PT _ (TS _ 25) -> cont 55;
	PT _ (TS _ 26) -> cont 56;
	PT _ (TS _ 27) -> cont 57;
	PT _ (TS _ 28) -> cont 58;
	PT _ (TS _ 29) -> cont 59;
	PT _ (TS _ 30) -> cont 60;
	PT _ (TS _ 31) -> cont 61;
	PT _ (TS _ 32) -> cont 62;
	PT _ (TS _ 33) -> cont 63;
	PT _ (TS _ 34) -> cont 64;
	PT _ (TS _ 35) -> cont 65;
	PT _ (TS _ 36) -> cont 66;
	PT _ (TV happy_dollar_dollar) -> cont 67;
	PT _ (TI happy_dollar_dollar) -> cont 68;
	PT _ (TD happy_dollar_dollar) -> cont 69;
	PT _ (TL happy_dollar_dollar) -> cont 70;
	_ -> happyError' (tk:tks)
	}

happyError_ 71 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => Err a -> (a -> Err b) -> Err b
happyThen = (thenM)
happyReturn :: () => a -> Err a
happyReturn = (returnM)
happyThen1 m k tks = (thenM) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> Err a
happyReturn1 = \a tks -> (returnM) a
happyError' :: () => [(Token)] -> Err a
happyError' = happyError

pProgram tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn8 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


returnM :: a -> Err a
returnM = return

thenM :: Err a -> (a -> Err b) -> Err b
thenM = (>>=)

happyError :: [Token] -> Err a
happyError ts =
  Bad $ "syntax error at " ++ tokenPos ts ++ 
  case ts of
    [] -> []
    [Err _] -> " due to lexer error"
    _ -> " before " ++ unwords (map (id . prToken) (take 4 ts))

myLexer = tokens
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 30 "templates/GenericTemplate.hs" #-}








{-# LINE 51 "templates/GenericTemplate.hs" #-}

{-# LINE 61 "templates/GenericTemplate.hs" #-}

{-# LINE 70 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
	happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
	 (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 148 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
	 sts1@(((st1@(HappyState (action))):(_))) ->
        	let r = fn stk in  -- it doesn't hurt to always seq here...
       		happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
        happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
       happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))
       where (sts1@(((st1@(HappyState (action))):(_)))) = happyDrop k ((st):(sts))
             drop_stk = happyDropStk k stk





             new_state = action


happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 246 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let (i) = (case x of { HappyErrorToken (i) -> i }) in
--	trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
						(saved_tok `HappyStk` _ `HappyStk` stk) =
--	trace ("discarding state, depth " ++ show (length stk))  $
	action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
	action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--	happySeq = happyDoSeq
-- otherwise it emits
-- 	happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 312 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
