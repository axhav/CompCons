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
	| HappyAbsSyn20 (Expr)
	| HappyAbsSyn21 ([Type])
	| HappyAbsSyn22 (Bracket)
	| HappyAbsSyn29 ([Expr])
	| HappyAbsSyn30 (AddOp)
	| HappyAbsSyn31 (MulOp)
	| HappyAbsSyn32 (RelOp)

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
 action_111,
 action_112,
 action_113,
 action_114,
 action_115,
 action_116,
 action_117,
 action_118,
 action_119,
 action_120,
 action_121,
 action_122,
 action_123,
 action_124,
 action_125,
 action_126,
 action_127,
 action_128,
 action_129,
 action_130,
 action_131,
 action_132,
 action_133,
 action_134 :: () => Int -> ({-HappyReduction (Err) = -}
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
 happyReduce_73,
 happyReduce_74,
 happyReduce_75,
 happyReduce_76,
 happyReduce_77,
 happyReduce_78,
 happyReduce_79,
 happyReduce_80,
 happyReduce_81,
 happyReduce_82 :: () => ({-HappyReduction (Err) = -}
	   Int 
	-> (Token)
	-> HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)
	-> [HappyState (Token) (HappyStk HappyAbsSyn -> [(Token)] -> (Err) HappyAbsSyn)] 
	-> HappyStk HappyAbsSyn 
	-> [(Token)] -> (Err) HappyAbsSyn)

action_0 (37) = happyShift action_8
action_0 (57) = happyShift action_9
action_0 (58) = happyShift action_10
action_0 (63) = happyShift action_11
action_0 (67) = happyShift action_12
action_0 (8) = happyGoto action_3
action_0 (9) = happyGoto action_4
action_0 (10) = happyGoto action_5
action_0 (18) = happyGoto action_6
action_0 (19) = happyGoto action_7
action_0 _ = happyFail

action_1 (72) = happyShift action_2
action_1 _ = happyFail

action_2 _ = happyReduce_1

action_3 (76) = happyAccept
action_3 _ = happyFail

action_4 (37) = happyShift action_8
action_4 (57) = happyShift action_9
action_4 (58) = happyShift action_10
action_4 (63) = happyShift action_11
action_4 (67) = happyShift action_12
action_4 (9) = happyGoto action_4
action_4 (10) = happyGoto action_17
action_4 (18) = happyGoto action_6
action_4 (19) = happyGoto action_7
action_4 _ = happyReduce_7

action_5 _ = happyReduce_5

action_6 (55) = happyShift action_16
action_6 (22) = happyGoto action_15
action_6 _ = happyReduce_39

action_7 (72) = happyShift action_2
action_7 (4) = happyGoto action_14
action_7 _ = happyFail

action_8 (37) = happyShift action_8
action_8 (57) = happyShift action_9
action_8 (58) = happyShift action_10
action_8 (63) = happyShift action_11
action_8 (67) = happyShift action_12
action_8 (18) = happyGoto action_6
action_8 (19) = happyGoto action_13
action_8 _ = happyFail

action_9 _ = happyReduce_35

action_10 _ = happyReduce_34

action_11 _ = happyReduce_33

action_12 _ = happyReduce_36

action_13 (38) = happyShift action_40
action_13 _ = happyFail

action_14 (37) = happyShift action_39
action_14 _ = happyFail

action_15 _ = happyReduce_38

action_16 (33) = happyShift action_30
action_16 (37) = happyShift action_31
action_16 (43) = happyShift action_32
action_16 (60) = happyShift action_33
action_16 (64) = happyShift action_34
action_16 (66) = happyShift action_35
action_16 (72) = happyShift action_2
action_16 (73) = happyShift action_36
action_16 (74) = happyShift action_37
action_16 (75) = happyShift action_38
action_16 (4) = happyGoto action_18
action_16 (5) = happyGoto action_19
action_16 (6) = happyGoto action_20
action_16 (7) = happyGoto action_21
action_16 (20) = happyGoto action_22
action_16 (23) = happyGoto action_23
action_16 (24) = happyGoto action_24
action_16 (25) = happyGoto action_25
action_16 (26) = happyGoto action_26
action_16 (27) = happyGoto action_27
action_16 (28) = happyGoto action_28
action_16 (29) = happyGoto action_29
action_16 _ = happyReduce_69

action_17 _ = happyReduce_8

action_18 (37) = happyShift action_68
action_18 _ = happyReduce_48

action_19 _ = happyReduce_49

action_20 _ = happyReduce_50

action_21 _ = happyReduce_54

action_22 (42) = happyShift action_67
action_22 _ = happyReduce_70

action_23 (45) = happyShift action_66
action_23 (55) = happyShift action_16
action_23 (22) = happyGoto action_65
action_23 _ = happyReduce_60

action_24 _ = happyReduce_62

action_25 (35) = happyShift action_62
action_25 (39) = happyShift action_63
action_25 (46) = happyShift action_64
action_25 (31) = happyGoto action_61
action_25 _ = happyReduce_64

action_26 (40) = happyShift action_59
action_26 (43) = happyShift action_60
action_26 (30) = happyGoto action_58
action_26 _ = happyReduce_66

action_27 (34) = happyShift action_51
action_27 (36) = happyShift action_52
action_27 (49) = happyShift action_53
action_27 (50) = happyShift action_54
action_27 (52) = happyShift action_55
action_27 (53) = happyShift action_56
action_27 (54) = happyShift action_57
action_27 (32) = happyGoto action_50
action_27 _ = happyReduce_68

action_28 (70) = happyShift action_49
action_28 _ = happyReduce_42

action_29 (56) = happyShift action_48
action_29 _ = happyFail

action_30 (37) = happyShift action_31
action_30 (60) = happyShift action_33
action_30 (66) = happyShift action_35
action_30 (72) = happyShift action_2
action_30 (73) = happyShift action_36
action_30 (74) = happyShift action_37
action_30 (75) = happyShift action_38
action_30 (4) = happyGoto action_18
action_30 (5) = happyGoto action_19
action_30 (6) = happyGoto action_20
action_30 (7) = happyGoto action_21
action_30 (23) = happyGoto action_47
action_30 _ = happyFail

action_31 (33) = happyShift action_30
action_31 (37) = happyShift action_31
action_31 (43) = happyShift action_32
action_31 (60) = happyShift action_33
action_31 (64) = happyShift action_34
action_31 (66) = happyShift action_35
action_31 (72) = happyShift action_2
action_31 (73) = happyShift action_36
action_31 (74) = happyShift action_37
action_31 (75) = happyShift action_38
action_31 (4) = happyGoto action_18
action_31 (5) = happyGoto action_19
action_31 (6) = happyGoto action_20
action_31 (7) = happyGoto action_21
action_31 (20) = happyGoto action_46
action_31 (23) = happyGoto action_23
action_31 (24) = happyGoto action_24
action_31 (25) = happyGoto action_25
action_31 (26) = happyGoto action_26
action_31 (27) = happyGoto action_27
action_31 (28) = happyGoto action_28
action_31 _ = happyFail

action_32 (37) = happyShift action_31
action_32 (60) = happyShift action_33
action_32 (66) = happyShift action_35
action_32 (72) = happyShift action_2
action_32 (73) = happyShift action_36
action_32 (74) = happyShift action_37
action_32 (75) = happyShift action_38
action_32 (4) = happyGoto action_18
action_32 (5) = happyGoto action_19
action_32 (6) = happyGoto action_20
action_32 (7) = happyGoto action_21
action_32 (23) = happyGoto action_45
action_32 _ = happyFail

action_33 _ = happyReduce_52

action_34 (37) = happyShift action_8
action_34 (57) = happyShift action_9
action_34 (58) = happyShift action_10
action_34 (63) = happyShift action_11
action_34 (67) = happyShift action_12
action_34 (18) = happyGoto action_6
action_34 (19) = happyGoto action_44
action_34 _ = happyFail

action_35 _ = happyReduce_51

action_36 _ = happyReduce_2

action_37 _ = happyReduce_3

action_38 _ = happyReduce_4

action_39 (37) = happyShift action_8
action_39 (57) = happyShift action_9
action_39 (58) = happyShift action_10
action_39 (63) = happyShift action_11
action_39 (67) = happyShift action_12
action_39 (11) = happyGoto action_41
action_39 (12) = happyGoto action_42
action_39 (18) = happyGoto action_6
action_39 (19) = happyGoto action_43
action_39 _ = happyReduce_10

action_40 _ = happyReduce_37

action_41 (42) = happyShift action_81
action_41 _ = happyReduce_11

action_42 (38) = happyShift action_80
action_42 _ = happyFail

action_43 (72) = happyShift action_2
action_43 (4) = happyGoto action_79
action_43 _ = happyFail

action_44 _ = happyReduce_41

action_45 (55) = happyShift action_16
action_45 (22) = happyGoto action_65
action_45 _ = happyReduce_58

action_46 (38) = happyShift action_78
action_46 _ = happyFail

action_47 (55) = happyShift action_16
action_47 (22) = happyGoto action_65
action_47 _ = happyReduce_59

action_48 (55) = happyShift action_16
action_48 (22) = happyGoto action_77
action_48 _ = happyReduce_47

action_49 (33) = happyShift action_30
action_49 (37) = happyShift action_31
action_49 (43) = happyShift action_32
action_49 (60) = happyShift action_33
action_49 (64) = happyShift action_34
action_49 (66) = happyShift action_35
action_49 (72) = happyShift action_2
action_49 (73) = happyShift action_36
action_49 (74) = happyShift action_37
action_49 (75) = happyShift action_38
action_49 (4) = happyGoto action_18
action_49 (5) = happyGoto action_19
action_49 (6) = happyGoto action_20
action_49 (7) = happyGoto action_21
action_49 (20) = happyGoto action_76
action_49 (23) = happyGoto action_23
action_49 (24) = happyGoto action_24
action_49 (25) = happyGoto action_25
action_49 (26) = happyGoto action_26
action_49 (27) = happyGoto action_27
action_49 (28) = happyGoto action_28
action_49 _ = happyFail

action_50 (33) = happyShift action_30
action_50 (37) = happyShift action_31
action_50 (43) = happyShift action_32
action_50 (60) = happyShift action_33
action_50 (66) = happyShift action_35
action_50 (72) = happyShift action_2
action_50 (73) = happyShift action_36
action_50 (74) = happyShift action_37
action_50 (75) = happyShift action_38
action_50 (4) = happyGoto action_18
action_50 (5) = happyGoto action_19
action_50 (6) = happyGoto action_20
action_50 (7) = happyGoto action_21
action_50 (23) = happyGoto action_23
action_50 (24) = happyGoto action_24
action_50 (25) = happyGoto action_25
action_50 (26) = happyGoto action_75
action_50 _ = happyFail

action_51 _ = happyReduce_82

action_52 (33) = happyShift action_30
action_52 (37) = happyShift action_31
action_52 (43) = happyShift action_32
action_52 (60) = happyShift action_33
action_52 (66) = happyShift action_35
action_52 (72) = happyShift action_2
action_52 (73) = happyShift action_36
action_52 (74) = happyShift action_37
action_52 (75) = happyShift action_38
action_52 (4) = happyGoto action_18
action_52 (5) = happyGoto action_19
action_52 (6) = happyGoto action_20
action_52 (7) = happyGoto action_21
action_52 (23) = happyGoto action_23
action_52 (24) = happyGoto action_24
action_52 (25) = happyGoto action_25
action_52 (26) = happyGoto action_26
action_52 (27) = happyGoto action_27
action_52 (28) = happyGoto action_74
action_52 _ = happyFail

action_53 _ = happyReduce_77

action_54 _ = happyReduce_78

action_55 _ = happyReduce_81

action_56 _ = happyReduce_79

action_57 _ = happyReduce_80

action_58 (33) = happyShift action_30
action_58 (37) = happyShift action_31
action_58 (43) = happyShift action_32
action_58 (60) = happyShift action_33
action_58 (66) = happyShift action_35
action_58 (72) = happyShift action_2
action_58 (73) = happyShift action_36
action_58 (74) = happyShift action_37
action_58 (75) = happyShift action_38
action_58 (4) = happyGoto action_18
action_58 (5) = happyGoto action_19
action_58 (6) = happyGoto action_20
action_58 (7) = happyGoto action_21
action_58 (23) = happyGoto action_23
action_58 (24) = happyGoto action_24
action_58 (25) = happyGoto action_73
action_58 _ = happyFail

action_59 _ = happyReduce_72

action_60 _ = happyReduce_73

action_61 (33) = happyShift action_30
action_61 (37) = happyShift action_31
action_61 (43) = happyShift action_32
action_61 (60) = happyShift action_33
action_61 (66) = happyShift action_35
action_61 (72) = happyShift action_2
action_61 (73) = happyShift action_36
action_61 (74) = happyShift action_37
action_61 (75) = happyShift action_38
action_61 (4) = happyGoto action_18
action_61 (5) = happyGoto action_19
action_61 (6) = happyGoto action_20
action_61 (7) = happyGoto action_21
action_61 (23) = happyGoto action_23
action_61 (24) = happyGoto action_72
action_61 _ = happyFail

action_62 _ = happyReduce_76

action_63 _ = happyReduce_74

action_64 _ = happyReduce_75

action_65 _ = happyReduce_55

action_66 (37) = happyShift action_31
action_66 (60) = happyShift action_33
action_66 (66) = happyShift action_35
action_66 (72) = happyShift action_2
action_66 (73) = happyShift action_36
action_66 (74) = happyShift action_37
action_66 (75) = happyShift action_38
action_66 (4) = happyGoto action_18
action_66 (5) = happyGoto action_19
action_66 (6) = happyGoto action_20
action_66 (7) = happyGoto action_21
action_66 (23) = happyGoto action_71
action_66 _ = happyFail

action_67 (33) = happyShift action_30
action_67 (37) = happyShift action_31
action_67 (43) = happyShift action_32
action_67 (60) = happyShift action_33
action_67 (64) = happyShift action_34
action_67 (66) = happyShift action_35
action_67 (72) = happyShift action_2
action_67 (73) = happyShift action_36
action_67 (74) = happyShift action_37
action_67 (75) = happyShift action_38
action_67 (4) = happyGoto action_18
action_67 (5) = happyGoto action_19
action_67 (6) = happyGoto action_20
action_67 (7) = happyGoto action_21
action_67 (20) = happyGoto action_22
action_67 (23) = happyGoto action_23
action_67 (24) = happyGoto action_24
action_67 (25) = happyGoto action_25
action_67 (26) = happyGoto action_26
action_67 (27) = happyGoto action_27
action_67 (28) = happyGoto action_28
action_67 (29) = happyGoto action_70
action_67 _ = happyReduce_69

action_68 (33) = happyShift action_30
action_68 (37) = happyShift action_31
action_68 (43) = happyShift action_32
action_68 (60) = happyShift action_33
action_68 (64) = happyShift action_34
action_68 (66) = happyShift action_35
action_68 (72) = happyShift action_2
action_68 (73) = happyShift action_36
action_68 (74) = happyShift action_37
action_68 (75) = happyShift action_38
action_68 (4) = happyGoto action_18
action_68 (5) = happyGoto action_19
action_68 (6) = happyGoto action_20
action_68 (7) = happyGoto action_21
action_68 (20) = happyGoto action_22
action_68 (23) = happyGoto action_23
action_68 (24) = happyGoto action_24
action_68 (25) = happyGoto action_25
action_68 (26) = happyGoto action_26
action_68 (27) = happyGoto action_27
action_68 (28) = happyGoto action_28
action_68 (29) = happyGoto action_69
action_68 _ = happyReduce_69

action_69 (38) = happyShift action_85
action_69 _ = happyFail

action_70 _ = happyReduce_71

action_71 (55) = happyShift action_16
action_71 (22) = happyGoto action_65
action_71 _ = happyReduce_57

action_72 _ = happyReduce_61

action_73 (35) = happyShift action_62
action_73 (39) = happyShift action_63
action_73 (46) = happyShift action_64
action_73 (31) = happyGoto action_61
action_73 _ = happyReduce_63

action_74 _ = happyReduce_67

action_75 (40) = happyShift action_59
action_75 (43) = happyShift action_60
action_75 (30) = happyGoto action_58
action_75 _ = happyReduce_65

action_76 _ = happyReduce_40

action_77 _ = happyReduce_46

action_78 _ = happyReduce_56

action_79 _ = happyReduce_9

action_80 (69) = happyShift action_84
action_80 (13) = happyGoto action_83
action_80 _ = happyFail

action_81 (37) = happyShift action_8
action_81 (57) = happyShift action_9
action_81 (58) = happyShift action_10
action_81 (63) = happyShift action_11
action_81 (67) = happyShift action_12
action_81 (11) = happyGoto action_41
action_81 (12) = happyGoto action_82
action_81 (18) = happyGoto action_6
action_81 (19) = happyGoto action_43
action_81 _ = happyReduce_10

action_82 _ = happyReduce_12

action_83 _ = happyReduce_6

action_84 (14) = happyGoto action_86
action_84 _ = happyReduce_14

action_85 _ = happyReduce_53

action_86 (33) = happyShift action_30
action_86 (37) = happyShift action_92
action_86 (43) = happyShift action_32
action_86 (48) = happyShift action_93
action_86 (57) = happyShift action_9
action_86 (58) = happyShift action_10
action_86 (60) = happyShift action_33
action_86 (61) = happyShift action_94
action_86 (62) = happyShift action_95
action_86 (63) = happyShift action_11
action_86 (64) = happyShift action_34
action_86 (65) = happyShift action_96
action_86 (66) = happyShift action_35
action_86 (67) = happyShift action_12
action_86 (68) = happyShift action_97
action_86 (69) = happyShift action_84
action_86 (71) = happyShift action_98
action_86 (72) = happyShift action_2
action_86 (73) = happyShift action_36
action_86 (74) = happyShift action_37
action_86 (75) = happyShift action_38
action_86 (4) = happyGoto action_87
action_86 (5) = happyGoto action_19
action_86 (6) = happyGoto action_20
action_86 (7) = happyGoto action_21
action_86 (13) = happyGoto action_88
action_86 (15) = happyGoto action_89
action_86 (18) = happyGoto action_6
action_86 (19) = happyGoto action_90
action_86 (20) = happyGoto action_91
action_86 (23) = happyGoto action_23
action_86 (24) = happyGoto action_24
action_86 (25) = happyGoto action_25
action_86 (26) = happyGoto action_26
action_86 (27) = happyGoto action_27
action_86 (28) = happyGoto action_28
action_86 _ = happyFail

action_87 (37) = happyShift action_68
action_87 (41) = happyShift action_109
action_87 (44) = happyShift action_110
action_87 _ = happyReduce_48

action_88 _ = happyReduce_17

action_89 _ = happyReduce_15

action_90 (72) = happyShift action_2
action_90 (4) = happyGoto action_106
action_90 (16) = happyGoto action_107
action_90 (17) = happyGoto action_108
action_90 _ = happyFail

action_91 (48) = happyShift action_104
action_91 (51) = happyShift action_105
action_91 _ = happyFail

action_92 (33) = happyShift action_30
action_92 (37) = happyShift action_92
action_92 (43) = happyShift action_32
action_92 (57) = happyShift action_9
action_92 (58) = happyShift action_10
action_92 (60) = happyShift action_33
action_92 (63) = happyShift action_11
action_92 (64) = happyShift action_34
action_92 (66) = happyShift action_35
action_92 (67) = happyShift action_12
action_92 (72) = happyShift action_2
action_92 (73) = happyShift action_36
action_92 (74) = happyShift action_37
action_92 (75) = happyShift action_38
action_92 (4) = happyGoto action_18
action_92 (5) = happyGoto action_19
action_92 (6) = happyGoto action_20
action_92 (7) = happyGoto action_21
action_92 (18) = happyGoto action_6
action_92 (19) = happyGoto action_13
action_92 (20) = happyGoto action_46
action_92 (23) = happyGoto action_23
action_92 (24) = happyGoto action_24
action_92 (25) = happyGoto action_25
action_92 (26) = happyGoto action_26
action_92 (27) = happyGoto action_27
action_92 (28) = happyGoto action_28
action_92 _ = happyFail

action_93 _ = happyReduce_16

action_94 (37) = happyShift action_103
action_94 _ = happyFail

action_95 (37) = happyShift action_102
action_95 _ = happyFail

action_96 (33) = happyShift action_30
action_96 (37) = happyShift action_31
action_96 (43) = happyShift action_32
action_96 (48) = happyShift action_101
action_96 (60) = happyShift action_33
action_96 (64) = happyShift action_34
action_96 (66) = happyShift action_35
action_96 (72) = happyShift action_2
action_96 (73) = happyShift action_36
action_96 (74) = happyShift action_37
action_96 (75) = happyShift action_38
action_96 (4) = happyGoto action_18
action_96 (5) = happyGoto action_19
action_96 (6) = happyGoto action_20
action_96 (7) = happyGoto action_21
action_96 (20) = happyGoto action_100
action_96 (23) = happyGoto action_23
action_96 (24) = happyGoto action_24
action_96 (25) = happyGoto action_25
action_96 (26) = happyGoto action_26
action_96 (27) = happyGoto action_27
action_96 (28) = happyGoto action_28
action_96 _ = happyFail

action_97 (37) = happyShift action_99
action_97 _ = happyFail

action_98 _ = happyReduce_13

action_99 (33) = happyShift action_30
action_99 (37) = happyShift action_31
action_99 (43) = happyShift action_32
action_99 (60) = happyShift action_33
action_99 (64) = happyShift action_34
action_99 (66) = happyShift action_35
action_99 (72) = happyShift action_2
action_99 (73) = happyShift action_36
action_99 (74) = happyShift action_37
action_99 (75) = happyShift action_38
action_99 (4) = happyGoto action_18
action_99 (5) = happyGoto action_19
action_99 (6) = happyGoto action_20
action_99 (7) = happyGoto action_21
action_99 (20) = happyGoto action_120
action_99 (23) = happyGoto action_23
action_99 (24) = happyGoto action_24
action_99 (25) = happyGoto action_25
action_99 (26) = happyGoto action_26
action_99 (27) = happyGoto action_27
action_99 (28) = happyGoto action_28
action_99 _ = happyFail

action_100 (48) = happyShift action_119
action_100 _ = happyFail

action_101 _ = happyReduce_23

action_102 (33) = happyShift action_30
action_102 (37) = happyShift action_31
action_102 (43) = happyShift action_32
action_102 (60) = happyShift action_33
action_102 (64) = happyShift action_34
action_102 (66) = happyShift action_35
action_102 (72) = happyShift action_2
action_102 (73) = happyShift action_36
action_102 (74) = happyShift action_37
action_102 (75) = happyShift action_38
action_102 (4) = happyGoto action_18
action_102 (5) = happyGoto action_19
action_102 (6) = happyGoto action_20
action_102 (7) = happyGoto action_21
action_102 (20) = happyGoto action_118
action_102 (23) = happyGoto action_23
action_102 (24) = happyGoto action_24
action_102 (25) = happyGoto action_25
action_102 (26) = happyGoto action_26
action_102 (27) = happyGoto action_27
action_102 (28) = happyGoto action_28
action_102 _ = happyFail

action_103 (37) = happyShift action_8
action_103 (57) = happyShift action_9
action_103 (58) = happyShift action_10
action_103 (63) = happyShift action_11
action_103 (67) = happyShift action_12
action_103 (18) = happyGoto action_6
action_103 (19) = happyGoto action_117
action_103 _ = happyFail

action_104 _ = happyReduce_28

action_105 (33) = happyShift action_30
action_105 (37) = happyShift action_31
action_105 (43) = happyShift action_32
action_105 (60) = happyShift action_33
action_105 (64) = happyShift action_34
action_105 (66) = happyShift action_35
action_105 (72) = happyShift action_2
action_105 (73) = happyShift action_36
action_105 (74) = happyShift action_37
action_105 (75) = happyShift action_38
action_105 (4) = happyGoto action_18
action_105 (5) = happyGoto action_19
action_105 (6) = happyGoto action_20
action_105 (7) = happyGoto action_21
action_105 (20) = happyGoto action_116
action_105 (23) = happyGoto action_23
action_105 (24) = happyGoto action_24
action_105 (25) = happyGoto action_25
action_105 (26) = happyGoto action_26
action_105 (27) = happyGoto action_27
action_105 (28) = happyGoto action_28
action_105 _ = happyFail

action_106 (51) = happyShift action_115
action_106 _ = happyReduce_29

action_107 (42) = happyShift action_114
action_107 _ = happyReduce_31

action_108 (48) = happyShift action_113
action_108 _ = happyFail

action_109 (48) = happyShift action_112
action_109 _ = happyFail

action_110 (48) = happyShift action_111
action_110 _ = happyFail

action_111 _ = happyReduce_21

action_112 _ = happyReduce_20

action_113 _ = happyReduce_18

action_114 (72) = happyShift action_2
action_114 (4) = happyGoto action_106
action_114 (16) = happyGoto action_107
action_114 (17) = happyGoto action_126
action_114 _ = happyFail

action_115 (33) = happyShift action_30
action_115 (37) = happyShift action_31
action_115 (43) = happyShift action_32
action_115 (60) = happyShift action_33
action_115 (64) = happyShift action_34
action_115 (66) = happyShift action_35
action_115 (72) = happyShift action_2
action_115 (73) = happyShift action_36
action_115 (74) = happyShift action_37
action_115 (75) = happyShift action_38
action_115 (4) = happyGoto action_18
action_115 (5) = happyGoto action_19
action_115 (6) = happyGoto action_20
action_115 (7) = happyGoto action_21
action_115 (20) = happyGoto action_125
action_115 (23) = happyGoto action_23
action_115 (24) = happyGoto action_24
action_115 (25) = happyGoto action_25
action_115 (26) = happyGoto action_26
action_115 (27) = happyGoto action_27
action_115 (28) = happyGoto action_28
action_115 _ = happyFail

action_116 (48) = happyShift action_124
action_116 _ = happyFail

action_117 (72) = happyShift action_2
action_117 (4) = happyGoto action_123
action_117 _ = happyFail

action_118 (38) = happyShift action_122
action_118 _ = happyFail

action_119 _ = happyReduce_22

action_120 (38) = happyShift action_121
action_120 _ = happyFail

action_121 (33) = happyShift action_30
action_121 (37) = happyShift action_92
action_121 (43) = happyShift action_32
action_121 (48) = happyShift action_93
action_121 (57) = happyShift action_9
action_121 (58) = happyShift action_10
action_121 (60) = happyShift action_33
action_121 (61) = happyShift action_94
action_121 (62) = happyShift action_95
action_121 (63) = happyShift action_11
action_121 (64) = happyShift action_34
action_121 (65) = happyShift action_96
action_121 (66) = happyShift action_35
action_121 (67) = happyShift action_12
action_121 (68) = happyShift action_97
action_121 (69) = happyShift action_84
action_121 (72) = happyShift action_2
action_121 (73) = happyShift action_36
action_121 (74) = happyShift action_37
action_121 (75) = happyShift action_38
action_121 (4) = happyGoto action_87
action_121 (5) = happyGoto action_19
action_121 (6) = happyGoto action_20
action_121 (7) = happyGoto action_21
action_121 (13) = happyGoto action_88
action_121 (15) = happyGoto action_129
action_121 (18) = happyGoto action_6
action_121 (19) = happyGoto action_90
action_121 (20) = happyGoto action_91
action_121 (23) = happyGoto action_23
action_121 (24) = happyGoto action_24
action_121 (25) = happyGoto action_25
action_121 (26) = happyGoto action_26
action_121 (27) = happyGoto action_27
action_121 (28) = happyGoto action_28
action_121 _ = happyFail

action_122 (33) = happyShift action_30
action_122 (37) = happyShift action_92
action_122 (43) = happyShift action_32
action_122 (48) = happyShift action_93
action_122 (57) = happyShift action_9
action_122 (58) = happyShift action_10
action_122 (60) = happyShift action_33
action_122 (61) = happyShift action_94
action_122 (62) = happyShift action_95
action_122 (63) = happyShift action_11
action_122 (64) = happyShift action_34
action_122 (65) = happyShift action_96
action_122 (66) = happyShift action_35
action_122 (67) = happyShift action_12
action_122 (68) = happyShift action_97
action_122 (69) = happyShift action_84
action_122 (72) = happyShift action_2
action_122 (73) = happyShift action_36
action_122 (74) = happyShift action_37
action_122 (75) = happyShift action_38
action_122 (4) = happyGoto action_87
action_122 (5) = happyGoto action_19
action_122 (6) = happyGoto action_20
action_122 (7) = happyGoto action_21
action_122 (13) = happyGoto action_88
action_122 (15) = happyGoto action_128
action_122 (18) = happyGoto action_6
action_122 (19) = happyGoto action_90
action_122 (20) = happyGoto action_91
action_122 (23) = happyGoto action_23
action_122 (24) = happyGoto action_24
action_122 (25) = happyGoto action_25
action_122 (26) = happyGoto action_26
action_122 (27) = happyGoto action_27
action_122 (28) = happyGoto action_28
action_122 _ = happyFail

action_123 (47) = happyShift action_127
action_123 _ = happyFail

action_124 _ = happyReduce_19

action_125 _ = happyReduce_30

action_126 _ = happyReduce_32

action_127 (33) = happyShift action_30
action_127 (37) = happyShift action_31
action_127 (43) = happyShift action_32
action_127 (60) = happyShift action_33
action_127 (64) = happyShift action_34
action_127 (66) = happyShift action_35
action_127 (72) = happyShift action_2
action_127 (73) = happyShift action_36
action_127 (74) = happyShift action_37
action_127 (75) = happyShift action_38
action_127 (4) = happyGoto action_18
action_127 (5) = happyGoto action_19
action_127 (6) = happyGoto action_20
action_127 (7) = happyGoto action_21
action_127 (20) = happyGoto action_131
action_127 (23) = happyGoto action_23
action_127 (24) = happyGoto action_24
action_127 (25) = happyGoto action_25
action_127 (26) = happyGoto action_26
action_127 (27) = happyGoto action_27
action_127 (28) = happyGoto action_28
action_127 _ = happyFail

action_128 (59) = happyShift action_130
action_128 _ = happyReduce_24

action_129 _ = happyReduce_26

action_130 (33) = happyShift action_30
action_130 (37) = happyShift action_92
action_130 (43) = happyShift action_32
action_130 (48) = happyShift action_93
action_130 (57) = happyShift action_9
action_130 (58) = happyShift action_10
action_130 (60) = happyShift action_33
action_130 (61) = happyShift action_94
action_130 (62) = happyShift action_95
action_130 (63) = happyShift action_11
action_130 (64) = happyShift action_34
action_130 (65) = happyShift action_96
action_130 (66) = happyShift action_35
action_130 (67) = happyShift action_12
action_130 (68) = happyShift action_97
action_130 (69) = happyShift action_84
action_130 (72) = happyShift action_2
action_130 (73) = happyShift action_36
action_130 (74) = happyShift action_37
action_130 (75) = happyShift action_38
action_130 (4) = happyGoto action_87
action_130 (5) = happyGoto action_19
action_130 (6) = happyGoto action_20
action_130 (7) = happyGoto action_21
action_130 (13) = happyGoto action_88
action_130 (15) = happyGoto action_133
action_130 (18) = happyGoto action_6
action_130 (19) = happyGoto action_90
action_130 (20) = happyGoto action_91
action_130 (23) = happyGoto action_23
action_130 (24) = happyGoto action_24
action_130 (25) = happyGoto action_25
action_130 (26) = happyGoto action_26
action_130 (27) = happyGoto action_27
action_130 (28) = happyGoto action_28
action_130 _ = happyFail

action_131 (38) = happyShift action_132
action_131 _ = happyFail

action_132 (33) = happyShift action_30
action_132 (37) = happyShift action_92
action_132 (43) = happyShift action_32
action_132 (48) = happyShift action_93
action_132 (57) = happyShift action_9
action_132 (58) = happyShift action_10
action_132 (60) = happyShift action_33
action_132 (61) = happyShift action_94
action_132 (62) = happyShift action_95
action_132 (63) = happyShift action_11
action_132 (64) = happyShift action_34
action_132 (65) = happyShift action_96
action_132 (66) = happyShift action_35
action_132 (67) = happyShift action_12
action_132 (68) = happyShift action_97
action_132 (69) = happyShift action_84
action_132 (72) = happyShift action_2
action_132 (73) = happyShift action_36
action_132 (74) = happyShift action_37
action_132 (75) = happyShift action_38
action_132 (4) = happyGoto action_87
action_132 (5) = happyGoto action_19
action_132 (6) = happyGoto action_20
action_132 (7) = happyGoto action_21
action_132 (13) = happyGoto action_88
action_132 (15) = happyGoto action_134
action_132 (18) = happyGoto action_6
action_132 (19) = happyGoto action_90
action_132 (20) = happyGoto action_91
action_132 (23) = happyGoto action_23
action_132 (24) = happyGoto action_24
action_132 (25) = happyGoto action_25
action_132 (26) = happyGoto action_26
action_132 (27) = happyGoto action_27
action_132 (28) = happyGoto action_28
action_132 _ = happyFail

action_133 _ = happyReduce_25

action_134 _ = happyReduce_27

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
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_1) `HappyStk`
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
	(HappyAbsSyn20  happy_var_2)
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
	(HappyAbsSyn20  happy_var_3) `HappyStk`
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
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (CondElse happy_var_3 happy_var_5 happy_var_7
	) `HappyStk` happyRest

happyReduce_26 = happyReduce 5 15 happyReduction_26
happyReduction_26 ((HappyAbsSyn15  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (While happy_var_3 happy_var_5
	) `HappyStk` happyRest

happyReduce_27 = happyReduce 8 15 happyReduction_27
happyReduction_27 ((HappyAbsSyn15  happy_var_8) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn20  happy_var_6) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_4) `HappyStk`
	(HappyAbsSyn18  happy_var_3) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (ForEach happy_var_3 happy_var_4 happy_var_6 happy_var_8
	) `HappyStk` happyRest

happyReduce_28 = happySpecReduce_2  15 happyReduction_28
happyReduction_28 _
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn15
		 (SExp happy_var_1
	)
happyReduction_28 _ _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_1  16 happyReduction_29
happyReduction_29 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (NoInit happy_var_1
	)
happyReduction_29 _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_3  16 happyReduction_30
happyReduction_30 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn16
		 (Init happy_var_1 happy_var_3
	)
happyReduction_30 _ _ _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  17 happyReduction_31
happyReduction_31 (HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:[]) happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_3  17 happyReduction_32
happyReduction_32 (HappyAbsSyn17  happy_var_3)
	_
	(HappyAbsSyn16  happy_var_1)
	 =  HappyAbsSyn17
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_32 _ _ _  = notHappyAtAll 

happyReduce_33 = happySpecReduce_1  18 happyReduction_33
happyReduction_33 _
	 =  HappyAbsSyn18
		 (Int
	)

happyReduce_34 = happySpecReduce_1  18 happyReduction_34
happyReduction_34 _
	 =  HappyAbsSyn18
		 (Doub
	)

happyReduce_35 = happySpecReduce_1  18 happyReduction_35
happyReduction_35 _
	 =  HappyAbsSyn18
		 (Bool
	)

happyReduce_36 = happySpecReduce_1  18 happyReduction_36
happyReduction_36 _
	 =  HappyAbsSyn18
		 (Void
	)

happyReduce_37 = happySpecReduce_3  18 happyReduction_37
happyReduction_37 _
	(HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn18
		 (happy_var_2
	)
happyReduction_37 _ _ _  = notHappyAtAll 

happyReduce_38 = happySpecReduce_2  19 happyReduction_38
happyReduction_38 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (ArrayT happy_var_1 happy_var_2
	)
happyReduction_38 _ _  = notHappyAtAll 

happyReduce_39 = happySpecReduce_1  19 happyReduction_39
happyReduction_39 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn18
		 (happy_var_1
	)
happyReduction_39 _  = notHappyAtAll 

happyReduce_40 = happySpecReduce_3  20 happyReduction_40
happyReduction_40 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EOr happy_var_1 happy_var_3
	)
happyReduction_40 _ _ _  = notHappyAtAll 

happyReduce_41 = happySpecReduce_2  20 happyReduction_41
happyReduction_41 (HappyAbsSyn18  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (EArr happy_var_2
	)
happyReduction_41 _ _  = notHappyAtAll 

happyReduce_42 = happySpecReduce_1  20 happyReduction_42
happyReduction_42 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_42 _  = notHappyAtAll 

happyReduce_43 = happySpecReduce_0  21 happyReduction_43
happyReduction_43  =  HappyAbsSyn21
		 ([]
	)

happyReduce_44 = happySpecReduce_1  21 happyReduction_44
happyReduction_44 (HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn21
		 ((:[]) happy_var_1
	)
happyReduction_44 _  = notHappyAtAll 

happyReduce_45 = happySpecReduce_3  21 happyReduction_45
happyReduction_45 (HappyAbsSyn21  happy_var_3)
	_
	(HappyAbsSyn18  happy_var_1)
	 =  HappyAbsSyn21
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_45 _ _ _  = notHappyAtAll 

happyReduce_46 = happyReduce 4 22 happyReduction_46
happyReduction_46 ((HappyAbsSyn22  happy_var_4) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn29  happy_var_2) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn22
		 (Brackets happy_var_2 happy_var_4
	) `HappyStk` happyRest

happyReduce_47 = happySpecReduce_3  22 happyReduction_47
happyReduction_47 _
	(HappyAbsSyn29  happy_var_2)
	_
	 =  HappyAbsSyn22
		 (NoBracket happy_var_2
	)
happyReduction_47 _ _ _  = notHappyAtAll 

happyReduce_48 = happySpecReduce_1  23 happyReduction_48
happyReduction_48 (HappyAbsSyn4  happy_var_1)
	 =  HappyAbsSyn20
		 (EVar happy_var_1
	)
happyReduction_48 _  = notHappyAtAll 

happyReduce_49 = happySpecReduce_1  23 happyReduction_49
happyReduction_49 (HappyAbsSyn5  happy_var_1)
	 =  HappyAbsSyn20
		 (ELitInt happy_var_1
	)
happyReduction_49 _  = notHappyAtAll 

happyReduce_50 = happySpecReduce_1  23 happyReduction_50
happyReduction_50 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn20
		 (ELitDoub happy_var_1
	)
happyReduction_50 _  = notHappyAtAll 

happyReduce_51 = happySpecReduce_1  23 happyReduction_51
happyReduction_51 _
	 =  HappyAbsSyn20
		 (ELitTrue
	)

happyReduce_52 = happySpecReduce_1  23 happyReduction_52
happyReduction_52 _
	 =  HappyAbsSyn20
		 (ELitFalse
	)

happyReduce_53 = happyReduce 4 23 happyReduction_53
happyReduction_53 (_ `HappyStk`
	(HappyAbsSyn29  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn4  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn20
		 (EApp happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyReduce_54 = happySpecReduce_1  23 happyReduction_54
happyReduction_54 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn20
		 (EString happy_var_1
	)
happyReduction_54 _  = notHappyAtAll 

happyReduce_55 = happySpecReduce_2  23 happyReduction_55
happyReduction_55 (HappyAbsSyn22  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EIndex happy_var_1 happy_var_2
	)
happyReduction_55 _ _  = notHappyAtAll 

happyReduce_56 = happySpecReduce_3  23 happyReduction_56
happyReduction_56 _
	(HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (happy_var_2
	)
happyReduction_56 _ _ _  = notHappyAtAll 

happyReduce_57 = happySpecReduce_3  24 happyReduction_57
happyReduction_57 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EDot happy_var_1 happy_var_3
	)
happyReduction_57 _ _ _  = notHappyAtAll 

happyReduce_58 = happySpecReduce_2  24 happyReduction_58
happyReduction_58 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Neg happy_var_2
	)
happyReduction_58 _ _  = notHappyAtAll 

happyReduce_59 = happySpecReduce_2  24 happyReduction_59
happyReduction_59 (HappyAbsSyn20  happy_var_2)
	_
	 =  HappyAbsSyn20
		 (Not happy_var_2
	)
happyReduction_59 _ _  = notHappyAtAll 

happyReduce_60 = happySpecReduce_1  24 happyReduction_60
happyReduction_60 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_60 _  = notHappyAtAll 

happyReduce_61 = happySpecReduce_3  25 happyReduction_61
happyReduction_61 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn31  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EMul happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_61 _ _ _  = notHappyAtAll 

happyReduce_62 = happySpecReduce_1  25 happyReduction_62
happyReduction_62 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_62 _  = notHappyAtAll 

happyReduce_63 = happySpecReduce_3  26 happyReduction_63
happyReduction_63 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn30  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAdd happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_63 _ _ _  = notHappyAtAll 

happyReduce_64 = happySpecReduce_1  26 happyReduction_64
happyReduction_64 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_64 _  = notHappyAtAll 

happyReduce_65 = happySpecReduce_3  27 happyReduction_65
happyReduction_65 (HappyAbsSyn20  happy_var_3)
	(HappyAbsSyn32  happy_var_2)
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (ERel happy_var_1 happy_var_2 happy_var_3
	)
happyReduction_65 _ _ _  = notHappyAtAll 

happyReduce_66 = happySpecReduce_1  27 happyReduction_66
happyReduction_66 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_66 _  = notHappyAtAll 

happyReduce_67 = happySpecReduce_3  28 happyReduction_67
happyReduction_67 (HappyAbsSyn20  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (EAnd happy_var_1 happy_var_3
	)
happyReduction_67 _ _ _  = notHappyAtAll 

happyReduce_68 = happySpecReduce_1  28 happyReduction_68
happyReduction_68 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn20
		 (happy_var_1
	)
happyReduction_68 _  = notHappyAtAll 

happyReduce_69 = happySpecReduce_0  29 happyReduction_69
happyReduction_69  =  HappyAbsSyn29
		 ([]
	)

happyReduce_70 = happySpecReduce_1  29 happyReduction_70
happyReduction_70 (HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn29
		 ((:[]) happy_var_1
	)
happyReduction_70 _  = notHappyAtAll 

happyReduce_71 = happySpecReduce_3  29 happyReduction_71
happyReduction_71 (HappyAbsSyn29  happy_var_3)
	_
	(HappyAbsSyn20  happy_var_1)
	 =  HappyAbsSyn29
		 ((:) happy_var_1 happy_var_3
	)
happyReduction_71 _ _ _  = notHappyAtAll 

happyReduce_72 = happySpecReduce_1  30 happyReduction_72
happyReduction_72 _
	 =  HappyAbsSyn30
		 (Plus
	)

happyReduce_73 = happySpecReduce_1  30 happyReduction_73
happyReduction_73 _
	 =  HappyAbsSyn30
		 (Minus
	)

happyReduce_74 = happySpecReduce_1  31 happyReduction_74
happyReduction_74 _
	 =  HappyAbsSyn31
		 (Times
	)

happyReduce_75 = happySpecReduce_1  31 happyReduction_75
happyReduction_75 _
	 =  HappyAbsSyn31
		 (Div
	)

happyReduce_76 = happySpecReduce_1  31 happyReduction_76
happyReduction_76 _
	 =  HappyAbsSyn31
		 (Mod
	)

happyReduce_77 = happySpecReduce_1  32 happyReduction_77
happyReduction_77 _
	 =  HappyAbsSyn32
		 (LTH
	)

happyReduce_78 = happySpecReduce_1  32 happyReduction_78
happyReduction_78 _
	 =  HappyAbsSyn32
		 (LE
	)

happyReduce_79 = happySpecReduce_1  32 happyReduction_79
happyReduction_79 _
	 =  HappyAbsSyn32
		 (GTH
	)

happyReduce_80 = happySpecReduce_1  32 happyReduction_80
happyReduction_80 _
	 =  HappyAbsSyn32
		 (GE
	)

happyReduce_81 = happySpecReduce_1  32 happyReduction_81
happyReduction_81 _
	 =  HappyAbsSyn32
		 (EQU
	)

happyReduce_82 = happySpecReduce_1  32 happyReduction_82
happyReduction_82 _
	 =  HappyAbsSyn32
		 (NE
	)

happyNewToken action sts stk [] =
	action 76 76 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	PT _ (TS _ 1) -> cont 33;
	PT _ (TS _ 2) -> cont 34;
	PT _ (TS _ 3) -> cont 35;
	PT _ (TS _ 4) -> cont 36;
	PT _ (TS _ 5) -> cont 37;
	PT _ (TS _ 6) -> cont 38;
	PT _ (TS _ 7) -> cont 39;
	PT _ (TS _ 8) -> cont 40;
	PT _ (TS _ 9) -> cont 41;
	PT _ (TS _ 10) -> cont 42;
	PT _ (TS _ 11) -> cont 43;
	PT _ (TS _ 12) -> cont 44;
	PT _ (TS _ 13) -> cont 45;
	PT _ (TS _ 14) -> cont 46;
	PT _ (TS _ 15) -> cont 47;
	PT _ (TS _ 16) -> cont 48;
	PT _ (TS _ 17) -> cont 49;
	PT _ (TS _ 18) -> cont 50;
	PT _ (TS _ 19) -> cont 51;
	PT _ (TS _ 20) -> cont 52;
	PT _ (TS _ 21) -> cont 53;
	PT _ (TS _ 22) -> cont 54;
	PT _ (TS _ 23) -> cont 55;
	PT _ (TS _ 24) -> cont 56;
	PT _ (TS _ 25) -> cont 57;
	PT _ (TS _ 26) -> cont 58;
	PT _ (TS _ 27) -> cont 59;
	PT _ (TS _ 28) -> cont 60;
	PT _ (TS _ 29) -> cont 61;
	PT _ (TS _ 30) -> cont 62;
	PT _ (TS _ 31) -> cont 63;
	PT _ (TS _ 32) -> cont 64;
	PT _ (TS _ 33) -> cont 65;
	PT _ (TS _ 34) -> cont 66;
	PT _ (TS _ 35) -> cont 67;
	PT _ (TS _ 36) -> cont 68;
	PT _ (TS _ 37) -> cont 69;
	PT _ (TS _ 38) -> cont 70;
	PT _ (TS _ 39) -> cont 71;
	PT _ (TV happy_dollar_dollar) -> cont 72;
	PT _ (TI happy_dollar_dollar) -> cont 73;
	PT _ (TD happy_dollar_dollar) -> cont 74;
	PT _ (TL happy_dollar_dollar) -> cont 75;
	_ -> happyError' (tk:tks)
	}

happyError_ 76 tk tks = happyError' tks
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
