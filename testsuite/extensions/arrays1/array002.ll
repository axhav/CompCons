declare void @printInt(i32)
declare void @printString(i8*)
declare void @printDouble(double)
declare i32 @readInt()
declare double @readDouble()
declare i8* @calloc(i32, i32)

%arrInt = type %arrIntStruct*
%arrIntStruct = type { i32, [0 x i32]* }
define %arrInt @doubleArray(%arrInt %a) {

%t2 = getelementptr i32* null, i32 1
%t3 = ptrtoint i32* %t2 to i32
%t0 = alloca %arrIntStruct
%t7 = getelementptr %arrInt %r0, i32 0 , i32 0
%t8 = load i32* %t7
%t1 = call i8* @calloc(i32 %t8, i32 %t3)
%t4 = bitcast i8* %t1 to [ 0 x i32 ]*
%t5 = getelementptr %arrInt %t0, i32 0 , i32 0
store i32 %t8 , i32* %t5
%t6 = getelementptr %arrInt %t0, i32 0 , i32 1
store [ 0 x i32 ]* %t4 , [ 0 x i32 ]** %t6
%r1 = alloca i32
store i32 0 , i32* %r1
%t9 = getelementptr %arrInt %r0, i32 0 , i32 0
%t10 = load i32* %t9
%r2 = alloca i32
%t11 = alloca i32
store i32 0 , i32* %t11
br label %L0
L0:
%t13 = load i32* %t11
%t12 = icmp eq i32 %t10 , %t13
br i1 %t12 , label %L2 , label %L1
L1:
%t14 = getelementptr %arrInt %r0, i32 0 , i32 1
%t15 = load [ 0 x i32 ]** %t14
%t16 = getelementptr [ 0 x i32 ]* %t15, i32 0 , i32 %t13
%t17 = load i32* %t16
store i32 %t17 , i32* %r2
%t19 = getelementptr %arrInt %t0, i32 0 , i32 1
%t20 = load [ 0 x i32 ]** %t19
%t22 = load i32* %r1
%t21 = getelementptr [ 0 x i32 ]* %t20, i32 0 , i32 %t22
%t23 = load i32* %r2
%t24 = mul i32 2 , %t23
store i32 %t24 , i32* %t21
%t25 = load i32* %r1
%t26 = add i32 %t25 , 1
store i32 %t26 , i32* %r1
%t18 = add i32 %t13 , 1
store i32 %t18 , i32* %t11
br label %L0
L2:
%t27 = load %arrInt* %t0
ret %arrInt %t27

}
define void @shiftLeft(%arrInt %a) {

%t28 = getelementptr %arrInt %r0, i32 0 , i32 1
%t29 = load [ 0 x i32 ]** %t28
%t30 = getelementptr [ 0 x i32 ]* %t29, i32 0 , i32 0
%t31 = load i32* %t30
%r1 = alloca i32
store i32 %t31 , i32* %r1
%r2 = alloca i32
store i32 0 , i32* %r2
br label %L3
L3:
%t32 = load i32* %r2
%t33 = getelementptr %arrInt %r0, i32 0 , i32 0
%t34 = load i32* %t33
%t35 = sub i32 %t34 , 1
%t36 = icmp slt i32 %t32 , %t35
br i1 %t36 , label %L4 , label %L5
L4:
%t37 = getelementptr %arrInt 0, i32 0 , i32 1
%t38 = load [ 0 x i32 ]** %t37
%t40 = load i32* %r2
%t39 = getelementptr [ 0 x i32 ]* %t38, i32 0 , i32 %t40
%t41 = getelementptr %arrInt %r0, i32 0 , i32 1
%t42 = load [ 0 x i32 ]** %t41
%t45 = load i32* %r2
%t46 = add i32 %t45 , 1
%t43 = getelementptr [ 0 x i32 ]* %t42, i32 0 , i32 %t46
%t44 = load i32* %t43
store i32 %t44 , i32* %t39
%t47 = load i32* %r2
%t48 = add i32 %t47 , 1
store i32 %t48 , i32* %r2
br label %L3
L5:
%t49 = getelementptr %arrInt 0, i32 0 , i32 1
%t50 = load [ 0 x i32 ]** %t49
%t52 = getelementptr %arrInt %r0, i32 0 , i32 0
%t53 = load i32* %t52
%t54 = sub i32 %t53 , 1
%t51 = getelementptr [ 0 x i32 ]* %t50, i32 0 , i32 %t54
%t55 = load i32* %r1
store i32 %t55 , i32* %t51
ret void
}
define i32 @scalProd(%arrInt %a , %arrInt %b) {

%r2 = alloca i32
store i32 0 , i32* %r2
%r3 = alloca i32
store i32 0 , i32* %r3
br label %L6
L6:
%t56 = load i32* %r3
%t57 = getelementptr %arrInt %r0, i32 0 , i32 0
%t58 = load i32* %t57
%t59 = icmp slt i32 %t56 , %t58
br i1 %t59 , label %L7 , label %L8
L7:
%t60 = load i32* %r2
%t61 = getelementptr %arrInt %r0, i32 0 , i32 1
%t62 = load [ 0 x i32 ]** %t61
%t65 = load i32* %r3
%t63 = getelementptr [ 0 x i32 ]* %t62, i32 0 , i32 %t65
%t64 = load i32* %t63
%t66 = getelementptr %arrInt %r1, i32 0 , i32 1
%t67 = load [ 0 x i32 ]** %t66
%t70 = load i32* %r3
%t68 = getelementptr [ 0 x i32 ]* %t67, i32 0 , i32 %t70
%t69 = load i32* %t68
%t71 = mul i32 %t64 , %t69
%t72 = add i32 %t60 , %t71
store i32 %t72 , i32* %r2
%t73 = load i32* %r3
%t74 = add i32 %t73 , 1
store i32 %t74 , i32* %r3
br label %L6
L8:
%t75 = load i32* %r2
ret i32 %t75

}
define i32 @main() {
%t78 = getelementptr i32* null, i32 1
%t79 = ptrtoint i32* %t78 to i32
%t76 = alloca %arrIntStruct
%t77 = call i8* @calloc(i32 5, i32 %t79)
%t80 = bitcast i8* %t77 to [ 0 x i32 ]*
%t81 = getelementptr %arrInt %t76, i32 0 , i32 0
store i32 5 , i32* %t81
%t82 = getelementptr %arrInt %t76, i32 0 , i32 1
store [ 0 x i32 ]* %t80 , [ 0 x i32 ]** %t82
%r0 = alloca i32
store i32 0 , i32* %r0
br label %L9
L9:
%t83 = load i32* %r0
%t84 = getelementptr %arrInt %t76, i32 0 , i32 0
%t85 = load i32* %t84
%t86 = icmp slt i32 %t83 , %t85
br i1 %t86 , label %L10 , label %L11
L10:
%t87 = getelementptr %arrInt %t76, i32 0 , i32 1
%t88 = load [ 0 x i32 ]** %t87
%t90 = load i32* %r0
%t89 = getelementptr [ 0 x i32 ]* %t88, i32 0 , i32 %t90
%t91 = load i32* %r0
store i32 %t91 , i32* %t89
%t92 = load i32* %r0
%t93 = add i32 %t92 , 1
store i32 %t93 , i32* %r0
br label %L9
L11:
%t94 = load %arrInt* %t76
call void @shiftLeft(i32 %t94)
%t95 = load %arrInt* %t76
%t96 = call i32 @doubleArray(i32 %t95)
%r1 = alloca i32
store i32 %t96 , i32* %r1
%t97 = getelementptr %arrInt %t76, i32 0 , i32 0
%t98 = load i32* %t97
%r2 = alloca i32
%t99 = alloca i32
store i32 0 , i32* %t99
br label %L12
L12:
%t101 = load i32* %t99
%t100 = icmp eq i32 %t98 , %t101
br i1 %t100 , label %L14 , label %L13
L13:
%t102 = getelementptr %arrInt %t76, i32 0 , i32 1
%t103 = load [ 0 x i32 ]** %t102
%t104 = getelementptr [ 0 x i32 ]* %t103, i32 0 , i32 %t101
%t105 = load i32* %t104
store i32 %t105 , i32* %r2
%t107 = load i32* %r2
call void @printInt(i32 %t107)
%t106 = add i32 %t101 , 1
store i32 %t106 , i32* %t99
br label %L12
L14:
%t108 = getelementptr %arrInt %r1, i32 0 , i32 0
%t109 = load i32* %t108
%r3 = alloca i32
%t110 = alloca i32
store i32 0 , i32* %t110
br label %L15
L15:
%t112 = load i32* %t110
%t111 = icmp eq i32 %t109 , %t112
br i1 %t111 , label %L17 , label %L16
L16:
%t113 = getelementptr %arrInt %r1, i32 0 , i32 1
%t114 = load [ 0 x i32 ]** %t113
%t115 = getelementptr [ 0 x i32 ]* %t114, i32 0 , i32 %t112
%t116 = load i32* %t115
store i32 %t116 , i32* %r3
%t118 = load i32* %r3
call void @printInt(i32 %t118)
%t117 = add i32 %t112 , 1
store i32 %t117 , i32* %t110
br label %L15
L17:
%t119 = load %arrInt* %t76
%t120 = load %arrInt* %r1
%t121 = call i32 @scalProd(i32 %t119 , i32 %t120)
call void @printInt(i32 %t121)
ret i32 0

}
