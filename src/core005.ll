declare void @printInt(i32)
declare void @printString(i8*)
declare void @printDouble(double)
declare i32 @readInt()
declare double @readDouble()
declare i32* @calloc(i32, i32)


%array = type %arrayStruct*
%arrayStruct = type { i32, [0 x i32] }; must be global!


define i32 @main() {

;allocs main struct and array
%main = alloca %arrayStruct
%arrayCall = call i32* @calloc(i32 4, i32 4)

;Stores array size into main struct
%arrPointer1 = getelementptr %array %main , i32 0, i32 0
store i32 4, i32* %arrPointer1

;Stores array pointer into main struct
%arrPointer2 = getelementptr %array %main , i32 0, i32 1
%arrayCast = bitcast i32* %arrayCall to [0 x i32]*
store [0 x i32]* %arrayCast, [0 x i32]** %arrPointer2

; Gets element in array and store 5
;May need to cast here
%arrP = getelementptr [2 x i32]* %mainCast , i32 0, i32 1 ;Gets pointer to array
%arrPCast = bitcast i32* %arrP to [4 x i32]*
%arrP1 = getelementptr [4 x i32]* %arrPCast , i32 0, i32 0 ;Gets fst elemt in array
store i32 15, i32* %arrP1

; Gets fst elemnt in array and prints it
%arrP2 = getelementptr [2 x i32]* %mainCast , i32 0, i32 1 ;Gets pointer to array
%arrP2Cast = bitcast i32* %arrP2 to [4 x i32]*
%arrP3 = getelementptr [4 x i32]* %arrP2Cast , i32 0, i32 0 ;Gets fst elemt in array
%res = load i32* %arrP3 
call void @printInt(i32 %res)









;%arrPointerElm = getelementptr [4 x i32]* %arr , i32 0, i32 1
;store i32 5, i32* %arrPointerElm
;%res = load i32* %arrPointerElm
;call void @printInt(i32 %res)



%r0 = alloca i32
store i32 0 , i32* %r0
%r1 = alloca i32
store i32 56 , i32* %r1
%t0 = load i32* %r1
%t1 = add i32 %t0 , 45
%t2 = icmp sle i32 %t1 , 2
br i1 %t2 , label %L0 , label %L1
L0:
store i32 1 , i32* %r0
br label %L2
L1:
store i32 2 , i32* %r0
br label %L2
L2:
%t3 = load i32* %r0
call void @printInt(i32 %t3)
ret i32 0

}
