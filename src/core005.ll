declare void @printInt(i32)
declare void @printString(i8*)
declare void @printDouble(double)
declare i32 @readInt()
declare double @readDouble()
declare i8* @calloc(i32, i32)

%array = type %arrayStruct*
%arrayStruct = type { i32, [0 x i32]* }; must be global!


define i32 @main() {

; start init array
;allocs main struct and array
; push the structs to the global stuff

%main = alloca %arrayStruct
%t1111 = add i32 0, 4
%arrayCall2 = call i8* @calloc(i32 4, i32 4)
%arrayCall = bitcast i8* %arrayCall2 to [0 x i32]* ; Gillar inte casten


;Stores array size into main struct
%arrPointer1 = getelementptr %array %main , i32 0, i32 0
store i32 4, i32* %arrPointer1

;Stores array pointer into main struct
%arrPointer2 = getelementptr %array %main , i32 0, i32 1
store [0 x i32]* %arrayCall, [0 x i32]** %arrPointer2

; end init array

%arrP1 = getelementptr %array %main , i32 0, i32 1
%t21 = load [0 x i32]** %arrP1 
%arrP = getelementptr [0 x i32]* %t21 , i32 0,  i32 1 
store i32 15, i32* %arrP

%t1 = add i32 0, 0 

%arrP3 = getelementptr %array %main , i32 0, i32 1 ;, i32 %t1 ;Gets pointer to array
%t211 = load [0 x i32]** %arrP3 
%arrP2 = getelementptr [0 x i32]* %t211 , i32 0, i32 1
%res = load i32* %arrP2




; Gets fst elemnt in array and prints it
call void @printInt(i32 %res)


ret i32 0
}
