%worker = type opaque
%instance = type opaque
%sigval_int = type {void (%worker*, %instance*, {i32})*, %instance*}

declare void @dovetail_end()
declare void @construct.slow.fib(%worker*, { i32, %sigval_int }) 

define void @construct_main(%worker* %w, i32 %x) nounwind {
  %b1       = insertvalue %sigval_int undef, void (%worker*, %instance*, {i32})* @print_end, 0
  %b2       = insertvalue %sigval_int %b1,   %instance* null, 1

  %tmp      = insertvalue {i32, %sigval_int } undef, i32 %x, 0
  %msg      = insertvalue {i32, %sigval_int } %tmp,  %sigval_int %b2, 1  
  
  tail call void @construct.slow.fib(%worker* %w, { i32, %sigval_int } %msg)
  ret void
}

define void @print_end(%worker* %w, %instance* %dummy, {i32} %result) {
  %val = extractvalue {i32} %result, 0
  tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str, i32 0, i32 0), i32 %val) nounwind
  call void @dovetail_end()
  ret void
}

declare i32 @printf(i8* nocapture, ...) nounwind
@.str = private unnamed_addr constant [13 x i8] c"Result = %d\0A\00", align 1
