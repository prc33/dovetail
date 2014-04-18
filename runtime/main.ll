%worker = type opaque
%instance = type opaque
%sigval_int = type {void (%worker*, %instance*, {i32})*, %instance*}

declare void @dovetail_end(i32)
declare void @construct.slow.main(%worker*, { i32, %sigval_int }) 

define void @construct_main(%worker* %w, i32 %x) nounwind {
  %b1       = insertvalue %sigval_int undef, void (%worker*, %instance*, {i32})* @print_end, 0
  %b2       = insertvalue %sigval_int %b1,   %instance* null, 1

  %tmp      = insertvalue {i32, %sigval_int } undef, i32 %x, 0
  %msg      = insertvalue {i32, %sigval_int } %tmp,  %sigval_int %b2, 1  
  
  tail call void @construct.slow.main(%worker* %w, { i32, %sigval_int } %msg)
  ret void
}

define fastcc void @print_end(%worker* %w, %instance* %dummy, {i32} %result) {
  %val = extractvalue {i32} %result, 0
  call void @dovetail_end(i32 %val)
  ret void
}

define i32 @print.int(i32 %val) {
  tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([13 x i8]* @.str, i32 0, i32 0), i32 %val) nounwind
  ret i32 0
}

define i32 @print.double(double %val) {
  tail call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([5 x i8]* @.str2, i32 0, i32 0), double %val) nounwind
  ret i32 0
}

declare i32 @printf(i8* nocapture, ...) nounwind
@.str = private unnamed_addr constant [13 x i8] c"Result = %d\0A\00", align 1
@.str2 = private unnamed_addr constant [5 x i8] c"%lf\0A\00", align 1
