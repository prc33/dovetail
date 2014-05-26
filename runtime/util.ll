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
