; ModuleID = 'elang_examples/mandelbrot_ppm.li.ll'
source_filename = "elang_examples/mandelbrot_ppm.li.ll"

declare void @print_char(i8) local_unnamed_addr

declare void @print_int(i32) local_unnamed_addr

define void @color(i32 %arg0, i32 %arg1, i32 %arg2) local_unnamed_addr {
entry:
  %temp_3 = trunc i32 %arg0 to i8
  tail call void @print_char(i8 %temp_3)
  %temp_8 = trunc i32 %arg1 to i8
  tail call void @print_char(i8 %temp_8)
  %temp_13 = trunc i32 %arg2 to i8
  tail call void @print_char(i8 %temp_13)
  ret void
}

; Function Attrs: noreturn
define i32 @main() local_unnamed_addr #0 {
entry:
  tail call void @print_char(i8 80)
  tail call void @print_char(i8 54)
  tail call void @print_char(i8 10)
  tail call void @print_int(i32 500)
  tail call void @print_char(i8 32)
  tail call void @print_int(i32 500)
  tail call void @print_char(i8 10)
  tail call void @print_int(i32 255)
  tail call void @print_char(i8 10)
  br label %bb5

bb5.loopexit:                                     ; preds = %bb9
  br label %bb5

bb5:                                              ; preds = %bb5.loopexit, %entry
  br label %bb9

bb9:                                              ; preds = %bb9, %bb5
  %local_9.0 = phi double [ 0.000000e+00, %bb5 ], [ %temp_123, %bb9 ]
  %local_8.0 = phi double [ 0.000000e+00, %bb5 ], [ %temp_112, %bb9 ]
  %temp_103 = fmul double %local_8.0, %local_8.0
  %temp_108 = fmul double %local_9.0, %local_9.0
  %temp_109 = fsub double %temp_103, %temp_108
  %temp_112 = fadd double %temp_109, -2.194000e+00
  %temp_117 = fmul double %local_8.0, 2.000000e+00
  %temp_120 = fmul double %local_9.0, %temp_117
  %temp_123 = fadd double %temp_120, -1.494000e+00
  %temp_136 = fmul double %temp_112, %temp_112
  %temp_141 = fmul double %temp_123, %temp_123
  %temp_142 = fadd double %temp_136, %temp_141
  %temp_143 = fcmp ogt double %temp_142, 1.000000e+02
  br i1 %temp_143, label %bb5.loopexit, label %bb9
}

attributes #0 = { noreturn }
