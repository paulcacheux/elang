declare void @print_char(i8)
declare void @print_int(i32)
define void @color(i32 %arg0, i32 %arg1, i32 %arg2) {
entry:
	%local_0 = alloca i32
	store i32 %arg0, i32* %local_0
	%local_1 = alloca i32
	store i32 %arg1, i32* %local_1
	%local_2 = alloca i32
	store i32 %arg2, i32* %local_2
	%local_global_0 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_0
	%local_global_1 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_1
	%local_global_2 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_2
	br label %bb0

bb0:
	%temp_0 = load void(i8)*, void(i8)** %local_global_0
	%temp_1 = getelementptr i32, i32* %local_0, i64 0
	%temp_2 = load i32, i32* %temp_1
	%temp_3 = trunc i32 %temp_2 to i8
	call void %temp_0(i8 %temp_3)
	%temp_5 = load void(i8)*, void(i8)** %local_global_1
	%temp_6 = getelementptr i32, i32* %local_1, i64 0
	%temp_7 = load i32, i32* %temp_6
	%temp_8 = trunc i32 %temp_7 to i8
	call void %temp_5(i8 %temp_8)
	%temp_10 = load void(i8)*, void(i8)** %local_global_2
	%temp_11 = getelementptr i32, i32* %local_2, i64 0
	%temp_12 = load i32, i32* %temp_11
	%temp_13 = trunc i32 %temp_12 to i8
	call void %temp_10(i8 %temp_13)
	
	ret void

}
define i32 @main() {
entry:
	%local_0 = alloca i32
	%local_1 = alloca i32
	%local_2 = alloca i32
	%local_3 = alloca double
	%local_4 = alloca i32
	%local_5 = alloca i32
	%local_6 = alloca double
	%local_7 = alloca double
	%local_8 = alloca double
	%local_9 = alloca double
	%local_10 = alloca i32
	%local_11 = alloca i32
	%local_12 = alloca double
	%local_13 = alloca double
	%local_global_0 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_0
	%local_global_1 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_1
	%local_global_2 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_2
	%local_global_3 = alloca void(i32)*
	store void(i32)* @print_int, void(i32)** %local_global_3
	%local_global_4 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_4
	%local_global_5 = alloca void(i32)*
	store void(i32)* @print_int, void(i32)** %local_global_5
	%local_global_6 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_6
	%local_global_7 = alloca void(i32)*
	store void(i32)* @print_int, void(i32)** %local_global_7
	%local_global_8 = alloca void(i8)*
	store void(i8)* @print_char, void(i8)** %local_global_8
	%local_global_9 = alloca void(i32, i32, i32)*
	store void(i32, i32, i32)* @color, void(i32, i32, i32)** %local_global_9
	%local_global_10 = alloca void(i32, i32, i32)*
	store void(i32, i32, i32)* @color, void(i32, i32, i32)** %local_global_10
	br label %bb0

bb0:
	%temp_0 = select i1 true, i32 500, i32 0
	%temp_1 = getelementptr i32, i32* %local_0, i64 0
	store i32 %temp_0, i32* %temp_1
	%temp_2 = select i1 true, i32 500, i32 0
	%temp_3 = getelementptr i32, i32* %local_1, i64 0
	store i32 %temp_2, i32* %temp_3
	%temp_4 = select i1 true, i32 100, i32 0
	%temp_5 = getelementptr i32, i32* %local_2, i64 0
	store i32 %temp_4, i32* %temp_5
	%temp_6 = select i1 true, double 1.0000000000, double 0.0
	%temp_7 = getelementptr double, double* %local_3, i64 0
	store double %temp_6, double* %temp_7
	%temp_8 = load void(i8)*, void(i8)** %local_global_0
	%temp_9 = select i1 true, i8 80, i8 0
	call void %temp_8(i8 %temp_9)
	%temp_11 = load void(i8)*, void(i8)** %local_global_1
	%temp_12 = select i1 true, i8 54, i8 0
	call void %temp_11(i8 %temp_12)
	%temp_14 = load void(i8)*, void(i8)** %local_global_2
	%temp_15 = select i1 true, i8 10, i8 0
	call void %temp_14(i8 %temp_15)
	%temp_17 = load void(i32)*, void(i32)** %local_global_3
	%temp_18 = getelementptr i32, i32* %local_0, i64 0
	%temp_19 = load i32, i32* %temp_18
	call void %temp_17(i32 %temp_19)
	%temp_21 = load void(i8)*, void(i8)** %local_global_4
	%temp_22 = select i1 true, i8 32, i8 0
	call void %temp_21(i8 %temp_22)
	%temp_24 = load void(i32)*, void(i32)** %local_global_5
	%temp_25 = getelementptr i32, i32* %local_1, i64 0
	%temp_26 = load i32, i32* %temp_25
	call void %temp_24(i32 %temp_26)
	%temp_28 = load void(i8)*, void(i8)** %local_global_6
	%temp_29 = select i1 true, i8 10, i8 0
	call void %temp_28(i8 %temp_29)
	%temp_31 = load void(i32)*, void(i32)** %local_global_7
	%temp_32 = select i1 true, i32 255, i32 0
	call void %temp_31(i32 %temp_32)
	%temp_34 = load void(i8)*, void(i8)** %local_global_8
	%temp_35 = select i1 true, i8 10, i8 0
	call void %temp_34(i8 %temp_35)
	%temp_37 = select i1 true, i32 1, i32 0
	%temp_38 = getelementptr i32, i32* %local_4, i64 0
	store i32 %temp_37, i32* %temp_38
	br label %bb1
bb1:
	%temp_39 = getelementptr i32, i32* %local_4, i64 0
	%temp_40 = load i32, i32* %temp_39
	%temp_41 = getelementptr i32, i32* %local_1, i64 0
	%temp_42 = load i32, i32* %temp_41
	%temp_43 = icmp sle i32 %temp_40, %temp_42
	br i1 %temp_43, label %bb2, label %bb4
bb2:
	%temp_44 = select i1 true, i32 1, i32 0
	%temp_45 = getelementptr i32, i32* %local_5, i64 0
	store i32 %temp_44, i32* %temp_45
	br label %bb5
bb5:
	%temp_46 = getelementptr i32, i32* %local_5, i64 0
	%temp_47 = load i32, i32* %temp_46
	%temp_48 = getelementptr i32, i32* %local_0, i64 0
	%temp_49 = load i32, i32* %temp_48
	%temp_50 = icmp sle i32 %temp_47, %temp_49
	br i1 %temp_50, label %bb6, label %bb1
bb6:
	%temp_51 = getelementptr i32, i32* %local_5, i64 0
	%temp_52 = load i32, i32* %temp_51
	%temp_53 = sitofp i32 %temp_52 to double
	%temp_54 = getelementptr i32, i32* %local_0, i64 0
	%temp_55 = load i32, i32* %temp_54
	%temp_56 = sitofp i32 %temp_55 to double
	%temp_57 = fdiv double %temp_53, %temp_56
	%temp_58 = select i1 true, double 0.5000000000, double 0.0
	%temp_59 = fsub double %temp_57, %temp_58
	%temp_60 = getelementptr double, double* %local_3, i64 0
	%temp_61 = load double, double* %temp_60
	%temp_62 = fdiv double %temp_59, %temp_61
	%temp_63 = select i1 true, double 3.0000000000, double 0.0
	%temp_64 = fmul double %temp_62, %temp_63
	%temp_65 = select i1 true, double 0.7000000000, double 0.0
	%temp_66 = fsub double %temp_64, %temp_65
	%temp_67 = getelementptr double, double* %local_6, i64 0
	store double %temp_66, double* %temp_67
	%temp_68 = getelementptr i32, i32* %local_4, i64 0
	%temp_69 = load i32, i32* %temp_68
	%temp_70 = sitofp i32 %temp_69 to double
	%temp_71 = getelementptr i32, i32* %local_1, i64 0
	%temp_72 = load i32, i32* %temp_71
	%temp_73 = sitofp i32 %temp_72 to double
	%temp_74 = fdiv double %temp_70, %temp_73
	%temp_75 = select i1 true, double 0.5000000000, double 0.0
	%temp_76 = fsub double %temp_74, %temp_75
	%temp_77 = getelementptr double, double* %local_3, i64 0
	%temp_78 = load double, double* %temp_77
	%temp_79 = fdiv double %temp_76, %temp_78
	%temp_80 = select i1 true, double 3.0000000000, double 0.0
	%temp_81 = fmul double %temp_79, %temp_80
	%temp_82 = getelementptr double, double* %local_7, i64 0
	store double %temp_81, double* %temp_82
	%temp_83 = select i1 true, double 0.0000000000, double 0.0
	%temp_84 = getelementptr double, double* %local_8, i64 0
	store double %temp_83, double* %temp_84
	%temp_85 = select i1 true, double 0.0000000000, double 0.0
	%temp_86 = getelementptr double, double* %local_9, i64 0
	store double %temp_85, double* %temp_86
	%temp_87 = getelementptr i32, i32* %local_2, i64 0
	%temp_88 = load i32, i32* %temp_87
	%temp_89 = select i1 true, i32 1, i32 0
	%temp_90 = add i32 %temp_88, %temp_89
	%temp_91 = getelementptr i32, i32* %local_10, i64 0
	store i32 %temp_90, i32* %temp_91
	%temp_92 = select i1 true, i32 1, i32 0
	%temp_93 = getelementptr i32, i32* %local_11, i64 0
	store i32 %temp_92, i32* %temp_93
	br label %bb9
bb9:
	%temp_94 = getelementptr i32, i32* %local_11, i64 0
	%temp_95 = load i32, i32* %temp_94
	%temp_96 = getelementptr i32, i32* %local_2, i64 0
	%temp_97 = load i32, i32* %temp_96
	%temp_98 = icmp sle i32 %temp_95, %temp_97
	br i1 %temp_98, label %bb10, label %bb5
bb10:
	%temp_99 = getelementptr double, double* %local_8, i64 0
	%temp_100 = load double, double* %temp_99
	%temp_101 = getelementptr double, double* %local_8, i64 0
	%temp_102 = load double, double* %temp_101
	%temp_103 = fmul double %temp_100, %temp_102
	%temp_104 = getelementptr double, double* %local_9, i64 0
	%temp_105 = load double, double* %temp_104
	%temp_106 = getelementptr double, double* %local_9, i64 0
	%temp_107 = load double, double* %temp_106
	%temp_108 = fmul double %temp_105, %temp_107
	%temp_109 = fsub double %temp_103, %temp_108
	%temp_110 = getelementptr double, double* %local_6, i64 0
	%temp_111 = load double, double* %temp_110
	%temp_112 = fadd double %temp_109, %temp_111
	%temp_113 = getelementptr double, double* %local_12, i64 0
	store double %temp_112, double* %temp_113
	%temp_114 = select i1 true, double 2.0000000000, double 0.0
	%temp_115 = getelementptr double, double* %local_8, i64 0
	%temp_116 = load double, double* %temp_115
	%temp_117 = fmul double %temp_114, %temp_116
	%temp_118 = getelementptr double, double* %local_9, i64 0
	%temp_119 = load double, double* %temp_118
	%temp_120 = fmul double %temp_117, %temp_119
	%temp_121 = getelementptr double, double* %local_7, i64 0
	%temp_122 = load double, double* %temp_121
	%temp_123 = fadd double %temp_120, %temp_122
	%temp_124 = getelementptr double, double* %local_13, i64 0
	store double %temp_123, double* %temp_124
	%temp_125 = getelementptr double, double* %local_8, i64 0
	%temp_126 = getelementptr double, double* %local_12, i64 0
	%temp_127 = load double, double* %temp_126
	store double %temp_127, double* %temp_125
	%temp_128 = getelementptr double, double* %local_9, i64 0
	%temp_129 = getelementptr double, double* %local_13, i64 0
	%temp_130 = load double, double* %temp_129
	store double %temp_130, double* %temp_128
	br label %bb13
bb13:
	%temp_131 = select i1 true, double 100.0000000000, double 0.0
	%temp_132 = getelementptr double, double* %local_8, i64 0
	%temp_133 = load double, double* %temp_132
	%temp_134 = getelementptr double, double* %local_8, i64 0
	%temp_135 = load double, double* %temp_134
	%temp_136 = fmul double %temp_133, %temp_135
	%temp_137 = getelementptr double, double* %local_9, i64 0
	%temp_138 = load double, double* %temp_137
	%temp_139 = getelementptr double, double* %local_9, i64 0
	%temp_140 = load double, double* %temp_139
	%temp_141 = fmul double %temp_138, %temp_140
	%temp_142 = fadd double %temp_136, %temp_141
	%temp_143 = fcmp olt double %temp_131, %temp_142
	br i1 %temp_143, label %bb14, label %bb9
bb14:
	%temp_144 = getelementptr i32, i32* %local_10, i64 0
	%temp_145 = getelementptr i32, i32* %local_11, i64 0
	%temp_146 = load i32, i32* %temp_145
	store i32 %temp_146, i32* %temp_144
	br label %bb5
bb4:
	%temp_147 = getelementptr i32, i32* %local_11, i64 0
	%temp_148 = getelementptr i32, i32* %local_11, i64 0
	%temp_149 = load i32, i32* %temp_148
	%temp_150 = select i1 true, i32 1, i32 0
	%temp_151 = add i32 %temp_149, %temp_150
	store i32 %temp_151, i32* %temp_147
	br label %bb20
bb20:
	%temp_152 = getelementptr i32, i32* %local_10, i64 0
	%temp_153 = load i32, i32* %temp_152
	%temp_154 = getelementptr i32, i32* %local_2, i64 0
	%temp_155 = load i32, i32* %temp_154
	%temp_156 = icmp slt i32 %temp_153, %temp_155
	br i1 %temp_156, label %bb21, label %bb23
bb21:
	%temp_157 = load void(i32, i32, i32)*, void(i32, i32, i32)** %local_global_9
	%temp_158 = select i1 true, i32 200, i32 0
	%temp_159 = select i1 true, i32 55, i32 0
	%temp_160 = getelementptr i32, i32* %local_10, i64 0
	%temp_161 = load i32, i32* %temp_160
	%temp_162 = mul i32 %temp_159, %temp_161
	%temp_163 = select i1 true, i32 100, i32 0
	%temp_164 = sdiv i32 %temp_162, %temp_163
	%temp_165 = add i32 %temp_158, %temp_164
	%temp_166 = select i1 true, i32 230, i32 0
	%temp_167 = select i1 true, i32 100, i32 0
	%temp_168 = getelementptr i32, i32* %local_10, i64 0
	%temp_169 = load i32, i32* %temp_168
	%temp_170 = sub i32 %temp_167, %temp_169
	%temp_171 = mul i32 %temp_166, %temp_170
	%temp_172 = select i1 true, i32 100, i32 0
	%temp_173 = sdiv i32 %temp_171, %temp_172
	%temp_174 = select i1 true, i32 230, i32 0
	%temp_175 = select i1 true, i32 100, i32 0
	%temp_176 = getelementptr i32, i32* %local_10, i64 0
	%temp_177 = load i32, i32* %temp_176
	%temp_178 = sub i32 %temp_175, %temp_177
	%temp_179 = mul i32 %temp_174, %temp_178
	%temp_180 = select i1 true, i32 100, i32 0
	%temp_181 = sdiv i32 %temp_179, %temp_180
	call void %temp_157(i32 %temp_165, i32 %temp_173, i32 %temp_181)
	br label %bb22
bb23:
	%temp_183 = load void(i32, i32, i32)*, void(i32, i32, i32)** %local_global_10
	%temp_184 = select i1 true, i32 0, i32 0
	%temp_185 = select i1 true, i32 255, i32 0
	%temp_186 = select i1 true, i32 255, i32 0
	call void %temp_183(i32 %temp_184, i32 %temp_185, i32 %temp_186)
	br label %bb22
bb22:
	%temp_188 = getelementptr i32, i32* %local_5, i64 0
	%temp_189 = getelementptr i32, i32* %local_5, i64 0
	%temp_190 = load i32, i32* %temp_189
	%temp_191 = select i1 true, i32 1, i32 0
	%temp_192 = add i32 %temp_190, %temp_191
	store i32 %temp_192, i32* %temp_188
	%temp_193 = getelementptr i32, i32* %local_4, i64 0
	%temp_194 = getelementptr i32, i32* %local_4, i64 0
	%temp_195 = load i32, i32* %temp_194
	%temp_196 = select i1 true, i32 1, i32 0
	%temp_197 = add i32 %temp_195, %temp_196
	store i32 %temp_197, i32* %temp_193
	%temp_198 = select i1 true, i32 0, i32 0
	ret i32 %temp_198;

}
