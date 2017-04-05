outpath="$1.ll"
objpath="$1.o"
expath="$1.out"
./target/debug/elang $1 -O -t=llvm -o $outpath
/usr/local/opt/llvm@4/bin/llc -O3 -filetype=obj $outpath -o $objpath
clang -c ./elang_examples/io.c -o ./elang_examples/io.o
clang $objpath ./elang_examples/io.o -o $expath
./$expath
rm $outpath
rm $objpath
