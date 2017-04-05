outpath="$1.ll"
objpath="$1.o"
expath="utils/$(basename $1).out"
./target/debug/elang $1 -O -t=llvm -o $outpath
/usr/local/opt/llvm@4/bin/llc -O3 -filetype=obj $outpath -o $objpath
clang -c ./utils/io.c -o ./utils/io.o
clang $objpath ./utils/io.o -o $expath
./$expath
rm $outpath
rm $objpath
