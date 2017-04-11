outpath="$1.ll"
optpath="$1.opt.ll"
objpath="$1.o"
expath="utils/$(basename $1).out"
./target/debug/elang $1 -O -t=llvm -o $outpath
opt -O3 -S $outpath -o $optpath
llc -filetype=obj $optpath -o $objpath
clang $objpath -o $expath
#./$expath
rm $optpath
rm $outpath
rm $objpath
