-fno-slp-vectorize doesn't prevent vectorization. Use '-mprefer-vector-width=1'
  $ clang++-18  -emit-llvm -Xclang -disable-O0-optnone -c fac.cpp -O3 -mprefer-vector-width=1 -fno-unroll-loops -Oz -fno-optimize-sibling-calls  -S -o main.ll
$ clang++-18  -emit-llvm -c 1.cpp -O1 -o fO1.bc
$ clang++-18  -emit-llvm -c 1.cpp -O2 -o fO2.bc
$ clang++-18  -emit-llvm -c 1.cpp -O2 -o fO3.bc
$ export SHORT=""

  $ llvm-extract-18 -func=factorial -S main.ll -o fac.ll
  $ cat fac.ll | grep -v -e datalay -e attributes -e '!{i32' -e triple -e source_filename -e '!llvm.' -e  '^;' -e '^$' -e 'clang version'
  define dso_local i32 @factorial(i32 noundef %0) #0 {
    %2 = icmp eq i32 %0, 0
    br i1 %2, label %7, label %3
  3:                                                ; preds = %1
    %4 = add nsw i32 %0, -1
    %5 = call i32 @factorial(i32 noundef %4) #1
    %6 = mul nsw i32 %5, %0
    br label %7
  7:                                                ; preds = %3, %1
    %8 = phi i32 [ %6, %3 ], [ 1, %1 ]
    ret i32 %8
  }
