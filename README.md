llvm version: 3.5.0

http://releases.llvm.org/3.5.0/docs/tutorial/index.html

clang++ -std=c++11 -g Kaleidoscope.cpp \`llvm-config --cppflags --ldflags --libs core jit native --system-libs\` -O3 -o Kaleidoscope
