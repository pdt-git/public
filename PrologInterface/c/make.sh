gcc -I/usr/local/lib/swipl/include -fpic -c ttymode.c
gcc -shared -o ttymode.so ttymode.o