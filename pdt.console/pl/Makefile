OBJ= stream_decorator.o
SRC= $(OBJ:.o=.c)

all: stream_decorator.so
clean: 
	rm $(OBJ)
	rm stream_decorator.so
stream_decorator.so: $(OBJ)

# ulla's mini mac:
	gcc  -Wall -g -undefined dynamic_lookup -dynamiclib -o stream_decorator.so $(OBJ)
# lu's linux box:
#gcc  -Wall -g -shared -o stream_decorator.so $(OBJ)

stream_decorator.o: stream_decorator.c
	gcc -Wall -g -I/usr/local/lib/swipl/include -fpic -c stream_decorator.c
