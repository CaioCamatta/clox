# Base project 
CC = gcc -std=c99
CFLAGS = -Wall -g
OBJ = ../../obj
SRC = ../../src
SOURCES := $(filter-out $(SRC)/main.c, $(wildcard $(SRC)/*.c))
OBJECTS := $(patsubst $(SRC)/%.c, $(OBJ)/%.o, $(filter-out $(SRC)/main.c, $(wildcard $(SRC)/*.c))) 

$(OBJ):
	mkdir -p $(OBJ)

$(OBJ)/%.o:	$(SRC)/%.c
	$(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

# Benchmarks
table_benchmark: $(SRC) $(OBJ) $(OBJECTS) table_benchmark.o
	$(CC) $(OBJECTS) table_benchmark.o -o $@