EXE = clox
CC = gcc -std=c99
CFLAGS = -Wall
OBJ = obj
SRC = src
SOURCES := $(wildcard $(SRC)/*.c)
OBJECTS := $(patsubst $(SRC)/%.c, $(OBJ)/%.o, $(wildcard $(SRC)/*.c)) 

all: $(EXE)

debug: CFLAGS += -g
debug: $(EXE)

$(EXE): $(SRC) $(OBJ) $(OBJECTS)
	$(CC) $(OBJECTS) -o $@

$(OBJ):
	mkdir -p $(OBJ)

$(OBJ)/%.o:	$(SRC)/%.c
	$(CC) $(DEPFLAGS) $(CFLAGS) $(CPPFLAGS) -c -o $@ $<

clean:
	$(RM) $(OBJECTS)
	$(RM) $(EXE)
