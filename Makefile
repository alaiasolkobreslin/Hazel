OCAMLBUILD=ocamlbuild -use-ocamlfind

MAIN_OUT=main.byte
TEST_OUT=test.byte

# source directories
SRC=src
TESTS=tests

# build main executable
default: $(MAIN_OUT)

$(MAIN_OUT): $(SRC)
	$(OCAMLBUILD) -cflag -g -lflag -g $(MAIN_OUT) -I $(SRC)

$(TEST_OUT): $(SRC) $(TESTS)
	$(OCAMLBUILD) -cflag -g -lflag -g $(TEST_OUT) -I $(SRC) -I $(TESTS)

clean:
	ocamlbuild -clean
	rm -f *.s

# run ounit tests
test: $(TEST_OUT)
	./$(TEST_OUT)