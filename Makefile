CXX = clang++
CXXFLAGS = -std=c++20 -Wall -g -MMD -MP
SRC = $(wildcard src/*.cpp)
OBJ = $(SRC:.cpp=.o)
DEP = $(OBJ:.o=.d)
OUT = main

$(OUT): $(OBJ)
	$(CXX) $(CXXFLAGS) -o $@ $^
	@rm -f $(OBJ) $(DEP)

-include $(DEP)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $< -o $@

.PHONY: clean
clean:
	rm -f $(OUT) $(OBJ) $(DEP)