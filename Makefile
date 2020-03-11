options = -std=c++17 -Wall --pedantic-error

kilopp: main.cpp
	$(CXX) $(options) $< -o $@

run: kilopp
	./kilopp

fmt: 
	clang-format -i main.cpp
