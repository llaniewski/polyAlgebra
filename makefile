main : main.o
	$(CXX) $(CXXFLAGS) -o $@ $^
%.o : %.cpp
	$(CXX) $(CXXFLAGS) -c -o $@ $<
