

clean up the different symbol tables there should be 
- a table thats temporary for local varaibles it gets cleared everytime a function is entered
    unordered_map<std::string, Symbol> local_table;

- a table for functions which is not temporary
    unordered_map<std::string, Function> function_table;

- a table for global varaibles, typedefs enums and structs
    unordered_map<std::string, Symbol> global_table;