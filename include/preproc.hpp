#pragma once


/*

the real C preprocessor does

-> File inclusion
#include "libarary.h"

-> Macro expansion
#define X 5
#define SQUARE(x) ((x)*(x))

-> conditional compilation
#ifdef, #ifndef, #if, #elif, #else, #endif
decides if something is included in the code or not

-> comments removal (done in the tokenizer)




*/
#include <string>
#include <unordered_map>
#include <vector>

class Preprocessor{
public:
    Preprocessor(const std::string& inputFile, std::unordered_map<std::string, std::string>& macros);

    std::string preprocess();

private:
    std::string readFile(const std::string& fileName);

    void processInclude(const std::string& line, std::ostringstream& output);
    void processDefine(const std::string& line);

    void processIfdef(const std::string& line){}
    void processIfndef(const std::string& line){}
    void processIf(const std::string& line){}
    void processElse(){}
    void processEndif(){}
    void processLine(const std::string& line){}

    std::string expandMacros(const std::string& line);


    std::unordered_map<std::string, std::string>& macros;

    std::vector<bool> ifStack; // Conditional compilation
    


    std::string currentFile;
};