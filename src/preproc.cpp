#include "../include/preproc.hpp"


#include <fstream>
#include <sstream>
#include <iostream>
#include <regex>
#include <string>

Preprocessor::Preprocessor(const std::string& inputFile, std::unordered_map<std::string, std::string>& macros)
    : macros(macros), currentFile(inputFile) {}



bool starts_with(const std::string& str, const std::string& prefix) {
    return str.size() >= prefix.size() && str.compare(0, prefix.size(), prefix) == 0;
}

std::string Preprocessor::preprocess() {
    std::string source = readFile(currentFile);

    // First pass: handle includes
    std::istringstream includeStream(source);
    std::ostringstream includeOutput;

    std::string line;
    while (std::getline(includeStream, line)) {
        // remove possible leading whitespace
        std::string trimmed = line;
        trimmed.erase(0, trimmed.find_first_not_of(" \t"));

        if (starts_with(trimmed, "#include")) {
            processInclude(trimmed, includeOutput);
            continue; // skip output, included file will be added in place
        } else {
            includeOutput << line << "\n"; // keep other lines as-is for now
        }
    }

    // Second pass: handle defines and expand macros
    std::istringstream macroStream(includeOutput.str());
    std::ostringstream macroOutput;

    while (std::getline(macroStream, line)) {
        std::string trimmed = line;
        trimmed.erase(0, trimmed.find_first_not_of(" \t"));

        if (starts_with(trimmed, "#define")) {
            processDefine(trimmed);
            continue; // skip output, macros are stored
        }

        // Write non-define lines to macroOutput
        macroOutput << line << "\n";
    }

    std::istringstream expandStream(macroOutput.str());
    std::ostringstream expandOutput;

    while (std::getline(expandStream, line)){
        std::string expanded = expandMacros(line);
        expandOutput << expanded << "\n";
    }

    return expandOutput.str();
}

std::string Preprocessor::readFile(const std::string& fileName) {
    std::ifstream file(fileName);
    if (!file) {
        std::cerr << "Preprocessor Error: could not open file " << fileName << "\n";
        std::exit(1);
    }

    std::ostringstream ss;
    ss << file.rdbuf();
    return ss.str();
}

void Preprocessor::processInclude(const std::string& line, std::ostringstream& output) {
    std::smatch match;
    std::regex includeRegex(R"(#include\s+[\"<](.*)[\">])");
    if (std::regex_search(line, match, includeRegex)) {
        std::string includeFile = match[1];

        Preprocessor subPreprocessor(includeFile, macros);
        std::string includedProcessed = subPreprocessor.preprocess();

        output << includedProcessed; // Insert fully preprocessed content into the buffer
    } else {
        std::cerr << "Preprocessor Error: malformed #include: " << line << "\n";
    }
}

void Preprocessor::processDefine(const std::string& line) {
    std::smatch match;
    std::regex defineRegex(R"(#define\s+(\w+)\s+(.*))");
    if (std::regex_search(line, match, defineRegex)) {
        std::string name = match[1];
        std::string value = match[2];
        macros[name] = value;
    } else {
        std::cerr << "Preprocessor Error: malformed #define: " << line << "\n";
    }
}

std::string Preprocessor::expandMacros(const std::string& line) {
    std::string result = line;

    for (const auto& [name, value] : macros) {
        std::regex wordRegex("\\b" + name + "\\b");
        result = std::regex_replace(result, wordRegex, value);
    }

    return result;
}