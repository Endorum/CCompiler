#include <ostream>
#include <string>

#include "../include/utils.hpp"

#include "../include/preproc.hpp"
#include "../include/lexer.hpp"

// #include "../old/parserOLD.hpp"
#include "../include/parser.hpp"

#include "../include/ir_codegen.hpp"



int main(){



    std::string baseFileName = "test.c";
    
    // std::string content = readFileToString("test.c");

    std::unordered_map<std::string, std::string> globalMacros;
    Preprocessor preproc(baseFileName, globalMacros);
    std::string content = preproc.preprocess();

    std::cout << content << std::endl;

    Lexer lexer(content);
    lexer.lex();
    lexer.printTokens();

    
    // Parser parser(lexer.getTokens());
    // parser.parse();

    // Parser parser(lexer.getTokens());

    // ASTNode* root = parser.parseProgram();

    // std::cout << root->str() << std::endl;
    


    // IR_Codegen irGen(root, parser.symbols);
    // irGen.generate();

    // irGen.symbols.showSymbols();
    // irGen.symbols.showFunctions();

    // const std::vector<IRInstruction>& instructions = irGen.get_instructions();
    // std::cout << "\n\nGenerated IR Instructions:\n";
    // for (const auto& instr : instructions) {
    //     std::cout << instr.str() << "\n";
    // }



    return 0;
}