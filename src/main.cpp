
#include "../include/utils.hpp"
#include "../include/lexer.hpp"
#include "../include/parser.hpp"

int main(){
    
    std::string content = readFileToString("test.c");
    

    Lexer lexer(content);
    lexer.lex();
    lexer.printTokens();


    Parser parser(lexer.getTokens());
    ASTNode* root = parser.parseProgram();

    std::cout << root->str() << std::endl;




    return 0;
}