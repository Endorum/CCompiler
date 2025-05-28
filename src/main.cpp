
#include "../include/utils.hpp"
#include "../include/lexer.hpp"

int main(){
    
    std::string content = readFileToString("test.c");
    

    Lexer lexer(content);

    lexer.lex();

    lexer.printTokens();



    return 0;
}