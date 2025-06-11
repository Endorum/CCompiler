#pragma once

#include <cstddef>
#include <iostream>
#include <ostream>
#include <string>
#include <vector>

#include "defs.hpp"
#include "utils.hpp"

typedef enum{
    // see KeyWordType in defs.hpp
    KEYWORD,

    // Identifiers and literals
    IDENTIFIER,
    INTEGER_LITERAL,
    FLOAT_LITERAL,
    CHAR_LITERAL,
    STRING_LITERAL,

    // operators
    PLUS, MINUS, STAR, SLASH, PERCENT,
    INCREMENT, DECREMENT,
    ASSIGN, PLUS_ASSIGN, MINUS_ASSIGN, STAR_ASSIGN, SLASH_ASSIGN,
    EQ, NEQ, GT, LT, GTE, LTE,
    AND, OR, NOT,
    BIT_AND, BIT_OR, BIT_XOR, BIT_NOT, SHIFT_LEFT, SHIFT_RIGHT,
    QUESTION, COLON,

    // Delimiters
    LPAREN, RPAREN, LBRACE, RBRACE, LBRACKET, RBRACKET,
    SEMICOLON, COMMA, DOT, ARROW,

    // Preprocessor
    PP_DIRECTIVE,
    INCLUDE_PATH,

    END_OF_FILE,

    UNKNOWN
}TokenType;

inline std::string stringFromTokenType(TokenType type){
    if(type == KEYWORD) return "keyword";
    if(type == IDENTIFIER) return "identifier";
    if(type == INTEGER_LITERAL) return "integer_literal";
    if(type == FLOAT_LITERAL) return "float_literal";
    if(type == CHAR_LITERAL) return "char_literal";
    if(type == STRING_LITERAL) return "string_literal";
    if(type == PLUS) return "plus";
    if(type == MINUS) return "minus";
    if(type == STAR) return "star";
    if(type == SLASH) return "slash";
    if(type == PERCENT) return "percent";
    if(type == INCREMENT) return "increment";
    if(type == DECREMENT) return "decrement";
    if(type == ASSIGN) return "assign";
    if(type == PLUS_ASSIGN) return "plus_assign";
    if(type == MINUS_ASSIGN) return "minus_assign";
    if(type == STAR_ASSIGN) return "star_assign";
    if(type == SLASH_ASSIGN) return "slash_assign";
    if(type == EQ) return "eq";
    if(type == NEQ) return "neq";
    if(type == GT) return "gt";
    if(type == LT) return "lt";
    if(type == GTE) return "gte";
    if(type == LTE) return "lte";
    if(type == AND) return "and";
    if(type == OR) return "or";
    if(type == NOT) return "not";
    if(type == BIT_AND) return "bit_and";
    if(type == BIT_OR) return "bit_or";
    if(type == BIT_XOR) return "bit_xor";
    if(type == BIT_NOT) return "bit_not";
    if(type == SHIFT_LEFT) return "shift_left";
    if(type == SHIFT_RIGHT) return "shift_right";
    if(type == QUESTION) return "question";
    if(type == COLON) return "colon";
    if(type == LPAREN) return "lparen";
    if(type == RPAREN) return "rparen";
    if(type == LBRACE) return "lbrace";
    if(type == RBRACE) return "rbrace";
    if(type == LBRACKET) return "lbracket";
    if(type == RBRACKET) return "rbracket";
    if(type == SEMICOLON) return "semicolon";
    if(type == COMMA) return "comma";
    if(type == DOT) return "dot";
    if(type == ARROW) return "arrow";
    if(type == PP_DIRECTIVE) return "pp_directive";
    if(type == INCLUDE_PATH) return "include_path";
    if(type == END_OF_FILE) return "end_of_file";
    return "unknown";
}

struct Token{
    Token(TokenType type, const std::string& value) : type(type), value(value) { kw_type = NOT_A_KEYWORD; }
    Token(KeyWordType kw_type) : kw_type(kw_type) { type = KEYWORD; value=stringFromKeyWordType(kw_type); }
    TokenType type;
    KeyWordType kw_type;
    std::string value;

    size_t line=1;
    size_t column=1;

    std::string str(){
        if(type == KEYWORD){
            return "Keyword: " + stringFromKeyWordType(kw_type);
        }else{
            return stringFromTokenType(type) + " : \'" + value + "\'";
        }
    }
};

class Lexer{
public:
    Lexer(std::string& input) : input(input) {}
    size_t position = 0;

    size_t curr_line = 1;
    size_t curr_column = 1;

    void lex();
    void printTokens(){
        int i=0;
        for(auto token : tokens){
            std::cout << std::to_string(i) << ": " << token.str() << std::endl;
            i++;
        }
    }
    std::vector<Token> getTokens() { return tokens; }
    
private:
    std::string input;
    std::vector<Token> tokens;


    char peek(int offset = 0){
        if(position + offset < input.size() - 1) return input.at(position + offset);

        return '\0';
    }

    void error(std::string msg){
        std::cout << "From lexer: " << std::endl;
        std::cerr << msg << std::endl;
        std::cout << "near:" << std::endl;

        int count = 0;
        for (int i = static_cast<int>(tokens.size()) - 3; i < static_cast<int>(tokens.size()); ++i) {
            if (i >= 0) {
                std::cout << tokens[i].str() << std::endl;
                count++;
            }
        }

        std::cout << "Approx. position: Line: " << std::to_string(curr_line) << " Column: " << std::to_string(curr_column) << std::endl;


        if (count == 0) {
            std::cout << "(no tokens emitted yet)" << std::endl;
        }

        

        exit(1);
    }

    

    void skipSingleLineComment();
    void skipMultiLineComment();
    char next();

    Token lexIdentifierOrKeyword();
    Token lexNumber();
    Token lexStringLiteral();
    Token lexCharLiteral();
    Token lexPreprocessorDirective();
    Token lexOperatorOrPunctuation();

};  