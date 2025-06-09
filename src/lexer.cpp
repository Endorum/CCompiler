#include <cctype>

#include "../include/lexer.hpp"
#include "../include/utils.hpp"

bool isOperatorStart(char c) {
    return std::string("+-*/%=!<>&|^~?:.,;(){}[]").find(c) != std::string::npos;
}

void Lexer::lex(){
    while(position < input.size()){
        char c = input.at(position);

        curr_column++;

        if(c == '\n'){
            curr_line++;
            curr_column = 1;
            position++;
            continue;
        }

        if(tokens.size() > 1) tokens.at(tokens.size() - 1).line = curr_line;
        if(tokens.size() > 1) tokens.at(tokens.size() - 1).column = curr_line;

        if(std::isspace(c)){
            position++;
            continue;
        }else if(c == '/' && peek(1) == '/'){
            skipSingleLineComment();
            continue;
        }else if(c == '/' && peek(1) == '*'){
            skipMultiLineComment();
            continue;
        }else if(std::isalpha(c) || c == '_'){
            tokens.push_back(lexIdentifierOrKeyword());
            continue;
        }else if(std::isdigit(c)){
            tokens.push_back(lexNumber());
            continue;
        }else if(c == '\"'){
            tokens.push_back(lexStringLiteral());
            continue;
        }else if(c == '\''){
            tokens.push_back(lexCharLiteral());
            continue;
        }else if(c == '#'){
            tokens.push_back(lexPreprocessorDirective());
            continue;
        }else if(isOperatorStart(c)){
            tokens.push_back(lexOperatorOrPunctuation());
            continue;
        }

        error("Unrecognized character: " + std::string(1, c));

        position++;
    }
    tokens.push_back(Token(TokenType::END_OF_FILE, "EOF"));
}



void Lexer::skipSingleLineComment() {
    while (position < input.size() && input.at(position) != '\n') {
        position++;
    }
}

void Lexer::skipMultiLineComment() {
    while (position + 1 < input.size()) {
        if (input.at(position) == '*' && input.at(position + 1) == '/') {
            position += 2;
            return;
        }
        position++;
    }
    error("ERROR: Unterminated multi-line comment");
}

Token Lexer::lexIdentifierOrKeyword(){
    std::string str;
    while (position < input.size()) {
        char c = input.at(position);
        if (std::isalnum(c) || c == '_') {
            str += c;
            position++;
        } else {
            break;
        }
    }

    KeyWordType type = keyWordTypeFromString(str);
    if(type != NOT_A_KEYWORD){
        return Token(type);
    }else if(str != "include" && str != "define"){
        return Token(IDENTIFIER, str);
    }else{
        return Token(PP_DIRECTIVE, str);
    }
}

Token Lexer::lexNumber(){
    std::string num_str;
    int dotCount = 0;
    while(position < input.size()){
        char c = input.at(position);
        if(c == '.') dotCount++;
        if(std::isdigit(c) || c == '.'){
            num_str += c;
            position++;
        }else break;
    }

    if(dotCount > 1){
        error("ERROR: Invalid float literal: " + num_str);
    }

    if(dotCount == 1){
        return Token(FLOAT_LITERAL, num_str);
    }else{
        return Token(INTEGER_LITERAL, num_str);
    }
}

Token Lexer::lexStringLiteral(){
    std::string str;
    position++;
    while(position < input.size()){
        char c = input.at(position++);
        if(c == '\"') break;

        str += c;
        if(c == '\\') switch (peek(0)) {
            case 'n': str+='n';  position++; continue;;
            case 't': str+='t';  position++; continue;;
            case 'r': str+='r';  position++; continue;;
            case '\\':str+='\\'; position++; continue;;
            case '\'':str+='\''; position++; continue;;
            case '\"':str+='\"'; position++; continue;;
            default:
                error(std::string("ERROR: Invalid escape character '\\") + peek(0) + "'");
        }

    }
    return Token(STRING_LITERAL, str);
}

Token Lexer::lexCharLiteral() {
    position++; // Skip the opening '

    if (position >= input.size()) {
        error("ERROR: Unexpected end of input in character literal");
    }

    char c = input.at(position++);

    // Handle escape sequences
    if (c == '\\') {
        if (position >= input.size()) {
            error("ERROR: Unexpected end of input after escape character");
        }
        char next = input.at(position++);
        switch (next) {
            case 'n': c = '\n'; break;
            case 't': c = '\t'; break;
            case 'r': c = '\r'; break;
            case '\\': c = '\\'; break;
            case '\'': c = '\''; break;
            case '\"': c = '\"'; break;
            default:
                error(std::string("ERROR: Invalid escape character '\\") + next + "'");
        }
    }

    if (position >= input.size() || input.at(position) != '\'') {
        error("ERROR: Expected closing single quote for character literal");
    }
    position++; // Skip closing '

    return Token(CHAR_LITERAL, std::string(1, c));
}

Token Lexer::lexPreprocessorDirective(){
    position++; // skip # symbol
    std::string directive;

    // Read directive (e.g., "include")
    while (position < input.size()) {
        char c = input.at(position);
        if (std::isalnum(c)) {
            directive += c;
            position++;
        } else {
            break;
        }
    }

    // Skip whitespace after directive
    while (position < input.size() && std::isspace(input.at(position))) {
        position++;
    }

    if(directive == "include"){
        // Handle <...> or "..." as a single token
        if (position >= input.size()) {
            error("ERROR: Unexpected end after #include");
        }

        char delim = input.at(position++);
        std::string path;

        if (delim == '<') {
            while (position < input.size() && input.at(position) != '>') {
                path += input.at(position++);
            }
            if (position >= input.size() || input.at(position) != '>') {
                error("ERROR: Unterminated include path");
            }
            position++; // skip closing '>'
        } else if (delim == '"') {
            while (position < input.size() && input.at(position) != '"') {
                path += input.at(position++);
            }
            if (position >= input.size() || input.at(position) != '"') {
                error("ERROR: Unterminated include path");
            }
            position++; // skip closing '"'
        } else {
            error("ERROR: Expected < or \" after #include");
        }

        return Token(INCLUDE_PATH, path);
    } else if(directive == "define"){
        return Token(PP_DIRECTIVE, directive); // You can extend handling later
    } else {
        error("ERROR Unknown preprocessor directive: " + directive);
    }

    return Token(UNKNOWN, "invalid");
}

char Lexer::next(){
    return peek(1);
}

Token Lexer::lexOperatorOrPunctuation() {
    char c = input.at(position);

    

    switch (c) {
        case '+':
            if (next() == '+') {
                position += 2;
                return Token(TokenType::INCREMENT, "++");
            } else if (next() == '=') {
                position += 2;
                return Token(TokenType::PLUS_ASSIGN, "+=");
            } else {
                position++;
                return Token(TokenType::PLUS, "+");
            }

        case '-':
            if (next() == '-') {
                position += 2;
                return Token(TokenType::DECREMENT, "--");
            } else if (next() == '=') {
                position += 2;
                return Token(TokenType::MINUS_ASSIGN, "-=");
            } else if (next() == '>') {
                position += 2;
                return Token(TokenType::ARROW, "->");
            } else {
                position++;
                return Token(TokenType::MINUS, "-");
            }

        case '*':
            if (next() == '=') {
                position += 2;
                return Token(TokenType::STAR_ASSIGN, "*=");
            } else {
                position++;
                return Token(TokenType::STAR, "*");
            }

        case '/':
            if (next() == '=') {
                position += 2;
                return Token(TokenType::SLASH_ASSIGN, "/=");
            } else {
                position++;
                return Token(TokenType::SLASH, "/");
            }

        case '=':
            if (next() == '=') {
                position += 2;
                return Token(TokenType::EQ, "==");
            } else {
                position++;
                return Token(TokenType::ASSIGN, "=");
            }

        case '!':
            if (next() == '=') {
                position += 2;
                return Token(TokenType::NEQ, "!=");
            } else {
                position++;
                return Token(TokenType::NOT, "!");
            }

        case '%':
            position++;
            return Token(TokenType::PERCENT, "%%");
        
        case '<':
            if(next() == '='){
                position += 2;
                return Token(TokenType::LTE, "<=");
            }
            if(next() == '<'){
                position += 2;
                return Token(TokenType::SHIFT_LEFT, "<<");
            }else{
                position++;
                return Token(TokenType::LT, "<");
            }

        case '>':
            if(next() == '='){
                position += 2;
                return Token(TokenType::GTE, ">=");
            }
            if(next() == '>'){
                position += 2;
                return Token(TokenType::SHIFT_RIGHT, ">>");
            }else{
                position++;
                return Token(TokenType::GT, ">");
            }

        case '&':
            if(next() == '&'){
                position += 2;
                return Token(TokenType::AND, "&&");
            }else{
                position++;
                return Token(TokenType::BIT_AND, "&");
            }
        case '|':
            if(next() == '|'){
                position += 2;
                return Token(TokenType::OR, "||");
            }else{
                position++;
                return Token(TokenType::BIT_OR, "|");
            }

        case '^':
            position++;
            return Token(TokenType::BIT_XOR, "^");

        case '~':
            position++;
            return Token(TokenType::BIT_NOT, "~");

        case '(':
            position++;
            return Token(TokenType::LPAREN, "(");

        case ')':
            position++;
            return Token(TokenType::RPAREN, ")");

        case '{':
            position++;
            return Token(TokenType::LBRACE, "{");

        case '}':
            position++;
            return Token(TokenType::RBRACE, "}");

        case '[':
            position++;
            return Token(TokenType::LBRACKET, "[");

        case ']':
            position++;
            return Token(TokenType::RBRACKET, "]");
                
        case '?':
            position++;
            return Token(TokenType::QUESTION, "?");

        case ':':
            position++;
            return Token(TokenType::COLON, ":");

        case ',':
            position++;
            return Token(TokenType::COMMA, ",");

        case '.':
            position++;
            return Token(TokenType::DOT, ".");

        case ';':
            position++;
            return Token(TokenType::SEMICOLON, ";");

        default:
            error("Invalid or unhandled operator: " + std::string(1, c));
            position++;
            return Token(TokenType::UNKNOWN, std::string(1, c));
    }

    
}