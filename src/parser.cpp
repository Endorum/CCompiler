#include <cmath>
#include <cstddef>
#include <exception>
#include <sys/errno.h>

#include "../include/parser.hpp"


Token Parser::peek(size_t off) {
    if (isAtEnd(off)) return Token(TokenType::END_OF_FILE, "");
    return tokens[pos+off];
}

Token Parser::advance() {
    if (!isAtEnd()) pos++;
    return tokens[pos - 1];
}

bool Parser::match(TokenType type) {
    if (isAtEnd()) return false;
    if (peek().type != type) return false;
    pos++;
    return true;
}

bool Parser::expect(TokenType type) {
    if (match(type)) return true;
    error("Expected token of type: " + stringFromTokenType(type));
    return false; // just in case
}

bool Parser::isAtEnd(size_t off) {
    return pos+off >= tokens.size(); // || tokens[pos].type == TokenType::END_OF_FILE;
}


ASTNode* Parser::parseProgram(){
    root = new ASTNode(NT_Program);

    while (!isAtEnd()) {
        if (peek().type == TokenType::END_OF_FILE) {
            break;
        }

        // Handle struct and enum declarations first
        if (peek().type == KEYWORD &&
            (peek().kw_type == KEYWORD_STRUCT || peek().kw_type == KEYWORD_ENUM)) {

            ASTNode* decl = nullptr;
            if (peek().kw_type == KEYWORD_STRUCT) {
                decl = parseStructDecl();
            } else if (peek().kw_type == KEYWORD_ENUM) {
                decl = parseEnumDecl();
            }

            if (decl) root->addChild(decl);
            continue;
        }

        // Handle type specifier-based entries (could be function or global variable)
        if (peek().type == KEYWORD && getTypeSpec(peek().value) != TS_NONE) {
            // Lookahead: if after type we have IDENTIFIER and then LPAREN, it's a function
            if (peek(1).type == TokenType::IDENTIFIER && peek(2).type == TokenType::LPAREN) {
                ASTNode* decl = parseFunctionDecl();
                if (decl) {
                    root->addChild(decl);
                } else {
                    error("Expected a function declaration/definition");
                }
            } else {
                root->addChild(parseVarDecl()); // global variable declaration
            }
            continue;
        }

        error("Unexpected token at top level: " + peek().str());
    }

    return root;
}

TypeSpecifier Parser::getTypeSpec(const std::string& str){
    if(str == "void") return TS_VOID;
    if(str == "char") return TS_CHAR;
    if(str == "short") return TS_SHORT;
    if(str == "int") return TS_INT;
    if(str == "long") return TS_LONG;
    if(str == "float") return TS_FLOAT;
    if(str == "double") return TS_DOUBLE;
    if(str == "signed") return TS_SIGNED;
    if(str == "unsigned") return TS_UNSIGNED;
    return TS_NONE;
}

bool Parser::isTypeSpecifierStart() {
    if (peek().type == TokenType::KEYWORD) {
        if (getTypeSpec(peek().value) != TS_NONE) return true;
        if (peek().kw_type == KEYWORD_STRUCT || peek().kw_type == KEYWORD_ENUM) return true;
    }
    return false;
}

ASTNode* Parser::parseFunctionDecl(){
    /*
    format:

    typespec.
    identifier
    '('
    param-list
    ')'
    '{' ? -> FuncDef : ';' ? -> FuncDec
    stmt-list
    '}'
    */


    ASTNode* returnType = parseTypeSpecifier();
    if(!returnType) error("Expected type specifier at function declaration");
    
    ASTNode* funcName = parseIdentifier();
    if(!funcName) error("Expected function name after return type");
    
    
    if(!match(LPAREN)) error("Expected '(' after function name");
    

    ASTNode* paramList = new ASTNode(NT_ParamList);
    while(peek().type != TokenType::RPAREN){
        if(isAtEnd()) error("Unterminated parameter list in function declaration");
        ASTNode* param = parseParameter();
        paramList->addChild(param);

        if(peek().type == TokenType::RPAREN) break;
        advance(); // past the comma

    }
    advance(); // past )

    ASTNode* func = new ASTNode(NT_FunctionDecl);
    func->addChild(returnType);
    func->addChild(funcName);
    func->addChild(paramList);

    if(match(LBRACE)){
        ASTNode* body = parseCompoundStmt(); // parse body
        func->addChild(body);
    } else if(!match(SEMICOLON)){
        error("Expected '{' for function body or ';' for declaration end");
    }

    return func;
}

ASTNode* Parser::parseStructDecl(){
    /*
    struct <ident> <statement>;
    */
    expect(KEYWORD);
    
    Token name = advance();
    ASTNode* structDecl = new ASTNode(NT_StructDecl, name.value);

    if(match(LBRACE)){
        while (!match(RBRACE)) {
            ASTNode* member = parseVarDecl(); // e.g. int x;
            structDecl->addChild(member);
        }
    }
    expect(SEMICOLON);

    return structDecl;

}

ASTNode* Parser::parseEnumDecl() {
    advance(); // consume 'enum'
    ASTNode* enumDecl = new ASTNode(NT_EnumDecl);

    // Optional name
    if (peek().type == IDENTIFIER) {
        std::string enumName = advance().value;
        enumDecl->addChild(new ASTNode(NT_Identifier, enumName));
    }

    // Forward declaration
    if (match(SEMICOLON)) {
        return enumDecl;
    }

    // Definition with member list
    expect(LBRACE);
    while (!match(RBRACE)) {
        if (peek().type != IDENTIFIER) {
            error("Expected identifier in enum member list");
        }

        std::string name = advance().value;
        ASTNode* member = new ASTNode(NT_EnumMember, name);

        if (match(ASSIGN)) {
            ASTNode* valueExpr = parseExpression();
            member->addChild(valueExpr);
        }

        enumDecl->addChild(member);

        if (peek().type == RBRACE) break;
        expect(COMMA);
    }
    if(peek().type == RBRACE) advance();
    
    expect(SEMICOLON);
    return enumDecl;
}



ASTNode* Parser::parseTypeSpecifier(){
    if (peek().type != TokenType::KEYWORD) return nullptr;
    // if (getTypeSpec(peek().value) == TS_NONE) return nullptr; // no longer works as struct <ident> is also a typename now

    if(peek().type == TokenType::KEYWORD){
        if(peek().kw_type == KEYWORD_STRUCT){
            advance(); // consume struct keyword
            if(peek().type != IDENTIFIER) error("Expected struct name in type specifier");

            Token structName = advance();
            ASTNode* structType = new ASTNode(NT_StructType, structName.value);

            while(match(TokenType::STAR)){
                ASTNode* ptr = new ASTNode(NT_PointerType);
                ptr->addChild(structType);
                structType = ptr;
            }

            return structType;
        }
        if(peek().kw_type == KEYWORD_ENUM){
            advance(); 
            if(peek().type != IDENTIFIER) error("Expected enum name in type specifier");

            Token enumName = advance();
            ASTNode* enumType = new ASTNode(NT_EnumType, enumName.value);

            while(match(TokenType::STAR)){
                ASTNode* ptr = new ASTNode(NT_PointerType);
                ptr->addChild(enumType);
                enumType = ptr;
            }

            return enumType;

        }
        if(getTypeSpec(peek().value) != TS_NONE){
            Token base = advance();
            ASTNode* type = new ASTNode(NT_TypeSpecifier, base.value);
            while(match(STAR)){
                ASTNode* ptr = new ASTNode(NT_PointerType);
                ptr->addChild(type);
                type = ptr;
            }
            return type;
        }
    }
    return nullptr;
}

ASTNode* Parser::parseIdentifier(){
    if(peek().type != TokenType::IDENTIFIER) return nullptr;
    return new ASTNode(NT_Identifier, advance().value);
}

ASTNode* Parser::parseParameter(){
    ASTNode* parameter = new ASTNode(NT_Parameter);

    ASTNode* typespec = parseTypeSpecifier();
    ASTNode* ident = parseIdentifier();

    if(!typespec) error("Expected a type specifier for a parameter");
    if(!ident) error("Expected an identifier for parameter");

    parameter->addChild(typespec);
    parameter->addChild(ident);

    return parameter;
}


ASTNode* Parser::parseStatement(){

    if(peek().kw_type == KEYWORD_RETURN){
        return parseReturnStmt();
    }

    if(peek().type == LBRACE){
        return parseCompoundStmt();
    }

    // Variable declaration (e.g. int x = 5;) or struct STRUCTNAME <objname>
    pos--; // for some reason, for "struct Obj object" peek() = "Obj" and not struct from 
    // whre isTypeSpecifierStart expects to start from
    if (isTypeSpecifierStart()) {
        return parseVarDecl();
    }
    pos++;

    if (peek().type == KEYWORD && peek().kw_type == KEYWORD_IF) {
        return parseIfStmt();
    }

    if (peek().type == KEYWORD && peek().kw_type == KEYWORD_WHILE) {
        return parseWhileStmt();
    }

    if (peek().type == KEYWORD && peek().kw_type == KEYWORD_DO) {
        return parseDoStmt();
    }

    if (peek().type == KEYWORD && peek().kw_type == KEYWORD_FOR) {
        return parseForStmt();
    }

    if(peek().type == KEYWORD && peek().kw_type == KEYWORD_SWITCH){
        return parseSwitchStmt();
    }

    if(peek().type == KEYWORD && peek().kw_type == KEYWORD_CASE){
        return parseCaseStmt();
    }

    if(peek().type == KEYWORD && peek().kw_type == KEYWORD_DEFAULT){
        return parseDefaultStmt();
    }

    if(peek().type == KEYWORD && peek().kw_type == KEYWORD_GOTO){
        return parseGotoStmt();
    }

    if(peek().type == KEYWORD && peek().kw_type == KEYWORD_CONTINUE){
        advance();
        expect(SEMICOLON);

        return new ASTNode(NT_ContinueStmt);
    }

    if(peek().type == KEYWORD && peek().kw_type == KEYWORD_BREAK){
        advance();
        expect(SEMICOLON);

        return new ASTNode(NT_BreakStmt);
    }

    ASTNode* expr = parseExpression();
    if(!expr) error("Expected expression statement");
    expect(SEMICOLON);

    ASTNode* exprStmt = new ASTNode(NT_ExpressionStmt);
    exprStmt->addChild(expr);

    return exprStmt;
}

ASTNode* Parser::parseCompoundStmt(){
    advance(); // past {
    ASTNode* block = new ASTNode(NT_CompoundStmt);

    while(!isAtEnd() && peek().type != RBRACE){
        ASTNode* stmt = parseStatement();
        if(!stmt){
            error("Invalid statement in compound block");
        }
        block->addChild(stmt);
    }

    if(!match(RBRACE)){
        error("Expeced closing '}' at end of compound statement");
    }

    return block;
}

ASTNode* Parser::parseReturnStmt(){
    advance(); // past return kw
    ASTNode* returnNode = new ASTNode(NT_ReturnStmt);

    if(peek().type != SEMICOLON){
        ASTNode* expr = parseExpression();
        if(!expr) error("Expected expression after return");
        returnNode->addChild(expr);
    }

    expect(SEMICOLON);
    return returnNode;
}
ASTNode* Parser::parseVarDecl(){
    ASTNode* decl = new ASTNode(NT_Declaration);

    ASTNode* type = parseTypeSpecifier();
    ASTNode* name = parseIdentifier();
    if (!name) {
        error("Expected identifier in declaration, got: " + peek().str());
    }

    decl->addChild(type);
    decl->addChild(name);

    if(match(ASSIGN)){
        ASTNode* expr = parseExpression();
        if(!expr) error("Expected initializer expression");
        decl->addChild(expr);
    }

    expect(SEMICOLON);
    return decl;
}
ASTNode* Parser::parseIfStmt(){
    advance(); // skip 'if'
    expect(LPAREN);
    ASTNode* cond = parseExpression();
    expect(RPAREN);
    ASTNode* trueStmt = parseStatement();
    ASTNode* ifStmt = new ASTNode(NT_IfStmt);
    ifStmt->addChild(cond);
    ifStmt->addChild(trueStmt);

    if (peek().type == KEYWORD && peek().kw_type == KEYWORD_ELSE) {
        advance(); // skip 'else'
        ASTNode* falseStmt = parseStatement();
        ifStmt->addChild(falseStmt);
    }

    return ifStmt;
}
ASTNode* Parser::parseWhileStmt(){
    advance(); // skip 'while'
    expect(LPAREN);
    ASTNode* cond = parseExpression();
    expect(RPAREN);
    ASTNode* body = parseStatement();
    ASTNode* whileStmt = new ASTNode(NT_WhileStmt);
    whileStmt->addChild(cond);
    whileStmt->addChild(body);
    return whileStmt;
}
ASTNode* Parser::parseDoStmt(){
    advance(); // skip 'do'
    ASTNode* body = parseStatement();
    if (peek().type != KEYWORD || peek().kw_type != KEYWORD_WHILE) {
        error("Expected 'while' after 'do' body");
    }
    advance(); // skip 'while'
    expect(LPAREN);
    ASTNode* cond = parseExpression();
    expect(RPAREN);
    expect(SEMICOLON);
    ASTNode* doStmt = new ASTNode(NT_DoStmt);
    doStmt->addChild(body);
    doStmt->addChild(cond);
    return doStmt;
}
ASTNode* Parser::parseForStmt(){
    advance(); // skip 'for'
    expect(LPAREN);

    ASTNode* forStmt = new ASTNode(NT_ForStmt);

    // Parse initializer (can be empty)
    // Parse initializer (can be a declaration or expression)
    if (peek().type != SEMICOLON) {
        ASTNode* init = nullptr;
        if (peek().type == KEYWORD && getTypeSpec(peek().value) != TS_NONE) {
            init = parseStatement(); // Will consume the semicolon
            // Remove the last child (ExpressionStmt) if you want to avoid nesting
        } else {
            init = parseExpression();
            expect(SEMICOLON);
        }
        forStmt->addChild(init);
    } else {
        expect(SEMICOLON);
        forStmt->addChild(nullptr);
    }
    

    // Parse condition (can be empty)
    if (peek().type != SEMICOLON) {
        ASTNode* cond = parseExpression();
        forStmt->addChild(cond);
    } else {
        forStmt->addChild(nullptr); // empty condition
    }
    expect(SEMICOLON);

    // Parse increment (can be empty)
    if (peek().type != RPAREN) {
        ASTNode* update = parseExpression();
        forStmt->addChild(update);
    } else {
        forStmt->addChild(nullptr); // empty update
    }
    expect(RPAREN);

    // Loop body
    ASTNode* body = parseStatement();
    forStmt->addChild(body);

    return forStmt;
}
ASTNode* Parser::parseSwitchStmt(){
    /*
    switch ( <expression> ) <statement>
    */
    advance(); // past switch kw
    
    expect(LPAREN);
    ASTNode* expr = parseExpression();
    expect(RPAREN);

    ASTNode* body = parseStatement();

    ASTNode* switchStmt = new ASTNode(NT_SwitchStmt);
    switchStmt->addChild(expr);
    switchStmt->addChild(body);

    return switchStmt;
}
ASTNode* Parser::parseCaseStmt(){
    /*
    case <const-expr> : <statement>
    */
    advance(); // past "case" kw
    ASTNode* constExpr = parseExpression();
    expect(COLON);
    ASTNode* body = parseStatement();

    ASTNode* caseStmt = new ASTNode(NT_CaseStmt);
    caseStmt->addChild(constExpr);
    caseStmt->addChild(body);

    return caseStmt;
}
ASTNode* Parser::parseDefaultStmt(){
    /*
    default: <statement>
    */
    advance(); // past "defalt" kw
    expect(COLON);
    ASTNode* body = parseStatement();
    ASTNode* defaultStmt = new ASTNode(NT_DefaultStmt);
    defaultStmt->addChild(body);

    return defaultStmt;
}
ASTNode* Parser::parseGotoStmt(){
    /*
    goto <identifier>
    */
    advance();
    ASTNode* ident = parseIdentifier();
    ASTNode* gotoStmt = new ASTNode(NT_GotoStmt);
    gotoStmt->addChild(ident);
    expect(SEMICOLON);

    return gotoStmt;
}

ASTNode* Parser::parseExpression() {
    if (peek().kw_type == KEYWORD_SIZEOF) {
        return parseSizeofExpr();
    }

    if (peek().type == TokenType::LPAREN && getTypeSpec(peek(1).value) != TS_NONE) {
        return parseTypeCastExpr();
    }

    if (
        peek().type == TokenType::MINUS || 
        peek().type == TokenType::NOT || 
        peek().type == TokenType::BIT_NOT || 
        peek().type == TokenType::STAR ||
        peek().type == TokenType::BIT_AND ||
        peek().type == TokenType::INCREMENT ||
        peek().type == TokenType::DECREMENT 
    ) {
        return parseUnaryExpr();
    }

    return parseBinaryExpr();
}

ASTNode* Parser::parseSizeofExpr(){
    advance(); // sizeof
    expect(LPAREN);
    ASTNode* expr = parseExpression();
    expect(RPAREN);
    ASTNode* sizeofExpr = new ASTNode(NT_SizeofExpr);
    sizeofExpr->addChild(expr);
    return sizeofExpr;
}
ASTNode* Parser::parseTypeCastExpr(){
    expect(LPAREN);
    ASTNode* type = parseTypeSpecifier();
    expect(RPAREN);
    ASTNode* expr = parsePrimary();
    ASTNode* cast = new ASTNode(NT_TypeCastExpr);
    cast->addChild(type);
    cast->addChild(expr);
    return cast;
}
ASTNode* Parser::parseUnaryExpr(){
    Token op = advance();
    ASTNode* operand = parsePrimary();
    ASTNode* node = new ASTNode(NT_UnaryExpr, op.value);
    node->addChild(operand);
    return node;
}
ASTNode* Parser::parseBinaryExpr(){
    ASTNode* left = parsePrimary();
    if (!left) return nullptr;

    while (!isAtEnd()) {
        Token op = peek();

        // Handle binary and assignment operators (simple version)
        if (op.type == TokenType::PLUS ||
            op.type == TokenType::MINUS ||
            op.type == TokenType::STAR ||
            op.type == TokenType::SLASH ||
            op.type == TokenType::PERCENT ||
            op.type == TokenType::ASSIGN ||
            op.type == TokenType::PLUS_ASSIGN ||
            op.type == TokenType::MINUS_ASSIGN ||
            op.type == TokenType::STAR_ASSIGN ||
            op.type == TokenType::SLASH_ASSIGN ||
            op.type == TokenType::EQ ||
            op.type == TokenType::NEQ ||
            op.type == TokenType::GT ||
            op.type == TokenType::GTE ||
            op.type == TokenType::LT ||
            op.type == TokenType::LTE ||
            op.type == TokenType::AND ||
            op.type == TokenType::OR ||
            op.type == TokenType::BIT_AND ||
            op.type == TokenType::BIT_OR ||
            op.type == TokenType::BIT_XOR) {
            
            advance(); // consume operator

            ASTNode* right = parsePrimary();
            if (!right) error("Expected expression after binary operator: " + op.value);

            ASTNode* binExpr = new ASTNode(
                op.type == TokenType::ASSIGN ? NT_Assignment : NT_BinaryExpr,
                op.value
            );
            binExpr->addChild(left);
            binExpr->addChild(right);
            left = binExpr;
        }
        else if (match(TokenType::QUESTION)) {
            ASTNode* ternary = new ASTNode(NT_TernaryExpr, "?");
            ternary->addChild(left);

            ASTNode* ifTrue = parseExpression();
            expect(TokenType::COLON);
            ASTNode* ifFalse = parseExpression();

            ternary->addChild(ifTrue);
            ternary->addChild(ifFalse);
            return ternary;
        }
        else {
            break;
        }
    }

    return left;
}



ASTNode* Parser::parsePrimary(){

    Token tok = peek();

    if(tok.type == TokenType::IDENTIFIER){
        advance();
        ASTNode* id = new ASTNode(NT_Identifier, tok.value);

        if(peek().type == LPAREN){
            advance();
            ASTNode* call = new ASTNode(NT_CallExpr);
            call->addChild(id);

            while(!match(TokenType::RPAREN)){
                ASTNode* arg = parseExpression();
                if(!arg) error("Expected expression in function call");
                call->addChild(arg);

                if(match(RPAREN)) break;
                expect(TokenType::COMMA);
            }

            return call;
        }

        if (match(TokenType::LBRACKET)) {
            ASTNode* arrSub = new ASTNode(NT_ArraySubscripting);
            arrSub->addChild(id); // the array variable

            ASTNode* indexExpr = parseExpression();
            if (!indexExpr) error("Expected index expression inside array subscript");
            arrSub->addChild(indexExpr);

            expect(TokenType::RBRACKET);
            return arrSub;
        }

        return id;
    }
    
    if(
        tok.type == TokenType::INTEGER_LITERAL ||
        tok.type == TokenType::FLOAT_LITERAL ||
        tok.type == TokenType::CHAR_LITERAL ||
        tok.type == TokenType::STRING_LITERAL 
    ) {
        advance();
        ASTNode* lit = new ASTNode(NT_Literal, tok.value);
        switch (tok.type) {
            default: lit->dataType = TS_NONE; break;
            case INTEGER_LITERAL: lit->dataType = TS_INT; break;
            case FLOAT_LITERAL: lit->dataType = TS_FLOAT; break;
            case CHAR_LITERAL: lit->dataType = TS_CHAR; break;
            case STRING_LITERAL: lit->dataType = TS_CHAR; break;
        }
        
        return lit;
    }

    if(tok.type == TokenType::LPAREN){
        advance();
        ASTNode* expr = parseExpression();
        expect(TokenType::RPAREN);
        return expr;
    }

    return nullptr;
}
