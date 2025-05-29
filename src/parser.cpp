#include "../include/parser.hpp"
#include <sys/errno.h>



Token Parser::peek() {
    if (isAtEnd()) return Token(TokenType::END_OF_FILE, "");
    return tokens[pos];
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

bool Parser::isAtEnd() {
    return pos >= tokens.size(); // || tokens[pos].type == TokenType::END_OF_FILE;
}


ASTNode* Parser::parseProgram(){
    ASTNode* root = new ASTNode(NT_Program);

    while(!isAtEnd()){
        if(peek().type == TokenType::END_OF_FILE){
            break;
        }if(peek().type == KEYWORD){
            ASTNode* decl = parseFunctionDecl();
            if(decl) {
                root->addChild(decl);
            }
            else{
                error("Expected a function declaration/definition at top level. got keyword: " + peek().value + " instead");
            }
        }else{
            error("Unexpected token at top level: " + peek().str());
        }
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

ASTNode* Parser::parseTypeSpecifier(){
    if (peek().type != TokenType::KEYWORD) return nullptr;
    if (getTypeSpec(peek().value) == TS_NONE) return nullptr;

    // Base type: int, char, etc.
    Token base = advance();
    ASTNode* type = new ASTNode(NT_TypeSpecifier, base.value);

    // Handle one or more '*' tokens indicating pointer depth
    while (match(TokenType::STAR)) {
        ASTNode* ptrType = new ASTNode(NT_PointerType);
        ptrType->addChild(type);
        type = ptrType;
    }

    return type;
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

ASTNode* Parser::parseCompoundStmt(){
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
ASTNode* Parser::parseStatement(){
    /*
    
    <statement> ::= <labeled-statement>
                | <expression-statement>
                | <compound-statement>
                | <selection-statement>
                | <iteration-statement>
                | <jump-statement>

    <labeled-statement> ::= <identifier> : <statement>
                        | case <constant-expression> : <statement>
                        | default : <statement>

    <expression-statement> ::= {<expression>}? ;

    <selection-statement> ::= if ( <expression> ) <statement>
                            | if ( <expression> ) <statement> else <statement>
                            | switch ( <expression> ) <statement>

    <iteration-statement> ::= while ( <expression> ) <statement>
                            | do <statement> while ( <expression> ) ;
                            | for ( {<expression>}? ; {<expression>}? ; {<expression>}? ) <statement>

    <jump-statement> ::= goto <identifier> ;
                    | continue ;
                    | break ;
                    | return {<expression>}? ;
    
    */

    if(peek().kw_type == KEYWORD_RETURN){
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

    if(peek().type == LBRACE){
        advance(); // past {
        return parseCompoundStmt();
    }

    // Variable declaration (e.g. int x = 5;)
    if(peek().type == KEYWORD && getTypeSpec(peek().value) != TS_NONE){
        ASTNode* decl = new ASTNode(NT_Declaration);

        ASTNode* type = parseTypeSpecifier();
        ASTNode* name = parseIdentifier();
        if(!name) error("Expected identifier in declaration");

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

    ASTNode* expr = parseExpression();
    if(!expr) error("Expected expression statement");
    expect(SEMICOLON);

    ASTNode* exprStmt = new ASTNode(NT_ExpressionStmt);
    exprStmt->addChild(expr);

    return exprStmt;
}
ASTNode* Parser::parseExpression(){
    /*
    •	Full operator precedence (using recursive descent or Pratt parsing)
    •	Unary operators (-x, !x, *ptr)
    •	Function calls (foo(x, y))
    •	Array subscripting (arr[3])
    •	Struct access (x.y, x->y)
    •	Ternary expressions (cond ? a : b)
    */
    Token op = peek();

    if(
        op.type == TokenType::MINUS || 
        op.type == TokenType::NOT || 
        op.type == TokenType::BIT_NOT || 
        op.type == TokenType::STAR ||
        op.type == TokenType::BIT_AND
    ){
        // handle unary expr;
        advance(); // past op
        ASTNode* operand = parsePrimary();
        ASTNode* unExpr = new ASTNode(NT_UnaryExpr, op.value);
        unExpr->addChild(operand);
        return unExpr;
    }

    ASTNode* left = parsePrimary();
    if(!left) return nullptr; // error("Expected primary") ?

    while(!isAtEnd()){
        Token op = peek();

        // only handle basic binary operators for now
        if(
            op.type != TokenType::ASSIGN &&

            op.type != TokenType::PLUS && 
            op.type != TokenType::MINUS && 
            op.type != TokenType::STAR && 
            op.type != TokenType::SLASH &&
            
            op.type != TokenType::GT && 
            op.type != TokenType::GTE && 
            op.type != TokenType::EQ && 
            op.type != TokenType::LT && 
            op.type != TokenType::LTE && 1

        ) break;

        advance();

        ASTNode* right = parsePrimary();
        if(!right) error("Expected expression after operator: " + op.value);

        ASTNode* binExpr = new ASTNode(
            (op.type == TokenType::ASSIGN) ? NT_Assignment : NT_BinaryExpr,
            op.value
        );

        binExpr->addChild(left);
        binExpr->addChild(right);
        left = binExpr; // left-associative
    }

    // Handle ternary operator (right-associative)
    if (match(TokenType::QUESTION)) {
        ASTNode* ternary = new ASTNode(NT_TernaryExpr, "?");

        ternary->addChild(left); // condition

        ASTNode* trueExpr = parseExpression();
        if (!trueExpr) error("Expected expression after '?' in ternary expression");
        ternary->addChild(trueExpr);

        expect(TokenType::COLON);

        ASTNode* falseExpr = parseExpression();
        if (!falseExpr) error("Expected expression after ':' in ternary expression");
        ternary->addChild(falseExpr);

        return ternary;
    }

    return left; // binExpr ?
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
        return new ASTNode(NT_Literal, tok.value);
    }

    if(tok.type == TokenType::LPAREN){
        advance();
        ASTNode* expr = parseExpression();
        expect(TokenType::RPAREN);
        return expr;
    }

    return nullptr;
}