#include <cmath>
#include <cstddef>
#include <exception>
#include <string>
#include <sys/errno.h>

#include "../include/parser.hpp"

void SymbolTable::enterScope() {
    scopes.emplace_back();
}

void SymbolTable::exitScope() {
    if (!scopes.empty()) {
        scopes.pop_back();
    }
}

void SymbolTable::define(const Symbol& sym) {
    if (scopes.empty()) enterScope();
    scopes.back()[sym.name] = sym;
}

void SymbolTable::define(SymbolKind kind, const std::string& name) {
    if (scopes.empty()) enterScope();
    scopes.back()[name] = Symbol{kind, name};
}

bool SymbolTable::isTypedef(const std::string& name) const {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return found->second.kind == SymbolKind::SYM_TYPEDEF;
        }
    }
    return false;
}

bool SymbolTable::isDefined(const std::string& name) const {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        if (it->find(name) != it->end()) return true;
    }
    return false;
}

SymbolKind SymbolTable::getKind(const std::string& name) const {
    for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
        auto found = it->find(name);
        if (found != it->end()) {
            return found->second.kind;
        }
    }
    return SymbolKind::SYM_VARIABLE; // default fallback or you can throw
}

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

        if(peek().type == TokenType::INCLUDE_PATH){
            advance();
            continue;
        }

        if(peek().kw_type == KEYWORD_STRUCT){
            ASTNode* decl = parseStructDecl();
            if (decl) root->addChild(decl);
            else
                error("Struct declaration was null");
            continue;
        }

        if(peek().kw_type == KEYWORD_ENUM){
            ASTNode* decl = parseEnumDecl();
            if (decl) root->addChild(decl);
            else
                error("Enum declaration was null");
            continue;
        }

        if(peek().kw_type == KEYWORD_TYPEDEF){
            root->addChild(parseTypedefDecl());
            continue;
        }

        TypeSpecifier typespec = getTypeSpec(peek().value);

        // Handle type specifier-based entries (could be function or global variable)
        if (typespec != TS_NONE) {
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
    // if str is typedef table
    return TS_NONE;
}

bool Parser::isTypeSpecifierStart() {
    if (peek().type == TokenType::KEYWORD) {
        if (getTypeSpec(peek().value) != TS_NONE) return true;
        if (peek().kw_type == KEYWORD_STRUCT || peek().kw_type == KEYWORD_ENUM) return true;
        if(symbols.isDefined(peek().value)) return true;
    }else if (symbols.isTypedef(peek().value)) {
        return true;
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

    currentScopeName = funcName->value; // for the locals and parameters
    

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

    func->typeInfo = returnType->typeInfo;

    Function funcStruct;
    funcStruct.name = funcName->value;
    funcStruct.returnType = returnType->typeInfo;
    funcStruct.paramCount = paramList->children.size();

    symbols.defineFunction(funcStruct);

    

    if(peek().type == LBRACE){
        ASTNode* body = parseCompoundStmt(); // parse body
        func->addChild(body);
    } else if(!match(SEMICOLON)){
        error("Expected '{' for function body or ';' for declaration end");
    }

    currentScopeName = ""; // back to global scope

    return func;
}

ASTNode* Parser::parseStructDecl(){
    /*
    struct <ident> <statement>;
    */
    expect(KEYWORD);
    
    ASTNode* name = parseIdentifier();


    if(name == nullptr){
        name = new ASTNode(NT_None, "Unnamed struct");
    }
    
    ASTNode* structDecl = new ASTNode(NT_StructDecl, name->value);
    structDecl->addChild(name);

    if(match(LBRACE)){
        while (!match(RBRACE)) {
            ASTNode* member = parseVarDecl(); // e.g. int x;
            structDecl->addChild(member);
        }
    }
    // expect(SEMICOLON);

    structDecl->typeInfo.base = BT_STRUCT;
    structDecl->typeInfo.name = name->value;

    return structDecl;

}

ASTNode* Parser::parseEnumDecl() {
    advance(); // consume 'enum'
    ASTNode* enumDecl = new ASTNode(NT_EnumDecl);

    std::string enumName = "Unnamed enum";
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

        Symbol enumMember;
        enumMember.kind = SYM_ENUM_MEMBER;
        enumMember.name = name;
        enumMember.owningScopeName = currentScopeName;
        enumMember.typeInfo.base = BT_INT;

        symbols.define(enumMember);

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

    enumDecl->typeInfo.base = BT_ENUM;
    enumDecl->typeInfo.name = enumName;
    
    return enumDecl;
}

ASTNode* Parser::parseTypedefDecl(){
    advance();
    ASTNode* decl = nullptr;
    if(peek().kw_type == KEYWORD_STRUCT){
        decl = parseStructDecl();
    }else if(peek().kw_type == KEYWORD_ENUM){
        decl = parseEnumDecl();
    }else if(getTypeSpec(peek().value) != TS_NONE){
        decl = parseVarDecl();
    }else{
        error("Invalid type definiton");
    }

    ASTNode* name = parseIdentifier();
    expect(SEMICOLON);
    ASTNode* TypedefDecl = new ASTNode(NT_Declaration, "typedef");
    TypedefDecl->addChild(name);
    TypedefDecl->addChild(decl);

    // ASTNode* type = parseTypeSpecifier();
    // ASTNode* name = parseIdentifier();
    // expect(SEMICOLON);

    symbols.define(SYM_TYPEDEF, name->value);

    // ASTNode* typedefNode = new ASTNode(NT_Declaration);
    // typedefNode->addChild(type);
    // typedefNode->addChild(name);

    TypedefDecl->typeInfo.base = BT_TYPEDEF_NAME;
    TypedefDecl->typeInfo.name = name->value;

    return TypedefDecl;
}

BaseType primTypeStrToBaseType(std::string str){
    if (str == "void") return BT_VOID;
    if (str == "char") return BT_CHAR;
    if (str == "short") return BT_SHORT;
    if (str == "int") return BT_INT;
    if (str == "long") return BT_LONG;
    if (str == "float") return BT_FLOAT;
    if (str == "double") return BT_DOUBLE;
    if (str == "signed") return BT_SIGNED;
    if (str == "unsigned") return BT_UNSIGNED;
    if (str == "struct") return BT_STRUCT;
    if (str == "enum") return BT_ENUM;
    if (str == "typedef") return BT_TYPEDEF_NAME;
    return BT_UNKNOWN;
}

ASTNode* Parser::parseTypeSpecifier(){
    ASTNode* node = nullptr;

    if (symbols.isTypedef(peek().value)) {
        node = new ASTNode(NT_TypeSpecifier, advance().value);

        node->typeInfo.base = BT_TYPEDEF_NAME;
        node->typeInfo.name = peek().value;

        return node;
    }

    if(peek().type == TokenType::KEYWORD){
        if(peek().kw_type == KEYWORD_STRUCT){
            advance(); // consume struct keyword
            if(peek().type != IDENTIFIER) error("Expected struct name in type specifier");

            Token structName = advance();
            ASTNode* structType = new ASTNode(NT_StructType, structName.value);

            structType->typeInfo.base = BT_STRUCT;
            structType->typeInfo.name = structName.value;

            while(match(TokenType::STAR)){
                structType->typeInfo.pointerLevel++;

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

            enumType->typeInfo.base = BT_ENUM;
            enumType->typeInfo.name = enumName.value;

            while(match(TokenType::STAR)){
                enumType->typeInfo.pointerLevel++;

                ASTNode* ptr = new ASTNode(NT_PointerType);
                ptr->addChild(enumType);
                enumType = ptr;
            }

            return enumType;

        }
        if(peek().kw_type == KEYWORD_TYPEDEF){
            advance();

        }
        if(getTypeSpec(peek().value) != TS_NONE){
            Token base = advance();
            ASTNode* type = new ASTNode(NT_TypeSpecifier, base.value);

            type->typeInfo.base = primTypeStrToBaseType(base.value);

            while(match(STAR)){
                type->typeInfo.pointerLevel++;

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

    parameter->typeInfo = typespec->typeInfo;

    Symbol parameterSym;

    parameterSym.kind = SYM_PARAMETER;
    parameterSym.name = ident->value;
    parameterSym.typeInfo = typespec->typeInfo;

    parameterSym.owningScopeName = currentScopeName;

    symbols.define(parameterSym);

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
    if (isTypeSpecifierStart()) {
        return parseVarDecl();
    }

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
    exprStmt->typeInfo = expr->typeInfo;

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

    ASTNode* expr;
    if(peek().type != SEMICOLON){
        expr = parseExpression();
        if(!expr) error("Expected expression after return");
        returnNode->addChild(expr);
    }else{
        expr = new ASTNode(NT_None);
        expr->typeInfo.base = BT_VOID;
    }

    returnNode->typeInfo = expr->typeInfo;

    expect(SEMICOLON);
    return returnNode;
}
ASTNode* Parser::parseVarDecl(){
    ASTNode* decl = new ASTNode(NT_Declaration);

    ASTNode* type = parseTypeSpecifier();
    Type typeInfo = type->typeInfo;

    ASTNode* name = parseIdentifier();
    if (!name) {
        error("Expected identifier in declaration, got: " + peek().str());
    }

    // Handle array subscripting after identifier (multi-dimensional allowed)
    while (match(LBRACKET)) {
        ASTNode* sizeExpr = nullptr;
        if (peek().type != RBRACKET) {
            sizeExpr = parseExpression(); // optional size
        }
        expect(RBRACKET);

        ASTNode* arraySub = new ASTNode(NT_ArraySubscripting);
        arraySub->addChild(name); // base
        if (sizeExpr)
            arraySub->addChild(sizeExpr);

        // Record this dimension in typeInfo
        if (sizeExpr && sizeExpr->getType() == NT_Literal) {
            // You could evaluate this to int, for now just push dummy value
            typeInfo.arrayDimensions.push_back(std::stoi(sizeExpr->getValue()));
        } else {
            // Flexible array → push -1 or 0 to mark unknown size
            typeInfo.arrayDimensions.push_back(-1);
        }

        name = arraySub; // chain arrays
    }

    decl->addChild(type);
    decl->addChild(name);
    decl->typeInfo = typeInfo;

    

    if(match(ASSIGN)){
        ASTNode* initNode = new ASTNode(NT_Initializer);

        if(peek(0).type == LBRACE){
            advance();
            ASTNode* initList = new ASTNode(NT_ExpressionList);
            initList->typeInfo.base = BT_INIT_LIST;

            if (!match(TokenType::RBRACE)) {
                while (true) {
                    ASTNode* element = parseExpression();
                    initList->addChild(element);

                    if (match(TokenType::RBRACE)) break;
                    expect(TokenType::COMMA);
                }
            }

            initNode->typeInfo = initList->typeInfo;

            initNode->addChild(initList);
        }else{
            ASTNode* expr = parseExpression();
            if(!expr) error("Expected initializer expression");

            initNode->typeInfo = expr->typeInfo;
            initNode->addChild(expr);
        }

        decl->addChild(initNode);
    }

    Symbol oldSym = symbols.getSymbol(name->getValue());

    


    // Add variable to symbol table
    Symbol sym;
    sym.kind = SYM_VARIABLE;
    sym.name = name->getValue(); // works even if name is wrapped in ArraySubscripting → its first child is Identifier

    // If name is ArraySubscripting, get the base identifier name
    ASTNode* baseNameNode = name;
    while (baseNameNode->getType() == NT_ArraySubscripting) {
        baseNameNode = baseNameNode->getChildren()[0]; // first child is base
    }

    sym.name = baseNameNode->getValue();
    sym.typeInfo = typeInfo;

    sym.owningScopeName = currentScopeName;

    // Now insert
    symbols.define(sym);


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

bool Parser::isConstantExpression(ASTNode* node){
    switch (node->getType()) {
        case NT_Literal: return true;

        // constant if operand is constant 
        case NT_UnaryExpr: return isConstantExpression(node->children.at(0));

        // constant if both sides are constant
        case NT_BinaryExpr:
            return isConstantExpression(node->children.at(0)) &&
                   isConstantExpression(node->children.at(1));

        case NT_Identifier:
            // Could be constant if it's an enum value or a #define (not implemented yet)
            if (symbols.isDefined(node->getValue())) {
                Symbol sym = symbols.getSymbol(node->getValue());
                return sym.kind == SYM_TYPEDEF || sym.kind == SYM_ENUM_MEMBER; // You may want SYM_CONSTANT or SYM_ENUM_MEMBER here!
            }
            return false;

        default: return false;

    }
}

ASTNode* Parser::parseCaseStmt(){
    /*
    case <const-expr> : <statement>
    */
    advance(); // past "case" kw

    ASTNode* constExpr = parseExpression();
    if(!isConstantExpression(constExpr)){
        error("Case expression must be a constant expression");
    }
    
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
    sizeofExpr->typeInfo.base = BT_INT; // unsinged int
    sizeofExpr->typeInfo.name = "unsigned"; // temp workaround
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
    cast->typeInfo = type->typeInfo;
    return cast;
}

ASTNode* Parser::parseUnaryExpr(){
    Token op = advance();
    ASTNode* operand = parsePrimary();
    ASTNode* node = new ASTNode(NT_UnaryExpr, op.value);

    if(op.type == BIT_AND){
        // address of -> pointerLevel++;
        node->typeInfo.pointerLevel++;
    }

    if (op.type == TokenType::STAR) {
        if (operand->typeInfo.pointerLevel > 0) {
            node->typeInfo.pointerLevel--;
        } else {
            node->typeInfo.base = BT_UNKNOWN;
        }
    }


    node->addChild(operand);
    node->typeInfo = operand->typeInfo;
    return node;
}

int Parser::getPrecedence(TokenType type) {
    switch (type) {
        case TokenType::ASSIGN:
        case TokenType::PLUS_ASSIGN:
        case TokenType::MINUS_ASSIGN:
        case TokenType::STAR_ASSIGN:
        case TokenType::SLASH_ASSIGN:
            return 1; // lowest precedence (right-associative)

        case TokenType::OR: return 2;
        case TokenType::AND: return 3;

        case TokenType::EQ:
        case TokenType::NEQ:
            return 4;

        case TokenType::LT:
        case TokenType::LTE:
        case TokenType::GT:
        case TokenType::GTE:
            return 5;

        case TokenType::PLUS:
        case TokenType::MINUS:
            return 6;

        case TokenType::STAR:
        case TokenType::SLASH:
        case TokenType::PERCENT:
            return 7;

        default:
            return -1; // not a binary operator
    }
}

bool Parser::isRightAssociative(TokenType type) {
    return (type == TokenType::ASSIGN ||
            type == TokenType::PLUS_ASSIGN ||
            type == TokenType::MINUS_ASSIGN ||
            type == TokenType::STAR_ASSIGN ||
            type == TokenType::SLASH_ASSIGN);
}

ASTNode* Parser::parseBinaryExpr(int minPrec) {
    ASTNode* left = parsePrimary();

    while (true) {
        Token op = peek();
        int prec = getPrecedence(op.type);
        if (prec < minPrec) break;

        advance();

        // Right-associative operators need (prec) vs (prec+1)
        int nextMinPrec = prec + (isRightAssociative(op.type) ? 0 : 1);
        ASTNode* right = parseBinaryExpr(nextMinPrec);

        ASTNode* binExpr = new ASTNode(
            op.type == TokenType::ASSIGN ? NT_Assignment : NT_BinaryExpr,
            op.value
        );
        


        if (op.type == TokenType::EQ ||
            op.type == TokenType::NEQ ||
            op.type == TokenType::LT ||
            op.type == TokenType::LTE ||
            op.type == TokenType::GT ||
            op.type == TokenType::GTE ||
            op.type == TokenType::AND ||
            op.type == TokenType::OR) {
            binExpr->typeInfo.base = BT_INT;
        }
        else if (left->typeInfo.base == BT_FLOAT || right->typeInfo.base == BT_FLOAT) {
            binExpr->typeInfo.base = BT_FLOAT;
        } else if (left->typeInfo.base == BT_INT || right->typeInfo.base == BT_INT) {
            binExpr->typeInfo.base = BT_INT;
        } else {
            binExpr->typeInfo.base = BT_UNKNOWN;
        }

        if (op.type == TokenType::ASSIGN) {
            binExpr->typeInfo = left->typeInfo;
        }

        binExpr->addChild(left);
        binExpr->addChild(right);

        left = binExpr;
    }

    return left;
}

bool isPostFixOperator(TokenType type){
    bool ret = (type == TokenType::INCREMENT ||
    type == TokenType::DECREMENT ||
    type == TokenType::ARROW ||
    type == TokenType::DOT);
    return ret;
}

ASTNode* Parser::parseAtom() {
    Token tok = peek();

    // Identifiers (e.g., variable names, maybe typedefs later)
    if (tok.type == TokenType::IDENTIFIER) {
        advance();
        ASTNode* node = new ASTNode(NT_Identifier, tok.value);

        // Look up the identifier in symbol table and assign type
        if (symbols.isDefined(tok.value)) {
            SymbolKind kind = symbols.getKind(tok.value);

            if (kind == SYM_VARIABLE 
                || kind == SYM_TYPEDEF 
                || kind == SYM_FUNCTION 
                || kind == SYM_PARAMETER 
                || kind == SYM_ENUM_MEMBER
            ) {
                
                Symbol sym = symbols.getSymbol(tok.value);
                node->typeInfo = sym.typeInfo;
                
            }
        } else {
            node->typeInfo.base = BT_UNKNOWN; // unknown identifier — will trigger error later
            error("Variable " + tok.value + " not defined in current scope: " + ((currentScopeName.empty()) ? "<global>" : currentScopeName));
        }

        return node;
    }

    // Literals
    if (
        tok.type == TokenType::INTEGER_LITERAL ||
        tok.type == TokenType::FLOAT_LITERAL ||
        tok.type == TokenType::CHAR_LITERAL ||
        tok.type == TokenType::STRING_LITERAL
    ) {
        advance();
        ASTNode* lit = new ASTNode(NT_Literal, tok.value);
        switch (tok.type) {
            default: lit->typeInfo.base = BT_UNKNOWN; break;
            case INTEGER_LITERAL: lit->typeInfo.base = BT_INT; break;
            case FLOAT_LITERAL: lit->typeInfo.base = BT_FLOAT; break;
            case CHAR_LITERAL: lit->typeInfo.base = BT_CHAR; break;
            case STRING_LITERAL:

                ///// temporary work around
                lit->typeInfo.base = BT_CHAR;
                lit->typeInfo.pointerLevel = 1;
                lit->typeInfo.arrayDimensions.push_back(tok.value.size());
                /////

                break;
        }
        return lit; 
    }

    // Parenthesized expression
    if (tok.type == TokenType::LPAREN) {
        advance(); // consume '('
        ASTNode* expr = parseExpression();
        expect(TokenType::RPAREN);
        return expr;
    }

    return nullptr; // no valid atom found
}

ASTNode* Parser::parseArgumentList(){
    ASTNode* ArgList = new ASTNode(NT_ExpressionList);

    if (!match(TokenType::RPAREN)) {
        while (true) {
            ArgList->addChild(parseExpression());
            if (match(TokenType::RPAREN)) break;
            expect(TokenType::COMMA);
        }
    }

    return ArgList;
}

ASTNode* Parser::parsePrimary() {
    ASTNode* expr = parseAtom(); // was the old parsePrimary()
    if (!expr) return nullptr;

    while (true) {
        Token tok = peek();

        if (tok.type == TokenType::LBRACKET) {
            advance();
            ASTNode* index = parseExpression();
            expect(TokenType::RBRACKET);

            ASTNode* arrSub = new ASTNode(NT_ArraySubscripting);
            arrSub->addChild(expr);
            arrSub->addChild(index);

            arrSub->typeInfo = expr->typeInfo;

            if (!arrSub->typeInfo.arrayDimensions.empty()) {
                arrSub->typeInfo.arrayDimensions.erase(
                    arrSub->typeInfo.arrayDimensions.begin()
                );
            } else {
                // Not an array? Fallback — treat as pointer dereference
                if (arrSub->typeInfo.pointerLevel > 0) {
                    arrSub->typeInfo.pointerLevel--;
                } else {
                    arrSub->typeInfo.base = BT_UNKNOWN;
                }
            }

            expr = arrSub;
        }
        else if (tok.type == TokenType::DOT || tok.type == TokenType::ARROW) {
            advance();
            if (peek().type != TokenType::IDENTIFIER) {
                error("Expected member name after '.' or '->'");
            }
            Token member = advance();

            ASTNode* access = new ASTNode(NT_StructAccess, tok.value);
            access->addChild(expr);
            access->addChild(new ASTNode(NT_Identifier, member.value));

            access->typeInfo.base = BT_UNKNOWN;
            expr = access;
        }
        else if (tok.type == TokenType::LPAREN) {
            advance();
            ASTNode* call = new ASTNode(NT_CallExpr);
            call->addChild(expr);

            ASTNode* ArgList = parseArgumentList();

            Function func = symbols.getFunction(expr->value);

            if(func.paramCount != ArgList->children.size()){
                error("Too many Arguments to function call, expected " + std::to_string(func.paramCount) + " have " + std::to_string(ArgList->children.size()));
            }

            call->addChild(ArgList);

            call->typeInfo = func.returnType;

            expr = call;
        }
        else if (tok.type == TokenType::INCREMENT || tok.type == TokenType::DECREMENT) {
            advance();
            ASTNode* postIncDec = new ASTNode(NT_PostFixExpr, tok.value);

            postIncDec->addChild(expr);
            postIncDec->typeInfo = expr->typeInfo;

            expr = postIncDec;
        } else {
            break;
        }
    }

    return expr;
}
