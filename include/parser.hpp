#pragma once

#include <cstddef>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>
// #include "utils.hpp"
// #include "defs.hpp"
#include "lexer.hpp"

enum BaseType{
    BT_VOID,
    BT_CHAR,
    BT_SHORT,
    BT_INT,
    BT_LONG,
    BT_FLOAT,
    BT_DOUBLE,
    BT_SIGNED,
    BT_UNSIGNED,
    BT_STRUCT,
    BT_ENUM,
    BT_TYPEDEF_NAME, // for typedef references
    BT_UNKNOWN,
    BT_INIT_LIST,
};


inline std::string baseTypeToString(BaseType base){
    switch(base){
        case BT_VOID: return "void";
        case BT_CHAR: return "char";
        case BT_SHORT: return "short";
        case BT_INT: return "int";
        case BT_LONG: return "long";
        case BT_FLOAT: return "float";
        case BT_DOUBLE: return "double";
        case BT_SIGNED: return "signed";
        case BT_UNSIGNED: return "unsigned";
        case BT_STRUCT: return "struct";
        case BT_ENUM: return "enum";
        case BT_TYPEDEF_NAME: return "typedef";
        case BT_INIT_LIST: return "init list";
        default: return "";
    }
}



struct Type{
    BaseType base=BT_UNKNOWN;
    std::string name=""; // for struct, enum, typedefs empty otherwise
    int pointerLevel = 0;
    std::vector<int> arrayDimensions; // e.g. int arr[5][6] -> (5,6)

    std::string str() const {
        std::string out;
        for (int i = 0; i < pointerLevel; ++i) out += "*";
        if (base == BT_STRUCT || base == BT_ENUM || base == BT_TYPEDEF_NAME) {
            out += name;
        } else {
            out += baseTypeToString(base);
        }
        for (int dim : arrayDimensions) {
            out += "[" + std::to_string(dim) + "]";
        }
        return out;
    }

};


struct Function{
    std::string name;
    Type returnType;
    size_t paramCount;
};

enum TypeSpecifier{
    TS_NONE,
    TS_VOID,
    TS_CHAR,
    TS_SHORT,
    TS_INT,
    TS_LONG,
    TS_FLOAT,
    TS_DOUBLE,
    TS_SIGNED,
    TS_UNSIGNED
};

enum NodeType {
    NT_None,
    NT_Program,
    NT_FunctionDecl,
    NT_FunctionDef,
    NT_ParamList,
    NT_ExpressionList,
    NT_Parameter,
    NT_CompoundStmt,
    NT_Declaration,
    NT_Assignment,
    NT_IfStmt,
    NT_WhileStmt,
    NT_ReturnStmt,
    NT_ExpressionStmt,
    NT_BinaryExpr,
    NT_UnaryExpr,
    NT_Literal,
    NT_Identifier,
    NT_CallExpr,
    NT_TypeSpecifier,
    NT_PointerType,
    NT_ArraySubscripting,
    NT_TernaryExpr,
    NT_StructAccess,
    NT_TypeCastExpr,
    NT_SizeofExpr,
    NT_DoStmt,
    NT_ForStmt,
    NT_SwitchStmt,
    NT_CaseStmt,
    NT_DefaultStmt,
    NT_GotoStmt,
    NT_ContinueStmt,
    NT_BreakStmt,
    NT_StructDecl,
    NT_EnumDecl,
    NT_StructType,
    NT_EnumMember,
    NT_EnumType,
    NT_PostFixExpr,
    NT_Initializer,
};

inline std::string nodeTypeToString(NodeType type){
    switch (type) {
    
    
        case NT_None: return "None";
        case NT_Program: return "Program";
        case NT_FunctionDecl: return "FunctionDecl";
        case NT_FunctionDef: return "FunctionDef";
        case NT_ParamList: return "ParamList";
        case NT_ExpressionList: return "ExpressionList";
        case NT_Parameter: return "Parameter";
        case NT_CompoundStmt: return "CompoundStmt";
        case NT_Declaration: return "Declaration";
        case NT_Assignment: return "Assignment";
        case NT_IfStmt: return "IfStmt";
        case NT_WhileStmt: return "WhileStmt";
        case NT_ReturnStmt: return "ReturnStmt";
        case NT_ExpressionStmt: return "ExpressionStmt";
        case NT_BinaryExpr: return "BinaryExpr";
        case NT_UnaryExpr: return "UnaryExpr";
        case NT_Literal: return "Literal";
        case NT_Identifier: return "Identifier";
        case NT_CallExpr: return "CallExpr";
        case NT_TypeSpecifier: return "TypeSpecifier";
        case NT_PointerType: return "PointerType";
        case NT_ArraySubscripting: return "ArraySubscripting";
        case NT_TernaryExpr: return "TernaryExpr";
        case NT_StructAccess: return "StructAccess";
        case NT_TypeCastExpr: return "TypeCastExpr";
        case NT_SizeofExpr: return "SizeofExpr";
        case NT_DoStmt: return "DoStmt";
        case NT_ForStmt: return "ForStmt";
        case NT_SwitchStmt: return "SwitchStmt";
        case NT_CaseStmt: return "CaseStmt";
        case NT_DefaultStmt: return "DefaultStmt";
        case NT_GotoStmt: return "GotoStmt";
        case NT_ContinueStmt: return "ContinueStmt";
        case NT_BreakStmt: return "BreakStmt";
        case NT_StructDecl: return "StructDecl";
        case NT_EnumDecl: return "EnumDecl";
        case NT_StructType: return "StructType";
        case NT_EnumMember: return "EnumMember";
        case NT_EnumType: return "EnumType";
        case NT_PostFixExpr: return "PostFixExpr";
        case NT_Initializer: return "Initilizer";
        
    }
    return "<None>";
}



enum SymbolKind{
    SYM_TYPEDEF,
    SYM_VARIABLE,
    SYM_FUNCTION,
    SYM_PARAMETER,
};

struct Symbol{
    SymbolKind kind;
    std::string name;
    Type typeInfo;
    std::string owningScopeName; // e.g. "foo" "main", "" for global scope
};

class SymbolTable{
public:
    SymbolTable(){
        enterScope();
    }
    void enterScope();
    void exitScope();
    void define(const Symbol& sym);
    void define(SymbolKind kind, const std::string& name);
    bool isTypedef(const std::string& name) const;
    bool isDefined(const std::string& name) const;
    SymbolKind getKind(const std::string& name) const;

    std::string kindToStr(SymbolKind kind){
        switch (kind) {
            case SYM_TYPEDEF: return "typedef";
            case SYM_VARIABLE: return "variable";
            case SYM_FUNCTION: return "function";
            case SYM_PARAMETER: return "parameter";
        }
    }

    void showScopes() {
        int scopeLevel = scopes.size();
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            std::cout << "Scope level " << --scopeLevel << ":\n";
            for (const auto& entry : *it) {
                const Symbol& sym = entry.second;
                std::cout   << "  " << sym.name
                            << " (kind: " << kindToStr(sym.kind) << ")"
                            << " type: " << sym.typeInfo.str()
                            << " scope: " << (sym.owningScopeName.empty() ? "<global>" : sym.owningScopeName)
                            << "\n";
            }
        }
    }

    bool hasSymbol(const std::string& name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) return true;
        }
        return false;
    }

    Symbol getSymbol(const std::string& name) const {
        for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
            auto found = it->find(name);
            if (found != it->end()) return found->second;
        }
        // Optional: throw an error or return dummy symbol
        return Symbol{SYM_VARIABLE, name, Type{BT_UNKNOWN}};
    }


    void defineFunction(Function& func){
        functions[func.name] = func;
    }

    Function getFunction(const std::string& name) const {
        auto it = functions.find(name);
        if (it == functions.end()) {
            std::cerr << "SymbolTable Error: function '" << name << "' not defined.\n";
            std::exit(1); // or throw std::runtime_error if you prefer exceptions
        }
        return it->second;
    }

    bool hasFunction(const std::string& name) const {
        return functions.find(name) != functions.end();
    }

    std::unordered_map<std::string, Function> functions;

private:    
    std::vector<std::unordered_map<std::string, Symbol>> scopes;

    
};

class ASTNode{
public:

    ASTNode() = default;
    ASTNode(const NodeType type) : type(type) {}
    ASTNode(const NodeType type, std::string value) :   value(value), type(type) {}

    void addChild(ASTNode* child){
        children.push_back(child);
    }

    NodeType getType() const {return type;}
    const std::string& getValue() {return value;}
    const std::vector<ASTNode*> getChildren() const {return children;}

    ~ASTNode(){
        for(auto child : children){
            delete child;
        }
    }

    std::string str(int indent = 0) const {
        std::string indentation(indent * 4, ' ');
        std::string output = indentation + nodeTypeToString(type);
        if (!value.empty()) {
            output += ": \'" + value + "\'";
        }
        bool debug = false;
        if(!typeInfo.str().empty() && debug) output += " type info: " + typeInfo.str() + " ";
        output += "\n";

        for (const auto& child : children) {
            output += child->str(indent + 1);
        }

        return output;
    }
    


    std::string value;
    NodeType type = NT_None;
    
    std::vector<ASTNode*> children;

    

    Type typeInfo;
};

class Parser{
public:
    explicit Parser(std::vector<Token> tokens) : tokens(tokens), pos(0) {}
    
    ASTNode* parseProgram();         // Entry point

    void printAST(){
        std::cout << root->str() << std::endl;
    }
    
    void error(const std::string& msg) {
        std::cerr << "Parser Error: " << msg << "\n";
        std::cerr << "At token position: " << pos;

        if (pos < tokens.size()) {
            Token& tok = tokens[pos];
            std::cerr << " → Token: " << tok.str();

            // Optional: if your Token has line/col info, print it
            // std::cerr << " at line " << std::to_string(tok.line) << ", col " << std::to_string(tok.column);
        }

        std::cerr << "\nContext (last few tokens):\n";
        size_t start = (pos >= 5) ? pos - 5 : 0;
        for (size_t i = start; i < pos && i < tokens.size(); ++i) {
            std::cerr << "  " << i << ": " << tokens[i].str() << "\n";
        }

        std::cerr << "\nPartial AST:\n";
        printAST();

        exit(1);
    }
    
    SymbolTable symbols;

private:
    std::vector<Token> tokens;
    size_t pos;
    ASTNode* root;

    std::string currentScopeName=""; // "" for global scope, otherwise the function name

    TypeSpecifier getTypeSpec(const std::string& str);
    bool isTypeSpecifierStart();
    bool isConstantExpression(ASTNode* node);

    ASTNode* parseFunctionDecl();   // Parses: int main() { ... }
    ASTNode* parseStructDecl();
    ASTNode* parseEnumDecl();
    ASTNode* parseTypedefDecl();

    ASTNode* parseTypeSpecifier();  // Parses: int, float, etc.
    ASTNode* parseIdentifier();
    ASTNode* parseParameter();      // Parses: <typespec> <ident>

    ASTNode* parseStatement();      // Parses: return x; or x = 1;
    ASTNode* parseCompoundStmt();   // Parses: { ... }
    ASTNode* parseReturnStmt();
    ASTNode* parseVarDecl();
    ASTNode* parseIfStmt();
    ASTNode* parseWhileStmt();
    ASTNode* parseDoStmt();
    ASTNode* parseForStmt();
    ASTNode* parseSwitchStmt();
    ASTNode* parseCaseStmt();
    ASTNode* parseDefaultStmt();
    ASTNode* parseGotoStmt();

    ASTNode* parseExpression();    
    ASTNode* parseSizeofExpr();
    ASTNode* parseTypeCastExpr();
    // ASTNode* parsePostFixExpr();
    ASTNode* parseUnaryExpr();

    int getPrecedence(TokenType type);
    bool isRightAssociative(TokenType type);
    ASTNode* parseBinaryExpr(int minPrec=0); // TODO: make parse expression order of operations correct

    ASTNode* parseAtom();

    ASTNode* parseArgumentList();
    ASTNode* parsePrimary();        // Parses: identifiers, literals


    bool match(TokenType type);        // Checks and consumes a token if matched
    Token peek(size_t off=0);                      // Peeks at current token
    Token advance();                   // Consumes and returns current token
    bool expect(TokenType type);       // Expects a token or throws an error
    bool isAtEnd(size_t off=0);                    // End of input

};