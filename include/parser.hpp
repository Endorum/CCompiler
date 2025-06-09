#pragma once

#include <cstddef>
#include <ostream>
#include <string>
#include <unordered_map>
#include <vector>
#include "utils.hpp"
#include "defs.hpp"
#include "lexer.hpp"



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



enum SymbolKind{
    SYM_TYPEDEF,
    SYM_VARIABLE,
    SYM_FUNCTION,
    SYM_PARAMETER,
    SYM_ENUM_MEMBER,
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
            case SYM_ENUM_MEMBER: return "enum member";
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
        bool debug = true;
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
            std::cerr << " at line " << std::to_string(tok.line) << ", col " << std::to_string(tok.column);
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