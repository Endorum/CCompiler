#pragma once

#include <cstddef>
#include <ostream>
#include <string>
#include <vector>
// #include "utils.hpp"
// #include "defs.hpp"
#include "lexer.hpp"

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
};

inline std::string nodeTypeToString(NodeType type){
    switch (type) {
    
    
        case NT_None: return "None";
        case NT_Program: return "Program";
        case NT_FunctionDecl: return "FunctionDecl";
        case NT_FunctionDef: return "FunctionDef";
        case NT_ParamList: return "ParamList";
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
        

    }
}


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
        if(!this) return "none";
        std::string indentation(indent * 4, ' ');
        std::string output = indentation + nodeTypeToString(type);
        if (!value.empty()) {
            output += ": " + value;
        }
        output += "\n";

        for (const auto& child : children) {
            output += child->str(indent + 1);
        }

        return output;
    }
    


    std::string value;
    NodeType type = NT_None;
    
    std::vector<ASTNode*> children;
};

class Parser{
public:
    explicit Parser(std::vector<Token> tokens) : tokens(tokens), pos(0) {}
    
    ASTNode* parseProgram();         // Entry point

    
    void error(const std::string& msg){
        std::cout << "From parser: " << std::endl;
        std::cerr << msg << std::endl;
        std::cout << "position: " << std::to_string(pos) << std::endl;
        exit(1);
    }
    

private:
    std::vector<Token> tokens;
    size_t pos;
    std::vector<ASTNode*> AST;

    TypeSpecifier getTypeSpec(const std::string& str);

    ASTNode* parseFunctionDecl();   // Parses: int main() { ... }
    ASTNode* parseTypeSpecifier();  // Parses: int, float, etc.
    ASTNode* parseIdentifier();
    ASTNode* parseParameter();      // Parses: <typespec> <ident>

    ASTNode* parseCompoundStmt();   // Parses: { ... }
    ASTNode* parseStatement();      // Parses: return x; or x = 1;
    ASTNode* parseExpression();     // Parses: a + b * c
    ASTNode* parsePrimary();        // Parses: identifiers, literals


    bool match(TokenType type);        // Checks and consumes a token if matched
    Token peek();                      // Peeks at current token
    Token advance();                   // Consumes and returns current token
    bool expect(TokenType type);       // Expects a token or throws an error
    bool isAtEnd();                    // End of input

};