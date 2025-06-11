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

    bool operator==(const Type& other) const {
        return base == other.base && pointerLevel == other.pointerLevel; // Add more fields if needed
    }
    bool operator!=(const Type& other) const {
        return !(*this == other);
    }

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

enum SymbolKind {
  SYM_TYPEDEF,
  SYM_VARIABLE,
  SYM_FUNCTION,
  SYM_PARAMETER,
  SYM_ENUM_MEMBER,
  SYM_STRUCT_MEMBER,
};

inline std::string kindToStr(SymbolKind kind){
    switch (kind) {
        case SYM_TYPEDEF: return "typedef";
        case SYM_VARIABLE: return "variable";
        case SYM_FUNCTION: return "function";
        case SYM_PARAMETER: return "parameter";
        case SYM_ENUM_MEMBER: return "enum member";
        case SYM_STRUCT_MEMBER: return "struct member";
        default: return "unknown";
    }
}

struct Value;

struct Symbol{
    SymbolKind kind;
    std::string name;
    Type typeInfo;

    Function scope;

    std::string loc; // e.g. "eax", "[ebp - 4]", etc.

    std::string str() const {
        return   "  " + name
               + " (kind: " + kindToStr(kind) + ")"
               + " type: " + typeInfo.str()
               + " scope: " + (scope.name.empty() ? "<global>" : scope.name)
               + "\n";
    }
};

class Parser;

// class SymbolTable{
// public:
//     SymbolTable() {
//         enterScope();
//     }


//     void enterScope();
//     void exitScope();
//     void define(const Symbol& sym);
//     void define(SymbolKind kind, const std::string& name);
//     bool isTypedef(const std::string& name) const;
//     bool isDefined(const std::string& name) const;
//     SymbolKind getKind(const std::string& name) const;

    

//     void showScopes() {
//         int scopeLevel = scopes.size();
//         for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
//             std::cout << "Scope level " << --scopeLevel << ":\n";
//             for (const auto& entry : *it) {
//                 const Symbol& sym = entry.second;
//                 std::cout << sym.str();
//             }
//         }
//     }

//     void showFunctions() {
//         std::cout << "Defined functions:\n";
//         for (auto it = functions.begin(); it != functions.end(); ++it) {
//             const std::string& name = it->first;
//             const Function& func = it->second;
//             std::cout << "  " << name << " (return type: " << func.returnType.str() 
//                       << ", param count: " << func.paramCount << ")\n";
//         }
//     }

//     bool hasSymbol(const std::string& name) const {
//         for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
//             auto found = it->find(name);
//             if (found != it->end()) return true;
//         }
//         return false;
//     }

//     Symbol getSymbol(const std::string& name) const {
//         for (auto it = scopes.rbegin(); it != scopes.rend(); ++it) {
//             auto found = it->find(name);
//             if (found != it->end()) return found->second;
//         }
//         // std::cerr << "SymbolTable Error: symbol '" << name << "' not found.\n";
//         // std::exit(1); // or throw an exception
//         return Symbol{SYM_VARIABLE, name, Type(), ""}; // return an empty symbol or handle it as you prefer
//     }


//     void defineFunction(Function& func){
//         functions[func.name] = func;
//     }

//     Function getFunction(const std::string& name) const {
//         auto it = functions.find(name);
//         if (it == functions.end()) {
//             std::cerr << "SymbolTable Error: function '" << name << "' not defined.\n";
//             std::exit(1); // or throw std::runtime_error if you prefer exceptions
//         }
//         return it->second;
//     }

//     bool hasFunction(const std::string& name) const {
//         return functions.find(name) != functions.end();
//     }

//     std::unordered_map<std::string, Function> functions;

//     std::vector<std::unordered_map<std::string, Symbol>> scopes;


    
// };

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

class SymbolTable{
public:
    std::unordered_map<std::string, Symbol> local_table;
    std::unordered_map<std::string, Function> function_table;
    std::unordered_map<std::string, Symbol> global_table;

    Symbol getLocalSymbol(const std::string &name, Function& currentFunction) const {
      auto it = local_table.find(name);
      if (it != local_table.end())
        return it->second;
      std::cerr << ("Local symbol '" + name +
            "' not found in scope: " + currentFunction.name);
      return Symbol{SYM_VARIABLE, name, Type(),
                    {""}}; // return empty symbol if not found
    }

    Symbol getGlobalSymbol(const std::string &name) const {
      auto it = global_table.find(name);
      if (it != global_table.end())
        return it->second;
      std::cerr << ("Global symbol '" + name + "' not found.");
      return Symbol{SYM_VARIABLE, name, Type(),
                    {""}}; // return empty symbol if not found
    }

    Symbol getSymbol(const std::string &name, Function& currentFunction) const {
      if (local_table.count(name))
        return local_table.at(name);
      if (global_table.count(name))
        return global_table.at(name);
      std::cerr << ("Symbol '" + name +
            "' not found in any scope: " + currentFunction.name + "\n");
      return Symbol{SYM_VARIABLE, name, Type(),
                    {""}}; // return empty symbol if not found
    }

    Function getFunction(const std::string &name) const {
      auto it = function_table.find(name);
      if (it != function_table.end())
        return it->second;
      return Function{name, Type(), 0}; // return empty function if not found
    }

    void defineLocalSymbol(const Symbol &sym, Function &currentFunction) {
    //   if (local_table.find(sym.name) != local_table.end()) {
    //     std::cerr << "Local symbol '" + sym.name +
    //           "' is already defined in this scope: " + currentFunction.name;
    //   }
      local_table[sym.name] = sym;
    }

    void defineGlobalSymbol(const Symbol &sym) {
    //   if (global_table.find(sym.name) != global_table.end()) {
    //     std::cerr << "Global symbol '" + sym.name + "' is already defined.";
    //   }

      global_table[sym.name] = sym;
    }

    void defineFunction(const Function &func) {
    //   if (function_table.find(func.name) != function_table.end()) {
    //     std::cerr << "Function '" + func.name + "' is already defined.";
    //   }

      function_table[func.name] = func;
    }

    bool isDefined(const std::string &name) const {
      return local_table.count(name) || global_table.count(name) ||
             function_table.count(name);
    }

    bool isTypedef(const std::string &name) const {
        return (local_table.count(name) &&
                 local_table.at(name).kind == SYM_TYPEDEF) ||
             (global_table.count(name) &&
                 global_table.at(name).kind == SYM_TYPEDEF);
    }

    bool isFunction(const std::string &name) const {
        return function_table.count(name) > 0;
    }

    bool isVariable(const std::string &name) const {
        return  (local_table.count(name) &&
                (local_table.at(name).kind == SYM_VARIABLE)) ||
                (global_table.count(name) &&
                (global_table.at(name).kind == SYM_VARIABLE));
    }

    void showSymbols() const {
        std::cout << "Local Symbols:\n";
        for (const auto &entry : local_table) {
            std::cout << entry.second.str();
        }
        std::cout << "\nGlobal Symbols:\n";
        for (const auto &entry : global_table) {
            std::cout << entry.second.str();
        }
    }

    void showFunctions() const {
        std::cout << "Defined Functions:\n";
        for (const auto &entry : function_table) {
            const Function &func = entry.second;
            std::cout << "  " << func.name
                      << " (return type: " << func.returnType.str()
                      << ", param count: " << func.paramCount << ")\n";
        }
    }
};

class Parser{
public:
    explicit Parser(std::vector<Token> tokens) : tokens(tokens),  pos(0) {}
    
    ASTNode* parseProgram();         // Entry point

    void printAST() const {
        std::cout << root->str() << std::endl;
    }
    
    void error(const std::string& msg) const {
        std::cerr << "Parser Error: " << msg << "\n";
        std::cerr << "At token position: " << pos;

        if (pos < tokens.size()) {
            Token tok = tokens[pos];
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

        std::cerr << "\nSymbol Table:\n";
        std::cerr << "Local Symbols:\n";
        for (auto it = symbols.local_table.begin();
             it != symbols.local_table.end(); ++it) {
          std::cerr << it->second.str();
        }
        std::cerr << "\nGlobal Symbols:\n";
        for (auto it = symbols.global_table.begin();
             it != symbols.global_table.end(); ++it) {
          std::cerr << it->second.str();
        }
        std::cerr << "\nDefined Functions:\n";
        for (auto it = symbols.function_table.begin();
             it != symbols.function_table.end(); ++it) {
          std::cerr << "  " << it->second.name
                    << " (return type: " << it->second.returnType.str()
                    << ", param count: " << it->second.paramCount << ")\n";
        }

        exit(1);
    }

    

    

    
    SymbolTable symbols; // Symbol table for current scope

  private:
    std::vector<Token> tokens;
    size_t pos;
    ASTNode* root;


    void defineSymbol(const Symbol& sym){
        if(currentFunction.name == "<global>"){
            symbols.defineGlobalSymbol(sym);
        } else {
            symbols.defineLocalSymbol(sym, currentFunction);
        }
    }

    Function currentFunction; // Current function being parsed

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
    ASTNode* parseTernaryExpr();
    

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