// #pragma once

// #include "../old/parserOLD.hpp"
// #include <cstddef>
// #include <memory>
// #include <unordered_map>
// #include <vector>
// #include <string>

// // Forward declare AST node
// class ASTNode;

// // Forward declare dataType or include its header if available

// typedef enum {
//     VT_UNKNOWN,
//     VT_PARAMETER,
//     VT_LOCAL,
//     VT_TEMP,

//     VT_IMM_INT,    // e.g. 42
//     VT_IMM_FLOAT,  // e.g. 3.14
//     VT_IMM_STRING, // e.g. "hello"

//     VT_REGISTER, // e.g. rax, rbx
//     VT_GLOBAL,   // e.g. global variable

//     VT_LABEL,
//     VT_FUNCTION, // e.g. function name

//     VT_VOID, // special case for void type
// } VarType;

// inline std::string varTypeToString(VarType type) {
//     switch (type) {
//         case VT_UNKNOWN: return "unknown";
//         case VT_PARAMETER: return "parameter";
//         case VT_LOCAL: return "local";
//         case VT_TEMP: return "temporary";
//         case VT_IMM_INT: return "immediate numeric";
//         case VT_IMM_FLOAT: return "immediate float";
//         case VT_IMM_STRING: return "immediate string";
//         case VT_REGISTER: return "register";
//         case VT_GLOBAL: return "global variable";
//         case VT_LABEL: return "label";
//         case VT_FUNCTION: return "function";
//         case VT_VOID: return "void";
//     }
//     return "invalid type";
// }

// struct Value{
//     std::string value;
//     VarType type = VT_UNKNOWN;
    
//     std::string loc = ""; // e.g. "eax" "[ebp - 4]" ...

//     Type typeInfo; // Type information, e.g. int, float, etc.

//     Value() = default;

//     Value(const std::string& n, VarType t = VT_UNKNOWN, const std::string& loc = "")
//         : value(n), type(t), loc(loc) {}

//     std::string str() const {
//         // if( value.empty()) {
//         //     return "<empty value>";
//         // }
//         // if(1){ // debug mode
//         //     return value + " (" + varTypeToString(type) + ")" + (loc.empty() ? "" : " at " + loc);
//         // }else{
//         //   return value + (loc.empty() ? "" : " at " + loc);
//         // }

//         bool debug = false;

//         std::string res;
//         if (value.empty()) return "";

//         res += "'" + value + "'";
//         if(debug) res += "(" + varTypeToString(type) + ")";
//         if(!loc.empty()) res += "@" + loc;

//         return res;
//     }
// };

// // Simple IR instruction representation
// struct IRInstruction {
//     Value result;
//     std::string op;
//     Value src1;
//     Value src2;

//     std::string str() const {
//         std::string res;

//         res += op + ": ";
//         res += "dst: " + result.str();
//         if (!src1.value.empty()) {
//             res += ", src1: " + src1.str();
//         }
//         if (!src2.value.empty()) {
//             res += ", src2:" + src2.str();
//         }

//         return res;
//     }


// };

// // IR Codegen class
// class IR_Codegen {
// public:
//     // Takes ownership or reference to AST root
//   explicit IR_Codegen(ASTNode *ast_root, SymbolTable& table)
//       : symbols(table), ast_root(ast_root)  {

    
//     current_function.name = "<global>"; // Default function name
//   };

//     // Generate IR code from AST
    
//     SymbolTable& symbols; // Symbol table for current scope
    

// private :
//     // external
//     ASTNode *ast_root;

//     void showScopes() const {
//         std::cout << "Local Symbols:\n";
//         for (const auto &entry : symbols.local_table) {
//             std::cout << entry.second.str();
//         }
//         std::cout << "\nGlobal Symbols:\n";
//         for (const auto &entry : symbols.global_table) {
//             std::cout << entry.second.str();
//         }
//     }

//     void showFunctions() const {
//         std::cout << "Defined Functions:\n";
//         for (const auto &entry : symbols.function_table) {
//             const Function &func = entry.second;
//             std::cout << "  " << func.name
//                     << " (return type: " << func.returnType.str()
//                     << ", param count: " << func.paramCount << ")\n";
//         }
//     }

    

//     // internal
//     size_t temp_counter = 0; // gets reset everytime a new function is generated
//     size_t local_counter = 0; // gets reset everytime a new function is generated

//     size_t id_counter = 0; // used for if while etc labels;

//     Function current_function; // Current function being processed

    
//     // output
//     std::vector<IRInstruction> instructions;

//     void error(const std::string& msg) const;


//     std::string gen_from_ast(const ASTNode* node);

//     // Temporary variable counter
//     int temp_counter_ = 0;

//     // Generate a new temporary variable name
//     std::string new_temp();

//     void emit(const IRInstruction &instruction) {
//       instructions.push_back(instruction);
//     }

//     void emit(const std::string &op, const Value &result=Value(),
//               const Value &src1 = Value(), const Value &src2 = Value()) {
//       IRInstruction instruction;
//       instruction.op = op;
//       instruction.result = result;
//       instruction.src1 = src1;
//       instruction.src2 = src2;
//       emit(instruction);
//     }

//     std::string operatorToIR(const std::string& op);

//     std::string getTempOffset(size_t offset, size_t size=4) ;

//     Value generate_tmp();

//     Value generate_local(const std::string &name);
//     Value generate_param(const std::string& name, size_t index, Type type);
//     Value generate_global(const std::string &name);

//     // Get generated IR instructions
    
//     std::string getPARAMLocation(size_t index, size_t size=4);
//     std::string getLocalLocation(size_t index, size_t size=4);
    
//     Value generate_FunctionDecl(const ASTNode* func);
//     Value genererate_returnStmt(const ASTNode *node);
//     Value generate_CompoundStmt(const ASTNode *node);
//     Value generate_BinaryExpr(const ASTNode *node);
//     Value generate_UnaryExpr(const ASTNode *node);
//     Value generate_IfStmt(const ASTNode *node);
//     Value generate_WhileStmt(const ASTNode *node);
//     Value generate_CallExpr(const ASTNode *node);
//     Value generate_Literal(const ASTNode *node);
//     Value generate_Identifier(const ASTNode *node);
//     Value generate_Assignment(const ASTNode *node);
//     Value generate_Declaration(const ASTNode *node);
//     Value generate_ArraySubscripting(const ASTNode *node);
    

//     Value generate_code(ASTNode* node);
    
    
// public:
//     const std::vector<IRInstruction> &get_instructions() const;

//     void generate();
// };