#include "../include/ir_codegen.hpp"
#include "../include/parser.hpp"
#include "../include/utils.hpp"
#include <iostream>

const std::vector<IRInstruction> &IR_Codegen::get_instructions() const {
    return instructions;
}

// Parameter are generally pushed before the function call, so they are at
// positive offsets from ebp +8 because the first two slots are reserved for the
// return address and ebp itself
std::string IR_Codegen::getPARAMLocation(size_t index) {
    return "[ebp + " + std::to_string(8 + index * 4) + "]";
}

// Local variables are at negative offsets from ebp, starting from -4 for the
// first local variable
std::string IR_Codegen::getLocalLocation(size_t index) {
    return "[ebp - " + std::to_string((index + 1) * 4) + "]";
}

void IR_Codegen::error(const std::string &msg) const {
    std::cerr << "IR Codegen Error: " << msg << "\n";
    std::cerr << "Current function: " << current_function.name << "\n";
    std::cerr << "Current temp counter: " << temp_counter << "\n";
    std::cerr << "AST Node: " << (ast_root ? ast_root->str() : "null") << "\n";
    std::cerr << "Symbol Table:\n";
    symbolTable.showScopes();
    symbolTable.showFunctions();
    exit(1);
}



std::string IR_Codegen::getTempOffset(size_t offset) {
    switch (offset) {
    case 0:
        return "eax"; // eax is often used for the first temp
    case 1:
        return "ebx"; // ebx for second temp
    case 2:
        return "ecx"; // ecx for third temp
    case 3:
        return "edx"; // edx for fourth temp
    }

    // For more than 4 temps, use stack
    return "[ebp - " + std::to_string((offset + 1) * 4) + "]"; // For more than 4 temps, use stack
}

Value IR_Codegen::generate_tmp() {
    Value tmp;
    tmp.value = "t" + std::to_string(temp_counter++);
    tmp.type = VT_TEMP;
    tmp.loc = getTempOffset(temp_counter);
    return tmp;
}

Value IR_Codegen::generate_local(const std::string& name) {
    Value local;
    local.value = name;
    local.type = VT_LOCAL;
    local.loc = getLocalLocation(local_counter++);
    return local;
}

Value IR_Codegen::generate_param(const std::string& name, size_t index) {
    Value param;
    param.value = name;
    param.type = VT_PARAMETER;
    param.loc = getPARAMLocation(index);
    return param;
}

Value IR_Codegen::generate_code(ASTNode* node){
    if (!node) {
        error("AST node is null");
        return Value(); // Return an empty Value
    }

    if(node->type == NodeType::NT_FunctionDecl){
        return generate_FunctionDecl(node);
    }

    if(node->type == NodeType::NT_CompoundStmt){
        return generate_CompoundStmt(node);
    }

    if(node->type == NodeType::NT_ExpressionStmt) {
        // Handle expression statements
        if (node->children.empty()) {
            error("Expression statement has no children");
            return Value(); // Return an empty Value
        }
        Value exprValue = generate_code(node->children[0]);
        if (exprValue.type == VT_UNKNOWN) {
            error("Failed to generate code for expression statement: " + node->str());
        }
        return exprValue;
    }

    if(node->type == NodeType::NT_BinaryExpr){
        if (node->children.size() < 2) {
            error("Binary expression node has less than 2 children");
            return Value(); // Return an empty Value
        }

        return generate_BinaryExpr(node);
    }

    if(node->type == NodeType::NT_Literal){
        return generate_Literal(node);
    }

    if(node->type == NodeType::NT_Identifier) {
        return generate_Identifier(node);
    }

    if(node->type == NodeType::NT_Declaration){
        return generate_Declaration(node);
    }

    return Value(); // Return an empty Value for unsupported node types
}

void IR_Codegen::generate() {
    if (!ast_root) {
        error("AST root is null");
        return;
    }
    if (ast_root->type != NodeType::NT_Program) {
        error("Expected AST root to be a Program node");
        return;
    }

    for(int i=0;i<ast_root->children.size();i++) {
        generate_code(ast_root->children[i]);
    }
}
Value IR_Codegen::generate_FunctionDecl(const ASTNode* func) {
    /*
    - TypeSpecifier: <TypeSpecifier>
    - Identifier: <name>
    - ParameterList:
        - Parameter
            - TypeSpecifier: <TypeSpecifier>
            - Identifier: <name>
    - CompoundStatement: <body>
    */

    ASTNode* typeSpecNode = func->children[0];
    ASTNode* identNode = func->children[1];
    ASTNode* paramListNode = func->children[2];
    ASTNode* bodyNode = func->children[3];

    if(!typeSpecNode || !identNode || !paramListNode || !bodyNode) {
        error("Function declaration node is missing required children");
        return Value(); // Return an empty Value
    }

    if (typeSpecNode->type != NodeType::NT_TypeSpecifier || 
        identNode->type != NodeType::NT_Identifier ||
        paramListNode->type != NodeType::NT_ParamList ||
        bodyNode->type != NodeType::NT_CompoundStmt) {
        error("Invalid function declaration node structure");
        return Value(); // Return an empty Value
    }

    // Reset counters for each function
    temp_counter = 0;
    local_counter = 0;
    variable_table.clear();


    current_function.name = identNode->value;
    current_function.returnType = typeSpecNode->typeInfo;
    current_function.paramCount = paramListNode->children.size();

    
    IRInstruction label;
    label.op = "label";
    label.result.value = current_function.name;
    label.result.type = VT_LABEL;
    label.result.loc = "L_" + current_function.name;

    emit(label);

    for(int i=0;i<paramListNode->children.size();i++) {
        ASTNode* paramNode = paramListNode->children[i];

        const std::string& paramName = paramNode->children[1]->value; // Assuming the second child is the identifier
        Value param = generate_param(paramName, i);

        param.typeInfo = paramNode->typeInfo; // Copy type information from the AST node

        variable_table[paramName] = param;
    }

    Value result = generate_code(bodyNode);

    Value dst;
    dst.value = current_function.name;
    dst.type = VT_TEMP;
    dst.loc = "eax"; // Assuming the return value is in eax

    functions[current_function.name] = current_function;

    if (result.typeInfo.base != BT_VOID) {
        // If the function has a return value, we need to handle it
        emit("return", result);
    } else {
        // If the function is void, we just emit a return instruction
        emit("return");
    }  

    

    return dst;

}

Value IR_Codegen::generate_CompoundStmt(const ASTNode *node) {
    /*
    
    - list of statements and declarations
    
    */

    if (!node || node->type != NodeType::NT_CompoundStmt) {
        error("Invalid compound statement node");
        return Value(); // Return an empty Value
    }

    Value result;
    for(int i=0;i<node->children.size();i++) {
        ASTNode* child = node->children[i];
        
        result = generate_code(child);
        if (result.type == VT_UNKNOWN) {
            result = Value("<empty>", VT_VOID);
        }
    }

    return result;
}


std::string operatorToIR(const std::string& op) {
    if (op == "+") return "add";
    if (op == "-") return "sub";
    if (op == "*") return "mul";
    if (op == "/") return "div";
    if (op == "%") return "mod";
    if (op == "==") return "eq";
    if (op == "!=") return "ne";
    if (op == "<") return "lt";
    if (op == "<=") return "le";
    if (op == ">") return "gt";
    if (op == ">=") return "ge";
    if (op == "&&") return "and";
    if (op == "||") return "or";
    
    return "<unknown op>"; // Unknown operator
}

Value IR_Codegen::generate_BinaryExpr(const ASTNode *node) {
    /*
    
    - BinaryExpr: '<operator>'
        - left: <left operand>
        - right: <right operand>
    */

    std::string op = node->value;
    if (node->children.size() < 2) {
        error("Binary expression node has less than 2 children");
        return Value(); // Return an empty Value
    }

    Value left = generate_code(node->children[0]);
    Value right = generate_code(node->children[1]);

    Value dst = generate_tmp();
    dst.typeInfo = left.typeInfo; // Assuming left and right have the same type
    
    // Allow valid C mixed-type binary expressions (e.g., int + string), but check for other mismatches.
    // For pointer arithmetic (int + pointer, pointer + int), allow as in C.
    // For other mismatches (e.g., float + string, struct + int), emit an error.

    bool valid = true;
    if (left.typeInfo.base == BT_FLOAT || right.typeInfo.base == BT_FLOAT) {
        // Disallow float + pointer or float + string, etc.
        if (left.typeInfo.pointerLevel > 0 || right.typeInfo.pointerLevel > 0) {
            valid = false;
        }
    } else if (left.typeInfo.base == BT_STRUCT || right.typeInfo.base == BT_STRUCT) {
        // Disallow struct in binary expressions
        valid = false;
    } else if (left.typeInfo.pointerLevel > 0 && right.typeInfo.pointerLevel > 0) {
        // Disallow pointer + pointer (except for subtraction)
        if (op != "-") {
            valid = false;
        }
    }

    if (!valid) {
        error("Invalid binary expression: " + left.typeInfo.str() + " " + op + " " + right.typeInfo.str());
        return Value();
    }

    emit(operatorToIR(op), dst, left, right);

    return dst;
}

Value IR_Codegen::generate_Literal(const ASTNode *node) {
    Value literal;
    literal.value = node->value;
    
    if(node->typeInfo.base == BT_INT){
        literal.type = VT_IMM_INT;
    } else if(node->typeInfo.base == BT_FLOAT){
        literal.type = VT_IMM_FLOAT;
    } else if(node->typeInfo.base == BT_CHAR){
        if(node->typeInfo.pointerLevel > 0) {
            // String literal
            literal.type = VT_IMM_STRING;
            literal.loc = "\"" + node->value + "\""; // Use quotes for string literals
            emit("load_string", literal, Value(node->value, VT_IMM_STRING));
        } else {
            // Character literal
            literal.type = VT_IMM_INT; // Store char as int
            literal.loc = "'" + node->value + "'";
        }
    } else {
        error("Unsupported literal type: " + node->typeInfo.str());
        literal.type = VT_UNKNOWN;
    }

    literal.typeInfo = node->typeInfo; // Copy type information from the AST node

    return literal;
}

Value IR_Codegen::generate_Identifier(const ASTNode *node) {
    /*
    
    First: make a lookup find the Symbol 
    it has a kind with which we can determine how to handle it
    
    */

    auto it = variable_table.find(node->value);
    if (it != variable_table.end()) {
        return it->second;
    }

    Symbol sym = symbolTable.getSymbol(node->value);

    Value ident;
    ident.value = node->value;
    ident.typeInfo = sym.typeInfo;

    if (sym.kind == SYM_FUNCTION) {
        ident.type = VT_FUNCTION;
        ident.loc = "L_" + sym.name;
    } else if (sym.kind == SYM_VARIABLE) {
        ident.type = VT_LOCAL;
        ident.loc = generate_local(sym.name).loc;
    } else {
        error("Unsupported or unhandled symbol kind for identifier: " + kindToStr(sym.kind));
    }

    emit("mov", ident, ident);
    return ident;
}

Value IR_Codegen::generate_Declaration(const ASTNode *node) {
    /*
    
    - Declaration: <TypeSpecifier> <Identifier> = <Initializer>?
    - initializer may be an expression or a list of expressions
    */

    if (!node || node->type != NodeType::NT_Declaration) {
        error("Invalid declaration node");
        return Value(); // Return an empty Value
    }

    if (node->children.size() < 2) {
        error("Declaration node has less than 2 children");
        return Value(); // Return an empty Value
    }

    ASTNode* typeNode = node->children[0];
    ASTNode* nameNode = node->children[1];

    if (typeNode->type != NodeType::NT_TypeSpecifier || nameNode->type != NodeType::NT_Identifier) {
        error("Invalid declaration node structure");
        return Value(); // Return an empty Value
    }

    Symbol sym;
    sym.kind = SYM_VARIABLE;

    sym.name = nameNode->value;
    sym.typeInfo = typeNode->typeInfo;
    variable_table[sym.name] = generate_local(sym.name);



    return Value(); // Return an empty Value for unsupported node types
}
