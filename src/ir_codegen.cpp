#include "../include/ir_codegen.hpp"
#include "../include/parser.hpp"
#include "../include/utils.hpp"
#include <cstddef>
#include <iostream>
#include <string>

const std::vector<IRInstruction> &IR_Codegen::get_instructions() const {
    return instructions;
}

// Parameter are generally pushed before the function call, so they are at
// positive offsets from ebp +8 because the first two slots are reserved for the
// return address and ebp itself
std::string IR_Codegen::getPARAMLocation(size_t index, size_t size) {
    return "[ebp + " + std::to_string(8 + index * size) + "]";
}

// Local variables are at negative offsets from ebp, starting from -4 for the
// first local variable
std::string IR_Codegen::getLocalLocation(size_t index, size_t size) {
    return "[ebp - " + std::to_string((index + 1) * size) + "]";
}

void IR_Codegen::error(const std::string &msg) const {
    std::cerr << "IR Codegen Error: " << msg << "\n";
    std::cerr << "Current function: " << current_function.name << "\n";
    std::cerr << "Current temp counter: " << temp_counter << "\n";
    std::cerr << "AST Node: " << (ast_root ? ast_root->str() : "null") << "\n";
    std::cerr << "Symbol Table:\n";
    
    showScopes();
    showFunctions();
    
    exit(1);
}



std::string IR_Codegen::getTempOffset(size_t offset, size_t size) {
    switch (offset) {
    case 0:
        return ((size == 4) ? "eax" : (size == 2) ? "ax" : "al"); // eax is often used for the first temp
    case 1:
        return ((size == 4) ? "ebx" : (size == 2) ? "bx" : "bl"); // eax is often used for the first temp
    case 2:
        return ((size == 4) ? "ecx" : (size == 2) ? "cx" : "cl"); // eax is often used for the first temp
    case 3:
        return ((size == 4) ? "edx" : (size == 2) ? "dx" : "dl"); // eax is often used for the first temp
    }

    // For more than 4 temps, use stack
    return "[ebp - " + std::to_string((offset + 1) * size) + "]"; // For more than 4 temps, use stack
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

Value IR_Codegen::generate_param(const std::string& name, size_t index, Type type=Type()) {
    Value param;
    param.value = name;
    param.type = VT_PARAMETER;
    param.loc = getPARAMLocation(index);

    Symbol sym;
    sym.kind = SYM_PARAMETER;
    sym.name = name;
    sym.typeInfo = type; // Assuming Type is defined elsewhere
    sym.loc = param.loc; // Use the location of the parameter
    sym.scope = current_function; // Set the current function as the scope

    symbols.defineLocalSymbol(sym, current_function);

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

    if(node->type == NodeType::NT_UnaryExpr) {
        return generate_UnaryExpr(node);
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

    if(node->type == NodeType::NT_Assignment) {
        return generate_Assignment(node);
    }

    if(node->type == NodeType::NT_ReturnStmt) {
        return genererate_returnStmt(node);
    }

    if(node->type == NodeType::NT_IfStmt) {
        return generate_IfStmt(node);
    }

    if(node->type == NodeType::NT_WhileStmt) {
        return generate_WhileStmt(node);
    }

    if(node->type == NodeType::NT_CallExpr){
        return generate_CallExpr(node);
    }

    if(node->type == NodeType::NT_TypeSpecifier) {
        // Type specifiers are usually handled during function declarations or variable declarations
        error("TypeSpecifier node encountered outside of declaration context: " + node->str());
        return Value(); // Return an empty Value
    }

    if(node->type == NodeType::NT_PointerType) {
        // Type specifiers are usually handled during function declarations or variable declarations
        error("PointerType node encountered outside of declaration context: " + node->str());
        return Value(); // Return an empty Value
    }

    if(node->type == NodeType::NT_ArraySubscripting) {
        return generate_ArraySubscripting(node);
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

    symbols.local_table.clear();
    current_function = Function(); // Reset current function


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
        Value param = generate_param(paramName, i, paramNode->typeInfo); // Generate parameter with type info

        param.typeInfo = paramNode->typeInfo; // Copy type information from the AST node

        
    }

    Value result = generate_code(bodyNode);



    
    symbols.defineFunction(current_function);

    
    return Value();

}

Value IR_Codegen::genererate_returnStmt(const ASTNode *node) {
    /*
    
    return type from currentFunction
    - ReturnStmt: 'return'
        - Expression: <expression>
    
    */

    Type returnType = current_function.returnType;
    if (!node || node->type != NodeType::NT_ReturnStmt) {
        error("Invalid return statement node");
        return Value(); // Return an empty Value
    }
    if (node->children.empty()) {
        // If there are no children, return void
        if (returnType.base != BT_VOID) {
            error("Return statement expects an expression of type " + returnType.str());
            return Value(); // Return an empty Value
        }
        emit("return");
        return Value("<void>", VT_VOID);
    }
    if (node->children.size() != 1) {
        error("Return statement should have exactly one child expression");
        return Value(); // Return an empty Value
    }
    Value exprValue = generate_code(node->children[0]);
    exprValue.typeInfo = node->children[0]->typeInfo; // Copy type information from the AST node

    if(exprValue.typeInfo != returnType) {
        error("Return type mismatch: expected " + returnType.str() + ", got " + exprValue.typeInfo.str());
        return Value(); // Return an empty Value
    }

    if(exprValue.loc != "eax") 
        emit("assign", Value("eax", VT_REGISTER, "eax"), exprValue); // Assuming return value is in eax
    emit("return");

    return Value();
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


std::string IR_Codegen::operatorToIR(const std::string& op) {
    if (op == "+") return "add";
    if (op == "-") return "sub";
    if (op == "*") return "mul";
    if (op == "/") return "div";
    if (op == "%") return "mod";
    if (op == "&") return "bw_and";
    if (op == "|") return "bw_or";
    if (op == "^") return "bw_xor";
    if (op == "~") return "bw_not";
    if (op == "<<") return "shl";
    if (op == ">>") return "shr";
    if (op == "==") return "eq";
    if (op == "!=") return "ne";
    if (op == "<") return "lt";
    if (op == "<=") return "le";
    if (op == ">") return "gt";
    if (op == ">=") return "ge";
    if (op == "&&") return "lo_and";
    if (op == "||") return "lo_or";

    if(op == "+=") return "add_assign";
    if(op == "-=") return "sub_assign";
    if(op == "*=") return "mul_assign";
    if(op == "/=") return "div_assign";
    if(op == "%=") return "mod_assign";
    if(op == "&=") return "and_assign";
    if(op == "|=") return "or_assign";
    if(op == "^=") return "xor_assign";
    if(op == "<<=") return "shl_assign";
    if(op == ">>=") return "shr_assign";


    error("Unknown operator: " + op);
    
    return "unknown_op"; // Return a placeholder for unknown operators
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

    if(op.back() == '=') {
        emit(operatorToIR(op), left, left, right);
        return left;
    }else{
        Value dst = generate_tmp();
        dst.typeInfo = left.typeInfo; // Assuming left and right have the same type
        emit(operatorToIR(op), dst, left, right);
        return dst;
    }


}

Value IR_Codegen::generate_UnaryExpr(const ASTNode *node) {
    /*
    
    */

    std::string op = node->value;

    if (node->children.empty()) {
        error("Unary expression node has no children");
        return Value(); // Return an empty Value
    }
    Value operand = generate_code(node->children[0]);
    if (operand.type == VT_UNKNOWN) {
        error("Failed to generate code for unary expression operand: " + node->str());
        return Value(); // Return an empty Value
    }

    Value dst = generate_tmp();
    dst.typeInfo = operand.typeInfo; // Copy type information from the operand

    if(op == "&") op = "address_of"; // Special case for address of operator
    else if(op == "*") op = "dereference"; // Special case for dereference operator
    else if(op == "++") op = "increment"; // Special case for increment operator
    else if(op == "--") op = "decrement"; // Special case for decrement operator
    else if(op == "!") op = "logical_not"; // Special case for logical not operator
    else if(op == "~") op = "bitwise_not"; // Special case for bitwise not operator
    else if(op == "-") op = "negate"; // Special case for negate operator
    else if(op == "+") op = "positive"; // Special case for identity operator

    emit(op, dst, operand);



    return dst; 
}

Value IR_Codegen::generate_IfStmt(const ASTNode *node) {
    /*
    If statement:
    - IfStmt: 'if'
        - Condition: <condition expression>
        - ThenStmt: <then statement>
        - ElseStmt?: <else statement>
    */

    ASTNode* condNode = node->children[0];
    ASTNode* thenNode = node->children[1];
    ASTNode* elseNode = (node->children.size() > 2) ? node->children[2] : nullptr;

    if (!condNode || !thenNode) {
        error("If statement node is missing condition or then statement");
        return Value(); // Return an empty Value
    }

    Value condValue = generate_code(condNode);

    std::string thenLabel = "L_then_" + current_function.name + "_" + std::to_string(id_counter++);
    std::string elseLabel = "L_else_" + current_function.name + "_" + std::to_string(id_counter++);
    std::string endLabel = "L_end_" + current_function.name + "_" + std::to_string(id_counter++);

    emit("if", condValue, Value(thenLabel, VT_LABEL));
    emit("label", Value(thenLabel, VT_LABEL));
    Value thenValue = generate_code(thenNode);
    emit("jmp", Value(endLabel, VT_LABEL));

    if( elseNode) {
        emit("label", Value(elseLabel, VT_LABEL));
        Value elseValue = generate_code(elseNode);
    }
    emit("label", Value(endLabel, VT_LABEL));


    

    return Value();
}

Value IR_Codegen::generate_WhileStmt(const ASTNode *node) {
    /*
    While statement:
    - WhileStmt: 'while'
        - Condition: <condition expression>
        - Body: <body statement>
    */

    ASTNode* condNode = node->children[0];
    ASTNode* bodyNode = node->children[1];
    if (!condNode || !bodyNode) {
        error("While statement node is missing condition or body");
        return Value(); // Return an empty Value
    }

    std::string startLabel = "L_while_start_" + current_function.name + "_" + std::to_string(id_counter++);
    emit("label", Value(startLabel, VT_LABEL));

    Value condValue = generate_code(condNode);
    
    std::string bodyLabel = "L_while_body_" + current_function.name + "_" + std::to_string(id_counter);  
    std::string endLabel = "L_while_end_" + current_function.name + "_" + std::to_string(id_counter);

    emit("if", condValue, Value(bodyLabel, VT_LABEL), Value(endLabel, VT_LABEL));
    emit("label", Value(bodyLabel, VT_LABEL));
    Value bodyValue = generate_code(bodyNode);
    emit("jmp", Value(startLabel, VT_LABEL)); // Jump back to the start of the loop
    emit("label", Value(endLabel, VT_LABEL)); // End label for the while loop

    

    return Value(); // Placeholder for while statement generation
}

Value IR_Codegen::generate_CallExpr(const ASTNode *node) {
    /*
    
    Call expression:
    - identifier function name
    - ExpressionList: list of arguments
    
    */

    if (!node || node->type != NodeType::NT_CallExpr) {
        error("Invalid call expression node");
        return Value(); // Return an empty Value
    }
    if (node->children.size() < 2) {
        error("Call expression node has less than 2 children");
        return Value(); // Return an empty Value
    }
    ASTNode* identNode = node->children[0];
    ASTNode* argListNode = node->children[1];
    if (identNode->type != NodeType::NT_Identifier || argListNode->type != NodeType::NT_ExpressionList) {
        error("Invalid call expression node structure");
        return Value(); // Return an empty Value
    }

    Function func = symbols.getFunction(identNode->getValue());

    Type returnType = func.returnType;

    if(func.paramCount != argListNode->children.size()) {
        error("Function call argument count mismatch: expected " + std::to_string(func.paramCount) + ", got " + std::to_string(argListNode->children.size()));
        return Value(); // Return an empty Value
    }

    for(int i=argListNode->children.size()-1;i>=0;i--) {
        emit("push", generate_code(argListNode->children[i]));
    }

    emit("call", Value(identNode->value, VT_FUNCTION, "L_" + identNode->value));

    emit("add_esp", Value(std::to_string(argListNode->children.size() * 4), VT_IMM_INT));

    

    return Value("eax", VT_REGISTER, "eax");
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

            Value oldLit = literal;
            literal = generate_tmp();

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

    
    auto it = symbols.local_table.find(node->value);
    if (it != symbols.local_table.end()) {
        Value localVar;
        localVar.value = node->value;
        localVar.type = VT_LOCAL;
        localVar.typeInfo = it->second.typeInfo;
        localVar.loc = it->second.loc; // Use the location from the symbol table
        return localVar;
    }

    Symbol sym = symbols.getSymbol(node->value, current_function);

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


bool isExpression(const ASTNode *node) {
    return node && (node->type == NodeType::NT_BinaryExpr || 
                    node->type == NodeType::NT_TypeCastExpr ||
                    node->type == NodeType::NT_UnaryExpr ||
                    node->type == NodeType::NT_CallExpr ||
                    node->type == NodeType::NT_TernaryExpr ||
                    node->type == NodeType::NT_TypeCastExpr ||
                    node->type == NodeType::NT_SizeofExpr ||
                    node->type == NodeType::NT_PostFixExpr ||
                    node->type == NodeType::NT_Literal || 
                    node->type == NodeType::NT_Identifier || 0
                );
}

Value IR_Codegen::generate_Assignment(const ASTNode *node) {
    /*
    
    Assignment: <Identifier> = <Expression>
    
    */

    if (!node || node->type != NodeType::NT_Assignment) {
        error("Invalid assignment node");
        return Value(); // Return an empty Value
    }
    if (node->children.size() < 2) {
        error("Assignment node has less than 2 children");
        return Value(); // Return an empty Value
    }
    ASTNode* identNode = node->children[0];
    ASTNode* exprNode = node->children[1];

    if (identNode->type != NodeType::NT_Identifier || !(isExpression(exprNode))) {
        error("Invalid assignment node structure");
        return Value(); // Return an empty Value
    }

    Symbol sym = symbols.getSymbol(identNode->value, current_function);
    Value value = generate_code(exprNode);

    if(value.typeInfo != sym.typeInfo) {
        error("Type mismatch in assignment: " + value.typeInfo.str() + " != " + sym.typeInfo.str());
        return Value(); // Return an empty Value
    }

    emit("assign", Value(identNode->value, VT_LOCAL, sym.loc), value);

    Value dst;
    dst.value = identNode->value;
    dst.type = VT_LOCAL;
    dst.loc = generate_local(sym.name).loc; // Generate local variable location
    dst.typeInfo = sym.typeInfo; // Copy type information from the symbol
    value.loc = dst.loc; // Ensure the value has the correct location

    return value;
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

    if ((typeNode->type != NodeType::NT_TypeSpecifier && typeNode->type != NodeType::NT_PointerType)|| nameNode->type != NodeType::NT_Identifier) {
        error("Invalid declaration node structure");
        return Value(); // Return an empty Value
    }

    Value initValue;

    if(node->children.size() > 2) {
        // Handle initializer if present
        ASTNode* initNode = node->children[2];
        if (initNode->type != NodeType::NT_Initializer) {
            error("Invalid initializer node type: " + initNode->str());
            return Value(); // Return an empty Value
        }

        

        if(initNode->children.at(0)->type == NodeType::NT_ExpressionList) {
            // Handle initializer list
            ASTNode* initList = initNode->children.at(0);
            if (initList->type != NodeType::NT_ExpressionList) {
                error("Invalid initializer list node type: " + initList->str());
                return Value(); // Return an empty Value
            }


            Value initValueSingle;

            // Process each expression in the initializer list
            for (ASTNode* expr : initList->children) {
                if (!isExpression(expr)) {
                    error("Invalid expression in initializer list: " + expr->str());
                    return Value(); // Return an empty Value
                }
                initValueSingle = generate_code(expr); // Generate code for each expression
                emit("make array", Value(nameNode->value, VT_LOCAL, generate_local(nameNode->value).loc), initValueSingle);
            }

            initValue = initValueSingle; // Use the last generated value as the initializer value

        } else {
            // Handle single expression initializer
            // if (!isExpression(initNode->children[0])) {
            //     error("Invalid initializer expression: " + initNode->str());
            //     return Value(); // Return an empty Value
            // }
            initValue = generate_code(initNode->children[0]); // Generate code for the initializer expression
        }

        

        
    }


    Symbol sym;
    sym.kind = SYM_VARIABLE;

    sym.name = nameNode->value;
    sym.scope = current_function; // Set the current function as the scope

    if(typeNode->type == NodeType::NT_PointerType){
        typeNode->typeInfo = typeNode->children[0]->typeInfo; // Get the type info from the pointer type
    }
    sym.typeInfo = typeNode->typeInfo;

    Value varValue = generate_local(sym.name) ;

    sym.loc = varValue.loc; // Generate local variable location

    symbols.defineLocalSymbol(sym, current_function);

    
    // If you need to store the initializer value, set it explicitly (if Value has such a field)
    // varValue.initializer = initValue; // Uncomment if such a field exists
    emit("assign", varValue, initValue);

    return Value(); // Return an empty Value for unsupported node types
}

Value IR_Codegen::generate_ArraySubscripting(const ASTNode *node) {
    /*
    
    Array subscripting:
    - ArraySubscripting: <Identifier> [ <Expression> ]
    
    */   

    ASTNode* identNode = node->children[0];
    ASTNode* indexNode = node->children[1];

    if (!identNode || !indexNode) {
        error("Array subscripting node is missing identifier or index expression");
        return Value(); // Return an empty Value
    }
    if (identNode->type != NodeType::NT_Identifier || !isExpression(indexNode)) {
        error("Invalid array subscripting node structure");
        return Value(); // Return an empty Value
    }

    /*
    
    array subscripting is just syntactic sugar for pointer arithmetic

    this way:
        arr[5] is equivalent to *(arr + 5) and
        arr[5] = 10 is equivalent to *(arr + 5) = 10 and
        arr[i] is equivalent to *(arr + i)

    */

    Value identValue = generate_code(identNode);
    Value indexValue = generate_code(indexNode);
    
    Value tmp = generate_tmp();
    tmp.typeInfo = identValue.typeInfo; // Assuming the identifier is an array, copy its type info
    emit("add", tmp, identValue, indexValue); // tmp = identValue + indexValue
    emit("dereference", tmp, tmp); // tmp = *tmp (dereference the pointer)

    return tmp;
}
