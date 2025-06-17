

#include "defs.hpp"
#include "lexer.hpp"
#include "preproc.hpp"
#include <cstddef>
#include <vector>

class Type{
private:
    BaseType baseType;
    Type* child = nullptr; // linked list for multiple pointers

public:
    Type(BaseType baseType = BT_NONE)
        : baseType(baseType), child(nullptr) {}

    // Copy constructor (deep copy)
    Type(const Type& other) : baseType(other.baseType), child(nullptr) {
        if (other.child) {
            child = new Type(*other.child);
        }
    }

    // Assignment operator (deep copy)
    Type& operator=(const Type& other){
        if(this == &other) return *this;

        delete child;
        baseType = other.baseType;
        child = other.child ? new Type(*other.child) : nullptr;
        return *this;
    }

    ~Type(){
        delete child;  
    }
    
    void addPointerLevel(size_t amount){
        for(int i=0;i<amount;i++) addPointerLevel();
    }

    // Add a pointer level to this type (e.g., int -> int*, int* -> int**)
    void addPointerLevel() {
        Type* newChild = new Type(*this);
        baseType = BT_POINTER;
        child = newChild;
    }

    size_t getByteSize(){
        switch (baseType) {
            case BT_VOID:       return 0;
            case BT_CHAR:       return 1;
            case BT_SHORT:      return 2;
            case BT_INT:        return 4;
            case BT_LONG:       return 8;
            case BT_FLOAT:      return 4;
            case BT_DOUBLE:     return 8;
            case BT_SIGNED:     return 4;
            case BT_UNSIGNED:   return 4;
            case BT_POINTER:    return 8;
            default: return -1;
        }
    }

    BaseType getBaseType() {
        if(baseType != BT_POINTER) return baseType;
        Type* type = this;
        while(type->baseType == BT_POINTER && type->child){
            type = type->child;
        }
        return type->baseType;
    }

    std::string str() {
        std::string out;
        std::string stars = "";

        Type* type = this;

        while(type && type->baseType == BT_POINTER){
            stars += "*";
            type = type->child;
        }

        switch (type ? type->baseType : BT_NONE) {
            case BT_VOID: out += "void"; break;
            case BT_CHAR: out += "char"; break;
            case BT_SHORT: out += "short"; break;
            case BT_INT: out += "int"; break;
            case BT_LONG: out += "long"; break;
            case BT_FLOAT: out += "float"; break;
            case BT_DOUBLE: out += "double"; break;
            case BT_SIGNED: out += "signed"; break;
            case BT_UNSIGNED: out += "unsigned"; break;
            default: out += "unknown"; break;
        }   

        out += stars;

        return out;
    }

};

class Node{

private:
    NodeType type;
    std::string value;
    Type dataType;

    std::vector<Node*> children;

public:
    Node() = default;
    Node(const NodeType type) : type(type) {}
    Node(const NodeType type, std::string value) :   type(type), value(value) {}

    ~Node(){
        for(auto child : children){
            delete child;
        }    
    }

    void addChild(Node* node){
        children.push_back(node);
    }

    std::string str() {
        std::string out = "";

        out += "Node of type: " + NodeType_str(type);

        if(!value.empty()) out += " value: '" + value + "' ";
        if(dataType.getBaseType() != BT_NONE) out += " data type: " + dataType.str();

        return out;
    }

    
};

class Parser{

private:
    std::vector<Token> tokens;
    Node* root = nullptr;

    size_t pos=0;

public:
    Parser(std::vector<Token> tokens) : tokens(tokens) {} 

    void parse();

private:

    // helper functions
    bool isAtEnd(size_t off=0);
    Token peek(size_t off=0);           // peeks at current token with an optinal offset
    Token advance();                    // consumes and returns the current token
    bool expect(TokenType type);        // Expects a token or throws an error pos++
    bool match(TokenType type);         // (peek().type == type) ? true : false ; pos++
    bool match_kw(KeyWordType type);    // ...

    void error(const std::string& msg); // throws an error with message and some context, and exit(1)'s

    // top level
    Node* parse_top_level();
    Node* parse_top_level_elem();
    
    // functions
    Node* parse_fun_definition();
    Node* parse_fun_signature();
    
    // statments: done
    Node* parse_labeled_stmt();
    Node* parse_statement_list();
    Node* parse_statement();
    Node* parse_compound_stmt();
    Node* parse_goto_stmt();
    Node* parse_null_stmt();
    Node* parse_return_stmt();
    Node* parse_continue_stmt();
    Node* parse_break_stmt();
    Node* parse_if_stmt();
    Node* parse_while_stmt();
    Node* parse_do_while_stmt();
    Node* parse_for_stmt();
    Node* parse_decl_stmt();

    // assign statement
    Node* parse_assign_list();
    Node* parse_assign();
    Node* parse_comp_init();
    Node* parse_dyn_init();
    Node* parse_const_init();
    Node* parse_lhs_id();

    // expression
    Node* parse_expr_list();
    Node* parse_expression();
    Node* parse_synthesized();
    Node* parse_inplace();
    Node* parse_ternary();

    // binary expression
    Node* parse_binary();
    Node* parse_logor_expr();
    Node* parse_logand_expr();
    Node* parse_bitor_expr();
    Node* parse_bitxor_expr();
    Node* parse_bitand_expr();
    Node* parse_eqop_expr();
    Node* parse_relop_expr();
    Node* parse_shift_expr();
    Node* parse_add_expr();
    Node* parse_mult_expr();
    
    // unary expression
    Node* parse_unary();
    Node* parse_unary_offsetof();
    Node* parse_unary_sizeof();
    Node* parse_unary_postdec();
    Node* parse_unary_postinc();
    Node* parse_unary_predec();
    Node* parse_unary_preinc();
    Node* parse_unary_deref();
    Node* parse_unary_ref();
    Node* parse_unary_lnot();
    Node* parse_unary_2scompl();
    Node* parse_unary_1scompl();
    Node* parse_unary_plus();

    // primary
    Node* parse_primary();
    Node* parse_prim_expr();
    Node* parse_prim_funcall();
    Node* parse_prim_literal();
    Node* parse_prim_ident();

    // types
    Node* parse_tyyid_pair_list();
    Node* parse_tyyid_pair();
    Node* parse_tyy_lit();
    Node* parse_tyy_enum_lit();
    Node* parse_tyy_enum_field();

    Node* parse_tyy_ext_lit();
    Node* parse_tyy_ext_field();
    Node* parse_tyy_decl();
    Node* parse_tyy_defn();
    Node* parse_tyy_storage();
    Node* parse_tyy_qualifier();
    Node* parse_tyy_cast();
    Node* parse_tyy_ref();
    Node* parse_tyy_body();
    Node* parse_tyy_ext();
    Node* parse_tyy_ext_union();
    Node* parse_tyy_ext_enum();
    Node* parse_tyy_ext_struct();
    Node* parse_tyy_alias();
    Node* parse_tyy_base();
    Node* parse_tyy_base_word();
    Node* parse_tyy_base_sign();
    Node* parse_tyy_base_body();

    // Idents
    Node* parse_long_index_opt();
    Node* parse_index_opt();
    Node* parse_index();
    Node* parse_long_ident();
    Node* parse_dot_ident();
    Node* parse_arrow_ident();

    // CompundLiteral
    Node* parse_comp_literal();
    Node* parse_list_literal();
    Node* parse_designated_init();

    // Keywords -> handled in lexer
    // Node* parse_kw_auto();
    // Node* parse_kw_break();
    // Node* parse_kw_case();
    // Node* parse_kw_char();
    // Node* parse_kw_const();
    // Node* parse_kw_continue();
    // Node* parse_kw_default();
    // Node* parse_kw_do();
    // Node* parse_kw_double();
    // Node* parse_kw_else();
    // Node* parse_kw_enum();
    // Node* parse_kw_extern();
    // Node* parse_kw_float();
    // Node* parse_kw_for();
    // Node* parse_kw_goto();
    // Node* parse_kw_if();
    // Node* parse_kw_int();
    // Node* parse_kw_long();
    // Node* parse_kw_register();
    // Node* parse_kw_return();
    // Node* parse_kw_short();
    // Node* parse_kw_signed();
    // Node* parse_kw_sizeof();
    // Node* parse_kw_static();
    // Node* parse_kw_struct();
    // Node* parse_kw_switch();
    // Node* parse_kw_typedef();
    // Node* parse_kw_union();
    // Node* parse_kw_unsigned();
    // Node* parse_kw_void();
    // Node* parse_kw_volatile();
    // Node* parse_kw_while();

    // LiteralTokens
    Node* parse_const_literal();
    Node* parse_null_const();
    Node* parse_expr_const();
    Node* parse_str_const();
    Node* parse_char_const();
    Node* parse_num_const();
    Node* parse_float_const();
    Node* parse_int_const();
    Node* parse_rational();
    Node* parse_integer();
    Node* parse_bin_integer();
    Node* parse_oct_integer();
    Node* parse_hex_integer();
    Node* parse_dec_integer();
    Node* parse_identifier();
    Node* parse_letter();
    Node* parse_lower_case();
    Node* parse_upper_case();
    Node* parse_hex_digit();
    Node* parse_digit();
    Node* parse_oct_digit();
    Node* parse_bin_digit();
    Node* parse_character();
    Node* parse_printable();
    Node* parse_char_escape();
    Node* parse_hex_escape();
    Node* parse_oct_escape();
    Node* parse_escapble();
    Node* parse_comment();
    
};






