#pragma once

#include <string>
typedef enum{
    NOT_A_KEYWORD,
    KEYWORD_AUTO,
    KEYWORD_BREAK,
    KEYWORD_CASE,
    KEYWORD_CHAR,
    KEYWORD_CONST,
    KEYWORD_CONTINUE,
    KEYWORD_DEFAULT,
    KEYWORD_DO,
    KEYWORD_DOUBLE,
    KEYWORD_ELSE,
    KEYWORD_ENUM,
    KEYWORD_EXTERN,
    KEYWORD_FLOAT,
    KEYWORD_FOR,
    KEYWORD_GOTO,
    KEYWORD_IF,
    KEYWORD_INLINE,
    KEYWORD_INT,
    KEYWORD_LONG,
    KEYWORD_REGISTER,
    KEYWORD_RESTRICT,
    KEYWORD_RETURN,
    KEYWORD_SHORT,
    KEYWORD_SIGNED,
    KEYWORD_SIZEOF,
    KEYWORD_STATIC,
    KEYWORD_STRUCT,
    KEYWORD_SWITCH,
    KEYWORD_TYPEDEF,
    KEYWORD_UNION,
    KEYWORD_UNSIGNED,
    KEYWORD_VOID,
    KEYWORD_VOLATILE,
    KEYWORD_WHILE,
    KEYWORD__ALIGNAS,
    KEYWORD__ALIGNOF,
    KEYWORD__ATOMIC,
    KEYWORD__BOOL,
    KEYWORD__COMPLEX,
    KEYWORD__GENERIC,
    KEYWORD__IMAGINARY,
    KEYWORD__NORETURN,
    KEYWORD__STATIC_ASSERT,
    KEYWORD__THREAD_LOCAL,
    KEYWORD_OFFSETOF,
}KeyWordType;


enum BaseType{
    BT_NONE,

    BT_VOID,
    BT_CHAR,
    BT_SHORT,
    BT_INT,
    BT_LONG,
    BT_FLOAT,
    BT_DOUBLE,
    BT_SIGNED,
    BT_UNSIGNED,

    // not really a base type i think
    // BT_STRUCT, 
    // BT_ENUM,
    
    BT_POINTER, // the underlying type is in the child
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

enum NodeTypeOLD {
  NT_OLD_None,
  NT_OLD_Program,
  NT_OLD_FunctionDecl,
  NT_OLD_FunctionDef,
  NT_OLD_ParamList,
  NT_OLD_ExpressionList,
  NT_OLD_Parameter,
  NT_OLD_CompoundStmt,
  NT_OLD_Declaration,
  NT_OLD_TypedefDeclaration,
  NT_OLD_Assignment,
  NT_OLD_IfStmt,
  NT_OLD_WhileStmt,
  NT_OLD_ReturnStmt,
  NT_OLD_ExpressionStmt,
  NT_OLD_BinaryExpr,
  NT_OLD_UnaryExpr,
  NT_OLD_Literal,
  NT_OLD_Identifier,
  NT_OLD_CallExpr,
  NT_OLD_TypeSpecifier,
  NT_OLD_PointerType,
  NT_OLD_ArraySubscripting,
  NT_OLD_TernaryExpr,
  NT_OLD_StructAccess,
  NT_OLD_TypeCastExpr,
  NT_OLD_SizeofExpr,
  NT_OLD_DoStmt,
  NT_OLD_ForStmt,
  NT_OLD_SwitchStmt,
  NT_OLD_CaseStmt,
  NT_OLD_DefaultStmt,
  NT_OLD_GotoStmt,
  NT_OLD_ContinueStmt,
  NT_OLD_BreakStmt,
  NT_OLD_StructDecl,
  NT_OLD_EnumDecl,
  NT_OLD_StructType,
  NT_OLD_EnumMember,
  NT_OLD_EnumType,
  NT_OLD_PostFixExpr,
  NT_OLD_Initializer,
};

enum NodeType{
    NT_None,
    NT_TOP_LEVEL,
    NT_TOP_LEVEL_ELEM,
    NT_FUN_DEFINITION,
    NT_FUN_SIGNATURE,
    NT_LABELED_STMT,
    NT_STATEMENT_LIST,
    NT_STATEMENT,
    NT_COMPOUND_STMT,
    NT_GOTO_STMT,
    NT_NULL_STMT,
    NT_RETURN_STMT,
    NT_CONTINUE_STMT,
    NT_BREAK_STMT,
    NT_IF_STMT,
    NT_WHILE_STMT,
    NT_DO_WHILE_STMT,
    NT_FOR_STMT,
    NT_DECL_STMT,
    NT_ASSIGN_LIST,
    NT_ASSIGN,
    NT_COMP_INIT,
    NT_DYN_INIT,
    NT_CONST_INIT,
    NT_LHS_ID,
    NT_EXPR_LIST,
    NT_EXPRESSION,
    NT_SYNTHESIZED,
    NT_INPLACE,
    NT_TERNARY,
    NT_BINARY,
    NT_LOGOR_EXPR,
    NT_LOGAND_EXPR,
    NT_BITOR_EXPR,
    NT_BITXOR_EXPR,
    NT_BITAND_EXPR,
    NT_EQOP_EXPR,
    NT_RELOP_EXPR,
    NT_SHIFT_EXPR,
    NT_ADD_EXPR,
    NT_MULT_EXPR,
    NT_UNARY,
    NT_UNARY_OFFSETOF,
    NT_UNARY_SIZEOF,
    NT_UNARY_POSTDEC,
    NT_UNARY_POSTINC,
    NT_UNARY_PREDEC,
    NT_UNARY_PREINC,
    NT_UNARY_DEREF,
    NT_UNARY_REF,
    NT_UNARY_LNOT,
    NT_2SCOMPL,
    NT_1SCOMPL,
    NT_UNARY_PLUS,
    NT_PRIMARY,
    NT_PRIM_EXPR,
    NT_PRIM_FUNCALL,
    NT_PRIM_LITERAL,
    NT_PRIM_IDENT,
    NT_TYYID_PAIR_LIST,
    NT_TYYID_PAIR,
    NT_TYY_LIT,
    NT_TYY_ENUM_LIT,
    NT_TYY_ENUM_FIELD,
    NT_TYY_EXT_LIT,
    NT_TYY_EXT_FIELD,
    NT_TYY_DECL,
    NT_TYY_DEFN,
    NT_TYY_STORAGE,
    NT_TYY_QUALIFIER,
    NT_TYY_CAST,
    NT_TYY_REF,
    NT_TYY_BODY,
    NT_TYY_EXT,
    NT_TYY_EXT_UNION,
    NT_TYY_EXT_ENUM,
    NT_TYY_EXT_STRUCT,
    NT_TYY_ALIAS,
    NT_TYY_BASE,
    NT_TYY_BASE_WORD,
    NT_TYY_BASE_SIGN,
    NT_TYY_BASE_BODY,
    NT_LONG_INDEX_OPT,
    NT_INDEX_OPT,
    NT_INDEX,
    NT_LONG_IDENT,
    NT_DOT_IDENT,
    NT_ARROW_IDENT,
    NT_COMP_LITERAL,
    NT_LIST_LITERAL,
    NT_DESIGNATED_INIT,
    NT_KW_AUTO,
    NT_KW_BREAK,
    NT_KW_CASE,
    NT_KW_CHAR,
    NT_KW_CONST,
    NT_KW_CONTINUE,
    NT_KW_DEFAULT,
    NT_KW_DO,
    NT_KW_DOUBLE,
    NT_KW_ELSE,
    NT_KW_ENUM,
    NT_KW_EXTERN,
    NT_KW_FLOAT,
    NT_KW_FOR,
    NT_KW_GOTO,
    NT_KW_IF,
    NT_KW_INT,
    NT_KW_LONG,
    NT_KW_REGISTER,
    NT_KW_RETURN,
    NT_KW_SHORT,
    NT_KW_SIGNED,
    NT_KW_SIZEOF,
    NT_KW_STATIC,
    NT_KW_INLINE,
    NT_KW_STRUCT,
    NT_KW_SWITCH,
    NT_KW_TYPEDEF,
    NT_KW_UNION,
    NT_KW_UNSIGNED,
    NT_KW_VOID,
    NT_KW_VOLATILE,
    NT_KW_WHILE,
    NT_KW_RESTRICT,
    NT_CONST_LITERAL,
    NT_NULL_CONST,
    NT_EXPR_CONST,
    NT_STR_CONST,
    NT_CHAR_CONST,
    NT_NUM_CONST,
    NT_FLOAT_CONST,
    NT_INT_CONST,
    NT_RATIONAL,
    NT_INTEGER,
    NT_BIN_INTEGER,
    NT_OCT_INTEGER,
    NT_HEX_INTEGER,
    NT_DEC_INTEGER,
    NT_IDENTIFIER,
    NT_LETTER,
    NT_LOWER_CASE,
    NT_UPPER_CASE,
    NT_HEX_DIGIT,
    NT_DIGIT,
    NT_OCT_DIGIT,
    NT_BIN_DIGIT,
    NT_CHARACTER,
    NT_STRING_LITERAL,
    NT_PRINTABLE,
    NT_CHAR_ESCAPE,
    NT_HEX_ESCAPE,
    NT_OCT_ESCAPE,
    NT_ESCAPBLE,
    NT_COMMENT,

    // internal
    NT_STAR,
    NT_INPLACE_OP, // '+=' etc
};

inline std::string NodeType_str(NodeType type){
    switch (type) {
        case NT_None: return "None";

        case NT_TOP_LEVEL: return "top_level";
        case NT_TOP_LEVEL_ELEM: return "top_level_elem";
        case NT_FUN_DEFINITION: return "fun_definition";
        case NT_FUN_SIGNATURE: return "fun_signature";
        case NT_LABELED_STMT: return "labeled_stmt";
        case NT_STATEMENT_LIST: return "statement_list";
        case NT_STATEMENT: return "statement";
        case NT_COMPOUND_STMT: return "compound_stmt";
        case NT_GOTO_STMT: return "goto_stmt";
        case NT_NULL_STMT: return "null_stmt";
        case NT_RETURN_STMT: return "return_stmt";
        case NT_CONTINUE_STMT: return "continue_stmt";
        case NT_BREAK_STMT: return "break_stmt";
        case NT_IF_STMT: return "if_stmt";
        case NT_WHILE_STMT: return "while_stmt";
        case NT_DO_WHILE_STMT: return "do_while_stmt";
        case NT_FOR_STMT: return "for_stmt";
        case NT_DECL_STMT: return "decl_stmt";
        case NT_ASSIGN_LIST: return "assign_list";
        case NT_ASSIGN: return "assign";
        case NT_COMP_INIT: return "comp_init";
        case NT_DYN_INIT: return "dyn_init";
        case NT_CONST_INIT: return "const_init";
        case NT_LHS_ID: return "lhs_id";
        case NT_EXPR_LIST: return "expr_list";
        case NT_EXPRESSION: return "expression";
        case NT_SYNTHESIZED: return "synthesized";
        case NT_INPLACE: return "inplace";
        case NT_TERNARY: return "ternary";
        case NT_BINARY: return "binary";
        case NT_LOGOR_EXPR: return "logor_expr";
        case NT_LOGAND_EXPR: return "logand_expr";
        case NT_BITOR_EXPR: return "bitor_expr";
        case NT_BITXOR_EXPR: return "bitxor_expr";
        case NT_BITAND_EXPR: return "bitand_expr";
        case NT_EQOP_EXPR: return "eqop_expr";
        case NT_RELOP_EXPR: return "relop_expr";
        case NT_SHIFT_EXPR: return "shift_expr";
        case NT_ADD_EXPR: return "add_expr";
        case NT_MULT_EXPR: return "mult_expr";
        case NT_UNARY: return "unary";
        case NT_UNARY_OFFSETOF: return "unary_offsetof";
        case NT_UNARY_SIZEOF: return "unary_sizeof";
        case NT_UNARY_POSTDEC: return "unary_postdec";
        case NT_UNARY_POSTINC: return "unary_postinc";
        case NT_UNARY_PREDEC: return "unary_predec";
        case NT_UNARY_PREINC: return "unary_preinc";
        case NT_UNARY_DEREF: return "unary_deref";
        case NT_UNARY_REF: return "unary_ref";
        case NT_UNARY_LNOT: return "unary_lnot";
        case NT_2SCOMPL: return "2scompl";
        case NT_1SCOMPL: return "1scompl";
        case NT_UNARY_PLUS: return "unary_plus";
        case NT_PRIMARY: return "primary";
        case NT_PRIM_EXPR: return "prim_expr";
        case NT_PRIM_FUNCALL: return "prim_funcall";
        case NT_PRIM_LITERAL: return "prim_literal";
        case NT_PRIM_IDENT: return "prim_ident";
        case NT_TYYID_PAIR_LIST: return "tyyid_pair_list";
        case NT_TYYID_PAIR: return "tyyid_pair";
        case NT_TYY_LIT: return "tyy_lit";
        case NT_TYY_ENUM_LIT: return "tyy_enum_lit";
        case NT_TYY_ENUM_FIELD: return "tyy_enum_field";
        case NT_TYY_EXT_LIT: return "tyy_ext_lit";
        case NT_TYY_EXT_FIELD: return "tyy_ext_field";
        case NT_TYY_DECL: return "tyy_decl";
        case NT_TYY_DEFN: return "tyy_defn";
        case NT_TYY_STORAGE: return "tyy_storage";
        case NT_TYY_QUALIFIER: return "tyy_qualifier";
        case NT_TYY_CAST: return "tyy_cast";
        case NT_TYY_REF: return "tyy_ref";
        case NT_TYY_BODY: return "tyy_body";
        case NT_TYY_EXT: return "tyy_ext";
        case NT_TYY_EXT_UNION: return "tyy_ext_union";
        case NT_TYY_EXT_ENUM: return "tyy_ext_enum";
        case NT_TYY_EXT_STRUCT: return "tyy_ext_struct";
        case NT_TYY_ALIAS: return "tyy_alias";
        case NT_TYY_BASE: return "tyy_base";
        case NT_TYY_BASE_WORD: return "tyy_base_word";
        case NT_TYY_BASE_SIGN: return "tyy_base_sign";
        case NT_TYY_BASE_BODY: return "tyy_base_body";
        case NT_LONG_INDEX_OPT: return "long_index_opt";
        case NT_INDEX_OPT: return "index_opt";
        case NT_INDEX: return "index";
        case NT_LONG_IDENT: return "long_ident";
        case NT_DOT_IDENT: return "dot_ident";
        case NT_ARROW_IDENT: return "arrow_ident";
        case NT_COMP_LITERAL: return "comp_literal";
        case NT_LIST_LITERAL: return "list_literal";
        case NT_DESIGNATED_INIT: return "designated_init";
        case NT_KW_AUTO: return "kw_auto";
        case NT_KW_BREAK: return "kw_break";
        case NT_KW_CASE: return "kw_case";
        case NT_KW_CHAR: return "kw_char";
        case NT_KW_CONST: return "kw_const";
        case NT_KW_CONTINUE: return "kw_continue";
        case NT_KW_DEFAULT: return "kw_default";
        case NT_KW_DO: return "kw_do";
        case NT_KW_DOUBLE: return "kw_double";
        case NT_KW_ELSE: return "kw_else";
        case NT_KW_ENUM: return "kw_enum";
        case NT_KW_EXTERN: return "kw_extern";
        case NT_KW_FLOAT: return "kw_float";
        case NT_KW_FOR: return "kw_for";
        case NT_KW_GOTO: return "kw_goto";
        case NT_KW_IF: return "kw_if";
        case NT_KW_INT: return "kw_int";
        case NT_KW_LONG: return "kw_long";
        case NT_KW_REGISTER: return "kw_register";
        case NT_KW_RETURN: return "kw_return";
        case NT_KW_SHORT: return "kw_short";
        case NT_KW_SIGNED: return "kw_signed";
        case NT_KW_SIZEOF: return "kw_sizeof";
        case NT_KW_STATIC: return "kw_static";
        case NT_KW_INLINE: return "kw_inline";
        case NT_KW_STRUCT: return "kw_struct";
        case NT_KW_SWITCH: return "kw_switch";
        case NT_KW_TYPEDEF: return "kw_typedef";
        case NT_KW_UNION: return "kw_union";
        case NT_KW_UNSIGNED: return "kw_unsigned";
        case NT_KW_VOID: return "kw_void";
        case NT_KW_VOLATILE: return "kw_volatile";
        case NT_KW_WHILE: return "kw_while";
        case NT_KW_RESTRICT: return "nt_kw_restrict";
        case NT_CONST_LITERAL: return "const_literal";
        case NT_NULL_CONST: return "null_const";
        case NT_EXPR_CONST: return "expr_const";
        case NT_STR_CONST: return "str_const";
        case NT_CHAR_CONST: return "char_const";
        case NT_NUM_CONST: return "num_const";
        case NT_FLOAT_CONST: return "float_const";
        case NT_INT_CONST: return "int_const";
        case NT_RATIONAL: return "rational";
        case NT_INTEGER: return "integer";
        case NT_BIN_INTEGER: return "bin_integer";
        case NT_OCT_INTEGER: return "oct_integer";
        case NT_HEX_INTEGER: return "hex_integer";
        case NT_DEC_INTEGER: return "dec_integer";
        case NT_IDENTIFIER: return "identifier";
        case NT_LETTER: return "letter";
        case NT_LOWER_CASE: return "lower_case";
        case NT_UPPER_CASE: return "upper_case";
        case NT_HEX_DIGIT: return "hex_digit";
        case NT_DIGIT: return "digit";
        case NT_OCT_DIGIT: return "oct_digit";
        case NT_BIN_DIGIT: return "bin_digit";
        case NT_CHARACTER: return "character";
        case NT_STRING_LITERAL: return "string_literal";
        case NT_PRINTABLE: return "printable";
        case NT_CHAR_ESCAPE: return "char_escape";
        case NT_HEX_ESCAPE: return "hex_escape";
        case NT_OCT_ESCAPE: return "oct_escape";
        case NT_ESCAPBLE: return "escapble";
        case NT_COMMENT: return "comment";
        case NT_STAR: return "star";

        default: return "Unknown";
    }
}