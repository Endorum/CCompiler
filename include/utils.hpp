#pragma once

#include <iostream>
#include <fstream>
#include <sstream>
#include <string>

#include "defs.hpp"

inline std::string readFileToString(const std::string& filename){
    std::ifstream file(filename);
    if(!file){
        std::cerr << "ERROR: Could not open file: " << filename << "\n";
        return "";
    }

    std::ostringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

inline KeyWordType keyWordTypeFromString(std::string str){
    if(str == "auto") return KEYWORD_AUTO; 
    if(str == "break") return KEYWORD_BREAK; 
    if(str == "case") return KEYWORD_CASE; 
    if(str == "char") return KEYWORD_CHAR; 
    if(str == "const") return KEYWORD_CONST; 
    if(str == "continue") return KEYWORD_CONTINUE; 
    if(str == "default") return KEYWORD_DEFAULT; 
    if(str == "do") return KEYWORD_DO; 
    if(str == "double") return KEYWORD_DOUBLE; 
    if(str == "else") return KEYWORD_ELSE;
    if(str == "enum") return KEYWORD_ENUM; 
    if(str == "extern") return KEYWORD_EXTERN; 
    if(str == "float") return KEYWORD_FLOAT; 
    if(str == "for") return KEYWORD_FOR; 
    if(str == "goto") return KEYWORD_GOTO; 
    if(str == "if") return KEYWORD_IF; 
    if(str == "inline") return KEYWORD_INLINE; 
    if(str == "int") return KEYWORD_INT; 
    if(str == "long") return KEYWORD_LONG; 
    if(str == "register") return KEYWORD_REGISTER;
    if(str == "restrict") return KEYWORD_RESTRICT; 
    if(str == "return") return KEYWORD_RETURN; 
    if(str == "short") return KEYWORD_SHORT; 
    if(str == "signed") return KEYWORD_SIGNED; 
    if(str == "sizeof") return KEYWORD_SIZEOF; 
    if(str == "static") return KEYWORD_STATIC; 
    if(str == "struct") return KEYWORD_STRUCT; 
    if(str == "switch") return KEYWORD_SWITCH;
    if(str == "typedef") return KEYWORD_TYPEDEF; 
    if(str == "union") return KEYWORD_UNION; 
    if(str == "unsigned") return KEYWORD_UNSIGNED; 
    if(str == "void") return KEYWORD_VOID; 
    if(str == "volatile") return KEYWORD_VOLATILE; 
    if(str == "while") return KEYWORD_WHILE; 
    if(str == "_Alignas") return KEYWORD__ALIGNAS; 
    if(str == "_Alignof") return KEYWORD__ALIGNOF;
    if(str == "_Atomic") return KEYWORD__ATOMIC; 
    if(str == "_Bool") return KEYWORD__BOOL; 
    if(str == "_Complex") return KEYWORD__COMPLEX; 
    if(str == "_Generic") return KEYWORD__GENERIC; 
    if(str == "_Imaginary") return KEYWORD__IMAGINARY; 
    if(str == "_Noreturn") return KEYWORD__NORETURN; 
    if(str == "_Static_assert") return KEYWORD__STATIC_ASSERT; 
    if(str == "_Thread_local") return KEYWORD__THREAD_LOCAL;
    return NOT_A_KEYWORD;
}

inline std::string stringFromKeyWordType(KeyWordType type){
    if(type == KEYWORD_AUTO) return "auto";
    if(type == KEYWORD_BREAK) return "break";
    if(type == KEYWORD_CASE) return "case";
    if(type == KEYWORD_CHAR) return "char";
    if(type == KEYWORD_CONST) return "const";
    if(type == KEYWORD_CONTINUE) return "continue";
    if(type == KEYWORD_DEFAULT) return "default";
    if(type == KEYWORD_DO) return "do";
    if(type == KEYWORD_DOUBLE) return "double";
    if(type == KEYWORD_ELSE) return "else";
    if(type == KEYWORD_ENUM) return "enum";
    if(type == KEYWORD_EXTERN) return "extern";
    if(type == KEYWORD_FLOAT) return "float";
    if(type == KEYWORD_FOR) return "for";
    if(type == KEYWORD_GOTO) return "goto";
    if(type == KEYWORD_IF) return "if";
    if(type == KEYWORD_INLINE) return "inline";
    if(type == KEYWORD_INT) return "int";
    if(type == KEYWORD_LONG) return "long";
    if(type == KEYWORD_REGISTER) return "register";
    if(type == KEYWORD_RESTRICT) return "restrict";
    if(type == KEYWORD_RETURN) return "return";
    if(type == KEYWORD_SHORT) return "short";
    if(type == KEYWORD_SIGNED) return "signed";
    if(type == KEYWORD_SIZEOF) return "sizeof";
    if(type == KEYWORD_STATIC) return "static";
    if(type == KEYWORD_STRUCT) return "struct";
    if(type == KEYWORD_SWITCH) return "switch";
    if(type == KEYWORD_TYPEDEF) return "typedef";
    if(type == KEYWORD_UNION) return "union";
    if(type == KEYWORD_UNSIGNED) return "unsigned";
    if(type == KEYWORD_VOID) return "void";
    if(type == KEYWORD_VOLATILE) return "volatile";
    if(type == KEYWORD_WHILE) return "while";
    if(type == KEYWORD__ALIGNAS) return "_Alignas";
    if(type == KEYWORD__ALIGNOF) return "_Alignof";
    if(type == KEYWORD__ATOMIC) return "_Atomic";
    if(type == KEYWORD__BOOL) return "_Bool";
    if(type == KEYWORD__COMPLEX) return "_Complex";
    if(type == KEYWORD__GENERIC) return "_Generic";
    if(type == KEYWORD__IMAGINARY) return "_Imaginary";
    if(type == KEYWORD__NORETURN) return "_Noreturn";
    if(type == KEYWORD__STATIC_ASSERT) return "_Static_assert";
    if(type == KEYWORD__THREAD_LOCAL) return "_Thread_local";
    return "not a keyword";
}