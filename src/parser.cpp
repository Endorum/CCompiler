
#include <cerrno>
#include <cmath>
#include <cstddef>
#include <regex>
#include <string>

#include "../include/parser.hpp"

Token Parser::peek(size_t off) {
    if (isAtEnd(off)) return Token(TokenType::END_OF_FILE, "");
    return tokens[pos+off];
}

Token Parser::advance() {
    if (!isAtEnd()) pos++;
    return tokens[pos - 1];
}

bool Parser::match(TokenType type) {
    if (isAtEnd()) return false;
    if (peek().type != type) return false;
    pos++;
    return true;
}

bool Parser::expect(TokenType type) {
    if (match(type)) return true;
    error("Expected token of type: " + stringFromTokenType(type));
    return false; // just in case
}

bool Parser::isAtEnd(size_t off) {
    return pos+off >= tokens.size(); // || tokens[pos].type == TokenType::END_OF_FILE;
}

bool Parser::match_kw(KeyWordType type) {
    if (isAtEnd()) return false;
    if (peek().kw_type != type) return false;
    pos++;
    return true;
}

void Parser::error(const std::string& msg) {
    std::cerr << "Parser Error: " << msg << "\n";
    std::cerr << "At token position: " << pos;

    if(pos < tokens.size()){
        Token token = tokens[pos];
        std::cerr << " Token: " << token.str();
    }

    std::cerr << "\nContext (last few tokens): \n";
    size_t start = (pos >= 5) ? pos - 5 : 0;
    for(size_t i = start; i < pos && i < tokens.size(); i++){
        std::cerr << " " << i << ": " << tokens[i].str() << "\n";
    }

    std::cerr << "\nPartial AST:\n";
    std::cerr << root->str();


    exit(1);
}

void Parser::parse(){
    parse_top_level();
}


Node* Parser::parse_top_level() {
    // top_level	::= { top_level_elem | comment }
    root = new Node(NT_TOP_LEVEL);

    while(!isAtEnd()){
        // comments are already handled in the tokenizer
        Node* elem = parse_top_level_elem();
        root->addChild(elem);
    }

    return root;

}

Node* Parser::parse_top_level_elem() {
    // top_level_elem	::= decl_stmt | fun_definition
    
    size_t backup = pos;
    Node* node;

    node = parse_decl_stmt();
    if(node != nullptr) return node;

    pos = backup;
    
    node = parse_fun_definition();
    if(node != nullptr) return node;

    error("Expected declaration or function definition");
    return nullptr;
}

Node* Parser::parse_fun_definition() {
    // fun_definition	::= fun_signature compound_statement
    Node* functionNode = new Node(NT_FUN_DEFINITION);

    Node* fun_signature = parse_fun_signature();
    if(!fun_signature) return nullptr;

    Node* compound_stmt = parse_compound_stmt();
    if(!compound_stmt) {
        delete functionNode;
        error("Expected a compound statement for function definition");
    }

    functionNode->addChild(fun_signature);
    functionNode->addChild(compound_stmt);

    return functionNode;
}

Node* Parser::parse_fun_signature() {
    // fun_signature	::= [ kw_static ] [ kw_inline ] tyy_decl identifier '(' tyyid_pair_list ')'
    Node* fun_signature = new Node(NT_FUN_SIGNATURE);

    if(match_kw(KEYWORD_STATIC)){
        Node* staticKw = new Node(NT_KW_STATIC, "static");
        fun_signature->addChild(staticKw);
    }

    if(match_kw(KEYWORD_INLINE)){
        Node* inlineKw = new Node(NT_KW_INLINE, "inline");
        fun_signature->addChild(inlineKw);
    }

    Node* returnType = parse_tyy_decl();
    if(!returnType){
        delete fun_signature;
        return nullptr;
    }
    fun_signature->addChild(returnType);

    if(!match(LPAREN)){
        error("Expected '(' in function siganture");
    }

    Node* paramList = parse_tyyid_pair_list(); // e.g. (int a, float b)
    fun_signature->addChild(paramList);

    if(!match(RPAREN)){
        error("Expected ')' after function parameter list");
    }

    return fun_signature;
}

Node* Parser::parse_labeled_stmt() {
    // labeled_stmt	::= identifier ':' statement_list
    Node* labeled_stmt = new Node(NT_LABELED_STMT);

    Node* ident = parse_identifier();
    if(!ident){
        delete labeled_stmt;
        return nullptr;
    }

    if(!match(COLON)){
        error("Expected ':' for labeled statment");
    }

    Node* statmente_list = parse_statement_list();
    if(!statmente_list){
        error("Expected statement list for labeled statement");
    }

    labeled_stmt->addChild(ident);
    labeled_stmt->addChild(statmente_list);

    return labeled_stmt;
}

Node* Parser::parse_statement_list() {
    // statement_list  ::= { statement }
    Node* statement_list = new Node(NT_STATEMENT_LIST);
    
    // im not sure if RBRACE is correct here as it not exactly described by the EBNF see 
    // compound_stmt	::= '{' statement_list '}'
    while(!isAtEnd() && peek().type != TokenType::RBRACE){
        Node* stmt = parse_statement();
        if(!stmt){
            error("Expected a statement in statement list");
        }
        statement_list->addChild(stmt);
    }
    
    return statement_list;
}

Node* Parser::parse_statement() {
    /*
    statement	::=   decl_stmt
		            | for_stmt
		            | break_stmt
		            | null_stmt
		            | return_stmt
		            | compound_stmt
		            | continue_stmt
		            | if_stmt
		            | while_stmt
		            | do_while_stmt
		            | labeled_stmt
		            | goto_stmt
    */


    Node* stmt = nullptr;

    if( (stmt = parse_decl_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_for_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_break_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_null_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_return_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_compound_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_continue_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_if_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_while_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_do_while_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_labeled_stmt()) != nullptr ) return stmt;
    if( ( stmt = parse_goto_stmt()) != nullptr ) return stmt;

    return nullptr;

}

Node* Parser::parse_compound_stmt() {
    // compound_stmt	::= '{' statement_list '}'

    Node* compound_stmt = new Node(NT_COMPOUND_STMT);
    
    if(!match(LBRACE)){
        delete compound_stmt;
        return nullptr;
    }

    Node* stmt_list = parse_statement_list();
    compound_stmt->addChild(stmt_list);

    if(!match(RBRACE)){
        delete compound_stmt;
        error("Error: Expected a closing brace after compound statement");
    }

    return compound_stmt;
}

Node* Parser::parse_goto_stmt() {
    // goto_stmt	::= kw_goto identifier ';'
    Node* goto_stmt = new Node(NT_GOTO_STMT);

    Node* gotoKw = new Node(NT_KW_STATIC, "goto");
    goto_stmt->addChild(gotoKw);

    Node* ident = parse_identifier();
    if(!ident){
        error("Expected an identifier for goto statement");
    }

    expect(SEMICOLON);

    return goto_stmt;
}

Node* Parser::parse_null_stmt() {
    // null_stmt	::= ';'
    if(match(SEMICOLON)) return new Node(NT_NULL_STMT);
    return nullptr;
}

Node* Parser::parse_return_stmt() {
    // return_stmt	::= kw_return expression ';'

    Node* return_stmt = new Node(NT_RETURN_STMT);

    if(!match_kw(KEYWORD_RETURN)){
        delete return_stmt;
        return nullptr;
    }

    Node* expr = parse_expression();
    if(!expr){
        error("Expected an expression for return statement");
    }

    expect(SEMICOLON);

    return return_stmt;
}

Node* Parser::parse_continue_stmt() {
    // continue_stmt	::= kw_continue ';'
    if(!match_kw(KEYWORD_CONTINUE)) return nullptr;
    expect(SEMICOLON);

    return new Node(NT_CONTINUE_STMT);
}

Node* Parser::parse_break_stmt() {
    // break_stmt	::= kw_break ';'
    if(!match_kw(KEYWORD_BREAK)) return nullptr;
    expect(SEMICOLON);

    return new Node(NT_BREAK_STMT);
}

Node* Parser::parse_if_stmt() {
    // if_stmt		::= kw_if '(' expr_list ')' compound_stmt { kw_else [ '(' expr_list ')' ] compound_stmt }
    Node* if_stmt = new Node(NT_IF_STMT);

    if(!match_kw(KEYWORD_IF)) return nullptr;
    
    expect(LPAREN);
    Node* cond = parse_expr_list();
    expect(RPAREN);

    Node* body = parse_compound_stmt();

    if_stmt->addChild(cond);
    if_stmt->addChild(body);

    if(match_kw(KEYWORD_ELSE)){

        if(peek().type == LPAREN){
            advance(); // consume '('
            Node* elseCond = parse_expr_list();
            expect(RPAREN);
            if_stmt->addChild(elseCond);
        }

        Node* elseBody = parse_compound_stmt();
        if_stmt->addChild(elseBody);

    }

    return if_stmt;
}

Node* Parser::parse_while_stmt() {
    // while_stmt	::= kw_while '(' expr_list ')' compound_stmt
    if(!match_kw(KEYWORD_WHILE)) return nullptr;

    Node* while_stmt = new Node(NT_WHILE_STMT);
    
    expect(LPAREN);
    Node* cond = parse_expr_list();
    expect(RPAREN);

    Node* body = parse_compound_stmt();

    while_stmt->addChild(cond);
    while_stmt->addChild(body);

    return while_stmt;
}

Node* Parser::parse_do_while_stmt() {
    // do_while_stmt	::= kw_do compound_stmt kw_while '(' expr_list ')' ';'
    if(!match_kw(KEYWORD_DO)) return nullptr;

    Node* body = parse_compound_stmt();
    if(!body){
        error("Expected body for do-while statement");
    }

    if(!match_kw(KEYWORD_WHILE)){
        error("Expected while keyword after body in do-while statement");
    }

    expect(LPAREN);
    Node* cond = parse_expr_list();
    expect(RPAREN);

    expect(SEMICOLON);

    Node* do_while_stmt = new Node(NT_DO_WHILE_STMT);
    do_while_stmt->addChild(body);
    do_while_stmt->addChild(cond);

    return do_while_stmt;
}

Node* Parser::parse_for_stmt() {
    // for_stmt	::= kw_for '(' assign_list ';' expr_list ';' expr_list ')' compound_stmt
    if(!match_kw(KEYWORD_FOR)) return nullptr;

    expect(LPAREN);

    Node* init = parse_assign_list();
    expect(SEMICOLON);

    Node* cond = parse_expr_list();
    expect(SEMICOLON);

    Node* inc = parse_expr_list();

    expect(RPAREN);

    Node* body = parse_compound_stmt();

    Node* for_stmt = new Node(NT_FOR_STMT);

    for_stmt->addChild(init);
    for_stmt->addChild(cond);
    for_stmt->addChild(inc);
    for_stmt->addChild(body);

    return for_stmt;
}

Node* Parser::parse_decl_stmt() {
    /*
    decl_stmt	::=   assign_list ';'
		            | tyy_defn ';'
		            | fun_signature ';'
    */


    size_t backup = pos;

    // Try assign_list ';'
    if(Node* assign = parse_assign_list()){
        if(match(SEMICOLON)){
            Node* stmt = new Node(NT_DECL_STMT);
            stmt->addChild(assign);
            return stmt;
        }
        pos = backup;
    } else {
        pos = backup;
    }

    // try tyy_defn ';'
    if(Node* defn = parse_tyy_defn()){
        if(match(SEMICOLON)){
            Node* stmt = new Node(NT_DECL_STMT);
            stmt->addChild(defn);
            return stmt;
        }
        pos = backup;
    }else{
        pos = backup;
    }

    // Try fun_signature ';' (for function declaration without a body)
    if(Node* signature = parse_fun_signature()){
        if(match(SEMICOLON)){
            Node* stmt = new Node(NT_DECL_STMT);
            stmt->addChild(signature);
            return stmt;
        }
        pos = backup;
    }else{
        pos = backup;
    }

    return nullptr;
}



Node* Parser::parse_assign_list(){
    // assign_list 	::= assign_stmt { ',' assign_stmt }

    Node* first = parse_assign();
    if(!first) return nullptr;

    Node* assign_list = new Node(NT_ASSIGN_LIST);
    assign_list->addChild(first);

    while(match(COMMA)){
        Node* next = parse_assign();
        if(!next) error("Expected assigment after ',' in assign list");
        assign_list->addChild(next);
    }

    
    return assign_list;
}
Node* Parser::parse_assign(){
    /*
    assign		::=   dyn_init
		            | const_init
		            | comp_literal
    */
    Node* assign = new Node(NT_ASSIGN);

    
    if(Node* dyn_init = parse_dyn_init()){
        assign->addChild(dyn_init);
    }else if(Node* const_init = parse_const_init()){
        assign->addChild(const_init);
    }else if(Node* comp_literal = parse_comp_literal()){
        assign->addChild(comp_literal);
    }else{
        delete assign;
        return nullptr;
    }


     
    return assign;
}
Node* Parser::parse_comp_init(){
    // comp_init	::= [ tyy_decl ] lhs_id [ '=' [ tyy_cast ] comp_literal ]
    Node* comp_init = new Node(NT_COMP_INIT);

    if(Node* tyy_decl = parse_tyy_decl()){
        comp_init->addChild(tyy_decl);
    }

    if(Node* lhs_id = parse_lhs_id()){
        comp_init->addChild(lhs_id);
    }else{
        delete comp_init;
        return nullptr;
    }

    if(match(ASSIGN)){
        Node* assignNode = new Node(NT_ASSIGN);


        if(Node* tyy_cast = parse_tyy_cast()){
            assignNode->addChild(tyy_cast);
        }

        if(Node* comp_literal = parse_comp_literal()){
            assignNode->addChild(comp_literal);
        }else{
            error("Expected compound literal after '='");
        }

        comp_init->addChild(assignNode);

    }



    return comp_init;
}
Node* Parser::parse_dyn_init(){
    // dyn_init	::= [ tyy_decl ] lhs_id [ '=' [ tyy_cast ] expression ]
    Node* dyn_init = new Node(NT_DYN_INIT);
    
    if(Node* tyy_decl = parse_tyy_decl()){
        dyn_init->addChild(tyy_decl);
    }

    if(Node* lhs_id = parse_lhs_id()){
        dyn_init->addChild(lhs_id);
    }else{
        delete dyn_init;
        return nullptr;
    }

    if(match(ASSIGN)){
        Node* assignNode = new Node(NT_ASSIGN);


        if(Node* tyy_cast = parse_tyy_cast()){
            assignNode->addChild(tyy_cast);
        }

        if(Node* expr = parse_expression()){
            assignNode->addChild(expr);
        }else{
            error("Expected compound literal after '='");
        }

        dyn_init->addChild(assignNode);

    }

    return dyn_init;
}
Node* Parser::parse_const_init(){
    // const_init	::= [ tyy_decl ] lhs_id [ '=' [ tyy_cast ] const_literal ]
    Node* const_init = new Node(NT_CONST_INIT);

    if(Node* tyy_decl = parse_tyy_decl()){
        const_init->addChild(tyy_decl);
    }

    if(Node* lhs_id = parse_lhs_id()){
        const_init->addChild(lhs_id);
    }else{
        delete const_init;
        return nullptr;
    }

    if(match(ASSIGN)){
        Node* assignNode = new Node(NT_ASSIGN);


        if(Node* tyy_cast = parse_tyy_cast()){
            assignNode->addChild(tyy_cast);
        }

        if(Node* lit = parse_const_literal()){
            assignNode->addChild(lit);
        }else{
            error("Expected compound literal after '='");
        }

        const_init->addChild(assignNode);

    }

    return const_init;
}
Node* Parser::parse_lhs_id(){
    // lhs_id		::= [ kw_const ] [ '*' ] ( long_index_opt | index_opt )
    Node* lhs_id = new Node(NT_LHS_ID);

    if(match_kw(KEYWORD_CONST)){
        lhs_id->addChild(new Node(NT_KW_CONST, "const"));
    }

    if(match(TokenType::STAR)){
        lhs_id->addChild(new Node(NT_STAR, "*"));
    }

    size_t backup = pos;
    Node* long_index = parse_long_index_opt();
    if(long_index){
        lhs_id->addChild(long_index);
        return lhs_id;
    }

    pos = backup;
    Node* index = parse_index_opt();
    if(index){
        lhs_id->addChild(index);
        return lhs_id;
    }

    error("Expected either a long_index or an index, but none was given");
    return nullptr;
}

// expression
Node* Parser::parse_expr_list(){
    // expr_list	::= expression { ',' expression }
    Node* expr_list = new Node(NT_EXPR_LIST);

    Node* first = parse_expression();
    if(!first){
        delete expr_list;
        return nullptr;
    }
    expr_list->addChild(first);

    while(match(COMMA)){
        Node* expr = parse_expression();
        if(!expr){
            error("Expected expression after ',' ");
        }
        expr_list->addChild(expr);
    }

    return expr_list;
}
Node* Parser::parse_expression(){
    /*
    expression	::=   primary
	                | unary
		            | binary
		            | ternary
		            | inplace
		            | synthesized
    */
    
    Node* node = nullptr;

    if ((node = parse_ternary())) return node;
    if ((node = parse_binary())) return node;
    if ((node = parse_unary())) return node;
    if ((node = parse_inplace())) return node;
    if ((node = parse_synthesized())) return node;
    if ((node = parse_primary())) return node;


    return nullptr;
}
Node* Parser::parse_synthesized(){
    // synthesized	::= '(' assign_list ')'

    if(!match(LPAREN)) return nullptr;

    Node* assign_list = parse_assign_list();
    expect(RPAREN);

    Node* synthesized = new Node(NT_SYNTHESIZED);
    synthesized->addChild(assign_list);

    return synthesized;
}
Node* Parser::parse_inplace(){
    /*
    inplace		::= long_index_opt  ( "+="  | "-=" 
                                    | "*="  | "/=" 
                                    | "%="  | "<<=" 
                                    | ">>=" | "&=" 
                                    | "|="  | "^=" ) expression
    */
    Node* inplace = new Node(NT_INPLACE);
    Node* long_index;
    if(!(long_index = parse_long_index_opt())){
        delete inplace;
        return nullptr;
    }

    size_t backup = pos;

    if(peek().type == TokenType::PLUS_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::MINUS_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::STAR_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::SLASH_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::MOD_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::SHL_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::SHR_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::BW_AND_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else if(peek().type == TokenType::BW_OR_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }if(peek().type == TokenType::BW_XOR_ASSIGN){
        inplace->addChild(new Node(NT_INPLACE_OP, peek().value));
    }else {
        delete inplace;
        error("Expected inplace operator, got: " + peek().value);
    }

    advance(); // consume inplace operator

    Node* expr = parse_expression();
    if(!expr){
        error("Expected expression after inplace operator: " + peek(-1).value);
    }
    inplace->addChild(expr);

    return inplace;
}
Node* Parser::parse_ternary(){
    // ternary		::= binary '?' binary ':' binary 

    size_t backup = pos;

    Node* cond = parse_binary();
    if(!cond) return nullptr;
    
    if(!match(QUESTION)){
        pos = backup;
        return nullptr;
    }

    Node* trueExpr = parse_binary();
    if(!trueExpr){
        error("Expected expression after '?' in ternary expression");
    }

    expect(COLON);

    Node* falseExpr = parse_binary();
    if(!falseExpr){
        error("Expected expression after ':' in ternary expression");
    }

    Node* ternary = new Node(NT_TERNARY);
    ternary->addChild(cond);
    ternary->addChild(trueExpr);
    ternary->addChild(falseExpr);
    
    return ternary;
}

// binary expression
Node* Parser::parse_binary(){
    // binary		::= logor_expr

    Node* node = nullptr;
    if( (node = parse_logor_expr()) ) return node;

    return node;
}
Node* Parser::parse_logor_expr(){
    // logor_expr	::= logand_expr "||" logor_expr
    Node* left = parse_logand_expr();
    if(!left) return nullptr;

    while(match(OR)){
        Node* right = parse_logand_expr();
        if(!right) error("Expected expression after '||' ");

        Node* op = new Node(NT_LOGOR_EXPR, "||");
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}
Node* Parser::parse_logand_expr(){
    // logand_expr	::= bitor_expr "&&" logand_expr
    Node* left = parse_bitor_expr();
    if(!left) return nullptr;

    while(match(AND)){
        Node* right = parse_bitor_expr();
        if(!right) error("Expected expression after '&&' ");

        Node* op = new Node(NT_LOGAND_EXPR, "&&");
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}
Node* Parser::parse_bitor_expr(){
    // bitor_expr	::= bitxor_expr '|' bitor_expr

    Node* left = parse_bitxor_expr();
    if(!left) return nullptr;

    while(match(BIT_OR)){
        Node* right = parse_bitxor_expr();
        if(!right) error("Expected expression after '|' ");

        Node* op = new Node(NT_BITOR_EXPR, "|");
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}
Node* Parser::parse_bitxor_expr(){
    // bitxor_expr	::= bitand_expr '^' bitxor_expr

    Node* left = parse_bitand_expr();
    if(!left) return nullptr;

    while(match(BIT_XOR)){
        Node* right = parse_bitand_expr();
        if(!right) error("Expected expression after '^' ");

        Node* op = new Node(NT_BITXOR_EXPR, "^");
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}
Node* Parser::parse_bitand_expr(){
    // bitand_expr	::= eqop_expr '&' bitand_expr
    Node* left = parse_eqop_expr();
    if(!left) return nullptr;

    while(match(BIT_AND)){
        Node* right = parse_eqop_expr();
        if(!right) error("Expected expression after '&' ");

        Node* op = new Node(NT_BITAND_EXPR, "&");
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}
Node* Parser::parse_eqop_expr(){
    /*
    eqop_expr	::= relop_expr "==" eqop_expr
	    	      | relop_expr "!=" eqop_expr
    */
    Node* left = parse_relop_expr();
    if(!left) return nullptr;


    while(peek().type == EQ || peek().type == NEQ){
        Token opTok = advance();
        Node* right = parse_relop_expr();

        if(!right){
            error("Expected expression after '" + opTok.value + "'");
        }

        Node* op = new Node(NT_EQOP_EXPR, opTok.value);
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}
Node* Parser::parse_relop_expr(){
    /*
    relop_expr	::= shift_expr '<' relop_expr
		          | shift_expr '>' relop_expr
		          | shift_expr "<=" relop_expr
		          | shift_expr ">=" relop_expr
    */

    Node* left = parse_shift_expr();
    if(!left) return nullptr;


    while(peek().type == TokenType::LT || 
          peek().type == TokenType::GT ||
          peek().type == TokenType::LTE ||
          peek().type == TokenType::GTE
        ){
        Token opTok = advance();
        Node* right = parse_shift_expr();

        if(!right){
            error("Expected expression after '" + opTok.value + "'");
        }

        Node* op = new Node(NT_RELOP_EXPR, opTok.value);
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}

Node* Parser::parse_shift_expr(){
    /*
    shift_expr	::= add_expr ">>" shift_expr
		          | add_expr "<<" shift_expr
    */
    Node* left = parse_add_expr();
    if(!left) return nullptr;


    while(peek().type == SHIFT_RIGHT || peek().type == SHIFT_LEFT){
        Token opTok = advance();
        Node* right = parse_add_expr();

        if(!right){
            error("Expected expression after '" + opTok.value + "'");
        }

        Node* op = new Node(NT_SHIFT_EXPR, opTok.value);
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}

Node* Parser::parse_add_expr(){
    /*
    add_expr	::= mult_expr '+' add_expr
		          | mult_expr '-' add_expr
    */
    Node* left = parse_mult_expr();
    if(!left) return nullptr;


    while(peek().type == PLUS || peek().type == MINUS){
        Token opTok = advance();
        Node* right = parse_mult_expr();

        if(!right){
            error("Expected expression after '" + opTok.value + "'");
        }

        Node* op = new Node(NT_ADD_EXPR, opTok.value);
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}
Node* Parser::parse_mult_expr(){
    /*
    mult_expr	::= unary '*' multi_expr
		          | unary '/' multi_expr
		          | unary '%' mutli_expr
    */

    Node* left = parse_unary();
    if(!left) return nullptr;


    while(peek().type == STAR || peek().type == SLASH || peek().type == PERCENT){
        Token opTok = advance();
        Node* right = parse_unary();

        if(!right){
            error("Expected expression after '" + opTok.value + "'");
        }

        Node* op = new Node(NT_MULT_EXPR, opTok.value);
        op->addChild(left);
        op->addChild(right);
        left = op;
    }

    return left;
}

// unary expression
Node* Parser::parse_unary(){
    /*
    unary		::= unary_plus
		          | unary_minus
		          | unary_1scomp
		          | unary_2scomp
		          | unary_lnot
		          | unary_ref
                  | unary_deref
		          | unary_preinc
		          | unary_predec
		          | unary_postinc
		          | unary_postdec
                  | unary_offsetof
		          | unary_sizeof
    */

    Node* unary = nullptr;

    if( (unary = parse_unary_plus()) != nullptr) return unary;
    if( (unary = parse_unary_1scompl()) != nullptr) return unary;
    if( (unary = parse_unary_2scompl()) != nullptr) return unary;
    if( (unary = parse_unary_lnot()) != nullptr) return unary;
    if( (unary = parse_unary_ref()) != nullptr) return unary;
    if( (unary = parse_unary_deref()) != nullptr) return unary;
    if( (unary = parse_unary_preinc()) != nullptr) return unary;
    if( (unary = parse_unary_predec()) != nullptr) return unary;
    if( (unary = parse_unary_postinc()) != nullptr) return unary;
    if( (unary = parse_unary_postdec()) != nullptr) return unary;
    if( (unary = parse_unary_offsetof()) != nullptr) return unary;
    if( (unary = parse_unary_sizeof()) != nullptr) return unary;

    return nullptr;

}
Node* Parser::parse_unary_offsetof(){
    // unary_offsetof	::= "offsetof" primary
    if(!match_kw(KEYWORD_OFFSETOF)) return nullptr;
    
    Node* primary = parse_primary();
    if(!primary) error("Expected primary after offsetof keyword");

    Node* unary_offsetof = new Node(NT_UNARY_OFFSETOF);
    unary_offsetof->addChild(primary);

    return unary_offsetof;
}
Node* Parser::parse_unary_sizeof(){
    // unary_sizeof	::= "sizeof" primary

    if(!match_kw(KEYWORD_SIZEOF)) return nullptr;
    
    Node* primary = parse_primary();
    if(!primary) error("Expected primary after sizeof keyword");

    Node* unary_sizeof = new Node(NT_UNARY_SIZEOF);
    unary_sizeof->addChild(primary);

    return unary_sizeof;
}
Node* Parser::parse_unary_postdec(){
    // unary_postdec	::= primary "--"

    Node* primary = parse_primary();
    if(!primary) return nullptr;

    Node* unary_postdec = new Node(NT_UNARY_POSTDEC, "--");
    unary_postdec->addChild(primary);

    return unary_postdec;
}
Node* Parser::parse_unary_postinc(){
    // unary_postinc	::= primary "++"

    Node* primary = parse_primary();
    if(!primary) return nullptr;

    Node* unary_postinc = new Node(NT_UNARY_POSTINC, "++");
    unary_postinc->addChild(primary);

    return unary_postinc;
}
Node* Parser::parse_unary_predec(){
    // unary_predec	::= "--" primary
    if(!match(DECREMENT)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after prefix decrement");

    Node* unary_predec = new Node(NT_UNARY_PREDEC, "--");
    unary_predec->addChild(primary);

    return unary_predec;
}
Node* Parser::parse_unary_preinc(){
    // unary_preinc	::= "++" primary
    if(!match(INCREMENT)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after prefix increment");

    Node* unary_preinc = new Node(NT_UNARY_PREINC, "++");
    unary_preinc->addChild(primary);

    return unary_preinc;
}
Node* Parser::parse_unary_deref(){
    // unary_deref	::= '*' primary

    if(!match(STAR)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after dereference operator");

    Node* unary_deref = new Node(NT_UNARY_DEREF, "*");
    unary_deref->addChild(primary);

    return unary_deref;
}
Node* Parser::parse_unary_ref(){
    // unary_ref	::= '&' primary

    if(!match(BIT_AND)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after reference operator");

    Node* unary_ref = new Node(NT_UNARY_REF);
    unary_ref->addChild(primary);

    return unary_ref;
}
Node* Parser::parse_unary_lnot(){
    // unary_lnot	::= '!' primary

    if(!match(NOT)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after logical not");

    Node* unary_lnot = new Node(NT_UNARY_LNOT);
    unary_lnot->addChild(primary);

    return unary_lnot;
}
Node* Parser::parse_unary_2scompl(){
    // unary-2scompl	::= '~' primary

    if(!match(BIT_NOT)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after '~' operator");

    Node* unary_2scompl = new Node(NT_2SCOMPL);
    unary_2scompl->addChild(primary);

    return unary_2scompl;
}
Node* Parser::parse_unary_1scompl(){
    // unary-1scompl	::= '-' primary

    if(!match(MINUS)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after '-' operator");

    Node* unary_1scompl = new Node(NT_1SCOMPL);
    unary_1scompl->addChild(primary);

    return unary_1scompl;
}
Node* Parser::parse_unary_plus(){
    // unary_plus	::= '+' primary

    if(!match(PLUS)) return nullptr;

    Node* primary = parse_primary();
    if(!primary) error("Expected primary after '+' operator");

    Node* plus = new Node(NT_UNARY_PLUS);
    plus->addChild(primary);

    return plus;
}

// primary
Node* Parser::parse_primary(){
    /*
    primary		::=   prim_expr 
                    | prim_funcall 
                    | prim_literal
                    | prim_ident
    */
    Node* node = nullptr;

    if( (node = parse_prim_expr()) != nullptr ) return node;
    if( (node = parse_prim_funcall()) != nullptr ) return node;
    if( (node = parse_prim_literal()) != nullptr ) return node;
    if( (node = parse_prim_ident()) != nullptr ) return node;

    return nullptr;
}
Node* Parser::parse_prim_expr(){
    // prim_expr	::= '(' expression ')'
    if(!match(LPAREN)) return nullptr;

    Node* expr = parse_expression();
    expect(RPAREN);

    Node* prim_expr = new Node(NT_PRIM_EXPR);
    prim_expr->addChild(expr);

    return prim_expr;
}
Node* Parser::parse_prim_funcall(){
    // prim_funcall	::= identifier '(' expr_list ')'
    Node* ident;

    if( (ident = parse_identifier()) == nullptr ) return nullptr;
    if(!match(LPAREN)) return nullptr;

    Node* expr_list = parse_expr_list();
    expect(RPAREN);

    Node* prim_funcall = new Node(NT_PRIM_FUNCALL);
    prim_funcall->addChild(ident);
    prim_funcall->addChild(expr_list);

    return prim_funcall;
}
Node* Parser::parse_prim_literal(){
    // prim_literal	::= const_literal

    Node* const_literal = parse_const_literal();
    if(!const_literal) return nullptr;

    Node* prim_literal = new Node(NT_PRIM_LITERAL);
    prim_literal->addChild(const_literal);

    return prim_literal;
}
Node* Parser::parse_prim_ident(){
    /*
    prim_ident	::= long_index_opt
		          | index_opt
    */

    Node* node = nullptr;

    if( (node = parse_long_index_opt()) != nullptr ) return node;
    if( (node = parse_index_opt()) != nullptr ) return node;


    return nullptr;
}

// types
Node* Parser::parse_tyyid_pair_list(){
    // tyyid_pair_list	::= tyyid_pair { ',' tyyid_pair }

    Node* first = parse_tyyid_pair();
    if(!first) return nullptr;

    Node* list = new Node(NT_TYYID_PAIR_LIST);
    list->addChild(first);

    while(match(COMMA)){
        Node* next = parse_tyyid_pair_list();
        if(!next) error("Expected type-identifier pair after ','");
        list->addChild(next);
    }

    return list;
}
Node* Parser::parse_tyyid_pair(){
    // tyyid_pair	::= tyy_decl [ kw_const ] [ '*' ] identifier

    Node* tyy_decl = parse_tyy_decl();
    if(!tyy_decl) return nullptr;

    
    Node* tyyid_pair = new Node(NT_TYYID_PAIR);

    if(match_kw(KEYWORD_CONST)){
        tyyid_pair->addChild(new Node(NT_KW_CONST, "const"));
    }

    if(match(STAR)){
        tyyid_pair->addChild(new Node(NT_STAR, "*"));
    }

    Node* ident = parse_identifier();
    tyy_decl->addChild(ident);

    return tyyid_pair;
}
Node* Parser::parse_tyy_lit(){
    /*
    tyy_lit		::= tyy_enum_lit
		          | tyy_ext_lit
    */


    Node* tyy_list = nullptr;

    if( (tyy_list = parse_tyy_enum_lit()) != nullptr ) return tyy_list;
    if( (tyy_list = parse_tyy_ext_lit()) != nullptr ) return tyy_list;

    return nullptr;
}
Node* Parser::parse_tyy_enum_lit(){
    // tyy_enum_lit	::= '{' tyy_enum_field { ',' tyy_enum_field } '}'

    if(!match(LBRACE)) return nullptr;

    Node* tyy_enum_lit = new Node(NT_TYY_ENUM_LIT);

    Node* tyy_enum_field = parse_tyy_enum_field();
    tyy_enum_lit->addChild(tyy_enum_field);

    while(match(COMMA)){
        Node* next = parse_tyy_enum_field();
        if(!next){
            error("Expected enum field after ',' for enum literal");
        }
        tyy_enum_lit->addChild(next);
    }

    return tyy_enum_lit;
}
Node* Parser::parse_tyy_enum_field(){
    // tyy_enum_field	::= identifier [ '=' int_const ]

    size_t backup = pos;
    Node* ident = parse_identifier();
    if(!ident){
        pos = backup;
        return nullptr;
    }
    
    Node* tyy_enum_field = new Node(NT_TYY_ENUM_FIELD);
    tyy_enum_field->addChild(ident);

    if(match(ASSIGN)){
        Node* int_const = parse_int_const();
        if(!int_const) error("Expected int constant after '=' for enum field");
        tyy_enum_field->addChild(int_const);
    }

    return tyy_enum_field;
}

Node* Parser::parse_tyy_ext_lit(){
    // tyy_ext_lit	::= '{' tyy_ext_field { ';' tyy_ext_field } '}'

    if(!match(LBRACE)) return nullptr;

    size_t backup = pos;
    Node* tyy_ext_field = parse_tyy_ext_field();
    if(!tyy_ext_field){
        pos = backup;
        return nullptr;
    }

    Node* tyy_ext_lit = new Node(NT_TYY_EXT_LIT);
    tyy_ext_lit->addChild(tyy_ext_field);

    while(match(SEMICOLON)){
        Node* next = parse_tyy_ext_field();
        if(!next){
            error("Expected type-identifier ext field after semicolon for type-identifier ext literal");
        }
        tyy_ext_lit->addChild(next);
    }


    return tyy_ext_lit;
}
Node* Parser::parse_tyy_ext_field(){
    // tyy_ext_field	::= tyy_decl identifier

    size_t backup = pos;
    Node* decl = parse_tyy_decl();
    if(!decl){
        pos = backup;
        return nullptr;
    }

    Node* ident = parse_identifier();
    if(!ident){
        error("Expected identifier for type identifier ext field");
    }

    Node* tyy_ext_field = new Node(NT_TYY_EXT_FIELD);
    tyy_ext_field->addChild(decl);
    tyy_ext_field->addChild(ident);

    return tyy_ext_field;
}
Node* Parser::parse_tyy_decl(){
    // tyy_decl	::= [ tyy_storage ] [ tyy_qualifier ] [ '*' ] tyy_body

    size_t backup = pos;
    Node* tyy_decl = new Node(NT_TYY_DECL);

    Node* tyy_storage = parse_tyy_storage();
    if(tyy_storage){
        tyy_decl->addChild(tyy_storage);
    }

    Node* tyy_qualifier = parse_tyy_qualifier();
    if(tyy_qualifier){
        tyy_decl->addChild(tyy_qualifier);
    }

    if(match(STAR)){
        tyy_decl->addChild(new Node(NT_STAR, "*"));
    }

    Node* tyy_body = parse_tyy_body();
    if(!tyy_body){
        pos = backup;
        return nullptr;
    }
    tyy_decl->addChild(tyy_body);

    return tyy_decl;
}
Node* Parser::parse_tyy_defn(){
    // tyy_defn	::= kw_typedef tyy_body tyy_alias


    if(!match_kw(KEYWORD_TYPEDEF)) return nullptr;

    Node* tyy_body = parse_tyy_body();
    if(!tyy_body) {
        error("Expected body for type definition");
    }

    Node* tyy_alias = parse_tyy_alias();
    if(!tyy_alias){
        error("Expected alias for type definition");
    }

    Node* tyy_defn = new Node(NT_TYY_DEFN);
    tyy_defn->addChild(tyy_body);
    tyy_defn->addChild(tyy_alias);

    return tyy_defn;
}
Node* Parser::parse_tyy_storage(){
    /*
    tyy_storage	::= kw_auto
                  | kw_static
                  | kw_extern
                  | kw_register
    */

    if(match_kw(KEYWORD_AUTO)) return new Node(NT_KW_AUTO, "auto");
    if(match_kw(KEYWORD_STATIC)) return new Node(NT_KW_STATIC, "static");
    if(match_kw(KEYWORD_EXTERN)) return new Node(NT_KW_EXTERN, "extern");
    if(match_kw(KEYWORD_REGISTER)) return new Node(NT_KW_REGISTER, "register");

    
    return nullptr;
}
Node* Parser::parse_tyy_qualifier(){
    /*
    tyy_qualifier	::= kw_const
                      | kw_volatile
                      | kw_restrict
    */
    if(match_kw(KEYWORD_CONST)) return new Node(NT_KW_CONST, "const");
    if(match_kw(KEYWORD_VOLATILE)) return new Node(NT_KW_VOLATILE, "volatile");
    if(match_kw(KEYWORD_RESTRICT)) return new Node(NT_KW_RESTRICT, "restrict");
    
    return nullptr;
}
Node* Parser::parse_tyy_cast(){
    // tyy_cast	::= '(' tyy_ref ')'
    if(!match(LPAREN)) return nullptr;

    size_t backup = pos;
    Node* tyy_ref = parse_tyy_ref();

    if(!tyy_ref){
        pos = backup;
        return nullptr;
    }

    expect(RPAREN);

    Node* tyy_cast = new Node(NT_TYY_CAST);
    tyy_cast->addChild(tyy_ref);

    return tyy_cast;
}
Node* Parser::parse_tyy_ref(){
    // tyy_ref		::= tyy_body [ '*' ]

    size_t backup = pos;
    Node* tyy_body = parse_tyy_body();

    if(!tyy_body){
        pos = backup;
        return nullptr;
    }

    Node* tyy_ref = new Node(NT_TYY_REF);
    tyy_ref->addChild(tyy_body);

    if(match(STAR)){
        tyy_ref->addChild(new Node(NT_STAR, "*"));
    }

    return tyy_ref;
}
Node* Parser::parse_tyy_body(){
    /*
    tyy_body	::= tyy_base
                  | tyy_ext
                  | tyy_lit
    */
    Node* tyy_body = nullptr;

    if( (tyy_body = parse_tyy_base()) != nullptr ) return tyy_body;
    if( (tyy_body = parse_tyy_ext()) != nullptr ) return tyy_body;
    if( (tyy_body = parse_tyy_lit()) != nullptr ) return tyy_body;

    return nullptr;
}
Node* Parser::parse_tyy_ext(){
    /*
    tyy_ext		::= tyy_ext_union
		          | tyy_ext_enum
		          | tyy_ext_struct
    */
    Node* tyy_ext = nullptr;

    if( (tyy_ext = parse_tyy_ext_union()) != nullptr ) return tyy_ext;
    if( (tyy_ext = parse_tyy_ext_enum()) != nullptr ) return tyy_ext;
    if( (tyy_ext = parse_tyy_ext_struct()) != nullptr ) return tyy_ext;

    return nullptr;
}
Node* Parser::parse_tyy_ext_union(){
    // tyy_ext_union	::= kw_union tyy_alias

    if(!match_kw(KEYWORD_UNION)) return nullptr;

    Node* alias = parse_tyy_alias();
    if(!alias){
        error("Expected alias for union");
    }

    Node* tyy_ext_union = new Node(NT_TYY_EXT_UNION);
    tyy_ext_union->addChild(alias);

    return tyy_ext_union;
}
Node* Parser::parse_tyy_ext_enum(){
    // tyy_ext_enum	::= kw_enum tyy_alias

    if(!match_kw(KEYWORD_ENUM)) return nullptr;

    Node* alias = parse_tyy_alias();
    if(!alias){
        error("Expected alias for enum");
    }

    Node* tyy_ext_enum = new Node(NT_TYY_EXT_ENUM);
    tyy_ext_enum->addChild(alias);

    return tyy_ext_enum;
}
Node* Parser::parse_tyy_ext_struct(){
    // tyy_ext_struct	::= kw_struct tyy_alias

    if(!match_kw(KEYWORD_STRUCT)) return nullptr;

    Node* alias = parse_tyy_alias();

    if(!alias){
        error("Expected alias for ext struct");
    }

    Node* tyy_ext_struct = new Node(NT_TYY_EXT_STRUCT);
    tyy_ext_struct->addChild(alias);

    return tyy_ext_struct;
}
Node* Parser::parse_tyy_alias(){
    // tyy_alias	::= identifier

    size_t backup = pos;
    Node* ident = parse_identifier();

    if(!ident){
        pos = backup;
        return nullptr;
    }

    Node* tyy_alias = new Node(NT_TYY_ALIAS);
    tyy_alias->addChild(ident);

    return tyy_alias;
}
Node* Parser::parse_tyy_base(){
    // tyy_base	::= [ tyy_base_word ] [ tyy_base_sign ] tyy_base_body

    size_t backup = pos;

    
    Node* tyy_base_word = parse_tyy_base_word();
    if(!tyy_base_word){
        pos = backup;
    }
    backup = pos;
    
    Node* tyy_base_sign = parse_tyy_base_sign();
    if(!tyy_base_sign){
        pos = backup;
    }
    backup = pos;
    
    Node* tyy_base_body = parse_tyy_base_body(); 
    if(!tyy_base_body){
        error("Expected base body for type identifier base");
    }

    
    Node* tyy_base = new Node(NT_TYY_BASE);
    if(tyy_base_word) tyy_base->addChild(tyy_base_word);
    if(tyy_base_sign) tyy_base->addChild(tyy_base_sign);
    tyy_base->addChild(tyy_base_body);

    return tyy_base;
}
Node* Parser::parse_tyy_base_word(){
    /*
    tyy_base_word	::= kw_long
		              | kw_short
    */


    Node* tyy_base_word = nullptr;

    if( (match_kw(KEYWORD_LONG)) ) return new Node(NT_KW_LONG, "long");
    if( (match_kw(KEYWORD_SHORT)) ) return new Node(NT_KW_SHORT, "short");

    return nullptr;
}
Node* Parser::parse_tyy_base_sign(){
    /*
    tyy_base_sign 	::= kw_signed
		              | kw_unsigned
    */
    Node* tyy_base_sign = nullptr;

    if( (match_kw(KEYWORD_SIGNED)) ) return new Node(NT_KW_SIGNED, "signed");
    if( (match_kw(KEYWORD_UNSIGNED)) ) return new Node(NT_KW_UNSIGNED, "unsigned");

    return nullptr;
}
Node* Parser::parse_tyy_base_body(){
    /*
    tyy_base_body	::= kw_char
		              | kw_int
		              | kw_float
		              | kw_double
		              | kw_void
    */


    Node* tyy_base_body = nullptr;

    if( (match_kw(KEYWORD_CHAR)) ) return new Node(NT_KW_CHAR, "char");
    if( (match_kw(KEYWORD_INT)) ) return new Node(NT_KW_INT, "int");
    if( (match_kw(KEYWORD_FLOAT)) ) return new Node(NT_KW_FLOAT, "float");
    if( (match_kw(KEYWORD_DOUBLE)) ) return new Node(NT_KW_DOUBLE, "double");
    if( (match_kw(KEYWORD_VOID)) ) return new Node(NT_KW_VOID, "void");

    return nullptr;
}

// Idents
Node* Parser::parse_long_index_opt(){
    // long_index_opt	::= long_ident [ index ]

    size_t backup = pos;
    Node* long_ident = parse_long_ident();
    if(!long_ident){
        pos = backup;
        return nullptr;
    }

    Node* long_index_opt = new Node(NT_LONG_INDEX_OPT);
    long_index_opt->addChild(long_ident);

    backup = pos;
    Node* index = parse_index();
    if(index){
        long_index_opt->addChild(index);
    }

    return long_index_opt;
}
Node* Parser::parse_index_opt(){
    // index_opt	::= identifier [ index ]

    size_t backup = pos;
    Node* ident = parse_identifier();
    if(!ident){
        pos = backup;
        return nullptr;
    }
    Node* index_opt = new Node(NT_INDEX_OPT);
    index_opt->addChild(ident);

    backup = pos;
    Node* index = parse_index();
    if(index){
        index_opt->addChild(index);
    }

    return index_opt;
}
Node* Parser::parse_index(){
    // index		::= '[' [ long_ident | const_literal ] ']'

    if(!match(LBRACKET)) return nullptr;

    Node* index = new Node(NT_INDEX);

    size_t backup = pos;

    Node* expr = parse_long_ident();
    if(!expr){
        pos = backup;
        expr = parse_const_literal();
    }

    if(expr){
        index->addChild(expr);
    }

    if(!match(RBRACKET)){
        delete index;
        error("Expected ']' at end of index expression");
    }

    return index;
}
Node* Parser::parse_long_ident(){
    // long_ident	::= identifier { ( dot_ident | arrow_ident ) }

    size_t backup = pos;
    Node* ident = parse_identifier();
    if(!ident){
        pos = backup;
        return nullptr;
    }

    Node* long_ident = new Node(NT_LONG_IDENT);
    long_ident->addChild(ident);

    while(!isAtEnd()){
        Node* dot = parse_dot_ident();
        if(dot){
            long_ident->addChild(dot);
            continue;
        }

        Node* arrow = parse_arrow_ident();
        if(arrow){
            long_ident->addChild(arrow);
            continue;
        }

        break;
    }

    return long_ident;
}
Node* Parser::parse_dot_ident(){
    // dot_ident	::= identifier { '.' identifier }

    size_t backup = pos;
    Node* identifier = parse_identifier();
    if(!identifier){
        pos = backup;
        return nullptr;
    }

    Node* dot_ident = new Node(NT_DOT_IDENT);
    dot_ident->addChild(identifier);

    while(match(DOT)){
        Node* next = parse_identifier();
        if(!next){
            error("Expected an identfier after '.'");
        }
        dot_ident->addChild(next);
    }

    return dot_ident;
}
Node* Parser::parse_arrow_ident(){
    // arrow_ident	::= identifier { "->" identifier }

    size_t backup = pos;
    Node* ident = parse_identifier();
    if(!ident){
        pos = backup;
        return nullptr;
    }

    Node* arrow_ident = new Node(NT_ARROW_IDENT);
    arrow_ident->addChild(ident);

    while(match(ARROW)){
        Node* next = parse_identifier();
        if(!next){
            error("Expected identifier after '->' ");
        }
        arrow_ident->addChild(next);
    }


    return arrow_ident;
}

// CompundLiteral
Node* Parser::parse_comp_literal(){
    // comp_literal	::= [ '(' tyy_decl ')' ] list_literal

    Node* comp_literal = new Node(NT_COMP_LITERAL);
    if(match(LPAREN)){
        Node* tyy_decl = parse_tyy_decl();
        if(!tyy_decl){
            error("Expected type identfier declaration after '(' for compound literal");
        }
        comp_literal->addChild(tyy_decl);
        expect(RPAREN);
    }

    Node* list_literal = parse_list_literal();
    if(!list_literal){
        error("Expected a list-literal for compound literal");
    }
    
    comp_literal->addChild(list_literal);

    return comp_literal;
}
Node* Parser::parse_list_literal() {
    // list_literal ::= '{' designated_init { ',' designated_init } '}'
    if (!match(LBRACE)) return nullptr;

    Node* list_literal = new Node(NT_LIST_LITERAL);

    if (peek().type != RBRACE) { // allow empty list: {}
        Node* designated_init = parse_designated_init();
        if (!designated_init) {
            error("Expected designated initializer inside list literal");
        }

        list_literal->addChild(designated_init);

        while (match(COMMA)) {
            Node* next = parse_designated_init();
            if (!next) {
                error("Expected designated initializer after ',' in list literal");
            }
            list_literal->addChild(next);
        }
    }

    expect(RBRACE);
    return list_literal;
}

Node* Parser::parse_designated_init(){
    // designated_init ::= [ '.' identifier '=' ] const_literal

    Node* designated_init = new Node(NT_DESIGNATED_INIT);

    if(match(DOT)){
        Node* ident = parse_identifier();
        if(!ident){
            error("Expected identfier after '.' for designated init");
        }
        if(!match(ASSIGN)){
            error("Expected '=' after identifier for designated init");
        }
        designated_init->addChild(ident);
    }

    size_t backup = pos;
    Node* const_literal = parse_const_literal();
    if(!const_literal){
        pos = backup;
        return nullptr;
    }

    designated_init->addChild(const_literal);

    return designated_init;
}


// LiteralTokens
Node* Parser::parse_const_literal(){
    // const_literal	::= expr_const | str_const | char_const | num_const | int_const | null_const

    Node* const_literal = nullptr;

    if( (const_literal = parse_expr_const()) != nullptr ) return const_literal;
    if( (const_literal = parse_str_const()) != nullptr ) return const_literal;
    if( (const_literal = parse_char_const()) != nullptr ) return const_literal;
    if( (const_literal = parse_num_const()) != nullptr ) return const_literal;
    if( (const_literal = parse_int_const()) != nullptr ) return const_literal;
    if( (const_literal = parse_null_const()) != nullptr ) return const_literal;

    return nullptr;
}
Node* Parser::parse_null_const(){
    // null_const	::= "NULL"
    if(peek().value == "NULL") return new Node(NT_NULL_CONST, "NULL");
}
Node* Parser::parse_expr_const(){
    // expr_const	::= int_const { ( '+' | '-' | '*' | '%' | '/' | '&' | '|' ) int_const }

    Node* left = parse_int_const();
    if(!left) return nullptr;

    Node* expr = left;

    while(!isAtEnd()){
        TokenType t = peek().type;

        if(
            t == PLUS ||
            t == MINUS ||
            t == STAR ||
            t == SLASH ||
            t == PERCENT ||
            t == BIT_AND ||
            t == BIT_OR
        ){
            Token opTok = advance();
            Node* right = parse_int_const();

            if(!right) error("Expected integer constant after operator: " + opTok.value);

            Node* binOp = new Node(NT_EXPR_CONST, opTok.value);
            binOp->addChild(expr);
            binOp->addChild(right);
            expr = binOp;
        }else{
            break;
        }

    }
    return expr;
}
Node* Parser::parse_str_const(){
    // str_const	::= [ 'L' ] '"' { character } '"'

    Node* str_const = new Node(NT_STR_CONST);
    if(peek().value == "L"){
        advance();
        str_const->addChild(new Node(NT_CHARACTER, "L"));
    }

    size_t backup = pos;
    if(peek().type == TokenType::STRING_LITERAL){
        str_const->addChild(new Node(NT_STRING_LITERAL, advance().value));
    }else{
        pos = backup;
        return nullptr;
    }

    return str_const;
}
Node* Parser::parse_char_const(){

    // char_const	::= "'" character  "'"

    if(peek().type != CHAR_LITERAL) return nullptr;
    
    return new Node(NT_CHARACTER, advance().value);
}
Node* Parser::parse_num_const(){
    // num_const	::= integer | rational

    if(peek().type == TokenType::INTEGER_LITERAL) return new Node(NT_NULL_CONST, advance().value);
    if(peek().type == TokenType::FLOAT_LITERAL) return new Node(NT_NULL_CONST, advance().value);
    
    return nullptr;
}
Node* Parser::parse_float_const(){
    // float_const	::= float

    if(peek().type == TokenType::FLOAT_LITERAL) return new Node(NT_FLOAT_CONST, advance().value);
    
    return nullptr;
}
Node* Parser::parse_int_const(){
    // int_const	::= integer | char_const

    if(peek().type == TokenType::INTEGER_LITERAL) return new Node(NT_INT_CONST, advance().value);
    if(peek().type == TokenType::CHAR_LITERAL) new Node(NT_INT_CONST, std::to_string(static_cast<int>(advance().value[0])));
    
    return nullptr;
}
Node* Parser::parse_rational(){
    // rational	::= [ intenger ] '.' integer
    Node* rational = new Node(NT_RATIONAL);

    if(match(TokenType::INTEGER_LITERAL)){
        Node* first = parse_integer();
        if(first) rational->addChild(first);
    }

    if(!match(DOT)) return nullptr;


    Node* second = parse_integer();
    if(!second){
        error("Expected another integer after '.' for rational");
    }

    rational->addChild(second);
    

    return rational;
}
Node* Parser::parse_integer(){
    // integer		::= dec_integer | hex_integer | oct_integer | bin_integer

    Node* integer = nullptr;
    
    if( (integer = parse_dec_integer()) != nullptr ) return integer;
    if( (integer = parse_hex_integer()) != nullptr ) return integer;
    if( (integer = parse_oct_integer()) != nullptr ) return integer;
    if( (integer = parse_bin_integer()) != nullptr ) return integer;

    return nullptr;
}

Node* Parser::parse_bin_integer(){
    // bin_integer	::= ( "0b" | "0B" ) bin_digit { bin_digit }

    Node* bin_integer = new Node(NT_BIN_INTEGER);

    

    return bin_integer;
}

Node* Parser::parse_oct_integer(){
    Node* oct_integer = new Node(NT_OCT_INTEGER);
    return oct_integer;
}
Node* Parser::parse_hex_integer(){
    Node* hex_integer = new Node(NT_HEX_INTEGER);
    return hex_integer;
}
Node* Parser::parse_dec_integer(){
    Node* dec_integer = new Node(NT_DEC_INTEGER);
    return dec_integer;
}
Node* Parser::parse_identifier(){
    Node* identifier = new Node(NT_IDENTIFIER);
    return identifier;
}
Node* Parser::parse_letter(){
    Node* letter = new Node(NT_LETTER);
    return letter;
}
Node* Parser::parse_lower_case(){
    Node* lower_case = new Node(NT_LOWER_CASE);
    return lower_case;
}
Node* Parser::parse_upper_case(){
    Node* upper_case = new Node(NT_UPPER_CASE);
    return upper_case;
}
Node* Parser::parse_hex_digit(){
    Node* hex_digit = new Node(NT_HEX_DIGIT);
    return hex_digit;
}
Node* Parser::parse_digit(){
    Node* digit = new Node(NT_DIGIT);
    return digit;
}
Node* Parser::parse_oct_digit(){
    Node* oct_digit = new Node(NT_OCT_DIGIT);
    return oct_digit;
}
Node* Parser::parse_bin_digit(){
    Node* bin_digit = new Node(NT_BIN_DIGIT);
    return bin_digit;
}
Node* Parser::parse_character(){
    Node* character = new Node(NT_CHARACTER);
    return character;
}
Node* Parser::parse_printable(){
    Node* printable = new Node(NT_PRINTABLE);
    return printable;
}
Node* Parser::parse_char_escape(){
    Node* char_escape = new Node(NT_CHAR_ESCAPE);
    return char_escape;
}
Node* Parser::parse_hex_escape(){
    Node* hex_escape = new Node(NT_HEX_ESCAPE);
    return hex_escape;
}
Node* Parser::parse_oct_escape(){
    Node* oct_escape = new Node(NT_OCT_ESCAPE);
    return oct_escape;
}
Node* Parser::parse_escapble(){
    Node* escapble = new Node(NT_ESCAPBLE);
    return escapble;
}
Node* Parser::parse_comment(){
    Node* comment = new Node(NT_COMMENT);
    return comment;
}