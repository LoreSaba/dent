#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define TAB_SIZE 8

/* Rapresent all the possible type of tokens. */
typedef enum {
	TOKEN_LCURLY_BRACKET, TOKEN_RCURLY_BRACKET, 
	TOKEN_LSQUARE_BRACKET, TOKEN_RSQUARE_BRACKET,
	TOKEN_COLON, TOKEN_COMMA,

	TOKEN_STRING, TOKEN_NUMBER,

	TOKEN_WHITESPACE, TOKEN_NEWLINE,

	TOKEN_IDENTIFIER,

	// keyword 
	TOKEN_TRUE, TOKEN_FALSE, TOKEN_NULL,

	TOKEN_EOF, TOKEN_ERROR
} TokenType;

/* Represent a single token that identifies a unit of meaning in a programming
 * language. */
typedef struct {
	TokenType type;
	const char *start;
	int len;
	int lineno;
} Token;

/* Represent a dynamic array of tokens. */
typedef struct {
	int count;
	int capacity;
	Token *tokens;
} TokenArray;

/* Represent the component that get in input the content of a source file and 
 * produce in output the tokens. */
typedef struct {
	const char *start;
	const char *curr;
	int lineno;
} Scanner;

/* Represent a keyword-tokentype entry in a table. */
typedef struct {
	const char *name;
	TokenType type;
} ktEntry;

/* Sorted list of language keyword with their corresponding token type. */
ktEntry keyword2Token[] = {
	{"false", TOKEN_FALSE},
	{"null", TOKEN_NULL},
	{"true", TOKEN_TRUE},
};

int keywordCount = sizeof(keyword2Token) / sizeof(keyword2Token[0]);


/* ========== Scanner ========== */

Scanner scanner;

/* Initialize the scanner fields. */
void initScanner(const char *source) {
	scanner.start  = source;
	scanner.curr   = source;
	scanner.lineno = 1;
}

/* Return true if the scanner reaches the EOF, false otherwise. */
bool isAtEnd(void) {
	return *scanner.curr == '\0';
}

/* Return the current character pointed by the scanner. */
char peek(void) {
	return *scanner.curr;
}

/* Return the next character pointed by the scanner. */
char peekNext(void) {
	if (isAtEnd()) return '\0';
	return scanner.curr[1];
}

/* Return the next character at distance i pointed by the scanner. */
char peekAhead(int i) {
	if (scanner.curr[i] == '\0') return '\0';
	return scanner.curr[i];
}

/* Return and increment the current character pointed by the scanner. */
char advanceScan(void) {
	char c = *scanner.curr;
	scanner.curr++;
	return c;
}

/* Return true if c is a digit, false otherwise. */
bool isDigit(char c) {
	return c >= '0' && c <= '9';
}

/* Return true if c is an alphabet letter, false otherwise. */
bool isAlpha(char c) {
	return (c >= 'a' && c <= 'z') ||
	       (c >= 'A' && c <= 'Z') ||
		c == '_';
}

/* Initialize and allocate memory for the dinamic array of tokens. */
void initTokenArray(TokenArray *a) {
	a->count = 0;
	a->capacity = 8;
	a->tokens = malloc(sizeof(Token) * a->capacity);
}

/* Add a token t into token array a. */
void addToken(TokenArray *a, Token t) {
	if (a->count >= a->capacity) {
		a->capacity *= 2;
		a->tokens = realloc(a->tokens, sizeof(Token) * a->capacity);
	}
	a->tokens[a->count++] = t;
}

/* Create a token with the specified token type. */
Token makeToken(TokenType type) {
	Token token;

	token.type   = type;
	token.start  = scanner.start;
	token.len    = (int)(scanner.curr - scanner.start);
	token.lineno = scanner.lineno;

	return token;
}

/* Create an error token with the specified input message. */
Token makeErrorToken(const char *message) {
	Token token;

	token.type   = TOKEN_ERROR;
	token.start  = message;
	token.len    = (int)strlen(message);
	token.lineno = scanner.lineno;

	return token;
}

/* Create a whitespace token. */
Token whitespace(void) {
	while (!isAtEnd()) {
		char c = peek();
		switch(c) {
		case ' ':
		case '\r':
		case '\t':
			advanceScan();
			break;
		default: 
			return makeToken(TOKEN_WHITESPACE);
		}
	}
	return makeToken(TOKEN_WHITESPACE);
}


/* Create a string token. */
Token string(void) {
	while(peek() != '"' && !isAtEnd()) {
		if (peek() == '\n') scanner.lineno++;
		advanceScan();
	}

	if (isAtEnd()) return makeErrorToken("Unterminated string");

	advanceScan(); /* skip the closed "*/
	return makeToken(TOKEN_STRING);
}

/* Create a number token. */
Token number(void) {
	while(isDigit(peek())) advanceScan();

	if (peek() == '.' && isDigit(peekNext())) { 
		/* fractional part */
	       	advanceScan();
		while(isDigit(peek())) advanceScan();
	}

	if ((peek() == 'e' || peek() == 'E') && 
	    (isDigit(peekNext()) ||
	    ((peekNext() == '-' || peekNext() == '+') && isDigit(peekAhead(2))))
	   ) { 
		/* exponent part */
		advanceScan();
		if (peek() == '-' || peek() == '+') advanceScan();
		while(isDigit(peek())) advanceScan();
	}

	return makeToken(TOKEN_NUMBER);
}

/* Return the value of the memory compare between the keyword k and the 
 * string s with length len. 
 * Return <0 if k < s, 0 if k == s, >0 if k > s.
 * Note that in memcmp we pass the minimum value of the length between k
 * and s. So we must check, after this operation, that the two strings
 * have equal lengths. Otherwise string does not match exactly the keyword. */
int keywordCmp(const char *k, const char *s, size_t slen) {
	size_t klen = strlen(k);
	size_t minlen = (slen < klen) ? slen : klen;

	int cmp = memcmp(k, s, minlen);

	if (cmp != 0) return cmp;
	
	if (klen < slen) return -1;
	if (klen > slen) return  1;
	return 0;
}

/* Return the index of the keyword in keyword2Token that match the string s
 * with length len if any. Otherwise return -1. */
int searchKeyword(const char *s, size_t len) {
	int i = 0;
	int j = keywordCount-1;

	while(i <= j) {
		int m = i + (j - i) / 2;
		int cmp = keywordCmp(keyword2Token[m].name, s, len);
		if (cmp == 0) return m;
		if (cmp < 0)  i = m + 1;
		else          j = m - 1;
	}

	return -1;
}

/* Return the token type of the identifer pointed by the scanner
 * [scanner.start, scanner.curr]. 
 * The identifier type can be a keyword of a user defined identifier. 
 * So the function first check if there is a keyword in the language
 * with the exact same characters, otherwise it's an identifer. */
TokenType identifierType(void) {
	size_t len = (size_t)(scanner.curr - scanner.start);
	int pos = searchKeyword(scanner.start, len);
	if (pos == -1) return TOKEN_IDENTIFIER;

	return keyword2Token[pos].type;
}

/* Create an identifier token. */
Token identifier(void) {
	while (isAlpha(peek()) || isDigit(peek()))
		advanceScan();
	return makeToken(identifierType());
}

/* Return the next token pointed by the scanner. */
Token scanToken(void) {
	scanner.start = scanner.curr;

	if (isAtEnd()) return makeToken(TOKEN_EOF);

	char c = advanceScan();

	if (isAlpha(c)) return identifier();
	if (isDigit(c)) return number();
	if (c == '-' && isDigit(peek())) {
		advanceScan();
		return number();
	}

	switch(c) {
	case '{': return makeToken(TOKEN_LCURLY_BRACKET);
	case '}': return makeToken(TOKEN_RCURLY_BRACKET);
	case '[': return makeToken(TOKEN_LSQUARE_BRACKET);
	case ']': return makeToken(TOKEN_RSQUARE_BRACKET);
	case ':': return makeToken(TOKEN_COLON);
	case ',': return makeToken(TOKEN_COMMA);
	case '"': return string();
	case ' ':
	case '\r':
	case '\t':
		  return whitespace();
	case '\n':
		  scanner.lineno++;
		  return makeToken(TOKEN_NEWLINE);
	}

	return makeErrorToken("Unexpected character");
}

/* Return the token list generated from the starting source of the scanner. */
TokenArray scanTokens(void) {
	TokenArray a;
	initTokenArray(&a);

	while (1) {
		Token t = scanToken();
		addToken(&a, t);
		if (t.type == TOKEN_EOF) return a;
	}
}

/* Print token to the stdin. */
void printToken(Token token) {
	/* Print the line number
	static int lineno = 0;

	if (token.lineno > lineno) {
		printf("%d: ", token.lineno);
		lineno = token.lineno;
	} else
		printf(" | ");
	*/
		
	switch(token.type) {
	case TOKEN_LCURLY_BRACKET:
		printf("%s ", "TOKEN_LCURLY_BRACKET");	
		break;
	case TOKEN_RCURLY_BRACKET:
		printf("%s ", "TOKEN_RCURLY_BRACKET");
		break;
	case TOKEN_LSQUARE_BRACKET:
		printf("%s ", "TOKEN_LSQUARE_BRACKET");	
		break;
	case TOKEN_RSQUARE_BRACKET:
		printf("%s ", "TOKEN_RSQUARE_BRACKET");	
		break;
	case TOKEN_COLON:	printf("%s ", "TOKEN_COLON");	break;
	case TOKEN_COMMA: 	printf("%s ", "TOKEN_COMMA");	break;
	case TOKEN_STRING:  	printf("%s ", "TOKEN_STRING");	break;
	case TOKEN_NUMBER: 	printf("%s ", "TOKEN_NUMBER");	break;
	case TOKEN_WHITESPACE:	printf("%s ", "TOKEN_WHITESPACE");break;
	case TOKEN_NEWLINE:	printf("%s ", "TOKEN_NEWLINE");	break;
	case TOKEN_IDENTIFIER: 	printf("%s ", "TOKEN_IDENTIFIER");break;
	case TOKEN_TRUE: 	printf("%s ", "TOKEN_TRUE");	break;
	case TOKEN_FALSE: 	printf("%s ", "TOKEN_FALSE");	break;
	case TOKEN_NULL: 	printf("%s ", "TOKEN_NULL");	break;
	case TOKEN_EOF:  	printf("%s ", "TOKEN_EOF");	break;
	case TOKEN_ERROR:  	printf("%s ", "TOKEN_ERROR");	break;
	}

	putchar('`');
	fwrite(token.start, 1, token.len, stdout);
	putchar('`');
	printf(" %d\n", token.len);
}

/* Print array of token to the stdin. */
void printTokens(TokenArray *a) {
	for (int i = 0; i < a->count; i++)
		printToken(a->tokens[i]);
}

/* ========== AST ========== */

/* Represent a concept in the most of programming languages. */
typedef enum {
	NODE_PROGRAM,

	NODE_BLOCK,
	NODE_ARRAY,
	NODE_KEYVALUE,

	NODE_LITERAL,
} NodeType;

typedef struct Node Node;

/* Dinamic array of nodes. */
typedef struct {
	int count;
	int capacity;
	Node **nodes;
} NodeArray;

typedef struct {
	Node  *key;
	Node  *value;	
} KeyValue;

/* Represent a token's role within a node. Classify how each token 
 * contributes to a syntax of a constuct. */
typedef enum {
	TOKEN_ROLE_DELIMITER_OPEN,	// { [ (
	TOKEN_ROLE_DELIMITER_CLOSE,	// } ] )
	TOKEN_ROLE_SEPARATOR,		// , ;
	TOKEN_ROLE_OPERATOR,		// : =
	TOKEN_ROLE_KEYWORD,
	TOKEN_ROLE_CONTENT		// child node
} TokenRole;

/* Dynamic array of tokens annotated with its syntatic role. */
typedef struct {
	int count;
	int capacity;
	Token **tokens;
	TokenRole *roles;
} TokenRoleArray;

/* Represent a single node of the AST. */
struct Node {
	NodeType type;

	union {
		NodeArray block;
		NodeArray array;
		KeyValue  keyvalue;
		Token     *literal;
	};

	TokenRoleArray allTokens;	
};

/* Initialize and allocate memory for the dinamic array of nodes. */
void initNodeArray(NodeArray *a) {
	a->count = 0;
	a->capacity = 8;
	a->nodes = malloc(sizeof(Node*) * a->capacity);
}

/* Add a node n into node array a. */
void addNode(NodeArray *a, Node *n) {
	if (a->count >= a->capacity) {
		a->capacity *= 2;
		a->nodes = realloc(a->nodes, sizeof(Node*) * a->capacity);
	}
	a->nodes[a->count++] = n;
}

/* Initialize and allocate memory for the dinamic array of token role. */
void initTokenRoleArray(TokenRoleArray *a) {
	a->count = 0;
	a->capacity = 8;
	a->tokens = malloc(sizeof(Token*) * a->capacity);
	a->roles  = malloc(sizeof(TokenRole) * a->capacity);
}

/* Add a token t and role r into the array a. */
void addTokenRole(TokenRoleArray *a, Token *t, TokenRole r) {
	if (a->count >= a->capacity) {
		a->capacity *= 2;
		a->tokens = realloc(a->tokens, sizeof(Token*) * a->capacity);
		a->roles = realloc(a->roles, sizeof(TokenRole) * a->capacity);
	}
	a->tokens[a->count] = t;
	a->roles[a->count] = r;
	a->count++;
}
/* Allocate and initialize a node with its type. */
Node *createNode(NodeType type) {
	Node *n = malloc(sizeof(Node));
	n->type = type;
	initTokenRoleArray(&n->allTokens);
	return n;
}

/* Create a program node. */
Node *createProgramNode(void) {
	Node *n = createNode(NODE_PROGRAM);
	initNodeArray(&n->block);
	return n;
}

/* Create a block node. */
Node *createBlockNode(void) {
	Node *n = createNode(NODE_BLOCK);
	initNodeArray(&n->block);
	return n;
}

/* Create an array node. */
Node *createArrayNode(void) {
	Node *n = createNode(NODE_ARRAY);
	initNodeArray(&n->array);
	return n;
}

/* Create a key value node. */
Node *createKeyvalueNode(Node *key, Node *value) {
	Node *n = createNode(NODE_KEYVALUE);
	n->keyvalue.key   = key;
	n->keyvalue.value = value;
	return n;
}

/* Create a literal node. */
Node *createLiteralNode(Token *token) {
	Node *n = createNode(NODE_LITERAL);
	n->literal = token;
	return n;
}

/* Free a node and all its children recursively. */
void freeNode(Node *n) {
	if (n == NULL) return;

	switch (n->type) {
	case NODE_PROGRAM:
	case NODE_BLOCK:
		for (int i = 0; i < n->block.count; i++)
			freeNode(n->block.nodes[i]);
		free(n->block.nodes);
		break;
	case NODE_ARRAY:
		for (int i = 0; i < n->array.count; i++)
			freeNode(n->array.nodes[i]);
		free(n->array.nodes);
		break;
	case NODE_KEYVALUE:
		freeNode(n->keyvalue.key);
		freeNode(n->keyvalue.value);
		break;
	case NODE_LITERAL:
	default:
		break;
	}

	free(n->allTokens.tokens);
	free(n->allTokens.roles);
	free(n);
}


/* ========== Parser ========== */

/* Represent the parser that generates the AST from the tokens using the
 * grammar rules of a language. */
typedef struct {
	Token* curr;
	Token* prev;
	bool hadErr;
	bool panicMode;
} Parser;

Parser parser;

/* Initialize the parser. */
void initParser(TokenArray *a) {
	parser.curr = a->tokens;
	parser.prev = a->tokens;
	parser.hadErr    = false;
	parser.panicMode = false;
}

/* Report an error at a given token. */
void errorAt(Token *token, const char *msg) {
	if (parser.panicMode) return;
	parser.panicMode = true;
	parser.hadErr    = true;

	fprintf(stderr, "[line %d] Error", token->lineno);

	if (token->type == TOKEN_EOF)
		fprintf(stderr, " at end");
	else if (token->type == TOKEN_ERROR) {

	} else
		fprintf(stderr, " at '%.*s'", token->len, token->start);

	fprintf(stderr, ": %s\n", msg);
}

/* Report an error at the previous token. */
void error(const char *msg) {
	errorAt(parser.prev, msg);
}

/* Report an error at the current token. */
void errorAtCurrent(const char *msg) {
	errorAt(parser.curr, msg);
}

/* Return the current token pointend by the parser. */
Token *peekToken(void) {
	return parser.curr;
}

/* Return the previous token pointend by the parser. */
Token *peekPrevToken(void) {
	return parser.prev;
}

/* Advance to the next token. */
void advancePars(void) {
	parser.prev = parser.curr;

	while (1) {
		parser.curr++;
		if (parser.curr->type == TOKEN_WHITESPACE ||
		    parser.curr->type == TOKEN_NEWLINE) continue;

		if (parser.curr->type != TOKEN_ERROR) break;

		errorAtCurrent(parser.curr->start);
	}
}

/* Check if the current token matches the expected type. */
bool check(TokenType type) {
	return parser.curr->type == type;
}

/* Return true if the current token matches the expected type.
 * Consume the current token if true. */
bool match(TokenType type) {
	if (!check(type)) return false;

	advancePars();
	return true;
}

/* Synchronize parser after an error by advancing to a safe point. */
void synchronize(void) {
	parser.panicMode = false;

	while (parser.curr->type != TOKEN_EOF) {
		/* Stop at tokens that start a new constructs. */
		switch (parser.curr->type) {
		case TOKEN_RCURLY_BRACKET:
		case TOKEN_RSQUARE_BRACKET:
		case TOKEN_COMMA:
			return;
		default:
			;
		}
		advancePars();
	}
}

/* Consume the current token and report an error if doesn't match. 
 * Returns true if consumed successfully, false otherwise. */
bool consume(TokenType type, const char *msg) {
	if (parser.curr->type == type) {
		advancePars();
		return true;
	}
	errorAtCurrent(msg);
	return false;
}

/* Grammar definition for json language:
 * Each rule is a name followed by a sequence of symbols.
 * Terminals are quoted strings or uppercase words.
 * Nonterminal are lowecase words which refer to another rule in the grammar.
 * | select one form a series of options.
 * * repeat zero or more times.
 * + repeat at least one.
 * ? select zero or one time.
 *
 * program	value EOF
 * value 	object | array | "true" | "false" | "null" | STRING | NUMBER
 * object	'{' (STRING ':' value (',' STRING ':' value)* )? '}'
 * array	'[' (value (',' value)* )? ']'
 */

Node *parseJsonValue(void);

/* Parse a JSON object.
 * object	 '{' (STRING ':' value (',' STRING ':' value)* )? '}' */
Node *parseJsonObject(void) {
	Node *obj = createBlockNode();

	Token *openBrace = parser.prev;
	addTokenRole(&obj->allTokens, openBrace, TOKEN_ROLE_DELIMITER_OPEN);

	// Empty object
	if (match(TOKEN_RCURLY_BRACKET)) {
		Token *closeBrace = parser.prev;
		addTokenRole(&obj->allTokens, closeBrace, 
			     TOKEN_ROLE_DELIMITER_CLOSE);
		return obj;
	}

	do {
		if (check(TOKEN_RCURLY_BRACKET)) break;

		if (!check(TOKEN_STRING)) {
			error("Expect property name");
			synchronize();
			continue;
		}

		// Parse key value
		Node *key = createLiteralNode(parser.curr);
		Token *keyToken = parser.curr;
		addTokenRole(&key->allTokens, keyToken, 
			     TOKEN_ROLE_CONTENT);
		advancePars();

		if (!consume(TOKEN_COLON, "Expect ':' after property name")) {
			freeNode(key);
			synchronize();
			continue;
		}

		Token *colonToken = parser.prev;
		Token *valueToken = parser.curr;

		Node *value = parseJsonValue();
		if (value == NULL) {
			freeNode(key);
			synchronize();
			continue;
		}


		Node *kv = createKeyvalueNode(key, value);
		addTokenRole(&kv->allTokens, keyToken, 
			     TOKEN_ROLE_CONTENT);
		addTokenRole(&kv->allTokens, colonToken, 
			     TOKEN_ROLE_OPERATOR);
		addTokenRole(&kv->allTokens, valueToken, 
			     TOKEN_ROLE_CONTENT);

		addNode(&obj->block, kv);
		addTokenRole(&obj->allTokens, keyToken, 
			     TOKEN_ROLE_CONTENT);

		if (check(TOKEN_COMMA)) {
			addTokenRole(&obj->allTokens, parser.curr, 
				     TOKEN_ROLE_SEPARATOR);
		}

	} while(match(TOKEN_COMMA));

	if (consume(TOKEN_RCURLY_BRACKET, 
	    "Expect '}' after object properties")) {
		addTokenRole(&obj->allTokens, parser.prev, 
			     TOKEN_ROLE_DELIMITER_CLOSE);
	}
	return obj;
}

/* Parse a JSON array.
 * array	'[' (value (',' value)* )? ']' */
Node *parseJsonArray(void) {
	Node *arr = createArrayNode();

	Token *openBracket = parser.prev;
	addTokenRole(&arr->allTokens, openBracket, 
		     TOKEN_ROLE_DELIMITER_OPEN);

	// Empty array
	if (match(TOKEN_RSQUARE_BRACKET)) {
		Token *closeBracket = parser.prev;
		addTokenRole(&arr->allTokens, closeBracket, 
			     TOKEN_ROLE_DELIMITER_CLOSE);
		return arr;
	}

	do {
		if (check(TOKEN_RSQUARE_BRACKET)) break;

		Token *elemToken = parser.curr;
		Node *e = parseJsonValue();
		if (e == NULL) {
			synchronize();
			continue;
		}

		addNode(&arr->array, e);
		addTokenRole(&arr->allTokens, elemToken, 
			     TOKEN_ROLE_CONTENT);

		if (check(TOKEN_COMMA)) {
			addTokenRole(&arr->allTokens, parser.curr, 
				     TOKEN_ROLE_SEPARATOR);
		}


	} while (match(TOKEN_COMMA));

	if (consume(TOKEN_RSQUARE_BRACKET, 
	    "Expect ']' after array elements")) {
		addTokenRole(&arr->allTokens, parser.prev, 
			     TOKEN_ROLE_DELIMITER_CLOSE);
	}

	return arr;
}

/* Parse a JSON value.
 * value 	object | array | "true" | "false" | "null" | STRING | NUMBER */
Node *parseJsonValue(void) {
	if (match(TOKEN_LCURLY_BRACKET))
		return parseJsonObject();
	if (match(TOKEN_LSQUARE_BRACKET))
		return parseJsonArray();
	if (match(TOKEN_TRUE) || match(TOKEN_FALSE) || match(TOKEN_NULL) ||
	    match(TOKEN_STRING) || match(TOKEN_NUMBER)) {
		Node *lit = createLiteralNode(parser.prev);
		addTokenRole(&lit->allTokens, parser.prev, 
			     TOKEN_ROLE_CONTENT);
		return lit;
	}

	error("Expect value");
	return NULL;
}

/* Parse a json program.
 * program	value EOF */
Node *parseProgram(void) {
	Node *prog = createProgramNode();
	
	Token *valueToken = parser.curr;
	Node  *value      = parseJsonValue();

	if (value != NULL) { 
		addNode(&prog->block, value);
		addTokenRole(&prog->allTokens, valueToken, 
			     TOKEN_ROLE_CONTENT);
	}

	if (consume(TOKEN_EOF, "Expected end of input")) {
		addTokenRole(&prog->allTokens, parser.prev, 
			     TOKEN_ROLE_DELIMITER_CLOSE);
	}
	return prog;
}

/* Return a node that represent the root of the AST. It is generated  using 
 * the recursive discent parsing method based on the grammar of the language. 
 * For this purpose it uses the parseProgram fuction as entry point. */
Node *parse(void) {
	// Skip the whitespaces
	while (check(TOKEN_WHITESPACE) || check(TOKEN_NEWLINE)) {
		advancePars();
	}

	return parseProgram();
}

/* ========== AST Printing ========== */

/* Print whitespace based on the provided depth. */
void printIndent(int depth) {
	for (int i = 0; i < depth; i++)
		printf("    ");
}

/* Recursively print nodes in the AST. */
void printAST(Node *n, int depth) {
	if (n == NULL) {
		printIndent(depth);
		printf("(null)\n");
		return;
	}

	printIndent(depth);

	switch (n->type) {
	case NODE_PROGRAM:
		printf("PROGRAM:\n");
		for (int i = 0; i < n->block.count; i++) {
			printAST(n->block.nodes[i], depth+1);
		}
		break;

	case NODE_BLOCK:
		printf("BLOCK (%d statement):\n", n->block.count);
		for (int i = 0; i < n->block.count; i++) {
			printAST(n->block.nodes[i], depth+1);
		}
		break;
	case NODE_ARRAY:
		printf("ARRAY (%d elements):\n", n->array.count);
		for (int i = 0; i < n->array.count; i++) {
			printAST(n->array.nodes[i], depth+1);
		}
		break;
	case NODE_KEYVALUE:
		printf("KEYVALUE:\n");
		printAST(n->keyvalue.key, depth+1);
		printAST(n->keyvalue.value, depth+1);
		break;
	case NODE_LITERAL:
		printf("LITERAL: ");
		printToken(*n->literal);
		break;
	default:
		printf("UNKNOWN NODE TYPE:\n");
		break;
	}
}


/* ========== String Builder ========== */
/* Represent a dynamic string buffer. */
typedef struct {
	int count;
	int capacity;
	char *buffer;
} StringBuilder;

void initStringBuilder(StringBuilder *sb) {
	sb->capacity = 1024;
	sb->count = 0;
	sb->buffer = malloc(sb->capacity);
	sb->buffer[0] = '\0';
}

/* Append the text of lenght len to the string builder sb. */
void appendString(StringBuilder *sb, const char *text, int len) {
	if (text == NULL || len == 0) return;

	if (sb->count + len + 1 > sb->capacity) {
		while (sb->count + len + 1 > sb->capacity)
			sb->capacity *= 2;
		sb->buffer = realloc(sb->buffer, sb->capacity);
		if (sb->buffer == NULL) {
			fprintf(stderr, "Failed to reallocate memory for string builder.\n");
			exit(1);
		}
	}
	memcpy(sb->buffer + sb->count, text, len);
	sb->count += len;
	sb->buffer[sb->count] = '\0';
}


/* ========== Formatting Rules ========== */

/* Represent all available formatting types. */
typedef enum {
	RULE_INDENT
} RuleType;

typedef struct {
	int count;
	int capacity;
	NodeType *contexts;
} ContextArray;

typedef struct {
	int tabSize;	// number of spaces per tab
			// default = 8
	int tabNum;	// number of tabs per indent 
			// default = 1
	bool useTabs;	// true for tabs, false for space 
			// default = true
	bool addLine;	// true for add a new line before indenting 
			// default = true
	bool useLevel;	// true for enable the formatting level
			// default = true
} IndentOpt;

/* Represent a single formatting rule. */
typedef struct {
	RuleType type;
	ContextArray ctxArray;	// elements where the rule applies
	//*conds	   conditions defining when the rule applies
	
	union {		// options that control formatting behavior
		IndentOpt indentOpt;
	};
} Rule;

/* Represent a dynamic array of rules. */
typedef struct {
	int count;
	int capacity;
	Rule *rules;
} RuleArray;

/* Initialize and allocate memory for the dinamic array of rules. */
void initRuleArray(RuleArray *a) {
	a->count = 0;
	a->capacity = 8;
	a->rules = malloc(sizeof(Rule) * a->capacity);
}

/* Add a rule r into rule array a. */
void addRule(RuleArray *a, Rule r) {
	if (a->count >= a->capacity) {
		a->capacity *= 2;
		a->rules = realloc(a->rules, sizeof(Rule) * a->capacity);
	}
	a->rules[a->count++] = r;
}

/* Initialize and allocate memory for the dinamic array of contexts. */
void initContextArray(ContextArray *a) {
	a->count = 0;
	a->capacity = 8;
	a->contexts = malloc(sizeof(NodeType) * a->capacity);
}

/* Add a context ctx into contexts array a. */
void addContext(ContextArray *a, NodeType ctx) {
	if (a->count >= a->capacity) {
		a->capacity *= 2;
		a->contexts = realloc(a->contexts, sizeof(NodeType) * a->capacity);
	}
	a->contexts[a->count++] = ctx;
}

/* Initialize a rule with its type. */
Rule createRule(RuleType type) {
	Rule rule;
	rule.type = type;
	initContextArray(&rule.ctxArray);
	return rule;
}

/* Create an indent rule. */
Rule createIndentRule(void) {
	Rule rule = createRule(RULE_INDENT);
	rule.indentOpt.tabSize  = 8;
	rule.indentOpt.tabNum   = 1;
	rule.indentOpt.useTabs  = true;
	rule.indentOpt.addLine  = true;
	rule.indentOpt.useLevel = true;
	return rule;
}

void freeRule(Rule *rule) {
	free(rule->ctxArray.contexts);
}

void freeRuleArray(RuleArray *a) {
	for (int i = 0; i < a->count; i++) {
		freeRule(&a->rules[i]);
	}
	free(a);
}


/* ========== Formatter ========== */

/* Represent the entity that produces the formatted text, appling the user rulesto the AST nodes. */
typedef struct {
	RuleArray *ruleArray;	// formatting rules
	const char *lastPos;	// last source position we output
	StringBuilder output;	// formatted source produced
	// State
	int indentLevel;	// indentation level
	int colNum;		// number of column
} Formatter;

Formatter formatter;

/* Initialize the formatter with an array of rules. */
void initFormatter(RuleArray *a, const char *source) {
	formatter.ruleArray = a;
	formatter.lastPos = source;
	initStringBuilder(&formatter.output);

	// Init the state
	formatter.indentLevel = 0;
	formatter.colNum      = 0;
}

/* Free the memory userd for allocate the formatter. */
void freeFormatter(void) {
	free(formatter.output.buffer);
}

/* Output the string s with lenght len provided as input. 
 * Then update the formatter state. */
void outputString(const char *s, int len) {
	if (s == NULL || len <= 0) return;

	for (int i = 0; i < len; i++) {
		if (s[i] == '\n') {
			formatter.colNum = 1;
		} else if (s[i] == '\t') {
			// Calculate the spaces needed to get to the next
			// tab stop
			int blanks = TAB_SIZE - ((formatter.colNum - 1) %
						 TAB_SIZE);
			formatter.colNum += blanks;
		} else {
			formatter.colNum++;
		}
	}

	appendString(&formatter.output, s, len);
	formatter.lastPos = s + len;
}

/* Output the text between start and end. */
void outputGap(const char *start, const char *end) {
	if (start >= end) return;
	int len = (int)(end - start);
	outputString(start, len);
}

/* Output an indentation with the provided options. 
 * The indentation is computed considering the tab stop. */
void indent(IndentOpt *opt) {
	// colNum is the number of the last output character
	int currCol = formatter.colNum + 1;
	int level   = opt->useLevel ? formatter.indentLevel : 1;

	// Calculate maximum possible size for the buffer
	int maxSize = opt->addLine + 
		      level * opt->tabNum * opt->tabSize + 1;
	char buf[maxSize];
	char *c = buf;

	if (opt->addLine) {
		*c = '\n';
		c++;
		currCol = 1;
	} 

	for (int i = 0; i < opt->tabNum * level; i++) {
		if (opt->tabSize == 0) break;
		int tabWidth  = opt->tabSize - 
				((currCol - 1) % opt->tabSize);
		int tabCol = currCol + tabWidth;
		// Use tabs to get as close as possible to the tab 
		// column
		if (opt->useTabs) {
			while (1) {
				// Calculate where I land if I put a tab
				int dist = TAB_SIZE - 
					   ((currCol - 1) % TAB_SIZE);
				int tabStop = currCol + dist;
				if (tabStop <= tabCol) {
					*c = '\t';
					c++;
					currCol = tabStop;
				} else {
					break;
				}
			}
		}

		// Use spaces to fill the remaining distance
		while (currCol < tabCol) {
			*c = ' ';
			c++;
			currCol++;
		}
	}

	outputString(buf, c - buf);
}

/* Return true if the rule conditions match the node, false otherwise. */
bool matchCondRule(Rule *rule, Node *n) {
	// Check the context
	bool match = false;
	for (int i = 0; i < rule->ctxArray.count; i++) {
		if (n->type == rule->ctxArray.contexts[i]) {
			match = true;
			break;
		}
	}

	if (!match) return false;
	return true;
}

/* Apply the before formatting rules to node n. */
void applyBeforeRules2Node(Node *n) {
	Token *t = n->allTokens.tokens[0]; // first token of the node
	bool applied = false;

	// Loop all the rules
	for (int i = 0; i < formatter.ruleArray->count; i++) {
		Rule *rule = &formatter.ruleArray->rules[i];
		// Check if the node matches the condition rules
		if (!matchCondRule(rule, n)) {
			continue;
		}

		switch (rule->type) {
		case RULE_INDENT:
			indent(&rule->indentOpt);
			applied = true;
			break;
		default:
			break;
		}
	}

	// If none of the rules matches, output the whitespaces before 
	// the first token of the node
	if (!applied) {
		outputGap(formatter.lastPos, t->start);
	} else {
		formatter.lastPos = t->start;
	}
}

/* Produce the formatted output traversing the AST and appling the formatting 
 * rules. */
void formatNode(Node *n) {
	if (n == NULL) return;
	
	// Loop all the tokens in the node
	int nodeIdx = 0;
	for (int i = 0; i < n->allTokens.count; i++) {
		Token *t    = n->allTokens.tokens[i];
		TokenRole r = n->allTokens.roles[i];
	
		/* Format the child nodes following this steps:
		 * - Update the formatter state.
		 * - Apply the "before" formatting rules.
		 * - Reursively format the child nodes.
		 * - Apply the "after" formatting rules.
		 * - Restore the formatter state.
		 * */
		if (r == TOKEN_ROLE_CONTENT) {
			Node *child = NULL;
			switch (n->type) {
			case NODE_PROGRAM:
				child = n->block.nodes[nodeIdx];
				applyBeforeRules2Node(child); 
				formatNode(child);
				break;
			case NODE_BLOCK:
				formatter.indentLevel++;
				child = n->block.nodes[nodeIdx];
				applyBeforeRules2Node(child); 
				formatNode(child);
				formatter.indentLevel--;
				break;
			case NODE_ARRAY:
				formatter.indentLevel++;
				child = n->array.nodes[nodeIdx];
				applyBeforeRules2Node(child); 
				formatNode(child);
				formatter.indentLevel--;
				break;
			case NODE_KEYVALUE:
				child = (nodeIdx == 0) ? 
						n->keyvalue.key :
						n->keyvalue.value;
				applyBeforeRules2Node(child); 
				formatNode(child);
				break;
			case NODE_LITERAL:
				outputString(n->literal->start,
					   n->literal->len);
				break;
			default:
				break;
			}	

			nodeIdx++;
		} else { // Output the token
			/* Apply the "before" formatting rules. */
			 // apply the formatting to the token
			 //applyBeforeRules2Token(NULL); 
			outputString(t->start, t->len);
		}
	}
}


/* ========== Main ========== */

int main(void) {
	// Get the user source file
	char buf[1024];
	if (fgets(buf, sizeof(buf), stdin) == NULL) return 1;

	// Get the user rules
	RuleArray rules;
	initRuleArray(&rules);	

	// Scan the source and produce tokens
	initScanner(buf);
	TokenArray tokens = scanTokens();
	printTokens(&tokens);

	// Parse the tokens
	initParser(&tokens);
	Node *ast = parse();

	if (ast == NULL) {
		free(tokens.tokens);
		free(rules.rules);
		return 1;
	}

	printf("\n\n");
	printAST(ast, 0);

	if (parser.hadErr) {
		fprintf(stderr, "Warning: Parsing compleated with errors."
			        " AST may be incompleate.\n");
	}

	// Format
	Rule rule = createIndentRule();
	rule.indentOpt.tabSize  = 4;
	rule.indentOpt.tabNum   = 1;
	rule.indentOpt.addLine  = true;
	rule.indentOpt.useTabs  = true;
	addContext(&rule.ctxArray, NODE_KEYVALUE);
	addRule(&rules, rule);

	initFormatter(&rules, buf);
	formatNode(ast);

	// Print the formatted output
	printf("\n=== Formatted Output ===\n");
	printf("%s", formatter.output.buffer);
	printf("\n========================\n");

	// Free the allocated memory
	freeNode(ast);
	free(tokens.tokens);
	free(rules.rules);
	freeFormatter();

	return 0;
}


















#if 0
typedef struct {
	int tab_size;	/* number of spaces per indent */
	int use_tabs;	/* 1 for tabs, 0 for spaces */
} Indent_Opt;

typedef struct {
	const char *anchor;	/* reference point for alignment */
} Align_Opt;

typedef struct {
    int max_line_length;		/* number of chars for starting wrapping*/
    int preserve_existing_breaks;	/* 1 for preserving breaks, 
					   0 for override them */
} Wrap_Opt;

typedef struct {
} Space_Opt;

/* 
 * This macros uses the variable arguments and the default values 
 * to initialize the Opt structs.
 * Doing so, the user must only supply the value of the fields 
 * that want to change. 
 */
#define indent(...)                             \
	indent_opt((Indent_Opt){.tab_size = 8, .use_tabs = 1, __VA_ARGS__ }); \

#define align(...)                             \
	align_opt((Align_Opt){__VA_ARGS__ }); \

#define wrap(...)                             \
	wrap_opt((Wrap_Opt){.max_line_length = 60, __VA_ARGS__ }); \

#define space(...)                             \
	space_opt((Space_Opt){__VA_ARGS__ }); \

void indent_opt(Indent_Opt opt) 
{
	printf("%d\n", opt.tab_size);
	printf("%d\n", opt.use_tabs);
}

void align_opt(Align_Opt opt)
{
	if (opt.anchor) printf("%s\n", opt.anchor);
}

void wrap_opt(Wrap_Opt opt)
{
}

void space_opt(Space_Opt opt)
{
}

int main(void) 
{
	indent(.tab_size = 4);
	align(.anchor="=");

	return 0;
}
#endif
