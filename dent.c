#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

/* ========== AST ========== */

/* Represent a concept in the most of programming languages. */
typedef enum {
	NODE_UNKNOWN,

	NODE_PROGRAM,

	NODE_BLOCK,
	NODE_ARRAY,
	NODE_KEYVALUE,

	NODE_LITERAL,
} NodeType;

typedef struct Node Node;
typedef struct NodeList NodeList;
typedef struct KeyValue KeyValue;

/* Dinamic array of nodes. */
struct NodeList {
	int count;
	int capacity;
	Node **nodes;
};

struct KeyValue {
	Node  *key;
	Node  *value;	
};

/* Represent a single node of the AST. */
struct Node {
	NodeType type;

	union {
		NodeList block;
		NodeList array;
		KeyValue keyvalue;
		Token    literal;
	};
};

/* Initialize and allocate memory for the dinamic array of nodes. */
void initNodeList(NodeList *l) {
	l->count = 0;
	l->capacity = 8;
	l->nodes = malloc(sizeof(Node*) * l->capacity);
}

/* Add a node n into node list l. */
void addNode(NodeList *l, Node *n) {
	if (l->count >= l->capacity) {
		l->capacity *= 2;
		l->nodes = realloc(l->nodes, sizeof(Node*) * l->capacity);
	}
	l->nodes[l->count++] = n;
}

/* Allocate and initialize a node with its type. */
Node *createNode(NodeType type) {
	Node *n = malloc(sizeof(Node));
	n->type = type;
	return n;
}

/* Create a program node. */
Node *createProgramNode(void) {
	Node *n = createNode(NODE_PROGRAM);
	return n;
}

/* Create a block node. */
Node *createBlockNode(NodeList l) {
	Node *n = createNode(NODE_BLOCK);
	n->block = l;
	return n;
}

/* Create an array node. */
Node *createArrayNode(NodeList elem) {
	Node *n = createNode(NODE_ARRAY);
	n->array = elem;
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
Node *createLiteralNode(Token token) {
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
	free(n);
}


/* ========== Parser ========== */

/* Represent the parser that generates the AST from the tokens using the
 * grammar rules of a language. */
typedef struct {
	Token curr;
	Token prev;
	bool hadErr;
	bool panicMode;
} Parser;

Parser parser;

/* Initialize the parser. */
void initParser(void) {
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
	errorAt(&parser.prev, msg);
}

/* Report an error at the current token. */
void errorAtCurrent(const char *msg) {
	errorAt(&parser.curr, msg);
}

/* Advance to the next token. */
void advancePars(void) {
	parser.prev = parser.curr;

	while (1) {
		parser.curr = scanToken();
		if (parser.curr.type == TOKEN_WHITESPACE ||
		    parser.curr.type == TOKEN_NEWLINE) continue;

		if (parser.curr.type != TOKEN_ERROR) break;

		errorAtCurrent(parser.curr.start);
	}
}

/* Check if the current token matches the expected type. */
bool check(TokenType type) {
	return parser.curr.type == type;
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

	while (parser.curr.type != TOKEN_EOF) {
		/* Stop at tokens that start a new constructs. */
		switch (parser.curr.type) {
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
	if (parser.curr.type == type) {
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

/* Parse a JSON object: '{' (STRING ':' value (',' STRING ':' value)* )? '}' */
Node *parseJsonObject(void) {
	NodeList props;
	initNodeList(&props);

	// Empty object
	if (match(TOKEN_RCURLY_BRACKET)) return createBlockNode(props);

	do {
		if (check(TOKEN_RCURLY_BRACKET)) break;

		if (!check(TOKEN_STRING)) {
			error("Expect property name");
			synchronize();
			continue;
		}

		Node *key = createLiteralNode(parser.curr);
		advancePars();

		if (!consume(TOKEN_COLON, "Expect ':' after property name")) {
			freeNode(key);
			synchronize();
			continue;
		}

		Node *value = parseJsonValue();
		if (value == NULL) {
			freeNode(key);
			synchronize();
			continue;
		}

		Node *p = createKeyvalueNode(key, value);
		addNode(&props, p);

	} while(match(TOKEN_COMMA));

	consume(TOKEN_RCURLY_BRACKET, "Expect '}' after object properties");
	return createBlockNode(props);
}

/* Parse a JSON array: '[' (value (',' value)* )? ']' */
Node *parseJsonArray(void) {
	NodeList elem;
	initNodeList(&elem);

	if (match(TOKEN_RSQUARE_BRACKET)) return createArrayNode(elem);

	do {
		if (check(TOKEN_RSQUARE_BRACKET)) break;

		Node *e = parseJsonValue();	
		if (e == NULL) {
			synchronize();
			continue;
		}
		
		addNode(&elem, e);

	} while (match(TOKEN_COMMA));

	consume(TOKEN_RSQUARE_BRACKET, "Expect ']' after array elements");
	return createArrayNode(elem);
}

/* Parse a JSON value: 
 * 	object | array | "true" | "false" | "null" | STRING | NUMBER */
Node *parseJsonValue(void) {
	if (match(TOKEN_LCURLY_BRACKET))
		return parseJsonObject();
	if (match(TOKEN_LSQUARE_BRACKET))
		return parseJsonArray();
	if (match(TOKEN_TRUE) || match(TOKEN_FALSE) || match(TOKEN_NULL) ||
	    match(TOKEN_STRING) || match(TOKEN_NUMBER)) {
		return createLiteralNode(parser.prev);
	}

	error("Expect value");
	return NULL;
}

/* Parse a json program: value EOF */
Node *parseProgram(void) {
	Node *value = parseJsonValue();

	Node *prog = createProgramNode();
	initNodeList(&prog->block);

	if (value != NULL)
		addNode(&prog->block, value);

	consume(TOKEN_EOF, "Expected end of input");
	return prog;
}

/* Return a node that represent the root of the AST. It is generated  using 
 * the recursive discent parsing method based on the grammar of the language. 
 * For this purpose it uses the parseProgram fuction as entry point. */
Node *parse(const char *source) {
	initScanner(source);
	initParser();

	advancePars();

	Node *prog = parseProgram();

	return prog;
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
		printToken(n->literal);
		break;
	default:
		printf("UNKNOWN NODE TYPE:\n");
		break;
	}
}


/* ========== Formatting ========== */



/* ========== Main ========== */

int main(void) {
	char buf[1024];
	if (fgets(buf, sizeof(buf), stdin) == NULL) return 1;

	Node *ast = parse(buf);

	if (ast == NULL) return 1;

	printf("\n\n");

	printAST(ast, 0);
	freeNode(ast);

	printf("\n\n");

	if (parser.hadErr) {
		fprintf(stderr, "Warning: Parsing compleated with errors. AST may be incompleate.\n");
	}

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
