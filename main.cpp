/*

	Program: Calculator

	Revision History:

	Date: 23/06/2024
		Got expressions added ('+','-','*' and '\').

	Date: 26/06/2024
		Made expressions as apart of statements. Which requires expressions to end with ';'

	Date: 30/06/2024
		Added global variables. Example:
			let fooey = 2-4*2;

	Date: 20/07/2024
		Added basic functions. Example: 
			fnt f(x) = x*2;

*/

#include <iostream>
#include <sstream>
#include <string>
#include <array>
#include <vector>
#include <unordered_map>
#include <variant>
#include <functional>
#include <algorithm>
#include <cmath>

constexpr int8_t tokErr = -2;
constexpr int8_t tokEndOfLine = '\n';
constexpr int8_t tokNum = 1;
constexpr int8_t tokStr = 2;
constexpr int8_t tokWord = 4;
constexpr int8_t tokStrQuote = '\'';
constexpr int8_t tokLetKeyword = 5;
constexpr int8_t tokFntKeyword = 6;
constexpr int8_t tokDelKeyword = 9;

constexpr bool isReservedKeyword(int8_t type) 
{
	return type >= tokLetKeyword && type <= tokDelKeyword;
}

struct Token {
	std::string lexeme = "";
	int8_t type = EOF;
	Token(const char* str, int8_t type) : lexeme(str), type(type) 
	{}
	Token() = default;
};

constexpr int keywordsMax = 3;

std::istream& operator>>(std::istream& is, Token& t) //supports reading tokens
{
	static std::array<std::string_view, keywordsMax> keywords =
	{
		"let",
		"fnt",
		"del",
	};

	static std::array<uint8_t, keywordsMax> keywordMap = 
	{
		tokLetKeyword,
		tokFntKeyword,
		tokDelKeyword,
	};

	char c = EOF;
	t.lexeme = "";
	t.type = EOF;

	is >> c; //scan first character
	
	if (c == EOF) {
		is.setf(std::ios::eofbit);
		return is;
	}

	if (isdigit(c)) {
		is.putback(c);
		double d;
		is >> d;
		t.lexeme = std::to_string(d);
		t.type = tokNum;
	}
	else if (isalpha(c)) {
		while (isalnum(c) || c == '_') {
			t.lexeme += c;
			c = is.get();
		}
		is.putback(c);

		auto keywordIter = std::find(keywords.begin(), keywords.end(), t.lexeme);
		t.type = keywordIter != keywords.end() ? keywordMap[keywordIter - keywords.begin()] : tokWord;
	}
	else {
		t.lexeme = c;
		t.type = c;
	}

	return is;
}

std::ostream& operator<<(std::ostream& os, Token& t) 
{
	return os << t.lexeme << ',' << ' ' << t.type << '\n';
}

class Tokeniser 
{
public:
	Tokeniser(std::istream& is) : m_is(is) {}
	
	Token get() 
	{
		if (m_isFull) {
			m_isFull = false;
			return m_lookAhead;
		}
		
		Token t;

		m_is >> t;

		return t;
	}
	void unget(Token& t) 
	{
		m_lookAhead = t;
		m_isFull = true;
	}
	Token peek() 
	{
		if (m_isFull)
			return m_lookAhead;
		Token t = get();
		m_lookAhead = t;
		m_isFull = true;
		return t;
	}
	void clear() 
	{
		m_isFull = false;
		m_is.clear();
	}
	void ignore(char ignoreUpto) 
	{
		char ch;
		while (m_is >> std::noskipws >> ch) {
			if (ignoreUpto == ch)
				break;
		}
		m_is.putback(ch);
		m_isFull = false;
	}
private:
	std::istream& m_is;
	Token m_lookAhead;
	bool m_isFull = false;
};

enum class Op : uint8_t {
	Halt,
	PushNum,
	Constant,
	PopNum,
	Add,
	Sub,
	Mul,
	Div,
	Pow,
	Equ,
	NEqu,
	Mt,
	Lt,
	Lte,
	Mte,
	GLoadD,
	GStoreD,
	RetF,
	LoadArg,
	CallF,
	PrintD,
};

struct Function {
	std::vector<uint8_t> bCode;
	uint8_t nArgs=0;
	Function(std::vector<uint8_t>& bCode, uint8_t nArgs) : bCode(bCode), nArgs(nArgs)
	{}
	Function() = default;
};

using SymbolVal = std::variant<uint16_t, Function>;

std::unordered_map<std::string, SymbolVal> globalSymTbl;

template<class T>
void writeArgToChunk(std::vector<uint8_t>&,const T&); //for writing args to code chunk

double execute(std::vector<uint8_t>&);

bool prsFunctDef(Tokeniser&, std::vector<uint8_t>&);

bool prsLetStmt(Tokeniser&, std::vector<uint8_t>&, std::unordered_map<std::string, SymbolVal>&);

bool prsDelStmt(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk, std::unordered_map<std::string, SymbolVal>& varTbl);

bool prsStmt(Tokeniser&,std::vector<uint8_t>&);

bool prsExpr(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk, std::unordered_map<std::string, SymbolVal>* localVars = nullptr);

bool prsVal(Tokeniser&, std::vector<uint8_t>&, std::unordered_map<std::string, SymbolVal>* localVars = nullptr);

bool addSymbol(std::unordered_map<std::string, SymbolVal>& tbl, const std::string& name, SymbolVal sym);

bool removeSymbol(std::unordered_map<std::string, SymbolVal>& tbl, const std::string& name);

bool expectToken(Tokeniser& tokeniser, int8_t tokTy, const char* errMsg, Token* storePtr);

template<class T>
void writeArgToChunk(std::vector<uint8_t>& codeChunk, const T& arg) 
{
	uint8_t bytes[sizeof(arg)];
	memcpy(bytes, &arg, sizeof(arg));
	for (uint8_t& byte : bytes)
		codeChunk.emplace_back(byte);
}

constexpr uint16_t globalsMax = 1024;

//The VM itself
double execute(uint8_t* code) 
{
	using StackData = std::variant<uint8_t*,double>;
	
#define STACK_MAX 1024
	static std::array<StackData, STACK_MAX> stack = { {0.0} };
	static StackData* sp = stack.data();
	
	auto stackPush = [&](StackData val) -> bool
	{
		return sp != stack.data() + STACK_MAX ? ((*sp++ = val), true) : false;
	};
#undef STACK_MAX
	auto stackPop = [&]() -> StackData*
	{
		return sp == stack.data() ? nullptr : (--sp);
	};

	auto stackEmpty = [&]() -> bool
	{
		return sp == stack.data();
	};

	auto binaryOp = [&](std::function<double(double&,double&)> operation) -> bool
	{
		StackData* v2 = stackPop();
		StackData* v1 = stackPop();
		
		if (!v2 || !v1) 
			return false;

		if (!std::get_if<double>(v1) || !std::get_if<double>(v2)) //make sure both elems are double and not uint8_t*
			return false;

		stackPush(operation(std::get<double>(*v1), std::get<double>(*v2)));
		return true;
	};

	const uint8_t* ip = code;
	StackData* bp = sp;
	
	static std::array<double, globalsMax> globals = { 0 };

	while (*ip != (uint8_t)Op::Halt) {
		const uint8_t* opcode = ip++;
		switch (*opcode) {
		case (uint8_t)Op::PushNum:
			stackPush(*(double*)ip);
			ip += sizeof(double);
			break;
		case (uint8_t)Op::PopNum:
			if (!stackEmpty())
				stackPop();
			break;
		case (uint8_t)Op::Add:
			binaryOp([](double& lhs, double& rhs) { return lhs + rhs;  });
			break;
		case (uint8_t)Op::Sub:
			binaryOp([](double& lhs, double& rhs) { return lhs - rhs;  });
			break;
		case (uint8_t)Op::Mul:
			binaryOp([](double& lhs, double& rhs) { return lhs * rhs;  });
			break;
		case (uint8_t)Op::Div:
			binaryOp([](double& lhs, double& rhs) { return lhs / rhs;  });
			break;
		case (uint8_t)Op::Pow:
			binaryOp([](double& lhs, double& rhs) { return std::pow(lhs, rhs);  });
			break;
		case (uint8_t)Op::Equ:
			binaryOp([](double& lhs, double& rhs) { return lhs == rhs;  });
			break;
		case (uint8_t)Op::Lt:
			binaryOp([](double& lhs, double& rhs) { return lhs < rhs;  });
			break;
		case (uint8_t)Op::Mt:
			binaryOp([](double& lhs, double& rhs) { return lhs > rhs;  });
			break;
		case (uint8_t)Op::PrintD:
			std::cout << (stackEmpty() ? "Nothing on the stack!" : std::to_string(std::get<double>(*stackPop()))) << '\n';
			break;
		case (uint8_t)Op::GStoreD: {
			uint16_t idx = *(uint16_t*)ip;
			ip += sizeof(uint16_t);
			globals[idx] = std::get<double>(*stackPop());
			break;
		}
		case (uint8_t)Op::GLoadD: {
			uint16_t idx = *(uint16_t*)ip;
			ip += sizeof(uint16_t);
			stackPush(globals[idx]);
			break;
		}
		case (uint8_t)Op::LoadArg: {
			uint8_t offset = *(uint8_t*)ip;
			ip += sizeof(uint8_t);
			stackPush(*(bp - offset));
			break;
		}
		case (uint8_t)Op::RetF: {
			*(sp - 1) = *bp;
			break;
		}
		case (uint8_t)Op::CallF: {
			intptr_t functAddr = *(intptr_t*)ip;
			execute((uint8_t*)functAddr);
			ip += sizeof(intptr_t);
			break;
		}
		}
	}

	return std::get<double>(*sp);
}

bool addSymbol(std::unordered_map<std::string,SymbolVal>& tbl, const std::string& name, SymbolVal sym)
{
	auto exists = tbl.find(name);

	if (exists == tbl.end())
		tbl.insert(std::make_pair(name, sym));
	else {
		std::cerr << name << ' ' << "cannot be defined twice\n";
		return false;
	}

	return true;
}

bool removeSymbol(std::unordered_map<std::string, SymbolVal>& tbl, const std::string& name)
{
	auto exists = tbl.find(name);

	if (exists != tbl.end())
		tbl.erase(name);
	else {
		std::cerr << name << ' ' << "doesn't exist\n";
		return false;
	}

	return true;
}

bool expectToken(Tokeniser& tokeniser, int8_t tokTy, const char* errMsg, Token* storePtr=nullptr) 
{
	Token t = tokeniser.get();
	
	storePtr ? ((*storePtr = t), storePtr) : nullptr;
	if (t.type == tokTy) {
		return true;
	}
	else if (errMsg)
		std::cerr << errMsg << '\n';
	
	return false;
}

//<Functor>::= word '('  ')'
bool prsFunctor(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk, bool isFunctCall, std::unordered_map<std::string,SymbolVal>* localVars=nullptr, std::vector<std::string>* params = nullptr)
{
	Token functName, t;

	bool succ = expectToken(tokeniser, tokWord, "Expected a function name", &functName);

	auto exists = globalSymTbl.find(functName.lexeme);

	Function* funct = nullptr;

	if (exists != globalSymTbl.end())
		funct = std::get_if<Function>(&exists->second);
	
	succ = expectToken(tokeniser, '(', "Expected a '('", &t);

	uint8_t nArgsParsed = 0;
	
	while ((t = tokeniser.get()).type != ')' && t.type != EOF) {
		tokeniser.unget(t);
		if (!isFunctCall && params) {
			succ = expectToken(tokeniser, tokWord, "Expected a parameter", &t);
			if (succ)
				params->emplace_back(t.lexeme);
			
		}
		else {
			succ = prsExpr(tokeniser, codeChunk,localVars);
			nArgsParsed = succ ? nArgsParsed + 1 : nArgsParsed;
		}
		
		t = tokeniser.get();
		if (t.type == ')') {
			tokeniser.unget(t);
			continue;
		}

		if (t.type != ',') {
			std::cerr << "Expected a ','\n";
		}
	}

	if (t.type != ')') {
		std::cerr << "Expected a ')'\n";
		succ = false;
	}
	
	if (funct && nArgsParsed != funct->nArgs) {
		std::cerr << "Invalid number of arguments: expected " << (int)funct->nArgs << '\n';
		succ = false;
	}

	if (isFunctCall && funct && succ) { //write the function call to code chunk
		codeChunk.emplace_back((uint8_t)Op::CallF);
		writeArgToChunk(codeChunk, (intptr_t)funct->bCode.data());
	}
	
	return succ;
}

//<FunctDef>::= 'funct' <Functor> '=' <Expr> 
bool prsFunctDef(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk)
{
	tokeniser.get(); //read the 'funct' keyword

	Token functName = tokeniser.get();

	tokeniser.unget(functName);

	std::vector<std::string> paramNames;

	std::unordered_map<std::string, SymbolVal> localVars;

	bool succ = prsFunctor(tokeniser, codeChunk, false, nullptr, &paramNames);
	
	if (paramNames.size() >= UINT8_MAX+1) { //check that parameters
		succ = false;
		std::cerr << "Parameter limit reached: 255\n";
	}

	for (int16_t i = 0; i < paramNames.size() && succ; ++i)
		addSymbol(localVars, paramNames[i], (uint16_t)(paramNames.size() - i));

	if (!expectToken(tokeniser, '=', "Expected a '='"))
		succ = false;

	if (!prsExpr(tokeniser, codeChunk, &localVars))
		succ = false;
	
	for (const std::string& name : paramNames) {
		removeSymbol(localVars,name);
		codeChunk.emplace_back((uint8_t)Op::PopNum);
	}

	codeChunk.emplace_back((uint8_t)Op::RetF);
	codeChunk.emplace_back((uint8_t)Op::Halt);

	if (succ)
		addSymbol(globalSymTbl,functName.lexeme, Function(codeChunk, paramNames.size()));

	return succ;
}

//<LetStmt>:: 'let' <identifier> '=' <Expr>
bool prsLetStmt(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk, std::unordered_map<std::string, SymbolVal>& varTbl)
{
	Token t = tokeniser.get(); //read 'let' keyword
	
	Token word;

	bool succ = expectToken(tokeniser, tokWord, "Expected a variable name", &word);
	if (!expectToken(tokeniser, '=', "Expected a '=' operator"))
		succ = false;

	if (!prsExpr(tokeniser, codeChunk))
		succ = false;

	t = tokeniser.get();
	tokeniser.unget(t);
	
	if (!succ || t.type != ';') //is there ';' ahead of time?
		return false;

	static uint16_t id = 0;
	if (id == UINT16_MAX)
		return false;

	addSymbol(varTbl,word.lexeme, id);

	codeChunk.emplace_back((uint8_t)Op::GStoreD);

	writeArgToChunk(codeChunk, id);
	++id;

	return succ;
}

//<DelStmt>:: 'del' <identifier>
bool prsDelStmt(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk, std::unordered_map<std::string, SymbolVal>& varTbl) 
{
	Token t = tokeniser.get(); //read 'del' keyword

	Token word;

	bool succ = expectToken(tokeniser, tokWord, "Expected a variable name", &word);

	if (!succ)
		return succ;

	auto exists = varTbl.find(word.lexeme);

	if (exists == varTbl.end()) 
		return succ = false;

	t = tokeniser.get();
	tokeniser.unget(t);
	if (t.type != ';')  //is a semi-colon is found ahead of time?
		return succ = false;

	removeSymbol(varTbl, word.lexeme);

	return succ;
}

/*
	<Val>:: <number> | <identifier> | '+' <Val> | '-' <Val> | '(' <Expr> ')'
*/
bool prsVal(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk, std::unordered_map<std::string, SymbolVal>* localVars)
{
	Token t = tokeniser.get();
	bool succ = true;

	switch (t.type) {
	case tokNum: {
		codeChunk.emplace_back((uint8_t)Op::PushNum);
		writeArgToChunk(codeChunk, atof(t.lexeme.c_str()));
		break;
	}
	case '(': {
		succ = prsExpr(tokeniser, codeChunk,localVars);
		if (!expectToken(tokeniser, ')', "Expected a ')'"))
			succ = false;
		break;
	}
	case '-': {
		codeChunk.emplace_back((uint8_t)Op::PushNum);
		writeArgToChunk(codeChunk, 0.0);
		succ = prsVal(tokeniser, codeChunk);
		codeChunk.emplace_back((uint8_t)Op::Sub);
		return succ;
	}
	case '+':
		return prsVal(tokeniser, codeChunk,localVars);
	case tokWord: {
		auto sym = globalSymTbl.find(t.lexeme);
		
		if (localVars) {
			auto localSym = localVars->find(t.lexeme);
			if (localSym != localVars->end()) { //if the variable is local write loadarg bytecode
				codeChunk.emplace_back((uint8_t)Op::LoadArg);
				writeArgToChunk(codeChunk, (uint8_t)std::get<uint16_t>(localSym->second));
				break;
			}
		}

		if (sym == globalSymTbl.end()) {
			std::cerr << t.lexeme << ' ' << "is not defined\n";
			return false;
		}

		if (Function* funct = std::get_if<Function>(&sym->second)) {
			tokeniser.unget(t);
			return prsFunctor(tokeniser, codeChunk, true,localVars);
		}

		codeChunk.emplace_back((uint8_t)Op::GLoadD);
		writeArgToChunk(codeChunk, std::get<uint16_t>(sym->second));

		break;
	}
	default:
		std::cerr << "Expected a value\n";
		tokeniser.unget(t);
		return false;
	}

	return succ;
}

#define LEFT_ASSOC (true)
#define RIGHT_ASSOC (false)
/*
	Binding power is denoted within curly brackets
	Precedence is in descending order
	<Expr>:: <Val> |
			 <Expr> '*' <Expr> {2}
			 <Expr> '/' <Expr> {2}
			 <Expr> '+' <Expr> {1}
			 <Expr> '-' <Expr> {1}
			 <Expr> '>' <Expr> {0}
			 <Expr> '<' <Expr> {0}
			 <Expr> '=' <Expr> {-1}
*/
bool prsExpr(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk, std::unordered_map<std::string, SymbolVal>* localVars)
{
	using OpInfo = std::pair<int8_t, bool>;
	static std::unordered_map<char, OpInfo> infixOpPrec
	{
		{'=',{-1,LEFT_ASSOC}},
		{'>',{0,LEFT_ASSOC}},
		{'<',{0,LEFT_ASSOC}},
		{'+',{1,LEFT_ASSOC}},
		{'-',{1,LEFT_ASSOC}},
		{'/',{2,LEFT_ASSOC}},
		{'*',{2,LEFT_ASSOC}},
	};
	
	bool succ = prsVal(tokeniser, codeChunk, localVars);

	static std::vector<int8_t> opStack;
	Token op;

	auto computeOp = [&](int8_t op)
	{
		switch (op) {
		case '+':
			codeChunk.emplace_back((uint8_t)Op::Add);
			break;
		case '*':
			codeChunk.emplace_back((uint8_t)Op::Mul);
			break;
		case '-':
			codeChunk.emplace_back((uint8_t)Op::Sub);
			break;
		case '/':
			codeChunk.emplace_back((uint8_t)Op::Div);
			break;
		case '>':
			codeChunk.emplace_back((uint8_t)Op::Mt);
			break;
		case '<':
			codeChunk.emplace_back((uint8_t)Op::Lt);
			break;
		case '=':
			codeChunk.emplace_back((uint8_t)Op::Equ);
			break;
		}
	};

	while (infixOpPrec.find((op = tokeniser.get()).type) != infixOpPrec.end() && op.type != ')') { //is the token an operator (exception being ')')?
		int8_t opPrec = infixOpPrec.at(op.type).first;
		
		while (!opStack.empty() && opPrec <= infixOpPrec.at(opStack.back()).first) { //if weaker precedence, pop and compute
			computeOp(opStack.back());
			opStack.pop_back();
		} 

		opStack.push_back(op.type);
		if (!prsVal(tokeniser, codeChunk, localVars))
			succ = false;
	}
	
	if (infixOpPrec.find(op.type) == infixOpPrec.end())
		tokeniser.unget(op);

	while (!opStack.empty()) { //pop off remaining operators
		computeOp(opStack.back());
		opStack.pop_back();
	}

	return succ;
}

//<Stmt>:: <LetStmt> ';' | <DelStmt> ';' | <Expr> ';' | <FntDef> ';'
bool prsStmt(Tokeniser& tokeniser, std::vector<uint8_t>& codeChunk) 
{
	Token t = tokeniser.get();
	tokeniser.unget(t);
	bool succ = true;

	if (t.type == tokLetKeyword)
		succ = prsLetStmt(tokeniser, codeChunk,globalSymTbl);
	else if (t.type == tokFntKeyword) {
		std::vector<uint8_t> functCode;
		succ = prsFunctDef(tokeniser, functCode);
	}
	else if (t.type == tokDelKeyword) {
		succ = prsDelStmt(tokeniser, codeChunk, globalSymTbl);
	}
	else {
		succ = prsExpr(tokeniser, codeChunk);
		codeChunk.emplace_back((uint8_t)Op::PrintD);
	}

	if (!expectToken(tokeniser, ';', "Expected a ';'")) {
		succ = false;
	}
	
	codeChunk.emplace_back((uint8_t)Op::Halt);
	return succ;
}

bool bufferInputTil(std::stringstream& strm, char terminator) 
{
	char c = EOF;
	
	while (std::cin >> std::noskipws >> c && c != terminator) {
		strm << c;
	}

	return c == terminator;
}

int main(int argc, char** argv)
{
	std::vector<uint8_t> codeChunk;
	Token t;
	std::cout << "Welcome to the calculator program.\n" 
		<< "All statements must end with a semi-colon.\n" 
		<< "Examples:\n\t1. (1+2)*4;\n\t2. let x = 4-2;\n\t3. fnt sqr(x) = x * x;\n";

	std::string line;
	std::stringstream is;
	
	Tokeniser stdTokeniser(is);
	std::cout << '>';
	while (bufferInputTil(is, '\n')) { //keep scanning til '\n'
		while ((t = stdTokeniser.get()).type != EOF) {
			stdTokeniser.unget(t);
			bool success = prsStmt(stdTokeniser, codeChunk); //parse
			
			if (success) execute(codeChunk.data()); //interpret
			
			memset(codeChunk.data(), 0, codeChunk.size());
			codeChunk.clear();
		}
		std::cout << '>';
		stdTokeniser.clear();
		is.clear();
	}

	return 0;
}
