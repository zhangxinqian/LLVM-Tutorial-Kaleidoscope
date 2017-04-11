#include <cctype>
#include <cstdio>
#include <string>
#include <vector>
#include <map>
#include "llvm/Analysis/Passes.h"
#include "llvm/ExecutionEngine/ExecutionEngine.h"
#include "llvm/ExecutionEngine/JIT.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/PassManager.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Transforms/Scalar.h"
using namespace llvm;

/////////////////////////////////////////////////////////////////////////////////
//                                 Lexer                                       //
/////////////////////////////////////////////////////////////////////////////////
enum Token {
	tok_eof = -1,	
	tok_def = -2,
	tok_extern = -3,	
	tok_identifier = -4,
	tok_number = -5,
	tok_if = -6,
	tok_then = -7,
	tok_else = -8,
	tok_for = -9, 
	tok_in = -10,
	tok_binary = -11,
	tok_unary = -12,
	tok_var = -13
};

static std::string IdentifierStr;
static double NumVal;

static int gettok() {
	static int LastChar = ' ';
	while (isspace(LastChar))
		LastChar = getchar();
	if (isalpha(LastChar)) {
		IdentifierStr = LastChar;
		while (isalnum((LastChar = getchar())))
			IdentifierStr += LastChar;	
		if (IdentifierStr == "def") return tok_def;
		if (IdentifierStr == "extern") return tok_extern;
		if (IdentifierStr == "if") return tok_if;
		if (IdentifierStr == "then") return tok_then;
		if (IdentifierStr == "else") return tok_else;
		if (IdentifierStr == "for") return tok_for;
    	if (IdentifierStr == "in") return tok_in;
		if (IdentifierStr == "binary") return tok_binary;
		if (IdentifierStr == "unary") return tok_unary;
		if (IdentifierStr == "var") return tok_var;
		return tok_identifier;
	}	
	if (isdigit(LastChar) || LastChar == '.') {
		std::string NumStr;
		do {
			NumStr += LastChar;
			LastChar = getchar();
		} while (isdigit(LastChar) || LastChar == '.');
		
		NumVal = strtod(NumStr.c_str(), 0);
		return tok_number;
	}	
	if (LastChar == '#') {
		do LastChar = getchar();
		while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');
		
		if (LastChar != EOF) return gettok();
	}	
	if (LastChar == EOF) return tok_eof;	
	int ThisChar = LastChar;
	LastChar = getchar();
	return ThisChar;
}

/////////////////////////////////////////////////////////////////////////////////
//                             Asbtract Syntax Tree                            //
/////////////////////////////////////////////////////////////////////////////////
class ExprAST {
public:
  virtual ~ExprAST() {}
  virtual Value *Codegen() = 0;
};

class NumberExprAST : public ExprAST {
	double Val;
public:
	NumberExprAST(double Val) : Val(Val) {}
	virtual Value *Codegen();
};

class VariableExprAST : public ExprAST {
	std::string Name;
public:
	VariableExprAST(const std::string &Name) : Name(Name) {}
	const std::string &getName() const { return Name; }
	virtual Value *Codegen();
};

class BinaryExprAST : public ExprAST {
	char Op;
	ExprAST *LHS, *RHS;
public:
	BinaryExprAST(char op, ExprAST *lhs, ExprAST *rhs) : Op(op), LHS(lhs), RHS(rhs) {}
	virtual Value *Codegen();
};

class UnaryExprAST : public ExprAST {
	char Opcode;
	ExprAST * Operand;
public:
	UnaryExprAST(char opcode, ExprAST *operand) : Opcode(opcode), Operand(operand) {}
	virtual Value *Codegen();
};

class CallExprAST : public ExprAST {
	std::string Callee;
	std::vector<ExprAST*> Args;
public:
	CallExprAST(const std::string &callee, std::vector<ExprAST*> &args) : Callee(callee), Args(args) {}
	virtual Value *Codegen();
};

class PrototypeAST {
	std::string Name;
	std::vector<std::string> Args;
	bool isOperator;
	unsigned Precedence;
public:
	PrototypeAST(const std::string &name, const std::vector<std::string> &args, bool isoperator = false, unsigned prec = 0) 
		: Name(name), Args(args), isOperator(isoperator), Precedence(prec){}
	bool isUnaryOp() const { return isOperator && Args.size() == 1; }
	bool isBinaryOp() const { return isOperator && Args.size() == 2; }
	char getOperatorName() const {
		assert(isUnaryOp() || isBinaryOp());
		return Name[Name.size()-1];
	}
	unsigned getBinaryPrecedence() const { return Precedence; }
	Function *Codegen();
	void CreateArgumentAllocas(Function *F);
};

class FunctionAST {
	PrototypeAST *Proto;
	ExprAST *Body;
public:
	FunctionAST(PrototypeAST *proto, ExprAST *body) : Proto(proto), Body(body) {}
	Function *Codegen();
};

class IfExprAST : public ExprAST {
	ExprAST *Cond, *Then, *Else;
public:
	IfExprAST(ExprAST *cond, ExprAST *then, ExprAST *_else) : Cond(cond), Then(then), Else(_else) {}
	virtual Value *Codegen();
};

class ForExprAST : public ExprAST {
  	std::string VarName;
  	ExprAST *Start, *End, *Step, *Body;
public:
  	ForExprAST(const std::string &varname, ExprAST *start, ExprAST *end, ExprAST *step, ExprAST *body) 
		: VarName(varname), Start(start), End(end), Step(step), Body(body) {}
  	virtual Value *Codegen();
};

class VarExprAST : public ExprAST {
	std::vector<std::pair<std::string, ExprAST*>> VarNames;
	ExprAST *Body;
public:
	VarExprAST(const std::vector<std::pair<std::string, ExprAST*>> &varnames, ExprAST *body) 
		: VarNames(varnames), Body(body) {}
	virtual Value *Codegen();
};
/////////////////////////////////////////////////////////////////////////////////
//                                  Parser                                     //
/////////////////////////////////////////////////////////////////////////////////
static int CurTok;
static int getNextToken() {
	return CurTok = gettok();
}

ExprAST *Error(const char *Str) { 
	fprintf(stderr, "Error: %s\n", Str); 
	return 0;
}

PrototypeAST *ErrorP(const char *Str) { 
	Error(Str); 
	return 0; 
}

FunctionAST *ErrorF(const char *Str) { 
	Error(Str); 
	return 0; 
}

static ExprAST *ParseExpression();

static ExprAST *ParseNumberExpr() {
	ExprAST *Result = new NumberExprAST(NumVal);
	getNextToken();
	return Result;
}

static ExprAST *ParseParenExpr() {
	getNextToken();
	ExprAST *V = ParseExpression();
	if (!V) return 0;
	if (CurTok != ')') return Error("expected ')'");
	getNextToken();
	return V;
}

static ExprAST *ParseIdentifierExpr() {
	std::string IdName = IdentifierStr;
	getNextToken();
	if (CurTok != '(') return new VariableExprAST(IdName);	
	getNextToken();
	std::vector<ExprAST*> Args;
	if (CurTok != ')') {
		while (1) {
			ExprAST *Arg = ParseExpression();
			if (!Arg) return 0;
			Args.push_back(Arg);
			if (CurTok == ')') break;
			if (CurTok != ',') return Error("Expected ')' or ',' in argument list");
			getNextToken();
		}
	}
	getNextToken();
	return new CallExprAST(IdName, Args);
}

static PrototypeAST *ParsePrototype() {
	std::string FnName;
	unsigned Kind = 0;
	unsigned BinaryPrecedence = 30;
	switch (CurTok) {
		default:
			return ErrorP("E");
		case tok_identifier:
			FnName = IdentifierStr;
			Kind = 0;
			getNextToken();
			break;
		case tok_unary:
			getNextToken();
			if (!isascii(CurTok)) return ErrorP("Expected unary operator");
			FnName = "unary";
			FnName += (char)CurTok;
			Kind = 1;
			getNextToken();
			break;
		case tok_binary:
			getNextToken();
			if (!isascii(CurTok)) return ErrorP("Expected binary operator");
			FnName = "binary";
			FnName += (char)CurTok;
			Kind = 2;
			getNextToken();
			if (CurTok == tok_number) {
				if (NumVal < 1 || NumVal > 100) return ErrorP("Invalid precedence: must be 1..100");
				BinaryPrecedence = (unsigned)NumVal;
				getNextToken();
			}
			break;
	}
	/*
	if (CurTok != tok_identifier)
		return ErrorP("Expected function name in prototype");
	std::string FnName = IdentifierStr;
	getNextToken();  
	*/
	if (CurTok != '(')
		return ErrorP("Expected '(' in prototype");
	std::vector<std::string> ArgNames;
	while (getNextToken() == tok_identifier)
		ArgNames.push_back(IdentifierStr);
	if (CurTok != ')')
		return ErrorP("Expected ')' in prototype");
	getNextToken();
	if (Kind && ArgNames.size() != Kind) return ErrorP("Invalid number of operands for operator");
	return new PrototypeAST(FnName, ArgNames, Kind != 0, BinaryPrecedence);
}

static FunctionAST *ParseDefinition() {
	getNextToken();
	PrototypeAST *Proto = ParsePrototype();
	if (Proto == 0) return 0;
	if (ExprAST *E = ParseExpression()) return new FunctionAST(Proto, E);
	return 0;
}

static FunctionAST *ParseTopLevelExpr() {
	if (ExprAST *E = ParseExpression()) {
		PrototypeAST *Proto = new PrototypeAST("", std::vector<std::string>());
		return new FunctionAST(Proto, E);
	}
	return 0;
}

static PrototypeAST *ParseExtern() {
	getNextToken();
	return ParsePrototype();
}

static ExprAST *ParseIfExpr() {
	getNextToken();
  	ExprAST *Cond = ParseExpression();
  	if (!Cond) return 0;
  	if (CurTok != tok_then)
    	return Error("expected then");
  	getNextToken();
  	ExprAST *Then = ParseExpression();
  	if (Then == 0) return 0;
  	if (CurTok != tok_else)
    	return Error("expected else");
  	getNextToken();
  	ExprAST *Else = ParseExpression();
  	if (!Else) return 0;
  	return new IfExprAST(Cond, Then, Else);
}

static ExprAST *ParseForExpr() {
  	getNextToken();
  	if (CurTok != tok_identifier)
    	return Error("expected identifier after for");
  	std::string IdName = IdentifierStr;
  	getNextToken();
  	if (CurTok != '=')
    	return Error("expected '=' after for");
  	getNextToken();
  	ExprAST *Start = ParseExpression();
  	if (Start == 0) return 0;
  	if (CurTok != ',')
    	return Error("expected ',' after for start value");
  	getNextToken();
  	ExprAST *End = ParseExpression();
  	if (End == 0) return 0;
 	ExprAST *Step = 0;
  	if (CurTok == ',') {
    	getNextToken();
    	Step = ParseExpression();
    	if (Step == 0) return 0;
  	}
  	if (CurTok != tok_in)
    	return Error("expected 'in' after for");
  	getNextToken();
  	ExprAST *Body = ParseExpression();
  	if (Body == 0) return 0;
  	return new ForExprAST(IdName, Start, End, Step, Body);
}

static ExprAST *ParseVarExpr() {
	getNextToken();
	std::vector<std::pair<std::string, ExprAST*>> VarNames;
	if (CurTok != tok_identifier) return Error("expected identifier after var");
	while (1) {
		std::string Name = IdentifierStr;
		getNextToken();
		ExprAST *Init = 0;
		if (CurTok == '=') {
			getNextToken();
			Init = ParseExpression();
			if (Init == 0) return 0;
		}
		VarNames.push_back(std::make_pair(Name, Init));
		if (CurTok != ',') break;
		getNextToken();
		if (CurTok != tok_identifier) return Error("expected identifier list after var");
	}
	if (CurTok != tok_in) return Error("expected 'in' keyword after 'var'");
	getNextToken();
	ExprAST *Body = ParseExpression();
	if (Body == 0) return 0;
	return new VarExprAST(VarNames, Body);
}

static ExprAST *ParsePrimary() {
	switch (CurTok) {
		default: 
			return Error("unknown token when expecting an expression");
		case tok_identifier: 
			return ParseIdentifierExpr();
		case tok_number:     
			return ParseNumberExpr();
		case '(':            
			return ParseParenExpr();
		case tok_if:
			return ParseIfExpr();
		case tok_for:        
			return ParseForExpr();
		case tok_var:
			return ParseVarExpr();
	}
}

static std::map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
	if (!isascii(CurTok)) return -1;
	int TokPrec = BinopPrecedence[CurTok];
	if (TokPrec <= 0) return -1;
	return TokPrec;
}

static ExprAST *ParseUnary() {
	if (!isascii(CurTok) || CurTok == '(' || CurTok == ',') return ParsePrimary();
	int Opc = CurTok;
	getNextToken();
	if (ExprAST *Operand = ParseUnary()) return new UnaryExprAST(Opc, Operand);
	return 0;
}

static ExprAST *ParseBinOpRHS(int ExprPrec, ExprAST *LHS) {
	while (1) {
		int TokPrec = GetTokPrecedence();
		if (TokPrec < ExprPrec) return LHS;
		int BinOp = CurTok;
		getNextToken();
		//ExprAST *RHS = ParsePrimary();
		ExprAST *RHS = ParseUnary();
		if (!RHS) return 0;
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec+1, RHS);
			if (RHS == 0) return 0;
		}
		LHS = new BinaryExprAST(BinOp, LHS, RHS);
	}
}

static ExprAST *ParseExpression() {
	//ExprAST *LHS = ParsePrimary();
	ExprAST *LHS = ParseUnary();
	if (!LHS) return 0;
	return ParseBinOpRHS(0, LHS);
}

/////////////////////////////////////////////////////////////////////////////////
//                             Code Generation                                 //
/////////////////////////////////////////////////////////////////////////////////
static Module *TheModule;
static IRBuilder<> Builder(getGlobalContext());
//static std::map<std::string, Value*> NamedValues;
static std::map<std::string, AllocaInst*> NamedValues;
static FunctionPassManager *TheFPM;

static AllocaInst *CreateEntryBlockAlloca(Function *TheFunction, const std::string &VarName) {
	IRBuilder<> TmpB(&TheFunction->getEntryBlock(), TheFunction->getEntryBlock().begin());
	return TmpB.CreateAlloca(Type::getDoubleTy(getGlobalContext()), 0, VarName.c_str());
}

Value *ErrorV(const char *Str) { 
	Error(Str); 
	return 0; 
}

Value *NumberExprAST::Codegen() {
	return ConstantFP::get(getGlobalContext(), APFloat(Val));
}

Value *VariableExprAST::Codegen() {
	Value *V = NamedValues[Name];
	//return V ? V : ErrorV("Unknown variable name");
	if (V == 0) return ErrorV("Unknown variable name");
	return Builder.CreateLoad(V, Name.c_str());
}

Value *BinaryExprAST::Codegen() {
	if (Op == '=') {
		VariableExprAST *LHSE = dynamic_cast<VariableExprAST*>(LHS);
		if (!LHSE) return ErrorV("destination of '=' must be a variable");
		Value *Val = RHS->Codegen();
		if (Val == 0) return 0;
		Value *Variable = NamedValues[LHSE->getName()];
		if (Variable == 0) return ErrorV("Unknown variable name");
		Builder.CreateStore(Val, Variable);
		return Val;
	}
	Value *L = LHS->Codegen();
	Value *R = RHS->Codegen();
	if (L == 0 || R == 0) return 0;
	switch (Op) {
		case '+': 
			return Builder.CreateFAdd(L, R, "addtmp");
		case '-': 
			return Builder.CreateFSub(L, R, "subtmp");
		case '*': 
			return Builder.CreateFMul(L, R, "multmp");
		case '<':
			L = Builder.CreateFCmpULT(L, R, "cmptmp");
			return Builder.CreateUIToFP(L, Type::getDoubleTy(getGlobalContext()), "booltmp");
		default: 
			//return ErrorV("invalid binary operator");
			break;
	}
	Function *F = TheModule->getFunction(std::string("binary")+Op);
	assert(F && "binary operator not found!");
	Value *Ops[2] = {L, R};
	return Builder.CreateCall(F, Ops, "binop");
}

Value *UnaryExprAST::Codegen() {
	Value *OperandV = Operand->Codegen();
	if (OperandV == 0) return 0;
	Function *F = TheModule->getFunction(std::string("unary")+Opcode);
	if (F == 0) return ErrorV("Unknown unary operator");
	return Builder.CreateCall(F, OperandV, "unop");
}

Value *CallExprAST::Codegen() {
	Function *CalleeF = TheModule->getFunction(Callee);
	if (CalleeF == 0) 
		return ErrorV("Unknown function referenced");
	if (CalleeF->arg_size() != Args.size()) 
		return ErrorV("Incorrect # arguments passed");
	std::vector<Value*> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i) {
		ArgsV.push_back(Args[i]->Codegen());
		if (ArgsV.back() == 0) return 0;
	}
	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}

Value *IfExprAST::Codegen() {
  	Value *CondV = Cond->Codegen();
  	if (CondV == 0) return 0;
  	CondV = Builder.CreateFCmpONE(CondV, ConstantFP::get(getGlobalContext(), APFloat(0.0)), "ifcond");
  	Function *TheFunction = Builder.GetInsertBlock()->getParent();
  	BasicBlock *ThenBB = BasicBlock::Create(getGlobalContext(), "then", TheFunction);
  	BasicBlock *ElseBB = BasicBlock::Create(getGlobalContext(), "else");
  	BasicBlock *MergeBB = BasicBlock::Create(getGlobalContext(), "ifcont");
  	Builder.CreateCondBr(CondV, ThenBB, ElseBB);
  	Builder.SetInsertPoint(ThenBB);
  	Value *ThenV = Then->Codegen();
  	if (ThenV == 0) return 0;
  	Builder.CreateBr(MergeBB);	
	
	ThenBB = Builder.GetInsertBlock();
  
  	TheFunction->getBasicBlockList().push_back(ElseBB);
	
  	Builder.SetInsertPoint(ElseBB);
  	Value *ElseV = Else->Codegen();
  	if (ElseV == 0) return 0;
  	Builder.CreateBr(MergeBB);
	
 	ElseBB = Builder.GetInsertBlock();
  
  	TheFunction->getBasicBlockList().push_back(MergeBB);
  	Builder.SetInsertPoint(MergeBB);
 	PHINode *PN = Builder.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2, "iftmp");
  	PN->addIncoming(ThenV, ThenBB);
  	PN->addIncoming(ElseV, ElseBB);
  	return PN;
}

Value *ForExprAST::Codegen() {
	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
  	Value *StartVal = Start->Codegen();
  	if (StartVal == 0) return 0;
	Builder.CreateStore(StartVal, Alloca);
  	BasicBlock *LoopBB = BasicBlock::Create(getGlobalContext(), "loop", TheFunction);
  	Builder.CreateBr(LoopBB);
  	Builder.SetInsertPoint(LoopBB);
	//BasicBlock *PreheaderBB = Builder.GetInsertBlock();
  	//PHINode *Variable = Builder.CreatePHI(Type::getDoubleTy(getGlobalContext()), 2, VarName.c_str());
  	//Variable->addIncoming(StartVal, PreheaderBB);
  	//Value *OldVal = NamedValues[VarName];
	//NamedValues[VarName] = Variable;
	AllocaInst *OldVal = NamedValues[VarName];
	NamedValues[VarName] = Alloca;
  	if (Body->Codegen() == 0) return 0;
  	Value *StepVal;
  	if (Step) {
    	StepVal = Step->Codegen();
    	if (StepVal == 0) return 0;
  	} else {
    	StepVal = ConstantFP::get(getGlobalContext(), APFloat(1.0));
  	}
  	//Value *NextVar = Builder.CreateFAdd(Variable, StepVal, "nextvar");
  	Value *EndCond = End->Codegen();
  	if (EndCond == 0) return EndCond;
	Value *CurVar = Builder.CreateLoad(Alloca, VarName.c_str());
	Value *NextVar = Builder.CreateFAdd(CurVar, StepVal, "nextvar");
	Builder.CreateStore(NextVar, Alloca);
  	EndCond = Builder.CreateFCmpONE(EndCond, ConstantFP::get(getGlobalContext(), APFloat(0.0)), "loopcond");
  	//BasicBlock *LoopEndBB = Builder.GetInsertBlock();
  	BasicBlock *AfterBB = BasicBlock::Create(getGlobalContext(), "afterloop", TheFunction);
  	Builder.CreateCondBr(EndCond, LoopBB, AfterBB);
  	Builder.SetInsertPoint(AfterBB);
  	//Variable->addIncoming(NextVar, LoopEndBB);
  	if (OldVal)
    	NamedValues[VarName] = OldVal;
  	else
    	NamedValues.erase(VarName);
  	return Constant::getNullValue(Type::getDoubleTy(getGlobalContext()));
}

Value *VarExprAST::Codegen() {
	std::vector<AllocaInst*> OldBindings;
	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
		const std::string &VarName = VarNames[i].first;
		ExprAST *Init = VarNames[i].second;
		Value *InitVal;
		if (Init) {
			InitVal = Init->Codegen();
			if (InitVal == 0) return 0;
		} else {
			InitVal = ConstantFP::get(getGlobalContext(), APFloat(0.0));
		}
		AllocaInst *Alloca = CreateEntryBlockAlloca(TheFunction, VarName);
		Builder.CreateStore(InitVal, Alloca);
		OldBindings.push_back(NamedValues[VarName]);
		NamedValues[VarName] = Alloca;
	}
	Value *BodyVal = Body->Codegen();
	if (BodyVal == 0) return 0;
	for (unsigned i = 0, e = VarNames.size(); i != e; ++i) {
		NamedValues[VarNames[i].first] = OldBindings[i];
	}
	return BodyVal;
}

void PrototypeAST::CreateArgumentAllocas(Function *F) {
	Function::arg_iterator AI = F->arg_begin();
	for (unsigned Idx = 0, e = Args.size(); Idx != e; ++Idx, ++AI) {
		AllocaInst *Alloca = CreateEntryBlockAlloca(F, Args[Idx]);
		Builder.CreateStore(AI, Alloca);
		NamedValues[Args[Idx]] = Alloca;
	}
}

Function *PrototypeAST::Codegen() {
	std::vector<Type*> Doubles(Args.size(), Type::getDoubleTy(getGlobalContext()));
	FunctionType *FT = FunctionType::get(Type::getDoubleTy(getGlobalContext()), Doubles, false);
	Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule);
	if (F->getName() != Name) {
		F->eraseFromParent();
		F = TheModule->getFunction(Name); 
		if (!F->empty()) {
			ErrorF("redefinition of function");
			return 0;
		}
		if (F->arg_size() != Args.size()) {
			ErrorF("redefinition of function with different # args");
			return 0;
		}
	}
	unsigned Idx = 0;
	for (Function::arg_iterator AI = F->arg_begin(); Idx != Args.size(); ++AI, ++Idx) {
		AI->setName(Args[Idx]);
		//NamedValues[Args[Idx]] = AI;
	}
	return F;
}

Function *FunctionAST::Codegen() {
	NamedValues.clear();
	Function *TheFunction = Proto->Codegen();
	if (TheFunction == 0) return 0;
	if (Proto->isBinaryOp()) BinopPrecedence[Proto->getOperatorName()] = Proto->getBinaryPrecedence();
	BasicBlock *BB = BasicBlock::Create(getGlobalContext(), "entry", TheFunction);
	Builder.SetInsertPoint(BB);
	Proto->CreateArgumentAllocas(TheFunction);
	if (Value *RetVal = Body->Codegen()) {
		Builder.CreateRet(RetVal);
		verifyFunction(*TheFunction);
		TheFPM->run(*TheFunction);
		return TheFunction;
	}
	TheFunction->eraseFromParent();
	if (Proto->isBinaryOp()) BinopPrecedence.erase(Proto->getOperatorName());
	return 0;
}

/////////////////////////////////////////////////////////////////////////////////
//                      Top-Level Parsing and JIT Driver                       //
/////////////////////////////////////////////////////////////////////////////////
static ExecutionEngine *TheExecutionEngine;

static void HandleDefinition() {	
	if (FunctionAST *F = ParseDefinition()) {
		if (Function *LF = F->Codegen()) {
			fprintf(stderr, "Read function definition:");
			LF->dump();
		}
	} else {
		getNextToken();
	}
}

static void HandleExtern() {	
	if (PrototypeAST *P = ParseExtern()) {
		if (Function *F = P->Codegen()) {
			fprintf(stderr, "Read extern: ");
			F->dump();
		}
	} else {
		getNextToken();
	}
}

static void HandleTopLevelExpression() {
	if (FunctionAST *F = ParseTopLevelExpr()) {
		if (Function *LF = F->Codegen()) {
			//fprintf(stderr, "Read top-level expression:");
			//LF->dump();
			void *FPtr = TheExecutionEngine->getPointerToFunction(LF);
			double (*FP)() = (double (*)())(intptr_t)FPtr;
			fprintf(stderr, "Evaluated to %f\n", FP());
		}
	} else {
		getNextToken();
	}
}

static void MainLoop() {
	while (1) {
		fprintf(stderr, "ready> ");
		switch (CurTok) {
			case tok_eof:    
				return;
			case ';':        
				getNextToken(); 
				break;
			case tok_def:    
				HandleDefinition(); 
				break;
			case tok_extern: 
				HandleExtern(); 
				break;
			default:
				HandleTopLevelExpression(); 
				break;
		}
	}
}

/////////////////////////////////////////////////////////////////////////////////
//                             Main Driver code                                //
/////////////////////////////////////////////////////////////////////////////////
int main() {
	InitializeNativeTarget();
	LLVMContext &Context = getGlobalContext();
	BinopPrecedence['='] = 2;
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40;
	fprintf(stderr, "ready> ");
	getNextToken();
	TheModule = new Module("my cool jit", Context);
	std::string ErrStr;
	TheExecutionEngine = EngineBuilder(TheModule).setErrorStr(&ErrStr).create();
	if (!TheExecutionEngine) {
		fprintf(stderr, "Could not create ExecutionEngine: %s\n", ErrStr.c_str());
		exit(1);
	}
	FunctionPassManager OurFPM(TheModule);
	TheModule->setDataLayout(TheExecutionEngine->getDataLayout());
	OurFPM.add(new DataLayoutPass(TheModule));
	OurFPM.add(createBasicAliasAnalysisPass());
	OurFPM.add(createPromoteMemoryToRegisterPass());
	OurFPM.add(createInstructionCombiningPass());
	OurFPM.add(createReassociatePass());
	OurFPM.add(createGVNPass());
	OurFPM.add(createCFGSimplificationPass());
	OurFPM.doInitialization();
	TheFPM = &OurFPM;
	MainLoop();
	TheFPM = 0;
	TheModule->dump();	
	return 0;
}