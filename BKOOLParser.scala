// Student: Nguyen Thanh Tung - MSSV: 51204401
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers 
import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.input.CharArrayReader.EofCh
//import ASTUtils

// BKOOLTokens for BKOOL language, student should not modified this interface
trait BKOOLTokens extends StdTokens {
  // Adapted from StdTokens
	case class FloatLit(chars: String) extends Token {
    override def toString = "FloatLit: "+chars
	}
  case class IntLit(chars: String) extends Token {
    override def toString = "IntLit: "+chars
  }
  case class BooleanLit(chars: String) extends Token {
    override def toString = "BooleanLit: " + chars
  }
  case class MyStringLit(chars: String) extends Token {
    override def toString = chars
  }
  case class UnclosedString(chars: String) extends Token {
    override def toString = "ErrorToken(Unclosed string: " + chars + ")"
  }
  case class IllegalTab(chars: String) extends Token {
    override def toString = "ErrorToken(Illegal tab in string: " + chars + ")"
  }
}

// TODO Copy your lexer here
class BKOOLLexer extends StdLexical with BKOOLTokens {
  import scala.util.parsing.input.CharArrayReader.EofCh


  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = {
      val source = in.source
      val offset = in.offset
      (r findPrefixMatchOf (source.subSequence(offset, source.length))) match {
        case Some(matched) =>
          Success(source.subSequence(offset, offset + matched.end).toString,
            in.drop(matched.end))
        case None =>
          Failure("string matching regex `" + r + "' expected but `" + in.first + "' found", in.drop(0))
      }
    }
  }

  // TODO student add code here to complete token
  reserved ++= List("bool", "break", "class", "continue", "do", "downto", "else",
		  		"extends", "float", "for", "if", "integer", "new", "repeat", "string",
		  		"then", "to", "until", "while", "return", "true", "false", "void", "null",
		  		"self", "final", "abstract");

   delimiters ++= List("+", "-", "*", "/", "\\", "%", ":=", "==", "<=", ">=",
		  			"<>", "<", ">", "&&", "!", "||", "^",
		  			"[", "]", "{", "}", "(", ")", ";", "::", ":", ".", ",", "=");

  override def token: Parser[Token] = {
    // Adapted from StdLexical
    (
    regex("([0-9]+)((\\.[0-9]*)((E|e)(\\+|\\-)?[0-9]+)|((E|e)(\\+|\\-)?[0-9]+)|(\\.[0-9]*))".r)	^^ { FloatLit(_)}
    |regex("[0-9]+".r)			^^ { IntLit(_) }
    |regex("(true|false)([A-Za-z]|[0-9]|_)+".r) ^^  (processIdent(_))
	|regex("true|false".r)				^^ { BooleanLit(_)}
	|regex("([A-Za-z]|_)([A-Za-z]|[0-9]|_)*".r)  		^^ {processIdent(_)}
	|regex("""(\")([^\n\r\f\t\"\\]|\\b|\\f|\\r|\\n|\\t|\\\"|\\\\)*(\")""".r)		^^ { MyStringLit(_)}
	|regex("""(\")([^\n\r\f\t\"\\]|\\b|\\f|\\r|\\n|\\t|\\\"|\\\\)*""".r)	^^ { UnclosedString(_)} 
	|EofCh 						^^^ EOF
    |delim

  )
  }

  override def whitespace: Parser[Any] = rep(
    whitespaceChar
    | '#' ~ rep(chrExcept(EofCh, '#', '\n'))
    | '(' ~ '*' ~ comment
    | '(' ~ '*' ~> failure("unclosed comment")
)

  override protected def comment: Parser[Any] = (
    '*' ~ ')' ^^ { case _ => ' ' }
    | chrExcept(EofCh) ~ comment)

}

import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers 

class BKOOLParser extends StdTokenParsers {
	type Tokens = BKOOLTokens
	val lexical = new BKOOLLexer
	def getLexical: BKOOLLexer = lexical

	//Student MUST NOT modify this method
	def show(result: ParseResult[Program]): String = {
		result match {
			case Failure(msg, next) =>
				"Error at line "+ next.pos.line +" col "+ next.pos.column + ":" + msg
			case Error(msg,next) => 
				"Fatal error at line "+ next.pos.line +" col "+ next.pos.column + ":" + msg
			case a => {val b =a.toString; val i = b.indexOf(':');b.substring(i+2)}
		}
	}  
  
	def parse(s:String) = phrase(program)(new lexical.Scanner(s))
 // For BKOOLParser, students may modify the following.

	
	//******************** Program structure *********************
  def program: Parser[Program] =  rep(classDecl)   ^^ { case a => new Program(a)}
  
  //def decl : Parser[Decl] = classDecl  | constDecl | varDecl | methodImpDecl | methodAbstractDecl
  //******************** Class Declaration **********************
  def classDecl: Parser[ClassDecl] = ("class" ~> ident) ~ opt("extends" ~> ident) ~ ("{" ~> listOfMembers <~ "}")	^^{
	case id ~ Some(sup) ~ d => ClassDecl(Id(id), Id(sup), d)
	case id ~ None ~ d => ClassDecl(Id(id), null, d)
  } 
  
  def listOfMembers: Parser[List[Decl]] = rep(methodAbstractDecl | methodImpDecl |  constDecl | varDecl)	^^{
    case listDecl => listDecl match {
      case Nil => List()
      case head :: tail => tail.foldLeft(head)((x,y) => x++y)
    }
  }
  
  
  //******************** Abstract method prototype ****************************
  def methodAbstractDecl : Parser[List[Decl]] = ("abstract" ~> allType) ~ ident ~ ("(" ~> paramDeclList <~ ")" ~ ";")		^^{
    case a ~ b ~ c => List(MethodAbstractDecl(a, Id(b), c))
  }
  
  //******************** Method declaration ****************************
  def methodImpDecl : Parser[List[Decl]] = methodConstructorDecl  | methodNonConstructorDecl
  
  def methodConstructorDecl : Parser[List[Decl]] = ident ~ ("(" ~> paramDeclList <~ ")") ~ blockStmtForConstructor		^^{
    case a ~ b ~ c => List(MethodImpDecl(null, Id(a), b, c))
  }
  
  def methodNonConstructorDecl : Parser[List[Decl]] = allTypeNoVoid ~ ident ~ ("(" ~> paramDeclList <~ ")") ~ blockStmt		^^{
    case a ~ b ~ c ~ d => List(MethodImpDecl(a, Id(b), c, d))
  }		| voidType ~ ident ~ ("(" ~> paramDeclList <~ ")") ~ blockStmtForConstructor		^^{
		    case a ~ b ~ c ~ d => List(MethodImpDecl(a, Id(b), c, d))
		  }	
  
  //******************** Constant declaration ****************************
  def constDecl : Parser[List[ConstDecl]] = ("final" ~> constType) ~ ident ~ ("=" ~> expr <~ ";")		^^{
    case a ~ b ~ c => List(ConstDecl(Id(b), a, c))
  }
  
  //******************** Variable declaration ****************************
  def varDecl : Parser[List[VarDecl]] = rep1sep(ident, ",") ~ (":" ~> varType  <~ ";")		^^{
    case il ~ a => il.map(x => VarDecl(Id(x), a))
  }
  
  //********************Method Implement***************************
  //def methodImplement : Parser[Any] = allType ~ ident ~ "::" ~ ident ~ "(" ~ paramDeclList ~ ")" ~ blockStmt
 
  // expression
  
  def expr : Parser[Expr] = opt(equalOrNot ~ ("<" | ">" | "<=" | ">=")) ~ equalOrNot		^^{
    case None ~ e => e
    case Some(a) ~ e => BinaryOp(a._2, a._1, e)
  }
  
  def equalOrNot : Parser[Expr] = opt(orAnd ~ ("==" | "<>")) ~ orAnd		^^{
    case None ~ e => e
    case Some(a) ~ e => BinaryOp(a._2, a._1, e)
  }
  
  def  orAnd : Parser[Expr] = AddSubBinOp ~ rep(("||" | "&&") ~ AddSubBinOp)		^^{
    case a ~ il => if(il == Nil) a else il.foldLeft(a)((x,y) => y match {
													      case c ~ d => BinaryOp(c, x, d)
													    })
  }
  
  def AddSubBinOp : Parser[Expr] = mulAndDiv ~ rep(("+" | "-") ~ mulAndDiv)		^^{
    case a ~ il => if(il == Nil) a else il.foldLeft(a)((x,y) => y match {
													      case c ~ d => BinaryOp(c, x, d)
													    })
  }
  
  def mulAndDiv : Parser[Expr] = concat ~ rep(("*" | "/" | "\\" | "%") ~ concat)		^^{
    case a ~ list => if(list == Nil) a else list.foldLeft(a)((x,y) => y match {
													      case c ~ d => BinaryOp(c, x, d)
													    })
  }
  
  def concat : Parser[Expr] = differ ~ rep("^" ~ differ)	^^{
    case a ~ il => if(il == Nil) a else il.foldLeft(a)((x,y) => y match {
													      case c ~ d => BinaryOp(c, x, d)
													    })
  }
  
  def differ : Parser[Expr] = opt("!") ~ addSubUna 	^^{
    case None ~ a => a
    case Some(a) ~ b => UnaryOp(a, b)
  }
  
  def addSubUna : Parser[Expr] = opt("+" | "-") ~ index  ^^{
    case None ~ a => a
    case Some(a) ~ b => UnaryOp(a, b)
  }
  
  def index : Parser[Expr] = memberAccess ~ opt("[" ~> expr <~ "]") ^^{ 
    case mA ~ None => mA
    case mA ~ Some(e) => ArrayCell(mA, e)
  }
  
  def memberAccess : Parser[Expr] = newExpr ~ rep("." ~> id ~ opt("(" ~> repsep(expr, ",") <~ ")"))		^^{
    case newExpr ~ list => list match {
      case Nil => newExpr
      case _ => list.foldLeft(newExpr)((x,y) => y match {
        case i ~ None => FieldAccess(x, i)
        case i ~ Some(listExpr) => CallExpr(x, i, listExpr)
      })
    }
  }
    
  def newExpr : Parser[Expr] = ("new" ~> id) ~ ("(" ~> repsep(expr, ",") <~ ")") ^^{
    case i ~ listExpr => NewExpr(i, listExpr)
  }		| other

  
  def other : Parser[Expr] = ("(" ~> expr <~ ")") 	^^{ case e => e}	| ident ^^{ case a => Id(a) } | literal
  
  
  // statement
  
  def stmt : Parser[Stmt] = blockStmt   | assStmt | ifStmt | whileStmt | repeatStmt | forStmt |
  							breakStmt | conStmt | returnStmt | methodInvocationStmt	^^{ case a => a}
  							
  // block statement
  							
  def blockStmt : Parser[Block] = "{" ~> rep(constDecl | varDecl) ~ rep(stmt) <~ "}"		^^{
    case a ~ b => a match {
      case Nil => Block(List(), b)
      case head::tail => Block(tail.foldLeft(head)((x,y) => x++y), b)
    }
  }
  
  def blockStmtForConstructor : Parser[Block] = "{" ~> rep(constDecl | varDecl) ~ listOfStmtForConstructor <~ "}"		^^{
    case a ~ b => a match {
      case Nil => Block(List(), b)
      case head::tail => Block(tail.foldLeft(head)((x,y) => x++y), b)
    }
  }
  
  //def e : Parser[Expr] = selfLit
  
  def listOfStmtForConstructor : Parser[List[Stmt]] = rep(blockStmt  | assStmt | ifStmt | whileStmt | repeatStmt | forStmt |
  							breakStmt | conStmt  | methodInvocationStmt)		^^{ case a => a}
  
  // assignemt statement
  
  def assStmt : Parser[Assign] = lhs ~ (":=" ~> expr <~ ";")		^^{ case a ~ b => Assign(a, b)}
  
  def id : Parser[Id] = ident ^^{ case i => Id(i)}
  
  
  def lhs : Parser[LHS] = arrayCell | fieldAccess	| id
  
  def fieldAccess : Parser[FieldAccess] = (newExpr <~ ".") ~ rep(id ~ opt("(" ~> repsep(expr, ",") <~ ")") <~ ".") ~ id	^^{
    case newE ~ list ~ i1 => list match {
      case Nil => FieldAccess(newE, i1)
      case _ => FieldAccess(list.foldLeft(newE)((x,y) => y match {
        case i2 ~ None => FieldAccess(x, i2)
        case i2 ~ Some(listExpr) => CallExpr(x, i2, listExpr)
      }), i1)
    }
  }
  
 def arrayCell : Parser[ArrayCell] = memberAccess ~ ("[" ~> expr <~ "]")	^^{
   case e1 ~ e2 => ArrayCell(e1,e2)
 }

 // def arrayCell : Parser[ArrayCell] = id ~ ("[" ~> expr <~ "]") ^^{ case a ~ b => ArrayCell(a,b)}
 
  // if statement
  
  def ifStmt : Parser[If] = ("if" ~> expr <~ "then") ~ stmt ~ opt("else" ~> stmt)	^^{
    case a ~ b ~ c => If(a, b, c)
  }
  
  // while statement
  
  def whileStmt : Parser[While] = ("while" ~> expr <~ "do") ~ stmt	^^{ case a ~ b => While(a, b)}
  
  // repeat statement
  
  def repeatStmt : Parser[Repeat] = ("repeat" ~> rep1(stmt) <~ "until") ~ (expr <~";")	^^{ 
    case stmtList ~ e => Repeat(stmtList, e)
    }
  
  // for statement
  
  def forStmt : Parser[For] = ("for" ~> ident <~ ":=") ~ expr ~ ("to" | "downto") ~ expr ~ ("do" ~> stmt)	^^{
    case a ~ b ~ c ~ d ~ e => c match {
      case "to" => For(Id(a), b, true, d, e)
      case "downto" => For(Id(a), b, false, d, e)
    }
  }
  
  // break statement
  
  def breakStmt[Break] = "break" ~ ";"		^^{ case _ => Break}
  
  // continue statement
  
  def conStmt[Continue] = "continue" ~ ";"		^^{ case _ => Continue}
  
  // return statement
  
  def returnStmt : Parser[Return] = "return" ~> expr <~ ";"		^^{ case a => Return(a)}
  
  // method invocation statement
  
  def methodInvocationStmt : Parser[Call] = (newExpr <~ ".") ~ rep(id ~ opt("(" ~> repsep(expr, ",") <~ ")") <~ ".") ~ id ~ ("(" ~> repsep(expr, ",") <~ ")" ~ ";") ^^{
    case newE ~ list ~ i1 ~ listExpr1 => list match {
      case Nil => Call(newE, i1, listExpr1)
      case _ => Call(list.foldLeft(newE)((x,y) => y match {
        case i2 ~ None => FieldAccess(x, i2)
        case i2 ~ Some(listExpr2) => CallExpr(x, i2, listExpr2)
      }), i1, listExpr1)
    }
  }
  

      //----------------------------LITERAL---------------------------------
  def literal: Parser[Literal] = floatLit | intLit | boolLit | myStringLit | nullLit | selfLit
	
  def boolLit: Parser[BooleanLiteral] = elem("bool", _.isInstanceOf[lexical.BooleanLit])  ^^ {
      case x => BooleanLiteral(java.lang.Boolean.parseBoolean(x.chars))
  }
	
  def floatLit: Parser[FloatLiteral] = elem("float", _.isInstanceOf[lexical.FloatLit]) ^^ {
      case x => FloatLiteral(java.lang.Float.parseFloat(x.chars))
  }
  
  def intLit: Parser[IntLiteral] = elem("integer", _.isInstanceOf[lexical.IntLit]) ^^ {
      case x => IntLiteral(java.lang.Integer.parseInt(x.chars))
  }
  
  def myStringLit: Parser[StringLiteral] = elem("string", _.isInstanceOf[lexical.MyStringLit]) ^^ {
        case x => StringLiteral(x.chars.substring(1, x.chars.length() - 1))
  }
  
  def nullLit[NullLiteral] = "null"		^^{ case _ => NullLiteral }
  
  def selfLit[SelfLiteral] = "self"		^^{ case _ => SelfLiteral }
  
    //------------------------------Type------------------------------------
  /*
  def vtype:Parser[Type] = intType

  def intType[IntType] =   "integer" ^^ {case _ => IntType}
  */
  //def typeForConstDecl: Parser[Any] = priTypeNoVoid
  
  //def typeForVarDecl: Parser[Any] =  arrayType |  classType | priTypeNoVoid
  
  //def allType : Parser[Any] =  arrayType | classType | priType
  
  //def priType : Parser[Any] = voidType | priTypeNoVoid
  
  //def priTypeNoVoid : Parser[Any] = "string" | "bool" | "integer" | "float"
  
  def allType[Type] =  arrayType | classType | stringType | boolType | intType | floatType | voidType	^^{
    case a => a
  }
  
  def allTypeNoVoid[Type] = arrayType | classType | stringType | boolType | intType | floatType	^^{
    case a => a
  }
  
  def varType[Type] = arrayType | classType | stringType | boolType | intType | floatType	^^{
    case a => a
  }
  
  def constType[Type] = stringType | boolType | intType | floatType	^^{
    case a => a
  }
  
  def stringType[StringType] = "string"		^^{ case _ => StringType}
  
  def boolType[BoolType] = "bool"			^^{ case _ => BoolType}
  
  def intType[IntType] = "integer"			^^{ case _ => IntType}
  
  def floatType[FloatType] = "float"		^^{	case _ => FloatType}
  
  def voidType[VoidType] = "void"			^^{ case _ => VoidType}
  
  def classType[ClassType] = ident			^^{ case a => ClassType(a)}
  
  def arrayType[ArrayType] = (classType | stringType | boolType | intType | floatType) ~ ("[" ~> intLit <~ "]")		^^{
    case a ~ b => ArrayType(b, a)
  }
  
  //-----------------------Para and para list ---------------
  
  def paramDeclList : Parser[List[ParamDecl]] = repsep(paramDecl, ";")	^^{case paramList => paramList match { case Nil => List()
  																											case head :: tail => tail.foldLeft(head)((x,y) => x++y)}
  }
  
  def paramDecl	: Parser[List[ParamDecl]] = (rep1sep(ident, ",") <~ ":") ~ (arrayType | classType | stringType | boolType | intType | floatType) ^^{
    case identList ~ t => identList.map(x => ParamDecl(Id(x), t))
  }
  
}
