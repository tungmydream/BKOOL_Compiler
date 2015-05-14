// Project name MP
// Create by: nhphung
// Create date: Aug 27, 2012
// Language: Scala
// Description: This file is about recognizer for MP language, see MP language specification for more information
// Student: Nguyen Thanh Tung - 51204401

import scala.util.matching.Regex
import scala.util.parsing.combinator.lexical.StdLexical
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.input.CharArrayReader.EofCh

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
    '*' ~ ')'	^^ { case _ => ' '}
    | chrExcept(EofCh) ~ comment
  )
  
}


 
import scala.util.parsing.combinator.token.StdTokens
import scala.util.parsing.combinator.syntactical.StdTokenParsers 

class BKOOLRecognizer extends StdTokenParsers {
  type Tokens = BKOOLTokens
  val lexical = new BKOOLLexer
  def getLexical: BKOOLLexer = lexical

  def show(result: ParseResult[Any]): String = {
    result match {
      case Failure(msg, next) =>
        "Error at line "+ next.pos.line +" col "+ next.pos.column
	  case Error(msg,next) => 
		"Fatal error at line "+ next.pos.line +" col "+ next.pos.column
      case _ => "Successful"
    }
  }  
  
  def parse(s:String) = phrase(program)(new lexical.Scanner(s))
 // For BKOOLRecognizer, students may modify the following.
  //******************** Program structure *********************
  def program: Parser[Any] =  rep(decl) 
  
  def decl : Parser[Any] = classDecl  | constDecl | varDecl | methodImplement
  //******************** Class Declaration **********************
  def classDecl: Parser[Any] = "class" ~ ident ~ opt("extends" ~ ident) ~ "{" ~ listOfMembers ~ "}" 
  
  def listOfMembers: Parser[Any] = rep(abstractMethodPrototype | methodDecl |  attributes)
  
  //******************** Atributes ****************************
  def attributes: Parser[Any] = immutableAttribute | mutableAttribute
  
  //******************** Abstract method prototype ****************************
  def abstractMethodPrototype : Parser[Any] = "abstract" ~ allType ~ ident ~ "(" ~ paraList ~ ")" ~ ";"
  
  //******************** Method declaration ****************************
  def methodDecl : Parser[Any] = (allType ~ ident) ~ "(" ~ paraList ~ ")" ~ methodBody |
  								 ident ~ "(" ~ paraList ~ ")" ~ methodConstructorBody 
  
  //******************** Mutable Attribute ****************************
  def mutableAttribute: Parser[Any] = varDecl
  
  //******************** Immutable Atributes ****************************
  def immutableAttribute: Parser[Any] = constDecl
  
  //******************** Constant declaration ****************************
  def constDecl : Parser[Any] = "final" ~ typeForConstDecl ~ ident ~ "=" ~ expr ~ ";"
  
  //******************** Variable declaration ****************************
  def varDecl : Parser[Any] = identList ~ ":" ~ typeForVarDecl  ~ ";"
  
  def identList : Parser[Any] = rep1sep(ident, ",")
  
  //********************Method Implement***************************
  def methodImplement : Parser[Any] = allType ~ ident ~ "::" ~ ident ~ "(" ~ paraList ~ ")" ~ blockStmt
 
  
  //******************** Type ****************************
  def typeForConstDecl: Parser[Any] = priTypeNoVoid
  
  def typeForVarDecl: Parser[Any] =  arrayType |  classType | priTypeNoVoid
  
  def allType : Parser[Any] =  arrayType | classType | priType
  
  def priType : Parser[Any] = voidType | priTypeNoVoid
  
  def priTypeNoVoid : Parser[Any] = "string" | "bool" | "integer" | "float"
  
  def voidType: Parser[Any] = "void"
    
  def arrayType : Parser[Any] = (classType | priTypeNoVoid) ~ "[" ~ intLit ~ "]"
  
  def classType : Parser[Any] = ident
  
  def paraList : Parser[Any] = repsep(para, ";")
  
  def para : Parser[Any] = identList ~ ":" ~ typeForVarDecl 
  
  
  
  
  //******************** Literal ****************************
  def literal : Parser[Any] = intLit | floatLit | boolLit | myStringLit
  
  def intLit: Parser[Any] = elem("integer", _.isInstanceOf[lexical.IntLit]) 
  
  def floatLit: Parser[Any] = elem("float", _.isInstanceOf[lexical.FloatLit]) 

  def boolLit: Parser[Any] = elem("boolean", _.isInstanceOf[lexical.BooleanLit])
  
  def myStringLit : Parser[Any] = elem("string", _.isInstanceOf[lexical.MyStringLit])
  
  
  // expression
   
  def expr : Parser[Any] = temp1 ~ opt(("<" | ">" | "<=" | ">=") ~ temp1)
  
  def temp1 : Parser[Any] = temp2 ~ opt(("==" | "<>") ~ temp2)
  
  def temp2 : Parser[Any] = rep(temp3 ~ ("||" | "&&")) ~ temp3
  
  def temp3 : Parser[Any] = rep(temp4 ~ ("+" | "-")) ~ temp4
  
  def temp4 : Parser[Any] = rep(temp5 ~ ("*" | "/" | "\\" | "%")) ~ temp5 
  
  def temp5 : Parser[Any] = rep(temp6 ~ "^") ~ temp6 
  
  def temp6 : Parser[Any] = opt("!") ~ temp7 
  
  def temp7 : Parser[Any] = opt("+" | "-") ~ temp8  
  
  def temp8 : Parser[Any] = temp9 ~ opt("[" ~ expr ~ "]")
  
  def temp9 : Parser[Any] = rep1(temp10 ~ ".") ~ ident ~ opt( "(" ~ listOfExpr ~ ")" ) | temp10
  
  def temp10 : Parser[Any] = temp11 | "new" ~ ident ~ "(" ~ listOfExpr ~ ")" | ident ~ "[" ~ expr ~ "]" 
  
  def temp11 : Parser[Any] = "(" ~ expr ~ ")" | operand
  
  //def temp12 : Parser[Any] = listOfExpr | operand
  
  def listOfExpr : Parser[Any] = repsep(expr, ",")
  
  def operand : Parser[Any] =  "null" | "self" | ident | literal
  
  
  // body of method
  
  def methodBody : Parser[Any] = blockStmt
  
  // body of constructor method
  
  def methodConstructorBody : Parser[Any] = blockStmtForConstructor
  
  // statement
  
  def stmt : Parser[Any] = blockStmt  | assStmt | ifStmt | whileStmt | repeatStmt | forStmt |
  							breakStmt | conStmt | returnStmt | methodInvocationStmt 
  							
  // block statement
  							
  def blockStmt : Parser[Any] = "{" ~ listOfDecl ~ listOfStmt ~ "}"
  
  def blockStmtForConstructor : Parser[Any] = "{" ~ listOfDecl ~ listOfStmtForConstructor ~ "}"
  
  def listOfDecl : Parser[Any] = rep(constDecl | varDecl)
  
  def listOfStmt : Parser[Any] = rep(stmt)
  
  def listOfStmtForConstructor : Parser[Any] = rep(blockStmt  | assStmt | ifStmt | whileStmt | repeatStmt | forStmt |
  							breakStmt | conStmt | methodInvocationStmt)
  
  // assignemt statement
  
  def assStmt : Parser[Any] = lhs ~ ":=" ~ expr ~ ";"
  
  def lhs : Parser[Any] = (memberAccess  | arrayMember | ident) // lhs = ident or MemberAccess or memberOfArray
  
  def memberAccess : Parser[Any] = ("self" | ident ~ opt("[" ~ expr ~ "]")) ~ "." ~ rep1sep(ident ~ opt("[" ~ expr ~ "]"), ".") 
  
  def arrayMember : Parser[Any] = opt(("self" | ident ~ opt("[" ~ expr ~ "]")) ~ "." ~ rep(ident ~ opt("[" ~ expr ~ "]") ~ ".")) ~ ident ~ "[" ~ expr ~ "]"
  // if statement
  
  def ifStmt : Parser[Any] = "if" ~ expr ~ "then" ~ stmt ~ opt("else" ~ stmt)
  
  // while statement
  
  def whileStmt : Parser[Any] = "while" ~ expr ~ "do" ~ stmt
  
  // repeat statement
  
  def repeatStmt : Parser[Any] = "repeat" ~ rep1(stmt) ~ "until" ~ expr ~";"
  
  // for statement
  
  def forStmt : Parser[Any] = "for" ~ ident ~ ":=" ~ expr ~ ("to" | "downto") ~ expr ~ "do" ~ stmt
  
  // break statement
  
  def breakStmt : Parser[Any] = "break" ~ ";"
  
  // continue statement
  
  def conStmt : Parser[Any] = "continue" ~ ";"
  
  // return statement
  
  def returnStmt : Parser[Any] = "return" ~ expr ~ ";"
  
  // method invocation statement
  
  def methodInvocationStmt : Parser[Any] =  ("self" | "new" ~ ident ~ "(" ~ listOfExpr ~ ")" | ident ~ opt("[" ~ expr ~ "]")) ~ "." ~ rep(ident ~ opt("[" ~ expr ~ "]") ~ ".")  ~ ident ~ "(" ~ listOfExpr ~ ")" ~ ";"
  											
  
  
 }