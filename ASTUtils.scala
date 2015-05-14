	import util.parsing.combinator.JavaTokenParsers
	
	
	trait Context
	trait AST 
	
	case class Program(val decl: List[Decl]) extends AST {
	        override def toString = "Program(" + (if (decl.isEmpty) "[]" else "[" + (decl.head.toString /: decl.tail)(_ + "," + _.toString()) + "]") + ")"
	}
	
	
	abstract class Decl extends AST {
	}
	case class VarDecl(val variable: Id, val varType: Type) extends Decl {
	        override def toString = "VarDecl(" + variable + "," + varType + ")"
	}
	case class ConstDecl(val id: Id, val constType: Type, val const: Expr) extends Decl {
	        override def toString = "ConstDecl(" + id + "," + constType + "," + const + ")"
	}
	case class ParamDecl(val id: Id, val paramType: Type) extends Decl {
	        override def toString = "param(" + id + "," + paramType + ")"
	}
	case class ClassDecl(val name: Id, val parent: Id, val decl: List[Decl]) extends Decl {
	        override def toString = "ClassDecl(" + name + "," + (if (parent != null) parent + "," else "") + (if (decl.isEmpty) "[])" else "[" + (decl.head.toString /: decl.tail)(_ + "," + _.toString()) + "])")
	}
	
	case class MethodAbstractDecl(val returnType: Type, val name: Id, val param: List[ParamDecl]) extends Decl {
	        override def toString = "MethodAbstractDecl(" + name + ",[" + (if (!param.isEmpty) (param.head.toString /: param.tail)(_ + "," + _.toString) else "") + "]," + (if (returnType != null) returnType + "," else "") + ")"
	}
	
	//method implement
	case class MethodImpDecl(val returnType: Type, val name: Id, val param: List[ParamDecl], val body: Stmt) extends Decl {
	        override def toString = "MethodImpDecl(" + "," + name + ",[" + (if (!param.isEmpty) (param.head.toString /: param.tail)(_ + "," + _.toString) else "") + "]," + (if (returnType != null) returnType + "," else "") + body + ")"
	}
	
	
	/*        TYPE        */
	trait Type extends AST
	object IntType extends Type {
	        override def toString = "IntType"
	}
	object FloatType extends Type {
	        override def toString = "FloatType"
	}
	object BoolType extends Type {
	        override def toString = "BoolType"
	}
	object StringType extends Type {
	        override def toString = "StringType"
	}
	object VoidType extends Type {
	        override def toString = "VoidType"
	}
	case class ArrayType(val dimen: IntLiteral, val eleType: Type) extends Type {
	        override def toString = "ArrayType(" + eleType.toString() + "," + dimen + ")"
	}
	case class ClassType(val classType: String) extends Type {
	        override def toString = "ClassType(" + classType + ")"
	}
	
	
	/*        expr        */
	trait Expr extends AST {
	        override def toString = "Expr"
	}
	case class BinaryOp(op: String, left: Expr, right: Expr) extends Expr {
	        override def toString = "BinaryOp(" + op + "," + left + "," + right + ")"
	}
	case class UnaryOp(op: String, body: Expr) extends Expr {
	        override def toString = "UnaryOp(" + op + "," + body + ")"
	}
	case class NewExpr(val name: Id, val exprs: List[Expr]) extends Expr {
	        override def toString = "NewExp(" + name + "," + "[" + (if (!exprs.isEmpty) (exprs.head.toString /: exprs.tail)(_ + "," + _.toString) else "") + "])"
	}
	case class CallExpr(val cName: Expr, val method: Id, val params: List[Expr]) extends LHS {
	        override def toString = "CallExp(" + (if (cName == null) "self" else cName) + "," + method + "," + "[" + (if (!params.isEmpty) (params.head.toString /: params.tail)(_ + "," + _.toString) else "") + "])"
	}
	
	
	//LHS ------------------------
	trait LHS extends Expr
	case class Id(val name: String) extends LHS {
	        override def toString = name
	}
	// element access
	case class ArrayCell(name: Expr, expr: Expr) extends LHS {
	        override def toString = "ArrayCell(" + name + "," + expr + ")"
	}
	
	case class FieldAccess(name: Expr, field: Id) extends LHS {
	        override def toString = "FieldAccess(" + name + "," + field + ")"
	}
	/*        STMT        */
	trait Stmt extends AST
	case class Block(val decl: List[Decl], val stmt: List[Stmt]) extends Stmt {
	        override def toString = "Block(" + "[" + (if (!decl.isEmpty) (decl.head.toString /: decl.tail)(_ + "," + _.toString)  else "") + "]," +
	                "[" + (if (!stmt.isEmpty) (stmt.head.toString /: stmt.tail)(_ + "," + _.toString) else "") + "])"
	}
	case class Assign(val leftHandSide: LHS, val expr: Expr) extends Stmt {
	        override def toString = "Assign(" + leftHandSide + "," + expr + ")"
	}
	case class If(val expr: Expr, val thenStmt: Stmt, val elseStmt: Option[Stmt]) extends Stmt {
	        override def toString = "If(" + expr + "," + thenStmt + "," + (if (!elseStmt.isEmpty) elseStmt.get else "") + ")"
	}
	case class Call(val parent: Expr, val method: Id, val params: List[Expr]) extends Stmt {
	        override def toString = "Call(" + parent + "," + method + "," + "[" + (if (!params.isEmpty) (params.head.toString /: params.tail)(_ + "," + _.toString) else "") + "])"
	}
	case class While(val expr: Expr, val loop: Stmt) extends Stmt {
	        override def toString = "While(" + expr + "," + loop + ")"
	}
	case class Repeat(val loop: List[Stmt], val expr: Expr) extends Stmt {
	        override def toString = "Repeat(" + expr + "," + "[" + (if (!loop.isEmpty) (loop.head.toString /: loop.tail)(_ + "," + _.toString) else "") + "])"
	}
	//up = true for "to"
	//up = false for "downto"
	case class For(val name: Id, val expr: Expr, val up: Boolean, val expr2: Expr, val loop: Stmt) extends Stmt {
	        override def toString = "For(" + name + "," + expr + "," + up + "," + expr2 + "," + loop + ")"
	}
	object Break extends Stmt {
	        override def toString = "Break()"
	}
	object Continue extends Stmt {
	        override def toString = "Continue()"
	}
	case class Return(val expr: Expr) extends Stmt {
	        override def toString = "Return(" + expr + ")"
	}
	
	
	/*        LITERAL        */
	trait Literal extends Expr
	case class IntLiteral(val value: Int) extends Literal {
	        override def toString = "IntLit(" + value.toString + ")"
	}
	case class FloatLiteral(val value: Float) extends Literal {
	        override def toString = "FloatLit(" + value.toString + ")"
	}
	case class StringLiteral(val value: String) extends Literal {
	        override def toString = "StringLit(" + value + ")"
	}
	case class BooleanLiteral(val value: Boolean) extends Literal {
	        override def toString = "BoolLit(" + value.toString() + ")"
	}
	object NullLiteral extends Literal {
	        override def toString = "Null()"
	}
	object SelfLiteral extends Literal {
	        override def toString = "Self()"
	}
	
	
	//For making AST from String of AST
	abstract class Item
	case class ItemList(items: List[Item]) extends Item {
	        override def toString = "[" + items.mkString(",") + "]"
	}
	case class Ident(name: String) extends Item {
	        override def toString = name
	}
	case class Term(name: String, args: List[Item]) extends Item {
	        override def toString = name + "(" + args.mkString(",") + ")"
	}
	case class IntItem(value: IntLiteral) extends Item {
	        override def toString = "" + value
	}
	case class FloatItem(value: FloatLiteral) extends Item {
	        override def toString = "" + value
	}
	case class StringItem(value: StringLiteral) extends Item {
	        override def toString = "" + value // TODO: Quotation
	}
	case class BoolItem(value: BooleanLiteral) extends Item {
	        override def toString = "" + value // TODO: Quotation
	}
	
	
	
	
	
