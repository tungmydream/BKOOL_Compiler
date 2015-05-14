//update: 22/07/2014
trait Kind
case object Class extends Kind
case object AbstractMethod extends Kind
case object Method extends Kind
case object Parameter extends Kind
case object Constant extends Kind
case object Variable extends Kind
case object Identifier extends Kind
case object Attribute extends Kind

/* These exception classes are used to throw when an error is detected */

/* 
//    k: the kind of the redeclared identifnier; it must be one of the eight above kinds
//    n: the name of the redeclared identifier; 
*/
case class Undeclared(k: Kind, n: String) extends Exception
case class Redeclared(k: Kind, n: String) extends Exception
case class CannotAssignToConstant(s: Assign) extends Exception
case class TypeMismatchInExpression(exp: Expr) extends Exception
case class TypeMismatchInStatement(stmt: Stmt) extends Exception 
case class TypeMismatchInConstant(cons: Decl) extends Exception

/* These exception classes are used by KSTN only */
case class CannotAccessPrivateAttribute(cName: String, attr: String) extends Exception
case class MethodNotReturn(m: String) extends Exception

