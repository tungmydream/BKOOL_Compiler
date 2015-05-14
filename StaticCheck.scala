// Student: Nguyen Thanh Tung - 51204401
case class MethodType(cName: String, params: List[Type], returnType: Type) extends Type {
    override def toString = "MethodType(" + cName + "," + params + "," + returnType + ")"
}
case class MyClassType(name: String, parent: String, lstAtt: List[Symbol], lstMethod: List[Symbol])
object NullType extends Type
case class Symbol(name: String, typ: Type, kind: Kind)
case class MethodImplement(cla: String, name: String, numParam: Int)

object NeverHappen extends Exception

object Checker {
    
    
    def printSyms(syms: List[Symbol]) {
        if (syms.isEmpty) println("()")
        else {
            println(syms.head)
            printSyms(syms.tail)
        }
    }
    
    def check_program(ast: Program) {
        
        val ioMethods = List[Symbol](
            Symbol("readInt", MethodType("io", List(), IntType), Method),
            Symbol("writeInt", MethodType("io", List(IntType), VoidType), Method),
            Symbol("writeIntLn", MethodType("io", List(IntType), VoidType), Method),
            Symbol("readFloat", MethodType("io", List(), FloatType), Method),
            Symbol("writeFloat", MethodType("io", List(FloatType), VoidType), Method),
            Symbol("writeFloatLn", MethodType("io", List(FloatType), VoidType), Method),
            Symbol("readBool", MethodType("io", List(), BoolType), Method),
            Symbol("writeBool", MethodType("io", List(BoolType), BoolType), Method),
            Symbol("writeBoolLn", MethodType("io", List(BoolType), BoolType), Method),
            Symbol("readStr", MethodType("io", List(), VoidType), Method),
            Symbol("writeStr", MethodType("io", List(StringType), VoidType), Method),
            Symbol("writeStrLn", MethodType("io", List(StringType), VoidType), Method))
            
        val ioClass = MyClassType("io", "", List(), ioMethods)
        
        val lstClass = makeListClass(ast.decl, List(ioClass))
        startCheck(ast, List(),lstClass)
    }

    def makeListClass(dl:List[Decl], lst:List[MyClassType]):List[MyClassType] = {
    	dl match {
    		case List() => lst
    		case head::tail => {
    			val mycls = getClassDecl(head,lst)
    			makeListClass(tail,mycls::lst)
    		}
    	}
    }
    
    /*
     * can throw exception
     */
    def getClassDecl(decl:Decl,lst:List[MyClassType]) = {
    	decl match {
    		case ClassDecl(cls,parent,dl) => {
    			if (lookup(cls.name,lst)) 
    			  throw Redeclared(Class,cls.name)
    			else{
	    			val ml = getListMethod(dl,cls,List())
	    			val al = getListAttribute(dl,List())
	    			MyClassType(cls.name,if (parent!=null) parent.name else "",al,ml)
    			}
    		}
    		case _ => throw NeverHappen
    	}
    }
  
    /*
     * can throw exception
     */
    def getListMethod(dl:List[Decl],cls:Id,sl:List[Symbol]):List[Symbol] = {
    	dl match {
    		case List() => sl
    		case MethodImpDecl(rt,id,prs,_)::tail => {
    		  if(lookupMethodOnMethodList(id.name, prs.length, sl))	
    		    throw Redeclared(Method, id.name)
    		  getListMethod(tail,cls,Symbol(id.name,MethodType(cls.name,getParamList(prs),rt),Method)::sl)//should check if redeclare method
    		}
    		case MethodAbstractDecl(rt,id,prs)::tail => {
    		  if(lookupMethodOnMethodList(id.name, prs.length, sl))  
    		    throw Redeclared(AbstractMethod, id.name)
    		  getListMethod(tail,cls,Symbol(id.name,MethodType(cls.name,getParamList(prs),rt),Method)::sl)//should check if redeclare method
    		}
    		case _::tail => getListMethod(tail,cls,sl)

    	}
    }

    /*
     * can throw exception
     */
    def getListAttribute(dl:List[Decl],sl:List[Symbol]):List[Symbol] = {
    	dl match {
    		case List() => sl
    		case VarDecl(id,typ)::tail => {
    		  if(lookupAttrOnAttrList(id.name, sl))
    		    throw Redeclared(Attribute, id.name)
    		  getListAttribute(tail,Symbol(id.name,typ,Variable)::sl) //should check if redeclare variable
    		}
    		case ConstDecl(id,typ,_)::tail => {
    		  if(lookupAttrOnAttrList(id.name, sl))
    		    throw Redeclared(Attribute, id.name)
    		  getListAttribute(tail,Symbol(id.name,typ,Constant)::sl) //should check if redeclare constant
    		}
    		case _::tail => getListAttribute(tail,sl)
    	}
    }

    /*
     * don't throw exception
     */
    def getParamList(pl:List[Decl]):List[Type] = {
    	pl match {
    		case List() => List()
    		case ParamDecl(_,typ)::tail => typ::getParamList(tail)
    		case _::tail => getParamList(tail)	
    	}
    }

    /*
     * use for first traversal
     * don't throw exception
     */
    def lookup(id:String,lst:List[MyClassType]):Boolean = {
    	lst match {
    		case List() => false
    		case MyClassType(cls,_,_,_)::tail =>if (id == cls) true else lookup(id,tail)
    	}
    }
    
    /*
     * Look up method in a class on method list
     * if successful 	return true
     * else				return false
     * use for first traversal
     * don't throw exception
     */
    def lookupMethodOnMethodList(name:String, numberOfPara:Int, lst:List[Symbol]):Boolean = {
      lst match {
        case Nil => false
        case Symbol(str, ty, ki)::tail => 
          if(str != name) lookupMethodOnMethodList(name, numberOfPara, tail)
          else
            ty match {
            case MethodType(_, paramList, _) => 
              if(numberOfPara == paramList.length)	true
              else	lookupMethodOnMethodList(name, numberOfPara, tail)
            case _ => lookupMethodOnMethodList(name, numberOfPara, tail)
          }
      }
    }
    
    /*
     * Lookup attribute in a class on attribute list
     * if successful	return true
     * else 			return false
     * use for first traversal
     * don't throw exception
     */
    def lookupAttrOnAttrList(name:String, lst:List[Symbol]):Boolean = {
      lst match {
        case Nil => return false
        case Symbol(str, _, _)::tail => 
          if(str == name)	return true
          else 				lookupAttrOnAttrList(name, tail)
      }
    }
    
    var symbolTable : Vector[Symbol] = Vector();
    var scope : Vector[Int] = Vector();
   
    /*
     * call when enter a scope
     * don't throw exception
     */
    def enterScope() = {
      scope = scope:+(symbolTable.length)
    }
    
    /*
     * call when exit a scope
     * don't throw exception
     * 
     */
    def exitScope() = {
      val currScope = scope(scope.length - 1)
      // remove entries in the scope
      while(symbolTable.length != currScope){
        symbolTable = symbolTable.dropRight(1)
      }
      // remove in scope in scope stack
      scope = scope.dropRight(1)
    }
    
    /*
     * clear all data in symbol table and scope
     * 
     */
    def clearAll() = {
      symbolTable = Vector()
      scope = Vector()
    }
    
    /*
     * look up an entry in a scope
     * use for insert function
     * don't throw exception
     * 
     */
    def lookupInCurrScope(entry : Symbol) : Option[Symbol] = {
      for(i <- (symbolTable.length - 1) to scope(scope.length - 1) by -1){
        entry.kind match {
          case Class => {
            if(symbolTable(i).kind == Class && symbolTable(i).name == entry.name)
              return Some(symbolTable(i))
          }
          case Method => {
            entry.typ match {
              case MethodType(_, eParams, _) => {
                symbolTable(i).typ match {
                  case MethodType(_, symParams, _) => {
                    if((symbolTable(i).kind == Method || symbolTable(i).kind == AbstractMethod) && symbolTable(i).name == entry.name && symParams.length == eParams.length)
                    	return Some(symbolTable(i))
                  }
                  case _ =>
                }
              }
              case _ =>
            }
          }
          case AbstractMethod => {
            entry.typ match {
              case MethodType(_, eParams, _) => {
                symbolTable(i).typ match {
                  case MethodType(_, symParams, _) => {
                    if((symbolTable(i).kind == Method || symbolTable(i).kind == AbstractMethod) && symbolTable(i).name == entry.name && symParams.length == eParams.length)
                    	return Some(symbolTable(i))
                  }
                  case _ =>
                }
              }
              case _ =>
            }
          }
          case _ =>
            if(symbolTable(i).kind != Class && symbolTable(i).kind != AbstractMethod && symbolTable(i).kind != Method){
	            if(symbolTable(i).name == entry.name)
	              return Some(symbolTable(i))
            }
        }
      }
      None
    }
    
    /*
     * look up in symbol table
     * don't throw exception
     * 
     */
    def lookupInSymbolTable(entry : Symbol) : Option[Symbol] = {
      for(i <- (symbolTable.length - 1) to 0 by -1){
        entry.kind match {
          case Class => {
            if(symbolTable(i).kind == Class && symbolTable(i).name == entry.name)
              return Some(symbolTable(i))
          }
          case Method => {
            entry.typ match {
              case MethodType(_, eParams, _) => {
                symbolTable(i).typ match {
                  case MethodType(_, symParams, _) => {
                    if((symbolTable(i).kind == Method || symbolTable(i).kind == AbstractMethod) && symbolTable(i).name == entry.name && symParams.length == eParams.length)
                    	return Some(symbolTable(i))
                  }
                }
              }
            }
          }
          case AbstractMethod => {
            entry.typ match {
              case MethodType(_, eParams, _) => {
                symbolTable(i).typ match {
                  case MethodType(_, symParams, _) => {
                    if((symbolTable(i).kind == Method || symbolTable(i).kind == AbstractMethod) && symbolTable(i).name == entry.name && symParams.length == eParams.length)
                    	return Some(symbolTable(i))
                  }
                }
              }
            }
          }
          case _ =>
            if(symbolTable(i).kind != Class && symbolTable(i).kind != AbstractMethod && symbolTable(i).kind != Method){
	            if(symbolTable(i).name == entry.name)
	              return Some(symbolTable(i))
            }
        }
      }
      return None
    }
    
    
    /*
     * Insert a symbol to symbolTable
     * can throw exception
     */
    def insert(sym : Symbol) : Boolean = {
      lookupInCurrScope(sym) match {
        case Some(entry) => {
          throw Redeclared(sym.kind, sym.name)
        }
        case None => {
          symbolTable = symbolTable :+ sym
          return true
        }
      }
    }
    
    
    
    /*
     * look up a class
     * don't throw exception
     * 
     */
    def lookupClassOnClassList(cls: String, lst: List[MyClassType]) : Option[MyClassType] = {
      lst match {
        case Nil => return None
        case head::tail => {
          if(head.name == cls)
            return Some(head)
          else
            lookupClassOnClassList(cls, tail)
        }
      }
    }
    
    /*
     * look up a field in a class on class list
     * use for second traversal
     * don't throw exception
     */
    def lookupFieldOnClassList(fld: String, cName: String, classList: List[MyClassType]) : Option[Symbol] = {
      lookupClassOnClassList(cName, classList) match {
        case None => throw Undeclared(Class, cName)
        case Some(someClass) => {
          val la = someClass.lstAtt
          for(i <- (la.length - 1) to 0 by -1){
            if(fld == la(i).name)
              return Some(la(i))
          }
          val prName = someClass.parent
          if(prName != "")
        	  lookupFieldOnClassList(fld, someClass.parent, classList)
          else
            return None
        }
      }
    }
    
    /*
     * look up a method in a class on class list
     * use for second traversal
     * can throw exception
     * 
     */
    def lookupMethodOnClassList(mName: String, numberOfParams: Int, cName: String, classList: List[MyClassType]) : Option[Symbol] = {
      lookupClassOnClassList(cName, classList) match {
        case None => throw Undeclared(Class, cName)
        case Some(someClass) => {
          val lm = someClass.lstMethod
          for(i <- (lm.length - 1) to 0 by -1){
            if(mName == lm(i).name) {
	            lm(i).typ match {
	              case MethodType(_, params, _) => 
	                if(numberOfParams == params.length)
	                  return Some(lm(i))
	            }
            }
          }
          val prName = someClass.parent
          if(prName != "")
        	  lookupMethodOnClassList(mName, numberOfParams, someClass.parent, classList)
          else
            return None
        }
      }
    }
    
    /*
     * lookup a class on symbol table
     * dont't throw exception
     * 
     */
    def lookupClassOnSymbolTable(cls: String) : Option[Symbol] = {
      for(i <- symbolTable.length - 1 to 0 by -1){
        if(cls == symbolTable(i).name){
	        symbolTable(i).kind match {
	          case Class => return Some(symbolTable(i))
	        }
        }
      }
      return None
    }
    
    
    /*
     * lookup a field on symbol table
     * don't throw exception
     * 
     */
    def lookupFieldOnSymbolTable(fld: String) : Option[Symbol] = {
      for(i <- symbolTable.length - 1 to 0 by -1){
        if(fld == symbolTable(i).name){
	        symbolTable(i).kind match {
	          case Variable => return Some(symbolTable(i))
	          case Constant => return Some(symbolTable(i))
	          case Parameter => return Some(symbolTable(i))
	        }
        }
      }
      return None
    }
    
    /*
     * look up a method on symbol table
     * don't throw exception
     * 
     */
    def lookupMethodOnSymbolTable(mName: String, numberOfParams: Int) : Option[Symbol] = {
      for(i <- symbolTable.length - 1 to 0 by -1){
        if(mName == symbolTable(i).name){
	        symbolTable(i).typ match {
	          case MethodType(_, params, _) => {
	            if(numberOfParams == params.length)
	              return Some(symbolTable(i))
	          }
	        }
        }
      }
      return None
    }
    
    /*
     * check a expression
     * can throw exception
     * 
     */
    def startCheckExpr(e:Expr, cName: String, cList: List[MyClassType]): Type = e match {
      case IntLiteral(_) => return IntType
      case FloatLiteral(_) => return FloatType
      case StringLiteral(_) => return StringType
      case BooleanLiteral(_) => return BoolType
      case SelfLiteral => return ClassType(cName)
      case NullLiteral => return NullType
      case Id(name) =>  {
        if(name == "io")
          return ClassType("io")
        else{
	        lookupInSymbolTable(Symbol(name, IntType , Attribute))  match {
		        case Some(sym) => return sym.typ
		        case None => lookupFieldOnClassList(name, cName, cList) match {
		          case Some(fi) => return fi.typ
		          case None => throw Undeclared(Identifier, name)
		        }
		      }
        }
      }
      
      // unary expression
      case UnaryOp(_, e1) => startCheckExpr(e1, cName: String, cList: List[MyClassType]) match {
        case IntType => return IntType
        case FloatType => return FloatType
        case _ => throw TypeMismatchInExpression(e)
      }
      // binary expression
      case BinaryOp(op, e1, e2) => (op, startCheckExpr(e1, cName, cList), startCheckExpr(e2, cName, cList)) match {
        case ("\\", IntType, IntType) => return IntType
        case ("\\", _, _) => throw TypeMismatchInExpression(e)
        
        case ("%", IntType, IntType) => return IntType
        case ("%", _, _) => throw TypeMismatchInExpression(e)
        
        case ("/", IntType, IntType) => return FloatType
        case ("/", FloatType, FloatType) => return FloatType
        case ("/", FloatType, IntType) => return FloatType
        case ("/", IntType, FloatType) => return FloatType
        case ("/", _, _) => throw TypeMismatchInExpression(e)
        
        case ("+", IntType, IntType) => return IntType
        case ("+", FloatType, FloatType) => return FloatType
        case ("+", IntType, FloatType) => return FloatType
        case ("+", FloatType, IntType) => return FloatType
        case ("+", _, _) => throw TypeMismatchInExpression(e)
        
        case ("-", IntType, IntType) => return IntType
        case ("-", FloatType, FloatType) => return FloatType
        case ("-", IntType, FloatType) => return FloatType
        case ("-", FloatType, IntType) => return FloatType
        case ("-", _, _) => throw TypeMismatchInExpression(e)
        
        case ("*", IntType, IntType) => return IntType
        case ("*", FloatType, FloatType) => return FloatType
        case ("*", IntType, FloatType) => return FloatType
        case ("*", FloatType, IntType) => return FloatType
        case ("*", _, _) => throw TypeMismatchInExpression(e)
        
        case ("==", IntType, IntType) => return BoolType
        case ("==", FloatType, FloatType) => return BoolType
        //case ("==", StringType, StringType) => return BoolType
        case ("==", BoolType, BoolType) => return BoolType
        /*
        case ("==", ArrayType(dimen1,elemType1), ArrayType(dimen2,elemType2)) => {
          if(dimen1 == dimen2 && elemType1 == elemType2)
            return BoolType
          else
              throw TypeMismatchInExpression(e)
        }
        */
        /*
        case ("==", ClassType(a), ClassType(b)) => {
          if(a==b)
            return BoolType
          else
              throw TypeMismatchInExpression(e)
        }
        */
        case ("==", ClassType(a), NullType) => return BoolType
        case ("==", NullType, ClassType(a)) => return BoolType
        case ("==", _, _) => throw TypeMismatchInExpression(e)
        
        case ("<>", IntType, IntType) => return BoolType
        case ("<>", FloatType, FloatType) => return BoolType
        //case ("<>", StringType, StringType) => return BoolType
        case ("<>", BoolType, BoolType) => return BoolType
        /*
        case ("<>", ArrayType(dimen1, elemType1), ArrayType(dimen2,elemType2)) => {
          if(dimen1 == dimen2 && elemType1 == elemType2)
            return BoolType
          else
              throw TypeMismatchInExpression(e)
        }
         */
        /*
        case ("<>", ClassType(a), ClassType(b)) => {
          if(a!=b)
            return BoolType
          else
              throw TypeMismatchInExpression(e)
        }
        */
        
        case("<>", ClassType(a), NullType) => return BoolType
        case ("<>", NullType, ClassType(a))=> return BoolType
        /*
         * a:A; if (a == null) OK
         */
        case ("<>", _, _) => throw TypeMismatchInExpression(e)
        
        case (">", IntType, IntType) => return BoolType
        case (">", FloatType, FloatType) => return BoolType
        case (">", IntType, FloatType) => return BoolType
        case (">", FloatType, IntType) => return BoolType
        case (">", _, _) => throw TypeMismatchInExpression(e)
        
        case ("<", IntType, IntType) => return BoolType
        case ("<", FloatType, FloatType) => return BoolType
        case ("<", IntType, FloatType) => return BoolType
        case ("<", FloatType, IntType) => return BoolType
        case ("<", _, _) => throw TypeMismatchInExpression(e)
        
        case (">=", IntType, IntType) => BoolType
        case (">=", FloatType, FloatType) => BoolType
        case (">=", IntType, FloatType) => BoolType
        case (">=", FloatType, IntType) => BoolType
        case (">=", _, _) => throw TypeMismatchInExpression(e)
        
        case ("<=", IntType, IntType) => BoolType
        case ("<=", FloatType, FloatType) => BoolType
        case ("<=", IntType, FloatType) => BoolType
        case ("<=", FloatType, IntType) => BoolType
        case ("<=", _, _) => throw TypeMismatchInExpression(e)
        
        case ("^", StringType, StringType) => StringType
        case ("^", _, _) => throw TypeMismatchInExpression(e)
      }
      
      case ArrayCell(e1, e2) => (startCheckExpr(e1, cName, cList), startCheckExpr(e2, cName, cList)) match {
        case (ArrayType(_, elemType), IntType) => elemType
        case (_, _) => throw TypeMismatchInExpression(e)
      }
      
      case FieldAccess(e1, fld) => startCheckExpr(e1, cName, cList) match {
        case ClassType(name) => lookupFieldOnClassList(fld.name, name, cList) match {
          case Some(sym) => {
            if(name == cName)
              return sym.typ
            else{
              if(isSubClass(cName, name, cList))
                return sym.typ
              else
                throw CannotAccessPrivateAttribute(fld.name, name)
                
            }            
            	//return sym.typ
          }
          case None => throw Undeclared(Attribute, fld.name)
        }
        case _ => throw TypeMismatchInExpression(e)
      }
      /*hello*/
      //case _ => IntType
      
      case CallExpr(e1, mId, params) => startCheckExpr(e1, cName, cList) match {
        case ClassType(name) => {
	          lookupMethodOnClassList(mId.name, params.length, name, cList) match {
		          case Some(mt) => mt.typ match {
		            case MethodType(_, paramsType, rt) => {
		            		
		            		if(checkParams(params, paramsType, name, cList) == false)
		            		  throw TypeMismatchInExpression(e)
		            		  
		            		
		            		if(rt!=null && rt == VoidType)
		            		  throw TypeMismatchInExpression(e)
		            		
		            		if(rt==null)
		            		  throw TypeMismatchInExpression(e)
		            		
		            		return rt
		            	}
		            case _ => throw TypeMismatchInExpression(e)
		          }
		          case None => throw Undeclared(Method, mId.name)
	          }
          }
        case _ => throw TypeMismatchInExpression(e)
      }
      
      case NewExpr(newId, exprList) => lookupClassOnClassList(newId.name, cList) match {
        case Some(sym) => {
	          if(exprList.length == 0)
	            return ClassType(newId.name)
	            
	          lookupMethodOnClassList(newId.name, exprList.length, newId.name, cList) match {
		          case Some(method) => method.typ match {
		            case MethodType(_, paramsType, _) =>{
		              
		              if(checkParams(exprList, paramsType, newId.name, cList) == false)
		                throw TypeMismatchInExpression(e)
		              
		              return ClassType(newId.name)
		            }
		          }
		          case None => throw Undeclared(Method, newId.name)
	        }
	      }
        case None => throw Undeclared(Class, newId.name)
      }
      
      case _ => throw TypeMismatchInExpression(e)
      
      
    }
    
    /*
     * check parameters of call expression
     * can throw exception
     */
    
    def checkParams(params : List[Expr], require: List[Type], cName: String, classList: List[MyClassType]):Boolean = {
      for(i <- 0 to params.length - 1) {
        (require(i), startCheckExpr(params(i), cName, classList)) match {
          case (IntType, IntType) => 
          case (FloatType, FloatType) => 
          case (StringType, StringType) => 
          case (BoolType, BoolType) => 
          case (FloatType, IntType) => 
          case (ClassType(a), ClassType(b)) => { 
            (lookup(a, classList), lookup(b, classList)) match {
              case (true, true) => {
                if(a != b){
		            if(!isSubClass(b, a, classList))
		              return false
		            else{}
		             // return
            	}
                else{
                  //return
                }
              }
              case (false, _) => throw Undeclared(Class, a)
              case (true, false) => throw Undeclared(Class, b)
            }
          }
          case (ArrayType(dimen1, elemType1),ArrayType(dimen2, elemType2)) => {
              if(dimen1 == dimen2){
                (elemType1, elemType2) match {
                  case (ClassType(myClass1), ClassType(myClass2)) => {
                    (lookup(myClass1, classList), lookup(myClass2, classList)) match {
                      case (true, true) => {
                        if(myClass1 == myClass2){}	//return
                        else{
                          if(isSubClass(myClass2, myClass1, classList))  {}    //return
                          else 
                            return false
                          
                        }
                      }
                      
                      case (false, _) => throw Undeclared(Class, myClass1)
                      
                      case (true, false) => throw Undeclared(Class, myClass2)
                    }
                  }
                  
                  case (IntType, IntType) => //return
                  case (FloatType, FloatType) => //return
                  case (BoolType, BoolType) => //return
                  case (StringType, StringType) => //return
                  case (FloatType, IntType) => //return
                  case (_,_) => return false
                }
              }
              else{
                return false
              }
            }
          
          case (_,_) => return false
        }
      }
      return true
    }
    
    /*
     * check a class is a subclass of a class
     * if yes	return true
     * if no	return false
     * 
     */
    def isSubClass(cName: String, pName: String, classList: List[MyClassType]): Boolean = {

      lookupClassOnClassList(cName, classList) match {
        case Some(MyClassType(_, pn, _, _)) => {
          if(pn != ""){
            if(pn == pName)
              return true
            else
              isSubClass(pn, pName, classList)
          }
          else
            false
        }
        case None => false
      }
    }
    
    /*
     * check a expression list
     * can throw exception
     * 
     */
    def checkExprList(exprList: List[Expr], cName: String, cList: List[MyClassType]):List[Type] = exprList match {
      case Nil => Nil
      case head::tail => {
        startCheckExpr(head, cName, cList)::checkExprList(tail, cName, cList)
      }
    }
    
   
    /*
     * check expression for constant
     * 
     */
   
    
    /*
     * check a statement
     * can throw exception
     * 
     */
    def startCheckStmt(stmt: Stmt, returnType:Type, cName: String, lstClass: List[MyClassType], enter: Boolean) {
      stmt match {
        case Block(declList, stmtList) => {
          if(enter == false)
            enterScope()
            
          for(i <- 0 to declList.length - 1) {
            declList(i) match {
              case VarDecl(varId, varType) => {
                varType match {
                  case ClassType(name) => lookupClassOnClassList(name, lstClass) match {
                    case Some(c) => insert(Symbol(varId.name, varType, Variable))
                    case None => throw Undeclared(Class, name)
                  }
                  case _ => insert(Symbol(varId.name, varType, Variable))
                }
              }
              case ConstDecl(constId, constType, e) => (constType, startCheckExpr(e, cName, lstClass)) match {
		          case (IntType, IntType) => insert(Symbol(constId.name, constType, Constant))
		          case (FloatType, FloatType) => insert(Symbol(constId.name, constType, Constant))
		          case (StringType, StringType) => insert(Symbol(constId.name, constType, Constant))
		          case (BoolType, BoolType) => insert(Symbol(constId.name, constType, Constant))
		          case (FloatType, IntType) => insert(Symbol(constId.name, constType, Constant))
		          case (_,_) => throw TypeMismatchInConstant(ConstDecl(constId, constType, e))
              }
              case _ => throw NeverHappen
            }
          }
          startCheckStmtList(stmtList, returnType, cName, lstClass)
          
          if(enter == false)
        	  exitScope()
        	  
        }
        
        case Assign(lhs, rhs) => lhs match {
          case Id(nameId) => lookupInSymbolTable(Symbol(nameId, IntType, Variable)) match {
            case None => lookupFieldOnClassList(nameId, cName , lstClass) match {
              case None => throw Undeclared(Identifier, nameId)
              case Some(sym1) => sym1.kind match {
                case Constant => throw CannotAssignToConstant(Assign(lhs, rhs))
                case _ => startCheckExprForAssignStmt(stmt, cName, lstClass)
              }
            }
            case Some(sym2) => sym2.kind match {
              case Constant => throw CannotAssignToConstant(Assign(lhs, rhs))
              case _ => startCheckExprForAssignStmt(stmt, cName, lstClass)
            }
          }
          
          case FieldAccess(e1, id1) => startCheckExpr(e1, cName, lstClass) match {
            case ClassType(name1) => {
              lookup(name1, lstClass) match {
                case true => 
                  lookupFieldOnClassList(id1.name, name1, lstClass) match {
                  case None => throw Undeclared(Attribute, id1.name)
                  case Some(sym1) => sym1.kind match {
                    case Constant => throw CannotAssignToConstant(Assign(lhs, rhs))
                    case _ => {
	                      // update on 6/11
                      
	                      startCheckExprForAssignStmt(stmt, cName, lstClass)                    		
                      
	                      if(cName!=name1 && !isSubClass(cName, name1, lstClass))
	                        throw CannotAccessPrivateAttribute(name1, id1.name)
	                      
                      }
                  }
                }
                case false => throw Undeclared(Class, name1)
              }
            }
            case _ => throw TypeMismatchInExpression(FieldAccess(e1, id1))
          }
          
          case _ => startCheckExprForAssignStmt(stmt, cName, lstClass)
          
          
        }
          
     
        case If(e, stmt1, None) => {
          startCheckExpr(e, cName, lstClass) match {
            case BoolType => startCheckStmt(stmt1, returnType, cName, lstClass, false)
            case _ => throw TypeMismatchInStatement(stmt)
          }
        }
        
        case If(e, stmt1, Some(stmt2)) => {
          startCheckExpr(e, cName, lstClass) match {
            case BoolType => {
              startCheckStmt(stmt1, returnType, cName, lstClass, false)
              startCheckStmt(stmt2, returnType, cName, lstClass, false)
            }
            case _ => throw TypeMismatchInStatement(stmt)
          }
        }
        
        case While(e, stmt1) => startCheckExpr(e, cName, lstClass) match {
          case BoolType => startCheckStmt(stmt1, returnType, cName, lstClass, false)
          case _ => throw TypeMismatchInStatement(stmt)
        }
        
        case Repeat(stmtList, e) => startCheckExpr(e, cName, lstClass) match {
          case BoolType => startCheckStmtList(stmtList, returnType, cName, lstClass)
          case _ => throw TypeMismatchInStatement(stmt)
        }
        
        case For(name, e1, _, e2, loop) => startCheckExpr(name, cName, lstClass) match {
          case IntType => startCheckExpr(e1, cName, lstClass) match {
            case IntType => startCheckExpr(e2, cName, lstClass) match {
              case IntType => startCheckStmt(loop, returnType, cName, lstClass, false)
              case _ => throw TypeMismatchInStatement(stmt)
            }
            case _ => throw TypeMismatchInStatement(stmt)
          }
          case _ => throw TypeMismatchInStatement(stmt)
        }
        
        case Break => return
        
        case Continue => return
        
        case Return(e) => returnType match {
          case VoidType => throw TypeMismatchInStatement(stmt)
          case IntType => startCheckExpr(e, cName, lstClass) match {
            case IntType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          case FloatType => startCheckExpr(e, cName, lstClass) match {
            case IntType => return
            case FloatType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          case StringType => startCheckExpr(e, cName, lstClass) match {
            case StringType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          case BoolType => startCheckExpr(e, cName, lstClass) match {
            case BoolType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          case ArrayType(dimen1, elemType1) => startCheckExpr(e, cName, lstClass) match {
            case ArrayType(dimen2, elemType2) => {
              if(dimen1 == dimen2){
                (elemType1, elemType2) match {
                  case (ClassType(myClass1), ClassType(myClass2)) => {
                    (lookup(myClass1, lstClass), lookup(myClass2, lstClass)) match {
                      case (true, true) => {
                        if(myClass1 == myClass2)	return
                        else{
                          if(isSubClass(myClass2, myClass1, lstClass))      return
                          else throw TypeMismatchInStatement(stmt)
                          
                        }
                      }
                      
                      case (false, _) => throw Undeclared(Class, myClass1)
                      
                      case (true, false) => throw Undeclared(Class, myClass2)
                    }
                  }
                  
                  case (IntType, IntType) => return
                  case (FloatType, FloatType) => return
                  case (BoolType, BoolType) => return
                  case (StringType, StringType) => return
                  case (FloatType, IntType) => return
                  case (_,_) => throw TypeMismatchInStatement(stmt)
                }
              }
              else{
                throw TypeMismatchInStatement(stmt)
              }
            }
            case _ => throw TypeMismatchInStatement(stmt)
          }
          
          case ClassType(name1) => 
            lookupClassOnClassList(name1, lstClass) match {
            case Some(myClass1) => {
              startCheckExpr(e, cName, lstClass) match {
	            case ClassType(name2) => 
	              lookupClassOnClassList(name2, lstClass) match {
	              case Some(myClass2) => {
	                if(name1 == name2)
	                  return
	                else{
	                  if(isSubClass(name2, name1, lstClass))
	                    return
	                  else{
	                    throw TypeMismatchInStatement(stmt)
	                  }
	                }
	              }
	              case None => throw Undeclared(Class, name2)
	            }
	            
	            // update on 6/11
	            case NullType => return
	            case _ => throw TypeMismatchInStatement(stmt)
	          }
            }
            case None => Undeclared(Class, name1)
            
          }
            
          
          case _ => throw TypeMismatchInStatement(stmt)
        }
        
        case Call(e, mt, params) => startCheckExpr(e, cName, lstClass) match { 
          case ClassType(name) => {
            lookupMethodOnClassList(mt.name, params.length, name, lstClass) match {
              case Some(sym) => sym.typ match {
                case MethodType(_, paramsType, rt) => {
                  
                  if(checkParams(params, paramsType, name, lstClass) == false)
                    throw TypeMismatchInStatement(stmt)
                  
                  
                  if(rt!=null && rt != VoidType)
                    throw TypeMismatchInStatement(stmt)
                 if(rt==null)
                   throw TypeMismatchInStatement(stmt)
                    
                }
                case _ => throw Undeclared(Method, mt.name)
              }
              case None => throw Undeclared(Method, mt.name)
            }
          }
          case _ => throw TypeMismatchInStatement(stmt)
        }
        
        
       /*hello*/ case _ =>	throw TypeMismatchInStatement(stmt)
        
      }
    }
    
    /*
     * check a assign statement
     * 
     */
    def startCheckExprForAssignStmt(stmt: Stmt, cName: String, lstClass: List[MyClassType])  {
      stmt match {
        case Assign(lhs, rhs) => {
          startCheckExpr(lhs, cName, lstClass) match {
          case VoidType => throw TypeMismatchInStatement(stmt)
          case ClassType(name1) => 
            lookupClassOnClassList(name1, lstClass) match {
            case Some(myClass1) => {
              startCheckExpr(rhs, cName, lstClass) match {
	            case ClassType(name2) => 
	              lookupClassOnClassList(name2, lstClass) match {
	              case Some(myClass2) => {
	                if(name1 == name2)
	                  return
	                else{
	                  if(isSubClass(name2, name1, lstClass))
	                    return
	                  else{
	                    throw TypeMismatchInStatement(stmt)
	                  }
	                }
	              }
	              case None => throw Undeclared(Class, name2)
	            }
	            case _ => throw TypeMismatchInStatement(stmt)
	          }
            }
            case None => Undeclared(Class, name1)
            
          }
          case ArrayType(dimen1, elemType1) => startCheckExpr(rhs, cName, lstClass) match {
            case ArrayType(dimen2, elemType2) => {
              if(dimen1 == dimen2){
                (elemType1, elemType2) match {
                  case (ClassType(myClass1), ClassType(myClass2)) => {
                    (lookup(myClass1, lstClass), lookup(myClass2, lstClass)) match {
                      case (true, true) => {
                        if(myClass1 == myClass2)	return
                        else{
                          if(isSubClass(myClass2, myClass1, lstClass))      return
                          else throw TypeMismatchInStatement(stmt)
                          
                        }
                      }
                      
                      case (false, _) => throw Undeclared(Class, myClass1)
                      
                      case (true, false) => throw Undeclared(Class, myClass2)
                    }
                  }
                  
                  case (IntType, IntType) => return
                  case (FloatType, FloatType) => return
                  case (BoolType, BoolType) => return
                  case (StringType, StringType) => return
                  case (FloatType, IntType) => return
                  case (_,_) => throw TypeMismatchInStatement(stmt)
                }
              }
              else{
                throw TypeMismatchInStatement(stmt)
              }
            }
            case _ => throw TypeMismatchInStatement(stmt)
          }
          
          case IntType => startCheckExpr(rhs, cName, lstClass) match {
            case IntType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          
          case FloatType => startCheckExpr(rhs, cName, lstClass) match {
            case FloatType => return
            case IntType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          case BoolType => startCheckExpr(rhs, cName, lstClass) match {
            case BoolType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          
          case StringType => startCheckExpr(rhs, cName, lstClass) match {
            case StringType => return
            case _ => throw TypeMismatchInStatement(stmt)
          }
          
          case _ => throw TypeMismatchInStatement(stmt)
            
        }
      }
        case _ => throw TypeMismatchInStatement(stmt)
      }
    }
   
    
    /*
     * check a stament list
     * can throw exception
     * 
     */
    def startCheckStmtList(stmtList: List[Stmt], returnType:Type, cName: String, lstClass: List[MyClassType]) {
      stmtList match {
        case Nil => return
        case head::tail => {
          startCheckStmt(head, returnType, cName, lstClass, false)
          startCheckStmtList(tail, returnType, cName, lstClass)
        }
      }
    }
    
   /*
    * 
    */
    
    /*
     * check a declaration in class
     * can throw exception
     * 
     */
    def startCheckDeclOfClass(decl: Decl, cName: String, lstClass: List[MyClassType]) {
      decl match {
        case VarDecl(varId, varType) => {
           varType match {
	          case ClassType(name) => lookupClassOnClassList(name, lstClass) match {
	            case Some(c) => insert(Symbol(varId.name, varType, Variable))
	            case None => throw Undeclared(Class, name)
	          }
	          case _ => insert(Symbol(varId.name, varType, Variable))
           }
         }
        case ConstDecl(constId, constType, e) => (constType, startCheckExpr(e, cName, lstClass)) match {
          case (IntType, IntType) => insert(Symbol(constId.name, constType, Constant))
          case (FloatType, FloatType) => insert(Symbol(constId.name, constType, Constant))
          case (StringType, StringType) => insert(Symbol(constId.name, constType, Constant))
          case (BoolType, BoolType) => insert(Symbol(constId.name, constType, Constant))
          case (FloatType, IntType) => insert(Symbol(constId.name, constType, Constant))
          case (_,_) => throw TypeMismatchInConstant(decl)
        }
        case MethodAbstractDecl(rt, mtName, params) => {
          rt match {
	          case ClassType(name) => lookupClassOnClassList(name, lstClass) match {
	            case Some(c) => insert(Symbol(mtName.name, MethodType(cName, getParamList(params), rt), AbstractMethod))
	            case None => throw Undeclared(Class, name)
	          }
	          case _ => insert(Symbol(mtName.name, MethodType(cName, getParamList(params), rt), AbstractMethod))
           }
         }
        case MethodImpDecl(rt, mtName, params, body) => {
          rt match {
	          case ClassType(name) => lookupClassOnClassList(name, lstClass) match {
	            case Some(c) => insert(Symbol(mtName.name, MethodType(cName, getParamList(params), rt), Method))
	            case None => throw Undeclared(Class, name)
	          }
	          //case null => insert(Symbol(mtName.name, MethodType(cName, getParamList(params), VoidType), Method))
	          case _ => insert(Symbol(mtName.name, MethodType(cName, getParamList(params), rt), Method))
           }
          
          enterScope()
          
          for(i <- 0 to params.length - 1){
            params(i).paramType match {
	          case ClassType(name1) => lookupClassOnClassList(name1, lstClass) match {
	            case Some(c) => insert(Symbol(params(i).id.name, params(i).paramType, Parameter))
	            case None => throw Undeclared(Class, name1)
	          }
	          
	          case _ => insert(Symbol(params(i).id.name, params(i).paramType, Parameter))
            }
	      }
          
          startCheckStmt(body, rt, cName, lstClass, true)
          
          if(rt!=null && rt!= VoidType && checkReturn(body) == false)
            throw MethodNotReturn(mtName.name)
          
          
          exitScope()
        }
      }
    }
    
    /*
     * check a method has return or not
     */
    def checkReturn(stmt:Stmt) : Boolean = {
      stmt match {
        case Block(_, stmtList) => {
          for(i <- 0 to stmtList.length - 1){
            stmtList(i) match {
              case Return(_) => return true
              case If(_,_,None) => return false
              case If(_, stmt1, Some(stmt2)) =>{
                if(checkReturn(stmt1) == true && checkReturn(stmt2) == true)
                  return true
                else
                    return false
              }
              case _ =>
            }
          }
          return false
        }
        case Return(_) => return true
        case _ => return false
        
      }
    }
    
    
    /*
     * check a decl list
     * use for second traversal
     * can throw exception
     * 
     */
    def startCheckDeclListOfClass(declList: List[Decl], cName: String, lstClass: List[MyClassType]) {
      declList match {
        case Nil => return
        case head::tail => {
          startCheckDeclOfClass(head, cName,lstClass)
          startCheckDeclListOfClass(tail, cName, lstClass)
          
        }
      }
    }

    /*
     * check class list
     * use for second traversal
     * can throw exception
     * 
     */
    def startCheckClassList(declList: List[Decl], lstClass: List[MyClassType]) {
      declList match {
        case Nil => return
        case ClassDecl(cName, pName, dList)::tail => {
          if(pName != null){
	          if(!lookup(pName.name, lstClass))
	            throw Undeclared(Class, pName.name)
          }
     
          
          insert(Symbol(cName.name, ClassType(cName.name), Class))
          
          enterScope()
          startCheckDeclListOfClass(dList, cName.name, lstClass)
          exitScope()
          
         startCheckClassList(tail, lstClass)
         
          
        }
        case _ => throw NeverHappen
      }
    }
    
    
    
  //This function will check other constraints as Undeclared identifier, type mismatch,... 
  /*
   * check program
   * use for second traversal
   */
  def startCheck(ast: Program, envs: List[Symbol], lstClass: List[MyClassType]) {
    try{
       enterScope()
       startCheckClassList(ast.decl, lstClass)
       exitScope()
    }catch{
	    case Undeclared(k, n) => {
	      clearAll()
	      throw Undeclared(k,n)
	    }
	    case CannotAssignToConstant(s) => {
	      clearAll()
	      throw CannotAssignToConstant(s)
	    }
	    case TypeMismatchInStatement(s) => {
	      clearAll()
	      throw TypeMismatchInStatement(s)
	    }
	    case TypeMismatchInExpression(e) => {
	      clearAll()
	      throw TypeMismatchInExpression(e)
	    }
	    case Redeclared(k, d) => {
	      clearAll()
	      throw Redeclared(k, d)
	    }
	    case TypeMismatchInConstant(c) => {
	      clearAll()
	      throw TypeMismatchInConstant(c)
	    }
	    case CannotAccessPrivateAttribute(c, m) => {
	      clearAll()
	      throw CannotAccessPrivateAttribute(c, m)
	    }
	    case MethodNotReturn(m) => {
	      clearAll()
	      throw MethodNotReturn(m)
	    }
	    case _ =>{
	      clearAll()
	     
	    }
    }
  }

    
}
