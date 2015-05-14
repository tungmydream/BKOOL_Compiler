// Student: Nguyen Thanh Tung - 51204401
/*
*	CodeGenerator
*/

import scala.collection.mutable.StringBuilder
import scala.collection.immutable.List
import scala.io.Source
import java.io.File
import java.io.PrintWriter

class CodeGenerator(input: Program, outdir: String, emitter: Emitter) {

  val env = Checker.makeListClass(input.decl, Checker.initial)
  // Don't change the prototype of this method 
  def run: Unit = {
    val output = generateCode(input.decl, env, List(Symbol("io", ClassType("IO"), Attribute, "IO")))
    printOutput(output, outdir)
  }

  def printOutput(out: List[(String, String)], outdir: String) = {
    for (i <- out) {
      val dest = new PrintWriter(new File(outdir + "\\" + i._1 + ".j"))
      dest.print(i._2)
      dest.close()
    }
  }

  def generateCode(lst: List[Decl], env: List[MyClassType], symTable: List[Symbol]) = {
    lst.map(x => generateClass(x.asInstanceOf[ClassDecl], symTable))
  }

  def generateClass(decl: ClassDecl, symTable: List[Symbol]) = {
    val className = decl.name.name

    val classHeader = emitter.emitPROLOG(decl)
    //val classCInit = emitter.emitCLINIT(decl,List())
    val classInit = if(!checkHasConstructor(decl.name.name, decl.decl)) generateInit(MethodType(decl.name.name, List(), VoidType), decl)
    				else ""
    val classFields = generateClassFields(decl)
    val classMethods = generateMethods(decl, symTable)

    val classBody = classHeader + "\n" /* + classCInit + "\n" */ + classFields + "\n" + classInit + "\n" + classMethods
    (className, classBody)
  }

  
  
  def initConstantAndArrayField(decl:ClassDecl, frame:Frame) = {
   
    var result = new StringBuffer()
    var constList : List[Symbol] = List()
    val declList = decl.decl
    for(i<- 0 to (declList.length - 1)){
      declList(i) match {
        case VarDecl(name, ArrayType(dimen, elemType)) => {
          result.append(emitter.emitALOAD(0, frame))
		  result.append(emitter.emitICONST(dimen.value, frame))
		  result.append(emitter.emitNEWARRAY(elemType, frame))
		  result.append(emitter.emitPUTFIELD(name.name, ArrayType(dimen, elemType), frame, ClassType(decl.name.name)))
        }
        case ConstDecl(name, ty, e) => {
          constList = constList:+Symbol(name.name, ty, Attribute)
          val code = generateExpression(e, decl, constList, frame,true)._1
          result.append(emitter.emitALOAD(0, frame))
          result.append(code)
          result.append(emitter.emitPUTFIELD(name.name, ty, frame, ClassType(decl.name.name)))
        }
        case _ =>
      }
    }
    result.toString
  }
  
  
  
  def generateInit(mthType: MethodType, decl: ClassDecl) = {
   
    
    var result = new StringBuffer()
	val in = decl.decl
	
    val tmp = mthType
    val frame = new Frame(false)
    frame.enterScope(true)
    result.append(emitter.emitMETHOD("<init>", tmp, false, frame))
    result.append(emitter.emitLIMITSTACK(3))
    result.append(emitter.emitLIMITLOCAL(1))
    result.append(emitter.emitVAR(0, "this", ClassType(decl.name.name), 0, 1))
    result.append(emitter.emitLABEL(0))
    result.append(emitter.emitALOAD(0, frame))
    val invokeSpecial = if(decl.parent == null) emitter.emitINVOKESPECIAL(frame)
    					else emitter.emitINVOKESPECIAL(decl.parent.name + "/<init>", MethodType("", List(), VoidType), frame)
    result.append(invokeSpecial)
    result.append(initConstantAndArrayField(decl, frame))
    result.append(emitter.emitLABEL(1))
    result.append(emitter.emitRETURN(null, frame))
    result.append(emitter.emitENDMETHOD())
    frame.exitScope()
    result.toString()
    
  }
  def checkHasConstructor(cName: String, declList: List[Decl]) : Boolean = {
    for(i <- 0 to (declList.length - 1)){
      declList(i) match {
        case MethodImpDecl(rt, mtName, _, _) => {
          if(rt == null)
            return true
        }
        case _ =>
      }
    }
    return false
  }

  //Return list of attributes with array type
  
  def getArrayAttributes(declList: List[Decl]):List[Symbol] = {
    declList match {
      case Nil => Nil
      case VarDecl(name, ArrayType(dimen, elemType))::tail => Symbol(name.name, ArrayType(dimen, elemType), Variable, Some(""))::getArrayAttributes(tail)
      //case ConstDecl(name, ArrayType(dimen, elemType), _)::tail => Symbol(name.name, ArrayType(dimen, elemType), Constant, Some(""))::getArrayAttributes(tail)
      case _ ::tail => getArrayAttributes(tail)
    }
  }

  def generateClassFields(decl: ClassDecl) = {
    /*
    val al = getAttributes(decl.decl)
    al.foldLeft("")((a, b) => a + emitter.emitINSTANCEFIELD(b.name, b.typ, b.kind, b.obj.get))
    */
    val declList = decl.decl
	var result = new StringBuffer()
	for(i <- 0 to (declList.length - 1)){
		declList(i) match {
			case VarDecl(name, ty) => {
				result.append(emitter.emitINSTANCEFIELD(name.name, ty, Variable, ""))
			}
			case ConstDecl(name, ty, e) => {
			 
				result.append(emitter.emitINSTANCEFIELD(name.name, ty, Constant, ""))
			}
			case _ =>
		}
	}
	result.toString()
  }

  def getAttributes(decl: List[Decl]): List[Symbol] = decl match {
    case List() => List()
    case head :: tail => head match {
      case VarDecl(i, t) => Symbol(i.name, t, Variable, Some("")) :: getAttributes(tail)
      case _ => getAttributes(tail)
    }
  }
 

  def generateMethods(decl: ClassDecl, sym: List[Symbol]) = {
    val ml = getMethods(decl.decl)
    ml.foldLeft("")((a, b) => a + generateMethod(b, decl, sym))
  }

  def getMethods(decl: List[Decl]): List[MethodImpDecl] = decl match {
    case List() => List()
    case head :: tail => head match {
      case MethodImpDecl(r, n, pl, b) => MethodImpDecl(r, n, pl, b) :: getMethods(tail)
      case _ => getMethods(tail)
    }
  }

  def generateMethod(decl: MethodImpDecl, cls: ClassDecl, sym: List[Symbol]) = {
    if(decl.returnType != null){
	    val isMain = decl.name.name == "main" && decl.param.length == 0
	    val frame = new Frame(isMain)
	    val prolog = emitter.emitMETHOD(decl.name.name, MethodType(cls.name.name, Checker.getParamList(decl.param), decl.returnType), isMain, frame)
	    val params = if(isMain) generateParam(decl.param, sym, frame)
	    			else generateParam(ParamDecl(Id("this"), ClassType(cls.name.name))::decl.param, sym, frame)
	    val startLabel = frame.getStartLabel
	    val endLabel = frame.getEndLabel
	    val label0 = emitter.emitLABEL(startLabel)
	    val body = generateStmt(decl.body, cls, params._2, true, frame, false)
	    val label1 = emitter.emitLABEL(endLabel)
	    val returnType = decl.returnType
	    val ret =  emitter.emitRETURN(returnType, frame)
	    val limits = emitter.emitLIMITSTACK(frame.getMaxOpStackSize) +
	      emitter.emitLIMITLOCAL(frame.getMaxIndex)
	    val endmt = emitter.emitENDMETHOD
	    prolog + params._1 + label0 +  body + label1 + ret + limits + endmt
    }
    else{
      // constructor
      val frame = new Frame(false)
      val prolog = emitter.emitMETHOD("<init>", MethodType(cls.name.name, Checker.getParamList(decl.param), VoidType), false, frame)
      val params = generateParam(ParamDecl(Id("this"), ClassType(cls.name.name))::decl.param, sym, frame)
      val startLabel = frame.getStartLabel
      val endLabel = frame.getEndLabel
	  val label0 = emitter.emitLABEL(startLabel)
	  val parent = if(cls.parent == null) "java/lang/Object" else cls.parent.name
	  val initConstArray = initConstantAndArrayField(cls, frame)
	  val body = emitter.emitALOAD(0, frame) + emitter.emitINVOKESPECIAL(parent + "/<init>", MethodType(cls.name.name, List(), VoidType), frame) + 
	  			initConstArray + generateStmt(decl.body, cls, params._2, true, frame, false)
	  val label1 = emitter.emitLABEL(endLabel)
	  val ret = emitter.emitRETURN(VoidType, frame)
	  val limits = emitter.emitLIMITSTACK(frame.getMaxOpStackSize) + 
	  			emitter.emitLIMITLOCAL(frame.getMaxIndex)
	  val endmt = emitter.emitENDMETHOD
	  prolog + params._1 + label0  + body + label1 + ret + limits + endmt
    }
  }

  def generateParam(dl: List[ParamDecl], sym: List[Symbol], frame: Frame) = {
    frame.enterScope(true)
    if (frame.isMain)
      (emitter.emitVAR(frame.getNewIndex(), "arg", "[Ljava/lang/String;", frame.getStartLabel(), frame.getEndLabel()), sym)
    else
      dl.foldLeft(("", sym))((a, b) => {
        val index = frame.getNewIndex
        (a._1 + emitter.emitVAR(index, b.id.name, b.paramType, frame.getStartLabel, frame.getEndLabel),
          Symbol(b.id.name, b.paramType, Parameter, index.toString) :: a._2)
      })
  }

  def generateVariable(dl: List[Decl], sym: List[Symbol], frame: Frame) = {
    dl.foldLeft(("", sym))((a, b) => b match {
      case VarDecl(id, t) => {
        val index = frame.getNewIndex
        (a._1 + emitter.emitVAR(index, id.name, t, frame.getStartLabel, frame.getEndLabel),
          Symbol(id.name, t, Variable, index.toString) :: a._2)
      }
	  case ConstDecl(name, ty, e) => {
		val index = frame.getNewIndex
		(a._1 + emitter.emitVAR(index, name.name, ty, frame.getStartLabel, frame.getEndLabel),
          Symbol(name.name, ty, Variable, index.toString) :: a._2)
	  }
      case _ => a
    })
  }

  def generateStmtList(sl: List[Stmt], cls: ClassDecl, sym: List[Symbol], isBody: Boolean, frame: Frame, isConst:Boolean) =
    sl.foldLeft("")((a, b) => a + generateStmt(b, cls, sym, isBody, frame,isConst))

  def generateStmt(stmt: Stmt, cls: ClassDecl, sym: List[Symbol], isBody: Boolean, frame: Frame, isConst:Boolean): String = {
    stmt match {
      case Block(dl, sl) => {
        if (!isBody) frame.enterScope(false)
        val decl = generateVariable(dl, sym, frame)
        val startLabel = frame.getStartLabel
        val endLabel = frame.getEndLabel
        val label0 = emitter.emitLABEL(startLabel)
        val stmts = generateStmtList(sl, cls, decl._2, false, frame,isConst)
        val label1 = emitter.emitLABEL(endLabel)
		
		var initArrayAndConstant = new StringBuffer()
		for(i <- 0 to (dl.length - 1)){
			dl(i) match {
				case VarDecl(name, ArrayType(dimen, elemType)) => {
					val sym = lookup(name.name, decl._2)
					val index = sym.obj.get.toInt
					initArrayAndConstant.append(emitter.emitINITARRAY(index, ArrayType(dimen, elemType), frame))
				}
				case ConstDecl(name, ty, e) => {
					val sym = lookup(name.name, decl._2)
					val index = sym.obj.get.toInt
					val code = generateStmt(Assign(name, e), cls, decl._2, false, frame,isConst)
					initArrayAndConstant.append(code)
				}
				case _ =>
			}
		}
        frame.exitScope()
        decl._1 + label0 + initArrayAndConstant.toString() + stmts + label1
      }

      case Assign(lhs, exp) => {
        lhs match {
	          case Id(n) => {
	            val expCode = generateExpression(exp, cls, sym, frame,isConst)
	            val expSym = expCode._2
	            val symbol = lookup(n, sym)
	            val lhsCode = emitter.emitWRITEVAR(symbol, frame)
	            if(symbol.typ == FloatType && expSym.typ == IntType)
	            	expCode._1 + emitter.emitI2F(frame) + lhsCode
	            
	            else
	            	  expCode._1 + lhsCode
	            
	          }
         
	        case ArrayCell(e1, e2) => (generateExpression(e1, cls, sym, frame,isConst), generateExpression(e2, cls, sym, frame,isConst)) match {
	          case ((e1Code, Symbol(_, ArrayType(_, elemType), _, _)), (e2Code, _)) => {
	            val expCode = generateExpression(exp, cls, sym, frame,isConst)
	            val expSym = expCode._2
	            val lhsCode = e1Code + e2Code
	            if(elemType == FloatType && expSym.typ == IntType)
	              lhsCode + expCode._1 + emitter.emitI2F(frame) + emitter.emitTASTORE(elemType, frame)
	            
	            else
	            	lhsCode + expCode._1 + emitter.emitTASTORE(elemType, frame)
	             
	          }
	          
	          // error on this case
	          case _ => ""
	        }
	        
	      
	        case FieldAccess(e, field) => generateExpression(e, cls, sym, frame,isConst) match {
	          case (eCode, Symbol(_, ClassType(cName), _, _)) => {
	        	  val fieldSymbol = lookupField(field.name, ClassType(cName), env)
		          val expCode = generateExpression(exp, cls, sym, frame,isConst)
		          val expSym = expCode._2
		          if(fieldSymbol.typ == FloatType && expSym.typ == IntType)
		            eCode + expCode._1 + emitter.emitI2F(frame) + emitter.emitPUTFIELD(field.name, fieldSymbol.typ, frame, ClassType(cName))
		          
		          else
		            eCode + expCode._1 + emitter.emitPUTFIELD(field.name, fieldSymbol.typ, frame, ClassType(cName))
		          
		          
		          
	          }
	          
	          // error on this case
	          case _ => ""
	        }
	     
        }
      }

      case If(e, stmt1, None) => {
        val expCode = generateExpression(e, cls, sym, frame,isConst)
        val label1 = frame.getNewLabel()
        val stmt1Code = generateStmt(stmt1, cls, sym, false, frame,isConst)
        expCode._1 + emitter.emitIFEQ(label1, frame) + stmt1Code + emitter.emitLABEL(label1)

      }

      case If(e, stmt1, Some(stmt2)) => {
        val expCode = generateExpression(e, cls, sym, frame,isConst)
        val label1 = frame.getNewLabel()
        val label2 = frame.getNewLabel()
        val stmt1Code = generateStmt(stmt1, cls, sym, false, frame,isConst)
        val stmt2Code = generateStmt(stmt2, cls, sym, false, frame,isConst)
        expCode._1 + emitter.emitIFEQ(label1, frame) + stmt1Code + emitter.emitGOTO(label2, frame) +
          emitter.emitLABEL(label1) + stmt2Code + emitter.emitLABEL(label2)
      }

      case While(e, stmt1) => {
        frame.enterLoop()
        val conLabel = frame.getContinueLabel()
        val breakLabel = frame.getBreakLabel()
        val expCode = generateExpression(e, cls, sym, frame,isConst)
        val stmt1Code = generateStmt(stmt1, cls, sym, false, frame,isConst)
        frame.exitLoop()
        emitter.emitLABEL(conLabel) + expCode._1 + emitter.emitIFEQ(breakLabel, frame) +
          stmt1Code + emitter.emitGOTO(conLabel, frame) + emitter.emitLABEL(breakLabel)
      }

      case Repeat(stmtList, e) => {
        frame.enterLoop()
        val conLabel = frame.getContinueLabel()
        val breakLabel = frame.getBreakLabel()
        val label3 = frame.getNewLabel()
        val expCode = generateExpression(e, cls, sym, frame,isConst)
        val stmtListCode = stmtList.foldLeft("")((x, y) => x + generateStmt(y, cls, sym, false, frame,isConst))
        frame.exitLoop()
        emitter.emitLABEL(label3) + stmtListCode + emitter.emitLABEL(conLabel) +
          expCode._1 + emitter.emitIFNE(breakLabel, frame) + emitter.emitGOTO(label3, frame) +
          emitter.emitLABEL(breakLabel)

      }

      case For(name, e1, up, e2, stmt1) => {
        frame.enterLoop()
        val conLabel = frame.getContinueLabel()
        val breakLabel = frame.getBreakLabel()
        val label3 = frame.getNewLabel()
        val assignCode = generateStmt(Assign(name, e1), cls, sym, false, frame,isConst)
        val conditionCode = if (up == true) generateExpression(BinaryOp("<=", name, e2), cls, sym, frame,isConst)
        else generateExpression(BinaryOp(">=", name, e2), cls, sym, frame,isConst)
        val stmt1Code = generateStmt(stmt1, cls, sym, false, frame,isConst)
        val reAssign = if (up == true) generateStmt(Assign(name, BinaryOp("+", name, IntLiteral(1))), cls, sym, false, frame,isConst)
        else generateStmt(Assign(name, BinaryOp("-", name, IntLiteral(1))), cls, sym, false, frame,isConst)
        frame.exitLoop()
        assignCode + emitter.emitLABEL(label3) + conditionCode._1 + emitter.emitIFEQ(breakLabel, frame) +
          stmt1Code + emitter.emitLABEL(conLabel) + reAssign + emitter.emitGOTO(label3, frame) + emitter.emitLABEL(breakLabel)

      }

      case Break => {
        val breakLabel = frame.getBreakLabel()
        emitter.emitGOTO(breakLabel, frame)
      }

      case Continue => {
        val conLabel = frame.getContinueLabel()
        emitter.emitGOTO(conLabel, frame)
      }

      case Return(e) => {
        val expCode = generateExpression(e, cls, sym, frame,isConst)
        expCode._1
      }

      case Call(e, n, pl) => {
        
        val lhscls = generateExpression(e, cls, sym, frame,isConst)
        val clstyp = lhscls._2.typ.asInstanceOf[ClassType]
        val mth = lookupMethod(n.name, pl.length, clstyp, env)
        val mthType = mth.typ.asInstanceOf[MethodType]
        var paramsCode = new StringBuffer()
        for(i <- 0 to (pl.length - 1)){
          val param = generateExpression(pl(i), cls, sym, frame,isConst)
          val paramSym = param._2
          if(mthType.params(i) == FloatType && paramSym.typ == IntType)
            paramsCode.append(param._1 + emitter.emitI2F(frame))
          else
            paramsCode.append(param._1)
        }
        val call = emitter.emitINVOKEVIRTUAL(clstyp.classType + "/" + n.name, mth.typ, frame)
        lhscls._1 + paramsCode.toString + call
      }

    }
  }

 
  
  def generateExpression(exp: Expr, cls: ClassDecl, sym: List[Symbol], frame: Frame, isConst:Boolean): (String, Symbol) = {
    exp match {
      case Id(n) => {
        val symbol = lookup(n, sym)
        if(isConst == false){
        	val expCode = emitter.emitREADVAR(symbol, frame)
        	(expCode, symbol)
        }
        else{
          // init for constant
          val expCode = emitter.emitALOAD(0, frame) + emitter.emitGETFIELD(n, symbol.typ, frame, ClassType(cls.name.name))
          (expCode, symbol)
        }
        
      }

      case IntLiteral(v) => {
        val symbol = Symbol("", IntType, Constant, v.toString)
        val expCode = emitter.emitICONST(v, frame)
        (expCode, symbol)
      }

      case FloatLiteral(v) => {
        val symbol = Symbol("", FloatType, Constant, v.toString)
        val expCode = emitter.emitFCONST(v.toString, frame)
        (expCode, symbol)
      }
      case StringLiteral(v) => {
        val symbol = Symbol("", StringType, Constant, v)
        val expCode = emitter.emitLDC(v, frame)
        (expCode, symbol)

      }

      case BooleanLiteral(v) => {
        val symbol = Symbol("", BoolType, Constant, v.toString)
        val expCode = emitter.emitICONST(v.toString, frame)
        (expCode, symbol)
      }

      case NullLiteral => {
        val symbol = Symbol("null", ClassType(""), Constant, "null")
        val expCode = emitter.emitNULLCONST(frame)
        (expCode, symbol)
      }

      case SelfLiteral => {
        val symbol = Symbol("self", ClassType(cls.name.name), Constant, cls.name.name)
        val expCode = emitter.emitALOAD(0, frame)
        (expCode, symbol)
      }

      case UnaryOp("+", e) => generateExpression(e, cls, sym, frame,isConst)

      case UnaryOp("-", e) => generateExpression(e, cls, sym, frame,isConst) match {
        case (code, Symbol(_, ty, _, _)) => (code + emitter.emitNEGOP("-", ty, frame), Symbol("", ty, Constant, Some("")))

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("+", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftTy, _, _)), (rightCode, Symbol(_, rightTy, _, _))) => {
          if (leftTy == IntType && rightTy == IntType) {
            (leftCode + rightCode + emitter.emitADDOP("+", IntType, frame), Symbol("", IntType, Constant, Some("")))
          } else if (leftTy == FloatType && rightTy == FloatType) {
            (leftCode + rightCode + emitter.emitADDOP("+", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } else if (leftTy == IntType && rightTy == FloatType) {
            (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitADDOP("+", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } // case FloatType && IntType
          else {
            (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitADDOP("+", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          }
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("-", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftTy, _, _)), (rightCode, Symbol(_, rightTy, _, _))) => {
          if (leftTy == IntType && rightTy == IntType) {
            (leftCode + rightCode + emitter.emitADDOP("-", IntType, frame), Symbol("", IntType, Constant, Some("")))
          } else if (leftTy == FloatType && rightTy == FloatType) {
            (leftCode + rightCode + emitter.emitADDOP("-", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } else if (leftTy == IntType && rightTy == FloatType) {
            (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitADDOP("-", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } // case FloatType && IntType
          else {
            (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitADDOP("-", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          }
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("*", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftTy, _, _)), (rightCode, Symbol(_, rightTy, _, _))) => {
          if (leftTy == IntType && rightTy == IntType) {
            (leftCode + rightCode + emitter.emitMULOP("*", IntType, frame), Symbol("", IntType, Constant, Some("")))
          } else if (leftTy == FloatType && rightTy == FloatType) {
            (leftCode + rightCode + emitter.emitMULOP("*", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } else if (leftTy == IntType && rightTy == FloatType) {
            (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitMULOP("*", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } // case FloatType && IntType
          else {
            (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitMULOP("*", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          }
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("\\", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftTy, _, _)), (rightCode, Symbol(_, rightTy, _, _))) => {
          if (leftTy == IntType && rightTy == IntType) {
            (leftCode + rightCode + emitter.emitMULOP("\\", IntType, frame), Symbol("", IntType, Constant, Some("")))
          } // error on this case
          else {
            ("", Symbol("", IntType, Constant, "0"))
          }
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("%", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftTy, _, _)), (rightCode, Symbol(_, rightTy, _, _))) => {
          if (leftTy == IntType && rightTy == IntType) {
            (leftCode + rightCode + emitter.emitIREM(frame), Symbol("", IntType, Constant, Some("")))
          } // error on this case
          else {
            ("", Symbol("", IntType, Constant, "0"))
          }
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("/", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftTy, _, _)), (rightCode, Symbol(_, rightTy, _, _))) => {
          if (leftTy == IntType && rightTy == IntType) {
            (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitI2F(frame) + emitter.emitMULOP("/", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } else if (leftTy == FloatType && rightTy == FloatType) {
            (leftCode + rightCode + emitter.emitMULOP("/", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } else if (leftTy == IntType && rightTy == FloatType) {
            (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitMULOP("/", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          } // case FloatType && IntType
          else {
            (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitMULOP("/", FloatType, frame), Symbol("", FloatType, Constant, Some("")))
          }
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("&&", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, BoolType, _, _)), (rightCode, Symbol(_, BoolType, _, _))) => {
          (leftCode + rightCode + emitter.emitANDOP(frame), Symbol("", BoolType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("||", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, BoolType, _, _)), (rightCode, Symbol(_, BoolType, _, _))) => {
          (leftCode + rightCode + emitter.emitOROP(frame), Symbol("", BoolType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case UnaryOp("!", e) => generateExpression(e, cls, sym, frame,isConst) match {
        case (code, Symbol(_, BoolType, _, _)) => {
          val label1 = frame.getNewLabel
          val label2 = frame.getNewLabel
          val expCode = code + emitter.emitIFNE(label1, frame) + emitter.emitICONST(1, frame) + emitter.emitGOTO(label2, frame) + 
          			emitter.emitLABEL(label1) + emitter.emitICONST(0, frame) + emitter.emitLABEL(label2)
          (expCode, Symbol("", BoolType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("==", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftType, _, _)), (rightCode, Symbol(_, rightType, _, _))) => {
          if(leftType == IntType && rightType == IntType){
        	  (leftCode + rightCode + emitter.emitEQOP("==", IntType, frame), Symbol("", BoolType, Constant, Some("")))
          }
          else if(leftType == BoolType && rightType == BoolType){
            (leftCode + rightCode + emitter.emitEQOP("==", IntType, frame), Symbol("", BoolType, Constant, Some("")))
          }
          else if(leftType == FloatType && rightType == FloatType){
            (leftCode + rightCode + emitter.emitEQOP("==", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
          }
          // StringType, ArrayType, ClassType
          else{
            (leftCode + rightCode + emitter.emitEQOP("==", StringType, frame), Symbol("", BoolType, Constant, Some("")))
          }
        }
        
        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("<>", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, leftType, _, _)), (rightCode, Symbol(_, rightType, _, _))) => {
          if(leftType == IntType && rightType == IntType){
        	  (leftCode + rightCode + emitter.emitEQOP("<>", IntType, frame), Symbol("", BoolType, Constant, Some("")))
          }
          else if(leftType == BoolType && rightType == BoolType){
            (leftCode + rightCode + emitter.emitEQOP("<>", IntType, frame), Symbol("", BoolType, Constant, Some("")))
          }
          else if(leftType == FloatType && rightType == FloatType){
            (leftCode + rightCode + emitter.emitEQOP("<>", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
          }
          // StringType, ArrayType, ClassType
          else{
            (leftCode + rightCode + emitter.emitEQOP("<>", StringType, frame), Symbol("", BoolType, Constant, Some("")))
          }
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp(">", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP(">", IntType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP(">", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitRELOP(">", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitRELOP(">", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp(">=", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP(">=", IntType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP(">=", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitRELOP(">=", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitRELOP(">=", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("<", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP("<", IntType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP("<", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitRELOP("<", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitRELOP("<", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("<=", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP("<=", IntType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + rightCode + emitter.emitRELOP("<=", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, IntType, _, _)), (rightCode, Symbol(_, FloatType, _, _))) => {
          (leftCode + emitter.emitI2F(frame) + rightCode + emitter.emitRELOP("<=", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        case ((leftCode, Symbol(_, FloatType, _, _)), (rightCode, Symbol(_, IntType, _, _))) => {
          (leftCode + rightCode + emitter.emitI2F(frame) + emitter.emitRELOP("<=", FloatType, frame), Symbol("", BoolType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }

      case BinaryOp("^", left, right) => (generateExpression(left, cls, sym, frame,isConst), generateExpression(right, cls, sym, frame,isConst)) match {
        case ((leftCode, Symbol(_, StringType, _, _)), (rightCode, Symbol(_, StringType, _, _))) => {
          val expCode = emitter.emitNEW(ClassType("java/lang/StringBuilder"), frame) + emitter.emitDUP(frame) + emitter.emitINVOKESPECIAL("java/lang/StringBuilder/<init>", MethodType("<init>", List(), VoidType), frame) +
            leftCode + emitter.emitINVOKEVIRTUAL("java/lang/StringBuilder/append", MethodType("append", List(StringType), ClassType("java/lang/StringBuilder")), frame) +
            rightCode + emitter.emitINVOKEVIRTUAL("java/lang/StringBuilder/append", MethodType("append", List(StringType), ClassType("java/lang/StringBuilder")), frame) +
            emitter.emitINVOKEVIRTUAL("java/lang/StringBuilder/toString", MethodType("toString", List(), StringType), frame)
          (expCode, Symbol("", StringType, Constant, Some("")))
        }

        // error on this case
        case _ => ("", Symbol("", IntType, Constant, "0"))
      }
      
      case ArrayCell(e1, e2) => (generateExpression(e1, cls, sym, frame,isConst), generateExpression(e2, cls, sym, frame,isConst)) match {
        case ((e1Code, Symbol(_, ArrayType(_, elemType), _, _)), (e2Code, _)) =>{
          (e1Code + e2Code + emitter.emitTALOAD(elemType, frame), Symbol("", elemType, Constant, Some("")))
        }
        
        // error on this case
        case _ => ("", Symbol("", IntType, Constant, ""))
      }
      
      case NewExpr(name, params) => {
        if(params.length !=0){
	        val mth = lookupMethod(name.name, params.length, ClassType(name.name), env)
	        val mthType = mth.typ.asInstanceOf[MethodType]
	        var paramsCode = new StringBuffer()
	        for(i <- 0 to (params.length - 1)){
	          generateExpression(params(i), cls, sym, frame,isConst) match {
	            case (code, Symbol(_, ty, _, _)) => {
	              if(mthType.params(i) == FloatType && ty == IntType)
	            	  paramsCode.append(code + emitter.emitI2F(frame))
	              else
	                paramsCode.append(code)
	            }
	            case _ =>
	          }
	        }
	        val expCode = emitter.emitNEW(ClassType(name.name), frame) + emitter.emitDUP(frame) + paramsCode.toString() +
        			emitter.emitINVOKESPECIAL(name.name + "/<init>", MethodType(name.name, mthType.params, VoidType), frame)
        	(expCode, Symbol("", ClassType(name.name), Constant, Some("")))
        }
        else{
          val expCode = emitter.emitNEW(ClassType(name.name), frame) + emitter.emitDUP(frame) + 
        			emitter.emitINVOKESPECIAL(name.name + "/<init>", MethodType(name.name, List(), VoidType), frame)
        	(expCode, Symbol("", ClassType(name.name), Constant, Some("")))
        }
        
        
      }
      
      case FieldAccess(e, field) => generateExpression(e, cls, sym, frame,isConst) match {
        case (eCode, Symbol(_, ClassType(cName), _, _)) => {
          val fieldSymbol = lookupField(field.name, ClassType(cName), env)
          val expCode = eCode + emitter.emitGETFIELD(field.name, fieldSymbol.typ, frame, ClassType(cName))
          (expCode, Symbol("", fieldSymbol.typ, Constant, Some("")))
        }
        
        // error on this case
        case _ => ("", Symbol("", IntType, Constant, ""))
      }
       
      case CallExpr(e1, name, params) => {
        
        val lhscls = generateExpression(e1, cls, sym, frame,isConst)
        val clstyp = lhscls._2.typ.asInstanceOf[ClassType]
        val mth = lookupMethod(name.name, params.length, clstyp, env)
        val mthType = mth.typ.asInstanceOf[MethodType]
        var paramsCode = new StringBuffer()
        for(i <- 0 to (params.length - 1)){
          val param = generateExpression(params(i), cls, sym, frame,isConst)
          val paramSym = param._2
          if(mthType.params(i) == FloatType && paramSym.typ == IntType)
            paramsCode.append(param._1 + emitter.emitI2F(frame))
          else
            paramsCode.append(param._1)
        }
        val call = emitter.emitINVOKEVIRTUAL(clstyp.classType + "/" + name.name, mth.typ, frame)
        (lhscls._1 + paramsCode.toString + call, Symbol("", mthType.returnType, Constant, Some("")))
      }

      case _ => ("", Symbol("", IntType, Constant, ""))
    }
  }

  def lookup(n: String, sym: List[Symbol]): Symbol = sym match {
    case List() => throw Undeclared(Identifier, n)
    case head :: tail => if (n == head.name) head else lookup(n, tail)
  }

  
  def lookupField(n: String, cls: ClassType, env: List[MyClassType]): Symbol = {
    for(i <- 0 to (env.length - 1)){
      if(env(i).name == cls.classType){
        val res = getField(n, env(i).lstAtt)
        res match {
          case Some(s) => return s
          case None => if(env(i).parent == null || env(i).parent == "")	throw Undeclared(Attribute, n)
          			else return lookupField(n, ClassType(env(i).parent), env)
        }
      }
    }
    throw Undeclared(Class, cls.classType)
  }
  

  def lookupMethod(n: String, dim: Integer, cls: ClassType, env: List[MyClassType]):Symbol = {
    for(i <- 0 to (env.length - 1)){
      if(env(i).name == cls.classType){
        val res = getMethod(n, dim, env(i).lstMethod)
        res match {
          case Some(s) => return s
          case None => if(env(i).parent == null || env(i).parent == "") throw Undeclared(Method, n)
          			else return lookupMethod(n, dim, ClassType(env(i).parent), env)
          
        }
      }
    }
    throw Undeclared(Class, cls.classType)
  }

  def getField(n: String, lst: List[Symbol]): Option[Symbol] =
    lst match {
      case List() => None
      case head :: tail => if (n == head.name) Some(head) else getField(n, tail)
    }

  def getMethod(n: String, dim: Integer, lst: List[Symbol]): Option[Symbol] =
    lst match {
      case List() => None
      case head :: tail => if (n == head.name && dim == head.typ.asInstanceOf[MethodType].params.length) Some(head) else getMethod(n, dim, tail)
    }
}

/**
 * 	Emitter.scala
 */

import java.text.DecimalFormat

class Emitter(machine: MachineCode) {

  val END = "\n"

  def emitICONST(i: Integer, frame: Frame) = {
    frame.push()
    if (i == -1)
      machine.emitICONST(i)
    else if (i >= 0 && i <= 5)
      machine.emitICONST(i)
    else if (i >= -128 && i <= 127)
      machine.emitBIPUSH(i)
    else if (i >= -32768 && i <= 32767)
      machine.emitSIPUSH(i)
    else
      machine.emitLDC(i.toString)
  }

  def emitICONST(in: String, frame: Frame): String = {

    if (in == "true")
      emitICONST(1, frame)
    else if (in == "false")
      emitICONST(0, frame)
    else {
      try {
        emitICONST(Integer.parseInt(in), frame)
      } catch {
        case e: NumberFormatException => throw IllegalOperandException
      }
    }
  }

  def emitFCONST(in: String, frame: Frame) = {
    try {
      val f = in.toFloat
      val myFormatter = new DecimalFormat("###0.0###")
      val rst = myFormatter.format(f)
      frame.push()
      machine.emitFCONST(rst)
    } catch {

      case e: NumberFormatException => { throw IllegalOperandException }
    }
  }

  def emitLDC(in: String, frame: Frame) = {
    frame.push()
    //machine.emitLDC(in)
    if (in == null || in == "")
      machine.emitLDC('\"' + in + '\"')
    else {
      if (in(0) == '\"' && in(in.length() - 1) == '\"') {
        machine.emitLDC(in)
      } else {
        machine.emitLDC('\"' + in + '\"')
      }
    }
  }

  def emitREADVAR(sym: Symbol, frame: Frame, cls: ClassType = null) = {
    if (sym.kind == Variable || sym.kind == Parameter) {
      if (sym.typ == IntType || sym.typ == BoolType)
        emitILOAD(sym.obj.get.toInt, frame)
      else if (sym.typ == FloatType)
        emitFLOAD(sym.obj.get.toInt, frame)
      else
        emitALOAD(sym.obj.get.toInt, frame)
    } else if (sym.kind == Attribute && sym.typ == ClassType("IO") && sym.name == "io") {
      emitGETSTATIC(sym.name, sym.typ, frame, ClassType(sym.obj.get))
    } else
      emitGETFIELD(sym.name, sym.typ, frame, ClassType(sym.obj.get))
  }

  def emitREADVAR2(sym: Symbol, frame: Frame) =
    sym.typ match {
      case ArrayType(dim, eT) => emitTALOAD(eT, frame)
      case _ => {
        if (sym.kind == Variable || sym.kind == Parameter) {
          if (sym.typ == IntType || sym.typ == BoolType)
            emitILOAD(sym.obj.get.toInt, frame)
          else
            emitFLOAD(sym.obj.get.toInt, frame)
        } else if (sym.kind == Attribute && sym.typ == ClassType("IO") && sym.name == "io") {
          emitGETSTATIC(sym.name, sym.typ, frame, ClassType(sym.obj.get))
        } else
          emitGETFIELD(sym.name, sym.typ, frame, ClassType(sym.obj.get))
      }
    }

  def emitWRITEVAR(sym: Symbol, frame: Frame, cls: ClassType = null) =
    sym.typ match {
      case ArrayType(dim, eT) => emitTASTORE(eT, frame)
      case _ => {
        if (sym.kind == Variable || sym.kind == Parameter) {
          if (sym.typ == IntType || sym.typ == BoolType)
            emitISTORE(sym.obj.get.toInt, frame)
          else if (sym.typ == FloatType)
            emitFSTORE(sym.obj.get.toInt, frame)
          else
            // sym.typ == String
            emitASTORE(sym.obj.get.toInt, frame)

        } else if (sym.kind == Attribute && sym.typ == ClassType("IO") && sym.name == "io") {
          emitPUTSTATIC(sym.name, sym.typ, frame, ClassType(sym.obj.get))
        } else
          emitPUTFIELD(sym.name, sym.typ, frame, ClassType(sym.obj.get))
      }
    }

  def emitILOAD(in: Integer, frame: Frame) = {
    frame.push()
    machine.emitILOAD(in)
  }

  def emitFLOAD(in: Integer, frame: Frame) = {
    frame.push()
    machine.emitFLOAD(in)
  }

  def emitISTORE(in: Integer, frame: Frame) = {
    frame.pop()
    machine.emitISTORE(in)
  }

  def emitFSTORE(in: Integer, frame: Frame) = {
    frame.pop()
    machine.emitFSTORE(in)
  }

  def emitALOAD(in: Integer, frame: Frame) = {
    frame.push()
    machine.emitALOAD(in)
  }

  def emitASTORE(in: Integer, frame: Frame) = {
    frame.pop()
    machine.emitASTORE(in)

  }

  def emitTALOAD(in: Type, frame: Frame) = {
    frame.pop()
    if (in == IntType)
      machine.emitIALOAD
    else if (in == FloatType)
      machine.emitFALOAD
    else if (in == BoolType)
      machine.emitBALOAD
    else
      // in == StringType || in == CLassType
      machine.emitAALOAD
  }

  def emitTASTORE(in: Type, frame: Frame) = {
    frame.pop()
    frame.pop()
    frame.pop()
    if (in == IntType)
      machine.emitIASTORE
    else if (in == FloatType)
      machine.emitFASTORE
    else if(in == BoolType)
      machine.emitBASTORE
    else
      // in == StringType || in == ClassType
      machine.emitAASTORE
  }

  def emitADDOP(lexeme: String, in: Type, frame: Frame) = {
    frame.pop()
    if (lexeme == "+") {
      if (in == IntType)
        machine.emitIADD
      else
        machine.emitFADD
    } else if (in == IntType)
      machine.emitISUB
    else
      machine.emitFSUB
  }

  def emitMULOP(lexeme: String, in: Type, frame: Frame) = {
    frame.pop()
    if (lexeme == "*") {
      if (in == IntType)
        machine.emitIMUL
      else
        machine.emitFMUL
    } else if (in == IntType)
      machine.emitIDIV
    else
      machine.emitFDIV

  }

  def emitANDOP(frame: Frame) = {
    frame.pop()
    machine.emitIAND
  }

  def emitOROP(frame: Frame) = {
    frame.pop()
    machine.emitIOR
  }

  def emitEQOP(lexeme: String, in: Type, label: Integer, frame: Frame) = {
    frame.pop()
    frame.pop()
    if (in == IntType || in == BoolType) {
      if (lexeme == "==")
        machine.emitIFICMPNE(label)
      else
        // lexeme == "<>"
        machine.emitIFICMPEQ(label)
    } 
    else if(in == FloatType) {
      if (lexeme == "==")
        machine.emitFCMPL() + machine.emitIFNE(label)
      else
        // lexeme == "<>"
        machine.emitFCMPL() + machine.emitIFEQ(label)
    }
    //in == StringType || ArrayType || ClassType
    else{
    	if (lexeme == "==")
    		machine.emitIFACMPNE(label)
        else
        // lexeme == "<>"
        	machine.emitIFACMPEQ(label)
    }
  }

  def emitEQOP(lexeme: String, in: Type, frame: Frame): String = {
    val label = frame.getNewLabel
    val label1 = frame.getNewLabel
    var buff = new StringBuffer()
    buff.append(emitEQOP(lexeme, in, label, frame))
    buff.append(emitICONST(1, frame))
    frame.pop() //da sua 01/12/2014
    buff.append(emitGOTO(label1, frame))
    buff.append(emitLABEL(label))
    buff.append(emitICONST(0, frame))
    buff.append(emitLABEL(label1))
    buff.toString()
  }

  def emitRELOP(lexeme: String, in: Type, label: Integer, frame: Frame) = {
    frame.pop()
    frame.pop()
    if (in == IntType) {
      if (lexeme == ">=")
        machine.emitIFICMPLT(label)
      else if (lexeme == ">")
        machine.emitIFICMPLE(label)
      else if (lexeme == "<=")
        machine.emitIFICMPGT(label)
      else
        machine.emitIFICMPGE(label)
    } else {
      if (lexeme == ">=")
        machine.emitFCMPL() + machine.emitIFLT(label)
      else if (lexeme == ">")
        machine.emitFCMPL() + machine.emitIFLE(label)
      else if (lexeme == "<=")
        machine.emitFCMPG() + machine.emitIFGT(label)
      else
        machine.emitFCMPG() + machine.emitIFGE(label)
    }
  }

  def emitRELOP(lexeme: String, in: Type, frame: Frame): String = {

    val label = frame.getNewLabel
    val label1 = frame.getNewLabel
    var buff = new StringBuffer()
    buff.append(emitRELOP(lexeme, in, label, frame))
    buff.append(emitICONST(1, frame))
    frame.pop() // da sua 1/12/2014
    buff.append(emitGOTO(label1, frame))
    buff.append(emitLABEL(label))
    buff.append(emitICONST(0, frame))
    buff.append(emitLABEL(label1))
    buff.toString()
  }

  def emitIFTRUE(label: Integer, frame: Frame) = {
    frame.pop()
    machine.emitIFGT(label)
  }

  def emitIFFALSE(label: Integer, frame: Frame) = {
    frame.pop()
    machine.emitIFLE(label)
  }

  def emitNEGOP(lexeme: String, in: Type, frame: Frame) = {
    if (lexeme == "-") {
      if (in == IntType || in == BoolType)
        machine.emitINEG
      else
        machine.emitFNEG
    } else
      ""
  }

  def emitGETSTATIC(lexeme: String, in: Type, frame: Frame, cls: ClassType) = {
    frame.push()
    machine.emitGETSTATIC(cls.classType + "." + lexeme, in.genCode)
  }

  def emitPUTSTATIC(lexeme: String, in: Type, frame: Frame, cls: ClassType) = {
    frame.pop()
    machine.emitPUTSTATIC(cls.classType + "." + lexeme, in.genCode)
  }

  def emitGETFIELD(lexeme: String, in: Type, frame: Frame, cls: ClassType) = {
    machine.emitGETFIELD(cls.classType + "." + lexeme, in.genCode)
  }

  def emitPUTFIELD(lexeme: String, in: Type, frame: Frame, cls: ClassType) = {
    frame.pop()
    frame.pop()
    machine.emitPUTFIELD(cls.classType + "." + lexeme, in.genCode)
  }
  def emitGOTO(label: Integer, frame: Frame) = {
    machine.emitGOTO(label)
  }

  def emitDUP(frame: Frame) = {
    frame.push()
    machine.emitDUP
  }

  def emitDUPX2(frame: Frame) = {
    frame.push()
    machine.emitDUPX2
  }
  def emitPOP(frame: Frame) = {
    frame.pop()
    machine.emitPOP
  }

  def emitI2F(frame: Frame) =
    machine.emitI2F

  def emitNEWARRAY(in: Type, frame: Frame) = {
    in match {
      case IntType => machine.emitNEWARRAY("int")
      case BoolType => machine.emitNEWARRAY("boolean")
      case FloatType => machine.emitNEWARRAY("float")
      case StringType => machine.emitANEWARRAY("java/lang/String")
      case ClassType(name) => machine.emitANEWARRAY(name)
      // error on this case
      case _ => ""
    }
  }

  def emitINVOKESTATIC(lexeme: String, in: Type, frame: Frame) = {
    val ft = in.asInstanceOf[MethodType]

    (1 to ft.params.length).foreach(_ => frame.pop)

    if (ft.returnType != null && ft.returnType != VoidType)
      frame.push()
    machine.emitINVOKESTATIC(lexeme, in.genCode)
  }

  def emitINVOKEVIRTUAL(lexeme: String, in: Type, frame: Frame) = {
    val ft = in.asInstanceOf[MethodType]

    (0 to ft.params.length).foreach(_ => frame.pop)

    if (ft.returnType != null && ft.returnType != VoidType)
      frame.push()
    machine.emitINVOKEVIRTUAL(lexeme, in.genCode)
  }

  def emitINVOKESPECIAL(frame: Frame) = {
    frame.pop()
    machine.emitINVOKESPECIAL
  }

  def emitINVOKESPECIAL(lexeme: String, in: Type, frame: Frame) = {
    val ft = in.asInstanceOf[MethodType]

    (0 to ft.params.length).foreach(_ => frame.pop())

    if (ft.returnType != null && ft.returnType != VoidType)
      frame.push()
    machine.emitINVOKESPECIAL(lexeme, in.genCode)
  }

  def emitRETURN(in: Type, frame: Frame) = {
    if (in == null || in == VoidType) {
      // in == VoidType
      machine.emitRETURN
    } else {
      frame.pop()
      if (in == IntType || in == BoolType)
        machine.emitIRETURN
      else if (in == FloatType)
        machine.emitFRETURN
      else
        // in == StringType, ClassType, ArrayType
        machine.emitARETURN
    }
  }

  def emitLIMITSTACK(in: Integer) =
    machine.emitLIMITSTACK(in)

  def emitLIMITLOCAL(in: Integer) =
    machine.emitLIMITLOCAL(in)
    

  def emitVAR(in: Integer, varName: String, inType: Type, fromLabel: Integer, toLabel: Integer) = {
     machine.emitVAR(in, varName, inType.genCode, fromLabel, toLabel)
  }

  def emitVAR(in: Integer, varName: String, inType: String, fromLabel: Integer, toLabel: Integer) =
    machine.emitVAR(in, varName, inType, fromLabel, toLabel)

  def emitVAR(in: Integer, varname: String, inType: Type) =
    machine.emitVAR(in, varname, inType.genCode)

  def emitMETHOD(lexeme: String, in: String, isStatic: Boolean, frame: Frame) =
    machine.emitMETHOD(lexeme, in, isStatic)

  def emitMETHOD(lexeme: String, in: Type, isStatic: Boolean, frame: Frame): String = {
    if (frame.isMain){
      val mthType = in.asInstanceOf[MethodType]
    		  emitMETHOD(lexeme, "([Ljava/lang/String;)" + mthType.returnType.genCode, true, frame)
    }
    else
      emitMETHOD(lexeme, in.genCode, isStatic, frame)
  }

  def emitENDMETHOD() =
    machine.emitENDMETHOD

  def emitLABEL(in: Integer) =
    machine.emitLABEL(in)

  def emitPROLOG(decl: ClassDecl) = {
    val cls = machine.emitCLASS(decl.name.name)
    val parent = machine.emitSUPER(if (decl.parent != null) decl.parent.name else "java/lang/Object")
    //val sour = ".source " + source + END
    cls + parent //+ sour
  }

  def emitSTATICFIELD(lexeme: String, in: Type, kind: Kind, value: String = "") = {
    machine.emitSTATICFIELD(lexeme, in.genCode, kind == Constant, value)
  }

  def emitINSTANCEFIELD(lexeme: String, in: Type, kind: Kind, value: String = "") = {
    machine.emitINSTANCEFIELD(lexeme, in.genCode, kind == Constant, value)
  }

  def emitCLINIT(decl: ClassDecl, in: List[Symbol]) = {
    var result = new StringBuffer()
    val prs: List[Type] = List()
    val tmp = MethodType(decl.name.name, prs, VoidType)
    val frame = new Frame(false)
    frame.enterScope(true)
    result.append(emitMETHOD("<clinit>", tmp, true, frame))
    result.append(emitLIMITSTACK(1))
    result.append(emitLIMITLOCAL(0))
    //if (CodeGenerator.DEBUG) System.out.println(in.size())
    in.foreach(sym => {
      val at = sym.typ.asInstanceOf[ArrayType]
      result.append(emitICONST(at.dimen.value, frame))
      result.append(emitNEWARRAY(at.eleType, frame))
      result.append(emitPUTSTATIC(sym.name, sym.typ, frame, ClassType(decl.name.name)))
    })
    result.append(emitRETURN(null, frame))
    result.append(emitENDMETHOD())
    frame.exitScope()
    //in.clear()
    result.toString()
  }
/*
  def emitINIT(decl: ClassDecl, in: List[Symbol]) = {
    var result = new StringBuffer()

    val tmp = MethodType(decl.name.name, List(), VoidType)
    val frame = new Frame(false)
    frame.enterScope(true)
    result.append(emitMETHOD("<init>", tmp, false, frame))
    result.append(emitLIMITSTACK(3))
    result.append(emitLIMITLOCAL(1))
    result.append(emitVAR(0, "this", ClassType(decl.name.name), 0, 1))
    result.append(emitLABEL(0))
    result.append(emitALOAD(0, frame))
    val invokeSpecial = if(decl.parent == null) emitINVOKESPECIAL(frame)
    					else emitINVOKESPECIAL(decl.parent.name + "/<init>", MethodType("", List(), VoidType), frame)
    result.append(invokeSpecial)
    for (sym <- in) {
      val at = sym.typ.asInstanceOf[ArrayType]
      result.append(emitALOAD(0, frame))
      result.append(emitICONST(at.dimen.value, frame))
      result.append(emitNEWARRAY(at.eleType, frame))
      result.append(emitPUTFIELD(sym.name, sym.typ, frame, ClassType(decl.name.name)))
    }
    result.append(emitLABEL(1))
    result.append(emitRETURN(null, frame))
    result.append(emitENDMETHOD())
    frame.exitScope()
    result.toString()
  }
  */
  def emitINIT(mthType: MethodType, decl: ClassDecl, constInit:List[String], frame1: Frame) = {
    var result = new StringBuffer()
	val in = decl.decl
	
    val tmp = mthType
    //val frame2 = new Frame(false)
    frame1.enterScope(true)
    result.append(emitMETHOD("<init>", tmp, false, frame1))
    result.append(emitLIMITSTACK(3))
    result.append(emitLIMITLOCAL(1))
    result.append(emitVAR(0, "this", ClassType(decl.name.name), 0, 1))
    result.append(emitLABEL(0))
    result.append(emitALOAD(0, frame1))
    val invokeSpecial = if(decl.parent == null) emitINVOKESPECIAL(frame1)
    					else emitINVOKESPECIAL(decl.parent.name + "/<init>", MethodType("", List(), VoidType), frame1)
    result.append(invokeSpecial)
    var count = 0;
    for (i <- 0 to (in.length - 1)) {
		in(i) match {
			case VarDecl(name, ArrayType(dimen, elemType)) => {
				  result.append(emitALOAD(0, frame1))
				  result.append(emitICONST(dimen.value, frame1))
				  result.append(emitNEWARRAY(elemType, frame1))
				  result.append(emitPUTFIELD(name.name, ArrayType(dimen, elemType), frame1, ClassType(decl.name.name)))
			}
			case ConstDecl(name, ty, e) => {
				result.append(emitALOAD(0, frame1))
				/*
				val eCode = e match {
								case IntLiteral(v) => emitICONST(v, frame)
								case FloatLiteral(v) => emitFCONST(v.toString, frame)
								case StringLiteral(v) => emitLDC(v, frame)
								case BooleanLiteral(v) => emitICONST(v.toString, frame)
								case _ => "\n"
							}
						
				*/
				val eCode = constInit(count)
				count = count + 1
				result.append(eCode)
				result.append(emitPUTFIELD(name.name, ty, frame1, ClassType(decl.name.name)))
				
			}
			case _ =>
		}
    }
    result.append(emitLABEL(1))
    result.append(emitRETURN(null, frame1))
    result.append(emitENDMETHOD())
    //frame2.exitScope()
    result.toString()
  }


  def emitINITARRAY(index: Integer, in: Type, frame: Frame) = {
    var buff = new StringBuffer()
    val at = in.asInstanceOf[ArrayType]
    buff.append(emitICONST(at.dimen.value, frame))
    buff.append(emitNEWARRAY(at.eleType, frame))
    buff.append(emitASTORE(index, frame))
    buff.toString()
  }

  def emitLISTARRAY(in: List[Symbol], frame: Frame) = {
    var result = new StringBuffer()
    for (it <- in) {
      //val at = it.typ.asInstanceOf[ArrayType]
      result.append(emitINITARRAY(it.obj.get.toInt, it.typ, frame))
    }
    result.toString()
  }

  def emitNULLCONST(frame: Frame) = {
    frame.push()
    machine.emitNULLCONST
  }

  def emitIREM(frame: Frame) = {
    frame.pop()
    machine.emitIREM()
  }

  def emitNEW(in: Type, frame: Frame) = {
    frame.push()
    val classType = in.asInstanceOf[ClassType]
    machine.emitNEW(classType.classType)
  }

  def emitIFEQ(label: Int, frame: Frame) = {
    frame.pop()
    machine.emitIFEQ(label)
  }

  def emitIFNE(label: Int, frame: Frame) = {
    frame.pop()
    machine.emitIFNE(label)
  }

}

/**
 * 	Frame.scala
 */
import scala.collection.mutable.Stack

class Frame(val isMain: Boolean) {

  var currentLabel = 0
  var currOpStackSize = 0
  var maxOpStackSize = 0
  var currIndex = 0
  var maxIndex = 0
  val startLabel = new Stack[Int]()
  val endLabel = new Stack[Int]()
  val indexLocal = new Stack[Int]()
  val conLabel = new Stack[Int]()
  val brkLabel = new Stack[Int]()

  def getCurrIndex(): Int = {
    currIndex
  }

  def setCurrIndex(index: Int) = {
    currIndex = index
  }

  /**
   *   return a new label in the method.
   *   @return an integer representing the label.
   */
  def getNewLabel(): Int = {
    val tmp = currentLabel
    currentLabel += 1;
    tmp
  }

  /**
   * 	simulate an instruction that pushes a value onto operand stack.
   */
  def push() {
    currOpStackSize += 1;
    if (maxOpStackSize < currOpStackSize)
      maxOpStackSize = currOpStackSize
  }

  /**
   * 	simulate an instruction that pops a value out of operand stack.
   */

  def pop() {
    currOpStackSize -= 1;
    if (currOpStackSize < 0)
      throw IllegalRuntimeException("Pop an empty stack(")
  }

  /**
   * 	return the maximum size of the operand stack that the method needs to use.
   * 	@return an integer that represent the maximum stack size
   */
  def getMaxOpStackSize(): Int = {
    maxOpStackSize
  }

  /**
   * 	check if the operand stack is empty or not.
   * 	@throws IllegalRuntimeException if the operand stack is not empty.
   */

  def checkOpStack() = {
    if (currOpStackSize != 0)
      throw IllegalRuntimeException("Operand Stack is not empty")
  }

  /**
   * 	invoked when parsing into a new scope inside a method.<p>
   * 	This method will create 2 new labels that represent the starting and ending points of the scope.<p>
   * 	Then, these labels are pushed onto corresponding stacks.<p>
   * 	These labels can be retrieved by getStartLabel() and getEndLabel().<p>
   * 	In addition, this method also saves the current index of local variable.
   */

  def enterScope(isProc: Boolean) {
    val start = getNewLabel()
    val end = getNewLabel()

    startLabel.push(start)
    endLabel.push(end)
    indexLocal.push(currIndex)

    if (isProc) {
      maxOpStackSize = 0
      maxIndex = 0
    }
  }

  /**
   * 	invoked when parsing out of a scope in a method.<p>
   * 	This method will pop the starting and ending labels of this scope
   * 	and restore the current index
   */

  def exitScope() = {
    if (startLabel.isEmpty || endLabel.isEmpty || indexLocal.isEmpty)
      throw IllegalRuntimeException("Exit scope but startLabel or endLabel or indexLocal is empty")

    startLabel.pop()
    endLabel.pop()
    currIndex = indexLocal.pop
  }

  /**
   * 	return the starting label of the current scope.
   * 	@return an integer representing the starting label
   */

  def getStartLabel(): Int = {
    if (startLabel.isEmpty)
      throw IllegalRuntimeException("Start label is empty ")

    startLabel.top
  }

  /**
   * 	return the ending label of the current scope.
   * 	@return an integer representing the ending label
   */

  def getEndLabel(): Int = {
    if (endLabel.isEmpty)
      throw IllegalRuntimeException("End Label is empty ")

    endLabel.top
  }

  /**
   * 	return a new index for a local variable declared in a scope.
   * 	@return an integer that represents the index of the local variable
   */
  def getNewIndex(): Int = {
    val tmp = currIndex

    currIndex += 1;
    if (currIndex > maxIndex)
      maxIndex = currIndex

    tmp
  }

  /**
   * 	return the maximum index used in generating code for the current method
   * 	@return an integer representing the maximum index
   */
  def getMaxIndex(): Int = {
    maxIndex
  }

  /**
   * 	invoked when parsing into a loop statement.<p>
   * 	This method creates 2 new labels that represent the starting and ending label of the loop.<p>
   * 	These labels are pushed onto corresponding stacks and are retrieved by getBeginLoopLabel() and getEndLoopLabel().
   */
  def enterLoop() {
    val con = getNewLabel()
    val brk = getNewLabel()
    conLabel.push(con)
    brkLabel.push(brk)
  }

  /**
   * 	invoked when parsing out of a loop statement.
   * 	This method will take 2 labels representing the starting and ending labels of the current loop out of its stacks.
   */

  def exitLoop() = {
    if (conLabel.isEmpty || brkLabel.isEmpty)
      throw IllegalRuntimeException("Continue or Break label is empty ")

    conLabel.pop
    brkLabel.pop
  }

  /**
   * 	return the label of the innest enclosing loop to which continue statement would jump
   * 	@return an integer representing the continue label
   */

  def getContinueLabel(): Int = {
    if (conLabel.isEmpty)
      throw IllegalRuntimeException("Continue label is empty ")

    conLabel.top
  }

  /**
   * 	return the label of the innest enclosing loop to which break statement would jump
   * 	@return an integer representing the break label
   */

  def getBreakLabel(): Int = {
    if (brkLabel.isEmpty)
      throw IllegalRuntimeException("Break label is empty ")

    brkLabel.top
  }
}

/**
 * 	MachineCode.scala
 */

trait MachineCode {
  def emitNULLCONST: String
  def emitICONST(i: Int): String
  def emitBIPUSH(i: Int): String
  def emitSIPUSH(i: Int): String
  def emitLDC(in: String): String
  def emitFCONST(in: String): String
  def emitILOAD(in: Int): String
  def emitFLOAD(in: Int): String
  def emitISTORE(in: Int): String
  def emitFSTORE(in: Int): String
  def emitALOAD(in: Int): String
  def emitASTORE(in: Int): String
  def emitIASTORE(): String
  def emitFASTORE(): String
  def emitBASTORE(): String
  def emitAASTORE(): String
  def emitIALOAD(): String
  def emitFALOAD(): String
  def emitBALOAD(): String
  def emitAALOAD(): String
  def emitGETSTATIC(lexeme: String, typ: String): String
  def emitPUTSTATIC(lexeme: String, typ: String): String
  def emitGETFIELD(lexeme: String, typ: String): String
  def emitPUTFIELD(lexeme: String, typ: String): String
  def emitIADD(): String
  def emitFADD(): String
  def emitISUB(): String
  def emitFSUB(): String
  def emitIMUL(): String
  def emitFMUL(): String
  def emitIDIV(): String
  def emitFDIV(): String
  def emitIAND(): String
  def emitIOR(): String
  def emitIREM(): String
  def emitIFICMPEQ(label: Int): String
  def emitIFICMPNE(label: Int): String
  def emitIFICMPLT(label: Int): String
  def emitIFICMPLE(label: Int): String
  def emitIFICMPGT(label: Int): String
  def emitIFICMPGE(label: Int): String
  def emitIFEQ(label: Int): String
  def emitIFNE(label: Int): String
  def emitIFLT(label: Int): String
  def emitIFLE(label: Int): String
  def emitIFGT(label: Int): String
  def emitIFGE(label: Int): String
  def emitLABEL(label: Int): String
  def emitGOTO(label: Int): String
  def emitINEG(): String
  def emitFNEG(): String
  def emitDUP(): String
  def emitDUPX2(): String
  def emitPOP(): String
  def emitI2F(): String
  def emitNEW(lexeme: String): String
  def emitNEWARRAY(lexeme: String): String
  def emitMULTIANEWARRAY(typ: String, dimensions: Int): String
  def emitINVOKESTATIC(lexeme: String, typ: String): String
  def emitINVOKESPECIAL(lexeme: String, typ: String): String
  def emitINVOKESPECIAL(): String
  def emitINVOKEVIRTUAL(lexeme: String, typ: String): String
  def emitIRETURN(): String
  def emitARETURN(): String
  def emitFRETURN(): String
  def emitRETURN(): String
  def emitLIMITSTACK(in: Int): String
  def emitFCMPL(): String
  def emitLIMITLOCAL(in: Int): String
  def emitVAR(in: Int, varName: String, intyp: String, fromLabel: Int, toLabel: Int): String
  def emitVAR(in: Int, varName: String, intyp: String): String
  def emitMETHOD(lexeme: String, typ: String, isStatic: Boolean): String
  def emitENDMETHOD(): String
  def emitSOURCE(lexeme: String): String
  def emitCLASS(lexeme: String): String
  def emitSUPER(lexeme: String): String
  def emitSTATICFIELD(lexeme: String, typ: String, isFinal: Boolean, value: String): String
  def emitINSTANCEFIELD(lexeme: String, typ: String, isFinal: Boolean, value: String): String
  def emitFCMPG(): String
  def emitANEWARRAY(lexeme: String): String
  def emitIFACMPEQ(label: Int): String
  def emitIFACMPNE(label: Int): String
}

object JasminCode extends MachineCode {
  val END = "\n"
  val INDENT = "\t"

  def emitNULLCONST(): String = {
    INDENT + "aconst_null" + END
  }

  def emitICONST(i: Int): String = {
    if (i == -1) {
      INDENT + "iconst_m1" + END
    } else if (i >= 0 && i <= 5) {
      INDENT + "iconst_" + i + END
    } else throw IllegalOperandException
  }

  def emitBIPUSH(i: Int): String = {
    if ((i >= -128 && i < -1) || (i > 5 && i <= 127))
      INDENT + "bipush " + i + END
    else
      throw IllegalOperandException
  }

  def emitSIPUSH(i: Int): String = {
    if ((i >= -32768 && i < -128) || (i > 127 && i <= 32767))
      INDENT + "sipush " + i + END
    else
      throw IllegalOperandException
  }

  def emitLDC(in: String): String = {
    INDENT + "ldc " + in + END
  }

  def emitFCONST(in: String): String = {
    
		if (in.equals("0.0")) {
			INDENT + "fconst_0" + END
		} else if (in.equals("1.0")) {
			INDENT + "fconst_1" + END
		} else if (in.equals("2.0")) {
			INDENT + "fconst_2" + END
		} else
			emitLDC(in)
	
 
    
  }

  def emitILOAD(in: Int): String = {
    if (in >= 0 && in <= 3)
      INDENT + "iload_" + in + END
    else
      INDENT + "iload " + in + END
  }

  def emitFLOAD(in: Int): String = {
    if (in >= 0 && in <= 3)
      INDENT + "fload_" + in + END
    else
      INDENT + "fload " + in + END
  }

  def emitISTORE(in: Int): String = {
    if (in >= 0 && in <= 3)
      INDENT + "istore_" + in + END
    else
      INDENT + "istore " + in + END
  }

  def emitFSTORE(in: Int): String = {
    if (in >= 0 && in <= 3)
      INDENT + "fstore_" + in + END
    else
      INDENT + "fstore " + in + END
  }

  def emitALOAD(in: Int): String = {
    if (in >= 0 && in <= 3)
      INDENT + "aload_" + in + END
    else
      INDENT + "aload " + in + END
  }

  def emitASTORE(in: Int): String = {
    if (in >= 0 && in <= 3)
      INDENT + "astore_" + in + END
    else
      INDENT + "astore " + in + END
  }

  def emitIASTORE(): String = {
    INDENT + "iastore" + END
  }

  def emitFASTORE(): String = {
    INDENT + "fastore" + END
  }

  def emitBASTORE(): String = {
    INDENT + "bastore" + END
  }

  def emitAASTORE(): String = {
    INDENT + "aastore" + END
  }

  def emitIALOAD(): String = {
    INDENT + "iaload" + END
  }

  def emitFALOAD(): String = {
    INDENT + "faload" + END
  }

  def emitBALOAD(): String = {
    INDENT + "baload" + END
  }

  def emitAALOAD(): String = {
    INDENT + "aaload" + END
  }

  def emitGETSTATIC(lexeme: String, typ: String): String = {
    INDENT + "getstatic " + lexeme + " " + typ + END
  }

  def emitPUTSTATIC(lexeme: String, typ: String): String = {
    INDENT + "putfield " + lexeme + " " + typ + END
  }

  def emitGETFIELD(lexeme: String, typ: String): String = {
    INDENT + "getfield " + lexeme + " " + typ + END
  }

  def emitPUTFIELD(lexeme: String, typ: String): String = {
    INDENT + "putfield " + lexeme + " " + typ + END
  }

  def emitIADD(): String = {
    INDENT + "iadd" + END
  }

  def emitFADD(): String = {
    INDENT + "fadd" + END
  }

  def emitISUB(): String = {
    INDENT + "isub" + END
  }

  def emitFSUB(): String = {
    INDENT + "fsub" + END
  }

  def emitIMUL(): String = {
    INDENT + "imul" + END
  }

  def emitFMUL(): String = {
    INDENT + "fmul" + END
  }

  def emitIDIV(): String = {
    INDENT + "idiv" + END
  }

  def emitFDIV(): String = {
    INDENT + "fdiv" + END
  }

  def emitIAND(): String = {
    INDENT + "iand" + END
  }

  def emitIOR(): String = {
    INDENT + "ior" + END
  }

  def emitIREM(): String = {
    INDENT + "irem" + END
  }

  def emitIFICMPEQ(label: Int): String = {
    INDENT + "if_icmpeq Label" + label + END
  }

  def emitIFICMPNE(label: Int): String = {
    INDENT + "if_icmpne Label" + label + END
  }

  def emitIFICMPLT(label: Int): String = {
    INDENT + "if_icmplt Label" + label + END
  }

  def emitIFICMPLE(label: Int): String = {
    INDENT + "if_icmple Label" + label + END
  }

  def emitIFICMPGT(label: Int): String = {
    INDENT + "if_icmpgt Label" + label + END
  }

  def emitIFICMPGE(label: Int): String = {
    INDENT + "if_icmpge Label" + label + END
  }

  def emitIFEQ(label: Int): String = {
    INDENT + "ifeq Label" + label + END
  }

  def emitIFNE(label: Int): String = {
    INDENT + "ifne Label" + label + END
  }

  def emitIFLT(label: Int): String = {
    INDENT + "iflt Label" + label + END
  }

  def emitIFLE(label: Int): String = {
    INDENT + "ifle Label" + label + END
  }

  def emitIFGT(label: Int): String = {
    INDENT + "ifgt Label" + label + END
  }

  def emitIFGE(label: Int): String = {
    INDENT + "ifge Label" + label + END
  }

  def emitLABEL(label: Int): String = {
    "Label" + label + ":" + END
  }

  def emitGOTO(label: Int): String = {
    INDENT + "goto Label" + label + END
  }

  def emitINEG(): String = {
    INDENT + "ineg" + END
  }

  def emitFNEG(): String = {
    INDENT + "fneg" + END
  }

  def emitDUP(): String = {
    INDENT + "dup" + END
  }

  def emitDUPX2(): String = {
    INDENT + "dup_x2" + END
  }

  def emitPOP(): String = {
    INDENT + "pop" + END
  }

  def emitI2F(): String = {
    INDENT + "i2f" + END
  }

  def emitNEW(lexeme: String): String = {
    INDENT + "new " + lexeme + END
  }

  def emitNEWARRAY(lexeme: String): String = {
    INDENT + "newarray " + lexeme + END
  }
  
 

  def emitMULTIANEWARRAY(typ: String, dimensions: Int): String = {
    INDENT + "multianewarray " + typ + " " + dimensions + END
  }

  def emitINVOKESTATIC(lexeme: String, typ: String): String = {
    INDENT + "invokestatic " + lexeme + typ + END
  }

  def emitINVOKESPECIAL(lexeme: String, typ: String): String = {
    INDENT + "invokespecial " + lexeme + typ + END
  }

  def emitINVOKESPECIAL(): String = {
    INDENT + "invokespecial java/lang/Object/<init>()V" + END
  }

  def emitINVOKEVIRTUAL(lexeme: String, typ: String): String = {
    INDENT + "invokevirtual " + lexeme + typ + END
  }

  def emitIRETURN(): String = {
    INDENT + "ireturn" + END
  }

  def emitARETURN(): String = {
    INDENT + "areturn" + END
  }

  def emitFRETURN(): String = {
    INDENT + "freturn" + END
  }

  def emitRETURN(): String = {
    INDENT + "return" + END
  }

  def emitLIMITSTACK(in: Int): String = {
    ".limit stack " + in + END
  }

  def emitFCMPL(): String = {
    INDENT + "fcmpl" + END
  }

  def emitLIMITLOCAL(in: Int): String = {
    ".limit locals " + in + END
  }

  def emitVAR(in: Int, varName: String, intyp: String, fromLabel: Int, toLabel: Int): String = {
    ".var " + in + " is " + varName + " " + intyp + " from Label" + fromLabel + " to Label" + toLabel + END
  }

  def emitVAR(in: Int, varName: String, intyp: String): String = {
    ".var " + in + " is " + varName + " " + intyp + END
  }

  def emitMETHOD(lexeme: String, typ: String, isStatic: Boolean): String = {
    if (isStatic)
      END + ".method public static " + lexeme + typ + END
    else
      END + ".method public " + lexeme + typ + END
  }

  def emitENDMETHOD(): String = {
    ".end method" + END
  }

  def emitSOURCE(lexeme: String): String = {
    ".source " + lexeme + END
  }

  def emitCLASS(lexeme: String): String = {
    ".class " + lexeme + END
  }

  def emitSUPER(lexeme: String): String = {
    ".super " + lexeme + END
  }

  def emitSTATICFIELD(lexeme: String, typ: String, isFinal: Boolean, value: String): String = {
    val init = if (value.length > 0) " = " + value else ""
    if (isFinal)
      ".field static final " + lexeme + " " + typ + init + END
    else
      ".field static " + lexeme + " " + typ + init + END
  }

  def emitINSTANCEFIELD(lexeme: String, typ: String, isFinal: Boolean, value: String): String = {
    val init = if (value.length > 0) " = " + value else ""
    if (isFinal)
      ".field protected final " + lexeme + " " + typ + init + END
    else
      ".field protected " + lexeme + " " + typ + init + END
  }

  def emitACONST(): String = {
    INDENT + "aconst_null" + END
  }

  def emitFCMPG(): String = {
    INDENT + "fcmpg" + END
  }
  
  def emitANEWARRAY(lexeme: String): String = {
    INDENT + "anewarray " + lexeme + END
  }
  
  def emitIFACMPEQ(label: Int): String = {
    INDENT + "if_acmpeq Label" + label + END
  }
  def emitIFACMPNE(label: Int): String = {
    INDENT + "if_acmpne Label" + label + END
  }
  
}
