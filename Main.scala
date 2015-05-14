// Project name MP
// Create by: nhphung
// Create date: Aug 27, 2013
// Language: Scala
// Description: This file is about driver for the recognizer for MP language, see MP language specification for more information

import scala.io.Source
import java.io.{PrintWriter,File}
import java.util.concurrent.{Executors,TimeUnit,TimeoutException}

object NeedHelp extends Exception

trait Timed {
	def timeoutAfter(timeout: Long)(codeToTest: => Unit): Unit = {
		val executor = Executors.newSingleThreadExecutor
		val future = executor.submit(new Runnable {
			def run = codeToTest
		})

		try {
			future.get(timeout, TimeUnit.MILLISECONDS)
		}
		finally {
			executor.shutdown()
		}
	}
}

object Main extends Timed {
  def main(args: Array[String]): Unit = {
  	try {
      	//if (args.length == 0 || args.length != 5) throw NeedHelp
		val option = args(0).drop(1)
		if (option == "help") throw NeedHelp
		
		//val start = java.lang.Integer.parseInt(args(1))
		//val end = java.lang.Integer.parseInt(args(2))
		//val indir = args(3)
		//val outdir = args(4)
    	//val sep = "\\"
	 	//for (i <- start to end) {
			val inputFile = args(1)
			val source = Source.fromFile(inputFile)
			//val dest = new PrintWriter(new File(outdir + sep + i + ".txt"))
			val lines = source.getLines
			val input = if (!lines.isEmpty) lines.reduceLeft[String](_ + '\n' + _) else ""
			
			option match {  
	  			case "testlexer" => {
					val lexical = new BKOOLLexer
					val scanner = new lexical.Scanner(input)
					try {
						timeoutAfter(5000) {
		  					runAll(scanner)
						}
					} catch {
						case te: TimeoutException => println("Test runs timeout")
						case e: Exception => println(e)
					} finally {
						source.close()
						//dest.close()
					}

					def clean(token: lexical.Token): String = {
						val t = token.chars
						if (t.indexOf("expected but") != -1) {
							val from = t.indexOf("but")
							val to = t.indexOf("found")
							"ErrorToken " + t.subSequence(from + 4, to - 1).toString
						} else token.toString
					}
			
					def runAll(scan: lexical.Scanner): Any = if (!scan.atEnd) {
						  println(clean(scan.first))
						  runAll(scan.rest)
					  }
				}

	  			case "testrecogniser" => {
					try {
						timeoutAfter(5000) {
							val parser = new BKOOLParser
							val result = parser.parse(input)
		  					println(parser.show(result))
						}
					} catch {
						case te: TimeoutException => println("Test runs timeout")
						case e: Exception => println(e)
					} finally {
						source.close()
						//dest.close()
					}
					
				}

				case "testparser" => {
					try {
						timeoutAfter(5000) {
							val parser = new BKOOLParser
							val result = parser.parse(input)
							val ast = result.get
							println(ast)
						}
					} catch {
						case te: TimeoutException => println("Test runs timeout")
						case e: Exception => println(e)
					} finally {
						source.close()
						//dest.close()
					}
				}

				case "testchecker" => {
					try {
						timeoutAfter(5000) {
							val parser = new BKOOLParser
							val result = parser.parse(input)
							val ast = result.get
							
							try {
								Checker.check_program(ast)
							} catch {
								case Undeclared(k, n) => println("Undeclared " + k + ": " + n)
								case CannotAssignToConstant(s) => println("Cannot Assign To Constant: " + s)
								case TypeMismatchInStatement(s) => println("Type Mismatch In Statement: " + s)
								case TypeMismatchInExpression(e) => println("Type Mismatch In Expression: " + e)
								case Redeclared(k, d) => println("Redeclared " + k + ": " + d)
								case TypeMismatchInConstant(c) => println("Type Mismatch In Constant: " + c)
								case CannotAccessPrivateAttribute(c, m) => println("Cannot Access Private Attribute: " + m + " of class " + c)
								case MethodNotReturn(m) => println("Method Not Return: " + m)
							}
						}
					} catch { 
						case e: RuntimeException => println(e)
						case te: TimeoutException => println("Test runs timeout")
					} finally {
						source.close()
						//dest.close()
					}
				}
				case "testcodegeneration" => {
					try {
						timeoutAfter(5000) {
							val parser = new BKOOLParser
							val result = parser.parse(input)
							val ast = result.get
							//val flder = new File(outdir + sep + i)
							//val succ = flder.mkdir
							//if (succ) {
								val emitter = new Emitter(JasminCode)
								val codeGenerator = new CodeGenerator(ast, "result",emitter)
								codeGenerator.run
							//} else
								//println("Cannot make a directory "+ outdir + sep + i)
						}
	    			} catch {
	    				case e:Exception  => e.printStackTrace
	    			}
				}
	  			case _ => throw NeedHelp
			}
		//}
		
	} catch {
		case NeedHelp => printHelp
		case t:Exception => println(t)
	} 
  }

  def printHelp = {
    println("Usage: scala Main -option start end inDirectory  outDirectory")
    println("where option is")
    println("\thelp: print this help")
    println("\ttestlexer: test lexer")
    println("\ttestrecogniser: test recogniser")
    println("\testparser: test parser")
    println("\ttestchecker: test checker")
    println("\ttestcodegeneration: test code ")
    println("start: the first test-case")
    println("end: the last test-case")
    println("inDirectory: the directory contains test-cases")
    println("outDirectory: the directory contains solutions")
  }
}
