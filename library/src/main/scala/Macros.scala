package coroutines

//TODO create private github repository, create junit tests TDD and then implement. 
//create notes. Create issues for questions.
//Create anonymous class
object Macros {
   
  import scala.quoted._
  import scala.quoted.matching._ 
   
  // inline def printTree[T](inline x: T): T = ${ printTreeImpl('{x}) }

  // def printTreeImpl[T: Type](x: Expr[T])(implicit qtx: QuoteContext): Expr[T] = {
  //   println(x.unseal)
    
  //   x
  // }
   
 
   

  inline def coroutine[T](inline body: Any): Coroutine[T] = ${ coroutineImpl[T]('{body}) }


  //We return a function from coroutine to expression because it is a way to pass the `this` coroutine
  //instance as a parameter called self (`this` doesnt work in place where we use self here)
  private def transformBody[T: Type](expr: Expr[_ <: Any], nextContext: () => Expr[Option[T]])(self: Expr[Coroutine[T]])(implicit qtx: QuoteContext): Expr[Option[T]] = {
    import qtx.tasty.{_, given _}

    def transformTree(tree: Statement, nextContext: () => Expr[Option[T]])(implicit qtx: QuoteContext): Expr[Option[T]] = tree match {
      case i @ Inlined(call, bindings, expansion) => 
        transformTree(expansion, nextContext)
      case Block(Nil, blockRet) => 
        transformTree(blockRet, nextContext)


      // applying this rule〚Z₀ Z*〛(c) = 〚Z₀〛{ () => 〚Z*〛(c) } if there are 2 or more elements in the block
      case Block(firstStat :: tailStats, blockRet) => 
        val newNextContext = () => transformTree(Block(tailStats, blockRet), nextContext)

        transformTree(firstStat, newNextContext)

      /*
      very similar to if transformation's the match/case transformation:

      [[ x match {  case e1 => Z₁*; case e2 => Z₂*; ...  case en => Zn* } 〛(c)  becomes 
      '{
         val next = () => ${c()}
         x match {
           case e1 =>  ${〚Z₁*〛{ () => '{ next()} } }
           case e2 => ${〚Z₂*〛〛{ () => '{next()} } }
           ...
           case en => ${〚Zn*〛〛{ () => '{next()} } }
         }
       }
       */
       case Match(selector, cases) => 
        '{
          val next = () => ${nextContext()}
          ${ 
            val newCaseDefs = 
              cases.map { case CaseDef(pattern, guard, rhs) => 
                val newRhs = transformTree(rhs, () => '{next()}).unseal
                CaseDef(pattern, guard, newRhs)
              }

            Match(selector, newCaseDefs).seal.cast[Option[T]]
          }
        }


      /*〚if (e₁) Z₁* else Z₂*〛(c)  becomes 
        '{
            val next = () => ${c()}
            if (e₁) ${〚Z₁*〛{ () => '{next()} } } else ${〚Z₂*〛〛{ () => '{next()} } }
          } 
      */
      case If(cond, thenp, elsep) => 
        '{
          val next = () => ${nextContext()}
          if (${cond.seal.cast[Boolean]}) {
            ${transformTree(thenp, () => '{next()})}
          } else {
            ${transformTree(elsep, () => '{next()})}
          }
        }
      
      // 〚yield e〛(c) = '{ this.next = () => ${c()}; Some(e) }
      case Apply(TypeApply(Ident("yieldval"), _), List(argument)) => 
        '{
          ${self}.next = () => ${nextContext()}
          Some(${argument.seal.cast[T]})
        }

      case While(condTerm, bodyTerm) => 
        // inline def body = 
        val cond = condTerm.seal.cast[Boolean]
        '{
          def f(): Option[T] = if (${cond}) ${
            transformTree(bodyTerm, () => '{ f() })
          } else ${ 
            nextContext() 
          }
           
          f()
        }
      //〚Z〛(c) = '{ Z; ${c()} }, otherwise
      case _ => 
        Block(tree::Nil, nextContext().unseal).seal.cast[Option[T]]
        
    }

    transformTree(expr.unseal, nextContext)
  }

  /*
  * this method enforces that no yieldval is contained within the @param parentTree
  */




  /*
  Let us first consider a language where yield may appear in if-statements and blocks. Formally, the language can be defined as follows:

  C ::= coroutine { Z* }
  L ::= n | true | false | null | ...
  S ::= e | x = e | e.f = e | while(e) S* | if(e) S* else S* | val x: T = e | MethodDef | ClassDef | TypeDef
  e ::= L | x | e.f | e.f[T](e, .., e) | if(e) e else e | { S*; e } | new p.C(e, .., e)  | (x: T) => e | TryCatch
  Z ::= yield e | if(e) Z* else Z* | e | { Z* } | e | x = e | e.f = e | while(e) S* | e match { case e1 => Z*; ... case en => Z*}

  In the above, we restrict that the language that may appear in the coroutine body must be of the form Z*. We can implement a checker easily to enforce the syntax.

  The checker enforces the following properties:

      An expression e never contains yield.
      Yield e never refers to a local definition defined in the coroutine body.
      The expression e within a yield has the same type as wrapping coroutine.

  @param type T is the expected type of yieldval.
  */
  private def invokeCheckerImpl[T](expr: Expr[_ <: Any])(implicit qtx: QuoteContext, t: Type[T]): Boolean = {
    import qtx.tasty.{_, given _} 

    val expectedYieldType = t

    def casuallyTraverse(tree: Tree)(implicit ctx: Context): Boolean = {

      val acc = new TreeAccumulator[Boolean] {
        def foldTree(errorFound: Boolean, tree: Tree)(implicit ctx: Context): Boolean = tree match {
          case Inlined(call, bindings, expansion) => 
            foldTree(errorFound, expansion) 

          case Block(statements, blockRet) => 
            (statements :+ blockRet).foldLeft(errorFound) { case (foundAcc, tree) =>
              foldTree(foundAcc, tree)
            }


          case Apply(TypeApply(Ident("yieldval"), _), List(argument)) /*yieldval(argument)*/ => 

            // typeComparer is protected
            // val typeCorrect: Boolean = ctx.typeComparer.isSubType(argument.getType, t)
            // val typeCorrect: Boolean = ${argument.seal}.isInstanceOf[${t}]
            //TODO check the type of argument is a subtype of T: check out dotty.core.TypeComparer.scala 
            // val typeCorrect: Boolean = argument.tpe == expectedYieldType
            val typeCorrect = true //TODO remove me
            // if (!typeCorrect) {
            //   System.err.println(
            //     s"""yield argument has the wrong type:
            //     ${tree.show} was found with an argument of type ${ argument.tpe }
            //     We expected a subtype of ${ expectedYieldType }.
            //     """.stripMargin
            //   ) //TODO take only 10 characters or so of parentTree.show and app.show
            // }
            
            checkNoYieldval(errorFound || !typeCorrect, argument, tree)
          
          
          case If(cond, thenp, elsep) => 
            val errFound1: Boolean = checkNoYieldval(errorFound, cond, tree)
            val errFound2: Boolean = foldTree(errFound1, thenp)
            foldTree(errFound2, elsep)
          
          case Block(stats, blockRet) => 
            val errFound1 = foldTrees(errorFound, stats)
            
            foldTree(errFound1, blockRet)
 
          case Match(selector, cases) => {
            val errSelector = checkNoYieldval(errorFound, selector, tree)

            cases.foldLeft(errSelector) { case (errFound, cdef @ CaseDef(pattern, optGuard, rhs)) => 
              val errPattern = checkNoYieldval(errFound, pattern, cdef)
              val errGuard = optGuard.map { guard => checkNoYieldval(errPattern, guard, cdef) }.getOrElse(false)
              foldTree(errPattern || errGuard, rhs)
            }

          }
            
          case While(condTerm, bodyTerm) => 
            val errFound1 = foldTree(errorFound, bodyTerm) 
            checkNoYieldval(errFound1, condTerm, tree)
            
          case _ => 
            checkNoYieldval(errorFound, tree, tree)
        }
      }
      
      acc.foldTree(false, tree)
    }

    //add red to the message
    def processErrorMsg(s: String): Unit = {
      val ANSI_RED = "\u001B[31m"
      val ANSI_RESET = "\u001B[0m"
      val modified =
        s
        .stripMargin
        .split("\n")
        .map(line => ANSI_RED+"[error]"+ANSI_RESET+"\t"+line)
        .mkString("\n") +"\n\n"

      System.err.println(modified)
    }
    def checkNoYieldval(errorFound: Boolean, tree: Tree, parentTree: Tree)(implicit ctx: Context): Boolean = {

      val acc = new qtx.tasty.TreeAccumulator[Boolean] {
        def foldTree(errorFound: Boolean, tree: Tree)(implicit ctx: Context): Boolean = tree match {
          case app @ Apply(TypeApply(Ident("yieldval"), _), List(argument)) /*yieldval(argument)*/ =>
            processErrorMsg(
              s"""A yield contained within this context is not allowed:
              yield in question:
              ${app.show}

              context:
              ${parentTree.show} 
              """
              ) //TODO take only 10 characters or so of parentTree.show and app.show
              
            foldTree(true, argument)
              
          case _ => foldOverTree(errorFound, tree)
        }
      }

      acc.foldTree(errorFound, tree)
    }
        
    
    return casuallyTraverse(expr.unseal)
  }
 

  inline def invokeChecker[T](inline body: Any): Boolean = ${ Expr(invokeCheckerImpl[T]('{body})) }

  private def coroutineImpl[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): Expr[Coroutine[T]] = {

    val errorFound: Boolean = invokeCheckerImpl[T](expr)
    if (errorFound) {
      throw YieldvalAtWrongLocationException("Error(s) found in the coroutine body")
    }

    def fetchBody(self: Expr[Coroutine[T]]): Expr[Option[T]] = {
      val lastNext = () => '{
        ${self}._isDone = true
        
        ${self}.next = () => None
        
        None
      }



      transformBody[T](expr, lastNext)(self)
    }

    
    val resultingCoroutineClass = '{
      new Coroutine[T] {
        lazy val body: Option[T] = {
          val self: Coroutine[T] = this
          ${
            val transformation = fetchBody('self)
            println("Resulting transformation  \n"+transformation.show)
            transformation
          }
        }

        
      } 
    }

    resultingCoroutineClass
  }


}
