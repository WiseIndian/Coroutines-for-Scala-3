//TODO create private github repository, create junit tests TDD and then implement. 
//create notes. Create issues for questions.
//Create anonymous class
object Macros {
  
  import scala.quoted._
  import scala.quoted.matching._ 
  
  import CoroutineUtils._
  inline def debug2[T](inline x: T): T = ${ debugImpl('{x}) }

  def debugImpl[T: Type](x: Expr[T])(implicit qtx: QuoteContext): Expr[T] = '{
    val a: T = ${x}
    println(${Expr(x.show(qtx))} +" = "+ a)
    a   
  }
  
  type IntermediaryFunExpr[T] = Expr[() => Option[T]]
  type IntermediaryFuns[T] = Seq[IntermediaryFunExpr[T]]

  //need qtx for typechecking the expression
  inline def test()(implicit qtx: QuoteContext) = ${
      fetchFunctions[Int]('{
        val x = 1
        yieldval(x)
        val y = x + 1
        yieldval(2*2)
        y
      })
    }
  


  /*This takes an expression @param expr like 
    `'{
          val x = 1
          yieldval(x)
          val y = x + 1
          yieldval(2*2)
      }`
    as parameter and returns a pair containing a sequence of blocks together with a block.
    The Sequence of blocks contains the anonymous functions (with no parameters) to which
    the blocks of codes like  
    `val x = 1
    ...
    yieldval(x)`
    were transformed. Such a sequence of code is transformed to () => { val x = 1; ...; x}

    thus the exemple input is rewritten to 
    (Seq(() => {val x = 1; x}, () => { val y = x+1; 2 * 2}), () => {y})
  */
  def fetchFunctions[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): (Seq[Block], Block) = {
    import qtx.tasty.{_, given _}  

    val initialExpr: IntermediaryFunExpr[T] = '{() => None}
    val initialSeq: IntermediaryFuns[T] = Seq(initialExpr)
    
    val ast: Term = expr.unseal

    print(ast.showExtractors)
 
    def helper(ast: Term, wrapper: Term => Term, extractedFunctionsASTs: Seq[Term])(implicit qtx: QuoteContext): (Seq[Block], Seq[Statement]) = ast match {

    case Inlined(call, bindings, expansion) => 
      def newWrapper(term: Term): Term = wrapper(Inlined(call, bindings, term))
      helper(expansion, newWrapper, extractedFunctionsASTs)
    case Block(statements, expr) => 
      val initialFunDefsAcc = Seq[Block]()
      val initialStatementsAcc = Seq[Statement]()

      val initialAccumulator = (initialFunDefsAcc, initialStatementsAcc)
      statements.foldLeft[(Seq[Block], Seq[Statement])](initialAccumulator) {
        case ((funDefs, statements), Apply(TypeApply(Ident("yieldval"), _), List(argument))) => //TODO verify that yield indeed contains List(argument)
        
          val anonFunName = "$anonfun"
          val anonFunSymbol: Symbol =
            Symbol.newMethod(
              /*parent symbol: rootContext.owner?*/Symbol.noSymbol,
              anonFunName, 
              ByNameType(typeOf[T]))
          //see tasty tests folder in tests/run-macros/tasty-create-method-symbol/Macro_1.scala
          //I could also use let ?
          val functionRepr = 
            Block(
              List(
                DefDef(
                  anonFunSymbol, {
                    case List() => {
                      case List() =>
                        Some(Block(statements.toList, argument))
                    }
                  }
                )
              ),
              Closure(Ref(anonFunSymbol), None)
            )
          
          (funDefs :+ functionRepr, Seq())

        //TODO inside foldleft treat blocks recursions.
        case ((funDefs, statements), anythingElse) => 
          (funDefs, statements :+ anythingElse)

      }

    }
 

    //a pair containing (anonymous functions, statements that were not in included in function bodies)
    val (funDefs, leftoverStatements): (Seq[Block], Seq[Statement]) = helper(ast, t => t, Seq())
    
    val anonFunName = "$anonfun"
    val anonFunSymbol =
      Symbol.newMethod(
        /*parent symbol: rootContext.owner?*/Symbol.noSymbol,
        anonFunName, 
        ByNameType(typeOf[Unit])
      )
    val unitRetAnonymousFun: Block = Block(
      List(
        DefDef(
          anonFunSymbol, {
            case List() => {
              case List() =>
                Some(Block(leftoverStatements.toList, Block(List(), Literal(Constant(())))   ))
            }
          }
        )
      ),
      Closure(Ref(anonFunSymbol), None)
    )

     
  }

  //inline def coroutine[T: Liftable: Type](inline body: Any): Coroutine[T] = ${ coroutineImpl('{body}) }

  //TODO 
  def coroutineImpl[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): Expr[Coroutine[T]] = '{
      new Coroutine[T] { 
          var state: Int = 0

 
          //split the initial function in multiple parts which are separated by yields
          //create a function for each part.
          //map each function to a case.

          def continue: Option[T] = ???
          ???
      }
  }
}
