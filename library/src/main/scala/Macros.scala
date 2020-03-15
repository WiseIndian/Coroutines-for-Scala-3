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
  

    //TODO do something with the context: problem can we cast the context(term) back to T?
  def fetchFunctionsFromBlock[T: Type](block: Block, context: Term => Term)(implicit qtx: QuoteContext): Seq[Expr[() => Option[T]]] { 

    type FunDefAcc =  Seq[Expr[() => T]]
    val initialFunDefsAcc = Seq[Expr[() => T]]()
    val initialStatementsAcc = Seq[Statement]()

    //the algorithm is as follows:
    /*
    * Traverse the list of statements from the block.
    * Accumulate statements until we fall upon a yieldval. When this happens,
    * we convert the list of statements that we accumulated as an expression.
    * To convert a statement to an expression we can simply do tree.seal.cast[T]
    * 
    */
    val initialAccumulator = (initialFunDefsAcc, initialStatementsAcc)

    val (funDefsExprs, leftOverStatements) = (block.stats :+ block.expr).foldLeft[(FunDefAcc, Seq[Statement])](initialAccumulator) {
      case ((funDefs, statements), Apply(TypeApply(Ident("yieldval"), _), List(argument))) => 
        val argumentAsExpr: Expr[Option[T]] = '{ Some(${argument.unseal}) }
        val newFunDefExpr: Expr[() => Option[T]] = '{ () =>
          ${Block(statements, argumentAsExpr.unseal).seal.cast[Option[T]]} 
        }
        (funDefs :+ newFunDefExpr, Seq()) 
      //TODO inside foldleft treat blocks recursions.
      case ((funDefs, statements), anythingElse) => 
        (funDefs, statements :+ anythingElse) 
    }

    /*if there are any leftover statements we create a function which
    * when invoked run those statements and returns None of type T
    */
    leftOverStatements match {
      case Seq() => None
      case statements => Some(Block(statements, '{None}.unseal))
    } map { case block => 
      funDefsExprs :+ '{ () => ${   block.seal.cast[Option[T]]   } }
    } getOrElse {
      funDefsExprs
    }
 
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
  def fetchFunctions[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): (Seq[Expr[() => T]], Option[Expr[() => Any]]) = {
    import qtx.tasty.{_, given _}  
 
    val ast: Term = expr.unseal

    print(ast.showExtractors) 
    /*
    * TODO: how to deal with values that are shared between functions?
    * We could pull variables in the Coroutine class. Maybe we could changes vals to vars? But then we would
    * need to track variables to see if they have been assigned twice. We could change functions within
    * the coroutine body and pass them arguments containing values of vals within the same block..
    * 
    * 
    *this function takes an @param abstract syntax tree ast as input.
    * It then traverses this ast and returns a pair of a seqeuence of expression of functions that return T.
    * and optionally an expression of a function which can return anything.
    * The sequence of expression of functions that return T are transformations of statements that end with a yieldval:
    * 
    * val x = 3
    * yieldval 2 * 2
    * val y = 2
    * yieldval 1+1
    * 
    * would be transformed and be returned as 
    * Seq(`{() => {val x = 3; 2 * 2}}, `{() => {val y = 2; 1+1}})
    **/
    def helper(ast: Term, context: Term => Term)(implicit qtx: QuoteContext): (Seq[Expr[() => T]], Option[Expr[() => Any]]) = ast match {

      case Inlined(call, bindings, expansion) => 
        def newContext(term: Term): Term = wrapper(Inlined(call, bindings, term))
        helper(expansion, newContext, extractedFunctionsASTs)
      case block: Block => 
        fetchFunctionsFromBlock(block, context)
    }
 

    //a pair containing (anonymous functions, statements that were not in included in function bodies)
    helper(ast, t => t) 
     
  }

  inline def coroutine[T: Liftable: Type](inline body: Any): Coroutine[T] = ${ coroutineImpl('{body}) }

  //TODO 
  def coroutineImpl[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): Expr[Coroutine[T]] = {
    //split the initial function in multiple parts which are separated by yields
    //create a function for each part.
    //map each function to a case.  
    val funDefsExprs: Seq[Expr[() => Option[T]]] = fetchFunctions[T](expr)
    
    val optionalLastFunAsSeq = optionalLastFun.map{lastFun => Seq(lastFun)}.getOrElse(Seq())
    '{
      new Coroutine[T] { 
        var state: Int = 0 

        def continue: Option[T] = state match { ${
          val nbFunDefs = funDefsExprs.knownSize

          (0 until nbFunDefs).foldLeft[Expr[_ <: Any]]( '{} ) {
            case (previousExpr, (funDef, index)) => 
              '{
                ${previousExpr} 
                case ${index} => f${index}()
              }
          }

        } }

        ${
          funDefsExprs.zipWithIndex.foldLeft[Expr[_ <: Any]]( '{} ) {
            case (previousExpr, (funDef, index)) => 
              '{
                ${previousExpr}
                private def f${index} = ${funDef}
              }
          }
        }

      }

    }
  }


}
