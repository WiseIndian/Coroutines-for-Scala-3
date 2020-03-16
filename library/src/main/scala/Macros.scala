package coroutines

//TODO create private github repository, create junit tests TDD and then implement. 
//create notes. Create issues for questions.
//Create anonymous class
object Macros {
   
  import scala.quoted._
  import scala.quoted.matching._ 
   
  inline def debug2[T](inline x: T): T = ${ debugImpl('{x}) }

  def debugImpl[T: Type](x: Expr[T])(implicit qtx: QuoteContext): Expr[T] = '{
    val a: T = ${x}
    println(${Expr(x.show(qtx))} +" = "+ a)
    a   
  } 
   
 
  

    //TODO do something with the context: problem can we cast the context(term) back to T?



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
  def fetchFunctions[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): Seq[Expr[() => Option[T]]] = {
    import qtx.tasty.{_, given _}  

    /*
    *problem cant define this function if we didnt do "import qtx.tasty.{_, given _}" because Block and Term are part of those packages
    * thus we are forced to define this function where we are given a QuoteContext.
    */
    def fetchFunctionsFromBlock(block: Block, context: Term => Term): Seq[Expr[() => Option[T]]] = { 
      type FunDefAcc =  Seq[Expr[() => Option[T]]]
      val initialFunDefsAcc = Seq[Expr[() => Option[T]]]()
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
      val Block(stats, expr) = block
      val (funDefsExprs, leftOverStatements) = (stats :+ expr).foldLeft[(FunDefAcc, Seq[Statement])](initialAccumulator) {
        case ((funDefs, statements), Apply(TypeApply(Ident("yieldval"), _), List(argument))) => 
          val argumentAsExpr: Expr[Option[T]] = '{ Some(   ${ argument.seal.cast[T] }   ) }
          val newFunDefExpr: Expr[() => Option[T]] = '{ () =>
            ${Block(statements.toList, argumentAsExpr.unseal).seal.cast[Option[T]]} 
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
        case statements => Some(Block(statements.toList, '{None}.unseal))
      } map { case block => 
        funDefsExprs :+ '{ () => ${   block.seal.cast[Option[T]]   } }
      } getOrElse {
        funDefsExprs
      }
  
    }
  
 
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
    def helper(ast: Term, context: Term => Term)(implicit qtx: QuoteContext): Seq[Expr[() => Option[T]]] = ast match {

      case Inlined(call, bindings, expansion) => 
        def newContext(term: Term): Term = context(Inlined(call, bindings, term))
        helper(expansion, newContext)
      case block: Block => 
        fetchFunctionsFromBlock(block, context)
    }
 

    val ast: Term = expr.unseal

    print(ast.showExtractors) 
    //a pair containing (anonymous functions, statements that were not in included in function bodies)
    helper(ast, t => t) 
     
  }

  inline def coroutine[T](inline body: Any): Coroutine[T] = ${ coroutineImpl('{body}) }


  def coroutineImpl[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): Expr[Coroutine[T]] = {
    import qtx.tasty.{_, given _}

    val funDefsExprs: Seq[Expr[() => Option[T]]] = fetchFunctions[T](expr)
    val nbFunDefs = funDefsExprs.knownSize
     
    '{
      new Coroutine[T] { 
        var state: Int = 0 

        def continue: Option[T] = ${
          val caseDefs: List[CaseDef] = (0 until nbFunDefs).map[CaseDef] {
            //index => CaseDef('{index}.unseal, None, '{f${index}()}.unseal)
            index => '{  case ${index} => f${index}() }.unseal //doesnt seem to work
          }
          
          Match('{state}.unseal, caseDefs).seal.cast[Option[T]] 
        } 
          
        ${
          funDefsExprs.zipWithIndex.foldLeft[Expr[_ <: Any]]( '{} ) {
            case (previousExpr, (funDef, index)) => 
              '{
                ${previousExpr};
                def f${index} = ${funDef} //is there a way to add the "private" modifier without having the compiler screaming?
              }
          }
        }

      }

    }
  }


}
