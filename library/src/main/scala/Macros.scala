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
   
 
   

  inline def coroutine[T](inline body: Any): Coroutine[T] = ${ coroutineImpl[T]('{body}) }


  //We return a function from coroutine to expression because it is a way to pass the `this` coroutine
  //instance as a parameter called self (`this` doesnt work in place where we use self here)
  def transformBody[T: Type](expr: Expr[_ <: Any], nextContext: () => Expr[Option[T]])(self: Expr[Coroutine[T]])(implicit qtx: QuoteContext): Expr[Option[T]] = {
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


      /*〚if (e₁) Z₁* else Z₂*〛(c)  = '{
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

  def coroutineImpl[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): Expr[Coroutine[T]] = {


    def fetchBody(self: Expr[Coroutine[T]]): Expr[Option[T]] = {
      val lastNext = () => '{${self}.next = () => None; None}
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
