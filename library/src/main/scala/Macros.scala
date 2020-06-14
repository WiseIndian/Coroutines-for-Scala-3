package coroutines


object Macros {
   
  import scala.quoted._
  import scala.quoted.matching._ 

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

 


      case  Try(tryBody: Term, catchCases: List[CaseDef], finalizer: Option[Term]) => 
        val tryCoExpr: Expr[Coroutine[T]] = coroutineImpl(tryBody.seal)

        '{
          lazy val tryCoroutine = ${tryCoExpr}
          
          def f(): Option[T] = {

            var tryRet: Option[T] = None
            
            ${
              val newTryBody: Term = '{ tryRet =  tryCoroutine.continue() }.unseal
              Try(newTryBody, catchCases, finalizer).seal
            } 

            if (tryRet.isDefined) { 
              ${ 
                val yieldvalExpr = '{ yieldval(tryRet.get) }.unseal
                
                transformTree(
                  yieldvalExpr, 
                  () => '{ f() } 
                )
              }
            }  else {
              ${ 
                nextContext()
              }
            }

          }
          f()
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

      /*
      〚join(co) 〛(c) =
        this.next = () => {
          val subresult = co.next()
          if (subResult.isDefined)
            return subResult

          this.next = () => ${c()}
          this.next()
        }
      */
      case Apply(TypeApply(Ident("join"), _), List(sub)) => 
        '{
          //the purpose of the following is to initialize the subcoroutine once and for all
          lazy val subcoroutine = ${sub.seal.cast[Coroutine[_ <: T]]}
          ${self}.next = () => {
            val subResult: Option[T] = subcoroutine.next()
            if (subResult.isDefined) {
              subResult
            } else {
              ${self}.next = () => ${nextContext()}
              ${self}.next()
            }
          }
          ${self}.next()
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




  /**
  Formally, the language can be defined as follows:

  C ::= coroutine { Z* }
  L ::= n | true | false | null | ...
  S ::= e | x = e | e.f = e | while(e) S* | if(e) S* else S* | val x: T = e | MethodDef | ClassDef | TypeDef
  e ::= L | x | e.f | e.f[T](e, .., e) | if(e) e else e | { S*; e } | new p.C(e, .., e)  | (x: T) => e | TryCatch
  Z ::= yield e | if(e) Z* else Z* | e | { Z* } | e | x = e | e.f = e | while(e) S* | e match { case e1 => Z*; ... case en => Z*} | join(co: Coroutine[_])
 
  The checker enforces the following properties:

      An expression e never contains yield or a join.
      Yield e never refers to a local definition defined in the coroutine body.
      The expression e within a yield has the same type as the wrapping coroutine.
      The argument co (of type Coroutine) of the join yields values that are subtypes of the current coroutine yielded values (the type T).

  @param type T is the expected return type of yieldval.
  */
  private def check[T](expr: Expr[_ <: Any])(implicit qtx: QuoteContext, expectedYieldType: quoted.Type[T], thisCoroutineType: quoted.Type[Coroutine[T]]): Unit = {
    import qtx.tasty.{_, given _} 

    def casuallyTraverse(tree: Tree)(implicit ctx: Context): Unit = {

      val treeTraverser = new TreeTraverser {
        override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
          case Inlined(call, bindings, expansion) => 
            traverseTree(expansion) 

          case Block(statements, blockRet) => 
            (statements :+ blockRet).foreach {traverseTree}
            
 
          case Apply(TypeApply(Ident("join"), List(joinTypeTree)), List(co)) /*join(co)*/ => 
            // get a typetree from co and thisCoroutineType
            val thisTypeTree: qtx.tasty.Type= thisCoroutineType.unseal.tpe

            /*
            * We get the information on the type of co by looking up the type parameter of the join method.
            * Indeed the type parameter of the join method is the same as the type parameter for the Coroutine class. 
            * See the definition of join. 
            */  
            val joinedIsSubtypeOfThisCoroutine: Boolean = thisTypeTree match {
              case AppliedType(coroutineTypeRef, List(parameterTypeRef: Type)) => 
               //Here we do a comparison between the join[T](.) parameter type T (that is the same co's type tree) AND parameterTypeRef that is the type of the current coroutine
                joinTypeTree.tpe <:< parameterTypeRef
              case _ => false
            }
 
            if (!joinedIsSubtypeOfThisCoroutine) {
              throw new Error(
                s"""The joined coroutine is not a subtype of the current coroutine
                ${tree.show} was found with this argument ${co.show} which is not a subtype of 
                the coroutine in which body this join occurs. You can join Coroutine[T2] in the body of Coroutine[T1] only if T2 <: T1.
                """.stripMargin
              )
            }

          case Apply(TypeApply(Ident("yieldval"), _), List(argument)) /*yieldval(argument)*/ => 

            // typeComparer is protected
            // val typeCorrect: Boolean = ctx.typeComparer.isSubType(argument.getType, t)
            // val typeCorrect: Boolean = ${argument.seal}.isInstanceOf[${t}]

            //is arguments type a subtype of the expected yield type?
            val correctArgumentType = (argument.tpe  <:< expectedYieldType.unseal.tpe)
            if (!correctArgumentType) {
              throw new Error(
                s"""yield argument has the wrong type:
                ${tree.show} was found with an argument of type ${ argument.tpe }
                We expected a subtype of ${ expectedYieldType }.
                """.stripMargin
              )
            }
            
            checkNoYieldval(argument, tree)
          
        
          case Try(tryBody, catchCases , finallyBodyOpt) => 
            traverseTree(tryBody)

            catchCases.foreach { case cdef @ CaseDef(pattern, optGuard, rhs) =>
              checkNoYieldval(pattern, tree)
              optGuard.foreach(guard => checkNoYieldval(guard, tree))
              checkNoYieldval(rhs, tree)
            }

            finallyBodyOpt.foreach(checkNoYieldval(_, tree))

          case If(cond, thenp, elsep) => 
            checkNoYieldval(cond, tree)
            traverseTree(thenp)
            traverseTree(elsep)
          

          case Match(selector, cases) => {
            val errSelector = checkNoYieldval(selector, tree)

            cases.foreach{ case cdef @ CaseDef(pattern, optGuard, rhs) => 
              checkNoYieldval(pattern, cdef)
              optGuard.foreach { guard => checkNoYieldval(guard, cdef) }
              traverseTree(rhs)
            }

          }
            
          case While(condTerm, bodyTerm) => 
            traverseTree(bodyTerm) 
            checkNoYieldval(condTerm, tree)
            
          case _ => 
            checkNoYieldval(tree, tree)
        }
      }
      
      treeTraverser.traverseTree(tree)
    }

    def checkNoYieldval(tree: Tree, parentTree: Tree)(implicit ctx: Context): Unit = {

      val traverser = new qtx.tasty.TreeTraverser {
        override def traverseTree(tree: Tree)(implicit ctx: Context): Unit = tree match {
          case Apply(TypeApply(Ident("yieldval"), _), List(argument)) /*yieldval(argument)*/ =>

            //TODO figure out how to print position of the code. Maybe with http://dotty.epfl.ch/docs/reference/metaprogramming/tasty-reflect.html#positions
            throw new Error(
              s"""A yield contained within this context is not allowed:
              The problematic yield is the following:
              ${tree.show}

              context:
              ${parentTree.show} 
              """.stripMargin
              )
            
          case Apply(TypeApply(Ident("join"), _), List(co)) /*join(coroutine)*/ =>
            throw new Error(
              s"""A join on another coroutine should not happen within this context:" +
              The problematic join call is the following:
              ${tree.show}

              context:
              ${parentTree.show}
              """.stripMargin
            )
              
          case _ => super.traverseTree(tree)
        }
      }

      traverser.traverseTree(tree)
    }
        
    
    return casuallyTraverse(expr.unseal)
  }
 


  private def coroutineImpl[T: Type](expr: Expr[_ <: Any])(implicit qtx: QuoteContext): Expr[Coroutine[T]] = {

    check[T](expr)

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
