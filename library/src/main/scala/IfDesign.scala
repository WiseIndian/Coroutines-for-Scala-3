package coroutines

class B {
    var y: Int  = 0
}

abstract class IfCoroutine[T] extends Coroutine[T] {
    private val INITIAL_SCOPE = 0


    val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()

    /*
    * this will contain a variable name to indices of scope within which a variable is defined
    * so if scope number 1 and 3 define the variable foo varNameToScopes will contain the entry "foo" -> List(3, 1)
    * */
    private var varNameToScopes: scala.collection.mutable.Map[String, List[Int]] = 
        scala.collection.mutable.Map[String, List[Int]]()
    
    private var scopesVars: List[List[String]] = List()
    private var currentScope = INITIAL_SCOPE

    private def getScopesIndices(varName: String): List[Int] =  varNameToScopes.getOrElse(varName, List())

    private def getVarNameForScope(varName: String): String = {
        val indices = getScopesIndices(varName)
        val lastDefScopeIndex = 
            indices.find(index => index <= currentScope).getOrElse(sys.error("This program should not compile!!"))
        varName + lastDefScopeIndex
    }

    private def addToScopeVars(varName: String): Unit = {
        scopesVars = ((varName + currentScope) :: scopesVars.head) :: scopesVars.tail
    }
    //add the index of the current scope to the variable 
    protected def addNewDefinition(varName: String, value: Any): Unit = {
        //the list of scopes where a variable with varName is defined is getScopesIndices(varName) it is sorted in decreasing order
        val (beginning, end) = getScopesIndices(varName).span(scope => scope > currentScope) 
        val sorted = beginning ++ (currentScope +: end)

        varNameToScopes +=  (varName -> sorted)
        addToScopeVars(varName)
        map += getVarNameForScope(varName) -> value
    }


    protected def getValueFor[T](varName: String): T = {
        map.getOrElse(getVarNameForScope(varName), sys.error("This program shouldnt compile")).asInstanceOf[T]
    }

    //adding a new list of names of variables of the scope to the list of such list of all the scopes we are within.
    //this is used to remember the list of variables of the current scope in order to do some
    //cleanup later
    protected def enterScope(): Unit = {
        currentScope += 1

        //the condition shall not be true when we reenter an if previously entered before yielding once.
        if (scopesVars.size < currentScope) {
            scopesVars  = List() :: scopesVars
        }
    }

    //called before yielding
    protected def intialize(): Unit = {
        currentScope = INITIAL_SCOPE
    }

    protected def beforeYielding(): Unit = {
        state += 1
    }

    protected def exitScope(): Unit = {
        //All elements that were defined in the current scope are removed from the global map.
        map.subtractAll(scopesVars.head)

        scopesVars = scopesVars.tail
        assert(currentScope >= 0)
        currentScope -= 1
    }
}


class IfCoroutineExample4 extends IfCoroutine[Int] {
  // coroutine[Int] {
  //   val x = 0
  //   if (x == 0) {
  //     val x = 1
  //     yieldval(x)
  //     println("WHATEVER")
  //   }
  //   yieldval(x)
  // }
    def continue: Option[Int] = {
        intialize()
        enterScope()
        if (state == 0) {
            addNewDefinition("x", 0)
            if (getValueFor[Int]("x") == 0) {
                enterScope()
                addNewDefinition("x", 1)
                beforeYielding()
                return Some(getValueFor[Int]("x"))
            }

            //if there is no else we do that:
            beforeYielding()
            this.continue
        } else if (state == 1) {
            if (getValueFor[Int]("x") == 0) {
                enterScope()
                exitScope()
            }

            beforeYielding()
            return Some(getValueFor[Int]("x"))
        } else {
            return None
        }
    }

}

/*
coroutine[Int] {
    
    if (x < 10) {
        if (y < 10) {
            yieldval(1)
        } else {
            yieldval(2)
        }
    } else {
        yieldval(3)
    }
}
*/



/*
coroutine[Int] {
    
    if (x < 10) {
        yieldval(1)
    } else {
        yieldval(2)
    }
}

would get transformed to
*/

class IfCoroutineExample2(val x: Int) extends IfCoroutine[Int] {


    def continue: Option[Int] = {
        intialize()
        enterScope()
        if (state == 0) {
            addNewDefinition("x", x)
            if (getValueFor[Int]("x") < 10) {
                //entering a new scope
                enterScope()

                beforeYielding()
                return Some(1)
            } else {
                enterScope()

                beforeYielding()
                return Some(2)
            }
        } else {
            //we have to do all if tests once again
            if (getValueFor[Int]("x") < 10) {
                enterScope()
                exitScope()
            } else {
                enterScope()
                exitScope()
            }
            return None
        } 
    }
}



/*
coroutine[Int] {
    
    if (x < 10) {
        if (y < 10) {
            yieldval(1)
            yieldval(2)
        } else {
            yieldval(3)
        }
    } else {
        yieldval(4)
    }
}

would get transformed to
*/

class IfCoroutineExample3(val x: Int, val y: Int) extends IfCoroutine[Int] {
    def continue: Option[Int] = {
        intialize()
        enterScope()

        addNewDefinition("x", x) // we add a new definition for values external to continue
        addNewDefinition("y", y) //we have to do it at the beginning of the continue method

        if (state == 0) {
            if (getValueFor[Int]("x") < 10) {
                enterScope()
                if (getValueFor[Int]("y") < 10) {
                    enterScope()

                    beforeYielding()
                    return Some(1)
                } else {
                    beforeYielding()
                    return Some(3)
                }
                exitScope()
            } else {
                enterScope()
                beforeYielding()
                return Some(4)
            }

            return None
        } else if (state == 1) {
            if (getValueFor[Int]("x") < 10) {
                enterScope()
                if (getValueFor[Int]("y") < 10) {
                    enterScope()
                    beforeYielding()
                    return Some(2)
                } else {
                    enterScope() //If there is nothing left to do in the current if expression its quite useless..
                    exitScope()
                }
                exitScope()
            } else {
                enterScope()
                exitScope()
            }

            return None
        } else {
            if (getValueFor[Int]("x") < 10) {
                enterScope()
                if (getValueFor[Int]("y") < 10) { //IDEA: we could merge nested ifs conditions within one if.
                    enterScope()
                    exitScope()
                }
                exitScope()
            }

            return None
        }
    }
}


/*
coroutine[Int] {
    val bVal = b.y 
    if (b.y < 10) {
        val x = 3
        yieldval 10
        println(x)
    }
    yieldval bVal
}

would get transformed to:
*/
class IfCoroutineExample1(val b: B) extends IfCoroutine[Int] {


    def continue: Option[Int] = {
        intialize()
        enterScope()

        if (state == 0) {

             
            //val bVal = b.y  //imagine b is some class B instance where B has a "y" Int attribute
            //first we add "bVal1" to the list of variables of the current scope
            addNewDefinition("bVal", b.y)
             
            addNewDefinition("b.y", b.y) //this is a snapshot of an object attribute this will be useful when we reenter continue when state == 1
            if (getValueFor[Int]("b.y") < 10) {
                //entering a new scope
                enterScope()
                addNewDefinition("x", 3)
                beforeYielding()
                return Some(10)
            }
            beforeYielding()
            this.continue //the solution could be to call this' continue again if nothing was returned yet
        } else if (state == 1) {
            //we have to do all if tests once again
            if (getValueFor[Int]("b.y") < 10) {
                enterScope()
                println(      getValueFor[Int]("x")         )
                //we have to create a mapping from the original variable name of variable to the current variable name
                //and we have to create one such mapping per scope.
                //this will be the way to know which is the renamed variable we have to use.
                //this is going to be useful when have a variable name reused accross scopes
                
                //clean up of scope 
                exitScope()
            }

            beforeYielding()
            return Some(getValueFor[Int]("bVal"))
        } else {
            //final cleanup
            exitScope()
            None
        }
    }
}
