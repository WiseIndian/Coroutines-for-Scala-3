package coroutines

class B {
    var y: Int  = 0
}

abstract class IfCoroutine[T] extends Coroutine[T] {
    val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()

    /*
    * this will contain a variable name to indices of scope within which a variable is defined
    * so if scope number 1 and 3 define the variable foo varNameToScopes will contain the entry "foo" -> List(3, 1)
    * */
    var varNameToScopes: scala.collection.mutable.Map[String, List[Int]] = 
        scala.collection.mutable.Map[String, List[Int]]()
    
    var scopesVars: List[List[String]] = List()
    var currentScope = 1

    protected def getScopesIndices(varName: String): List[Int] =  varNameToScopes.getOrElse(varName, List())

    //add the index of the current scope to the variable 
    protected def addNewDefinition(varName: String, value: Any): Unit = {
        varNameToScopes +=  (varName -> (getScopesIndices(varName) :+ currentScope))
        addToScopeVars(varName)
        map += getLatestDefinitionFor(varName) -> value
    }

    protected def getLatestDefinitionFor(varName: String): String = {
        /*
        * TODO: Is it possible to compile the code given in the coroutine body before actually transforming it? This would 
        * avoid the obligation to do checks on definitions..
        */
        val lastDefScopeIndex = getScopesIndices(varName).headOption.getOrElse(sys.error("This program should not compile!!"))
        varName + lastDefScopeIndex
    }

    protected def addToScopeVars(varName: String): Unit = {
        scopesVars = ((varName + currentScope) :: scopesVars.head) :: scopesVars.tail
    }

    protected def getLatestValueFor[T](varName: String): T = {
        map.getOrElse(getLatestDefinitionFor(varName), sys.error("This program shouldnt compile")).asInstanceOf[T]
    }

    //adding a new list of names of variables of the scope to the list of such list of all the scopes we are within.
    //this is used to remember the list of variables of the current scope in order to do some
    //cleanup later
    protected def enterScope(): Unit = {
        currentScope += 1
        scopesVars  = List() :: scopesVars
    }

    protected def exitScope(): Unit = {
        //remove all elements in map that are within scoperVars.head
        map.subtractAll(scopesVars.head)

        scopesVars = scopesVars.tail
        assert(currentScope > 0)
        currentScope -= 1
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
        enterScope()
        if (state == 0) {
            addNewDefinition("x", x)
            if (getLatestValueFor[Int]("x") < 10) {
                //entering a new scope
                enterScope()

                state += 1
                return Some(1)
            } else {
                enterScope()
                state += 1
                return Some(2)
            }
        } else {
            //we have to do all if tests once again
            if (getLatestValueFor[Int]("x") < 10) {
                exitScope()
            } else {
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
        enterScope()
        addNewDefinition("x", x) // we add a new definition for values external to continue
        addNewDefinition("y", y) //we have to do it at the beginning of the continue method
        
        if (state == 0) {
            if (getLatestValueFor[Int]("x") < 10) {
                enterScope()
                if (getLatestValueFor[Int]("y") < 10) {
                    enterScope()

                    state += 1
                    return Some(1)
                } else {
                    state += 1
                    return Some(3)
                }
                exitScope()
            } else {
                enterScope()
                state += 1
                return Some(4)
            }

            return None
        } else if (state == 1) {
            if (getLatestValueFor[Int]("x") < 10) {
                if (getLatestValueFor[Int]("y") < 10) {
                    state += 1
                    return Some(2)
                } else {
                    exitScope()
                }
                exitScope()
            } else {
                exitScope()
            }

            return None
        } else {
            if (getLatestValueFor[Int]("x") < 10) {
                if (getLatestValueFor[Int]("y") < 10) { //IDEA: we could merge nested ifs conditions within one if.
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
        enterScope()
        if (state == 0) {

             
            //val bVal = b.y  //imagine b is some class B instance where B has a "y" Int attribute
            //first we add "bVal1" to the list of variables of the current scope
            addNewDefinition("bVal", b.y)
             
            addNewDefinition("b.y", b.y) //this is a snapshot of an object attribute this will be useful when we reenter continue when state == 1
            if (getLatestValueFor[Int]("b.y") < 10) {
                //entering a new scope
                enterScope()
                addNewDefinition("x", 3)

                state += 1

                return Some(10)
            }

            state += 1
            this.continue //the solution could be to call this' continue again if nothing was returned yet
        } else if (state == 1) {
            //we have to do all if tests once again
            if (getLatestValueFor[Int]("b.y") < 10) {
                println(      getLatestValueFor[Int]("x")         )
                //we have to create a mapping from the original variable name of variable to the current variable name
                //and we have to create one such mapping per scope.
                //this will be the way to know which is the renamed variable we have to use.
                //this is going to be useful when have a variable name reused accross scopes
                
                //clean up of scope 
                exitScope()
            }

            state += 1
            return Some(getLatestValueFor[Int]("bVal"))
        } else {
            //final cleanup
            exitScope()
            None
        }
    }
}