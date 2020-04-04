package coroutines
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

class B {
    var y: Int  = 0
}

class IfCoroutine(val b: B) extends Coroutine[Int] {
    val map: scala.collection.mutable.Map[String, Any] = scala.collection.mutable.Map[String, Any]()

    /*
    * this will contain a variable name to indices of scope within which a variable is defined
    * so if scope number 1 and 3 define the variable foo varNameToScopes will contain the entry "foo" -> List(3, 1)
    * */
    var varNameToScopes: scala.collection.mutable.Map[String, List[Int]] = 
        scala.collection.mutable.Map[String, List[Int]]()
    
    var scopesVars: List[List[String]] = List()
    var currentScope = 1

    private def getScopesIndices(varName: String): List[Int] =  varNameToScopes.getOrElse(varName, List())

    //add the index of the current scope to the variable 
    private def addNewDefinition(varName: String, value: Any): Unit = {
        varNameToScopes +=  (varName -> (getScopesIndices(varName) :+ currentScope))
        addToScopeVars(varName)
        map += getLatestDefinitionFor(varName) -> value
    }

    private def getLatestDefinitionFor(varName: String): String = {
        /*
        * TODO: Is it possible to compile the code given in the coroutine body before actually transforming it? This would 
        * avoid the obligation to do checks on definitions..
        */
        val lastDefScopeIndex = getScopesIndices(varName).headOption.getOrElse(sys.error("This program should not compile!!"))
        varName + lastDefScopeIndex
    }

    private def addToScopeVars(varName: String): Unit = {
        scopesVars = ((varName + currentScope) :: scopesVars.head) :: scopesVars.tail
    }

    private def getLatestValueFor[T](varName: String): T = {
        map.getOrElse(getLatestDefinitionFor(varName), sys.error("This program shouldnt compile")).asInstanceOf[T]
    }

    //adding a new list of names of variables of the scope to the list of such list of all the scopes we are within.
    //this is used to remember the list of variables of the current scope in order to do some
    //cleanup later
    private def enterScope(): Unit = {
        currentScope += 1
        scopesVars  = List() :: scopesVars
    }

    private def exitScope(): Unit = {
        //remove all elements in map that are within scoperVars.head
        map.subtractAll(scopesVars.head)

        scopesVars = scopesVars.tail
        currentScope -= 1
    }

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