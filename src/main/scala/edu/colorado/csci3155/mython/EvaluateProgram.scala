package edu.colorado.csci3155.mython

import scala.collection.mutable.Map

object EvaluateProgram {


    /* TODO: Implement this.
       1. Create a new environment,
       2. Create a new store
       3. Process each declaration (make a helper function)
       4. Return the final environment and store.
     */
    def evalMultipleDecls(decls: List[Declaration]) : (Map[String, Double], MythonStore) =  {

        var env: Map[String, Double] = Map[String,Double]()
        val store = new MythonStore()

        def evalValue (expr: Expr) : Double = {
            expr match {
                case ConstNum(e1) => e1
                case Ident(e1) => env(e1)
                case Plus(e1, e2) => evalValue(e1) + evalValue(e2)
                case Minus(e1, e2) => evalValue(e1) - evalValue(e2)
                case Mult(e1, e2) => evalValue(e1) * evalValue(e2)
                case Div(e1, e2) => evalValue(e1) / evalValue(e2)
                case _ => {
                    throw new IllegalArgumentException("Now Support")
                    println("Not Support")
                    println(expr)
                    1.0
                };
            }

        }

        for (decl <- decls){
            decl match {
                case VarDecl(s, expr) => {
                    var value = evalValue(expr).toInt
                    env += (s -> value)
                    store.createNewCell(NumValue(value))
                }
            }

        }

        return (env, store)
    }


    /* TODO: Implement this.

        The single step function takes a list of statements, an environment and
        store (mutable) and returns
        a value (can be UnitValue or value of return expression)
        a list of statements remaining to execute
        Please follow the semantics given in the assignment description.
        Note: that we deviate from the semantics in that Store is a mutable.
     */
    def singleStep(stmtList: List[Statement],
                   env: Map[String, Double],
                   store: MythonStore): (MythonValue, List[Statement]) = {

        val stat = stmtList.head
        val remain = stmtList.drop(1)
       // println(remain)

        stat match {
            case ReturnStmt(retExpr) => return (EvaluateExpression.evalExpr(retExpr, env, store), remain);
            case AssignStmt(identifier, rhsExpr) => {
                var value = EvaluateExpression.evalExpr(rhsExpr, env, store)
                env(identifier) = MythonValueUtils.numOfValue(value)
                return (value, remain)
            }
            case WhileStmt(cond, stmts) => {
                while (EvaluateExpression.evalCondExpr(cond, env, store)) {
                    var stmtsRemaining: List[Statement] = stmts
                    var curVal: MythonValue = UnitValue
                    //3. While there are statements remaining
                    while (stmtsRemaining.length > 0) {
                        //4. Run a single step
                        val (a: MythonValue, b: List[Statement]) = singleStep(stmtsRemaining, env, store)
                        curVal = a
                        stmtsRemaining = b
                    }
                }
                return (UnitValue, remain)
            }
            case IfThenElseStmt(cond, stmtsThen, stmtsElse) => {
                if (EvaluateExpression.evalCondExpr(cond, env, store)) {
                    println(stmtsThen)
                    var stmtsRemaining: List[Statement] = stmtsThen
                    var curVal: MythonValue = UnitValue
                    //3. While there are statements remaining
                    while (stmtsRemaining.length > 0) {
                        //4. Run a single step
                        val (a: MythonValue, b: List[Statement]) = singleStep(stmtsRemaining, env, store)
                        curVal = a
                        stmtsRemaining = b
                    }
                    return (curVal, remain)
                } else {
                    println(stmtsElse)
                    var stmtsRemaining: List[Statement] = stmtsElse
                    var curVal: MythonValue = UnitValue
                    //3. While there are statements remaining
                    while (stmtsRemaining.length > 0) {
                        //4. Run a single step
                        val (a: MythonValue, b: List[Statement]) = singleStep(stmtsRemaining, env, store)
                        curVal = a
                        stmtsRemaining = b
                    }
                    return (curVal, remain)
                }

            }
        }
    }

    def evalProgram(p: Program): MythonValue = {
        p match {
            case Program(decls, stmts) =>
                //1. Initialize environment and store
                val (env, store) = evalMultipleDecls(decls)
                //2. Set the statements remaining.
                var stmtsRemaining: List[Statement] = stmts
                var curVal: MythonValue = UnitValue
                //3. While there are statements remaining
                while (stmtsRemaining.length > 0) {
                    //4. Run a single step
                    val (a:MythonValue, b: List[Statement]) = singleStep(stmtsRemaining, env, store)
                    curVal = a
                    stmtsRemaining = b
                }
                //5. Return the value
                curVal
        }
    }

}
