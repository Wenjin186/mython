package edu.colorado.csci3155.mython

import scala.collection.mutable.Map
/*--
    We will define the expression evaluation functions in this object.
 */

object EvaluateExpression {

    /*TODO: Complete this */
    def evalExpr(e: Expr, env: Map[String, Double], store: MythonStore): MythonValue = e match {
        case ConstNum(d) => NumValue(d)
        case Ident(s) => NumValue(env(s))
        case Plus(e1, e2) => MythonValueUtils.plusValues(evalExpr(e1, env, store), evalExpr(e2, env, store))
        case Minus(e1, e2) => MythonValueUtils.minusValues(evalExpr(e1, env, store), evalExpr(e2, env, store))
        case Mult(e1, e2) => MythonValueUtils.multValues(evalExpr(e1, env, store), evalExpr(e2, env, store))
        case Div(e1, e2) => {
            var temp = MythonValueUtils.divValues(evalExpr(e1, env, store), evalExpr(e2, env, store))
            //println(temp)
            return temp
        }
        case _ => throw new IllegalArgumentException("Not Support");
    }

    /*TODO: Complete this */
    def evalCondExpr(e: CondExpr, env: Map[String, Double], store: MythonStore): Boolean = e match {
        case ConstFalse => false
        case ConstTrue => true
        case Geq(e1, e2) => MythonValueUtils.geqValue(evalExpr(e1, env, store), evalExpr(e2, env, store))
        case Leq(e1, e2) => MythonValueUtils.leqValue(evalExpr(e1, env, store), evalExpr(e2, env, store))
        case Eq(e1, e2) => MythonValueUtils.eqValue(evalExpr(e1, env, store), evalExpr(e2, env, store))
        case And(e1, e2) => evalCondExpr(e1, env, store) && evalCondExpr(e2, env, store)
        case Or(e1, e2) => evalCondExpr(e1, env, store) || evalCondExpr(e2, env, store)
        case Not(e1)=> {
            ! evalCondExpr(e1, env, store)
        }

    }
}
