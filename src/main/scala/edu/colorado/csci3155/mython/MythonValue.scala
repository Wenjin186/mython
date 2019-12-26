package edu.colorado.csci3155.mython

sealed trait MythonValue
case class NumValue(f: Double) extends MythonValue
case object UnitValue extends MythonValue

/* Some utility functions are given for your convenience.  Use them if you wish to directly
manipulate values and avoid large number of case statements
 .
 */
object MythonValueUtils {

    /* numOfValue: convert a value to a double */
    def numOfValue(v: MythonValue): Double = v match {
        case NumValue(f) => f
        case _ => throw TypeError("Trying to convert a string to a numeric value")
    }


    def plusValues(v1: MythonValue, v2: MythonValue): MythonValue = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 + f2)
       // case (StringValue(s1), StringValue(s2)) => StringValue(s1 + s2)
        case _ => throw TypeError("Incompatible arguments for the + operator.")
    }

    def minusValues(v1: MythonValue, v2: MythonValue): MythonValue = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 - f2)
        case _ => throw TypeError("Incompatible arguments for the - operator.")
    }

    def multValues(v1: MythonValue, v2: MythonValue): MythonValue = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => NumValue(f1 * f2)
        //case (StringValue(s1), NumValue(f2)) => StringValue(s1 * f2.toInt)
        case _ => throw TypeError("Incompatible arguments for the * operator.")
    }

    def divValues(v1: MythonValue, v2: MythonValue): MythonValue = (v1, v2 ) match {
        case (NumValue(f1), NumValue(f2)) if f2 != 0.0 => NumValue(f1/f2)
        case (NumValue(f1), NumValue(0.0)) => throw RunTimeError("Divison by Zero!")
        case _ => throw TypeError("Incompatible arguments for the / operator.")
    }

    def geqValue(v1: MythonValue, v2: MythonValue): Boolean = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => f1 >= f2
        //case   (StringValue(s1), StringValue(s2)) => s1 >= s2
        case _ => throw TypeError("Incompatible arguments for the comparison operator.")
    }

    def leqValue(v1: MythonValue, v2: MythonValue): Boolean = geqValue(v2, v1)

    def eqValue(v1: MythonValue, v2: MythonValue): Boolean = (v1, v2) match {
        case (NumValue(f1), NumValue(f2)) => f1 == f2
        //case (StringValue(s1), StringValue(s2)) => s1 == s2
        case _ => throw TypeError("Incompatible arguments for the equality operator.")
    }
}


