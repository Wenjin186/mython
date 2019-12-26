package edu.colorado.csci3155.mython

import org.scalatest.FunSuite

class InterpreterTests extends FunSuite {
    def processString(s: String): MythonValue = {
        val p = new MythonParser()
        val prog = p.parseString(s)
        println(s"Parsed Program AST: $prog")
        EvaluateProgram.evalProgram(prog)
    }
    test ("Simple Program 1") {
        val s =
            """
              |var x := 1
              |var y := 5
              |var z := x + y
              |return x + y * z
            """.stripMargin
        val x = processString(s)
        println(s"Computed Value $x")
        assert(x == NumValue(31.0))
    }

    test ("Simple Program 2") {
        val s =
            """
              | var x := 5
              | var y := x - 10
              | var z := x * y - 10
              | return z/(z+1)
            """.stripMargin
        val x = processString(s)
        println(s"Computed Value $x")
        x match {
            case NumValue(f) => assert (math.abs(f -1.0294117647058822 ) <= 1e-06, "Value out of range")
            case _ => assert (false, "Wrong value type")
        }
    }

    test("Program 3") {
        val s =
            """
              | var x := 8
              | var y := 4
              | var z := 1
              | var i := 0
              | while (i <= 8)
              | begin
              |     x := x  + y + 3
              |     y := y +  x - 2
              |     z := z +  z - 1
              |     i := i + 1
              | end
              | return (x + y + z)
            """.stripMargin
        val x = processString(s)
        println(s"Computed Value $x")
        x match {
            case NumValue(f) => assert (math.abs(f - 72441.0 ) <= 1e-06, "Value out of range")
            case _ => assert (false, "Wrong value type")
        }
    }

    test("Program 4") {
        val s =
            """
              | var x := 1.0
              | var y := 5.0
              | while (!(x*x - y <= 0.000001 && x*x - y >= -0.00000001))
              | begin
              |     x := x - (x*x - y)/(2.0*x)
              | end
              | return x
            """.stripMargin
        val x = processString(s)
        println(s"Computed Value $x")
        x match {
            case NumValue(f) => assert (math.abs(f - 2.236067977499978 ) <= 1e-06, "Value out of range")
            case _ => assert (false, "Wrong value type")
        }
    }

    test("Program 5") {
        val s =
            """
              | var sum := 1.0
              | var term := 1.0
              | var prevSum := -1.0
              | var n := 1
              | while (!(prevSum - sum <= 0.000000001 && prevSum - sum >= -0.000000001))
              | begin
              |    prevSum := sum
              |    term := term * 1/n
              |    sum := sum + term
              |    n := n + 1
              | end
              |
              | if (sum <= 3.0)
              | then begin
              |     return 1
              |     end
              | else begin
              |    return 0
              |    end
            """.stripMargin
        val x = processString(s)
        println(s"Computed Value $x")
        x match {
            case NumValue(f) => assert (math.abs(f - 1.0 ) <= 1e-06, "Value out of range")
            case _ => assert (false, "Wrong value type")
        }
    }


}
