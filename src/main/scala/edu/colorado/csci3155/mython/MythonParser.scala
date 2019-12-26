package edu.colorado.csci3155.mython
import scala.util.parsing.combinator._

/* Recursive Descent Parser : Do not modify */
class ExprParser extends RegexParsers {
    def floatingPointNumber: Parser[String] = {
        """-?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?[fFdD]?""".r
    }
    def identifier: Parser[String] = {
        """[a-zA-Z0-9_]+""".r
    }


    def expr: Parser[Expr] = {
       term ~ opt(("-" | "+") ~ expr) ^^ {
            case s1~None => s1
            case s1~Some("-"~s2) => Minus(s1, s2)
            case s1~Some("+"~s2) => Plus(s1, s2)
        }
    }

    def term: Parser[Expr] = {
        val opt1 = leaf ~ opt(("*" | "/") ~ term) ^^ {
            case s1 ~ Some("/" ~ s2) => Div(s1, s2)
            case s1 ~ Some("*" ~ s2) => Mult(s1, s2)
            case s1 ~ None => s1
        }

        val opt4 = "-" ~> term ^^ {
            s => Minus( ConstNum(0.0), s)
        }


        opt4 | opt1
    }

    def leaf: Parser[Expr] = {
        val opt1 = floatingPointNumber ^^ {
            s => ConstNum(s toDouble)
        }
        val opt2 = identifier ^^ {
            s => Ident(s)
        }


        val opt4 = "(" ~> (expr <~ ")")



        opt1 |  opt4 | opt2


    }


}

class CondExprParser extends ExprParser {
    def constBool: Parser[CondExpr] = {
        "true"^^{ _ => ConstTrue }  |   "false"^^{_ => ConstFalse}
    }

    def relOp : Parser[String] = {
        ">=" | "<=" | "=="
    }

    def condExpr: Parser[CondExpr] = {
        condClause ~ opt("||" ~ condExpr) ^^ {
            case t1 ~ None => t1
            case t1 ~ Some("||"~t2) => Or(t1, t2)
        }
    }

    def condClause: Parser[CondExpr] = {
        condLit ~ opt("&&" ~ condClause) ^^ {
            case t1 ~ None => t1
            case t1 ~ Some("&&"~t2) => And(t1, t2)
        }
    }

    def condLit: Parser[CondExpr] = {
        val opt1 = ("!" ~> condLit) ^^ {Not(_)}
        val opt2 = ("(" ~> condExpr) <~ ")"
        val opt3 = expr ~ (">="|"<="|"==") ~ expr ^^ {
            case e1 ~">="~ e2 => Geq(e1, e2)
            case e1 ~"<="~ e2 => Leq(e1, e2)
            case e1 ~"=="~ e2 => Eq(e1, e2)
        }
        val opt4 = constBool

        opt2 | opt1 | opt3 | opt4
    }
}


class MythonParser extends CondExprParser {
    def program: Parser[Program] = rep(declaration)~rep(statement) ^^ {
        case l1~l2 =>  Program(l1, l2)
        }


    def declaration: Parser[VarDecl] = ("var" ~> identifier) ~ (":=" ~> expr) ^^ {
        case id ~ e => VarDecl(id, e)
    }

    def statement: Parser[Statement] = assignStatement | whileStatement | ifThenElseStatement | returnStatement

    def assignStatement: Parser[Statement] = (identifier <~ ":=") ~ expr ^^ { case id ~ e => AssignStmt(id, e) }

    def whileStatement: Parser[Statement] = "while" ~> (condExpr ~ stmtBlock) ^^ {case c ~ blk => WhileStmt(c, blk)}

    def ifThenElseStatement: Parser[Statement] = ("if"~> condExpr) ~ ("then" ~> stmtBlock) ~ opt("else" ~> stmtBlock) ^^ {
        case c ~ st ~ None => IfThenElseStmt(c, st, List())
        case c ~ st1 ~ Some(st2) => IfThenElseStmt(c, st1, st2)
    }

    def returnStatement: Parser[Statement] = "return" ~> expr ^^ {
        ReturnStmt(_)
    }

    def stmtBlock: Parser[List[Statement]] = ("begin" ~> rep(statement)) <~ "end"

    def parseString(str: String): Program = {
        parseAll(program, str)  match {
            case Success(mt, _) => mt
            case Failure(msg, _) => throw new IllegalArgumentException(msg)
            case Error(msg, _) => throw new IllegalArgumentException(msg)
        }
    }
}
