package com.joehalliwell.sp

import scala.collection.immutable.{Nil, Seq}
import scala.util.parsing.combinator.{PackratParsers, RegexParsers}
import scala.util.parsing.input.CharSequenceReader
import scala.util.matching.Regex

class Parser(val operators: Seq[Regex]) extends RegexParsers with PackratParsers {
  override protected val whiteSpace = """(\s|%.*|(?m)/\*(\*(?!/)|[^*])*\*/)+""".r
  val string                        = """([^"\p{Cntrl}\\]|\\[\\/bfnrt]|\\u[a-fA-F0-9]{4})*""".r
  val quote                         = "(\"|')".r
  val eol                           = ".".r
  var _counter: Int                 = 0

  val atom: Parser[Atom] =
    """!|[a-z]\w*""".r ^^ {
      case v => Atom(v)
    }

  val number: Parser[Number] = """(\d+(\.\d+)?|\d*\.\d+)""".r ^^ {
      case v => Number(BigDecimal.valueOf(v.toDouble))
    }

  val str: Parser[Str] = quote ~> string <~ quote ^^ {
      case v => Str(v.replaceAll("\\\\n", "\n"))
    }

  val variable: Parser[Variable] =
    """(_?[A-Z]\w*|_)""".r ^^ {
      case name =>
        val n = if (name == "_") {
          val c = _counter
          _counter += 1
          s"__$c"
        } else name

        Variable(n)
    }

  def predicate: PackratParser[Predicate] =
    """[a-z]\w*""".r ~ ("(" ~> term <~ ")") ^^ {
      case head ~ arglist => {
        val args = unlist(arglist)
        Predicate(head, args.length, args)
      }
    }

  def list: Parser[Term] = "[" ~> listbody <~ "]"

  def listbody: Parser[Term] = repsep(exp, ",") ~ opt("|" ~> (variable | list)) ^^ {
    case headList ~ Some(tail) => list(headList, tail)
    case headList ~ None       => list(headList, Prolog.EmptyList)
  }

  lazy val exp: PackratParser[Term]      = predicate | atom | variable | number | str | list
  lazy val term                          = infix(operators)
  lazy val sentence: PackratParser[Term] = term <~ eol

  def infix(ops: Seq[Regex]): Parser[Term] =
    ops match {
      case op :: tail => {
        val lower = infix(tail)
        rep(lower ~ op) ~ lower ^^ {
          case left ~ right =>
            left.reverse.foldLeft(right)(
              (current, rest) => {
                rest match {
                  case left ~ op =>
                    Predicate(op, 2, Seq(left, current))
                }
              }
            )
        }
      }

      case _ =>
        //case Nil => exp | "(" ~> infix(operators) <~ ")"
        exp | "(" ~> infix(operators) <~ ")"
    }

  def parse(s: String): Either[String, Term] = phrase(sentence)(reader(s)) match {
    case Success(result, _) => Right(result)
    case failure: NoSuccess => Left(failure.msg)
  }

  def reader(s: String) = new PackratReader(new CharSequenceReader(s))

  def unlist(list: Term): Seq[Term] = list match {
    case Predicate(",", 2, Seq(head, tail), _, _) => head +: unlist(tail)
    case default                                  => Seq(default)
  }

  // Helper function to make lists
  def list(args: Seq[Term], tail: Term): Term = args match {
    case Nil     => tail
    case default => Predicate(Prolog.ListPredicate, 2, Seq(args.head, list(args.tail, tail)))
  }
}
