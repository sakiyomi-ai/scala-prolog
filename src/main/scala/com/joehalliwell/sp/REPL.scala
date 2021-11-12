package com.joehalliwell.sp

import scala.annotation.tailrec
import scala.collection.immutable._
import io.ConsoleImpl

object REPL {

  val prompt = "\n? "
  val welcome =
    """|Simple Prolog Interpreter in Scala
          |
          |Type \? for help""".stripMargin

  val p = new Prolog(new ConsoleImpl())

  def main(args: Array[String]) = {

    p.console.setPrompt("? ")

    p.console.println(welcome)
    p.console.println("-" * welcome.length)

    p.consult("initial.pl")

    // The REPL
    while (true) {
      try {
        process(p.console.readLine())
      } catch {
        case t: Throwable => t.printStackTrace();
      }
    }
  }

  val HELP = """|\q  quit
    |\l list
    |\c clear database
    |\? help
    |\t trace""".stripMargin

  def process(line: String) = line match {
    case "\\q" => System.exit(0)
    case "\\l" => p.console.println(p.database.mkString("\n"))
    case "\\c" =>
      p.clear()
      p.consult("initial.pl")
    case "\\?" => p.console.println(HELP)
    case "\\t" => {
      p.trace = !p.trace
      p.console.println("Tracing " + (if (p.trace) "on" else "off") + ".")
    }
    case _ => {
      p.parser.parse(line) match {
        case Right(term) =>
          if (p.trace) p.console.println("Parse: " + term)
          prove(term)
        case Left(emsg) => p.console.println("Syntax error : " + emsg)
      }
    }
  }

  def prove(goal: Term) = {
    val solution = State(Seq(goal), Seq(), -1)

    @tailrec def next(solution: State): Unit =
      p.solve(solution) match {
        case None => {
          p.console.println("No.")
        }
        case Some(solution) => {
          if (!solution.env.binding.isEmpty) {
            p.console.println(solution.env)
            printProbability(solution)
          }

          if (!solution.mayHaveMore) {
            p.console.println("Yes.")
            if (solution.env.binding.isEmpty) printProbability(solution)
          } else {
            p.console.print("More y/n")
            if (p.console.readLine().startsWith("y")) next(solution)
          }
        }
      }
    next(solution)
  }

  def printProbability(solution: State) =
    if (!solution.env.probability.isEmpty) {
      p.console.println(s"P=${solution.env.probabilityResult}")
      if (p.trace) p.console.println(solution.env.probabilityTerm)
    }

}
