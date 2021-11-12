package com.joehalliwell.sp

import scala.annotation.tailrec
import scala.collection.immutable._
import scala.util.matching.Regex

import builtin._
import io.{Console, ConsoleSimple}

/*
 * An experiment with writing a Prolog interpreter in Scala
 * @author Joe Halliwell <joe.halliwell@gmail.com>
 */

object Prolog {
  val EmptyList     = Atom("[]")
  val ListPredicate = "."
}

class Prolog(val console: Console = new ConsoleSimple()) {
  var trace    = false
  val database = scala.collection.mutable.Buffer[Term]()
  val operators = Seq[Regex](
    """(:-|query)""".r,
    """(;|,)""".r,
    """(is|=)""".r,
    """(::|==|=:=|\\=|=\\=|\\==|=>|<=|>=|=<|>|<)""".r,
    """(\+|-)""".r,
    """(\*|/|mod|\^|log)""".r
  )
  val parser           = new Parser(operators)
  implicit val context = this

  //type Builtin = (Seq[Term], com.joehalliwell.sp.Env) => com.joehalliwell.sp.Env

  def assert(fact: Term)        = database += fact
  def consult(filename: String) = Consult.consult(filename)
  def clear()                   = database.clear()

  // Unused?
  def solve(term: Term): Option[State] = solve(State(Seq(term), Seq(), -1))

  // Find a solution
  def solve(initialState: State): Option[State] = {

    @tailrec def step(s: Option[State]): Option[State] = {
      if (trace) console.println(s)
      s match {
        case None                     => None
        case Some(s) if s.isSolution  => Some(s)
        case Some(s) if s.index == -1 => step(tryBuiltins(s))
        case Some(s)                  => step(trySearch(s))
      }
    }

    step(if (initialState.isSolution) initialState.pop() else Some(initialState))
  }

  protected def tryBuiltins(state: State): Option[State] =
    //if (state.index > -1) return trySearch(state) // HACK!
    //println(s"Checking for builtins...:$state")
    state.goals.head match {
      case Atom("!", _, _)                     => Cut.exec(state, Seq.empty)
      case Predicate("assert", 1, args, _, _)  => Assert.exec(state, args)
      case Predicate("consult", 1, args, _, _) => Consult.exec(state, args)
      case Predicate("write", 1, args, _, _)   => Write.exec(state, args)
      case Predicate("query", 1, args, _, _)   => Query.exec(state, args)
      case Predicate(",", 2, args, _, _)       => And.exec(state, args)
      case Predicate(";", 2, args, _, _)       => Or.exec(state, args)
      case Predicate("=", 2, args, _, _)       => Equal.exec(state, args)
      case Predicate("is", 2, args, _, _)      => Is.exec(state, args)
      case Predicate("::", 2, args, _, _)      => Probability.exec(state, args)
      case Predicate(op, 2, args, _, _)
          if op == "==" |
          op == "=:=" |
          op == "\\=" |
          op == "\\==" |
          op == "=\\=" |
          op == "=>" |
          op == ">=" |
          op == "<=" |
          op == "=<" |
          op == ">" |
          op == "<" =>
        Arithmetic.exec(state, args)
      case Predicate("evidence", 2, args, _, _) => Evidence.exec(state, args)
      case default                              => Some(State(state.goals, state.stack, 0))
    }

  def prepare(terms: Term): Term = terms match {
    case Predicate("::", 2, args, _, _) =>
      Probability.evaluate(args) match {
        case Some(nterms) => prepare(nterms)
        case _            => terms
      }
    case _ => terms
  }

  protected def trySearch(state: State): Option[State] = {
    //println("Searching database..." + state.goals.head)
    @tailrec
    def f(index: Int, length: Int): Option[State] =
      if (index >= length) {
        state.pop()
      } else {
        database(index) match {
          // Rules
          case Predicate(":-", 2, args, _, _) => {
            //println(s"=> unify(rule)... head= ${state.goals.head}, args=$args")
            val pf = prepare(args.head)
            state.env.unify((state.goals.head, pf.renameVars(state.stack.length + 1))) match {
              case env: Success => {
                //println("==> success..." + env)
                Some(
                  State(
                    args.tail.map(_.renameVars(state.stack.length + 1)) ++ state.goals.tail,
                    new Stackframe(state.goals.head, env, index) +: state.stack,
                    -1
                  )
                )
              }
              //case default => f(index + 1, length) //Option.empty[State]
              case _ => f(index + 1, length)
            }
          }

          // Simple facts
          case fact => {
            //println(s"=> unify(fact)... head= ${state.goals.head}, $fact")
            val pf = prepare(fact)
            state.env.unify((state.goals.head, pf.renameVars(state.stack.length + 1))) match {
              case env: Success => {
                //println("==>Matched fact...")
                state.push(env, index)
              }
              //case default => f(index + 1, length) //Option.empty[State]
              case _ => f(index + 1, length)
            }
          }
        }
      }

    f(state.index, database.length)
  }

}
