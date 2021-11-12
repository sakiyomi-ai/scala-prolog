package com.joehalliwell.sp
import scala.util.Try

package builtin {
  object Evidence extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] = {
      val rhs = evaluate(state.env, (args.head, args.tail.head))
      if (rhs.boolVal) {
        state.env.unify((args.head, rhs)) match {
          case Fail         => state.pop()
          case env: Success => state.push(env, -1)
        }
      } else state.pop()
    }

    def evaluate(env: Env, terms: (Term, Term))(implicit context: Prolog): Bool =
      terms match {
        case (t1: Atom, t2: Atom) =>
          val v      = Try(t2.value.toLowerCase.toBoolean).getOrElse(false)
          val target = t1.extract(env)
          //println(s"Evidence.target = $target")
          val t = context.database.map(e => context.prepare(e)).filter {
            case e: Atom => e == target
            case _       => false
          }
          t.foreach {
            case e: Atom => e.evidence = Option(v)
            case _       => ()
          }
          Bool(!t.isEmpty)
        case (t1: Predicate, t2: Atom) =>
          val v      = Try(t2.value.toLowerCase.toBoolean).getOrElse(false)
          val target = t1.extract(env)
          //println(s"Evidence.target = $target")
          val t = context.database.map(e => context.prepare(e)).filter {
            case e: Predicate => e deepEquals target
            case _            => false
          }
          t.foreach {
            case e: Predicate => e.evidence = Option(v)
            case _            => ()
          }
          Bool(!t.isEmpty)
        case _ =>
          Bool(false)
      }
  }
}
