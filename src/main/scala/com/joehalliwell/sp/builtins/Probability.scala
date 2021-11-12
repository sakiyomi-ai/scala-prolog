package com.joehalliwell.sp

package builtin {
  object Probability extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] =
      //println(s"probability.exec state=$state, args=$args")
      evaluate(args) match {
        case Some(rhs) =>
          state.env.unify((args.tail.head, rhs)) match {
            case Fail         => state.pop()
            case env: Success => state.push(env, -1)
          }
        case _ => state.pop()
      }

    def evaluate(args: Seq[Term]): Option[Term] =
      //println(s"probability set, ${args.tail.head}, ${args.head}")
      args.tail.head match {
        case t1: Atom =>
          t1.probability = Option(args.head)
          Option(t1)
        case t1: Predicate =>
          t1.probability = Option(args.head)
          Option(t1)
        case _ => None
      }
  }
}
