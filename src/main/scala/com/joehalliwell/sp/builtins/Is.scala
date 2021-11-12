package com.joehalliwell.sp

package builtin {
  object Is extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] = {
      val rhs = Arithmetic.evaluate(args.tail.head, state.env)
      state.env.unify((args.head, rhs)) match {
        case Fail         => state.pop()
        case env: Success => state.push(env, -1)
      }
    }
  }
}
