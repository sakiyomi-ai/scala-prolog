package com.joehalliwell.sp

package builtin {
  object Equal extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] =
      state.env.unify((args.head, args.tail.head)) match {
        case Fail         => state.pop()
        case env: Success => state.push(env, -1)
      }
  }
}
