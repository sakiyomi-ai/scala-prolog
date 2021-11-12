package com.joehalliwell.sp

package builtin {
  object And extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] =
      Some(
        State(args ++ state.goals.tail, new Stackframe(state.goals.head, state.env, state.index) +: state.stack, -1)
      )
  }
}
