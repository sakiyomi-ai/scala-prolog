package com.joehalliwell.sp

package builtin {
  object Or extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] =
      Some(
        State(
          Seq(args.head) ++ state.goals.tail,
          new Stackframe(state.goals.head, state.env, state.index) +: state.stack,
          -1
        )
      )
  }
}
