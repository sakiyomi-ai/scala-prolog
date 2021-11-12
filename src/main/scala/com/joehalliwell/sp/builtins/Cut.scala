package com.joehalliwell.sp

package builtin {
  object Cut extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] =
      Some(State(state.goals.tail, Seq(Stackframe(Atom("!"), state.env, 0)), -1)) // No backtracking!
  }
}
