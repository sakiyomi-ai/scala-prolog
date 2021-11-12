package com.joehalliwell.sp

package builtin {
  object Assert extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] = {
      val term = args.head
      context.assert(term)
      state.push()
    }
  }
}
