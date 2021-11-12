package com.joehalliwell.sp

package builtin {
  object Write extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] = {
      write(args.head, state.env)
      state.push()
    }

    protected def write(term: Term, env: Success)(implicit context: Prolog): Unit = term match {
      case v: Variable =>
        env.binding.get(v) match {
          case Some(t: Term) => write(t, env)
          case None          => context.console.print(v)
        }
      case default => context.console.print(term)
    }
  }
}
