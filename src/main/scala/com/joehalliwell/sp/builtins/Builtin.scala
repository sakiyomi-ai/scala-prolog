package com.joehalliwell.sp

package builtin {
  trait Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State]
  }
}
