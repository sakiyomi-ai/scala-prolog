package com.joehalliwell.sp

import scala.annotation.tailrec

package builtin {
  object Query extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] = {
      val rhs = evaluate(args.head)
      state.env.unify((args.head, rhs)) match {
        case Fail         => state.pop()
        case env: Success => state.push(env, -1)
      }
    }

    def evaluate(goal: Term)(implicit context: Prolog): Bool = {
      val solution                            = State(Seq(goal), Seq(), -1)
      var collect: Seq[Map[Term, BigDecimal]] = Seq.empty

      @tailrec
      def next(solution: State): Unit = context.solve(solution) match {
        case None => ()
        case Some(solution) => {
          if (!solution.env.probability.isEmpty) {
            if (context.trace) {
              context.console.println(s"P=${solution.env.probabilityResult}")
              context.console.println(solution.env.probabilityTerm)
            }
            collect = collect :+ solution.env.probability
          }

          if (solution.mayHaveMore) {
            if (context.trace) context.console.println(";")
            next(solution)
          }
        }
      }

      next(solution)

      val r  = collect.map(e => e.values.foldLeft(Number.one)((x, y) => x * y))
      val r2 = Number.one - r.map(e => Number.one - e).foldLeft(Number.one)((x, y) => x * y)

      context.console.println(s"P=$r2")
      if (context.trace) context.console.println(collect)

      Bool(true)
    }

  }
}
