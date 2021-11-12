package com.joehalliwell.sp

package builtin {
  object Arithmetic extends Builtin {
    def exec(state: State, args: Seq[Term])(implicit context: Prolog): Option[State] = {
      val rhs = evaluate(state.goals.head, state.env)
      state.env.unify((args.head, rhs)) match {
        case Fail         => state.pop()
        case env: Success => state.push(env, -1)
      }
    }

    /**
      * Simple arithmetic expression evaluator.
      * TODO: Handle brackets? Can this be made tail-recursive?
      */
    final def evaluate(term: Term, env: Env): Term = term match {
      case v: Atom   => v
      case v: Number => v
      case v: Bool   => v
      case v: Str    => v
      case v: Variable =>
        env match {
          case senv: Success =>
            senv.binding.get(v) match {
              case None       => throw new Exception("Unbound variable on RHS " + v);
              case Some(term) => evaluate(term, env)
            }
          case _ => throw new Exception("Unbound variable on RHS " + v);
        }
      case Predicate("+", 2, args, _, _)   => Number(evaluate(args.head, env) + evaluate(args.tail.head, env))
      case Predicate("*", 2, args, _, _)   => Number(evaluate(args.head, env) * evaluate(args.tail.head, env))
      case Predicate("-", 2, args, _, _)   => Number(evaluate(args.head, env) - evaluate(args.tail.head, env))
      case Predicate("/", 2, args, _, _)   => Number(evaluate(args.head, env) / evaluate(args.tail.head, env))
      case Predicate("mod", 2, args, _, _) => Number(evaluate(args.head, env) % evaluate(args.tail.head, env))
      case Predicate("^", 2, args, _, _)   => Number(evaluate(args.head, env) ^ evaluate(args.tail.head, env))
      case Predicate("log", 2, args, _, _) => Number(evaluate(args.head, env) log evaluate(args.tail.head, env))

      case Predicate("==", 2, args, _, _)   => Bool(evaluate(args.head, env) == evaluate(args.tail.head, env))
      case Predicate("=:=", 2, args, _, _)  => Bool(evaluate(args.head, env) == evaluate(args.tail.head, env))
      case Predicate("=\\=", 2, args, _, _) => Bool(evaluate(args.head, env) != evaluate(args.tail.head, env))
      case Predicate("\\=", 2, args, _, _)  => Bool(evaluate(args.head, env) != evaluate(args.tail.head, env))
      case Predicate("\\==", 2, args, _, _) => Bool(evaluate(args.head, env) != evaluate(args.tail.head, env))
      case Predicate("=>", 2, args, _, _)   => Bool(evaluate(args.head, env) >= evaluate(args.tail.head, env))
      case Predicate(">=", 2, args, _, _)   => Bool(evaluate(args.head, env) >= evaluate(args.tail.head, env))
      case Predicate("<=", 2, args, _, _)   => Bool(evaluate(args.head, env) <= evaluate(args.tail.head, env))
      case Predicate("=<", 2, args, _, _)   => Bool(evaluate(args.head, env) <= evaluate(args.tail.head, env))
      case Predicate(">", 2, args, _, _)    => Bool(evaluate(args.head, env) > evaluate(args.tail.head, env))
      case Predicate("<", 2, args, _, _)    => Bool(evaluate(args.head, env) < evaluate(args.tail.head, env))
      case default                          => default
    }
  }
}
