package com.joehalliwell.sp

/** *****************************************************************
  *
  * ******************************************************************/
/*
 * Bindings
 * TODO: Occurs check? Merge with State?
 */
sealed trait Env {
  def unify(terms: (Term, Term)) = this
}

case object Fail extends Env

case class Success(binding: Map[Variable, Term] = Map.empty, var probability: Map[Term, BigDecimal] = Map.empty)
    extends Env {

  def bind(orig: Variable, v: Variable, t: Term): Env =
    //println("B: " + v + "=" + t)
    binding.get(v) match {
      case None =>
        Success(binding + (v -> t), collectProbability(t))
      case Some(v2) =>
        v2 match {
          // Disallow circularities
          case v2: Variable             => if (orig.equals(v2)) Fail else bind(orig, v2, t)
          case t2: Term if t.equals(t2) => this
          case default                  => unify((v2, t))
        }
    }

  // Unify two terms, extending our bindings
  // TODO: Add note on case ordering (it's important!)
  override def unify(terms: (Term, Term)): Env =
    //println(s"unify lh:${terms._1}, rh:${terms._2}")
    terms match {
      case (t1: Atom, t2: Atom) if t1 == t2 =>
        probability = collectProbability(t2)
        this
      case (t1, t2: Variable)                         => bind(t2, t2, t1)
      case (t1: Variable, t2)                         => bind(t1, t1, t2)
      case (t1: Predicate, t2: Predicate) if t1 == t2 =>
        // This is neat, but inefficient! Is there a nice way to write it?
        t1.args.zip(t2.args).foldLeft(Success(binding, collectProbability(t2)): Env)((x, y) => x.unify(y))
      case (t1, t2: Bool) if t2.boolVal => this
      case _                            => Fail
    }

  override def toString = {
    for {
      (variable, value) <- binding
      if variable.level == 0
    } yield variable.name + "=" + value.extract(this)
  } mkString "\n"

  def probabilityResult: BigDecimal = probability.values.foldLeft(Number.one)((x, y) => x * y)
  def probabilityTerm: String       = probability.keys.toString

  protected def collectProbability(term: Term): Map[Term, BigDecimal] =
    term match {
      case t2: Atom =>
        t2.probabilityVal(this) match {
          case Some(e) => probability + (t2 -> e)
          case _       => probability
        }
      case t2: Predicate =>
        t2.probabilityVal(this) match {
          case Some(e) => probability + (t2 -> e)
          case _       => probability
        }
      case _ => probability
    }
}

// TODO: Hide this implementation detail -- by returning an com.joehalliwell.sp.Env thunk?

case class Stackframe(goal: Term, env: Success, index: Int)

// Index scrolls through the database, but it also used as flag
// to indicate that built-in procedures have been tried.
// It is assumed they can only complete in one way.
case class State(goals: Seq[Term], stack: Seq[Stackframe], index: Int) {
  def env = if (stack.isEmpty) Success() else stack.head.env

  def pop(): Option[State] =
    if (stack.isEmpty) None
    else
      stack.head.goal match {
        // HACK: Handle disjunctions
        case Predicate(";", 2, args, _, _) => Some(State(args.tail.head +: goals.tail, stack.tail, -1))
        case default                       => Some(State(stack.head.goal +: goals, stack.tail, stack.head.index + 1))
      }

  def push(): Option[State] = push(env, 0)

  def push(env: Env, index: Int): Option[State] = env match {
    case env: Success => Some(State(goals.tail, Stackframe(goals.head, env, index) +: stack, -1))
    case Fail         => None
  }

  def isSolution = (goals.length == 0)

  def mayHaveMore = !env.binding.isEmpty
}
