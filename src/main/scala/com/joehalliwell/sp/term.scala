package com.joehalliwell.sp

import scala.collection.immutable._
import scala.math.BigDecimal
import builtin.Arithmetic

/**
  * AST for Prolog
  */
trait Term extends Ordered[Term] {
  def renameVars(level: Int) = this
  def boolVal: Boolean
  val numberVal: BigDecimal

  override def equals(other: Any) =
    other match {
      case that: Term => boolVal == that.boolVal
      case _          => false
    }
  override def compare(other: Term): Int = boolVal.compare(other.boolVal)

  def +(other: Any) =
    other match {
      case that: Term => numberVal + that.numberVal
      case _          => Number.zero
    }
  def -(other: Any) =
    other match {
      case that: Term => numberVal - that.numberVal
      case _          => Number.zero
    }
  def *(other: Any) =
    other match {
      case that: Term => numberVal * that.numberVal
      case _          => Number.zero
    }
  def /(other: Any) =
    other match {
      case that: Term => if (that.numberVal != 0.0) numberVal / that.numberVal else Number.zero
      case _          => Number.zero
    }
  def %(other: Any) =
    other match {
      case that: Term => if (that.numberVal != 0.0) numberVal % that.numberVal else Number.zero
      case _          => Number.zero
    }
  def ^(other: Any) =
    other match {
      case that: Term => BigDecimal.valueOf(math.pow(numberVal.toDouble, that.numberVal.toDouble))
      case _          => Number.zero
    }
  def log(other: Any) =
    other match {
      case that: Term =>
        BigDecimal.valueOf(Math.log(numberVal.toDouble)) / BigDecimal.valueOf(Math.log(that.numberVal.toDouble))
      case _ => Number.zero
    }

  def extract(env: Env): Term = extract(env, this)

  protected def extract(env: Env, t: Term): Term = t match {
    case Predicate(name, arity, args, probability, evidence) => {
      probability match {
        case Some(probTerm) =>
          Predicate(
            "::",
            2,
            Seq(probTerm) :+
            Predicate(name, arity, args.map(x => extract(env, x)), probability, evidence)
          )
        case _ =>
          Predicate(name, arity, args.map(x => extract(env, x)), probability, evidence)
      }
    }
    case v: Variable =>
      env match {
        case s: Success =>
          s.binding.get(v) match {
            case None    => v
            case Some(t) => extract(env, t)
          }
        case _ => v
      }
    case v: Atom =>
      v.probability match {
        case Some(probTerm) => Predicate("::", 2, Seq(probTerm) :+ v)
        case _              => v
      }
    case t => t
  }
}

case class Number(value: BigDecimal) extends Term {
  override def toString               = value.toString
  override def renameVars(level: Int) = Number(value)
  def boolVal: Boolean                = value != Number.zero
  val numberVal: BigDecimal           = value

  override def equals(other: Any) =
    other match {
      case that: Number => value == that.value
      case _            => super.equals(other)
    }
  override def compare(other: Term): Int =
    other match {
      case that: Number => value.compareTo(that.value)
      case _            => super.compare(other)
    }
}
object Number {
  val zero = BigDecimal.valueOf(0)
  val one  = BigDecimal.valueOf(1)
}

case class Bool(value: Boolean) extends Term {
  override def toString               = value.toString
  override def renameVars(level: Int) = Bool(value)
  def boolVal: Boolean                = value
  val numberVal: BigDecimal           = { if (value) Number.one else Number.zero }

  override def equals(other: Any) =
    other match {
      case that: Bool => value == that.value
      case _          => super.equals(other)
    }
  override def compare(other: Term): Int =
    other match {
      case that: Bool => value.compareTo(that.value)
      case _          => super.compare(other)
    }
}

case class Str(value: String) extends Term {
  override def toString               = value
  override def renameVars(level: Int) = Str(value)
  def boolVal: Boolean                = !value.isEmpty
  val numberVal: BigDecimal           = Number.one

  override def equals(other: Any) =
    other match {
      case that: Str => value == that.value
      case _         => super.equals(other)
    }
  override def compare(other: Term): Int =
    other match {
      case that: Str => value.compareTo(that.value)
      case _         => super.compare(other)
    }
}

case class Atom(
  value: String,
  var probability: Option[Term] = None,
  var evidence: Option[Boolean] = None
) extends Term {
  override def toString = {
    val e = evidence.map(e => s"#$e").getOrElse("")
    s"${value}${e}"
  }
  override def renameVars(level: Int) = Atom(value, probability, evidence)
  def boolVal: Boolean                = evidence.getOrElse(true)
  val numberVal: BigDecimal           = Number.one

  override def equals(other: Any) =
    other match {
      case that: Atom => value == that.value && evidence.getOrElse(true) && that.evidence.getOrElse(true)
      case _          => false
    }
  override def compare(other: Term): Int =
    other match {
      case that: Atom => value.compareTo(that.value)
      case _          => super.compare(other)
    }

  def probabilityVal(env: Env): Option[BigDecimal] = probability.flatMap { p =>
    Arithmetic.evaluate(p, env) match {
      case n: Number => Some(n.value)
      case _         => None
    }
  }
}

case class Predicate(
  name: String,
  arity: Int,
  args: Seq[Term],
  var probability: Option[Term] = None,
  var evidence: Option[Boolean] = None
) extends Term {
  override def toString = {
    val e = evidence.map(e => s"#$e").getOrElse("")
    name match {
      case Prolog.ListPredicate => s"[${unpackList().mkString(", ")}]${e}"
      case default              => s"${name}(${args.mkString(", ")})${e}"
    }
  }
  override def renameVars(level: Int) = Predicate(name, arity, args.map(_.renameVars(level)), probability, evidence)
  def boolVal: Boolean                = evidence.getOrElse(true)
  val numberVal: BigDecimal           = Number.one

  def unpackList(): Seq[Term] = args.tail.head match {
    case Prolog.EmptyList => Seq(args.head)
    case tail: Predicate  => Seq(args.head) ++ tail.unpackList()
    case default          => Seq(args.head, Atom("|"), args.tail.head)
  }

  override def equals(other: Any) =
    other match {
      case that: Predicate =>
        name == that.name && arity == that.arity && evidence.getOrElse(true) && that.evidence.getOrElse(true)
      case _ => false
    }
  override def compare(other: Term): Int =
    other match {
      case that: Predicate =>
        name.compareTo(that.name) match {
          case 0 => arity.compareTo(that.arity)
          case v => v
        }

      case _ => super.compare(other)
    }

  def deepEquals(other: Any) = {
    def eq(lh: Seq[Term], rh: Seq[Term]): Boolean =
      if (lh.size <= 0 && rh.size <= 0) true
      else {
        (lh.head, rh.head) match {
          case (t1: Atom, t2: Atom)           => t1 == t2
          case (t1: Predicate, t2: Predicate) => t1 == t2 && eq(t1.args, t2.args)
          case _                              => false
        }
      }

    other match {
      case that: Predicate => this == that && eq(args, that.args)
      case _               => false
    }
  }

  def probabilityVal(env: Env): Option[BigDecimal] = probability.flatMap { p =>
    Arithmetic.evaluate(p, env) match {
      case n: Number => Some(n.value)
      case _         => None
    }
  }
}

case class Variable(name: String, level: Int = 0) extends Term {
  override def toString               = name + "_" + level
  override def renameVars(level: Int) = Variable(name, level)
  def boolVal: Boolean                = true
  val numberVal: BigDecimal           = Number.one

  override def equals(other: Any) =
    other match {
      case that: Variable => name == that.name && level == that.level
      case _              => false
    }
  override def compare(other: Term): Int =
    other match {
      case that: Variable =>
        name.compareTo(that.name) match {
          case 0 => level.compareTo(that.level)
          case v => v
        }
      case _ => super.compare(other)
    }
}
