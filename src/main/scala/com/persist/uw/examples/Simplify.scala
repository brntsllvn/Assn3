package com.persist.uw.examples

object Simplify {

  // use pattern matching
  // no vars or mutable data
  // pass all tests

  def simplify(t: Tree3): Tree3 = {
    t match {
      case Int3(i) => Int3(i)
      case Name3(a) => Name3(a)
      case Add3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i + j)
        case (Int3(0), right) => right
        case (left, Int3(0)) => left
        case (Multiply3(Name3(a1), Int3(a2)), Multiply3(Name3(b1), Int3(b2))) =>
          if (a1 == b1) Multiply3(Name3(a1), Int3(a2 + b2))
          else Add3(left, right)
        case (_, _) => simplify(Add3(simplify(left), simplify(right)))
      }
      case Subtract3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i - j)
        case (Name3(b), Int3(0)) => Name3(b)
        case (Multiply3(Name3(a1), Int3(a2)), Multiply3(Name3(b1), Int3(b2))) =>
          if (a1 == b1) Multiply3(Name3(a1), Int3(a2 - b2))
          else Subtract3(left, right)
        case (_, _) =>
          if (left == right) Int3(0)
          else simplify(Subtract3(simplify(left), simplify(right)))
      }
      case Multiply3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i * j)
        case (Int3(1), right) => right
        case (left, Int3(1)) => left
        case (Int3(0), _) => Int3(0)
        case (_, Int3(0)) => Int3(0)
        case (_, _) => simplify(Multiply3(simplify(left), simplify(right)))
      }
      case Divide3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i / j)
        case (Name3(a), Name3(b)) => Divide3(Name3(a), Name3(b))
        case (_, _) =>
          if (left == right) Int3(1)
          else simplify(Divide3(simplify(left), simplify(right)))
      }
      case If3(condition, left, right) => condition match {
        case Int3(0) => right
        case Int3(1) => left
      }
    }
  }
}

