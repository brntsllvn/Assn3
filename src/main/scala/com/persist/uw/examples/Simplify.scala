package com.persist.uw.examples

object Simplify {

  // use pattern matching
  // no vars or mutable data
  // pass all tests

  def simplify(t: Tree3): Tree3 = {
    t match {
      case Int3(i) => Int3(i)
      case Add3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i + j)
        case (_, _) => simplify(Add3(simplify(left), simplify(right)))
      }
      case Subtract3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i - j)
        case (_, _) => simplify(Subtract3(simplify(left), simplify(right)))
      }
      case Multiply3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i * j)
        case (_, _) => simplify(Multiply3(simplify(left), simplify(right)))
      }
      case Divide3(left, right) => (left, right) match {
        case (Int3(i), Int3(j)) => Int3(i / j)
        case (_, _) => simplify(Divide3(simplify(left), simplify(right)))
      }
    }
  }
}

