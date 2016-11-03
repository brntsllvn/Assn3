package com.persist.uw.examples

import org.specs2._

class TestSimplify extends mutable.Specification {

  import Simplify.simplify

  "add3 adds two trees representing integers together" >> {
    val tree1 = Int3(4) // 4
    val tree2 = Int3(2) // 2
    val addTree = Add3(tree1, tree2)
    val s = simplify(addTree)
    s mustEqual Int3(6)
  }

  "add3 adds a nested trees" >> {
    val tree1 = Int3(4) // 4
    val tree2 = Int3(2) // 2
    val addTree = Add3(Add3(Add3(tree1,tree1), Add3(tree1,tree1)), Add3(tree2, tree2)) // 4+4+4+4+2+2=20
    val s = simplify(addTree)
    s mustEqual Int3(20)
  }

  "add0" >> {
    val t1 = Add3(Name3("b"), Int3(0)) // b + 0 = b
    val t2 = Add3(Int3(0), Name3("b")) // 0 + b = b
    (simplify(t1) shouldEqual Name3("b")) and
      (simplify(t2) shouldEqual Name3("b"))
  }

  "eval" >> {
    val t1 = Add3(Int3(4), Int3(2)) // 4 + 2 = 6
    val t2 = Subtract3(Int3(4), Int3(2)) // 4 - 2 = 2
    val t3 = Multiply3(Int3(4), Int3(2)) // 4 * 2 = 8
    val t4 = Divide3(Int3(4), Int3(2)) // 4 / 2 = 2
    val t5 = Add3(Add3(t1, t3), Multiply3(t2, t4)) // (6+8)+(2*2) = 18
    simplify(t5) shouldEqual Int3(18)
  }

  "subtractEqual" >> {
    val t1 = Divide3(Name3("a"), Name3("b")) // a/b
    val t2 = Divide3(Name3("a"), Add3(Name3("b"), Int3(0))) // a/(b+0) = a/b
    simplify(Subtract3(t1, t2)) shouldEqual Int3(0) // a/b - a/b = 0
  }

  "divideEqual" >> {
    val t1 = Divide3(Name3("a"), Name3("b")) // a/b
    val t2 = Divide3(Name3("a"), Add3(Name3("b"), Int3(0))) // a/(b+0)
    simplify(Divide3(t1, t2)) shouldEqual Int3(1) // a/b * b/a = 1

  }

  "testIf" >> {
    val t1 = If3(Int3(0), Name3("a"), Name3("b"))
    val t2 = If3(Int3(1), Name3("a"), Name3("b"))
    (simplify(t1) shouldEqual Name3("b")) and
      (simplify(t2) shouldEqual Name3("a"))
  }

  "testDist+" >> {
    val t1 = Add3(Multiply3(Name3("a"), Int3(5)), Multiply3(Name3("a"), Int3(3)))
    val t2 = Multiply3(Name3("a"), Int3(8))
    simplify(t1) shouldEqual t2
  }

  "testDist-" >> {
    val t1 = Subtract3(Multiply3(Name3("a"), Int3(5)), Multiply3(Name3("a"), Int3(3)))
    val t2 = Multiply3(Name3("a"), Int3(2))
    simplify(t1) shouldEqual t2
  }



  "multiply1" >> {
    val t1 = Multiply3(Name3("b"), Int3(1))
    val t2 = Multiply3(Int3(1), Name3("b"))
    (simplify(t1) shouldEqual Name3("b")) and
      (simplify(t2) shouldEqual Name3("b"))
  }

  "multiply0" >> {
    val t1 = Multiply3(Name3("b"), Int3(0))
    val t2 = Multiply3(Int3(0), Name3("b"))
    (simplify(t1) shouldEqual Int3(0)) and
      (simplify(t2) shouldEqual Int3(0))
  }

}
