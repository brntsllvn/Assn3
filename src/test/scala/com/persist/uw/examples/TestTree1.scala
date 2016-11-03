package com.persist.uw.examples

import org.specs2._

class TestTree1 extends mutable.Specification {

  val t1 = If1(Add1(Int1(1), Int1(-1)), Name1("a"), Name1("b"))
  val t2 = Add1(Multiply1(Int1(2), Add1(Int1(3), Name1("c"))), Int1(4))

  "Int1 is a tree's head element" >> {
    val myTree = Int1(5)
    myTree.i mustEqual 5
    myTree.depth mustEqual 1
    myTree.size mustEqual 1
  }

  "Name1 is a tree's name" >> {
    val myTree = Name1("Seneca")
    myTree.name mustEqual "Seneca"
  }

  "Add1 adds two trees together, but I don't know which comes out on top" >> {
    val leftTree = Int1(1)
    val rightTree = Int1(-1)
    val addTree = Add1(leftTree, rightTree)
    addTree.size mustEqual 3
    addTree.depth mustEqual 2
  }


  "size" >> {
    (t1.size shouldEqual 6) and
      (t2.size shouldEqual 7)
  }

  "depth" >> {
    (t1.depth shouldEqual 3) and
      (t2.depth shouldEqual 4)
  }

}
