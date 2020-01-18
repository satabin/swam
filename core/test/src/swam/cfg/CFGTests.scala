package swam.cfg

import utest._

object CFGTests extends TestSuite {

  def tests: Tests = Tests {
    test("diamond") {
      val builder = CFGBuilder.make()
      val (t1, e1) = builder.entry.conditionallyJumpToNew()
      val n1 = t1.jumpToNew("next")
      e1.jumpTo(n1)
      val (t2, e2) = n1.conditionallyJumpToNew()
      val n2 = t2.jumpToNew("next")
      e2.jumpTo(n2)
      n2.addReturn()

      val cfg = builder.result()
      val idoms = cfg.idoms

      assert(idoms == Vector(1, 4, 4, 4, 7, 7, 7, 7))
    }

    test("break-if") {
      val builder = CFGBuilder.make()
      val b = builder.entry.jumpToNew("some block")
      val (t, e) = b.conditionallyJumpToNew()
      e.jumpTo(t)
      t.addReturn()

      val cfg = builder.result()
      val idoms = cfg.idoms

      assert(idoms == Vector(1, 3, 3, 4, 4))
    }

    test("irreductible") {
      // this is the graph in figure 4 of A Simple, Fast Dominance Algorithm
      val builder = CFGBuilder.make()
      val (n5, n4) = builder.entry.conditionallyJumpToNew()
      n5.addReturn()
      val (n2, n3) = n4.conditionallyJumpToNew()
      n2.conditionallyJumpTo(builder.end, n3)
      n3.jumpTo(n2)
      builder.end.jumpTo(n2)

      val cfg = builder.result()
      val idoms = cfg.idoms

      assert(idoms == Vector(5, 5, 5, 5, 5, 5))
    }
  }

}
