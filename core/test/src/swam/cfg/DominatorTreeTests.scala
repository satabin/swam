package swam
package cfg

import cats.implicits._

import utest._

object DominatorTreeTests extends TestSuite {

  def block(id: Int): BasicBlock =
    BasicBlock(id, s"B$id", Nil, None)(Nil)

  val idoms = Vector(8, 8, 0, 0, 3, 3, 1, 6, 8)
  val tree = new DominatorTree(idoms)

  val expectedPostorder = List(2, 4, 5, 3, 0, 7, 6, 1, 8)
  val expectedPreorder = List(8, 0, 2, 3, 4, 5, 1, 6, 7)

  def tests: Tests = Tests {

    test("postorder") {
      val postorder = tree.postorder(List.empty[Int])((acc, _, node) => node :: acc).reverse
      assert(postorder == expectedPostorder)
    }

    test("postorderM") {
      val postorder = tree.postorderM(List.empty[Int])((acc, _, node) => (node :: acc).some).map(_.reverse)
      assert(postorder == Some(expectedPostorder))
    }

    test("preorder") {
      val preorder = tree.preorder(List.empty[Int])((acc, _, node) => node :: acc).reverse
      assert(preorder == expectedPreorder)
    }

    test("preorderM") {
      val preorder = tree.preorderM(List.empty[Int])((acc, _, node) => (node :: acc).some).map(_.reverse)
      assert(preorder == Some(expectedPreorder))
    }

  }

}
