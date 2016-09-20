package utils

import org.scalatest.{ FlatSpec, Matchers }

class TreeSumSpec extends FlatSpec with Matchers {

  "A TreeSum" should "insert unlinked amounts" in {
    val tree = TreeSum[Double]()
    tree.insert(1L, 1.0)
    tree.insert(2L, 2.0)
    tree.insert(3L, 3.0)

    tree.get(1L) shouldBe Some(1.0)
    tree.get(2L) shouldBe Some(2.0)
    tree.get(3L) shouldBe Some(3.0)
  }

  it should "return None when fed with invalid id" in {
    val tree = TreeSum[Double]()
    tree.get(1L) shouldBe None
    tree.get(2L) shouldBe None
    tree.get(3L) shouldBe None
  }

  it should "update sums properly when inserting linked amounts" in {
    val tree = TreeSum[Double]()
    tree.insert(1L, 1.0)
    tree.get(1L) shouldBe Some(1.0)

    tree.insert(2L, 1.0, Some(1L))      //          1 (1.0)
    tree.get(1L) shouldBe Some(2.0)     //           \
    tree.get(2L) shouldBe Some(1.0)     //            2 (1.0)

    tree.insert(3L, 2.0, Some(1L))      //          1 (1.0)
    tree.get(1L) shouldBe Some(4.0)     //         / \
    tree.get(2L) shouldBe Some(1.0)     //  (2.0) 3   2 (1.0)
    tree.get(3L) shouldBe Some(2.0)

    tree.insert(4L, 5.0, Some(2L))      //          1 (1.0)
    tree.get(1L) shouldBe Some(9.0)     //         / \
    tree.get(2L) shouldBe Some(6.0)     //  (2.0) 3   2 (1.0)
    tree.get(3L) shouldBe Some(2.0)     //             \
    tree.get(4L) shouldBe Some(5.0)     //              4 (5.0)
  }

  it should "prevent inserting a value with an already existing key" in {
    val tree = TreeSum[Double]()
    tree.insert(1L, 1.0) shouldBe a[Some[_]]
    tree.insert(1L, 1.0) shouldBe None
  }
}
