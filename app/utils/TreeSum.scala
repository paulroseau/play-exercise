package utils

import annotation.tailrec
import collection.mutable.{ Map => MutableMap }

// Basic data structure to maintain the sums of previously inserted keys as we
// insert a new key. This gives us O(1) time complexity on consequent reads.
// This data structure is valuable in the case where retreive values more often
// than we insert them.
// Otherwise the mechanic in `insert` should be done in `get`, without mutating
// the nodes (just computing the sum). (Nodes value should be immutable in this
// case).
class TreeSum[T : Summable] {

  import Summable._

  private case class Node[T](var value: T, parent: Option[Node[T]]) {
    @tailrec
    final def propagateUp(f: Node[T] => Unit): Unit =
      parent match {
        case None => ()
        case Some(p) =>
          f(p)
          p.propagateUp(f)
      }
  }

  private val nodeMap: MutableMap[Long, Node[T]] = MutableMap[Long, Node[T]]()


  /* 
   * - returns Some(()) if insertion succeeded
   * - returns None if it didn't (because `key` has already been inserted)
   * - If `parentId` does not refer to any already inserted key, it is silently
   *   ignored, and node gets inserted anyway, without a parent.
   * - Time complexity is O(n) time complexity where n is the number of element
   *   in the tree (actually it is O(h) time complexity where h is the height of
   *   the tree, but h == n in the worst case) 
   * */
  def insert(key: Long, t: T, parentId: Option[Long] = None): Option[Unit] =
    nodeMap.get(key) match {
      case Some(_) => None
      case None => Some {
        val parentNode = parentId.flatMap(k => nodeMap.get(k))
        val newNode = Node(t, parentNode)
        nodeMap += (key -> newNode)
        newNode.propagateUp { n =>
          n.value = n.value plus newNode.value
        }
      }
    }

  // O(1) time complexity
  def get(key: Long): Option[T] = nodeMap.get(key).map(_.value)
}

object TreeSum {
  def apply[T : Summable]() = new TreeSum[T]
}
