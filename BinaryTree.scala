/**
  * Binary Tree
**/

case class Node(left: Option[Node], right: Option[Node], data: String)

object BinaryTree {
  // recursive
  def recursSerializeTreeToList(node: Node, depth: Int): List[List[String]] = {
    leftList = node.left match {
      case Some(n) => recursSerializeTreeToList(n, depth + 1) 
      case None => List()
    }

    rightList = node.right match {
      case Some(n) => recursSerializeTreeToList(n, depth + 1) 
      case None => List()
    }

    List(node.data) ++ leftList.zip(rightList).map { l => l._1 ++ l._2 }
  }

  //iterative
  def iterSerializeTreeToList(node: Node, depth: Int): List[List[String]] = {
  }
}

// vim: set ts=4 sw=4 et:
