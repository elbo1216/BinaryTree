/**
  * Binary Tree
**/
import scala.collection.mutable.ListBuffer

case class Node(left: Option[Node], right: Option[Node], data: String)

object BinaryTree {
  // recursive - DFS
  def recursSerializeTreeToList(node: Node, depth: Int): List[List[String]] = {
    val leftList = node.left match {
      case Some(n) => recursSerializeTreeToList(n, depth + 1) 
      case None => List()
    }

    val rightList = node.right match {
      case Some(n) => recursSerializeTreeToList(n, depth + 1) 
      case None => List()
    }

    List(List(node.data)) ++ leftList.zip(rightList).map { l => l._1 ++ l._2 }
  }

  //iterative - BFS
  def iterSerializeTreeToList(node: Node, depth: Int): List[List[String]] = {
    val nodeContainer: ListBuffer[Node] = ListBuffer(node)
    val result:ListBuffer[List[String]] = ListBuffer()
    while (!nodeContainer.isEmpty) {
      val tmpContainer: ListBuffer[Node] = ListBuffer()
      result += nodeContainer.map { node => 
          node.left match {
            case Some(n) => tmpContainer += n
            case None =>
          }

          node.right match {
            case Some(n) => tmpContainer += n
            case None =>
          }

          node.data
      }.toList
      nodeContainer.clear
      nodeContainer ++= tmpContainer
    }
    result.toList
  }

  // List of List to node
  def ListToNode(tree: List[List[String]]): List[Node] = {
    tree.foldRight(List(Node(None, None, tree(0)(0)))) { (a,b) => 
      if (b.size == 1) {
        a.map(n => Node(None, None, n))
      } else { 
        var counter = 0
        a.map{n =>
          counter+=2
          Node(Option(b(counter-2)), Option(b(counter-1)), n)
        }
      }
    }
  }
}

val a  = List(List("1"), List("2","3"), List("4","5","6","7"), List("8","9","10","11","12","13","14","15"))
val tree = BinaryTree.ListToNode(a)
println(tree.size)
val recursive = BinaryTree.recursSerializeTreeToList(tree(0), 0)
val iterative = BinaryTree.iterSerializeTreeToList(tree(0), 0)
println(recursive)
println(iterative)


// vim: set ts=4 sw=4 et:
