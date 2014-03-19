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

  def randomBalancedBinaryTreeGenerator(depth: Int): List[List[String]] = {
    Seq.range(0, depth + 1, 1).map( elem => Seq.fill(scala.math.pow(2,elem).toInt)(scala.util.Random.alphanumeric.take(5).mkString).toList).toList
  }

  def printPretty(tree: List[List[String]]) {

    def spacer(size: Int): String = Seq.fill(size)(" ").mkString

    // Find the tree's max width/height
    val numSpacePerNode = 7
    val width = tree.last.map( node => node.size).sum + numSpacePerNode*(tree.last.size)
    val height = tree.size*2
    val resList:ListBuffer[String] = ListBuffer()

    tree.reverse.foreach { level =>
      var treeLeg = ""
      var leg = "/"
      if (resList.size == 0) {
        val nodeStr = level.foldLeft("") {(b,a) => 
          treeLeg = treeLeg + spacer(a.size/2) + leg + spacer(a.size-a.size/2 - 1) + spacer(numSpacePerNode)
          if (leg == "/") { leg = "\\"} else { leg = "/" }
          b ++ a ++ spacer(numSpacePerNode)
        }
        resList.append(nodeStr)
        resList.append(treeLeg)
      } else {
        val prevTreeLeg = resList.last
        var legCounter = 0
        var nodeStr = ""
        level.foreach { node =>
          val leftLeg = prevTreeLeg.indexOf("/", legCounter)
          val rightLeg = prevTreeLeg.indexOf("\\", legCounter)
          val position = if (rightLeg-leftLeg > node.size) {
            leftLeg + 1 + (rightLeg-leftLeg-node.size)/2
          } else {
            leftLeg + 1 - (node.size-rightLeg-leftLeg/2)
          }
          nodeStr = nodeStr + spacer(position - nodeStr.size) + node
          treeLeg = treeLeg + spacer(position - treeLeg.size  + node.size/2) + leg
          if (leg == "/") { leg = "\\"} else { leg = "/" }
          legCounter = rightLeg + 1
        }
        resList.append(nodeStr)
        if (level.size > 1)
          resList.append(treeLeg)
      }
    }
    resList.reverse.foreach { item => println(item) }

  }
}

//TEST
val a  = BinaryTree.randomBalancedBinaryTreeGenerator(2)
BinaryTree.printPretty(a)
val tree = BinaryTree.ListToNode(a)
val recursive = BinaryTree.recursSerializeTreeToList(tree(0), 0)
val iterative = BinaryTree.iterSerializeTreeToList(tree(0), 0)
//println(recursive)
//println(iterative)


// vim: set ts=4 sw=4 et:
