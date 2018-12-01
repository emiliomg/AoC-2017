package de.emiliomg.adventofcode.y2017.day7

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers
import scala.io.Source

/**
  * https://adventofcode.com/2017/day/7
  */
object Day7 extends App {

  val testDataStar1 = getDataFromResource("y2017/day7/star1.txt")
  val testDataTrees: List[Tree] = TreeParser.parse(TreeParser.trees, testDataStar1).get

  assert(firstStar(testDataTrees) == "tknk")
  assert(secondStar(testDataTrees) == 60)

//  secondStar(testDataTrees)

  val data = getDataFromResource("y2017/day7/input.txt")
  val trees: List[Tree] = TreeParser.parse(TreeParser.trees, data).get

  println(s"Result first star: ${firstStar(trees)}")
  println(s"Result second star: ${secondStar(trees)}")

  def firstStar(trees: List[Tree]): String = {
    val rootNodes = trees.map(tree => tree.name).toSet
    val subTreeNodes = trees.flatMap(tree => tree.childNames).toSet

    val baseRootNode = rootNodes.filterNot(subTreeNodes)
    assert(baseRootNode.size == 1)

    baseRootNode.head
  }

  def secondStar(trees: List[Tree]): Int = {
    val baseRootNodeName: String = firstStar(trees)
    val baseRootNode: Tree = findTreeByName(baseRootNodeName, trees)

    baseRootNode.getCorrectedWeight(trees)
  }

  def getDataFromResource(path: String) = Source.fromResource(path).getLines().toList.mkString("\n")

  def findTreeByName(name: String, trees: List[Tree]): Tree = trees.find(tree => tree.name == name).getOrElse(throw new Exception(s"Tree $name not found!"))

  case class Tree(name: String, weight: Int, childNames: List[String]) {

    def getCorrectedWeight(trees: List[Tree]): Int = {
      val childTreeHeights = getChildrenTreeWeights(trees)

      if (childNames.map(findTreeByName(_, trees)).exists(!_.isBalanced(trees))) {
        childTreeHeights.find(_._2.size == 1).get._2.head.getCorrectedWeight(trees)
      } else {
        val (deviatorTreeWeight, deviator) = childTreeHeights.find(_._2.size == 1).get
        val restTreeWeight = childTreeHeights.find(_._2.size != 1).get._1
        val offset = deviatorTreeWeight - restTreeWeight

        deviator.head.weight - offset
      }
    }

    def getTreeWeight(trees: List[Tree]): Int = weight + childNames.map(findTreeByName(_, trees).getTreeWeight(trees)).sum

    def getChildrenTreeWeights(trees: List[Tree]): Map[Int, List[Tree]] =
      this.
        childNames.
        map(findTreeByName(_, trees)).
        groupBy(tree => tree.getTreeWeight(trees))

    def isBalanced(trees: List[Tree]): Boolean = {
      getChildrenTreeWeights(trees).keys.size == 1
    }
  }

  object TreeParser extends RegexParsers {
    override val whiteSpace: Regex = "[ \t]+".r

    def name: Parser[String] = "\\p{Alpha}+".r

    def arrow: Parser[Unit] = "->" ^^ (_ => ())

    def comma: Parser[Unit] = "," ^^ (_ => ())

    def nl: Parser[Unit] = "\n" ^^ (_ ⇒ ())

    def weight: Parser[Int] = "(" ~> "\\p{Digit}+".r <~ ")" ^^ (_.toInt)

    def subTreeNames: Parser[List[String]] = name ~ rep(comma ~> name) ^^ {
      case firstSubTree ~ otherSubtrees =>
        firstSubTree :: otherSubtrees
    }

    def subTree: Parser[List[String]] = (arrow ~> subTreeNames).? ^^ {
      case Some(names) => names
      case None => Nil
    }

    def tree: Parser[Tree] = name ~ weight ~ subTree ^^ {
      case n ~ w ~ s => Tree(name = n, weight = w, childNames = s)
    }

    def trees: Parser[List[Tree]] = rep((tree <~ nl.?) ^^ (t => t))
  }
}
