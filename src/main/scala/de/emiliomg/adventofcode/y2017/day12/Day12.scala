package de.emiliomg.adventofcode.y2017.day12

import scala.collection.immutable.HashSet
import scala.collection.{mutable, immutable}
import scala.io.Source

/**
  * https://adventofcode.com/2017/day/12
  * @todo Refactor the shit out of this so we can use return values instead of implicitly changing the given ListBuffer *puke
  */
object Day12 extends App {

  val testDataStar1 = getDataFromResource("y2017/day12/star1.txt")

  secondStar(testDataStar1)

  assert(firstStar(testDataStar1) == 6)
  assert(secondStar(testDataStar1) == 2)

  val data = getDataFromResource("y2017/day12/input.txt")
  println(s"Result first star: ${firstStar(data)}") // 239
  println(s"Result second star: ${secondStar(data)}") // 215

  def firstStar(data: List[String]): Int = {
    val edges = getEdgesFromData(data)

    val foundNodes = mutable.ListBuffer[Int]()
    processNodeGroup(0, foundNodes, edges)
    foundNodes.length
  }

  def secondStar(data: List[String]): Int = {
    val edges = getEdgesFromData(data)
    val allNodes: Set[Int] = edges.keys.toSet ++ edges.values.foldLeft(Set[Int]())((acc, ids) ⇒ acc ++ ids)
    val processedNodes: mutable.ListBuffer[Int] = mutable.ListBuffer[Int]()
    var counter = 0

    def findAllGroups(node: Int, processedNodes: mutable.ListBuffer[Int]): Unit = {
      if (processedNodes.contains(node)) {
        return
      }

      counter = counter + 1
      processNodeGroup(node, processedNodes, edges)
      val diff = allNodes -- processedNodes

      diff.foreach(childNode ⇒ findAllGroups(childNode, processedNodes))
    }

    // start with group 0
    findAllGroups(0, processedNodes)

    counter
  }

  def getEdgesFromData(lines: List[String]): Map[Int, Set[Int]] = {
    val nodeRegex = """(\d+) <-> (\d+(?:, \d+)*)""".r

    lines.foldLeft(mutable.HashSet[(Int, Int)]())((set, line) ⇒ {
      line match {
        case nodeRegex(nodeId, neighborIds) ⇒
          val neighborIdList = neighborIds.split(",").map(_.trim)
          set ++= neighborIdList.foldLeft(mutable.HashSet[(Int, Int)]())((tmp, neighborId) ⇒ {
            tmp ++= List((nodeId.toInt, neighborId.toInt), (neighborId.toInt, nodeId.toInt))
          })
        case _ ⇒ set
      }
    }).toSet[(Int, Int)].groupBy(_._1).mapValues(_.map(_._2))
  }

  def processNodeGroup(node: Int, alreadyProcessed: mutable.ListBuffer[Int], edges: Map[Int, Set[Int]]): Unit = {
    if (!alreadyProcessed.contains(node)) {
      alreadyProcessed.append(node)
      edges(node).foreach(childNode ⇒ processNodeGroup(childNode, alreadyProcessed, edges))
    }
  }

  def getDataFromResource(path: String): List[String] = Source.fromResource(path).getLines().toList
}
