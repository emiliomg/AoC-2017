package de.emiliomg.adventofcode.y2017.day14

import de.emiliomg.adventofcode.y2017.day10.Day10

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/14
  */
object Day14 extends App {

  type Position = (Int, Int)
  type Region = Set[Position]

  val testData = getDataFromResource("y2017/day14/test.txt")

  assert(firstStar(testData) == 8108)
  assert(secondStar(testData) == 1242)

  val data = getDataFromResource("y2017/day14/input.txt")
  println(s"Result first star: ${firstStar(data)}") // 8074
  println(s"Result second star: ${secondStar(data)}") // 1212

  def firstStar(data: String): Int = {
    createBitMatrix(data).map(_.sum).sum
  }

  def secondStar(data: String): Int = {
    // Transform a List[List[Int]] (multiple lines of bitsequences) into a
    // Set of (lineNumber -> position) tuples of enabled bits
    val enabledBits: Set[Position] = createBitMatrix(data)
      .zipWithIndex
      .groupBy(_._2)
      .map{
        case (lineNumber, listOfBitsWithLineIndex) ⇒ {
          listOfBitsWithLineIndex
            .flatMap(_._1) // remove line index
            .zipWithIndex // add position to each bit
            .filter(_._1 == 1)
            .map{case (bit, index) ⇒ lineNumber → index}
        }
      }
      .flatten
      .toSet

    // copied from https://www.reddit.com/r/adventofcode/comments/7jpelc/2017_day_14_solutions/dr8r19n/
    val allRegions = enabledBits.foldLeft(Set.empty[Region]) {
      case (regions, position: Position) if !regions.exists(_.contains(position)) =>
        regions + fillRegion(position, enabledBits)
      case (regions, _) =>
        regions
    }

    allRegions.size
  }

  def fillRegion(pos: Position, unusedSquares: Set[Position]): Region = {
    val neighbours = Seq(
      pos.copy(_1 = pos._1 - 1),
      pos.copy(_1 = pos._1 + 1),
      pos.copy(_2 = pos._2 - 1),
      pos.copy(_2 = pos._2 + 1)
    ).filter(unusedSquares.contains)

    Set(pos) ++ neighbours.flatMap(fillRegion(_, unusedSquares - pos -- neighbours))
  }

  def createBitMatrix(hashPrefix: String): List[List[Int]] = {
    (0 to 127).
      map(i ⇒ Day10.knotHash(Day10.prepareStringForKnotHash(s"$hashPrefix-$i"))).
      map(stringToBinary).
      toList
  }

  def stringToBinary(str: String): List[Int] = str.
    map(_.asDigit.toBinaryString).
    map(_.reverse.padTo(4, "0").reverse.mkString("")).
    mkString("").
    map(_.asDigit).
    toList

  def getDataFromResource(path: String): String = Source.fromResource(path).getLines().toList.head
}
