package de.emiliomg.adventofcode.y2017.day4

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/4
  */
object Day4 extends App {

  val testDataStar1 = getDataFromResource("y2017/day4/star1.txt")
  val testDataStar2 = getDataFromResource("y2017/day4/star2.txt")

  assert(firstStar(testDataStar1) == 2)
  assert(secondStar(testDataStar2) == 3)

  val data = getDataFromResource("y2017/day4/input.txt")
  println(s"Result first star: ${firstStar(data)}")
  println(s"Result second star: ${secondStar(data)}")

  def firstStar(data: List[List[String]]): Int = {
    data.count(row => row.size == row.distinct.size)
  }

  def secondStar(data: List[List[String]]): Int = {
    def rowContainsAnagram(data: List[String]) = data.map(_.sorted).distinct.size != data.size
    data.count(!rowContainsAnagram(_))
  }

  def getDataFromResource(path: String): List[List[String]] = Source.fromResource(path).getLines().toList.map(l => l.split(" ").toList)
}
