package de.emiliomg.adventofcode.y2017.day2

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/2
  */
object Day2 extends App {

  assert(firstStar(getIntMatrixFromResources("y2017/day2/star1.txt")) == 18)
  assert(secondStar(getIntMatrixFromResources("y2017/day2/star2.txt")) == 9)

  val data = getIntMatrixFromResources("y2017/day2/input.txt")

  val resultFirstStar = firstStar(data)
  val resultSecondStar = secondStar(data)

  println(s"Result first Star: $resultFirstStar")
  println(s"Result second Star: $resultSecondStar")

  def firstStar(data: List[List[Int]]): Int = data.
    map(row => row.max - row.min).
    sum

  def secondStar(data: List[List[Int]]): Int = data.
    map(row => row.combinations(2).toList).
    flatMap(rowCombinations => {
      rowCombinations.
        find(combination => combination.max % combination.min == 0).
        map(combination => combination.max / combination.min)
    }).
    sum

  def getIntMatrixFromResources(path: String): List[List[Int]] = Source.fromResource(path).getLines().map(_.split("\\p{Space}").map(_.toInt).toList).toList
}
