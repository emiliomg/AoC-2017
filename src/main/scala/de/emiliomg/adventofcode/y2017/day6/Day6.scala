package de.emiliomg.adventofcode.y2017.day6

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

/**
  * https://adventofcode.com/2017/day/6
  */
object Day6 extends App {

  val testDataStar1 = getDataFromResource("y2017/day6/star1.txt")

  assert(firstAndSecondStar(testDataStar1) == 5)
  assert(firstAndSecondStar(testDataStar1, secondStar = true) == 4)

  val data = getDataFromResource("y2017/day6/input.txt")
  println(s"Result first star: ${firstAndSecondStar(data)}")
  println(s"Result second star: ${firstAndSecondStar(data, secondStar = true)}")

  def firstAndSecondStar(originalData: Vector[Int], secondStar: Boolean = false): Int = {
    val seen: mutable.Set[String] = mutable.HashSet()
    val seenOrder: mutable.ListBuffer[String] = mutable.ListBuffer()
    val data: mutable.ArrayBuffer[Int] = originalData.to[mutable.ArrayBuffer]

    def process(step: Int): Int = {

      @tailrec
      def distribute(pos: Int, cnt: Int): Unit = cnt match {
        case c if c > 0 =>
          data(pos) = data(pos) + 1
          distribute(getIndex(pos + 1), cnt - 1)
        case _ => ;
      }

      def getIndex(pos: Int): Int = if (data.length > pos) pos else 0

      val maxVal = data.max
      val maxPos = data.indexOf(maxVal)

      data(maxPos) = 0
      distribute(getIndex(maxPos + 1), maxVal)

      val hash = data.mkString("")
      if (seen.contains(hash)) {
        if (secondStar) {
          seenOrder.length - seenOrder.indexOf(hash)
        } else
          step
      } else {
        seen.add(hash)
        seenOrder.append(hash)
        process(step + 1)
      }

    }

    process(1)
  }

  def getDataFromResource(path: String): Vector[Int] = Source.fromResource(path).getLines().
    map(_.split("\\s+").
    toVector.
    map(_.toInt)).
    toList.
    head
}
