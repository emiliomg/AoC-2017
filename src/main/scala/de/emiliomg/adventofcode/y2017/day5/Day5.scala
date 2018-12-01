package de.emiliomg.adventofcode.y2017.day5

import scala.annotation.tailrec
import scala.io.Source

/**
  * https://adventofcode.com/2017/day/5
  */
object Day5 extends App {

  val testData = getDataFromResource("y2017/day5/test1.txt")

  assert(firstStar(testData) == 5)
  assert(secondStar(testData) == 10)

  val data = getDataFromResource("y2017/day5/input.txt")
  println(s"Result first star: ${firstStar(data)}")
  println(s"Result second star: ${secondStar(data)}")

  def firstStar(stack: Vector[Int]): Int = processStack(stack, _ => 1)

  def secondStar(stack: Vector[Int]): Int = processStack(stack, x => if(x >= 3) -1 else 1)

  def processStack(stack: Vector[Int], incFunc: Int => Int): Int = {
    @tailrec def step(stack: Vector[Int], pos: Int, curStep: Int): Int = {
      stack.lift(pos) match {
        case Some(offset) =>
          step(stack.updated(pos, offset + incFunc(offset)), pos + offset, curStep + 1)
        case _ => curStep
      }
    }

    step(stack, 0, 0)
  }

  def getDataFromResource(path: String): Vector[Int] = Source.fromResource(path).getLines().map(_.toInt).toVector
}
