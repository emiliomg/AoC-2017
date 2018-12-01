package de.emiliomg.adventofcode.y2017.day17

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/17
  */
object Day17 extends App {

  val testData = getDataFromResource("y2017/day17/test.txt")

  assert(firstStar(testData) == 638)
  assert(secondStar(testData, 2017) == firstStar(testData, returnFirstPosition = true))

  val data = getDataFromResource("y2017/day17/input.txt")
  println(s"Result first star: ${firstStar(data)}") // 355
  println(s"Result second star: ${secondStar(data)}") // 6154117

  def firstStar(steps: Int, returnFirstPosition: Boolean = false): Int = {
    val lastStep = Iterator.
      iterate(SpinPart1(Vector(0), 0, steps))(_.next()).
      drop(2017).
      next()

    if (returnFirstPosition)
      lastStep.buffer(1)
    else
      lastStep.getResult
  }

  def secondStar(steps: Int, iterations: Int = 50000000): Int = {
    Iterator.
      iterate(SpinPart2(1, 0, None, steps))(_.next()).
      drop(iterations).
      next().
      getResult
  }

  def getDataFromResource(path: String): Int = Source.fromResource(path).getLines().next().toInt
}

trait BaseSpin {
  def next(): BaseSpin
  def getResult: Int
}

case class SpinPart1(buffer: Vector[Int], currentPosition: Int, steps: Int) extends BaseSpin {
  override def next(): SpinPart1 = {
    val insertVal = buffer(currentPosition) + 1
    val newPosition = ((currentPosition + steps) % buffer.length) + 1
    val newBuffer = buffer.patch(newPosition, Vector(insertVal), 0)

    this.copy(
      newBuffer,
      newPosition,
      steps
    )
  }

  override def getResult: Int = buffer(currentPosition + 1)
}

case class SpinPart2(size: Int, position: Int, posOneValue: Option[Int], steps: Int) extends BaseSpin {
  override def next(): SpinPart2 = {
    val insertVal = size
    val insertPos = ((position + steps) % size) + 1

    this.copy(
      size + 1,
      insertPos,
      if (insertPos == 1) Option(insertVal) else posOneValue,
      steps
    )
  }

  override def getResult: Int = posOneValue.get
}

