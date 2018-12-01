package de.emiliomg.adventofcode.y2017.day10

import scala.io.Source
import CircularVector._

import scala.annotation.tailrec

/**
  * https://adventofcode.com/2017/day/10
  */
object Day10 extends App {

  def firstStarProcess(str: String): Vector[Int] = str.split(",").map(_.toInt).toVector
  def prepareStringForKnotHash(str: String): Vector[Int] = str.map(_.toInt).toVector ++ Vector(17, 31, 73, 47, 23)

  val testDataStar1 = getDataFromResource("y2017/day10/star1.txt")(firstStarProcess)

  assert(firstStar((0 to 4).toVector)(testDataStar1) == 12)
  assert(knotHash(prepareStringForKnotHash("")) == "a2582a3a0e66e6e86e3812dcb672a272")
  assert(knotHash(prepareStringForKnotHash("AoC 2017")) == "33efeb34ea91902bb2f59c9920caa6cd")
  assert(knotHash(prepareStringForKnotHash("1,2,3")) == "3efbe78a8d82f29979031a4aa0b16a9d")
  assert(knotHash(prepareStringForKnotHash("1,2,4")) == "63960835bcdc130f0b66d7ff4f6a5a8e")

  val inputDataStringPartial = getDataFromResource("y2017/day10/input.txt")_
  println(s"Result first star: ${firstStar((0 to 255).toVector)(inputDataStringPartial(firstStarProcess))}") // 37230
  println(s"Result second star: ${knotHash(inputDataStringPartial(prepareStringForKnotHash))}") // 70b856a24d586194331398c7fcfa0aaf

  def firstStar(data: Vector[Int])(input: Vector[Int]): Int = {
    val resultState = circleKnot(input, State(data, Vector(), 0))
    resultState.getDataWithoutShifts.take(2).product
  }

  def knotHash(input: Vector[Int]): String = {
    val finalState = (1 to 64).foldLeft(State((0 to 255).toVector, Vector(), 0))((state, _) ⇒ circleKnot(input, state))

    val sparseHash: Vector[Int] = finalState.getDataWithoutShifts //try with just .data for shit and giggles
    val denseHash: Vector[Int] = sparseHash.grouped(16).map(group ⇒ group.reduce(_ ^ _)).toVector
    denseHash.
      map(_.toHexString.reverse.padTo(2, "0").reverse.mkString("")). // Convert integer to two-char hex representation
      mkString("")
  }

  def circleKnot(input: Vector[Int], state: State): State = {
    input.foldLeft(state)((state, length) ⇒ {
      val (workSlice, tail) = state.data.splitAt(length)
      val offset = length + state.skipSize
      val newData = workSlice.reverse ++ tail
      val newDataShifted = newData.shift(offset)

      state.copy(
        data = newDataShifted,
        storedOffsets = state.storedOffsets :+ offset,
        skipSize = state.skipSize + 1
      )
    })
  }

  def getDataFromResource(path: String)(converter: String ⇒ Vector[Int]): Vector[Int] = converter(Source.fromResource(path).getLines().toList.head)
}

case class State(data: Vector[Int], storedOffsets: Vector[Int], skipSize: Int) {
  def getDataWithoutShifts: Vector[Int] = {
    val restoreOffset = data.length - (storedOffsets.sum % data.length)
    data.shift(restoreOffset)
  }
}

class CircularVector(underlying: Vector[Int]) {
  def shift(start: Int): Vector[Int] = {
    val (before, after) = underlying.splitAt(start % underlying.length)
    after ++ before
  }
}

object CircularVector {
  implicit def toCircularVector(that: Vector[Int]): CircularVector = new CircularVector(that)
}
