package de.emiliomg.adventofcode.y2017.day1

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/1
  */
object Day1 extends App {

  assert(inverseCaptcha("1122".toList, 1) == 3)
  assert(inverseCaptcha("1111".toList, 1) == 4)
  assert(inverseCaptcha("1234".toList, 1) == 0)
  assert(inverseCaptcha("91212129".toList, 1) == 9)

  assert(inverseCaptcha("1212".toList, 2) == 6)
  assert(inverseCaptcha("1221".toList, 2) == 0)
  assert(inverseCaptcha("123425".toList, 3) == 4)
  assert(inverseCaptcha("123123".toList, 3) == 12)
  assert(inverseCaptcha("12131415".toList, 4) == 4)

  val data = Source.fromResource("y2017/day1/input.txt").getLines().toList.head.toList

  println(s"Solution first star: ${inverseCaptcha(data, 1)}")
  println(s"Solution second star: ${inverseCaptcha(data, data.length / 2)}")

  def inverseCaptcha(data: List[Char], offset: Int): Int = {
    data.
      indices.
      filter(idx => data(idx) == data((idx + offset) % data.length)).
      map(data(_).asDigit)
      .sum
  }
}
