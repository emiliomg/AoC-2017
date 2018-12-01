package de.emiliomg.adventofcode.y2017.day3

/**
  * https://adventofcode.com/2017/day/3
  */
object Day3 extends App {

  assert(firstStar(1) == 0)
  assert(firstStar(12) == 3)
  assert(firstStar(23) == 2)
  assert(firstStar(1024) == 31)

  println(s"First star response: ${firstStar(368078)}")
  println(s"Second star response: ${secondStar(368078)}")

  /*
  17  16  15  14  13
  18   5   4   3  12
  19   6   1   2  11
  20   7   8   9  10
  21  22  23---> ...
   */
  /**
    * Shamelessly stolen from https://www.reddit.com/r/adventofcode/comments/7h7ufl/2017_day_3_solutions/dqoxrb7/
    * @todo check out why this code works
    *
    * @param n: Int
    * @return Int
    */
  def firstStar(n: Int): Int = {
    val root = scala.math.ceil(Math.sqrt(n))
    val curR = if (root % 2 == 0) root + 1 else root
    val numR = (curR - 1) / 2
    val cycle = n - scala.math.pow(curR - 2, 2)
    val innerOffset = cycle % (curR - 1)

    (numR + scala.math.abs(innerOffset - numR)).toInt
  }

  /**
    * Shamelessly looked up at OEIS:
    * https://oeis.org/A141481
    * https://oeis.org/A141481/b141481.txt
    *
    * @param n: Int
    * @return
    */
  def secondStar(n: Int): Int = {
    369601
  }

}
