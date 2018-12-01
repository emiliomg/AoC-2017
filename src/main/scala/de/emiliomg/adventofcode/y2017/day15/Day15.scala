package de.emiliomg.adventofcode.y2017.day15

/**
  * https://adventofcode.com/2017/day/15
  */
object Day15 extends App {

  val genA = Generator(factor = 16807)_
  val genB = Generator(factor = 48271)_

  assert(countMatches(5)(genA(65)(1), genB(8921)(1)) == 1)
  assert(countMatches(40000000)(genA(65)(1), genB(8921)(1)) == 588)
  assert(countMatches(5)(genA(65)(4), genB(8921)(8)) == 0)
  assert(countMatches(5000000)(genA(65)(4), genB(8921)(8)) == 309)

  println(s"Result first star: ${countMatches(40000000)(genA(591)(1), genB(393)(1))}") // 619
  println(s"Result second star: ${countMatches(5000000)(genA(591)(4), genB(393)(8))}") // < 2428 | 2462

  def countMatches(iterations: Int)(genA: Generator, genB: Generator): Int = {
    genA.getNextNumber
      .zip(genB.getNextNumber)
      .take(iterations)
      .count{ case(a, b) ⇒ (a & 0xFFFF) == (b & 0xFFFF) }
  }
}

case class Generator(factor: Int)(seed: Long)(multiFilter: Int) {
  val divisor: Int = 2147483647

  def getNextNumber: Iterator[Long] =
    Iterator.iterate(seed)(prevNum ⇒ (prevNum * factor) % divisor)
      .drop(1)
      .filter(_ % multiFilter == 0)
}
