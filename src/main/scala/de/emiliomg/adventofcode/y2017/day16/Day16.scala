package de.emiliomg.adventofcode.y2017.day16

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/16
  * Day 2 solution inspired by https://www.reddit.com/r/adventofcode/comments/7k572l/2017_day_16_solutions/drc26rx/ (but written by myself!)
  */
object Day16 extends App {

  val testMoves = parseMoveStrings(getDataFromResource("y2017/day16/test.txt"))
  assert(firstStar(('a' to 'e').mkString(""), testMoves) == "baedc")

  val moves = parseMoveStrings(getDataFromResource("y2017/day16/input.txt"))
  println(s"Result first star: ${firstStar(('a' to 'p').mkString(""), moves)}") // bijankplfgmeodhc
  println(s"Result second star: ${secondStar(('a' to 'p').mkString(""), moves)}") // bpjahknliomefdgc

  def firstStar(startPosition: String, moves: List[Move]): String = {
    dance(startPosition, moves)
  }

  def secondStar(startPosition: String, moves: List[Move]): String = {
    val danceIterator = Iterator.iterate(startPosition)(dance(_, moves))
    val (periodIterator, runIterator) = danceIterator.duplicate

    val period = periodIterator.indexOf(startPosition, 1)

    runIterator.drop(1000000000 % period).next()
  }

  def dance(startPosition: String, moves: List[Move]) = moves.foldLeft(startPosition)((position, move) ⇒ move.apply(position))

  def parseMoveStrings(moves: List[String]): List[Move] = {
    val spin = "s(\\d+)".r
    val exchange = "x(\\d+)/(\\d+)".r
    val partner = "p(.+)/(.+)".r

    moves.map({
      case spin(pos) ⇒
//        println(s"spin $pos")
        Spin(pos.toInt)
      case exchange(posA, posB) ⇒
//        println(s"Exchange positions: $posA <-> $posB")
        Exchange(posA.toInt, posB.toInt)
      case partner(nameA, nameB) ⇒
//        println(s"Swap partners: $nameA <-> $nameB")
        Partner(nameA.charAt(0), nameB.charAt(0))
      case move ⇒ throw new IllegalArgumentException(s"Cannot parse move '$move'")
    })
  }

  def getDataFromResource(path: String): List[String] = Source.fromResource(path).getLines().toList.head.split(",").toList
}

sealed trait Move {
  def apply(state: String): String
}

case class Spin(pos: Int) extends Move {
  def apply(state: String): String = state.takeRight(pos.toInt) ++ state.dropRight(pos.toInt)
}

case class Exchange(posA: Int, posB: Int) extends Move {
  def apply(state: String): String = state.updated(posA.toInt, state(posB.toInt)).updated(posB.toInt, state(posA.toInt))
}

case class Partner(posA: Char, posB: Char) extends Move {
  def apply(state: String): String = Exchange(state.indexOf(posA), state.indexOf(posB)).apply(state)
}
