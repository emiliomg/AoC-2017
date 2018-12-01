package de.emiliomg.adventofcode.y2017.day9

import scala.io.Source

object Day9 extends App {

  assert(parseDataStream("{}").groupCount == 1)
  assert(parseDataStream("{{{}}}").groupCount == 3)
  assert(parseDataStream("{{},{}}").groupCount == 3)
  assert(parseDataStream("{{{},{},{{}}}}").groupCount == 6)
  assert(parseDataStream("{<{},{},{{}}>}").groupCount == 1)
  assert(parseDataStream("{<a>,<a>,<a>,<a>}").groupCount == 1)
  assert(parseDataStream("{{<a>},{<a>},{<a>},{<a>}}").groupCount == 5)
  assert(parseDataStream("{{<!>},{<!>},{<!>},{<a>}}").groupCount == 2)

  assert(parseDataStream("{}").score == 1)
  assert(parseDataStream("{{{}}}").score == 6)
  assert(parseDataStream("{{},{}}").score == 5)
  assert(parseDataStream("{{{},{},{{}}}}").score == 16)
  assert(parseDataStream("{<a>,<a>,<a>,<a>}").score == 1)
  assert(parseDataStream("{{<ab>},{<ab>},{<ab>},{<ab>}}").score == 9)
  assert(parseDataStream("{{<!!>},{<!!>},{<!!>},{<!!>}}").score == 9)
  assert(parseDataStream("{{<a!>},{<a!>},{<a!>},{<ab>}}").score ==  3)

  assert(parseDataStream("<>").garbageCount == 0)
  assert(parseDataStream("<random characters>").garbageCount == 17)
  assert(parseDataStream("<<<<>").garbageCount == 3)
  assert(parseDataStream("<{!>}>").garbageCount == 2)
  assert(parseDataStream("<!!>").garbageCount == 0)
  assert(parseDataStream("<!!!>>").garbageCount == 0)
  assert(parseDataStream("<{o\"i!a,<{i<a>").garbageCount == 10)

  val data = getDataFromResource("y2017/day9/input.txt")
  val resultState = parseDataStream(data)

  println(s"Result first star: ${resultState.score}")
  println(s"Result second star: ${resultState.garbageCount}")

  def parseDataStream(stream: String): State = {
    stream.foldLeft(State(score = 0, depth = 0, groupCount = 0, garbageCount = 0, inGarbage = false, ignoreNext = false)) {
      case (state@State(_, _, _, _, _, true), _) ⇒ state.copy(ignoreNext = false)
      case (state@State(_, _, _, _, true, _), '!') ⇒ state.copy(ignoreNext = true)
      case (state@State(_, _, _, _, false, _), '<') ⇒ state.copy(inGarbage = true)
      case (state@State(_, _, _, _, true, _), '>') ⇒ state.copy(inGarbage = false)
      case (state@State(_, _, _, _, true, _), _) ⇒ state.copy(garbageCount = state.garbageCount + 1)
      case (state@State(_, _, _, _, false, _), '{') ⇒ state.copy(depth = state.depth + 1)
      case (state@State(_, _, _, _, false, _), '}') ⇒ state.copy(score = state.score + state.depth, depth = state.depth - 1, groupCount = state.groupCount + 1)
      case (state, _) ⇒ state
    }
  }

  case class State(score: Int, depth: Int, groupCount: Int, garbageCount: Int, inGarbage: Boolean, ignoreNext: Boolean)

  def getDataFromResource(path: String): String = Source.fromResource(path).getLines().toList.head
}
