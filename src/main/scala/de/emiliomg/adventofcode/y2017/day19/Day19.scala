package de.emiliomg.adventofcode.y2017.day19

import scala.annotation.tailrec
import scala.io.Source

object Day19 extends App {

  /*
    #################################################################
    #     DO NOT OPEN AND SAVE THE TEST/INPUT FILES IN INTELLIJ     #
    # THE PADDING SPACES WILL BE REMOVED WHICH WILL BREAK THIS CODE #
    #################################################################
   */
  type TubeMap = Array[Array[Char]]

  val testData = getDataFromResource("y2017/day19/test.txt")
  assert(processMap(testData) == ("ABCDEF", 38))

  val input = getDataFromResource("y2017/day19/input.txt")
  println(processMap(input)) // (EOCZQMURF, 16312)

  def processMap(data: TubeMap): (String, Int) = {
//    println(data.map(_.mkString("")).mkString("\n"))
    @tailrec
    def step(y: Int, x: Int, direction: Direction, cnt: Int, result: String): (String, Int) = {
//      println(s"$y,$x,$direction,${data(y)(x)}")
      if (direction.isInstanceOf[UpDown]) {
        data(y)(x) match {
          case ' ' ⇒
            (result, cnt)
          case '+' ⇒
            if   (data(y)(x + Left.modifier) != ' ') step(y, x + Left.modifier, Left, cnt + 1, result)
            else step(y, x + Right.modifier, Right, cnt + 1, result)
          case other ⇒
            if (other != '|' && other != '-') step(y + direction.modifier, x, direction, cnt + 1, result + other)
            else step(y + direction.modifier, x, direction, cnt + 1, result)
        }
      } else {
        data(y)(x) match {
          case ' ' ⇒
            (result, cnt)
          case '+' ⇒
            if   (data(y + Up.modifier)(x) != ' ') step(y + Up.modifier, x, Up, cnt + 1, result)
            else step(y + Down.modifier, x, Down, cnt + 1, result)
          case other ⇒
            if (other != '|' && other != '-') step(y, x + direction.modifier, direction, cnt + 1, result + other)
            else step(y, x + direction.modifier, direction, cnt + 1, result)
        }
      }
    }

    step(0, getStartXPosition(data), Down, 0, "")
  }

  def getStartXPosition(data: TubeMap): Int = data(0).indexWhere(_ != ' ')

  def getDataFromResource(path: String): Array[Array[Char]] = Source.fromResource(path).getLines().toArray.map(_.toArray)

  sealed trait Direction { val modifier: Int }
  sealed trait UpDown
  sealed trait LeftRight
  case object Up extends Direction with UpDown { override val modifier: Int = -1 }
  case object Down extends Direction with UpDown { override val modifier: Int = 1 }
  case object Left extends Direction with LeftRight { override val modifier: Int = -1 }
  case object Right extends Direction with LeftRight { override val modifier: Int = +1 }
}
