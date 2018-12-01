package de.emiliomg.adventofcode.y2017.day13

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/13
  */
object Day13 extends App {

  type Layers = Map[Int, Int]

  val testData: Layers = getDataFromResource("y2017/day13/test.txt")

  assert(firstStar(testData) == 24)
  assert(secondStar(testData) == 10)

  val data = getDataFromResource("y2017/day13/input.txt")

  println(s"Result first star: ${firstStar(data)}") // 1588
  println(s"Result second star: ${secondStar(data)}")

  def firstStar(layers: Layers): Int = {
    layersCaught(layers).map(c ⇒ c._1 * c._2).sum
  }

  def secondStar(layers: Layers): Int = {
    Stream.from(0).find(delay ⇒ layersCaught(layers, delay).isEmpty).get
  }

  def layersCaught(layers: Layers, delay: Int = 0): Map[Int, Int] =
    layers.filter({
      case (depth, range) ⇒ layerCaught(range, depth + delay)
    })

  /*
   * layersCaught: depth x range
   * layerCought: range x TIME (depth =⇒ time)
   */
  def layerCaught(range: Int, time: Int): Boolean = {
    time % (2 * (range - 1)) == 0
  }

  def getDataFromResource(path: String): Layers = Source.
    fromResource(path).
    getLines().
    toList.
    foldLeft(Map[Int, Int]())((acc, str) ⇒ {
      val Array(layer, depth) = str.split(": ").map(_.toInt)
      acc + (layer → depth)
    })
}
