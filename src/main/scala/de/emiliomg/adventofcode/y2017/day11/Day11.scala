package de.emiliomg.adventofcode.y2017.day11

import scala.io.Source

/**
  * https://adventofcode.com/2017/day/11
  * Algorithms taken from https://www.redblobgames.com/grids/hexagons/#distances-axial
  */
object Day11 extends App{

  assert(processInputString("ne,ne,ne").getDistanceToCenter == 3)
  assert(processInputString("ne,ne,sw,sw").getDistanceToCenter == 0)
  assert(processInputString("ne,ne,s,s").getDistanceToCenter == 2)
  assert(processInputString("se,sw,se,sw,sw").getDistanceToCenter == 3)

  val inputString = Source.fromResource("y2017/day11/input.txt").getLines().toList.head
  val position = processInputString(inputString)

  println(s"Result first star: ${position.getDistanceToCenter}") // 810
  println(s"Result second star: ${position.maxDistance}") // 1567

  def processInputString(str: String): HexPosition = {
    str.split(",").foldLeft(HexPosition(0,0))((pos, direction) ⇒ direction match {
      case "n" ⇒
        val newQ = pos.q
        val newR = pos.r - 1
        val newDistance = HexPosition(q = newQ, r = newR).getDistanceToCenter
        pos.copy(q = newQ, r = newR, maxDistance = math.max(pos.maxDistance, newDistance))
      case "ne" ⇒
        val newQ = pos.q + 1
        val newR = pos.r - 1
        val newDistance = HexPosition(q = newQ, r = newR).getDistanceToCenter
        pos.copy(q = newQ, r = newR, maxDistance = math.max(pos.maxDistance, newDistance))
      case "se" ⇒
        val newQ = pos.q + 1
        val newR = pos.r
        val newDistance = HexPosition(q = newQ, r = newR).getDistanceToCenter
        pos.copy(q = newQ, r = newR, maxDistance = math.max(pos.maxDistance, newDistance))
      case "s" ⇒
        val newQ = pos.q
        val newR = pos.r + 1
        val newDistance = HexPosition(q = newQ, r = newR).getDistanceToCenter
        pos.copy(q = newQ, r = newR, maxDistance = math.max(pos.maxDistance, newDistance))
      case "sw" ⇒
        val newQ = pos.q - 1
        val newR = pos.r + 1
        val newDistance = HexPosition(q = newQ, r = newR).getDistanceToCenter
        pos.copy(q = newQ, r = newR, maxDistance = math.max(pos.maxDistance, newDistance))
      case "nw" ⇒
        val newQ = pos.q - 1
        val newR = pos.r
        val newDistance = HexPosition(q = newQ, r = newR).getDistanceToCenter
        pos.copy(q = newQ, r = newR, maxDistance = math.max(pos.maxDistance, newDistance))
    })
  }
}

case class HexPosition(q: Int, r: Int, maxDistance: Int = 0) {
  def getDistanceToCenter: Int = {
    val a = CubePosition.fromHexPosition(this)
    val b = CubePosition.fromHexPosition(HexPosition(0, 0))

    a.getDistanceTo(b)
  }
}

case class CubePosition(x: Int, y: Int, z: Int) {
  def getDistanceTo(that: CubePosition): Int =
    (math.abs(this.x - that.x) + math.abs(this.y - that.y) + math.abs(this.z - that.z)) / 2
}

object CubePosition {
  def fromHexPosition(hexPos: HexPosition): CubePosition = {
    val  x = hexPos.q
    val  z = hexPos.r
    val  y = -x-z

    new CubePosition(x, y, z)
  }
}
