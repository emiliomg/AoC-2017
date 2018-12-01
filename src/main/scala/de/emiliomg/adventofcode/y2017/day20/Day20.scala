package de.emiliomg.adventofcode.y2017.day20

import scala.annotation.tailrec
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Day20 extends App {

  val testDataStar1 = getData("y2017/day20/testStar1.txt")
  val testParticlesStar1: List[Particle] = ParticleParser.parse(ParticleParser.particles, testDataStar1).get

  val testDataStar2 = getData("y2017/day20/testStar2.txt")
  val testParticlesStar2: List[Particle] = ParticleParser.parse(ParticleParser.particles, testDataStar2).get

  assert(firstStar(testParticlesStar1) == 0)
  assert(secondStar(testParticlesStar2) == 1)

  val data = getData("y2017/day20/input.txt")
  val particles: List[Particle] = ParticleParser.parse(ParticleParser.particles, data).get
  assert(particles.length == 1000)

  // First star
  println(firstStar(particles)) // 364
  // Second star
  println(secondStar(particles)) // 420

  /**
    * The particle with the smallest acceleration will stay closest to (0,0,0) in the long run.
    * If some particles have the same acceleration, use the starting velocity.
    * And if that is the same, use the starting point.
    */
  def firstStar(particles: List[Particle]): Int = {
    particles
      .zipWithIndex
      .map { case (p, idx) ⇒
        val acc = p.acceleration.x.abs + p.acceleration.y.abs + p.acceleration.z.abs
        val vel = p.velocity.x.abs + p.velocity.y.abs + p.velocity.z.abs
        val pos = p.position.x.abs + p.position.y.abs + p.position.z.abs
        (idx, acc, vel, pos)
      }
      .minBy { case (_, acc, vel, pos) ⇒ (acc, vel, pos) }
      ._1
  }

  def secondStar(initialParticles: List[Particle]): Int = {
    val maxStepAmountWitoutChange = 50

    @tailrec
    def step(particles: List[Particle], stepsWithoutChange: Int): List[Particle] = {
      val nextTick = particles.map { p ⇒
        val tmp = p.copy(velocity = p.velocity + p.acceleration)
        tmp.copy(position = tmp.position + tmp.velocity)
      }

      val collisions: List[ParticleVector] = nextTick
        .groupBy(_.position)
        .mapValues(_.size)
        .filter(_._2 > 1)
        .keys
        .toList

      val nextTickWithoutCollisions = nextTick
          .filterNot(p ⇒ collisions.contains(p.position))


//      println(s"Start: $particles")
//      println(s"NextStep: $nextTick")
//      println(s"Collisions: $collisions")
//      println(s"nextTickWithoutCollisions: $nextTickWithoutCollisions")
//      println("------------------------")

      if (stepsWithoutChange == maxStepAmountWitoutChange) {
        nextTickWithoutCollisions
      } else {
        if (collisions.nonEmpty) step(nextTickWithoutCollisions, 0)
        else step(nextTickWithoutCollisions, stepsWithoutChange + 1)
      }
    }

    val particlesWithoutCollision = step(initialParticles, 0)
    particlesWithoutCollision.size
  }

  def getData(path: String): String = Source.fromResource(path).getLines().mkString("\n")

  case class Particle(position: ParticleVector, velocity: ParticleVector, acceleration: ParticleVector) {
    override def toString: String = s"<p:$position,v: $velocity, a:$acceleration>"
  }
  case class ParticleVector(x: Int, y: Int, z: Int) {
    def +(that: ParticleVector): ParticleVector = ParticleVector(x + that.x, y + that.y, z + that.z)

    override def toString: String = s"($x,$y,$z)"
  }

  object ParticleParser extends RegexParsers {
    override val whiteSpace: Regex = "[ \t]".r

    def scalar: Parser[String] = "-?[0-9]+".r <~ ",".?
    def vector: Parser[ParticleVector] = "<" ~> scalar ~ scalar ~ scalar <~ ">" ^^ { case x ~ y ~ z ⇒ ParticleVector(x.toInt, y.toInt, z.toInt) }
    def position: Parser[ParticleVector] = "p=" ~> vector <~ ",".?
    def velocity: Parser[ParticleVector] = "v=" ~> vector <~ ",".?
    def acceleration: Parser[ParticleVector] = "a=" ~> vector <~ ",".?
    def particle: Parser[Particle] = position ~ velocity ~ acceleration ^^ { case pos ~ vel ~ acc ⇒ Particle(pos, vel, acc)}

    def particles: Parser[List[Particle]] = (particle <~ "\n".?).*
  }
}
