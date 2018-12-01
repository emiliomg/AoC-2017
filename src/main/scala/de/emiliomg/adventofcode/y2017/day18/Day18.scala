package de.emiliomg.adventofcode.y2017.day18

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * https://adventofcode.com/2017/day/18
  */
object Day18 extends App {

  val testDataStar1 = getDataFromResource("y2017/day18/testStar1.txt")
  val testDataStar2 = getDataFromResource("y2017/day18/testStar2.txt")
  val testInstructionsStar1: Vector[Instruction] = InstructionParserStar1.parse(InstructionParserStar1.instructions, testDataStar1).get.toVector
  val testInstructionsStar2: Vector[Instruction] = InstructionParserStar2.parse(InstructionParserStar2.instructions, testDataStar2).get.toVector

  assert(firstStar(testInstructionsStar1) == 4)
  assert(secondStar(testInstructionsStar2) == 3)

  val data = getDataFromResource("y2017/day18/input.txt")
  val instructionsStar1: Vector[Instruction] = InstructionParserStar1.parse(InstructionParserStar1.instructions, data).get.toVector
  val instructionsStar2: Vector[Instruction] = InstructionParserStar2.parse(InstructionParserStar2.instructions, data).get.toVector

  println(s"Result first star: ${firstStar(instructionsStar1)}") // 7071
  println(s"Result second star: ${secondStar(instructionsStar2)}") // 8001

  def firstStar(instructions: Vector[Instruction]): Long = {
    val machine = new MachineStar1(instructions)
    machine.run()
    machine.soundLastPlayed.get
  }

  def secondStar(instructions: Vector[Instruction]): Long = {
    val inputM1 = mutable.Queue[Long]()
    val inputM2 = mutable.Queue[Long]()
    val m0 = new MachineStar2(0, instructions)
    val m1 = new MachineStar2(1, instructions)

    def step(m0Pos: Int, m1Pos: Int): Long = {
      val newM0Pos = m0.run(position = m0Pos, source = inputM1, target = inputM2)
      val newM1Pos = m1.run(position = m1Pos, source = inputM2, target = inputM1)

      if (inputM1.isEmpty && inputM2.isEmpty) m1.sendCounter
      else step(newM0Pos, newM1Pos)
    }

    step(0, 0)
    m1.sendCounter
  }

  def getDataFromResource(path: String): String = Source.fromResource(path).getLines().mkString("\n")
}

class MachineStar2(id: Int, instructions: Vector[Instruction]){
  implicit val registry: RegistryHost = new RegistryHost(Some(id))
  var sendCounter = 0

  def run(position: Int, source: mutable.Queue[Long], target: mutable.Queue[Long]): Int = {

//    println(s"Starting machine $id at pos $position, sourceQueue is $source")

    @tailrec def step(position: Int): Int = {
      instructions(position) match {
        case Send(param) ⇒
//          println(s"Sending ${param.getValue} to target queue")
          target.enqueue(param.getValue)
          sendCounter += 1
          step(position + 1)
        case Receive(reg) ⇒
          if (source.isEmpty) {
//            println(s"Machine $id stopping at position $position, sourceQueue is empty")
            position
          } else {
            val value = source.dequeue
//            println(s"Receiving $value from source queue")
            reg.setValue(value)
            step(position + 1)
          }
        case Set(reg, param) ⇒
          reg.setValue(param)
          step(position + 1)
        case Increase(reg, param) ⇒
          reg.setValue(reg.getValue + param.getValue)
          step(position + 1)
        case Multiply(reg, param) ⇒
          reg.setValue(reg.getValue * param.getValue)
          step(position + 1)
        case Modulo(reg, param) ⇒
          reg.setValue(reg.getValue % param.getValue)
          step(position + 1)
        case JumpIfGreaterZero(condition, offset) ⇒
          val newPosition = if (condition.getValue > 0) position + offset.getValue else position + 1
          step(newPosition.toInt)
        case nope ⇒
          throw new Exception(s"Machine $id received $nope - this should not happen!")
      }
    }

    step(position)
  }
}

class MachineStar1(instructions: Vector[Instruction]){
  implicit val registry: RegistryHost = new RegistryHost(None)
  var soundLastPlayed: Option[Long] = None

  def run(): Unit = {
    @tailrec def step(position: Int): Unit = {
      instructions(position) match {
        case PlaySound(reg) ⇒
          soundLastPlayed = Some(reg.getValue)
          step(position + 1)
        case Set(reg, param) ⇒
          reg.setValue(param)
          step(position + 1)
        case Increase(reg, param) ⇒
          reg.setValue(reg.getValue + param.getValue)
          step(position + 1)
        case Multiply(reg, param) ⇒
          reg.setValue(reg.getValue * param.getValue)
          step(position + 1)
        case Modulo(reg, param) ⇒
          reg.setValue(reg.getValue % param.getValue)
          step(position + 1)
        case JumpIfGreaterZero(condition, offset) ⇒
          val newPosition = if (condition.getValue > 0) position + offset.getValue else position + 1
          step(newPosition.toInt)
        case Recover(param) ⇒
          if (param.getValue != 0) return
          step(position + 1)
        case nope ⇒
          throw new Exception(s"Received $nope - this should not happen!")
      }
    }

    step(0)
  }
}

class RegistryHost(programId: Option[Int]) {
  val registries: mutable.Map[String, Long] = collection.mutable.Map[String, Long]()

  if (programId.isDefined) setValue("p", programId.get)

  def getValue(name: String): Long = registries.getOrElse(name, 0)
  def setValue(name: String, value: Long): Unit = registries.update(name, value)

  def reset(): Unit = registries.clear()
}

case class Register(name: String, value: Long)

// ######################
// # Instruction Parser #
// ######################


sealed trait Parameter {
  def getValue(implicit registry: RegistryHost): Long
}
case class Value(value: Long) extends Parameter {
  override def getValue(implicit registry: RegistryHost): Long = value
}
case class RegisterRef(name: String) extends Parameter {
  override def getValue(implicit registry: RegistryHost): Long = registry.getValue(name)
  def setValue(newVal: Long)(implicit registry: RegistryHost): Unit = registry.setValue(name, newVal)
  def setValue(param: Parameter)(implicit registry: RegistryHost): Unit = setValue(param.getValue)
}

sealed trait Instruction
sealed trait SND extends Instruction
sealed trait RCV extends Instruction

case class Set(reg: RegisterRef, param: Parameter) extends Instruction
case class Increase(reg: RegisterRef, param: Parameter) extends Instruction
case class Multiply(reg: RegisterRef, param: Parameter) extends Instruction
case class Modulo(reg: RegisterRef, param: Parameter) extends Instruction
case class JumpIfGreaterZero(condition: Parameter, param: Parameter) extends Instruction

// For first star
case class PlaySound(reg: RegisterRef) extends SND
case class Recover(param: Parameter) extends RCV

// For second star
case class Send(reg: Parameter) extends SND
case class Receive(param: RegisterRef) extends RCV

trait BaseInstructionParser extends RegexParsers {
  override val whiteSpace: Regex = "[ \t]+".r
  def register: Parser[RegisterRef] = "\\p{Alpha}+".r ^^ (str ⇒ RegisterRef(str))
  def value: Parser[Value] = "-?\\p{Digit}+".r ^^ (number ⇒ Value(number.toLong))

  def set: Parser[Set] = "set" ~> register ~ (register | value) ^^ { case reg ~ param ⇒ Set(reg, param) }
  def add: Parser[Increase] = "add" ~> register ~ (register | value) ^^ { case reg ~ param ⇒ Increase(reg, param) }
  def mul: Parser[Multiply] = "mul" ~> register ~ (register | value) ^^ { case reg ~ param ⇒ Multiply(reg, param) }
  def mod: Parser[Modulo] = "mod" ~> register ~ (register | value) ^^ { case reg ~ param ⇒ Modulo(reg, param) }
  def jgz: Parser[JumpIfGreaterZero] = "jgz" ~> (register | value) ~ (register | value) ^^ { case cond ~ offset ⇒ JumpIfGreaterZero(cond, offset) }

  def snd: Parser[SND]
  def rcv: Parser[RCV]

  def instruction: Parser[Instruction] = snd | set | add | mul | mod | rcv | jgz

  def instructions: Parser[List[Instruction]] = rep(instruction <~ "\n".?)
}

object InstructionParserStar1 extends BaseInstructionParser {
  override def snd: Parser[PlaySound] = "snd" ~> register ^^ ( reg ⇒ PlaySound(reg))
  override def rcv: Parser[Recover] = "rcv" ~> (register | value) ^^ (cond ⇒ Recover(cond))
}

object InstructionParserStar2 extends BaseInstructionParser {
  override def snd: Parser[Send] = "snd" ~> (register | value) ^^ ( reg ⇒ Send(reg))
  override def rcv: Parser[Receive] = "rcv" ~> register ^^ (cond ⇒ Receive(cond))
}
