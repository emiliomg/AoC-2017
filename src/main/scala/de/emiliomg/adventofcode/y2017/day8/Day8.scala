package de.emiliomg.adventofcode.y2017.day8

import scala.collection.mutable
import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

/**
  * https://adventofcode.com/2017/day/8
  */
object Day8 extends App {

  val testData = getDataFromResource("y2017/day8/star1.txt")
  val testMachine = createMachine(testData)

  assert(testMachine.firstStar == 1)
  assert(testMachine.secondStar == 10)

  val data = getDataFromResource("y2017/day8/input.txt")
  val machine = createMachine(data)
  println(s"Result first star: ${machine.firstStar}")
  println(s"Result second star: ${machine.secondStar}")

  def createMachine(data: String): Machine = {
    val machine = new Machine()
    val instructions: List[Instruction] = InstructionParser.parse(InstructionParser.instructions, data).get
    machine.processInstructions(instructions)

    machine
  }

  def getDataFromResource(path: String): String = Source.fromResource(path).getLines().toList.mkString("\n")

  class Machine() {
    val registers: mutable.Map[String, Int] = mutable.Map()
    var overallRegisterMax: mutable.Set[Int] = mutable.Set()

    def firstStar: Int = registers.values.max

    def secondStar: Int = overallRegisterMax.max

    def processInstructions(instructions: List[Instruction]): Unit = {
      instructions.foreach(processInstruction)
    }

    private def processInstruction(inst: Instruction): Unit = {
      if (inst.condition.applies(registers)) {
        val regValue = registers.getOrElse(inst.registerName, 0)
        val newRegValue = matchCommand(inst.command)(regValue, inst.offset)

        registers(inst.registerName) = newRegValue
        overallRegisterMax.add(registers.values.max)
      }
    }

    private def matchCommand(op: String): (Int, Int) ⇒ Int = op match {
      case "inc"  ⇒ _ + _
      case "dec"  ⇒ _ - _
      case _    ⇒ throw new NotImplementedError("Operator not implemented")
    }
  }

  case class Instruction(registerName: String, command: String, offset: Int, condition: Condition)

  case class Condition(registerName: String, op: String, number: Int) {
    def applies(registers: mutable.Map[String, Int]): Boolean = {
      val regValue = registers.getOrElse(registerName, 0)
      matchOperator(op)(regValue, number)
    }

    private def matchOperator(op: String): (Int, Int) ⇒ Boolean = op match {
      case ">=" ⇒ _ >= _
      case "<=" ⇒ _ <= _
      case "==" ⇒ _ == _
      case "!=" ⇒ _ != _
      case "<"  ⇒ _ < _
      case ">"  ⇒ _ > _
      case _    ⇒ throw new NotImplementedError("Operator not implemented")
    }
  }

  object InstructionParser extends RegexParsers {
    override val whiteSpace: Regex = "[ \t]+".r

    def nl: Parser[Unit] = "\n" ^^ (_ ⇒ ())

    def register: Parser[String] = "\\p{Alpha}+".r

    def command: Parser[String] = "inc|dec".r

    def number: Parser[Int] = "-?\\p{Digit}+".r ^^ (_.toInt)

    def operator: Parser[String] = ">=|<=|==|!=|<|>".r

    def condition: Parser[Condition] = register ~ operator ~ number ^^ {
      case reg ~ op ~ num ⇒ Condition(reg, op, num)
    }

    def instruction: Parser[Instruction] = register ~ command ~ number ~ "if" ~ condition ^^ {
      case reg ~ cmd ~ offset ~ _ ~ cond ⇒ Instruction(reg, cmd, offset, cond)
    }

    def instructions: Parser[List[Instruction]] = rep(instruction <~ nl.?) ^^ (i ⇒ i)
  }
}
