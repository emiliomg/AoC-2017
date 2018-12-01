package de.emiliomg.adventofcode.y2017.day21

import scala.io.Source
import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

object Day21 extends App {

  val testDataStar1 = getData("y2017/day21/test.txt")
  val parsedTestData: List[Rule] = RulesParser.parse(RulesParser.rules, testDataStar1).get

  assert(parsedTestData.length == 2)

  println(testDataStar1)
  parsedTestData.foreach(println)


//  val input = getData("y2017/day21/input.txt")
//  val parsedInput: List[Rule] = RulesParser.parse(RulesParser.rules, input).get
//  assert(parsedInput.length == 108)

  def getData(path: String): String = Source.fromResource(path).getLines().mkString("\n")

  case class Grid[T](lst: List[List[T]]) {

    lst.foreach(row ⇒ assert(row.length == lst.length))

    def flip: Grid[T] = Grid(lst.reverse)
    def rotate: Grid[T] = Grid(lst.reverse.transpose)

    override def toString: String = {
      lst.map(_.mkString("")).mkString("\n")
    }
  }

  case class Rule(pattern: Grid[Char], result: Grid[Char]) {
    override def toString: String = {
      "------------\n" + pattern.toString + "\n => \n" + result.toString + "\n------------"
    }
  }

  object RulesParser extends RegexParsers {
    override val whiteSpace: Regex = "[ \t]+".r

    def bit: Parser[Char] = ".|#".r ^^ (b ⇒ b.toCharArray.head)
    def sep: Parser[String] = "/".r
    def arrow: Parser[String] = "=>".r

    def size2: Parser[Grid[Char]] = repN(2, bit) ~ sep ~ repN(2, bit) ^^ {
      case a ~ _ ~ b ⇒ Grid(List(a,b))
    }

    def size3: Parser[Grid[Char]] = repN(3, bit) ~ sep ~ repN(3, bit) ~ sep ~ repN(3, bit) ^^ {
      case a ~ _ ~ b ~ _ ~ c ⇒ Grid(List(a, b, c))
    }

    def size4: Parser[Grid[Char]] = repN(4, bit) ~ sep ~ repN(4, bit) ~ sep ~ repN(4, bit) ~ sep ~ repN(4, bit) ^^ {
      case a ~ _ ~ b ~ _ ~ c ~ _ ~ d ⇒ Grid(List(a, b, c, d))
    }

    def _2_to_3: Parser[Rule] = size2 ~ arrow ~ size3 ^^ { case sa ~ _ ~ sb ⇒ Rule(sa, sb) }
    def _3_to_4: Parser[Rule] = size3 ~ arrow ~ size4 ^^ { case sa ~ _ ~ sb ⇒ Rule(sa, sb) }

    def rule: Parser[Rule] = _2_to_3 | _3_to_4
    def rules: Parser[List[Rule]] = (rule <~ "\n".?).*
  }
}
