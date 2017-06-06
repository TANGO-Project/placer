/*
 * Placer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Placer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

/*
 * Placer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Placer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

/*
 * Placer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Placer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License  for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with OscaR.
 * If not, see http://www.gnu.org/licenses/lgpl-3.0.en.html
 *
 */

package placerT.io

import main.scala.placerT.metadata._
import net.liftweb.json.MappingException

import scala.util.parsing.combinator.RegexParsers

object FormulaParser {
  def apply(s: String): Formula = new FormulaParser().apply(s)
}

class FormulaParser() extends RegexParsers {
  def apply(s: String): Formula = {
    val tmp = parseAll(formula, s)
    tmp match {
      case Success(result, _) => result
      case n: NoSuccess =>
        println(n)
        throw new MappingException(n.toString)
    }
  }

  def formula: Parser[Formula] = expr

  def expr: Parser[Formula] =
    term ~ rep("[+-]".r ~ term) ^^ {
      case t ~ ts => ts.foldLeft(t) {
        case (t1, "+" ~ t2) => Plus(t1, t2)
        case (t1, "-" ~ t2) => Minus(t1, t2)
      }
    }

  def term: Parser[Formula] =
    factor ~ rep("[*/]".r ~ factor) ^^ {
      case t ~ ts => ts.foldLeft(t) {
        case (t1, "*" ~ t2) => Times(t1, t2)
        case (t1, "/" ~ t2) => Div(t1, t2)
      }
    }

  def factor: Parser[Formula] = ("(" ~> expr <~ ")") | naturalParser | identifier | array

  def naturalParser: Parser[Formula] = """[0-9]+""".r ^^ { case s: String => Const(s.toInt) }

  def identifier: Parser[Formula] = """[a-zA-Z_]+""".r ^^ { case s => Dim(s.toString) }

  def array: Parser[Formula] = ("[" ~> repsep(formula, ",")) ~ ("](" ~> formula <~ ")") ^^ { case (arrayValues: List[Formula]) ~ (index: Formula) => Table(index, arrayValues.toArray) }
}
