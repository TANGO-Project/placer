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

package placerT.metadata

import scala.collection.immutable.{SortedMap, SortedSet}

object Formula {
  implicit def intToConst(a: Int): Formula = Const(a)

  implicit def stringToConst(a: String): Formula = Dim(a)

  def isLinear(f: Formula): Boolean = {
    f match {
      case Plus(a, b) => isLinear(a) && isLinear(b)
      case Minus(a, b) => isLinear(a) && isLinear(b)
      case Times(a, b) => (isConstant(a) && isLinear(b)) || (isLinear(a) && isConstant(b))
      case Const(_) => true
      case Dim(d) => true
      case Div(a, b) => isLinear(a) && isConstant(b)
      case Table(index, values) => false
    }
  }

  def isConstant(f: Formula): Boolean = {
    f match {
      case Plus(a, b) => isConstant(a) && isConstant(b)
      case Minus(a, b) => isConstant(a) && isConstant(b)
      case Times(a, b) => isConstant(a) && isConstant(b)
      case Const(_) => true
      case Dim(d) => false
      case Div(a, b) => isConstant(a) && isConstant(b)
      case Table(index, values) => false
    }
  }

  def simplifyConstants(f: Formula, setValues: SortedMap[String, Int] = SortedMap.empty): Formula = {
    f match {
      case Plus(a, b) =>
        (simplifyConstants(a, setValues), simplifyConstants(b, setValues)) match {
          case (Const(x), Const(y)) => Const(x + y)
          case (Const(0), bb) => bb
          case (aa, Const(0)) => aa
          case (aa, bb) => Plus(aa, bb)
        }
      case Minus(a, b) =>
        (simplifyConstants(a, setValues), simplifyConstants(b, setValues)) match {
          case (Const(x), Const(y)) => Const(x - y)
          case (aa, Const(0)) => aa
          case (aa, bb) => Minus(aa, bb)
        }
      case Times(a, b) =>
        (simplifyConstants(a, setValues), simplifyConstants(b, setValues)) match {
          case (Const(x), Const(y)) => Const(x * y)
          case (Const(0), bb) => Const(0)
          case (aa, Const(0)) => Const(0)
          case (Const(1), bb) => bb
          case (aa, Const(1)) => aa
          case (aa, bb) => Times(aa, bb)
        }
      case Div(a, b) =>
        (simplifyConstants(a, setValues), simplifyConstants(b, setValues)) match {
          case (Const(x), Const(y)) => Const(x / y)
          case (Const(0), bb) => Const(0)
          case (aa, Const(0)) => throw new Error("divide by zero")
          case (aa, Const(1)) => aa
          case (aa, bb) => Div(aa, bb)
        }
      case Const(_) => f
      case Dim(x) => setValues.get(x) match {
        case Some(v) => v
        case None => Dim(x)
      }
      case Table(index, values) =>
        val newIndex = simplifyConstants(index, setValues)
        val newValues = values.map(simplifyConstants(_, setValues))

        newIndex match {
          case Const(c) => newValues(c)
          case f => Table(newIndex, newValues)
        }
    }
  }

  def splitConstant(f: Formula): (Const, Formula) = {
    f match {
      case Plus(a, b) =>
        val (Const(ac), af) = splitConstant(a)
        val (Const(bc), bf) = splitConstant(b)
        (Const(ac + bc), Plus(af, bf))
      case Minus(a, b) =>
        val (Const(ac), af) = splitConstant(a)
        val (Const(bc), bf) = splitConstant(b)
        (Const(ac - bc), Minus(af, bf))
      case Times(a, b) =>
        val (Const(ac), af) = splitConstant(a)
        val (Const(bc), bf) = splitConstant(b)
        (Const(ac * bc), Minus(Times(a, b), Const(ac * bc)))
      case Div(a, b) =>
        val (Const(ac), af) = splitConstant(a)
        val (Const(bc), bf) = splitConstant(b)
        (Const(ac / bc), Minus(Div(a, b), Const(ac / bc)))
      case Const(x) => (Const(x), Const(0))
      case Dim(x) => (Const(0), Dim(x))
      case t: Table => (Const(0), t)
    }
  }

  def eval(f: Formula, values: SortedMap[String, Int]): Int = {
    f match {
      case Plus(a, b) => eval(a, values) + eval(b, values)
      case Minus(a, b) => eval(a, values) - eval(b, values)
      case Times(a, b) => eval(a, values) * eval(b, values)
      case Const(c) => c
      case Dim(x) => values(x)
      case Div(a, b) => eval(a, values) / eval(b, values)
      case Table(index, tvalues) =>
        val i = eval(index, values)
        eval(tvalues(i), values)
    }
  }
}

sealed abstract class Formula(subFormula: Formula*) {
  def +(b: Formula): Formula = Plus(this, b)

  def -(b: Formula): Formula = Minus(this, b)

  def *(b: Formula): Formula = Times(this, b)

  def /(b: Formula): Formula = Div(this, b)

  def terms: SortedSet[String] = subFormula.foldLeft(SortedSet.empty[String])({ case (acc, f) => acc ++ f.terms })

  def splitSum: List[Formula] = List(this)

  def prettyPrint(priorityOut: Int = 0): String = toString

  def parenteses(p: Int, outP: Int, s: String): String = if (outP > p) "(" + s + ")" else s
}

case class Plus(a: Formula, b: Formula) extends Formula(a, b) {
  override def splitSum: List[Formula] = List(a, b).flatMap(_.splitSum)

  override def prettyPrint(priorityOut: Int): String = parenteses(1, priorityOut, a.prettyPrint(1) + "+" + b.prettyPrint(1))
}

case class Minus(a: Formula, b: Formula) extends Formula(a, b) {
  override def splitSum: List[Formula] = a.splitSum ::: b.splitSum.map(x => Times(x, Const(-1)))

  override def prettyPrint(priorityOut: Int): String = parenteses(2, priorityOut, a.prettyPrint(1) + "-" + b.prettyPrint(2))
}

case class Times(a: Formula, b: Formula) extends Formula(a, b) {
  override def prettyPrint(priorityOut: Int): String = parenteses(3, priorityOut, a.prettyPrint(3) + "*" + b.prettyPrint(3))
}

case class Div(a: Formula, b: Formula) extends Formula(a, b) {
  override def prettyPrint(priorityOut: Int): String = parenteses(3, priorityOut, a.prettyPrint(3) + "/" + b.prettyPrint(3))
}

case class Const(value: Int) extends Formula() {
  override def prettyPrint(priorityOut: Int): String = "" + value
}

case class Dim(dimension: String) extends Formula() {
  override def terms: SortedSet[String] = SortedSet(dimension)

  override def prettyPrint(priorityOut: Int): String = dimension
}

// [1,2+a,3,4](g) ==> ...
case class Table(index: Formula, values: Array[Formula]) extends Formula() {
  override def terms: SortedSet[String] = index.terms ++ values.flatMap(_.terms)

  override def prettyPrint(priorityOut: Int): String = "[" + values.map(_.prettyPrint()).mkString(",") + "](" + index.prettyPrint() + ")"
}

/*
min
max
ite

<
>
>=
<=
==
and
or
not
*/