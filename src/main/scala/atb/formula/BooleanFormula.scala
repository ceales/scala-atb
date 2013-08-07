// Scala-atb
//
// This software is distributed under The MIT License (MIT):
//
// Copyright (c) 2013 Craig Eales
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

package atb
package formula {

  object BooleanFormula {

    type VarType = String

    case class Value(val v: Boolean) extends BooleanFormula
    case class Variable(val v: VarType) extends BooleanFormula
    case class Not(val v: BooleanFormula) extends BooleanFormula
    case class And(val left: BooleanFormula, val right: BooleanFormula)
        extends BooleanFormula
    case class Or(val left: BooleanFormula, val right: BooleanFormula)
        extends BooleanFormula

    val True = Value(true)
    val False = Value(false)

    def isAtom(formula: BooleanFormula): Boolean =
      (formula) match {
        case Value(_) => true
        case Variable(_) => true
        case Not(Value(_)) => true
        case Not(Variable(_)) => true
        case _ => false
      }

    def variables(formula: BooleanFormula): Set[VarType] =
      (formula) match {
        case Value(_) => Set.empty
        case Variable(v) => Set(v)
        case Not(f) => variables(f)
        case And(l,r) => variables(l) union variables(r)
        case Or(l,r) => variables(l) union variables(r)
      }

    def depth(formula: BooleanFormula): Int =
        (formula) match {
          case Value(_) => 0
          case Variable(_) => 0
          case Not(f) => 1+depth(f)
          case And(l,r) => 1+Math.max(depth(l),depth(r))
          case Or(l,r) => 1+Math.max(depth(l),depth(r))
        }

    def size(formula: BooleanFormula): Int =
        (formula) match {
          case Value(_) => 1
          case Variable(_) => 1
          case Not(f) => 1+size(f)
          case And(l,r) => 1+size(l)+size(r)
          case Or(l,r) => 1+size(l)+size(r)
        }

    def atoms(formula: BooleanFormula): Set[BooleanFormula] =
        (formula) match {
          case x if isAtom(x) => Set(x)
          case Not(f) => atoms(f)
          case And(l,r) => atoms(l) union atoms(r)
          case Or(l,r) => atoms(l) union atoms(r)
        }

    def isConjunctionOfAtoms(formula: BooleanFormula): Boolean =
      (formula) match {
        case x if isAtom(x) => true
        case And(l,r) => isConjunctionOfAtoms(l) && isConjunctionOfAtoms(r)
        case _ => false
      }

    object Hierarchy {

      private def nextLevel(baseFormulae: Set[BooleanFormula]):
          Set[BooleanFormula] = {
        val nots =
          for ( x <- baseFormulae )
          yield Not(x):BooleanFormula
        val ands =
          for ( x <- baseFormulae; y <- baseFormulae)
          yield And(x,y):BooleanFormula
        val ors =
          for ( x <- baseFormulae; y <- baseFormulae)
          yield Or(x,y):BooleanFormula

        nots union ands union ors
      }

      private def levelZero(variables: Set[VarType]): Set[BooleanFormula] = {
        val vars: Set[BooleanFormula] = variables map Variable
        val bools: Set[BooleanFormula] = Set(True,False)
          vars union bools
      }

      def formulaeToLevel(depth: Int,
                          variables: Set[VarType]) =
        (1 to depth).scanLeft(levelZero(variables)) {
          (current,_) =>
          nextLevel(current)
        }
    }

  }

  sealed abstract class BooleanFormula {
    def and(right: BooleanFormula) = BooleanFormula.And(this,right)
    def or(right: BooleanFormula) = BooleanFormula.Or(this,right)
    def unary_! = BooleanFormula.Not(this)
  }

}
