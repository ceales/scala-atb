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

  import scala.collection.SortedSet
  import scala.collection.SortedSetLike
  import scala.collection.immutable.TreeSet

  sealed class SetBooleanFormula private(
    val underlyingSet: TreeSet[BooleanFormula]
  ) extends SortedSet[BooleanFormula]
      with SortedSetLike[BooleanFormula,SetBooleanFormula]
      with Ordered[SetBooleanFormula] {

    def iterator = underlyingSet.iterator

    def -(elem: BooleanFormula) = new SetBooleanFormula(underlyingSet-elem)
    def +(elem: BooleanFormula) = new SetBooleanFormula(underlyingSet+elem)

    def contains(elem: BooleanFormula) = underlyingSet contains elem

    def rangeImpl(
      from: Option[BooleanFormula],
      until: Option[BooleanFormula]): SetBooleanFormula =
      new SetBooleanFormula(underlyingSet.rangeImpl(from,until))

    val ordering = BooleanFormula.ordering

    override val empty = SetBooleanFormula.empty

    import Ordering.Implicits.seqDerivedOrdering;

    def compare(that: SetBooleanFormula) = {
      import Ordering.Implicits.seqDerivedOrdering;
      val thisAsSeq = this.underlyingSet.toSeq
      val thatAsSeq = that.underlyingSet.toSeq
      seqDerivedOrdering(ordering).compare(thisAsSeq,thatAsSeq)
    }

    def map(f: BooleanFormula=>BooleanFormula) =
      new SetBooleanFormula(underlyingSet map f)

    override def toString() = {
      this.mkString(
        "SetBooleanFormula(",
        ",",
        ")"
      );
    }

  }

  object SetBooleanFormula {
    val empty = new SetBooleanFormula(new TreeSet[BooleanFormula]())
    def apply(elems: BooleanFormula*) : SetBooleanFormula =
      empty ++ elems
  }

  object BooleanFormula {

    object ordering extends Ordering[BooleanFormula] {
      def compare(o1: BooleanFormula, o2: BooleanFormula) =
        (o1,o2) match {
          case (Value(x),Value(y)) => x compare y
          case (Value(_),_) => -1
          case (_,Value(_)) => 1
          case (Variable(x),Variable(y)) => x compare y
          case (Variable(_),_) => -1
          case (_,Variable(_)) => 1
          case (Not(x),Not(y)) => x compare y
          case (Not(_),_) => -1
          case (_,Not(_)) => 1
          case (And(x),And(y)) => x compare y
          case (And(_),_) => -1
          case (_,And(_)) => 1
          case (Or(x),Or(y)) => x compare y
        }
    }

    type VariableType = String

    case class Value(val v: Boolean) extends BooleanFormula
    case class Variable(val v: VariableType) extends BooleanFormula
    case class Not(val b: BooleanFormula) extends BooleanFormula
    case class And(val s: SetBooleanFormula) extends BooleanFormula
    case class Or(val s: SetBooleanFormula) extends BooleanFormula

    val True = Value(true)
    val False = Value(false)

    def isNegative(f: BooleanFormula) =
      (f) match {
        case Not(x) => true
        case Value(false) => true
        case _ => false
      }

    def isPositive(f: BooleanFormula) = !isNegative(f)

    def isConjunction(f: BooleanFormula) =
      (f) match {
        case And(_) => true
        case _ => false
      }

    def isDisjunction(f: BooleanFormula) =
      (f) match {
        case Or(_) => true
        case _ => false
      }

    def isNegation(f: BooleanFormula) =
      (f) match {
        case Not(_) => true
        case _ => false
      }

    def isValue(f: BooleanFormula) =
      (f) match {
        case Value(_) => true
        case _ => false
      }

    def isVariable(f: BooleanFormula) =
      (f) match {
        case Variable(_) => true
        case _ => false
      }

    def negate(f: BooleanFormula) =
      (f) match {
        case Value(x) => Value(!x)
        case Not(x) => x
        case x => Not(x)
      }

    def contradictions(fs: SetBooleanFormula) = {
      val (negative,positive) = fs partition isNegative
      val contra = positive intersect (negative map negate _)
      contra map ((x: BooleanFormula) => (x, negate(x)))
    }

    def conjoin(fs :SetBooleanFormula): BooleanFormula = {
      if ( fs.isEmpty ) {
        True
      } else if (fs.size == 1) {
        fs.head
      } else {
        if (fs.exists(isConjunction)) {
          val conjunctions = fs.filter(isConjunction) map {
            (x: BooleanFormula) => {
              x match {
                case And(y) => y
              }
            }
          }
          conjoin(
            conjunctions.fold(SetBooleanFormula.empty)( _.union(_) )
              union
              fs.filter((x : BooleanFormula) => !isConjunction(x))
          )
        }
        else if ( !contradictions(fs).isEmpty ) {
          False
        } else {
          And(fs)
        }
      }
    }

    def disjoin(fs :SetBooleanFormula): BooleanFormula = {
      if ( fs.isEmpty ) {
        False
      } else if (fs.size == 1) {
        fs.head
      } else {
        if ( !contradictions(fs).isEmpty ) {
          True
        } else {
          Or(fs)
        }
      }
    }

    def simplify(f: BooleanFormula): BooleanFormula =
      (f) match {
        case x@Value(_) => x
        case x@Variable(_) => x
        case Not(x) => negate(simplify(x))
        case And(x) => conjoin(x map (simplify _ ))
        case Or(x) => disjoin(x map (simplify _))
      }
  }

  sealed abstract class BooleanFormula extends Ordered[BooleanFormula] {

    def compare(that: BooleanFormula): Int =
      BooleanFormula.ordering.compare(this,that)

    def and(other: BooleanFormula) =
      BooleanFormula.conjoin(SetBooleanFormula(this,other))

    def or(other: BooleanFormula) =
      BooleanFormula.disjoin(SetBooleanFormula(this,other))

    def not() = BooleanFormula.negate(this)

    def implies(other: BooleanFormula) =
      this.not() or other

    def xor(other: BooleanFormula) =
      (this or other) and (this and other).not()

    def iff(other: BooleanFormula) =
      (this xor other).not()

  }

}
