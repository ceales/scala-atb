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
  import scala.collection.mutable.Builder
  import scala.collection.mutable.ArrayBuffer
  import scala.collection.generic.CanBuildFrom

  object SetBooleanFormula {
    val empty = new SetBooleanFormula(new TreeSet[BooleanFormula]())
    def apply(elems: BooleanFormula*) =
      fromSeq(elems)
    def fromSeq(elems: Seq[BooleanFormula]) =
      empty ++ elems

    def newBuilder: Builder[BooleanFormula,SetBooleanFormula] =
      new ArrayBuffer[BooleanFormula] mapResult fromSeq

    implicit def canBuildFrom:
        CanBuildFrom[SetBooleanFormula,BooleanFormula,SetBooleanFormula] =
      new CanBuildFrom[SetBooleanFormula,BooleanFormula,SetBooleanFormula] {
        def apply() : Builder[BooleanFormula,SetBooleanFormula] = newBuilder
        def apply(from: SetBooleanFormula):
            Builder[BooleanFormula,SetBooleanFormula] = newBuilder
      }
  }

  sealed class SetBooleanFormula private(
    val underlyingSet: TreeSet[BooleanFormula]
  ) extends SortedSet[BooleanFormula]
      with SortedSetLike[BooleanFormula,SetBooleanFormula]
      with Ordered[SetBooleanFormula] {

    require(underlyingSet != null)

    override def newBuilder: Builder[BooleanFormula,SetBooleanFormula] =
      SetBooleanFormula.newBuilder;

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

    def compare(that: SetBooleanFormula) = {
      import Ordering.Implicits.seqDerivedOrdering;
      val thisAsSeq = this.underlyingSet.toSeq
      val thatAsSeq = that.underlyingSet.toSeq
      seqDerivedOrdering(ordering).compare(thisAsSeq,thatAsSeq)
    }

    override def toString() = {
      this.mkString(
        "SetBooleanFormula(",
        ",",
        ")"
      );
    }

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
    private def variableToRepr(x: VariableType) = x.mkString("\"","","\"")

    case class Value(val v: Boolean) extends BooleanFormula
    case class Variable(val v: VariableType) extends BooleanFormula {
      private lazy val name = {
        val sb = new StringBuilder("Variable(")
        sb.append(variableToRepr(v))
        sb.append(")")
        sb.result
      }
      override def toString = name
    }
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

    def conjunctants(f: And) =
      (f) match {
        case And(c) => c
      }

    def disjunctants(f: Or) =
      (f) match {
        case Or(c) => c
      }

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

    def conjoin(fs: BooleanFormula*): BooleanFormula =
      conjoin(SetBooleanFormula fromSeq fs)

    def conjoin(fs: SetBooleanFormula): BooleanFormula = {
      if ( fs.isEmpty ) {
        True
      } else if (fs contains False) {
        False
      } else if (fs.size == 1) {
        fs.head
      } else if (fs.exists(isConjunction)) {
        val conjunctions: Seq[SetBooleanFormula] =
          fs.toSeq.filter(isConjunction) map {
            (x) => x.asInstanceOf[And]
          } map conjunctants _
        conjoin(
          conjunctions.foldLeft(SetBooleanFormula.empty)(_.union(_) )
            union
            fs.filterNot(isConjunction)
        )
      } else if ( !contradictions(fs).isEmpty ) {
        False
      } else if ( fs contains True ) {
        conjoin(fs filterNot(True.equals _))
      } else {
        And(fs)
      }
    }

    def disjoin(fs: BooleanFormula*): BooleanFormula =
      disjoin(SetBooleanFormula fromSeq fs)

    def disjoin(fs: SetBooleanFormula): BooleanFormula = {
      if ( fs.isEmpty ) {
        False
      } else if (fs contains True) {
        True
      } else if (fs.size == 1) {
        fs.head
      } else if (fs.exists(isDisjunction)) {
        val disjunctions: Seq[SetBooleanFormula] =
          fs.toSeq.filter(isDisjunction) map {
            (x) => x.asInstanceOf[Or]
          } map disjunctants _
        conjoin(
          disjunctions.foldLeft(SetBooleanFormula.empty)(_.union(_) )
            union
            fs.filterNot(isDisjunction)
        )
      } else if ( !contradictions(fs).isEmpty ) {
          True
      } else if ( fs contains False ) {
          disjoin(fs.filterNot(False.equals _))
      } else {
        Or(fs)
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
      BooleanFormula.conjoin(this,other)

    def or(other: BooleanFormula) =
      BooleanFormula.disjoin(this,other)

    def not() = BooleanFormula.negate(this)

    def unary_! = not()

    def implies(other: BooleanFormula) =
      !this or other

    def xor(other: BooleanFormula) =
      (this or other) and !(this and other)

    def iff(other: BooleanFormula) =
      !(this xor other)

  }

}
