// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb.tribool {

  private object TriBoolRepresentation {
    case object True extends TriBoolRepresentation
    case object False extends TriBoolRepresentation
    case object Unknown extends TriBoolRepresentation
  }

  abstract sealed trait TriBoolRepresentation

  object TriBoolean {

    abstract trait Negatable[A <: TriBoolean] {
      this: A =>
      def not: A
      def unary_! = not
    }

    abstract trait Conjunctable[A <: TriBoolean] {
      this: A =>
      def and(other: A): A
    }

    abstract trait Disjunctable[A <: TriBoolean] {
      this: A =>
      def or(other: A): A
    }

    abstract trait Implicable[A <: TriBoolean] {
      this: A =>
      def implies(other: A): A
    }

    abstract trait Builder[A <: TriBoolean] {
      def makeTriBoolean(x: TriBoolRepresentation): A
    }
  }

  class TriBoolean(val value: TriBoolRepresentation) {
    override def equals(other: Any) =
      other match {
        case that: TriBoolean => value == that.value
        case _ => false
      }

    override def hashCode = value.hashCode
    override def toString = value.toString
  }

  trait KleeneNegation[A <: TriBoolean]
      extends TriBoolean.Negatable[A] {

    this: A with TriBoolean.Builder[A] =>

    def not: A = {
      import TriBoolRepresentation._
      makeTriBoolean (
        value match {
          case True => False
          case False => True
          case Unknown => Unknown
        }
      )
    }
  }

  trait KleeneConjunction[A <: TriBoolean]
      extends TriBoolean.Conjunctable[A] {

    this: A with TriBoolean.Builder[A] =>

    def and(other: A): A = {
      import TriBoolRepresentation._
      makeTriBoolean(
        value match {
          case True => other.value
          case False => False
          case Unknown =>
            if ( other.value == False ) False else Unknown
        }
      )
    }
  }

  trait KleeneDisjunction[A <: TriBoolean]
      extends TriBoolean.Disjunctable[A] {

    this: A with TriBoolean.Builder[A] =>

    def or(other: A): A = {
      import TriBoolRepresentation._
      makeTriBoolean(
        value match {
          case True => True
          case False => other.value
          case Unknown =>
            if ( other.value == True ) True else Unknown
        }
      )
    }
  }

  trait AnalogyImplication[
    A <: TriBoolean
        with TriBoolean.Disjunctable[A]
        with TriBoolean.Negatable[A]] {

    this: A =>

    def implies(other: A): A = !this or other
  }

  trait KleeneImplication[
    A <: TriBoolean
        with KleeneDisjunction[A]
        with KleeneNegation[A]]
      extends AnalogyImplication[A] {

    this: A =>
  }

  trait LukasiewiczImplication[A <: TriBoolean]
      extends TriBoolean.Implicable[A] {

    this: A with TriBoolean.Builder[A] =>

    def implies(other: A): A = {
      import TriBoolRepresentation._
      makeTriBoolean(
        value match {
          case False => True
          case True => other.value
          case Unknown =>
            if ( other.value == False ) Unknown else True
        }
      )
    }
  }

  object KleeneTriBoolean {
    case object True extends KleeneTriBoolean(TriBoolRepresentation.True)
    case object False extends KleeneTriBoolean(TriBoolRepresentation.False)
    case object Unknown extends KleeneTriBoolean(TriBoolRepresentation.Unknown)
  }

  sealed abstract class KleeneTriBoolean(value: TriBoolRepresentation)
      extends TriBoolean(value)
      with KleeneNegation[KleeneTriBoolean]
      with KleeneConjunction[KleeneTriBoolean]
      with KleeneDisjunction[KleeneTriBoolean]
      with KleeneImplication[KleeneTriBoolean]
      with TriBoolean.Builder[KleeneTriBoolean] {

    override def equals(other: Any) =
      other match {
        case that: KleeneTriBoolean => super.equals(other)
        case _ => false
      }

    def makeTriBoolean(x: TriBoolRepresentation): KleeneTriBoolean =
      x match {
        case TriBoolRepresentation.True => KleeneTriBoolean.True
        case TriBoolRepresentation.False => KleeneTriBoolean.False
        case TriBoolRepresentation.Unknown => KleeneTriBoolean.Unknown
      }
  }

  object LukasiewiczTriBoolean {
    case object True extends LukasiewiczTriBoolean(TriBoolRepresentation.True)
    case object False extends LukasiewiczTriBoolean(TriBoolRepresentation.False)
    case object Unknown extends LukasiewiczTriBoolean(TriBoolRepresentation.Unknown)
  }

  sealed abstract class LukasiewiczTriBoolean(value: TriBoolRepresentation)
      extends TriBoolean(value)
      with KleeneNegation[LukasiewiczTriBoolean]
      with KleeneConjunction[LukasiewiczTriBoolean]
      with KleeneDisjunction[LukasiewiczTriBoolean]
      with LukasiewiczImplication[LukasiewiczTriBoolean]
      with TriBoolean.Builder[LukasiewiczTriBoolean] {

    override def equals(other: Any) =
      other match {
        case that: LukasiewiczTriBoolean => super.equals(other)
        case _ => false
      }

    def makeTriBoolean(x: TriBoolRepresentation): LukasiewiczTriBoolean =
      x match {
        case TriBoolRepresentation.True => LukasiewiczTriBoolean.True
        case TriBoolRepresentation.False => LukasiewiczTriBoolean.False
        case TriBoolRepresentation.Unknown => LukasiewiczTriBoolean.Unknown
      }

    def isNotFalse: LukasiewiczTriBoolean =
      !this implies this

    def isTrue: LukasiewiczTriBoolean =
      !((!this).isNotFalse)

    def isUnknown: LukasiewiczTriBoolean =
      (!isNotFalse) and (!isTrue)
  }

}
