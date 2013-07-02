// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
{

  object ConcreteTriBoolean {
    case object True extends ConcreteTriBoolean
    case object False extends ConcreteTriBoolean
    case object Unknown extends ConcreteTriBoolean
  }

  abstract sealed trait ConcreteTriBoolean

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
      def makeTriBoolean(x: ConcreteTriBoolean): A
    }

  }

  class TriBoolean(val value: ConcreteTriBoolean) {
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
      import ConcreteTriBoolean._
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
      import ConcreteTriBoolean._
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
      import ConcreteTriBoolean._
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
      import ConcreteTriBoolean._
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
    case object True extends KleeneTriBoolean(ConcreteTriBoolean.True)
    case object False extends KleeneTriBoolean(ConcreteTriBoolean.False)
    case object Unknown extends KleeneTriBoolean(ConcreteTriBoolean.Unknown)
  }

  sealed abstract class KleeneTriBoolean(value: ConcreteTriBoolean)
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

    def makeTriBoolean(x: ConcreteTriBoolean): KleeneTriBoolean =
      x match {
        case ConcreteTriBoolean.True => KleeneTriBoolean.True
        case ConcreteTriBoolean.False => KleeneTriBoolean.False
        case ConcreteTriBoolean.Unknown => KleeneTriBoolean.Unknown
      }
  }

  object LukasiewiczTriBoolean {
    case object True extends LukasiewiczTriBoolean(ConcreteTriBoolean.True)
    case object False extends LukasiewiczTriBoolean(ConcreteTriBoolean.False)
    case object Unknown extends LukasiewiczTriBoolean(ConcreteTriBoolean.Unknown)
  }

  sealed abstract class LukasiewiczTriBoolean(value: ConcreteTriBoolean)
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

    def makeTriBoolean(x: ConcreteTriBoolean): LukasiewiczTriBoolean =
      x match {
        case ConcreteTriBoolean.True => LukasiewiczTriBoolean.True
        case ConcreteTriBoolean.False => LukasiewiczTriBoolean.False
        case ConcreteTriBoolean.Unknown => LukasiewiczTriBoolean.Unknown
      }

    def isNotFalse: LukasiewiczTriBoolean =
      !this implies this

    def isTrue: LukasiewiczTriBoolean =
      !((!this).isNotFalse)

    def isUnknown: LukasiewiczTriBoolean =
      (!isNotFalse) and (!isTrue)
  }

}
