// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
{

  object ConcreteTriBoolean
  {
    case object True extends ConcreteTriBoolean
    case object False extends ConcreteTriBoolean
    case object Unknown extends ConcreteTriBoolean
  }

  abstract sealed trait ConcreteTriBoolean
  {
  }

  object TriBoolean
  {

    abstract trait Negatable[T <: TriBoolean]
    {
      this :T =>
      def not: T
      def unary_! = not
    }

    abstract trait Conjunctable[T <: TriBoolean]
    {
      this :T =>
      def and(other: T): T
    }

    abstract trait Disjunctable[T <: TriBoolean]
    {
      this :T =>
      def or(other: T): T
    }

    abstract trait Implicable[T <: TriBoolean]
    {
      this :T =>
      def implies(other: T): T
    }

    abstract trait Builder[T <: TriBoolean]
    {
      def makeTriBoolean(x : ConcreteTriBoolean) : T
    }

  }

  class TriBoolean(val value : ConcreteTriBoolean)
  {
    override def equals(other: Any) =
      other match {
        case that: TriBoolean => value == that.value
        case _ => false
      }

    override def hashCode = value.hashCode
    override def toString = value.toString
  }

  trait KleeneNegation[T <: TriBoolean]
      extends TriBoolean.Negatable[T]
  {
    this : T with TriBoolean.Builder[T] =>

    def not : T =
    {
      import ConcreteTriBoolean._
      makeTriBoolean (
        (value) match {
          case True => False
          case False => True
          case Unknown => Unknown
        }
      )
    }
  }

  trait KleeneConjunction[T <: TriBoolean]
      extends TriBoolean.Conjunctable[T]
  {
    this : T with TriBoolean.Builder[T] =>

    def and(other : T) : T =
    {
      import ConcreteTriBoolean._
      makeTriBoolean(
        (value) match {
          case True => other.value
          case False => False
          case Unknown =>
            if ( other.value == False ) False else Unknown
        }
      )
    }
  }

  trait KleeneDisjunction[T <: TriBoolean]
      extends TriBoolean.Disjunctable[T]
  {
    this : T with TriBoolean.Builder[T] =>

    def or(other : T) : T =
    {
      import ConcreteTriBoolean._
      makeTriBoolean(
        (value) match {
          case True => True
          case False => other.value
          case Unknown =>
            if ( other.value == True ) True else Unknown
        }
      )
    }
  }

  trait AnalogyImplication[
    T <: TriBoolean
        with TriBoolean.Disjunctable[T]
        with TriBoolean.Negatable[T]]
  {
    this : T =>
    def implies(other : T) : T = !this or other
  }

  trait KleeneImplication[
    T <: TriBoolean
        with KleeneDisjunction[T]
        with KleeneNegation[T]]
      extends AnalogyImplication[T]
  {
    this : T =>
  }

  trait LukasiewiczImplication[T <: TriBoolean]
      extends TriBoolean.Implicable[T]
  {
    this : T with TriBoolean.Builder[T] =>

    def implies(other : T) : T =
    {
      import ConcreteTriBoolean._
      makeTriBoolean(
        (this.value) match {
          case False => True
          case True => other.value
          case Unknown =>
            if ( other.value == False ) Unknown else True
        }
      )
    }
  }

  object KleeneTriBoolean
  {
    case object True extends KleeneTriBoolean(ConcreteTriBoolean.True)
    case object False extends KleeneTriBoolean(ConcreteTriBoolean.False)
    case object Unknown extends KleeneTriBoolean(ConcreteTriBoolean.Unknown)
  }

  sealed abstract class KleeneTriBoolean(value : ConcreteTriBoolean)
      extends TriBoolean(value)
      with KleeneNegation[KleeneTriBoolean]
      with KleeneConjunction[KleeneTriBoolean]
      with KleeneDisjunction[KleeneTriBoolean]
      with KleeneImplication[KleeneTriBoolean]
      with TriBoolean.Builder[KleeneTriBoolean]
  {

    override def equals(other: Any) =
      other match {
        case that: KleeneTriBoolean => super.equals(other)
        case _ => false
      }

    def makeTriBoolean(x : ConcreteTriBoolean) : KleeneTriBoolean =
      (x) match {
        case ConcreteTriBoolean.True => KleeneTriBoolean.True
        case ConcreteTriBoolean.False => KleeneTriBoolean.False
        case ConcreteTriBoolean.Unknown => KleeneTriBoolean.Unknown
      }
  }

  object LukasiewiczTriBoolean
  {
    case object True extends LukasiewiczTriBoolean(ConcreteTriBoolean.True)
    case object False extends LukasiewiczTriBoolean(ConcreteTriBoolean.False)
    case object Unknown extends LukasiewiczTriBoolean(ConcreteTriBoolean.Unknown)
  }

  sealed abstract class LukasiewiczTriBoolean(value : ConcreteTriBoolean)
      extends TriBoolean(value)
      with KleeneNegation[LukasiewiczTriBoolean]
      with KleeneConjunction[LukasiewiczTriBoolean]
      with KleeneDisjunction[LukasiewiczTriBoolean]
      with LukasiewiczImplication[LukasiewiczTriBoolean]
      with TriBoolean.Builder[LukasiewiczTriBoolean]
  {
    override def equals(other: Any) =
      other match {
        case that: LukasiewiczTriBoolean => super.equals(other)
        case _ => false
      }

    def makeTriBoolean(x : ConcreteTriBoolean) : LukasiewiczTriBoolean =
      (x) match {
        case ConcreteTriBoolean.True => LukasiewiczTriBoolean.True
        case ConcreteTriBoolean.False => LukasiewiczTriBoolean.False
        case ConcreteTriBoolean.Unknown => LukasiewiczTriBoolean.Unknown
      }

    def isNotFalse : LukasiewiczTriBoolean =
      !this implies this

    def isTrue : LukasiewiczTriBoolean =
      !((!this).isNotFalse)

    def isUnknown : LukasiewiczTriBoolean =
      (!isNotFalse) and (!isTrue)
  }

}
