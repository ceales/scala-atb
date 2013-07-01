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
        extends TriBoolean
    {
      def not: T
      def unary_! = not
    }

    abstract trait Conjunctable[T <: TriBoolean]
        extends TriBoolean
    {
      def and(other: T): T
    }

    abstract trait Disjunctable[T <: TriBoolean]
        extends TriBoolean
    {
      def or(other: T): T
    }

    abstract trait Implicable[T <: TriBoolean]
        extends TriBoolean
    {
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
    this : TriBoolean.Builder[T] =>

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
    this : TriBoolean.Builder[T] =>

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
    this : TriBoolean.Builder[T] =>

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

  trait KleeneImplication[T <: TriBoolean]
      extends TriBoolean.Implicable[T]
  {
    this : TriBoolean.Builder[T] =>

    def implies(other : T) : T =
      {
        import ConcreteTriBoolean._
        makeTriBoolean(
          (this.value) match {
            case False => True
            case True => other.value
            case Unknown =>
              if ( other.value == True ) True else Unknown
          }
        )
      }
  }

  trait LukasiewiczImplication[T <: TriBoolean]
      extends TriBoolean.Implicable[T]
  {
    this : TriBoolean.Builder[T] =>

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

  object KleeneBoolean
  {
    case object True extends KleeneBoolean(ConcreteTriBoolean.True)
    case object False extends KleeneBoolean(ConcreteTriBoolean.False)
    case object Unknown extends KleeneBoolean(ConcreteTriBoolean.Unknown)
  }

  sealed abstract class KleeneBoolean(value : ConcreteTriBoolean)
      extends TriBoolean(value)
      with KleeneNegation[KleeneBoolean]
      with KleeneConjunction[KleeneBoolean]
      with KleeneDisjunction[KleeneBoolean]
      with KleeneImplication[KleeneBoolean]
      with TriBoolean.Builder[KleeneBoolean]
  {

    override def equals(other: Any) =
      other match {
        case that: KleeneBoolean => super.equals(other)
        case _ => false
      }

    def makeTriBoolean(x : ConcreteTriBoolean) : KleeneBoolean =
      (x) match {
        case ConcreteTriBoolean.True => KleeneBoolean.True
        case ConcreteTriBoolean.False => KleeneBoolean.False
        case ConcreteTriBoolean.Unknown => KleeneBoolean.Unknown
      }
  }

  object LukasiewiczBoolean
  {
    case object True extends LukasiewiczBoolean(ConcreteTriBoolean.True)
    case object False extends LukasiewiczBoolean(ConcreteTriBoolean.False)
    case object Unknown extends LukasiewiczBoolean(ConcreteTriBoolean.Unknown)
  }

  sealed abstract class LukasiewiczBoolean(value : ConcreteTriBoolean)
      extends TriBoolean(value)
      with KleeneNegation[LukasiewiczBoolean]
      with KleeneConjunction[LukasiewiczBoolean]
      with KleeneDisjunction[LukasiewiczBoolean]
      with LukasiewiczImplication[LukasiewiczBoolean]
      with TriBoolean.Builder[LukasiewiczBoolean]
  {
    override def equals(other: Any) =
      other match {
        case that: LukasiewiczBoolean => super.equals(other)
        case _ => false
      }

    def makeTriBoolean(x : ConcreteTriBoolean) : LukasiewiczBoolean =
      (x) match {
        case ConcreteTriBoolean.True => LukasiewiczBoolean.True
        case ConcreteTriBoolean.False => LukasiewiczBoolean.False
        case ConcreteTriBoolean.Unknown => LukasiewiczBoolean.Unknown
      }

    def isNotFalse : LukasiewiczBoolean =
      !this implies this

    def isTrue : LukasiewiczBoolean =
      !((!this).isNotFalse)

    def isUnknown : LukasiewiczBoolean =
      (!isNotFalse) and (!isTrue)
  }

}
