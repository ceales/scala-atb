// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb.tribool {

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

  trait KleeneImplication[
    A <: TriBoolean
        with KleeneDisjunction[A]
        with KleeneNegation[A]]
      extends AnalogyImplication[A] {

    this: A =>
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


}
