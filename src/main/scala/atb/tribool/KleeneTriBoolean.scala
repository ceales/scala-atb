// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
package tribool {

  trait KleeneNegation[A <: TriBoolean[A]]
      extends Negatable[A] {

    this: A =>

    def not: A = {
      this match {
        case True => False
        case False => True
        case Unknown => Unknown
      }
    }
  }

  trait KleeneConjunction[A <: TriBoolean[A]]
      extends Conjunctable[A] {

    this: A =>

    def and(other: A): A = {
      this match {
        case True => other
        case False => False
        case Unknown =>
          if ( other == False ) False else Unknown
      }
    }
  }

  trait KleeneDisjunction[A <: TriBoolean[A]]
      extends Disjunctable[A] {

    this: A =>

    def or(other: A): A = {
      this match {
        case True => True
        case False => other
        case Unknown =>
          if ( other == True ) True else Unknown
      }
    }
  }

  trait KleeneImplication[
    A <: TriBoolean[A]
        with KleeneDisjunction[A]
        with KleeneNegation[A]]
      extends AnalogyImplication[A] {

    this: A =>
  }

  object KleeneTriBoolean {
    case object True extends KleeneTriBoolean
    case object False extends KleeneTriBoolean
    case object Unknown extends KleeneTriBoolean
  }

  sealed abstract class KleeneTriBoolean
      extends TriBoolean[KleeneTriBoolean]
      with KleeneNegation[KleeneTriBoolean]
      with KleeneConjunction[KleeneTriBoolean]
      with KleeneDisjunction[KleeneTriBoolean]
      with KleeneImplication[KleeneTriBoolean] {

    lazy protected val True = KleeneTriBoolean.True
    lazy protected val False = KleeneTriBoolean.False
    lazy protected val Unknown = KleeneTriBoolean.Unknown
  }


}
