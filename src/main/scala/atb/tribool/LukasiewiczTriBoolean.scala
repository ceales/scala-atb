// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
package tribool {

  trait LukasiewiczImplication[A <: TriBoolean[A]]
      extends Implicable[A] {

    this: A =>

    def implies(other: A): A = {
      this match {
        case False => True
        case True => other
        case Unknown =>
          if ( other == False ) Unknown else True
      }
    }
  }

  object LukasiewiczUnaryOperators {

    def isNotFalse[
      A <: TriBoolean[A]
          with Negatable[A]
          with Implicable[A]](x: A): A =
      !x implies x

    def isTrue[
      A <: TriBoolean[A]
          with Negatable[A]
          with Implicable[A]](x: A): A =
      !(isNotFalse[A](!x))

    def isUnknown[
      A <: TriBoolean[A]
          with Negatable[A]
          with Implicable[A]
          with Conjunctable[A]] (x: A): A =
      !isNotFalse[A](x) and !isTrue[A](x)

  }

  object LukasiewiczTriBoolean {
    case object True extends LukasiewiczTriBoolean
    case object False extends LukasiewiczTriBoolean
    case object Unknown extends LukasiewiczTriBoolean
  }

  sealed abstract class LukasiewiczTriBoolean private()
      extends TriBoolean[LukasiewiczTriBoolean]
      with KleeneNegation[LukasiewiczTriBoolean]
      with KleeneConjunction[LukasiewiczTriBoolean]
      with KleeneDisjunction[LukasiewiczTriBoolean]
      with LukasiewiczImplication[LukasiewiczTriBoolean] {

    lazy protected val True = LukasiewiczTriBoolean.True
    lazy protected val False = LukasiewiczTriBoolean.False
    lazy protected val Unknown = LukasiewiczTriBoolean.Unknown

  }

}
