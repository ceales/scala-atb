// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb.tribool {

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
