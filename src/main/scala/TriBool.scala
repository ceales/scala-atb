// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
{

trait TriBoolRepresentation
{
  type Representation

  protected val True : Representation
  protected val False : Representation
  protected val Unknown : Representation

  import scala.language.implicitConversions

  implicit def convertFromBoolean(x : Boolean) =
    x match
    {
      case true => True
      case false => False
    }
}

trait CoreTriBool extends TriBoolRepresentation
{
  def not =
    this match
    {
      case True => False
      case False => True
      case Unknown => Unknown
    }

  def unary_! = not

  def and(other : Representation) =
    (this) match
      {
        case True => other
        case False => False
        case Unknown =>
          if ( other == False ) False else Unknown
      }

  def or(other : Representation) =
    (this) match
      {
        case True => True
        case False => other
        case Unknown =>
          if ( other == True ) True else Unknown
      }

  def |(other : Representation) = this or other
  def &(other : Representation) = this and other

  def implies(other : Representation) : Representation
}

object KleeneTriBool
{
  case object True extends KleeneTriBool
  case object False extends KleeneTriBool
  case object Unknown extends KleeneTriBool
}

sealed abstract class KleeneTriBool extends CoreTriBool
{
  type Representation = KleeneTriBool

  protected lazy val True = KleeneTriBool.True
  protected lazy val False = KleeneTriBool.False
  protected lazy val Unknown = KleeneTriBool.Unknown

  def implies(other : Representation) = !this or other
}

object LukasiewiczTriBool
{
  case object True extends LukasiewiczTriBool
  case object False extends LukasiewiczTriBool
  case object Unknown extends LukasiewiczTriBool
}

sealed abstract class LukasiewiczTriBool extends CoreTriBool
{
  type Representation = LukasiewiczTriBool

  protected lazy val True = LukasiewiczTriBool.True
  protected lazy val False = LukasiewiczTriBool.False
  protected lazy val Unknown = LukasiewiczTriBool.Unknown

  def implies(other : Representation) =
    (this) match
    {
      case False => True
      case True => other
      case Unknown =>
        if ( other == False ) Unknown else True
    }
}

}
