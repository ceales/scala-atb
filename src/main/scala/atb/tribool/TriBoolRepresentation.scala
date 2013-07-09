// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
package tribool {

  /** An abstract type to hold the physical
    * representations of the three different
    * values of a tribool
    */

  abstract sealed trait TriBoolRepresentation

  /** The physical values representing the three
    * different truth values of a tribool
    *
    * These values are not intended to be used naked,
    * they are just place holders for names, they only
    * have an interpretation once packed inside a
    * [[atb.tribool.TriBoolean]]. Their names may be
    * suggestive, but that is all they are.
    */

  object TriBoolRepresentation {
    case object True extends TriBoolRepresentation
    case object False extends TriBoolRepresentation
    case object Unknown extends TriBoolRepresentation

  }

}
