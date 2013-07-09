// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
package tribool {

  /** Abstract class representing a TriBool with behaviour
    *
    * @constructor create a TriBoolean that represents a value
    * with assoicated operations
    *
    * @param value TriBoolRepresentation that is the base value
    * that is having meaning added to.
    */

  abstract class TriBoolean(val value: TriBoolRepresentation) {

    override def equals(other: Any) =
      other match {
        case that: TriBoolean =>
          (that canEqual this) &&
          (value == that.value)
        case _ =>
          false
      }

    /** canEqual is a helper method to allow the correct semantics of
      * equals to be implemented in the hierarchy
      *
      * @see "Programming In Scala: A Comprehnsive Step-By-Step Guide"
      * (Stairway book) chapter 30 - Object Equality
      */

    def canEqual(other: Any): Boolean =
      other.isInstanceOf[TriBoolean]

    override def hashCode = value.hashCode
    override def toString = value.toString
  }

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

  trait AnalogyImplication[
    A <: TriBoolean
        with TriBoolean.Disjunctable[A]
        with TriBoolean.Negatable[A]] {

    this: A =>

    def implies(other: A): A = !this or other
  }

}
