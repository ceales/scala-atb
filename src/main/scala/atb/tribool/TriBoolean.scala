// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
package tribool {

  /** Abstract class representing a TriBool with behaviour
    *
    * @constructor create a [[TriBoolean]] that represents a value
    * with assoicated operations
    *
    * @param value [[TriBoolRepresentation]] that is the base value
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
      * Chapter 30 - Object Equality
      */

    def canEqual(other: Any): Boolean =
      other.isInstanceOf[TriBoolean]

    override def hashCode = value.hashCode
    override def toString = value.toString
  }

  object TriBoolean {

    /**
      * Negatable captures the notion of a [[TriBoolean]] that
      * can be negated, both with the property `not` and unary `!`
      *
      * @tparam A the name of the implementing [[TriBoolean]] Class
      */

    abstract trait Negatable[A <: TriBoolean] {
      this: A =>
      def not: A
      def unary_! = not
    }

    /**
      * Conjunctable captures the notion of a [[TriBoolean]] that
      * can be conjoined, with the binary operator `and`
      *
      * @tparam A the name of the implementing [[TriBoolean]] Class
      */

    abstract trait Conjunctable[A <: TriBoolean] {
      this: A =>
      def and(other: A): A
    }

    /**
      * Disjunctable captures the notion of a [[TriBoolean]] that
      * can be disjoined, with the binary operator `or`
      *
      * @tparam A the name of the implementing [[TriBoolean]] Class
      */

    abstract trait Disjunctable[A <: TriBoolean] {
      this: A =>
      def or(other: A): A
    }

    /**
      * Implicable captures the notion of a [[TriBoolean]] that
      * can be used in an implication,
      * via the binary operator `implies`
      *
      * @tparam A the name of the implementing [[TriBoolean]] Class
      */

    abstract trait Implicable[A <: TriBoolean] {
      this: A =>
      def implies(other: A): A
    }

    /**
      * A Builder is a utility trait that provides a mechanism
      * for a subclass of [[TriBoolean]] to provide an instance
      * of itself that is its wrapper around the passed
      * [[TriBoolRepresentation]]
      *
      * @tparam A the type of [[TriBoolean]] that is constructed
      */

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
