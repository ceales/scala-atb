// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb.tribool {

  /**
    * See [[atb.tribool.TriBoolean.Negatable]]
    */
  class TriBoolean(val value: TriBoolRepresentation) {
    override def equals(other: Any) =
      other match {
        case that: TriBoolean => value == that.value
        case _ => false
      }

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
