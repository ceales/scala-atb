// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
package tribool {

  abstract class TriBoolean[A] {
    this: A =>

    protected val True: A
    protected val False: A
    protected val Unknown: A
  }

  /**
    * Negatable captures the notion of a [[TriBoolean]] that
    * can be negated, both with the property `not` and unary `!`
    *
    * @tparam A the name of the implementing [[TriBoolean]] Class
    */

  trait Negatable[A <: TriBoolean[A]] {
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

  trait Conjunctable[A <: TriBoolean[A]] {
    this: A =>
    def and(other: A): A
  }

  /**
    * Disjunctable captures the notion of a [[TriBoolean]] that
    * can be disjoined, with the binary operator `or`
    *
    * @tparam A the name of the implementing [[TriBoolean]] Class
    */

  trait Disjunctable[A <: TriBoolean[A]] {
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

  trait Implicable[A <: TriBoolean[A]] {
    this: A =>
    def implies(other: A): A
  }

  trait AnalogyImplication[
    A <: TriBoolean[A]
        with Disjunctable[A]
        with Negatable[A]] {

    this: A =>

    def implies(other: A): A = !this or other
  }

}
