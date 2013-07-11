// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

package atb
package tribool {

  /**
    * A TriBoolean is a class with three distinguished values
    *  - True
    *  - False
    *  - Unknown
    *
    * @tparam A The implementing type of the values. Note `A` needs
    * to be subclass of `TriBoolean[A]`.
    */

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
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait Negatable[A <: TriBoolean[A]] {
    def not: A
    def unary_! = not
  }

  /**
    * Conjunctable captures the notion of a [[TriBoolean]] that
    * can be conjoined, with the binary operator `and`
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait Conjunctable[A <: TriBoolean[A]] {
    def and(other: A): A
  }

  /**
    * Disjunctable captures the notion of a [[TriBoolean]] that
    * can be disjoined, with the binary operator `or`
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait Disjunctable[A <: TriBoolean[A]] {
    def or(other: A): A
  }

  /**
    * Implicable captures the notion of a [[TriBoolean]] that
    * can be used in an implication,
    * via the binary operator `implies`
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait Implicable[A <: TriBoolean[A]] {
    def implies(other: A): A
  }

  /**
    * AnalogyImplication, the implementation of implies
    * Carried over from the two-value logic.
    *
    * `a implies b === !a or b`
    */

  trait AnalogyImplication[
    A <: TriBoolean[A]
        with Disjunctable[A]] {

    this: Negatable[A] =>

    def implies(other: A): A = !this or other
  }

}
