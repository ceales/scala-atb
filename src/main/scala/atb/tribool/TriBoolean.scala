// Scala-atb
//
// This software is distributed under The MIT License (MIT):
//
// Copyright (c) 2013 Craig Eales
//
// Permission is hereby granted, free of charge, to any person obtaining a copy
// of this software and associated documentation files (the "Software"), to deal
// in the Software without restriction, including without limitation the rights
// to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
// copies of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be included in
// all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
// IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
// FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
// AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
// LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
// OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
// THE SOFTWARE.

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
        with Disjunctable[A]
        with Negatable[A]] {

    this: A =>

    def implies(other: A): A = !this or other
  }

}
