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
    * Lukasiewicz formulation of Implication in three valued logic.
    *
    * Its truth function is the same as [[KleeneImplication]] except in the
    * case of `Unknown => Unknown`, in Kleene's logic this is `Unknown` while
    * in Lukasiewicz this implication is `True`.
    *
    * This specific case gives us back tautologies, these are missing in
    * Kleene logic as there
    * a universal assignment of `Unknown` makes any formula `Unknown`
    *
    * @see [[http://en.wikipedia.org/wiki/Tribool#.C5.81ukasiewicz_logic]]
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

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

  /**
    * Given Lukasiewicz's formulation of Implication, we can define a useful
    * set of unary operators.
    */

  object LukasiewiczUnaryOperators {

    /**
      * Tarski's unary operator defined as `!x => x`
      */

    def itIsNotFalseThat[
      A <: TriBoolean[A]
          with Negatable[A]
          with Implicable[A]](x: A): A =
      !x implies x

    /**
      * Tarski's unary operator defined as `!itIsNotFalseThat(!x)`
      */

    def itIsTrueThat[
      A <: TriBoolean[A]
          with Negatable[A]
          with Implicable[A]](x: A): A =
      !(itIsNotFalseThat[A](!x))

    /**
      * Tarski's unary operator defined as
      * `itIsNotFalseThat(x) and !itIsTrueThat(x)`
      */

    def itIsUnknownThat[
      A <: TriBoolean[A]
          with Negatable[A]
          with Implicable[A]
          with Conjunctable[A]] (x: A): A =
      itIsNotFalseThat[A](x) and !itIsTrueThat[A](x)

  }

  /**
    * The Companion object of [[LukasiewiczTriBoolean]], holds the three concrete
    * instances: `True`, `False`, and `Unknown`, each an instance and
    * sub-object of [[LukasiewiczTriBoolean]]
    */

  object LukasiewiczTriBoolean {

    /**
      * The `True` value for [[LukasiewiczTriBoolean]]
      */

    case object True extends LukasiewiczTriBoolean

    /**
      * The `False` value for [[LukasiewiczTriBoolean]]
      */

    case object False extends LukasiewiczTriBoolean

    /**
      * The `Unknown` value for [[LukasiewiczTriBoolean]]
      */

    case object Unknown extends LukasiewiczTriBoolean

    /**
      * A list containing the three values
      */

    val values = List(True,False,Unknown)

  }

  /**
    * Assemblege of a working [[TriBoolean]] with the Lukasiewicz semantics.
    *
    * @see [[http://en.wikipedia.org/wiki/Tribool#.C5.81ukasiewicz_logic]]
    */

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
