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
    * The natural extension of Negation to three valed logic.
    *  - not `True` == `False`
    *  - not `False` == `True`
    *  - not `Unknown` == `Unknown`
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait KleeneNegation[A <: TriBoolean[A]]
      extends Negatable[A] {

    this: A =>

    def not: A = {
      this match {
        case True => False
        case False => True
        case Unknown => Unknown
      }
    }
  }

  /**
    * An extension of `and` from two to three valued logic.
    *
    * The idea, is that `Unknown` is actually a concrete `True` or `False` it
    * is just not known which one.
    *
    *  - `True and x` == `x`
    *  - `False and  x` == `False`
    *  - `Unknown and Unknown` == `Unknown`
    *  - `Unknown and x` == `x and Unknown`
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait KleeneConjunction[A <: TriBoolean[A]]
      extends Conjunctable[A] {

    this: A =>

    def and(other: A): A = {
      this match {
        case True => other
        case False => False
        case Unknown =>
          if ( other == False ) False else Unknown
      }
    }
  }

  /**
    * An extension of `or` from two to three valued logic.
    *
    * The idea, is that `Unknown` is actually a concrete `True` or `False` it
    * is just not known which one.
    *
    *  - `True or x` == `True`
    *  - `False or x` == `x`
    *  - `Unknown or Unknown` == `Unknown`
    *  - `Unknown or x` == `x or Unknown`
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait KleeneDisjunction[A <: TriBoolean[A]]
      extends Disjunctable[A] {

    this: A =>

    def or(other: A): A = {
      this match {
        case True => True
        case False => other
        case Unknown =>
          if ( other == True ) True else Unknown
      }
    }
  }

  /**
    * Analogy Implication, with the implemetation of the two supporting
    * operations constrained to be the Kleene implementations:
    *  - `Disjunctable[A]` == `KleeneDisjunction[A]`
    *  - `Negatable[A]` == `KleeneNegation[A]`
    *
    * @see [[Disjunctable]]
    * @see [[Negatable]]
    * @see [[KleeneDisjunction]]
    * @see [[KleeneNegation]]
    * @see [[AnalogyImplication]]
    *
    * @tparam A the implementing [[TriBoolean]] Class
    */

  trait KleeneImplication[
    A <: TriBoolean[A]
        with KleeneDisjunction[A]
        with KleeneNegation[A]]
      extends AnalogyImplication[A] {

    this: A =>
  }

  /**
    * The Companion object of [[KleeneTriBoolean]], holds the three concrete
    * instances: `True`, `False`, and `Unknown`, each an instance and
    * sub-object of [[KleeneTriBoolean]]
    */

  object KleeneTriBoolean {

    /**
      * The `True` value for [[KleeneTriBoolean]]
      */

    case object True extends KleeneTriBoolean

    /**
      * The `False` value for [[KleeneTriBoolean]]
      */

    case object False extends KleeneTriBoolean

    /**
      * The `Unknown` value for [[KleeneTriBoolean]]
      */

    case object Unknown extends KleeneTriBoolean
  }

  /**
    * Assemblege of a working [[TriBoolean]] with the Kleene semantics.
    *
    * @see [[http://en.wikipedia.org/wiki/Tribool#Kleene_logic]]
    */

  sealed abstract class KleeneTriBoolean private()
      extends TriBoolean[KleeneTriBoolean]
      with KleeneNegation[KleeneTriBoolean]
      with KleeneConjunction[KleeneTriBoolean]
      with KleeneDisjunction[KleeneTriBoolean]
      with KleeneImplication[KleeneTriBoolean] {

    lazy protected val True = KleeneTriBoolean.True
    lazy protected val False = KleeneTriBoolean.False
    lazy protected val Unknown = KleeneTriBoolean.Unknown
  }


}
