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

  trait KleeneImplication[
    A <: TriBoolean[A]
        with KleeneDisjunction[A]
        with KleeneNegation[A]]
      extends AnalogyImplication[A] {

    this: A =>
  }

  object KleeneTriBoolean {
    case object True extends KleeneTriBoolean
    case object False extends KleeneTriBoolean
    case object Unknown extends KleeneTriBoolean
  }

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
