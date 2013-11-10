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

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

import atb.tribool.KleeneTriBoolean

class TestKleeneTriBoolean
    extends FunSpec
    with Matchers
    with TableDrivenPropertyChecks
{

  import KleeneTriBoolean._

  val negationTable = Table (
    ("value" , "negation" ),
    (True, False),
    (False, True),
    (Unknown, Unknown)
  )

  val andTable = Table (
    ("a" , "b", "conjunction"),
    (True, True, True),
    (True, False, False),
    (True, Unknown, Unknown),
    (False, False, False),
    (False, Unknown, False),
    (Unknown,Unknown, Unknown)
  )

  val orTable = Table (
    ("a" , "b", "disjunction"),
    (True, True, True),
    (True, False, True),
    (True, Unknown, True),
    (False, False, False),
    (False, Unknown, Unknown),
    (Unknown,Unknown, Unknown)
  )

  val impliesTable = Table (
    ("a" , "b", "implication"),
    (True, True, True),
    (True, False, False),
    (True, Unknown, Unknown),
    (False, False,  True),
    (False, Unknown, True),
    (False, True, True),
    (Unknown, Unknown, Unknown),
    (Unknown, True, True),
    (Unknown, False, Unknown)
  )

  describe("A KleeneTriBoolean") {

    it("Should be negatable") {
      forAll (negationTable) {
        ( value: KleeneTriBoolean,
          negation: KleeneTriBoolean ) =>
        !value should equal (negation)
      }
    }
    it("Should be conjoinable") {
      forAll (andTable) {
        ( a: KleeneTriBoolean,
          b: KleeneTriBoolean,
          conjunction: KleeneTriBoolean ) =>
        (a and b) should equal (conjunction)
        (b and a) should equal (conjunction)
      }
    }
    it("Should be disjoinable") {
      forAll (orTable) {
        ( a: KleeneTriBoolean,
          b: KleeneTriBoolean,
          disjunction: KleeneTriBoolean ) =>
        (a or b) should equal (disjunction)
        (b or a) should equal (disjunction)
      }
    }
    it("Should be implicable") {
      forAll (impliesTable) {
        ( a: KleeneTriBoolean,
          b: KleeneTriBoolean,
          implication: KleeneTriBoolean ) =>
        (a implies b) should equal (implication)
      }
    }
  }

}
