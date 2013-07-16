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
import org.scalatest.GivenWhenThen
import org.scalatest.matchers.ShouldMatchers

import atb.tribool.KleeneTriBoolean

class TestKleeneTriBoolean
    extends FunSpec
    with ShouldMatchers
    with GivenWhenThen
{

  describe("A KleeneTriBoolean") {

    it("Should obey normal two value logic") {
      Given("A True boolean")
      val t = KleeneTriBoolean.True

      {
        When("Conjoined with True")
        val result=t and KleeneTriBoolean.True
        Then("The result should be True")
        result should equal (KleeneTriBoolean.True)
      }

      {
        When("Conjoined with False")
        val result=t and KleeneTriBoolean.False
        Then("The result should be False")
        result should equal (KleeneTriBoolean.False)
      }

    }
  }

}
