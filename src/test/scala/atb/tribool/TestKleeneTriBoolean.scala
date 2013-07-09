// Scala-atb
// copyright 2013 Craig Eales
//
// distributed under the MIT License (MIT)
//

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
