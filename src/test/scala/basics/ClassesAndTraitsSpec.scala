package basics

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers._

import ClassesAndTraits._

class ClassesAndTraitsSpec extends AnyFlatSpec {

  "Bounded" should "be correct" in {
    val rectangle1 = new Rectangle(2,2,2,2)
    val rectangle2 = new Rectangle(3,3,3,5)
    val bounds = minimumBoundingRectangle(Set(rectangle1, rectangle2))
    bounds.minX shouldEqual 2
    bounds.maxX shouldEqual 6
    bounds.minY shouldEqual 2
    bounds.maxY shouldEqual 8
  }

  /*"allBooleans" should "contain all possible boolean values" in {
    allBooleans.size shouldEqual 2
    allBooleans.reduce(_ && _) shouldEqual false
    allBooleans.reduce(_ || _) shouldEqual true
  }

  "stringLength" should "work for all strings" in {
    forAll { x: String =>
      stringLength(x) shouldEqual x.length
    }
  }

  "power" should "be correct" in {
    forAll { n: Int =>
      power(2)(n) shouldEqual Math.pow(n.toDouble, 2)
    }

    forAll { n: Byte =>
      power(3)(n.toInt) shouldEqual Math.pow(n.toDouble, 3)
    }
  }*/


}

