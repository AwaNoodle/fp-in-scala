package fp_in_scala.chapter_4

import org.scalatest.{FlatSpec, Matchers}

class myOption_spec extends FlatSpec with Matchers {
  
    "MyOption" should "map from one MySome type to another" in {
      MySome(1).map(x => x.toString()) shouldBe MySome("1")
    }

    it should "not transform a MyNone via map" in {
      MyNone.map(x => x.toString()) shouldBe MyNone
    }

    it should "flatMap from one MySome type to another" in {
      MySome(1).flatMap(x => MySome(x.toString())) shouldBe MySome("1")
    }

    it should "not transform a MyNone via flatmap" in {
      MyNone.flatMap(x => MySome(x.toString())) shouldBe MyNone
    }
}
