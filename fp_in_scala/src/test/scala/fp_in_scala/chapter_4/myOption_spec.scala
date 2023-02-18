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

    it should "return the wrapped value or a default if MyNone" in {
      MySome(5).getOrElse(-99) shouldBe 5
      MyNone.getOrElse(-99) shouldBe -99
    }

    it should "return the original option is MySome or a replacement MySome if MyNone" in {
      MySome(5).orElse(MySome(-99)) shouldBe MySome(5)
      MyNone.orElse(MySome(-99)) shouldBe MySome(-99)
    }

    it should "convert a MySome to MyNone if the filter predicate fails" in {
      def test(x: Int) = x == 5
      MySome(5).filter(test) shouldBe MySome(5)
      MySome(10).filter(test) shouldBe MyNone
      MyNone.filter(test) shouldBe MyNone
    }
}
