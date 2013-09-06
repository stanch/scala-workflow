package scala.workflow

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class AnnotationInterfaceSpec extends FlatSpec with ShouldMatchers {
  "@workflow" should "support DefDefs" in {
    @workflow(option) def test = "asd" + Some(3)
    test should equal (Some("asd3"))
  }

  it should "support ValDefs" in {
    @workflow(option) val x = "qwe" + Some(4)
    x should equal (Some("qwe4"))
  }

  it should "work with type args as well" in {
    @workflow[Option] val x = Some(1) + Some(4) * 2
    x should equal (Some(9))
  }

  "@context" should "support DefDefs" in {
    @context(option) def test = {
      val foo = "bar"
      $( "asd" + Some(3) )
    }
    test should equal (Some("asd3"))
  }

  it should "support ValDefs" in {
    @context(option) val x = {
      val bar = "baz"
      $( "qwe" + Some(4) )
    }
    x should equal (Some("qwe4"))
  }

  it should "support ModuleDefs" in {
    @context(option) object test {
      $("asd" + Some(3)) should equal (Some("asd3"))
      def ping = 3
    }
    test.ping
  }

  it should "support type args as well" in {
    @context[List] val x = $(List(2, 3) * 2)
    x should equal (List(4, 6))
  }
}