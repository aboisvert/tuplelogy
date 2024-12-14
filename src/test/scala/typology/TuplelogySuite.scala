package typology

import Tuplelogy.mapByName
import scala.annotation.nowarn

class TuplelogySuite extends munit.FunSuite 
  with typology.utils.CompileUtils:

  test("can reverse a named tuple"):
    val tuple = (foo = "foo", bar = 42, baz = 128.0)
    val reversed = tuple.mapByName[(baz: Double, bar: Int, foo: String)]
    assert(reversed == (baz = 128.0, bar = 42, foo = "foo"))

  test("can map to a subset"):
    val tuple = (foo = "foo", bar = 42, baz = 128.0)
    val subset = tuple.mapByName[(bar: Int, foo: String)]
    assert(subset == (bar = 42, foo = "foo"))

  test("reports error trying to map to a regular tuple"):
    @nowarn // don't warn about unused definitions due to quoted failing code
    val tuple = (foo = "foo", bar = 42, baz = 128.0)
    expectCompileError("[Target] must be a named tuple: (Int, String)"):
      """
      tuple.mapByName[(Int, String)]
      """

  test("reports error trying to map to same name but incompatible type"):
    @nowarn 
    val tuple = (foo = "foo", bar = 42, baz = 128.0)
    expectCompileError(
      """
      | Cannot mapByName from 
      | NamedTuple[
      |   (("foo" : String), ("bar" : String), ("baz" : String)), (String, Int, Double)
      | ] to (foo : Int, bar : String)
      """.stripMargin
    ):
      """
      tuple.mapByName[(foo: Int, bar: String)]
      """

  test(
    "reports error trying to map to a named tuple with fields not present in source"
  ):
    @nowarn 
    val tuple = (foo = "foo", bar = 42, baz = 128.0)
    expectCompileError("Cannot mapByName from NamedTuple [...]"):
      """
      tuple.mapByName[(foo: Int, bar: String, missing: Double)]
      """
