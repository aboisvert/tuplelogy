package typology

import Tuplelogy.{mapByName, update, update_!, ^}
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

  test("can append a field with ++ on named tuples (standard library)"):
    val t1 = (name = "Alex", age = 42)
    val t2 = t1 ++ (city = "Chicago")
    assert(t2 == (name = "Alex", age = 42, city = "Chicago"))
    assert(t2.name == "Alex" && t2.age == 42 && t2.city == "Chicago")

  test("can append a field with `with`[label] := value"):
    val t1 = (name = "Alex", age = 42)
    val t2 = t1("city") = "Chicago"
    assert(t2 == (name = "Alex", age = 42, city = "Chicago"))
    assert(t2.city == "Chicago")

  test("reports error when `update` appends a name already present"):
    expectCompileError("cannot update — that name is already present in this named tuple"):
      """{
      val t1 = (name = "Alex", age = 42)
      t1("age") = 99
      }"""

  test("can replace an existing field with `update_!`"):
    val t1 = (name = "Alex", age = 42)
    val t2 = t1.update_!("age")(99)
    assert(t2 == (name = "Alex", age = 99))
    assert(t2.name == "Alex" && t2.age == 99)

  test("reports error when `update_!` targets a name not in the tuple"):
    expectCompileError("that name is not present in this named tuple"):
      """{
      val t1 = (name = "Alex", age = 42)
      t1.update_!("city")("Chicago")
      }"""

  test("Accessor ~ accesses the first field by default"):
    val t1 = (name = "Alex", age = 42)
    assert(t1.^ == "Alex")

  test("Accessor ~ accesses combined with method call"):
    val t1 = (name = "Alex", age = 42)
    assert(t1.^.toString == "Alex")

end TuplelogySuite