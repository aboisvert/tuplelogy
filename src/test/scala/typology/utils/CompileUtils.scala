package typology.utils

object CompileUtils:
  /** Compile `code` and report errors, if any */
  transparent inline def compileAndReportErrors(inline code: String): Option[String] =
    val errors = scala.compiletime.testing.typeCheckErrors(code)
    if errors.isEmpty then None
    else Some(errors.mkString("\n"))

/** Mixin for munit test suite */
trait CompileUtils:
  self: munit.Assertions =>

  transparent inline def expectCompileError(subMessage: String)(inline code: String): Unit =
    CompileUtils.compileAndReportErrors(code) match
      case None =>
        throw new Exception("Expected a compile error but no errors were reported")
      case Some(errors) =>
        /** Ignore all differences in whitespace, newlines, and ellipsis [...] */
        def clean(str: String) = 
          str.trim
            .replaceAll("\\s", "")
            .replaceAll("\n", "")
            .replace("[...]", "")
        val expected = clean(subMessage)
        // println("Expected:\n" + expected)
        val actual = clean(errors)
        // println("Actual:\n" + actual)
        assert(
          actual.contains(expected),
          clue = "Error messages differ:\n" + actual
        )
