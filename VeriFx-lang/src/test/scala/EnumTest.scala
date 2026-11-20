import org.scalatest.FlatSpec
import org.verifx.Compiler.Plugins.{JavaScriptCompilerPlugin, ScalaCompilerPlugin, Z3CompilerPlugin}
import org.verifx.Compiler.ProgramCompiler
import org.verifx.{ParseError, TypeError}

class EnumTest extends FlatSpec {
  "Enumerations" should "be supported" in {
    val enumeration =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}""".stripMargin

    assert(ProgramCompiler(enumeration).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    val enumeration2 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V)
        |  }
        |}""".stripMargin

    assert(ProgramCompiler(enumeration2).compileToLanguage(new ScalaCompilerPlugin)._2 != "")
  }

  it should "compile to valid Scala without type parameters" in {
    val enumeration =
      """object Mode_enum {
        |  enum Mode {
        |    On() | Off()
        |  }
        |}""".stripMargin

    val scalaCode = ProgramCompiler(enumeration).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(scalaCode.contains("case class On() extends Mode "))
    assert(!scalaCode.contains("Mode[]"))
  }

  it should "compile a pattern match that is an operand to valid Scala" in {
    val program =
      """object Mode_enum {
        |  enum Mode {
        |    On() | Off()
        |  }
        |}
        |
        |class Foo(mode: Mode, flag: Boolean) {
        |  def isOn() = this.flag && (this.mode match {
        |    case On() => true
        |    case _ => false
        |  })
        |}""".stripMargin

    // `match` binds weaker than `&&`, so the pattern match needs parentheses
    val scalaCode = ProgramCompiler(program).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(scalaCode.contains("(this.flag && (this.mode match {"))
  }

  it should "have the enumeration's type if branches return different constructors" in {
    val program =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Foo {
        |  def declared(x: Int): Op[String] = if (x > 0) new Ins[String](x, "a") else new Del[String](x)
        |  def inferred(x: Int) = if (x > 0) new Ins[String](x, "a") else if (x < 0) new Del[String](x) else new Id[String]()
        |  def matched(op: Op[String]) = op match {
        |    case Id() => new Del[String](0)
        |    case _ => new Id[String]()
        |  }
        |  def sameCtor(x: Int) = if (x > 0) new Del[String](1) else new Del[String](2)
        |  def sameCtorField(x: Int) = this.sameCtor(x).pos
        |}""".stripMargin

    // Constructors of one enumeration are subtypes of each other, which must not make the first branch's constructor the type
    val scalaCode = ProgramCompiler(program).compileToLanguage(new ScalaCompilerPlugin)._2
    assert(scalaCode.contains("def declared(x: Int): Op[String] ="))
    assert(scalaCode.contains("def inferred(x: Int): Op[String] ="))
    assert(scalaCode.contains("def matched(op: Op[String]): Op[String] ="))
    // Branches with the same constructor keep it, so that its fields remain accessible
    assert(scalaCode.contains("def sameCtor(x: Int): Del[String] ="))
  }

  it should "support type casts" in {
    // We should be able to type cast an enumeration type to a specific data constructor
    val correctTypeCast1 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Foo {
        |  def getC(d: Op[String]) = d.asInstanceOf[Del[String]].pos
        |}""".stripMargin

    assert(ProgramCompiler(correctTypeCast1).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // We should be able to type cast an data constructor to its enumeration type
    val correctTypeCast2 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Foo {
        |  def getC(i: Ins[String]) = i.asInstanceOf[Op[String]]
        |}""".stripMargin

    assert(ProgramCompiler(correctTypeCast2).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // We should be able to type cast a wrapped data constructor to its wrapped enumeration type
    val correctTypeCast3 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Foo {
        |  def getC(i: Set[Op[String]]) = i.asInstanceOf[Set[Ins[String]]]
        |}""".stripMargin

    assert(ProgramCompiler(correctTypeCast3).compileToLanguage(new ScalaCompilerPlugin)._2 != "")

    // Objects that are not enumerations should not support type casts
    val wrongTypeCast1 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Test[V]
        |
        |class Foo {
        |  def getC(i: Test[String]) = i.asInstanceOf[Op[String]]
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongTypeCast1).compileToLanguage(new ScalaCompilerPlugin)
    }

    val wrongTypeCast2 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Test[V]
        |
        |class Foo {
        |  def getC(i: Test[String]) = i.asInstanceOf[Ins[String]]
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongTypeCast2).compileToLanguage(new ScalaCompilerPlugin)
    }

    // It should not be possible to type cast to a type that is not an enumeration or data constructor
    val wrongTypeCast3 =
      """object Op_enum {
        |  enum Op[V] {
        |    Ins(pos: Int, c: V) | Del(pos: Int) | Id()
        |  }
        |}
        |
        |class Test[V]
        |
        |class Foo {
        |  def getC(i: Op[String]) = i.asInstanceOf[String]
        |}""".stripMargin

    assertThrows[TypeError] {
      ProgramCompiler(wrongTypeCast3).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "throw an error if syntax is wrong" in {
    val enumeration =
      """object Op_enum {
        |  enum Op[V] {
        |    class Ins(pos: Int, c: V)
        |  }
        |}""".stripMargin

    assertThrows[ParseError] {
      ProgramCompiler(enumeration).compileToLanguage(new ScalaCompilerPlugin)
    }
  }

  it should "transpile to Z3" in {
    val enumeration =
      """object Op_enum {
        |  enum Op[V, W] {
        |    Ins(pos: Int, c: V, y: W) | Del(pos: Int, y: W) | Id()
        |  }
        |}
        |
        |class Test {
        |  def foo(): Ins[String, Int] = {
        |    new Ins(5, "abc", 9)
        |  }
        |  def bar(): Del[String, Boolean] = {
        |    new Del[String, Boolean](6, true)
        |  }
        |}
        |""".stripMargin

    ProgramCompiler(enumeration).compileToLanguage(new Z3CompilerPlugin)._2
  }
}
