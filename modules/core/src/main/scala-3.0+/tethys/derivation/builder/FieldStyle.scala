package tethys.derivation.builder

import java.util.regex.Pattern
import scala.annotation.StaticAnnotation

private[builder] object FieldStyleUtils:
  private val regexp1: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val regexp2: Pattern = Pattern.compile("([a-z\\d])([A-Z])")
  private val replacement: String = "$1_$2"

  def splitName(name: String): List[String] =
    val first = regexp1.matcher(name).replaceAll(replacement)
    regexp2.matcher(first).replaceAll(replacement).split("_").toList


enum FieldStyle(val applyStyle: String => String):
  def apply(s: String): String = applyStyle(s)

  case SnakeCase extends FieldStyle(FieldStyleUtils.splitName(_).mkString("_"))
  case KebabCase extends FieldStyle(FieldStyleUtils.splitName(_).mkString("-"))

  case LowerCase extends FieldStyle(_.toLowerCase)
  case UpperCase extends FieldStyle(_.toUpperCase)

  case LowerSnakeCase extends FieldStyle(
    FieldStyle.SnakeCase.apply andThen FieldStyle.LowerCase.apply
  )
  case UpperSnakeCase extends FieldStyle(
    FieldStyle.SnakeCase.apply andThen FieldStyle.UpperCase.apply
  )

  case LowerKebabCase extends FieldStyle(
    FieldStyle.KebabCase.apply andThen FieldStyle.LowerCase.apply
  )
  case UpperKebabCase extends FieldStyle(
    FieldStyle.KebabCase.apply andThen FieldStyle.UpperCase.apply
  )

  @deprecated("use FieldStyle.SnakeCase")
  case snakecase extends FieldStyle(FieldStyleUtils.splitName(_).mkString("_"))
  @deprecated("use FieldStyle.KebabCase")
  case kebabcase extends FieldStyle(FieldStyleUtils.splitName(_).mkString("-"))
  @deprecated("use FieldStyle.LowerCase")
  case lowercase extends FieldStyle(_.toLowerCase)
  @deprecated("use FieldStyle.UpperCase")
  case uppercase extends FieldStyle(_.toUpperCase)
  @deprecated("use FieldStyle.LowerSnakeCase")
  case lowerSnakecase extends FieldStyle(
    FieldStyle.SnakeCase.apply andThen FieldStyle.LowerCase.apply
  )
  @deprecated("use FieldStyle.UpperSnakeCase")
  case upperSnakecase extends FieldStyle(
    FieldStyle.SnakeCase.apply andThen FieldStyle.UpperCase.apply
  )

  @deprecated("use FieldStyle.LowerKebabCase")
  case lowerKebabcase extends FieldStyle(
    FieldStyle.KebabCase.apply andThen FieldStyle.LowerCase.apply
  )

  @deprecated("use FieldStyle.UpperKebabCase")
  case upperKebabcase extends FieldStyle(
    FieldStyle.KebabCase.apply andThen FieldStyle.UpperCase.apply
  )
