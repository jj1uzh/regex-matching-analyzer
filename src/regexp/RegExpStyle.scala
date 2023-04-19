package matching.regexp

import enumeratum.Enum
import enumeratum.EnumEntry

sealed trait RegExpStyle extends EnumEntry

object RegExpStyle extends Enum[RegExpStyle] {

  def values = findValues

  case object Raw extends RegExpStyle
  case object PCRE extends RegExpStyle
}
