package matching.transition

import enumeratum.Enum
import enumeratum.EnumEntry

sealed trait BacktrackMethod extends EnumEntry

object BacktrackMethod extends Enum[BacktrackMethod] {

  def values = findValues

  case object Lookahead extends BacktrackMethod
  case object SubsetPrune extends BacktrackMethod
  case object Nondeterminism extends BacktrackMethod

  case object BDM extends BacktrackMethod
  case object KM extends BacktrackMethod
}
