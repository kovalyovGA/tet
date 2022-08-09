package tethys.derivation.builder

case class ReaderDerivationConfig(fieldStyle: Option[FieldStyle],
                                  isStrict: Boolean) {
  def withFieldStyle(fieldStyle: FieldStyle): ReaderDerivationConfig = this.copy(fieldStyle = Some(fieldStyle))
  def strict: ReaderDerivationConfig = this.copy(isStrict = true)
}

object ReaderDerivationConfig {
  def empty: ReaderDerivationConfig = ReaderDerivationConfig(
    fieldStyle = None,
    isStrict = false
  )
  def withFieldStyle(fieldStyle: FieldStyle): ReaderDerivationConfig = empty.withFieldStyle(fieldStyle)
  def strict: ReaderDerivationConfig = empty.strict
}
