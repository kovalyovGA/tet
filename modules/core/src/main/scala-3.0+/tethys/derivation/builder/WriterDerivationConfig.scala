package tethys.derivation.builder

@deprecated("use field style directly or on a writer builder, use discriminator directly")
case class WriterDerivationConfig(fieldStyle: Option[FieldStyle], discriminator: Option[String]) {
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig = this.copy(fieldStyle = Some(fieldStyle))
  def withDiscriminator(discriminator: String): WriterDerivationConfig = this.copy(discriminator = Some(discriminator))
}

@deprecated("use field style and discriminator directly")
object WriterDerivationConfig {
  def empty: WriterDerivationConfig =
    WriterDerivationConfig(fieldStyle = None, discriminator = None)
    
  def withFieldStyle(fieldStyle: FieldStyle): WriterDerivationConfig =
    WriterDerivationConfig(fieldStyle = Some(fieldStyle), discriminator = None)
    
  def withDiscriminator(discriminator: String): WriterDerivationConfig =
    WriterDerivationConfig(fieldStyle = None, discriminator = Some(discriminator))
}
