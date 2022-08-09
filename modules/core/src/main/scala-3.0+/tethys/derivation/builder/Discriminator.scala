package tethys.derivation.builder

import scala.annotation.compileTimeOnly

sealed trait Discriminator[A]:
  def byClass(label: String): Discriminator[A]
  def by[B](f: A => B): Discriminator[A]

object Discriminator:
  @compileTimeOnly("Discriminator should be defined in derived method only")
  def apply[A]: Discriminator[A] = throw new NotDescribedException



