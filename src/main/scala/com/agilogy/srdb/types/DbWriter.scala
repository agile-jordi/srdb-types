package com.agilogy.srdb.types

import java.sql.PreparedStatement

/**
 * A writer capable of setting one or more parameters in a `PreparedStatement` to pass an instance of `T`
 * @tparam T The Scala type representing the parameter or parameters to set in the `PreparedStatement`
 * @group API
 */
trait DbWriter[T] {

  self =>

  def length(value: T): Int

  def set(ps: PreparedStatement, value: T): Unit = set(ps, 1, value)

  def set(ps: PreparedStatement, pos: Int, value: T): Unit

  def contraMap[T2](f: (T2) => T): DbWriter[T2]

}

trait DbWriterOps {

  self =>

  implicit def argument[T: DbWriter](v: T): (PreparedStatement, Int) => Unit = {
    (ps, pos) =>
      implicitly[DbWriter[T]].set(ps, pos, v)
  }

  implicit class PreparedStatementOps(ps: PreparedStatement) {
    def set[T: DbWriter](value: T): Unit = {
      self.set(ps, value)
    }

    def set[T: DbWriter](pos: Int, value: T): Unit = {
      self.set(ps, pos, value)
    }
  }

  /**
   * Sets a `T` parameter in a `PreparedStatement` using a [[DbWriter]].
   *
   * Equivalent to `set(ps,1,value)`.
   *
   * Depending on the [[DbWriter]] being used, one or more parameters of the `PreparedStatement` will be set.
   *
   * @param ps The `PreparedStatement`
   * @param value The value to set
   * @tparam T The Scala type of the value to be set
   * @group API
   */
  def set[T: DbWriter](ps: PreparedStatement, value: T): Unit = implicitly[DbWriter[T]].set(ps, value)

  /**
   * Sets a `T` parameter in a `PreparedStatement`, starting at `pos` using a [[DbWriter]].
   *
   * As in JDBC, `pos` is 1-based, so 1 is the first parameter
   * Depending on the [[DbWriter]] being used, one or more parameters of the `PreparedStatement` will be set. If so,
   * the parameters are set starting at parameter `pos`.
   *
   * @param ps The `PreparedStatement`
   * @param pos The parameter to start at
   * @param value The value to set
   * @tparam T The Scala type of the value to be set
   * @group API
   */
  def set[T: DbWriter](ps: PreparedStatement, pos: Int, value: T): Unit = implicitly[DbWriter[T]].set(ps, pos, value)

}

trait AtomicDbWriter[T] extends DbWriter[T] {
  override def length(value: T): Int = 1

  override def contraMap[T2](cmf: (T2) => T): AtomicDbWriter[T2] = MappedAtomicDbWriter[T, T2](this, cmf)
}

case class MappedAtomicDbWriter[T1, T2](source: DbWriter[T1], cmf: T2 => T1) extends AtomicDbWriter[T2] {

  override def set(ps: PreparedStatement, pos: Int, value: T2): Unit = source.set(ps, pos, cmf(value))

  override def contraMap[T3](f: (T3) => T2): MappedAtomicDbWriter[T1, T3] = MappedAtomicDbWriter[T1, T3](source, f.andThen(cmf))
}

case class AtomicNotNullDbWriter[T]()(implicit columnType: ColumnType[T]) extends AtomicDbWriter[T] {

  type NotNull = T
  type Optional = Option[T]

  override def set(ps: PreparedStatement, pos: Int, value: T): Unit = columnType.set(ps, pos, Some(value))

  def optional: AtomicOptionalDbWriter[T] = AtomicOptionalDbWriter[T]
  def notNull: AtomicDbWriter[T] = this
}

case class AtomicOptionalDbWriter[T]()(implicit columnType: ColumnType[T]) extends AtomicDbWriter[Option[T]] {

  type NotNull = T
  type Optional = Option[T]

  override def set(ps: PreparedStatement, pos: Int, value: Option[T]): Unit = columnType.set(ps, pos, value)

  def notNull: AtomicNotNullDbWriter[T] = AtomicNotNullDbWriter[T]
  def optional: AtomicDbWriter[Option[T]] = this
}

trait CombinedDbWriter[T] extends DbWriter[T] {
  protected val t0 = HasLength0

  override def contraMap[T2](cmf: (T2) => T): CombinedDbWriter[T2] = CombinedMappedDbWriter[T, T2](this, cmf)
}

case class CombinedMappedDbWriter[T1, T2](source: DbWriter[T1], cmf: T2 => T1) extends CombinedDbWriter[T2] {

  override def length(value: T2): Int = source.length(cmf(value))

  override def set(ps: PreparedStatement, pos: Int, value: T2): Unit = source.set(ps, pos, cmf(value))

  override def contraMap[T3](f: (T3) => T2): CombinedMappedDbWriter[T1, T3] = CombinedMappedDbWriter(source, f.andThen(cmf))

}
