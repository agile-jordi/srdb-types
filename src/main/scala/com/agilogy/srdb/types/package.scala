package com.agilogy.srdb

import java.sql.{ ResultSet, PreparedStatement }

import scala.language.implicitConversions

package object types extends ColumnTypeInstances with DbTypeCombinators {

  /**
   * Exposes an implicit [[NotNullAtomicDbType]] for each [[ColumnType]] implicitly available
   * @tparam T The Scala type for which the [[NotNullAtomicDbType]] is to be exposed
   * @group API
   */
  implicit def notNullView[T: ColumnType]: NotNullAtomicDbType[T] = NotNullAtomicDbType[T]

  /**
   * Exposes an implicit [[OptionalAtomicDbType]] for each [[ColumnType]] implicitly available
   * @tparam T The Scala type for which the [[OptionalAtomicDbType]] is to be exposed
   * @group API
   */
  implicit def optionalView[T: ColumnType]: OptionalAtomicDbType[T] = OptionalAtomicDbType[T]

  /**
   * Returns a [[NotNullAtomicNamedDbReader]] that reads a column with the given name using the implicit [[ColumnType]] for `T`
   * @tparam T The Scala type for which the [[NotNullAtomicNamedDbReader]] is to be returned
   * @group API
   */
  def notNull[T: ColumnType](name: String): NotNullAtomicNamedDbReader[T] = NotNullAtomicNamedDbReader[T](name)

  /**
   * Returns an [[OptionalAtomicNamedReader]] that reads a column with the given name using the implicit [[ColumnType]] for `T`
   * @tparam T The Scala type for which the [[OptionalAtomicNamedReader]] is to be returned
   * @group API
   */
  def optional[T: ColumnType](name: String): OptionalAtomicNamedReader[T] = OptionalAtomicNamedReader[T](name)

  /**
   * Sets a `T` parameter in a `PreparedStatement` using a [[DbWriter]] (or [[DbType]]).
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
   * Sets a `T` parameter in a `PreparedStatement`, starting at `pos` using a [[DbWriter]] (or [[DbType]]).
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

  /**
   * Reads a value from a `ResultSet` using the given [[DbReader]] (or [[DbType]]), which may read the column(s) by position or by name
   * @param rs The `ResultSet` to get the value from
   * @tparam T The Scala type of the read value
   * @return The Scala representation of one or more columns according to the [[DbReader]] used
   * @group API
   */
  def get[T: DbReader](rs: ResultSet): T = implicitly[DbReader[T]].get(rs)

  /** @group API */
  implicit def argument[T: DbType](v: T): (PreparedStatement, Int) => Unit = {
    (ps, pos) =>
      implicitly[DbType[T]].set(ps, pos, v)
  }

}
