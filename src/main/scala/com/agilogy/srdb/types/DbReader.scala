package com.agilogy.srdb.types

import java.sql.ResultSet

import scala.util.control.NonFatal

/**
 * A type class of readers of instances of type `T`.
 *
 * Given a `ResultSet`, a DbReader may read any number of columns and return an instance of `T`
 *
 * It MUST NOT call `ResultSet.next`
 *
 * An [[AtomicNotNullPositionalDbReader]] and an [[AtomicOptionalPositionalDbReader]], which are subclasses of
 * [[DbReader]] are implicitly available for every [[ColumnType]]
 *
 * @tparam T The Scala class returned when reading from the `ResultSet`
 * @group API
 */
trait DbReader[T] {

  def get(rs: ResultSet): T

  def apply(rs: ResultSet): T = get(rs)

  def map[T2](f: T => T2): DbReader[T2]

}

trait DbReaderOps {

  self =>

  implicit class DbReaderResultSetOps(rs: ResultSet) {

    def get[T: AtomicPositionalDbReader](pos: Int): T = self.get[T](pos)(rs)
    def get[T: AtomicPositionalDbReader](name: String): T = self.get[T](name)(rs)
  }

  /**
   * Reads a value from a `ResultSet` using the given [[DbReader]], which may read the column(s) by position or by name
   * @param rs The `ResultSet` to get the value from
   * @tparam T The Scala type of the read value
   * @return The Scala representation of one or more columns according to the [[DbReader]] used
   * @group API
   */
  def get[T: DbReader](rs: ResultSet): T = implicitly[DbReader[T]].get(rs)
  def get[T: AtomicPositionalDbReader](pos: Int)(rs: ResultSet): T = implicitly[PositionalDbReader[T]].get(rs, pos)
  def get[T: AtomicPositionalDbReader](name: String)(rs: ResultSet): T = implicitly[AtomicPositionalDbReader[T]].as(name).get(rs)

}

//trait NotNullDbReader[T] extends DbReader[T] {
//
//  type NotNull = T
//  type Optional = Option[T]
//
//  def optional: OptionalDbReader[T]
//
//}
//
//trait OptionalDbReader[T] extends DbReader[Option[T]] {
//
//  type NotNull = T
//  type Optional = Option[T]
//
//  def notNull: NotNullDbReader[T]
//}