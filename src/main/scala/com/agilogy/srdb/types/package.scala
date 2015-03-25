package com.agilogy.srdb

import java.sql.{ ResultSet, PreparedStatement }

import scala.language.implicitConversions

package object types extends ColumnTypeInstances with DbTypeCombinators {

  implicit def notNullView[T: ColumnType]: NotNullAtomicDbType[T] = NotNullAtomicDbType[T]

  implicit def optionalView[T: ColumnType]: OptionalAtomicDbType[T] = OptionalAtomicDbType[T]

  def notNull[T: ColumnType](name: String): NotNullAtomicNamedDbReader[T] = NotNullAtomicNamedDbReader[T](name)

  def optional[T: ColumnType](name: String): OptionalAtomicNamedReader[T] = OptionalAtomicNamedReader[T](name)

  def set[T: DbWriter](ps: PreparedStatement, value: T): Unit = implicitly[DbWriter[T]].set(ps, value)
  def set[T: DbWriter](ps: PreparedStatement, pos: Int, value: T): Unit = implicitly[DbWriter[T]].set(ps, pos, value)
  def get[T: DbReader](rs: ResultSet): T = implicitly[DbReader[T]].get(rs)

  implicit def argument[T: DbType](v: T): (PreparedStatement, Int) => Unit = {
    (ps, pos) =>
      implicitly[DbType[T]].set(ps, pos, v)
  }

}
