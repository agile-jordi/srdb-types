package com.agilogy.srdb.types

trait DbType[T] extends DbReader[T] with DbWriter[T]{
  protected val t0 = HasLength0
}

trait NamedDbType[T] extends NamedDbReader[T] with DbType[T]

trait PositionalDbType[T] extends PositionalDbReader[T] with DbType[T]


