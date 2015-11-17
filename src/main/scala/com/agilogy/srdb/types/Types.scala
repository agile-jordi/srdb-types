package com.agilogy.srdb.types

trait Types extends ColumnTypeInstances with DbTypeCombinators {

  implicit def reader1[T: ColumnType]: AtomicNotNullPositionalDbReader[T] = AtomicNotNullPositionalDbReader[T]

  //  implicit def optionalReader1[T: ColumnType]: AtomicOptionalPositionalDbReader[T] = AtomicOptionalPositionalDbReader[T]

  def reader[T: ColumnType]: AtomicNotNullPositionalDbReader[T] = reader1[T]

  implicit def optionalReader[T: AtomicNotNullPositionalDbReader]: AtomicOptionalPositionalDbReader[T] = implicitly[AtomicNotNullPositionalDbReader[T]].optional

  implicit def optionalCombinedReader[T: CombinedNotNullPositionalDbReader]: CombinedOptionalPositionalDbReader[T] =
    implicitly[CombinedNotNullPositionalDbReader[T]].optional

  implicit def writer1[T: ColumnType]: AtomicNotNullDbWriter[T] = AtomicNotNullDbWriter[T]

  def writer[T: ColumnType]: AtomicNotNullDbWriter[T] = writer1[T]

  implicit def optionalWriter[T: ColumnType]: AtomicOptionalDbWriter[T] = AtomicOptionalDbWriter[T]

  /**
   * Returns a [[NotNullNamedDbReader]] that reads a column with the given name using the implicit [[ColumnType]] for `T`
   * @tparam T The Scala type for which the [[NotNullNamedDbReader]] is to be returned
   * @group API
   */
  def notNull[T: ColumnType](name: String): AtomicNotNullNamedDbReader[T] = AtomicNotNullNamedDbReader[T](name)

  /**
   * Returns an [[OptionalNamedDbReader]] that reads a column with the given name using the implicit [[ColumnType]] for `T`
   * @tparam T The Scala type for which the [[OptionalNamedDbReader]] is to be returned
   * @group API
   */
  def optional[T: ColumnType](name: String): AtomicOptionalNamedDbReader[T] = AtomicOptionalNamedDbReader[T](name)

}

