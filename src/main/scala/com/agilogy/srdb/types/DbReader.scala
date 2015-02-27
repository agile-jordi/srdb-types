package com.agilogy.srdb.types

import java.sql.ResultSet

trait DbReader[T] {

  self =>

  def get(rs: ResultSet): T
  
  def map[T2](f: T => T2): DbReader[T2]

}

trait PositionalDbReader[T] extends DbReader[T]{
  
  self =>

  val length: Int

  def get(rs: ResultSet): T = get(rs,1)
  def get(rs: ResultSet, pos:Int): T

  override def map[T2](f: (T) => T2): PositionalDbReader[T2] = new PositionalDbReader[T2] {
    
    override val length: Int = self.length

    override def get(rs: ResultSet): T2 = f(self.get(rs))
    override def get(rs: ResultSet, pos:Int): T2 = f(self.get(rs,pos))
  }
}

trait NamedDbReader[T] extends DbReader[T] {
  
  self =>

  override def get(rs: ResultSet): T

  override def map[T2](f: (T) => T2): NamedDbReader[T2] = new NamedDbReader[T2] {

    override def get(rs: ResultSet): T2 = f(self.get(rs))

  }
}

case class NotNullAtomicNamedDbReader[T:ColumnType](name:String) extends NamedDbReader[T] {

  override def get(rs: ResultSet): T = implicitly[ColumnType[T]].get(rs,name).getOrElse(throw new NullColumnReadException)
}

case class OptionalAtomicNamedReader[T:ColumnType](name:String) extends NamedDbReader[Option[T]]{

  override def get(rs: ResultSet): Option[T] = implicitly[ColumnType[T]].get(rs,name)
}