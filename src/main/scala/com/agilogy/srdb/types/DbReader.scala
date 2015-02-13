package com.agilogy.srdb.types

import java.sql.ResultSet


trait DbReader[T] {

  self =>

  def as(name:String) = NamedDbReader(this,name)

  val length: Int

  def get(rs: ResultSet): T = get(rs,1)

  def get(rs: ResultSet, pos: Int): T

  def get(rs: ResultSet, name: String): T

  def map[T2](f: T => T2): DbReader[T2] = new DbReader[T2] {

    override val length: Int = self.length

    override def get(rs: ResultSet): T2 = f(self.get(rs))

    override def get(rs: ResultSet, pos: Int): T2 = f(self.get(rs, pos))

    override def get(rs: ResultSet, name: String): T2 = f(self.get(rs, name))

  }

}

case class NamedDbReader[T](dbReader: DbReader[T], as: String) extends DbReader[T] {

  self =>

  override val length: Int = dbReader.length

  override def get(rs: ResultSet): T = get(rs, "")

  def get(rs: ResultSet, name: String): T = dbReader.get(rs, as + name)

  override def get(rs: ResultSet, pos: Int): T = dbReader.get(rs,pos)
}

trait DbReaderImplicits{

  implicit def combinedReader[T1:DbReader,T2:DbReader]:DbReader[(T1,T2)] = new DbReader[(T1,T2)] {

    private val t1 = implicitly[DbReader[T1]]
    private val t2 = implicitly[DbReader[T2]]

    override val length: Int = t1.length + t2.length

    override def get(rs: ResultSet): (T1, T2) = (t1.get(rs), t2.get(rs))

    override def get(rs: ResultSet, name: String): (T1, T2) = (t1.get(rs,name),t2.get(rs,name))

    override def get(rs: ResultSet, pos: Int): (T1, T2) = (t1.get(rs,pos),t2.get(rs,pos + t1.length))

  }

}