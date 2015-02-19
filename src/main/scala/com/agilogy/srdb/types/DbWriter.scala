package com.agilogy.srdb.types

import java.sql.PreparedStatement

trait DbWriter[T] {

  self =>

  val length: Int

  def set(ps: PreparedStatement, value: T): Unit = set(ps,1,value)

  def set(ps: PreparedStatement, pos:Int, value: T): Unit

  def contramap[T2](xf: (T2) => T): DbWriter[T2] = new DbWriter[T2] {

    override def set(ps: PreparedStatement, pos:Int, value: T2): Unit = self.set(ps,pos,xf(value))

    override val length: Int = self.length
  }

}

