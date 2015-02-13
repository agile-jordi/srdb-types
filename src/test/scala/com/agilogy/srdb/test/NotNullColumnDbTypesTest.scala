package com.agilogy.srdb.test

import java.sql._
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class NotNullColumnDbTypesTest extends FlatSpec with MockFactory {

  val ps = mock[PreparedStatement]
  val rs = mock[ResultSet]
  val db = new DummyDb(ps, rs)

  behavior of "not null DbType implicit conversion"

  import com.agilogy.srdb.types._
  import DbType._


  it should "prepare statements with a Byte param and read resultsets with a Byte column" in {
    inSequence {
      (ps.setByte(_, _)).expects(1, 3.toByte)
      (rs.getByte(_: Int)).expects(1).returning(3.toByte)
      (rs.wasNull _).expects().returning(false)
      (rs.getByte(_: String)).expects("c").returning(3.toByte)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(3.toByte)
    db.read(reader[Byte])
    db.read(reader[Byte]("c"))
  }

  it should "prepare statements with a Short param and read resultsets with a Short column" in {
    inSequence {
      (ps.setShort(_, _)).expects(1, 3.toShort)
      (rs.getShort(_: Int)).expects(1).returning(3.toShort)
      (rs.wasNull _).expects().returning(false)
      (rs.getShort(_: String)).expects("c").returning(3.toByte)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(3.toShort)
    db.read(reader[Short])
    db.read(reader[Short]("c"))
  }

  it should "execute selects with an Int param" in {
    inSequence {
      (ps.setInt(_, _)).expects(1, 3)
      (rs.getInt(_: Int)).expects(1).returning(3)
      (rs.wasNull _).expects().returning(false)
      (rs.getInt(_: String)).expects("c").returning(3)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(3)
    db.read(reader[Int])
    db.read(reader[Int]("c"))
  }

  it should "prepare statements with a Long param and read resultsets with a Long column" in {
    inSequence {
      (ps.setLong(_, _)).expects(1, 3l)
      (rs.getLong(_: Int)).expects(1).returning(3l)
      (rs.wasNull _).expects().returning(false)
      (rs.getLong(_: String)).expects("c").returning(3l)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(3l)
    db.read(reader[Long])
    db.read(reader[Long]("c"))
  }

  it should "prepare statements with a Float param and read resultsets with a Float column" in {
    inSequence {
      (ps.setFloat _).expects(1, 3.0f)
      (rs.getFloat(_: Int)).expects(1).returning(3.0f)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(3.0f)
    db.read(reader[Float])
  }

  it should "prepare statements with a Double param and read resultsets with a Double column" in {
    inSequence {
      (ps.setDouble _).expects(1, 3.0)
      (rs.getDouble(_: Int)).expects(1).returning(3.0)
      (rs.wasNull _).expects().returning(false)
      (rs.getDouble(_: String)).expects("c").returning(3.0)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(3.0)
    db.read(reader[Double])
    db.read(reader[Double]("c"))
  }

  it should "prepare statements with a String param and read resultsets with a String column" in {
    inSequence {
      (ps.setString _).expects(1, "hi!")
      (rs.getString(_: Int)).expects(1).returning("hi!")
      (rs.wasNull _).expects().returning(false)
      (rs.getString(_: String)).expects("c").returning("hi!")
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare("hi!")
    db.read(reader[String])
    db.read(reader[String]("c"))
  }

  it should "prepare statements with a Boolean param and read resultsets with a Boolean column" in {
    inSequence {
      (ps.setBoolean _).expects(1, false)
      (rs.getBoolean(_: Int)).expects(1).returning(false)
      (rs.wasNull _).expects().returning(false)
      (rs.getBoolean(_: String)).expects("c").returning(false)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(false)
    db.read(reader[Boolean])
    db.read(reader[Boolean]("c"))
  }

  it should "prepare statements with a java.util.Date param and read resultsets with a java.util.Date column" in {
    val d = new java.util.Date(123456l)
    val sqlDate = new java.sql.Date(d.getTime)
    inSequence {
      (ps.setDate(_: Int, _: java.sql.Date)).expects(1, sqlDate)
      (rs.getDate(_: Int)).expects(1).returning(sqlDate)
      (rs.wasNull _).expects().returning(false)
      (rs.getDate(_: String)).expects("c").returning(sqlDate)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(d)
    db.read(reader[java.util.Date])
    db.read(reader[java.util.Date]("c"))
  }

  it should "prepare statements with a BigDecimal param and read resultsets with a BigDecimal column" in {
    val value = BigDecimal("2.0")
    val javaBd = value.bigDecimal
    inSequence {
      (ps.setBigDecimal(_: Int, _: java.math.BigDecimal)).expects(1, javaBd)
      (rs.getBigDecimal(_: Int)).expects(1).returning(javaBd)
      (rs.wasNull _).expects().returning(false)
      (rs.getBigDecimal(_: String)).expects("c").returning(javaBd)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(value)
    db.read(reader[BigDecimal])
    db.read(reader[BigDecimal]("c"))
  }

}
