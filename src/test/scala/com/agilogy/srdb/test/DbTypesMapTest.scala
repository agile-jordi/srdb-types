package com.agilogy.srdb.test

import java.sql.{ResultSet, PreparedStatement}

import com.agilogy.srdb.types
import org.scalamock.scalatest.MockFactory
import org.scalatest.FlatSpec

class DbTypesMapTest extends FlatSpec with MockFactory{

  import types._

  val ps = mock[PreparedStatement]
  val rs = mock[ResultSet]
  val db = new DummyDb(ps, rs)

  behavior of "atomic db types xmap"
  
  case class Name(v:String)
  
  it should "create a new db type mapping over a function" in {
    implicit val nameDbType = notNull[String].xmap[Name](Name.apply,_.v)
    inSequence{
      (ps.setString _).expects(1,"Jane")
      (rs.getString(_:Int)).expects(1).returning("John")
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(Name("Jane"))
    assert(db.read(rsReader[Name]) === Name("John"))
  }
  
  behavior of "named db types xmap"

  it should "create a new db type mapping over a function" in {
    implicit val nameDbType: ColumnType[Name] = notNull[String].xmap[Name](Name.apply,_.v)
    implicit val nameReader = nameDbType("name")
    inSequence{
      (ps.setString _).expects(1,"Jane")
      (rs.getString(_:String)).expects("name").returning("John")
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(Name("Jane"))
    assert(db.read(nameReader) === Name("John"))
  }
  
  behavior of "combined db types xmap"
  
  case class Person(name:String, age:Int)
  

//  it should "create a new db type mapping over a function" in {
//    implicit val personDbType = dbType[(String,Int)].xmap[Person](Person.tupled,Person.unapply(_).get)
//    inSequence{
//      (ps.setString _).expects(1,"Jane")
//      (ps.setInt _).expects(2,25)
//      (rs.getString(_:Int)).expects(1).returning("John")
//      (rs.wasNull _).expects().returning(false)
//      (rs.getInt(_:Int)).expects(2).returning(23)
//      (rs.wasNull _).expects().returning(false)
//    }
//    db.prepare(Person("Jane",25))
//    assert(db.read(rsReader[Person]) === Person("John",23))
//  }

  behavior of "combined named db readers map"

  it should "create a new db type mapping over a function" in {
    implicit val personReader = reader(DbString.notNull("name"),DbInt.notNull("age")).map[Person]((Person.apply _).tupled)
    inSequence{
      (rs.getString(_:String)).expects("name").returning("John")
      (rs.wasNull _).expects().returning(false)
      (rs.getInt(_:String)).expects("age").returning(23)
      (rs.wasNull _).expects().returning(false)
    }
    assert(db.read(personReader) === Person("John",23))
  }

  behavior of "combined positional db readers"

  def read[T:DbReader,T2](rs:ResultSet, f:T => T2):T2 = implicitly[DbReader[T]].map(f).get(rs)

  it should "read using the new read function" in{
    (rs.getString(_:Int)).expects(1).returning("John")
    (rs.wasNull _).expects().returning(false)
    (rs.getInt(_:Int)).expects(2).returning(23)
    (rs.wasNull _).expects().returning(false)

    assert(read(rs, (Person.apply _).tupled) === Person("John",23))

  }
  
}
