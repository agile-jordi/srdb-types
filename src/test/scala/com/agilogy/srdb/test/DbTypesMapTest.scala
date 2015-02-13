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
    implicit val nameDbType = DbType.notNull[String].xmap[Name](Name.apply,_.v)
    inSequence{
      (ps.setString _).expects(1,"Jane")
      (rs.getString(_:Int)).expects(1).returning("John")
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(Name("Jane"))
    assert(db.read(reader[Name]) === Name("John"))
  }
  
  behavior of "named db types xmap"

  it should "create a new db type mapping over a function" in {
    implicit val nameDbType = reader[String]("c").xmap[Name](Name.apply,_.v)
    inSequence{
      (ps.setString _).expects(1,"Jane")
      
      (rs.getString(_:String)).expects("c").returning("John")
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(Name("Jane"))
    assert(db.read(nameDbType) === Name("John"))
  }
  
  behavior of "combined positional db types xmap"
  
  case class Person(name:String, age:Int)

  it should "create a new db type mapping over a function" in {
    implicit val personReader = dbType[(String,Int)].xmap[Person]({case ((n,a)) => Person(n,a)},p => p.name -> p.age)
    inSequence{
      (ps.setString _).expects(1,"Jane")
      (ps.setInt _).expects(2,25)
      (rs.getString(_:Int)).expects(1).returning("John")
      (rs.wasNull _).expects().returning(false)
      (rs.getInt(_:Int)).expects(2).returning(23)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(Person("Jane",25))
    assert(db.read(reader[Person]) === Person("John",23))
  }

  behavior of "combined named db types xmap"

  it should "create a new db type mapping over a function" in {
    implicit val personReader = combine(reader[String]("name"),reader[Int]("age")).xmap[Person]({case ((n,a)) => Person(n,a)},p => p.name -> p.age)
    inSequence{
      (ps.setString _).expects(1,"Jane")
      (ps.setInt _).expects(2,25)
      (rs.getString(_:String)).expects("name").returning("John")
      (rs.wasNull _).expects().returning(false)
      (rs.getInt(_:String)).expects("age").returning(23)
      (rs.wasNull _).expects().returning(false)
    }
    db.prepare(Person("Jane",25))
    assert(db.read(personReader) === Person("John",23))
  }

}
