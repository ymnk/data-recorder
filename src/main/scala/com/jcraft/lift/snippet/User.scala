/*
 * Copyright 2009 ymnk, JCraft,Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 */
package com.jcraft.lift.snippet

import scala.xml.{NodeSeq,Text}

import net.liftweb.http.{RequestVar,S,SHtml}
import net.liftweb.util.{Helpers,Log}
import S._
import Helpers._

import _root_.com.jcraft.lift.model._
import _root_.com.jcraft.lift.model.Model._
import _root_.org.scala_libs.jdo._
import _root_.org.scala_libs.jdo.criterion._

class UserOps {

  def list (xhtml : NodeSeq) : NodeSeq = {
    val users = Model.withPM{ from(_, classOf[User]).resultList }

    def findDataByUser(a:User) = {
      Model.withPM{ 
        from(_, classOf[Data])
            .where(eqC("user", a))
            .resultList 
      }
    }

    import SHtml.link
    users.flatMap(user =>
      bind("user", xhtml,
           "email" -> Text(user.email),
           "name" -> Text(user.name),
           "edit" -> link("add.html", 
                          () => userVar(user),
                          Text(?("Edit"))),
           "delete" -> link("list.html", 
                            () => Model.withPM{ _.deletePersistent(user)},
                            Text(?("Delete")))))
  }

  object userVar extends RequestVar(new User())
  lazy val user = userVar.is

  def add (xhtml : NodeSeq) : NodeSeq = {
    def doAdd () = {
      if (user.name.length == 0) {
        error("emptyUser", "The user's name cannot be blank")
      } 
      else {
        Model.withPM{ _.makePersistent(user) }
        redirectTo("/")
      }
    }

    import SHtml.{text, submit}
    bind("user", xhtml,
	 "email" -> Text(user.email),
	 "name" -> text(user.name, user.name=_),
	 "twitter" -> text(user.twitter, user.twitter=_),
	 "submit" -> submit(?("Save"), doAdd))
  }

  def edit (xhtml : NodeSeq) : NodeSeq = {

    User.currentUser map (userVar(_))

    def doAdd () = {
      if (user.name.length == 0) {
        error("emptyUser", "The user's name cannot be blank")
      } 
      else {
        Model.withPM{ _.makePersistent(user) }
        redirectTo("/")
      }
    }

    import SHtml.{text, submit}
    bind("user", xhtml,
	 "email" -> Text(user.email),
	 "name" -> text(user.name, user.name=_),
	 "twitter" -> text(user.twitter, user.twitter=_),
	 "submit" -> submit(?("Save"), doAdd))
  }
}
