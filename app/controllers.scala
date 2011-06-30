package controllers

import play._
import play.mvc._
import play.libs.{Codec, WS}
import play.cache.Cache
import java.security.SecureRandom
import JsonConverters._

object Application extends Controller {
    

    def index = Template

    def fetch(slug: String) = {
    	    val req = WS.url("https://michaelvideo.cloudbees.cloudant.com/urly/" + slug)      
	    	req.setHeader("Authorization", Couch.auth)
	    	val mapping = fromJson(req.get().getString, classOf[Mapping])
	    	mapping.url
    }

    def lengthen(slug: String) = {
    	val url = Cache.get(slug) match {
    		case None => {
    			val url = fetch(slug)
    			Cache.set(slug, url, "30mn")
    			url
    		}
			case Some(url) => url.asInstanceOf[String]
    	}
    	Redirect(url)	    
	}


    val total_chars = (48 to 57) ++  (65 to 90) ++ (97 to 122) 


    def shorten(url: String) = {
    	val slug = random_slug(5)
    	val short_url = request.getBase + "/" + store_mapping(slug, url)
 		short_url
    }


    def store_mapping(slg: Array[Char], url: String) = {
    	val slug = new String(slg)
	    val req = WS.url("https://michaelvideo.cloudbees.cloudant.com/urly/" + slug)      
    	req.setHeader("Authorization", Couch.auth)
    	req.body(toJson(Mapping(if (url.startsWith("http")) url else "http://" + url)))
    	req.put()
    	slug
    }


    def random_slug(size: Int) = 
    	(1 to size).map{ x => 			    
			    total_chars((new SecureRandom).nextInt(total_chars.size - 1)).asInstanceOf[Char]
	    }.toArray


    /** show a mapping of URL to slug - an example of a view */
    def showUrls = {
            val req = WS.url("https://michaelvideo.cloudbees.cloudant.com/urly/_design/urls/_view/all")      
            req.setHeader("Authorization", Couch.auth)
            Json(req.get().getString)
    }

    

    
}



object Couch {
	    def auth = "Basic " + 
    		Codec.encodeBASE64(Play.configuration.get("couch.user").toString 
    		+ ":" + Play.configuration.get("couch.password").toString)


}

case class Mapping(url: String)



object JsonConverters {
  import com.google.gson.Gson
  def fromJson[T](str: String, cls: Class[T]) : T = (new Gson).fromJson(str, cls)
  def toJson[T](obj: AnyRef) = (new Gson).toJson(obj)
}

